module BaseApp where

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Game
import Data.List (intercalate)
import Data.Maybe (isJust, fromJust)
import Debug.Trace


-------------------------------------------------
-- параметры жидкости
-- TODO: возможно их надо приклеить к миру или к отдельным частицам
-- здоровенные числа напрягают
const_k :: Float      -- жёсткость (сжимаемость)
const_k = 1.0 * 3000000000.0

const_p_0 :: Float    -- плотность окружающей среды
const_p_0 = 0.0

const_myu :: Float    -- вязкость
const_myu = 1.0 * 300000000.0

const_sigma :: Float  -- поверхностное натяжение
const_sigma = 1.0 * 3000000000.0

const_g :: Float      -- ускорение свободного падения
const_g = 0.1 * 500.0

const_r :: Float
const_r = 0.2         -- отражение от границ мира
-------------------------------------------------

data Application = App             -- объект окна с приложением
  { elems :: [Interface]           -- интерактивные и не очень элементы интерфейса
  , mouse_pos :: (Float, Float)}   -- положение указателя
  deriving Show

-- "базовая часть" элементов интерфейса
-- считаем, что элемент интерфейса - прямоугольник, параллельный осям координат, в который можно кликать мышью
data BaseInterface = IBase
  { place   :: Point                                      -- положение в окне (левый нижний угол)
  , size    :: Vector                                     -- направление на правый верхний угол прямоугольника
  , action  :: (Event -> Application -> IO Application)   -- действие при активации
  , draw    :: (Interface -> Picture)                     -- функция отрисовки
  , process :: (Float -> Interface -> Interface)}         -- функция обработки времени

instance Show BaseInterface where
    show (IBase p s _ _ _) = show ("IBase: ", p, s)

data Interface = Button              -- кнопка
  { ibase       :: BaseInterface     -- базовая часть
  , b_text      :: String            -- надпись на кнопке
  , b_col       :: Color             -- цвет кнопки
  , b_click_col :: Color             -- цвет после клика
  , b_text_col  :: Color             -- цвет текста
  , b_time      :: Float             -- время после последнего клика
  }
  | World                        -- интерактивный симулируемый мир
  { ibase      :: BaseInterface  -- базовая часть
  , h_smooth   :: Float          -- константа h - длина сглаживания
  , entities   :: [Entity]       -- сущности в игровом мире
  , back_col   :: Color}         -- цвет фона
  deriving Show

data Entity = Particle           -- частица
  { e_pos    :: Point            -- положение в мире
  , e_speed  :: Vector           -- скорость
  , e_mass   :: Float            -- масса
  , e_dense  :: Float            -- плотность, пересчитывается автоматически
  , e_radius :: Float            -- размер частицы (для рисования/столкновений)
  , e_color  :: Color}           -- цвет частицы
  deriving Show

-------------------------- обработчик событий --------------------------
app_handle_events :: Event -> Application -> IO Application
app_handle_events event app = (foldr (elem_action event) (return app) (elems app))
 
elem_action :: Event -> Interface -> IO Application -> IO Application
elem_action event element app = do cur_app <- app
                                   (action (ibase element)) event cur_app

action_none :: Event -> Application -> IO Application
action_none _ app = return app


-- клик по миру: создаёт новую частицу в точке клика, которая летит в центр мира
action_pain :: Event -> Application -> IO Application
action_pain (EventKey (MouseButton LeftButton) Down _ pos) app | pointInBox pos w_place (w_place + w_size) = return (replace_int 0 new_world app)
                                                               | otherwise = return app
  where
    new_world = old_world {entities = (new_particle : (entities old_world))}
    old_world = (elems app) !! 0     -- считаем, что мир - нулевой интерфейс приложения
    new_particle = Particle {e_pos = w_pos,
                             e_speed = (mulSV (-0.5 * 0.0001) (2 * w_pos - w_size)),
                             e_mass = 1,
                             e_dense = 1.0, -- само пусть считается
                             e_radius = 20,
                             e_color = makeColor 0.0 0.0 0.6 1.0}
    w_place = (place (ibase old_world))
    w_size = (size (ibase old_world))
    w_pos = pos - (place (ibase old_world)) -- положение указателя относительно мира
    --wx = (fst (size (ibase old_world))) / 2
    --wy = (snd (size (ibase old_world))) / 2
action_pain _ app = return app

-- клик по элементу интерфейса
action_click :: BaseInterface -> (Application -> IO Application) -> Event -> Application -> IO Application
action_click base f (EventKey (MouseButton LeftButton) Down _ pos) app | pointInBox pos ld_corner ru_corner = f app
                                                                              | otherwise = return app
  where
    ld_corner = (place base)
    ru_corner = ld_corner + (size base)
action_click _ _ _ app = return app


-- клик по кнопке
action_button_click :: Int -> (Application -> IO Application) -> Application -> IO Application
action_button_click num f app = f (replace_int num new_button app)
  where
    old_button = (elems app) !! num
    new_button = old_button {b_time = 0.0}


action_load_world :: String -> Application -> IO Application
action_load_world filename app = do r_world <- load_world h_smooth_ place_ size_ filename
                                    return (replace_int 0 r_world app)
                                    where
                                      world = (elems app) !! 0
                                      h_smooth_ = (h_smooth world)
                                      place_ = (place (ibase world))
                                      size_ = (size (ibase world))

action_save_world :: String -> Application -> IO Application
action_save_world filename app = do save_world ((elems app) !! 0) filename
                                    return app
------------------------------------------------------------------------

-------------------------- отрисовка приложения ------------------------
app_draw :: Application -> IO Picture
app_draw app = return (pictures (map elem_draw (elems app)))

elem_draw :: Interface -> Picture
elem_draw element = (draw (ibase element)) $ element

draw_none :: Interface -> Picture
draw_none _ = Blank

draw_button :: Interface -> Picture
draw_button button = translate (x + sx / 2) (y + sy / 2) (pictures [color cur_col (rectangleSolid sx sy),
                                              color (b_col button) (rectangleSolid (sx - 6) (sy - 6)),
                                              color (b_text_col button) (text_ (b_text button))])
  where
    (x, y) = (place (ibase button))
    (sx, sy) = (size (ibase button))
    cur_col = if (b_time button) < 0.4 then (b_click_col button) else black

draw_world :: Interface -> Picture
draw_world world = translate x y (pictures [color (back_col world) (polygon [(0, 0), (0, sy), (sx, sy), (sx, 0)]),
                                           (pictures (map draw_entity (entities world)))])
  where
    (x, y) = (place (ibase world))
    (sx, sy) = (size (ibase world))

-- x, y = координаты частицы; r = радиус; c = цвет
draw_entity :: Entity -> Picture
draw_entity (Particle (x, y) _ _ _ r c) = color c (translate x y (circleSolid r))
--draw_entity _ = Blank

-- нарисовать текст "приблизительно" по центру с "адекватным" размером
text_ :: String -> Picture
text_ txt = translate (-7 * fromIntegral (length txt)) (-10) (scale 0.2 0.2 (text txt))
------------------------------------------------------------------------

-------------------------- обработчик времени --------------------------
app_process :: Float -> Application -> IO Application
app_process time app = return (app {elems = map (elem_process time) (elems app)})

elem_process :: Float -> Interface -> Interface
elem_process time element = ((process (ibase element)) $ time) element

process_none :: Float -> Interface -> Interface
process_none _ element = element


process_time :: Float -> Interface -> Interface
process_time time (Button a b c d e t) = (Button a b c d e (t + time))
process_time _ elem_ = elem_


process_world :: Float -> Interface -> Interface
process_world time world = tmp_world { entities = (map (process_entity tmp_world time) (entities tmp_world))}
  where
    tmp_world = world { entities = (map (refresh_density world) (entities world))}

refresh_density :: Interface -> Entity -> Entity
refresh_density world ent = ent {e_dense = density (get_vicinity r pos world) pos}
  where
    r = (e_radius ent)
    pos = (e_pos ent)

process_entity :: Interface -> Float -> Entity -> Entity
process_entity world time (Particle (x, y) (vx, vy) m p r c) = Particle new_pos new_vel m p r c 
-- что-то может пойти не так, если world не (World ...), а другой интерфейс (Button/Slider)
  where
    --(wx, wy) = (size (ibase world))
    --tmp_x = x + time * dx
    --new_x = if tmp_x < 0 then (-tmp_x) else (if tmp_x > wx then (2 * wx - tmp_x) else tmp_x)
    --tmp_y = y + time * dy
    --new_y = if tmp_y < 0 then (-tmp_y) else (if tmp_y > wy then (2 * wy - tmp_y) else tmp_y)
    --tmp_dx = if (tmp_x < 0 || tmp_x > wx) then (-dx) else dx
    --tmp_dy = if (tmp_y < 0 || tmp_y > wy) then (-dy) else dy
    (f_x, f_y) = use_force p (vx, vy) (get_vicinity r (x, y) world) (x, y)
    tmp_vx = vx + time * (f_x / m)
    tmp_vy = vy + time * (f_y / m) - const_g * time  -- добавляем ускорение свободного падения
    (new_pos, new_vel) = (bound_bounce (size (ibase world))
                          ((x + time * tmp_vx, y + time * tmp_vy), 
                           (tmp_vx, tmp_vy)))
-- посчитали силу -> использовали силу -> проехали -> отразились
--process_entity _ _ e = e

bound_bounce :: Vector -> (Point, Vector) -> (Point, Vector)
bound_bounce size_ ((x, y), (vx, vy)) = ((new_x, new_y), (new_vx, new_vy))
  where
    (wx, wy) = size_
    new_x = if x < 0 then (-const_r * x) else (if x > wx then (wx - const_r * (wx - x)) else x)
    new_y = if y < 0 then (-const_r * y) else (if y > wy then (wy - const_r * (wy - y)) else y)
    new_vx = if (x < 0 || x > wx) then (-const_r * vx) else vx
    new_vy = if (y < 0 || y > wy) then (-const_r * vy) else vy

get_vicinity :: Float -> Point -> Interface -> Interface
get_vicinity r pos world = world {entities = filter check (entities world)
                                  } -- TODO: сделать длину сглаживания зависящей от радиуса частицы
  where
    check = (\ent -> (dist (e_pos ent) pos) <= r)

------------------------------- физика ---------------------------------
------------------------------------------------------------------------

use_force :: Float -> Vector -> Interface -> Point -> Vector
use_force p_i v_i world pos = ((mulSV (-const_k) (f_pressure p_i world pos)) +
                               (mulSV const_myu (f_viscosity v_i world pos)) + 
                               (mulSV const_sigma (f_tension world pos)))


density :: Interface -> Point -> Float
density world pos = sum (map (p_dense (h_smooth world) pos) (entities world)) -- + const_p_0

p_dense :: Float -> Point -> Entity -> Float
p_dense h c (Particle pos _ m _ _ _) = -- if ((dist pos c) > h) then 0.0 else 
                                       m * (ker_density (dist pos c) h)
-- p_dense _ _ _ = 0


get_value :: (Entity -> Float) -> (Float -> Float -> Float) -> Interface -> Point -> Vector
get_value func kernel world pos = sum (map map_f (entities world))
  where map_f = (\ent -> -- if ((dist pos (e_pos ent)) > (h_smooth world)) then (0.0, 0.0) else 
                         (mulSV ((func ent) * (e_mass ent) / (e_dense ent) *
                                (kernel (dist pos (e_pos ent)) (h_smooth world))) 
                         (normalize (pos - (e_pos ent)))))
--get_value _ _ _ _ = 0

get_value_v :: (Entity -> Vector) -> (Float -> Float -> Float) -> Interface -> Point -> Vector
get_value_v func kernel world pos = sum (map map_f (entities world))
  where map_f = (\ent -> -- if ((dist pos (e_pos ent)) > (h_smooth world)) then (0.0, 0.0) else 
                         (mulSV ((e_mass ent) / (e_dense ent) *
                               (kernel (dist pos (e_pos ent)) (h_smooth world))) 
                         (func ent)))
--get_value_v _ _ _ _ = 0



f_pressure :: Float -> Interface -> Point -> Vector
f_pressure p_i world pos = (get_value func ker_nab_poly6 world pos)
  where func = (\ent -> ((p_i + (e_dense ent)) / 2 - const_p_0))

f_viscosity :: Vector -> Interface -> Point -> Vector
f_viscosity v_i world pos = (get_value_v func ker_nab2_poly6 world pos)
  where func = (\ent -> (v_i - (e_speed ent)))

f_tension :: Interface -> Point -> Vector
f_tension world pos = (get_value (\_ -> 1.0) ker_nab2_poly6 world pos)

-- магические ядра --
ker_poly6 :: Float -> Float -> Float
ker_poly6 r h | (0 <= r && r <= h) = (315 / 64 / pi) * ((h^2 - r^2) ^ 3) / (h^9)
              | otherwise = 0

ker_nab_poly6 :: Float -> Float -> Float
ker_nab_poly6 r h | (0 <= r && r <= h) = (-315 / 64 / pi) * (6 * r * ((h^2 - r^2) ^ 2)) / (h^9)
                  | otherwise = 0

ker_nab2_poly6 :: Float -> Float -> Float
ker_nab2_poly6 r h | (0 <= r && r <= h) = (315 / 64 / pi) * (6 * (h^2 - r^2) * (7 * r^2 - 3 * h^2)) / (h^9)
  --(315 / 64 / pi) * (6 * (h^2 - r^2) * (4 * r^2 - (h^2 - r^2))) / (h^9)
                   | otherwise = 0

ker_density :: Float -> Float -> Float
ker_density r h | (0 <= r && r <= h) = (1 - r / h) ^ 2
                | otherwise = 0

------------------------------------------------------------------------

dist :: Point -> Point -> Float
dist (x1, y1) (x2, y2) = ((x1 - x2) ** 2 + (y1 - y2) ** 2) ** 0.5

normalize :: Vector -> Vector
normalize vec | (magV vec == 0) = vec
              | otherwise = mulSV (1.0 / magV vec) vec

------------------------------------------------------------------------
------------------------------------------------------------------------

-- изменить один элемент интерфейса
replace_int :: Int -> Interface -> Application -> Application
replace_int n new app = app {elems = (take n el) ++ (new : (drop (n + 1) el))}
                        where el = elems app

------------------------------------------------------------------------

-- загрузка состояния мира их файла, принимает путь к файлу
-- формат файла: описание частицы в отдельной строке
-- '#' - комментарий

--                                 TODO : запихать остальные параметры в файл                

load_world :: Float -> Point -> Vector -> String -> IO Interface
load_world h_smooth_ place_ size_ file = 
  do file_text <- (readFile file)
     let strings = (filter (\s -> (s /= []) && ((head (head s)) /= '#')) (map words (lines file_text))) ++ [[]]
     return World {ibase = IBase {place = place_, 
                                  size = size_,
                                  action = action_pain,            ---------------------------------------------------------------------------------------------
                                  draw = draw_world,               ---------------------------------------------------------------------------------------------
                                  process = process_world}         ---------------------------------------------------------------------------------------------
                  , h_smooth = h_smooth_
                  , entities = (map fromJust (filter isJust (map load_particle (tail strings))))
                  , back_col = load_color_l (head strings)}


load_particle :: [String] -> Maybe Entity
load_particle (x_pos : (y_pos : (x_speed : (y_speed : (mass : (rad : col)))))) = 
  Just Particle { e_pos = ((read x_pos :: Float), (read y_pos :: Float))
                , e_speed = ((read x_speed :: Float), (read y_speed :: Float))
                , e_mass = (read mass :: Float)
                , e_dense = 1.0 -- всё равно пересчитается
                , e_radius = (read rad :: Float)
                , e_color = load_color_l col}
load_particle _ = Nothing
-- получить цвет из первых 4 элементов списка (RGBA)
load_color_l :: [String] -> Color
load_color_l (col_r : (col_g : (col_b : (col_a : _)))) = makeColor (read col_r :: Float) (read col_g :: Float)
                                                                   (read col_b :: Float) (read col_a :: Float)
load_color_l _ = black


-- сохранение мира в файл
save_world :: Interface -> String -> IO ()
save_world world file = writeFile file ((write_color (back_col world)) ++ (write_entities (entities world)))

write_color :: Color -> String
write_color col = drop 5 (show col)

write_entities :: [Entity] -> String
write_entities ents = foldr write_next_entity "" ents

-- страшная строчка
write_next_entity :: Entity -> String -> String
write_next_entity (Particle (x, y) (vx, vy) m _ r col) buf = buf ++ "\n" ++ (intercalate " " (map show [x, y, vx, vy, m, r])) ++ " " ++ (write_color col)
--write_next_entity _ buf = buf
