module Particles where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Debug.Trace
import Data.List (intercalate)

run :: IO ()
run = do world <- load_world "world.txt"
         let app = App {elems = [world, button_load, button_save, button_reload], mouse_pos = (0, 0)}
         playIO display white 50 app app_draw app_handle_events app_process 
           where
             display = (InWindow "TEST" (1000, 800) (0, 0))
             b_l_base = IBase {place = (200, 200), 
                               size = (200, 50),
                               action = (action_click b_l_base (action_button_click 1 (action_load_world "world.txt"))),  -- "1" - индекс кнопки в списке элементов интерфейса
                               draw = draw_button,
                               process = process_time
                               }
             button_load = Button {ibase = b_l_base,
                                   b_text = "Load world",
                                   b_col = (makeColor 0.5 0.5 0.5 1.0),
                                   b_click_col = (makeColor 0.1 0.7 0.1 1.0),
                                   b_text_col = black,
                                   b_time = 1.0}
             b_s_base = IBase {place = (200, 100), 
                               size = (200, 50),
                               action = (action_click b_s_base (action_button_click 2 (action_save_world "world.txt"))),  -- "2" - индекс кнопки в списке элементов интерфейса
                               draw = draw_button,
                               process = process_time
                               }
             button_save = Button {ibase = b_s_base,
                                   b_text = "Save world",
                                   b_col = (makeColor 0.5 0.5 0.5 1.0),
                                   b_click_col = (makeColor 0.1 0.7 0.1 1.0),
                                   b_text_col = black,
                                   b_time = 1.0}
             b_r_base = IBase {place = (200, 300), 
                               size = (200, 50),
                               action = (action_click b_r_base (action_button_click 3 (action_load_world "base_world.txt"))),  -- "3" - индекс кнопки в списке элементов интерфейса
                               draw = draw_button,
                               process = process_time
                               }
             button_reload = Button {ibase = b_r_base,
                                     b_text = "Reload world",
                                     b_col = (makeColor 0.5 0.5 0.5 1.0),
                                     b_click_col = (makeColor 0.1 0.7 0.1 1.0),
                                     b_text_col = black,
                                     b_time = 1.0}
------------------------------------------------------------------------------------------------------------------------------
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
  , entities   :: [Entity]       -- сущности в игровом мире
  , back_col   :: Color}         -- цвет фона
  deriving Show

data Entity = Particle           -- частица
  { e_pos :: Point               -- положение в мире
  , e_speed :: Vector            -- скорость
  , e_radius :: Float            -- размер частицы (для рисования/столкновений)
  , e_color :: Color}            -- цвет частицы
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
                             e_speed = (mulSV (-0.5) (2 * w_pos - w_size)),
                             e_radius = 5,
                             e_color = black}
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
action_load_world filename app = do r_world <- load_world filename
                                    return (replace_int 0 r_world app)

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
    x = (fst (place (ibase button)))
    y = (snd (place (ibase button)))
    sx = (fst (size (ibase button)))
    sy = (snd (size (ibase button)))
    cur_col = if (b_time button) < 0.4 then (b_click_col button) else black

draw_world :: Interface -> Picture
draw_world world = translate x y (pictures [color (back_col world) (polygon [(0, 0), (0, sy), (sx, sy), (sx, 0)]),
                                           (pictures (map draw_entity (entities world)))])
  where
    x = (fst (place (ibase world)))
    y = (snd (place (ibase world)))
    sx = (fst (size (ibase world)))
    sy = (snd (size (ibase world)))

-- x, y = координаты частицы; r = радиус; c = цвет
draw_entity :: Entity -> Picture
draw_entity (Particle (x, y) _ r c) = color c (translate x y (circleSolid r))
draw_entity _ = Blank

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
process_world time world = world { entities = (map (process_entity world time) (entities world))}

process_entity :: Interface -> Float -> Entity -> Entity
process_entity (World (IBase _ (wx, wy) _ _ _) _ _) time (Particle (x, y) (dx, dy) r c) = Particle (new_x, new_y) (new_dx, new_dy) r c 
  where
    new_x = if tmp_x < 0 then (-tmp_x) else (if tmp_x > wx then (2 * wx - tmp_x) else tmp_x)
    tmp_x = x + time * dx
    new_y = if tmp_y < 0 then (-tmp_y) else (if tmp_y > wy then (2 * wy - tmp_y) else tmp_y)
    tmp_y = y + time * dy
    new_dx = if (tmp_x < 0 || tmp_x > wx) then (-dx) else dx
    new_dy = if (tmp_y < 0 || tmp_y > wy) then (-dy) else dy
process_entity _ _ e = e
------------------------------------------------------------------------

-- изменить один элемент интерфейса
replace_int :: Int -> Interface -> Application -> Application
replace_int n new app = app {elems = (take n el) ++ (new : (drop (n + 1) el))}
                        where el = elems app

------------------------------------------------------------------------

-- загрузка состояния мира их файла, принимает путь к файлу
-- формат файла: описание частицы в отдельной строке
-- '#' - комментарий
load_world :: String -> IO Interface
load_world file = do file_text <- (readFile file)
                     let strings = (filter (\s -> ((head (head s)) /= '#')) (map words (lines file_text)))
                     return World {ibase = IBase {place = (-450, -50), 
                                                  size = (600, 400),
                                                  action = action_pain,            --------------------------------------------------------------------------------------------- палево!
                                                  draw = draw_world,               --------------------------------------------------------------------------------------------- палево!
                                                  process = process_world}         --------------------------------------------------------------------------------------------- палево!
                                  , entities = map load_particle (tail strings)
                                  , back_col = load_color_l (head strings)}
                              

load_particle :: [String] -> Entity
load_particle (x_pos : (y_pos : (x_speed : (y_speed : (rad : col))))) = Particle { e_pos = ((read x_pos :: Float), (read y_pos :: Float))
                                                                                 , e_speed = ((read x_speed :: Float), (read y_speed :: Float))
                                                                                 , e_radius = (read rad :: Float)
                                                                                 , e_color = load_color_l col}
load_particle _ = Particle (0, 0) (0, 0) 0 black
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
write_next_entity (Particle (x, y) (vx, vy) r col) buf = buf ++ "\n" ++ (intercalate " " (map show [x, y, vx, vy, r])) ++ " " ++ (write_color col)
write_next_entity _ buf = buf
