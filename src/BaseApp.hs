module BaseApp where

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Game
import Data.List (intercalate)
import Data.Maybe (isJust, fromJust)
import Debug.Trace

import BaseClasses
import Physics

-------------------------- обработчик событий --------------------------
app_handle_events :: Event -> Application -> IO Application
app_handle_events event app = (foldr (elem_action event) (return app) (elems app))
 
elem_action :: Event -> Interface -> IO Application -> IO Application
elem_action event element app = do cur_app <- app
                                   (action (ibase element)) event cur_app

action_none :: Event -> Application -> IO Application
action_none _ app = return app

-- запихнуть несколько действий в одно
action_multi :: [(Event -> Application -> IO Application)] -> Event -> Application -> IO Application
action_multi acts event app = (foldr do_action (return app) (map (\a -> (a event)) acts))
-- кажется, где-то уже была композиция списка функций, но проще написать ещё раз
do_action :: (Application -> IO Application) -> IO Application -> IO Application
do_action func app = do cur_app <- app
                        func cur_app


-- клик по миру: создаёт новую частицу в точке клика, которая летит в центр мира
action_pain :: Event -> Application -> IO Application
action_pain (EventKey (MouseButton LeftButton) Down _ pos) app | pointInBox pos w_place (w_place + w_size) = return (replace_int 0 new_world app)
                                                               | otherwise = return app
  where
    new_world = old_world {entities = (new_particle : (entities old_world))}
    old_world = (elems app) !! 0     -- считаем, что мир - нулевой интерфейс приложения
    new_particle = Particle {e_pos = w_pos,
                             e_speed = (0, 0), -- (mulSV (-0.5 * 0.0001) (2 * w_pos - w_size)),
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

-- проверка попадания точки в интерфейс
check_pos :: BaseInterface -> Point -> Bool
check_pos base pos = pointInBox pos ld_corner ru_corner
  where
    ld_corner = (place base)
    ru_corner = ld_corner + (size base)

-- клик по элементу интерфейса
action_click :: BaseInterface -> (Application -> IO Application) -> Event -> Application -> IO Application
action_click base f (EventKey (MouseButton LeftButton) Down _ pos) app | check_pos base pos = f app
                                                                       | otherwise = return app
action_click _ _ _ app = return app


-- клик по кнопке
action_button_click :: Int -> (Application -> IO Application) -> Application -> IO Application
action_button_click num f app = f (replace_int num new_button app)
  where
    old_button = (elems app) !! num
    new_button = old_button {b_time = 0.0}


-- взаимодействие со слайдером
action_slider :: Int -> Event -> Application -> IO Application
action_slider num (EventKey (MouseButton LeftButton) Down _ pos) app | check_pos base pos = set_slider_pos num pos app
                                                                     | otherwise = return app
  where
    base = (ibase ((elems app) !! num))
action_slider num (EventMotion pos) app | (s_m_act ((elems app) !! num)) = set_slider_pos num pos app
action_slider num (EventKey (MouseButton LeftButton) Up _ _) app = return (replace_int num new_sl app)
  where
    new_sl = ((elems app) !! num) {s_m_act = False}
action_slider _ _ app = return app

set_slider_pos :: Int -> Point -> Application -> IO Application
set_slider_pos num pos app = return (replace_int num new_sl app)
  where
    old_sl = ((elems app) !! num)
    new_sl = old_sl {s_curpt = new_curpt, s_m_act = True}
    new_curpt = get_sl_pt old_sl pos

-- вычислить номер деления слайдера по клику
get_sl_pt :: Interface -> Point -> Int
get_sl_pt sl pos = (max 0 (min ((s_pts sl) - 1) (round (x / d_pt))))
  where 
    x = (fst pos) - (fst (place (ibase sl)))
    (sx, _) = (size (ibase sl))
    (dx, _) = (s_indent sl)
    d_pt = (sx - 2 * dx) / ((fromIntegral (s_pts sl)) - 1)


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
                                              color (b_text_col button) (text_ 0.2 (b_text button))])
  where
    (x, y) = (place (ibase button))
    (sx, sy) = (size (ibase button))
    cur_col = if (b_time button) < 0.4 then (b_click_col button) else black

draw_text_field :: Interface -> Picture
draw_text_field tf = translate (x + sx / 2) (y + sy / 2) 
                     (color (t_col tf) (text_ (t_scale tf) (t_text tf)))
  where
    (x, y) = (place (ibase tf))
    (sx, sy) = (size (ibase tf))

draw_slider :: Interface -> Picture
draw_slider sl = translate (x + sx / 2) (y + sy / 2) (pictures [color cur_col (rectangleSolid sx sy),
                                  color (s_col sl) (rectangleSolid (sx - 2 * dx) (sy - 2 * dy)),
                                  color (s_sl_col sl) (translate (-sx / 2 + sl_pos) 0
                                                  (rectangleSolid (s_sl_size sl) (sy - 2 * dy)))])
  where
    (x, y) = (place (ibase sl))
    (sx, sy) = (size (ibase sl))
    (dx, dy) = (s_indent sl)
    sl_pos = dx + (fromIntegral (s_curpt sl)) * d_pt
    d_pt = (sx - 2 * dx) / ((fromIntegral (s_pts sl)) - 1)
    cur_col = black


draw_world :: Interface -> Picture
draw_world world = translate x y (pictures [color (back_col world) (polygon [(0, 0), (0, sy), (sx, sy), (sx, 0)]),
                                           (pictures (map draw_entity (entities world)))])
  where
    (x, y) = (place (ibase world))
    (sx, sy) = (size (ibase world))

-- x, y = координаты частицы; r = радиус; c = цвет
draw_entity :: Entity -> Picture
draw_entity (Particle (x, y) _ _ _ r c) = pictures [color c (translate x y (circleSolid r)),
                                                    color (withAlpha 0.025 c) (translate x y (circleSolid 60))]
                                          -- color c (translate x y (circleSolid r))
--draw_entity _ = Blank

-- нарисовать текст "приблизительно" по центру с масштабом s
text_ :: Float -> String -> Picture
text_ s txt = translate (-35 * s * fromIntegral (length txt)) (-50 * s) (scale s s (text txt))
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
process_world time world | (not (is_pause world)) = tmp_world { entities = (map (process_entity tmp_world dt) (entities tmp_world))}
                         | otherwise = world
  where
    dt = (time * (time_speed world))
    tmp_world = world { entities = (map (refresh_density world) (entities world))}

refresh_density :: Interface -> Entity -> Entity
refresh_density world ent = ent {e_dense = density (get_vicinity r pos world) pos}
  where
    -- покойся с миром, идиотский баг: "r = (e_radius ent)"
    r = (h_smooth world)
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
    (f_x, f_y) = use_force p (vx, vy) (get_vicinity (h_smooth world) (x, y) world) (x, y)  -- и здесь был r вместо h
    tmp_vx = vx + time * (f_x / m)
    tmp_vy = vy + time * (f_y / m) - const_g * time  -- добавляем ускорение свободного падения
    (new_pos, new_vel) = (bound_bounce (size (ibase world))
                          ((x + time * tmp_vx, y + time * tmp_vy), 
                           (tmp_vx, tmp_vy)))
-- посчитали силу -> использовали силу -> проехали -> отразились
--process_entity _ _ e = e

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
                  , back_col = load_color_l (head strings)
                  , time_speed = 1.0
                  , is_pause = True
                  , constants = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]} -- костылик


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
