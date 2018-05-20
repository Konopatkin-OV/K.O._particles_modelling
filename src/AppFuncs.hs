module AppFuncs where

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Game
import Text.Printf
--import Debug.Trace

import BaseClasses
import BaseApp
import Physics


-- новый текст получаем по состоянию приложения (из какого-нибудь слайдера)
-- (а можно, например, посчитать что-нибудь про мир)
-- грустно, что будет переделываться при любом событии
-- но пересчитывать мир с кучей частиц всё равно сильно дольше
action_change_text :: Int -> (Application -> String) -> Event -> Application -> IO Application
action_change_text num f _ app = return (replace_int num new_text_field app)
  where 
    old_text_field = ((elems app) !! num)
    new_text_field = old_text_field {t_text = (f app)}

slider_to_text :: Int -> (Float -> Float) -> String -> Application -> String
slider_to_text num func fmt app = printf fmt (func (s_value sl))
  where
    sl = ((elems app) !! num)

get_slider_val :: Int -> Application -> Float
get_slider_val num app = (s_value ((elems app) !! num))


get_slider_val_with_h :: (Float -> Float) -> Int -> Application -> Float
get_slider_val_with_h f_h num app = (get_slider_val num app) * (f_h (h_smooth (head (elems app))))

-- специальный костыль для отображения паузы
action_change_time_text :: Int -> (Application -> String) -> Event -> Application -> IO Application
action_change_time_text num f _ app = return (replace_int num new_text_field app)
  where 
    old_text_field = ((elems app) !! num)
    new_text_field = old_text_field {t_text = if (is_pause ((elems app) !! 0)) 
                                              then ((f app) ++ " (paused)") -- костылик
                                              else (f app)}

-- для кнопок => без Event-а
action_set_world_pause :: Bool -> Application -> IO Application
action_set_world_pause state app = return (replace_int 0 new_world app)
  where
    new_world = ((elems app) !! 0) {is_pause = state}

-- аналогично action_change_text, всё ещё считаем, что World - нулевой в (elems app)
action_set_world_time_speed :: (Application -> Float) -> Event -> Application -> IO Application
action_set_world_time_speed f _ app = return (replace_int 0 new_world app)
  where
    new_world = ((elems app) !! 0) {time_speed = f app}

action_set_world_h :: (Application -> Float) -> Event -> Application -> IO Application
action_set_world_h f _ app = return (replace_int 0 new_world app)
  where
    new_world = ((elems app) !! 0) {h_smooth = f app}

action_set_world_file :: (Application -> String) -> Event -> Application -> IO Application
action_set_world_file f _ app = return (replace_int 0 new_world app)
  where
    new_world = ((elems app) !! 0) {filename = f app}

action_set_world_const :: Int -> (Application -> Float) -> Event -> Application -> IO Application
action_set_world_const num f _ app = return (replace_int 0 new_world app)
  where
    world = ((elems app) !! 0)
    new_world = (world {constants = new_consts})
    consts = (constants world)
    new_consts = replace_elem num (f app) consts

-- немного сдвинуть каждую частицу
action_shake_world :: Application -> IO Application
action_shake_world app = return new_app
  where
    new_app = replace_int 0 new_world app
    world = head (elems app)
    new_world = world {entities = shaken}
    shaken = zipWith f_shake (entities world) [0..]

f_shake :: Entity -> Int -> Entity
f_shake ent n = new_ent
  where
    (x, y) = (e_pos ent)
    shift = const_EPS * (fromIntegral n)
    new_ent = ent {e_pos = (x + shift, y + shift)}
    const_EPS = 0.001

-- может ещё где-нибудь пригодится, из встроенных есть модуль lens со стрёмным синтаксисом
replace_elem :: Int -> a -> [a] -> [a]
replace_elem n x list | (length list > n) = (take n list) ++ (x : (drop (n + 1) list))
                      | otherwise = list

------------------------------------------------------------------------------------------

-- задать параметр скрытости элементам интерфейса
action_set_page :: [Bool] -> Event -> Application -> IO Application
action_set_page flags _ app = return app {elems = new_elems}
  where
    new_elems = zipWith set (elems app) flags
    set = (\int act -> int {is_active = act})

-- задать время после клика нужным кнопкам (костыль зажатых/отжатых кнопок переключения страниц)
action_set_buttons_time :: Float -> [Int] -> Event -> Application -> IO Application
action_set_buttons_time time indexes _ app = return new_app
  where
    new_app = foldr (set_button_time time) app indexes

set_button_time :: Float -> Int -> Application -> Application
set_button_time time index app = new_app
  where
    new_app = replace_int index new_button app
    new_button = ((elems app) !! index) {b_time = time}

--------------------------------------------------------------

-- клик по миру: создаёт/удаляет частицы в радиусе от точки клика
action_edit :: Event -> Application -> IO Application
action_edit (EventKey (MouseButton LeftButton) Down _ pos) app | check_pos (ibase old_world) pos = return (replace_int 0 new_world app)
                                                               | otherwise = return app
  where
    old_world = (elems app) !! 0
    act = (action_type old_world)
    tmp_world = (old_world {m_act = True, m_pos = pos})
    new_world | (act == 1) = add_blob tmp_world
              | otherwise = edit_world tmp_world
action_edit (EventMotion pos) app = return new_app
  where
    new_app = (replace_int 0 (edit_world (((elems app) !! 0) {m_pos = pos})) app)
action_edit (EventKey (MouseButton LeftButton) Up _ _) app = return (replace_int 0 new_world app)
  where
    new_world = edit_world (((elems app) !! 0) {m_act = False})
action_edit _ app = return app

-- замощение шестиугольниками (треугольниками) из точек
add_blob :: Interface -> Interface
add_blob world = new_world
  where
    new_world = world {entities = (entities world) ++ blob}
    blob = (make_blob (new_particle world) pos (h_smooth world) (edit_radius world))
    pos = (m_pos world) - (place (ibase world))

make_blob :: Entity -> Point -> Float -> Float -> [Entity]
make_blob samp pos h r = filter (\ent -> ((dist (e_pos ent) pos) <= r)) blob
  where
    dirs = [( 1.0, 0.0), ( 0.5,  sqrt(3.0) / 2.0), (-0.5,  sqrt(3.0) / 2), 
            (-1.0, 0.0), (-0.5, -sqrt(3.0) / 2.0), ( 0.5, -sqrt(3.0) / 2)]
    blob = ((samp {e_pos = pos}) : foldr (++) [] (map (make_circle samp pos dirs h) 
                                                 ([1..(ceiling (r / (0.9 * h) / (sqrt(3.0) / 2.0)))] :: [Int])))

make_circle :: Entity -> Point -> [Vector] -> Float -> Int -> [Entity]
make_circle samp (x_0, y_0) dirs h n = circle
  where
    n_f = (fromIntegral n)
    pts = map (\(x, y) -> (x_0 + x * h * n_f * 0.9, y_0 + y * h * n_f * 0.9)) dirs
    segms = zipWith (,) pts ((tail pts) ++ [(head pts)])
    circle = foldr (++) [] (map (make_segm samp n) segms)

make_segm :: Entity -> Int -> (Point, Point) -> [Entity]
make_segm samp n ((x_1, y_1), (x_2, y_2)) = map put [0..(n - 1)]
  where
    dx = (x_2 - x_1) / (fromIntegral n)
    dy = (y_2 - y_1) / (fromIntegral n)
    put = (\k -> samp {e_pos = (x_1 + (fromIntegral k) * dx, 
                                y_1 + (fromIntegral k) * dy)})


-- нафиг оно оказалось не нужно
numerate_particles :: Int -> [Entity] -> [Entity]
numerate_particles n pts = zipWith (\ent k -> ent {e_id = k}) pts [n..]