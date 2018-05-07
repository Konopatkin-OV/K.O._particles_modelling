module AppFuncs where

--import Graphics.Gloss.Data.Point
--import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Game
import Text.Printf
--import Debug.Trace

import BaseClasses
import BaseApp


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
    (shaken, _) = foldr f_shake ([], 0) (entities world)

f_shake :: Entity -> ([Entity], Int) -> ([Entity], Int)
f_shake ent (res, n) = ((new_ent : res), n + 1)
  where
    (x, y) = (e_pos ent)
    shft = const_EPS * (fromIntegral n)
    new_ent = ent {e_pos = (x + shft, y + shft)}
    const_EPS = 0.001

-- может ещё где-нибудь пригодится, из встроенных есть модуль lens со стрёмным синтаксисом
replace_elem :: Int -> a -> [a] -> [a]
replace_elem n x list | (length list > n) = (take n list) ++ (x : (drop (n + 1) list))
                      | otherwise = list