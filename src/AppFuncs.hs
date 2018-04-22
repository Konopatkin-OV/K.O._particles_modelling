module AppFuncs where

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Game
import Text.Printf
import Debug.Trace

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

slider_to_text :: Int -> String -> Application -> String
slider_to_text num fmt app = printf fmt (s_value sl)
  where
    sl = ((elems app) !! num)

get_slider_val :: Int -> Application -> Float
get_slider_val num app = (s_value ((elems app) !! num))


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