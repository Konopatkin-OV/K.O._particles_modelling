module AppFuncs where

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Game
import Text.Printf
import Debug.Trace

import BaseClasses
import BaseApp


action_change_text :: Int -> (Application -> String) -> Event -> Application -> IO Application
action_change_text num f _ app = return (replace_int num new_text_field app)
  where 
    old_text_field = ((elems app) !! num)
    new_text_field = old_text_field {t_text = (f app)}

slider_to_text :: Int -> String -> Application -> String
slider_to_text num fmt app = printf fmt (s_value sl)
  where
    sl = ((elems app) !! num)