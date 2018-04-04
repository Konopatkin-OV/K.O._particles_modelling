module Particles where

import BaseApp
import Graphics.Gloss.Interface.IO.Game

run :: IO ()
run = do world <- load_world 600 (-450, -50) (600, 400) "world.txt"
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
