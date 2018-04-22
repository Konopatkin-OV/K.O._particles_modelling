module Particles where

import BaseApp
import AppFuncs
import BaseClasses
import Graphics.Gloss.Interface.IO.Game

run :: IO ()
run = do world <- load_world 60 (-350, -350) (400, 700) "world.txt"
         let app = App {elems = [world, button_load, button_save, button_reload, text_field_test, slider_test], mouse_pos = (0, 0)}
         playIO display white 100 app app_draw app_handle_events app_process 
           where
             display = (InWindow "TEST" (800, 800) (0, 0))

             b_l_base = IBase {place = (100, 200), 
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

             b_s_base = IBase {place = (100, 100), 
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

             b_r_base = IBase {place = (100, 300), 
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

             text_field_test = TextField {ibase = t_f_t_base,
                                          t_text = "Slider value: 0",
                                          t_col = red,
                                          t_scale = 0.15}
             t_f_t_base = IBase {place = (100, 0),
                                 size = (200, 50),
                                 action = action_none,
                                 draw = draw_text_field,
                                 process = process_none}

             slider_test = Slider {ibase = s_t_base,
                                   s_indent = (5, 5),
                                   s_min = 0,
                                   s_max = 100,
                                   s_pts = 11,
                                   s_curpt = 0,
                                   s_col = (makeColor 0.5 0.25 0.0 1.0),
                                   s_sl_size = 10,
                                   s_sl_col = (makeColor 0.8 0.8 0.8 1.0),
                                   s_m_act = False}
             s_t_base = IBase {place = (100, -60),
                               size = (200, 50),
                               action = action_multi [(action_change_text 4 (slider_to_text 5 "Slider value: %.0f")),
                                                      (action_slider 5)],
                                                      -- 4 - текстовое поле, которое зависит от 5-слайдера
                               draw = draw_slider,
                               process = process_none}
------------------------------------------------------------------------------------------------------------------------------
