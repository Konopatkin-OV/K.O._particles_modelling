module Particles where

import BaseApp
import AppFuncs
import BaseClasses
import Graphics.Gloss.Interface.IO.Game

run :: IO ()
run = do world <- load_world 60 (-350, -350) (400, 700) "world.txt"
         let app = App {elems = [world, button_load, button_save, button_reload, text_field_time, slider_time, button_go, button_pause], mouse_pos = (0, 0)}
         playIO display white 100 app app_draw app_handle_events app_process 
           where
             display = (InWindow "TEST" (800, 800) (0, 0))

             b_l_base = IBase {place = (100, 240), 
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

             b_s_base = IBase {place = (100, 180), 
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

             text_field_time = TextField {ibase = t_f_t_base,
                                          t_text = "Time speed: 0 (paused)",
                                          t_col = red,
                                          t_scale = 0.15}
             t_f_t_base = IBase {place = (100, 40),
                                 size = (200, 50),
                                 action = action_none,
                                 draw = draw_text_field,
                                 process = process_none}

             slider_time = Slider {ibase = s_t_base,
                                   s_indent = (5, 5),
                                   s_min = 0.1,
                                   s_max = 5.0,
                                   s_pts = 50,
                                   s_curpt = 9,
                                   s_col = (makeColor 0.5 0.25 0.0 1.0),
                                   s_sl_size = 10,
                                   s_sl_col = (makeColor 0.8 0.8 0.8 1.0),
                                   s_m_act = False}
             s_t_base = IBase {place = (100, -5),
                               size = (200, 50),
                               action = action_multi [(action_change_time_text 4 (slider_to_text 5 "Time speed: %.1f")),
                                                      (action_slider 5),
                                                      (action_set_world_time_speed (get_slider_val 5))],
                                                      -- 4 - текстовое поле, которое зависит от 5-слайдера
                               draw = draw_slider,
                               process = process_none}

             b_go_base = IBase {place = (100, 100), 
                                size = (90, 50),
                                action = (action_click b_go_base (action_button_click 6 (action_set_world_pause False))),
                                draw = draw_button,
                                process = process_time
                                }
             button_go = Button {ibase = b_go_base,
                                 b_text = "Start",
                                 b_col = (makeColor 0.5 0.5 0.5 1.0),
                                 b_click_col = (makeColor 0.1 0.7 0.1 1.0),
                                 b_text_col = black,
                                 b_time = 1.0}

             b_pause_base = IBase {place = (210, 100), 
                                   size = (90, 50),
                                   action = (action_click b_pause_base (action_button_click 7 (action_set_world_pause True))),
                                   draw = draw_button,
                                   process = process_time
                                   }
             button_pause = Button {ibase = b_pause_base,
                                    b_text = "Pause",
                                    b_col = (makeColor 0.5 0.5 0.5 1.0),
                                    b_click_col = (makeColor 0.1 0.7 0.1 1.0),
                                    b_text_col = black,
                                    b_time = 1.0}
------------------------------------------------------------------------------------------------------------------------------
