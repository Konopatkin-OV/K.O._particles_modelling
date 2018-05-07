module Particles where

import BaseApp
import AppFuncs
import BaseClasses
import Graphics.Gloss.Interface.IO.Game

run :: IO ()
run = do world <- load_world 10 (-350, -380) (400, 780) "world.txt"
         let app = App {elems = [world, button_load, button_save, button_reload, 
                                 text_field_time, slider_time, button_go, button_pause,
                                 text_field_const_k, slider_const_k, text_field_const_p_0, slider_const_p_0,
                                 text_field_const_myu, slider_const_myu, text_field_const_sigma, slider_const_sigma,
                                 text_field_const_g, slider_const_g, text_field_const_r, slider_const_r,
                                 text_field_const_h, slider_const_h
                                 , button_shake -- костылики
                                ], 
                        mouse_pos = (0, 0)}
         playIO display white const_FPS app app_draw app_handle_events app_process 
           where
             display = (InWindow "Particles" (800, 900) (0, 0))
-----------------------------------------------------   кнопки загрузки/сохранения   ------------------------------------------
             b_l_base = IBase {place = (100, 290), 
                               size = (250, 50),
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

             b_s_base = IBase {place = (100, 230), 
                               size = (250, 50),
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

             b_r_base = IBase {place = (100, 350), 
                               size = (250, 50),
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
-----------------------------------------------------   скорость времени   ---------------------------------------------------
             text_field_time = TextField {ibase = t_f_t_base,
                                          t_text = "Time speed: 0 (paused)",
                                          t_col = red,
                                          t_scale = 0.15}
             t_f_t_base = IBase {place = (100, 90),
                                 size = (250, 50),
                                 action = action_none,
                                 draw = draw_text_field,
                                 process = process_none}

             slider_time = Slider {ibase = s_t_base,
                                   s_indent = (10, 5),
                                   s_min = 0.1,
                                   s_max = 5.0,
                                   s_pts = 50,
                                   s_curpt = 9,
                                   s_col = (makeColor 0.5 0.25 0.0 1.0),
                                   s_sl_size = 10,
                                   s_sl_col = (makeColor 0.8 0.8 0.8 1.0),
                                   s_m_act = False}
             s_t_base = IBase {place = (100, 45),
                               size = (250, 50),
                               action = action_multi [(action_change_time_text 4 (slider_to_text 5 id "Time speed: %.1f")),
                                                      (action_slider 5),
                                                      (action_set_world_time_speed (get_slider_val 5))],
                                                      -- 4 - текстовое поле, которое зависит от 5-слайдера
                               draw = draw_slider,
                               process = process_none}
-----------------------------------------------------   кнопки старт/пауза   -------------------------------------------------
             b_go_base = IBase {place = (100, 150), 
                                size = (115, 50),
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

             b_pause_base = IBase {place = (235, 150), 
                                   size = (115, 50),
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
-----------------------------------------------------   манипуляции константами   --------------------------------------------
             text_field_const_k     = TextField {ibase = t_f_c_0_base,
                                                 t_text = "Pressure: 2.5",
                                                 t_col = black,
                                                 t_scale = 0.15}
             t_f_c_0_base = IBase {place = (100, 0),
                                   size = (250, 30),
                                   action = action_none,
                                   draw = draw_text_field,
                                   process = process_none}

             slider_const_k     = Slider {ibase = s_c_0_base,
                                          s_indent = (10, 3),
                                          s_min = 0,
                                          s_max = 50,
                                          s_pts = 51,
                                          s_curpt = 10,
                                          s_col = (makeColor 0.5 0.5 0.5 1.0),
                                          s_sl_size = 10,
                                          s_sl_col = (makeColor 0.9 0.9 0.9 1.0),
                                          s_m_act = False}
             s_c_0_base = IBase {place = (100, -30),
                                 size = (250, 30),
                                 action = action_multi [(action_change_text 8 (slider_to_text 9 (* 0.1) "Pressure: %.1f")),
                                                        (action_slider 9),
                                                        (action_set_world_const 0 ((* 1) . (get_slider_val_with_h (** 5) 9)))],
                                                     -- (action_set_world_const 0 ((* 100000000) . (get_slider_val 9)))],
                                                        -- 8 - текстовое поле, которое зависит от 9-слайдера
                                 draw = draw_slider,
                                 process = process_none}
----------------------------------------------------------------------------------
             text_field_const_p_0   = TextField {ibase = t_f_c_1_base,
                                                 t_text = "Environment density: 2.5",
                                                 t_col = black,
                                                 t_scale = 0.15}
             t_f_c_1_base = IBase {place = (100, -70),
                                   size = (250, 30),
                                   action = action_none,
                                   draw = draw_text_field,
                                   process = process_none}

             slider_const_p_0   = Slider {ibase = s_c_1_base,
                                          s_indent = (10, 3),
                                          s_min = 0,
                                          s_max = 50,
                                          s_pts = 51,
                                          s_curpt = 5,
                                          s_col = (makeColor 0.5 0.5 0.5 1.0),
                                          s_sl_size = 10,
                                          s_sl_col = (makeColor 0.9 0.9 0.9 1.0),
                                          s_m_act = False}
             s_c_1_base = IBase {place = (100, -100),
                                 size = (250, 30),
                                 action = action_multi [(action_change_text 10 (slider_to_text 11 (* 0.1) "Environment density: %.1f")),
                                                        (action_slider 11),
                                                        (action_set_world_const 1 ((* 0.1) . (get_slider_val 11)))],
                                 draw = draw_slider,
                                 process = process_none}
----------------------------------------------------------------------------------
             text_field_const_myu   = TextField {ibase = t_f_c_2_base,
                                                 t_text = "Viscosity: 2.5",
                                                 t_col = black,
                                                 t_scale = 0.15}
             t_f_c_2_base = IBase {place = (100, -140),
                                   size = (250, 30),
                                   action = action_none,
                                   draw = draw_text_field,
                                   process = process_none}

             slider_const_myu   = Slider {ibase = s_c_2_base,
                                          s_indent = (10, 3),
                                          s_min = 0,
                                          s_max = 50,
                                          s_pts = 51,
                                          s_curpt = 10,
                                          s_col = (makeColor 0.5 0.5 0.5 1.0),
                                          s_sl_size = 10,
                                          s_sl_col = (makeColor 0.9 0.9 0.9 1.0),
                                          s_m_act = False}
             s_c_2_base = IBase {place = (100, -170),
                                 size = (250, 30),
                                 action = action_multi [(action_change_text 12 (slider_to_text 13 (* 0.1) "Viscosity: %.1f")),
                                                        (action_slider 13),
                                                        (action_set_world_const 2 ((* 0.03) . (get_slider_val_with_h (** 5) 13)))],
                                                     -- (action_set_world_const 2 ((* 3000000) . (get_slider_val 13)))],
                                 draw = draw_slider,
                                 process = process_none}
----------------------------------------------------------------------------------
             text_field_const_sigma = TextField {ibase = t_f_c_3_base,
                                                 t_text = "Tension: 2.5",
                                                 t_col = black,
                                                 t_scale = 0.15}
             t_f_c_3_base = IBase {place = (100, -210),
                                   size = (250, 30),
                                   action = action_none,
                                   draw = draw_text_field,
                                   process = process_none}

             slider_const_sigma = Slider {ibase = s_c_3_base,
                                          s_indent = (10, 3),
                                          s_min = 0,
                                          s_max = 50,
                                          s_pts = 51,
                                          s_curpt = 10,
                                          s_col = (makeColor 0.5 0.5 0.5 1.0),
                                          s_sl_size = 10,
                                          s_sl_col = (makeColor 0.9 0.9 0.9 1.0),
                                          s_m_act = False}
             s_c_3_base = IBase {place = (100, -240),
                                 size = (250, 30),
                                 action = action_multi [(action_change_text 14 (slider_to_text 15 (* 0.1) "Tension: %.1f")),
                                                        (action_slider 15),
                                                        (action_set_world_const 3 ((* 1) . (get_slider_val_with_h (** 5) 15)))],
                                                    --  (action_set_world_const 3 ((* 100000000) . (get_slider_val 15)))],
                                 draw = draw_slider,
                                 process = process_none}
----------------------------------------------------------------------------------
             text_field_const_g     = TextField {ibase = t_f_c_4_base,
                                                 t_text = "Gravity: 2.5",
                                                 t_col = black,
                                                 t_scale = 0.15}
             t_f_c_4_base = IBase {place = (100, -280),
                                   size = (250, 30),
                                   action = action_none,
                                   draw = draw_text_field,
                                   process = process_none}

             slider_const_g     = Slider {ibase = s_c_4_base,
                                          s_indent = (10, 3),
                                          s_min = 0,
                                          s_max = 50,
                                          s_pts = 51,
                                          s_curpt = 10,
                                          s_col = (makeColor 0.5 0.5 0.5 1.0),
                                          s_sl_size = 10,
                                          s_sl_col = (makeColor 0.9 0.9 0.9 1.0),
                                          s_m_act = False}
             s_c_4_base = IBase {place = (100, -310),
                                 size = (250, 30),
                                 action = action_multi [(action_change_text 16 (slider_to_text 17 (* 1) "Gravity: %.1f")),
                                                        (action_slider 17),
                                                        (action_set_world_const 4 ((* 10) . (get_slider_val 17)))],
                                 draw = draw_slider,
                                 process = process_none}
----------------------------------------------------------------------------------
             text_field_const_r     = TextField {ibase = t_f_c_5_base,
                                                 t_text = "Border reflection: 0.1",
                                                 t_col = black,
                                                 t_scale = 0.15}
             t_f_c_5_base = IBase {place = (100, -350),
                                   size = (250, 30),
                                   action = action_none,
                                   draw = draw_text_field,
                                   process = process_none}

             slider_const_r     = Slider {ibase = s_c_5_base,
                                          s_indent = (10, 3),
                                          s_min = 0,
                                          s_max = 50,
                                          s_pts = 51,
                                          s_curpt = 10,
                                          s_col = (makeColor 0.5 0.5 0.5 1.0),
                                          s_sl_size = 10,
                                          s_sl_col = (makeColor 0.9 0.9 0.9 1.0),
                                          s_m_act = False}
             s_c_5_base = IBase {place = (100, -380),
                                 size = (250, 30),
                                 action = action_multi [(action_change_text 18 (slider_to_text 19 (* 0.02) "Border reflection: %.2f")),
                                                        (action_slider 19),
                                                        (action_set_world_const 5 ((* 0.02) . (get_slider_val 19)))],
                                 draw = draw_slider,
                                 process = process_none}
------------------------------------------------------------------------------------------------------------------------------
             text_field_const_h     = TextField {ibase = t_f_c_6_base,
                                                 t_text = "Smooth radius: 10",
                                                 t_col = black,
                                                 t_scale = 0.15}
             t_f_c_6_base = IBase {place = (100, -420),
                                   size = (250, 30),
                                   action = action_none,
                                   draw = draw_text_field,
                                   process = process_none}

             slider_const_h     = Slider {ibase = s_c_6_base,
                                          s_indent = (10, 3),
                                          s_min = 1,
                                          s_max = 50,
                                          s_pts = 50,
                                          s_curpt = 9,
                                          s_col = (makeColor 0.5 0.5 0.5 1.0),
                                          s_sl_size = 10,
                                          s_sl_col = (makeColor 0.9 0.9 0.9 1.0),
                                          s_m_act = False}
             s_c_6_base = IBase {place = (100, -450),
                                 size = (250, 30),
                                 action = action_multi [(action_change_text 20 (slider_to_text 21 id "Smooth radius: %.0f")),
                                                        (action_slider 21),
                                                        (action_set_world_h (get_slider_val 21))],
                                 draw = draw_slider,
                                 process = process_none}
------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------   кнопка для расклеивания частиц   ------------------------------------------
             b_shk_base = IBase {place = (-300, -450), 
                                 size = (300, 50),
                                 action = (action_click b_shk_base (action_button_click 22 (action_shake_world))),
                                 draw = draw_button,
                                 process = process_time
                                 }
             button_shake = Button {ibase = b_shk_base,
                                    b_text = "separate particles",
                                    b_col = (makeColor 0.5 0.5 0.5 1.0),
                                    b_click_col = (makeColor 0.7 0.1 0.1 1.0),
                                    b_text_col = (makeColor 0.7 0.1 0.1 1.0),
                                    b_time = 1.0}
------------------------------------------------------------------------------------------------------------------------------