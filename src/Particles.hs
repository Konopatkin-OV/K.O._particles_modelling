module Particles where

import BaseApp
import AppFuncs
import BaseClasses
import Physics

import Graphics.Gloss.Interface.IO.Game
import Text.Printf

-- бесконечные списки
inf_true :: [Bool]
inf_true  = (True  : inf_true)

inf_false :: [Bool]
inf_false = (False : inf_false)

-- распределение элементов интерфейса по "страницам"
page_save_load :: [Bool]
page_save_load = (take 4 inf_true) ++ (take 18 inf_false) ++ [True, True, False] ++ inf_true

page_physics :: [Bool]
page_physics = [True, False, False, False] ++ (take 18 inf_true) ++ [False, False, False] ++ inf_true

-- слайдеры живут по индексам [9, 11, 13, 15, 17, 19, 21]
run :: IO ()
run = do app <- load_world "worlds/world_01.txt" init_app
         playIO display white const_FPS app app_draw app_handle_events app_process 
           where
             display = (InWindow "Particles" (1200, 900) (0, 0))
             init_app = App {elems = [init_world, button_load, button_save, button_reload, 
                                 text_field_time, slider_time, button_go, button_pause,
                                 text_field_const_k, slider_const_k, text_field_const_p_0, slider_const_p_0,
                                 text_field_const_myu, slider_const_myu, text_field_const_sigma, slider_const_sigma,
                                 text_field_const_g, slider_const_g, text_field_const_r, slider_const_r,
                                 text_field_const_h, slider_const_h,
                                 text_field_filename, slider_filename
                                 , button_shake -- костылики
                                 , button_page_1, button_page_2
                                ], 
                        app_scale = 1.0, 
                        base_size = (1200, 900),
                        mouse_pos = (0, 0)}
             init_world = World {ibase = IBase {place = (-550, -380),
                                                size = (500, 780),
                                                action = action_pain,            --------------------------------------------
                                                draw = draw_world,               --------------------------------------------
                                                process = process_world          --------------------------------------------
                                               }
                               , h_smooth = 10
                               , entities = []
                               , back_col = makeColor 0.5 0.5 0.5 1.0
                               , time_speed = 1.0
                               , is_pause = True
                               , constants = base_consts
                               , filename = "world_1.txt"
                               , is_active = True
                             }
-----------------------------------------------------   кнопки загрузки/сохранения   ------------------------------------------
             b_l_base = IBase {place = (000, 240), 
                               size = (300, 50),
                               action = (action_click b_l_base (action_button_click 1 action_load_world)),  -- "1" - индекс кнопки в списке элементов интерфейса
                               draw = draw_button,
                               process = process_time
                               }
             button_load = Button {ibase = b_l_base,
                                   b_text = "Load world",
                                   b_col = (makeColor 0.5 0.5 0.5 1.0),
                                   b_click_col = (makeColor 0.1 0.7 0.1 1.0),
                                   b_text_col = black,
                                   b_time = 1.0,
                                   is_active = True}

             b_s_base = IBase {place = (000, 180), 
                               size = (300, 50),
                               action = (action_click b_s_base (action_button_click 2 action_save_world)),  -- "2" - индекс кнопки в списке элементов интерфейса
                               draw = draw_button,
                               process = process_time
                               }
             button_save = Button {ibase = b_s_base,
                                   b_text = "Save world",
                                   b_col = (makeColor 0.5 0.5 0.5 1.0),
                                   b_click_col = (makeColor 0.1 0.7 0.1 1.0),
                                   b_text_col = black,
                                   b_time = 1.0,
                                   is_active = True}

             b_r_base = IBase {place = (000, 320), 
                               size = (300, 50),
                               action = (action_click b_r_base (action_button_click 3 action_clear_world)),  -- "3" - индекс кнопки в списке элементов интерфейса
                               draw = draw_button,
                               process = process_time
                               }
             button_reload = Button {ibase = b_r_base,
                                     b_text = "Clear world",
                                     b_col = (makeColor 0.5 0.5 0.5 1.0),
                                     b_click_col = (makeColor 0.1 0.7 0.1 1.0),
                                     b_text_col = black,
                                     b_time = 1.0,
                                     is_active = True}
-----------------------------------------------------   скорость времени   ---------------------------------------------------
             text_field_time = TextField {ibase = t_f_t_base,
                                          t_text = "Time speed: 0 (paused)",
                                          t_col = red,
                                          t_scale = 0.15,
                                          is_active = False}
             t_f_t_base = IBase {place = (000, 355),
                                 size = (300, 50),
                                 action = action_none,
                                 draw = draw_text_field,
                                 process = process_none
                                }

             slider_time = Slider {ibase = s_t_base,
                                   s_indent = (10, 5),
                                   s_min = 0.1,
                                   s_max = 5.0,
                                   s_pts = 50,
                                   s_curpt = 9,
                                   s_col = (makeColor 0.5 0.25 0.0 1.0),
                                   s_sl_size = 10,
                                   s_sl_col = (makeColor 0.8 0.8 0.8 1.0),
                                   s_m_act = False,
                                   is_active = False}
             s_t_base = IBase {place = (000, 310),
                               size = (300, 50),
                               action = action_multi [(action_change_time_text 4 (slider_to_text 5 id "Time speed: %.1f")),
                                                      (action_slider 5),
                                                      (action_set_world_time_speed (get_slider_val 5))],
                                                      -- 4 - текстовое поле, которое зависит от 5-слайдера
                               draw = draw_slider,
                               process = process_none
                              }
-----------------------------------------------------   кнопки старт/пауза   -------------------------------------------------
             b_go_base = IBase {place = (000, 245), 
                                size = (140, 50),
                                action = (action_click b_go_base (action_button_click 6 (action_set_world_pause False))),
                                draw = draw_button,
                                process = process_time
                                }
             button_go = Button {ibase = b_go_base,
                                 b_text = "Start",
                                 b_col = (makeColor 0.5 0.5 0.5 1.0),
                                 b_click_col = (makeColor 0.1 0.7 0.1 1.0),
                                 b_text_col = black,
                                 b_time = 1.0,
                                 is_active = False}

             b_pause_base = IBase {place = (160, 245), 
                                   size = (140, 50),
                                   action = (action_click b_pause_base (action_button_click 7 (action_set_world_pause True))),
                                   draw = draw_button,
                                   process = process_time
                                   }
             button_pause = Button {ibase = b_pause_base,
                                    b_text = "Pause",
                                    b_col = (makeColor 0.5 0.5 0.5 1.0),
                                    b_click_col = (makeColor 0.1 0.7 0.1 1.0),
                                    b_text_col = black,
                                    b_time = 1.0,
                                    is_active = False}
------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------   манипуляции константами   --------------------------------------------
             text_field_const_k     = TextField {ibase = t_f_c_0_base,
                                                 t_text = "Pressure: 1.0",
                                                 t_col = black,
                                                 t_scale = 0.15,
                                                 is_active = False}
             t_f_c_0_base = IBase {place = (000, 150),
                                   size = (300, 30),
                                   action = action_none,
                                   draw = draw_text_field,
                                   process = process_none
                                  }

             slider_const_k     = Slider {ibase = s_c_0_base,
                                          s_indent = (10, 3),
                                          s_min = 0,
                                          s_max = 50,
                                          s_pts = 51,
                                          s_curpt = 10,
                                          s_col = (makeColor 0.5 0.5 0.5 1.0),
                                          s_sl_size = 10,
                                          s_sl_col = (makeColor 0.9 0.9 0.9 1.0),
                                          s_m_act = False,
                                          is_active = False}
             s_c_0_base = IBase {place = (000, 120),
                                 size = (300, 30),
                                 action = action_multi [(action_set_world_const 0 ((* 1) . (get_slider_val_with_h (** 5) 9))),
                                                        (action_slider 9),
                                                        (action_change_text 8 (slider_to_text 9 (* 0.1) "Pressure: %.1f"))],
                                                     -- (action_set_world_const 0 ((* 100000000) . (get_slider_val 9)))],
                                                        -- 8 - текстовое поле, которое зависит от 9-слайдера
                                 draw = draw_slider,
                                 process = process_none
                                }
----------------------------------------------------------------------------------
             text_field_const_p_0   = TextField {ibase = t_f_c_1_base,
                                                 t_text = "Environment density: 1.0",
                                                 t_col = black,
                                                 t_scale = 0.15,
                                                 is_active = False}
             t_f_c_1_base = IBase {place = (000, 80),
                                   size = (300, 30),
                                   action = action_none,
                                   draw = draw_text_field,
                                   process = process_none
                                  }

             slider_const_p_0   = Slider {ibase = s_c_1_base,
                                          s_indent = (10, 3),
                                          s_min = 0,
                                          s_max = 50,
                                          s_pts = 51,
                                          s_curpt = 10,
                                          s_col = (makeColor 0.5 0.5 0.5 1.0),
                                          s_sl_size = 10,
                                          s_sl_col = (makeColor 0.9 0.9 0.9 1.0),
                                          s_m_act = False,
                                          is_active = False}
             s_c_1_base = IBase {place = (000, 50),
                                 size = (300, 30),
                                 action = action_multi [(action_set_world_const 1 ((* 0.1) . (get_slider_val 11))),
                                                        (action_slider 11),
                                                        (action_change_text 10 (slider_to_text 11 (* 0.1) "Environment density: %.1f"))],
                                 draw = draw_slider,
                                 process = process_none
                                 }
----------------------------------------------------------------------------------
             text_field_const_myu   = TextField {ibase = t_f_c_2_base,
                                                 t_text = "Viscosity: 1.0",
                                                 t_col = black,
                                                 t_scale = 0.15,
                                                 is_active = False}
             t_f_c_2_base = IBase {place = (000, 10),
                                   size = (300, 30),
                                   action = action_none,
                                   draw = draw_text_field,
                                   process = process_none
                                  }

             slider_const_myu   = Slider {ibase = s_c_2_base,
                                          s_indent = (10, 3),
                                          s_min = 0,
                                          s_max = 50,
                                          s_pts = 51,
                                          s_curpt = 10,
                                          s_col = (makeColor 0.5 0.5 0.5 1.0),
                                          s_sl_size = 10,
                                          s_sl_col = (makeColor 0.9 0.9 0.9 1.0),
                                          s_m_act = False,
                                          is_active = False}
             s_c_2_base = IBase {place = (000, -20),
                                 size = (300, 30),
                                 action = action_multi [(action_set_world_const 2 ((* 0.03) . (get_slider_val_with_h (** 5) 13))),
                                                        (action_slider 13),
                                                        (action_change_text 12 (slider_to_text 13 (* 0.1) "Viscosity: %.1f"))],
                                 draw = draw_slider,
                                 process = process_none
                                }
----------------------------------------------------------------------------------
             text_field_const_sigma = TextField {ibase = t_f_c_3_base,
                                                 t_text = "Tension: 1.0",
                                                 t_col = black,
                                                 t_scale = 0.15,
                                                 is_active = False}
             t_f_c_3_base = IBase {place = (000, -60),
                                   size = (300, 30),
                                   action = action_none,
                                   draw = draw_text_field,
                                   process = process_none
                                  }

             slider_const_sigma = Slider {ibase = s_c_3_base,
                                          s_indent = (10, 3),
                                          s_min = 0,
                                          s_max = 50,
                                          s_pts = 51,
                                          s_curpt = 10,
                                          s_col = (makeColor 0.5 0.5 0.5 1.0),
                                          s_sl_size = 10,
                                          s_sl_col = (makeColor 0.9 0.9 0.9 1.0),
                                          s_m_act = False,
                                          is_active = False}
             s_c_3_base = IBase {place = (000, -90),
                                 size = (300, 30),
                                 action = action_multi [(action_set_world_const 3 ((* 1) . (get_slider_val_with_h (** 5) 15))),
                                                        (action_slider 15),
                                                        (action_change_text 14 (slider_to_text 15 (* 0.1) "Tension: %.1f"))],
                                 draw = draw_slider,
                                 process = process_none
                                }
----------------------------------------------------------------------------------
             text_field_const_g     = TextField {ibase = t_f_c_4_base,
                                                 t_text = "Gravity: 1.0",
                                                 t_col = black,
                                                 t_scale = 0.15,
                                                 is_active = False}
             t_f_c_4_base = IBase {place = (000, -130),
                                   size = (300, 30),
                                   action = action_none,
                                   draw = draw_text_field,
                                   process = process_none
                                  }

             slider_const_g     = Slider {ibase = s_c_4_base,
                                          s_indent = (10, 3),
                                          s_min = 0,
                                          s_max = 50,
                                          s_pts = 51,
                                          s_curpt = 10,
                                          s_col = (makeColor 0.5 0.5 0.5 1.0),
                                          s_sl_size = 10,
                                          s_sl_col = (makeColor 0.9 0.9 0.9 1.0),
                                          s_m_act = False,
                                          is_active = False}
             s_c_4_base = IBase {place = (000, -160),
                                 size = (300, 30),
                                 action = action_multi [(action_set_world_const 4 ((* 10) . (get_slider_val 17))),
                                                        (action_slider 17),
                                                        (action_change_text 16 (slider_to_text 17 (* 1) "Gravity: %.1f"))],
                                 draw = draw_slider,
                                 process = process_none
                                }
----------------------------------------------------------------------------------
             text_field_const_r     = TextField {ibase = t_f_c_5_base,
                                                 t_text = "Border reflection: 0.2",
                                                 t_col = black,
                                                 t_scale = 0.15,
                                                 is_active = False}
             t_f_c_5_base = IBase {place = (000, -200),
                                   size = (300, 30),
                                   action = action_none,
                                   draw = draw_text_field,
                                   process = process_none
                                  }

             slider_const_r     = Slider {ibase = s_c_5_base,
                                          s_indent = (10, 3),
                                          s_min = 0,
                                          s_max = 50,
                                          s_pts = 51,
                                          s_curpt = 10,
                                          s_col = (makeColor 0.5 0.5 0.5 1.0),
                                          s_sl_size = 10,
                                          s_sl_col = (makeColor 0.9 0.9 0.9 1.0),
                                          s_m_act = False,
                                          is_active = False}
             s_c_5_base = IBase {place = (000, -230),
                                 size = (300, 30),
                                 action = action_multi [(action_set_world_const 5 ((* 0.02) . (get_slider_val 19))),
                                                        (action_slider 19),
                                                        (action_change_text 18 (slider_to_text 19 (* 0.02) "Border reflection: %.2f"))],
                                 draw = draw_slider,
                                 process = process_none
                                }
------------------------------------------------------------------------------------------------------------------------------
             text_field_const_h     = TextField {ibase = t_f_c_6_base,
                                                 t_text = "Smooth radius: 10",
                                                 t_col = black,
                                                 t_scale = 0.15,
                                                 is_active = False}
             t_f_c_6_base = IBase {place = (000, -270),
                                   size = (300, 30),
                                   action = action_none,
                                   draw = draw_text_field,
                                   process = process_none
                                  }

             slider_const_h     = Slider {ibase = s_c_6_base,
                                          s_indent = (10, 3),
                                          s_min = 1,
                                          s_max = 50,
                                          s_pts = 50,
                                          s_curpt = 9,
                                          s_col = (makeColor 0.5 0.5 0.5 1.0),
                                          s_sl_size = 10,
                                          s_sl_col = (makeColor 0.9 0.9 0.9 1.0),
                                          s_m_act = False,
                                          is_active = False}
             s_c_6_base = IBase {place = (000, -300),
                                 size = (300, 30),
                                 action = action_multi [(action_set_world_h (get_slider_val 21)),
                                                        (action_slider 21),
                                                        (action_change_text 20 (slider_to_text 21 id "Smooth radius: %.0f"))],
                                 draw = draw_slider,
                                 process = process_none
                                 }
------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------   кнопка для расклеивания частиц   ------------------------------------------
             b_shk_base = IBase {place = (-400, -450), 
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
                                    b_time = 1.0,
                                    is_active = False}

------------------------------------------------------------------------------------------------------------------------------

             text_field_filename = TextField {ibase = t_f_fn_base,
                                              t_text = "Filename: world_1.txt",
                                              t_col = black,
                                              t_scale = 0.16,
                                              is_active = True}
             t_f_fn_base = IBase {place = (000, 130),
                                  size = (300, 40),
                                  action = action_none,
                                  draw = draw_text_field,
                                  process = process_none
                                 }

             slider_filename = Slider {ibase = s_fn_base,
                                       s_indent = (10, 5),
                                       s_min = 1,
                                       s_max = 30,
                                       s_pts = 30,
                                       s_curpt = 0,
                                       s_col = (makeColor 0.4 0.4 0.4 1.0),
                                       s_sl_size = 10,
                                       s_sl_col = (makeColor 0.8 0.8 0.8 1.0),
                                       s_m_act = False,
                                       is_active = True}
             s_fn_base = IBase {place = (000, 90),
                               size = (300, 40),
                               action = action_multi [(action_set_world_file ((printf "worlds/world_%02.0f.txt") . (get_slider_val 23))),
                                                      (action_slider 23),
                                                      (action_change_text 22 (slider_to_text 23 id "Filename: world_%02.0f.txt"))],
                                                      -- 4 - текстовое поле, которое зависит от 5-слайдера
                               draw = draw_slider,
                               process = process_none
                              }

------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------  переключения между "страницами" интерфейса --------------------------------------
             b_p_1_base = IBase {place = (400, 350), 
                                 size = (150, 50),
                                 action = (action_click b_p_1_base (action_multi [
                                           action_set_page page_save_load,
                                           action_set_buttons_time 0.0 [25],
                                           action_set_buttons_time 1.0 [26]
                                           ])),
                                 draw = draw_button,
                                 process = process_none
                                 }
             button_page_1 = Button {ibase = b_p_1_base,
                                     b_text = "Save/Load",
                                     b_col = (makeColor 0.5 0.5 0.5 1.0),
                                     b_click_col = (makeColor 0.1 0.7 0.1 1.0),
                                     b_text_col = black,
                                     b_time = 0.0,
                                     is_active = True}

             b_p_2_base = IBase {place = (400, 300), 
                                 size = (150, 50),
                                 action = (action_click b_p_2_base (action_multi [
                                           action_set_page page_physics,
                                           action_set_buttons_time 0.0 [26],
                                           action_set_buttons_time 1.0 [25]
                                           ])),
                                 draw = draw_button,
                                 process = process_none
                                 }
             button_page_2 = Button {ibase = b_p_2_base,
                                     b_text = "Physics",
                                     b_col = (makeColor 0.5 0.5 0.5 1.0),
                                     b_click_col = (makeColor 0.1 0.7 0.1 1.0),
                                     b_text_col = black,
                                     b_time = 1.0,
                                     is_active = True}

