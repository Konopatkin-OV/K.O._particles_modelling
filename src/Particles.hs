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
page_save_load = (take 9 inf_true) ++ (take 18 inf_false) ++ [True, True, False] ++ inf_false

page_physics :: [Bool]
page_physics = (take 6 inf_true) ++ (take 3 inf_false) ++ (take 18 inf_true) ++ inf_false

page_editor :: [Bool]
page_editor = (take 6 inf_true) ++ (take 3 inf_false) ++ (take 4 inf_true) ++ (take 16 inf_false) ++ (take 7 inf_true) ++ inf_false

page_world :: [Bool]
page_world = (take 6 inf_true) ++ (take 3 inf_false) ++ (take 4 inf_true) ++ (take 16 inf_false) ++ inf_false

-- слайдеры живут по индексам [9, 11, 13, 15, 17, 19, 21]
run :: IO ()
run = do app <- load_world "worlds/world_01.txt" init_app
         playIO display (makeColor 0.9 0.9 0.9 1.0) const_FPS app app_draw app_handle_events app_process 
           where
             display = (InWindow "Particles" (1200, 900) (0, 0))
             init_app = App {elems = [init_world
                                 , button_page_1, button_page_2, button_page_3, button_page_4
                                 , button_menu_bckg, -- костыльная рамочка для страницы
                                 button_load, button_save, button_reload, 
                                 text_field_time, slider_time, button_go, button_pause,
                                 text_field_const_k, slider_const_k, text_field_const_p_0, slider_const_p_0,
                                 text_field_const_myu, slider_const_myu, text_field_const_sigma, slider_const_sigma,
                                 text_field_const_g, slider_const_g, text_field_const_r, slider_const_r,
                                 text_field_const_h, slider_const_h,
                                 text_field_filename, slider_filename,
                                 button_spawn, button_erase,
                                 text_field_edit_r, slider_edit_r,
                                 text_field_p_param,
                                 text_field_edit_m, slider_edit_m



                                 , button_shake -- костыльное расклеивание частиц
                                ], 
                        app_scale = 1.0, 
                        base_size = (1200, 900),
                        mouse_pos = (0, 0)}
             init_world = World {ibase = IBase {place = (-550, -400),
                                                size = (500, 800),
                                                action = action_edit,            --------------------------------------------
                                                draw = draw_world,               --------------------------------------------
                                                process = process_world          --------------------------------------------
                                               }
                               , h_smooth = 10
                               , entities = []
                               , back_col = makeColor 0.5 0.5 0.5 1.0
                               , time_speed = 1.0
                               , is_pause = True
                               , constants = base_consts
                               , filename = "world_01.txt"
                               , is_active = True

                               , edit_radius = 10.0
                               , new_particle = Particle {
                                              e_pos = (0, 0),
                                              e_speed = (0, 0),
                                              e_mass = 1,
                                              e_dense = 1.0, -- само пересчитывается
                                              e_radius = 20,
                                              e_color = makeColor 0.0 0.0 0.6 1.0,
                                              e_id = 0
                                              }
                                , action_type = 1
                                , m_act = False
                                , m_pos = (0, 0)
                              }
-----------------------------------------------------   кнопки загрузки/сохранения   ------------------------------------------
             b_l_base = IBase {place = (50, 240), 
                               size = (300, 50),
                               action = (action_click b_l_base (action_button_click 6 action_load_world)),  -- "6" - индекс кнопки в списке элементов интерфейса
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

             b_s_base = IBase {place = (50, 180), 
                               size = (300, 50),
                               action = (action_click b_s_base (action_button_click 7 action_save_world)),  -- "7" - индекс кнопки в списке элементов интерфейса
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

             b_r_base = IBase {place = (50, 320), 
                               size = (300, 50),
                               action = (action_click b_r_base (action_button_click 8 action_clear_world)),  -- "8" - индекс кнопки в списке элементов интерфейса
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
                                          t_col = black,
                                          t_scale = 0.15,
                                          is_active = False}
             t_f_t_base = IBase {place = (50, 355),
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
             s_t_base = IBase {place = (50, 310),
                               size = (300, 50),
                               action = action_multi [(action_change_time_text 9 (slider_to_text 10 id "Time speed: %.1f")),
                                                      (action_set_world_time_speed (get_slider_val 10)),
                                                      (action_slider 10)],
                                                      -- 9 - текстовое поле, которое зависит от 10-слайдера
                               draw = draw_slider,
                               process = process_none
                              }
-----------------------------------------------------   кнопки старт/пауза   -------------------------------------------------
             b_go_base = IBase {place = (50, 245), 
                                size = (140, 50),
                                action = (action_click b_go_base (action_button_click 11 (action_set_world_pause False))),
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

             b_pause_base = IBase {place = (210, 245), 
                                   size = (140, 50),
                                   action = (action_click b_pause_base (action_button_click 12 (action_set_world_pause True))),
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
             t_f_c_0_base = IBase {place = (50, 150),
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
             s_c_0_base = IBase {place = (50, 120),
                                 size = (300, 30),
                                 action = action_multi [(action_set_world_const 0 ((* 1) . (get_slider_val_with_h (** 5) 14))),
                                                        (action_change_text 13 (slider_to_text 14 (* 0.1) "Pressure: %.1f")),
                                                        (action_slider 14)],
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
             t_f_c_1_base = IBase {place = (50, 80),
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
             s_c_1_base = IBase {place = (50, 50),
                                 size = (300, 30),
                                 action = action_multi [(action_set_world_const 1 ((* 0.1) . (get_slider_val 16))),
                                                        (action_change_text 15 (slider_to_text 16 (* 0.1) "Environment density: %.1f")),
                                                        (action_slider 16)],
                                 draw = draw_slider,
                                 process = process_none
                                 }
----------------------------------------------------------------------------------
             text_field_const_myu   = TextField {ibase = t_f_c_2_base,
                                                 t_text = "Viscosity: 1.0",
                                                 t_col = black,
                                                 t_scale = 0.15,
                                                 is_active = False}
             t_f_c_2_base = IBase {place = (50, 10),
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
             s_c_2_base = IBase {place = (50, -20),
                                 size = (300, 30),
                                 action = action_multi [(action_set_world_const 2 ((* 0.03) . (get_slider_val_with_h (** 5) 18))),
                                                        (action_change_text 17 (slider_to_text 18 (* 0.1) "Viscosity: %.1f")),
                                                        (action_slider 18)],
                                 draw = draw_slider,
                                 process = process_none
                                }
----------------------------------------------------------------------------------
             text_field_const_sigma = TextField {ibase = t_f_c_3_base,
                                                 t_text = "Tension: 1.0",
                                                 t_col = black,
                                                 t_scale = 0.15,
                                                 is_active = False}
             t_f_c_3_base = IBase {place = (50, -60),
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
             s_c_3_base = IBase {place = (50, -90),
                                 size = (300, 30),
                                 action = action_multi [(action_set_world_const 3 ((* 1) . (get_slider_val_with_h (** 5) 20))),
                                                        (action_change_text 19 (slider_to_text 20 (* 0.1) "Tension: %.1f")),
                                                        (action_slider 20)],
                                 draw = draw_slider,
                                 process = process_none
                                }
----------------------------------------------------------------------------------
             text_field_const_g     = TextField {ibase = t_f_c_4_base,
                                                 t_text = "Gravity: 1.0",
                                                 t_col = black,
                                                 t_scale = 0.15,
                                                 is_active = False}
             t_f_c_4_base = IBase {place = (50, -130),
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
             s_c_4_base = IBase {place = (50, -160),
                                 size = (300, 30),
                                 action = action_multi [(action_set_world_const 4 ((* 10) . (get_slider_val 22))),
                                                        (action_change_text 21 (slider_to_text 22 (* 1) "Gravity: %.1f")),
                                                        (action_slider 22)],
                                 draw = draw_slider,
                                 process = process_none
                                }
----------------------------------------------------------------------------------
             text_field_const_r     = TextField {ibase = t_f_c_5_base,
                                                 t_text = "Border reflection: 0.2",
                                                 t_col = black,
                                                 t_scale = 0.15,
                                                 is_active = False}
             t_f_c_5_base = IBase {place = (50, -200),
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
             s_c_5_base = IBase {place = (50, -230),
                                 size = (300, 30),
                                 action = action_multi [(action_set_world_const 5 ((* 0.02) . (get_slider_val 24))),
                                                        (action_change_text 23 (slider_to_text 24 (* 0.02) "Border reflection: %.2f")),
                                                        (action_slider 24)],
                                 draw = draw_slider,
                                 process = process_none
                                }
------------------------------------------------------------------------------------------------------------------------------
             text_field_const_h     = TextField {ibase = t_f_c_6_base,
                                                 t_text = "Smooth radius: 10",
                                                 t_col = black,
                                                 t_scale = 0.15,
                                                 is_active = False}
             t_f_c_6_base = IBase {place = (50, -270),
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
             s_c_6_base = IBase {place = (50, -300),
                                 size = (300, 30),
                                 action = action_multi [(action_set_world_h (get_slider_val 26)),
                                                        (action_change_text 25 (slider_to_text 26 id "Smooth radius: %.0f")),
                                                        (action_slider 26)],
                                 draw = draw_slider,
                                 process = process_none
                                 }
------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------   кнопка для расклеивания частиц   ------------------------------------------
             b_shk_base = IBase {place = (-400, -450), 
                                 size = (300, 50),
                                 action = (action_click b_shk_base (action_button_click 27 (action_shake_world))),
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
                                              t_text = "Filename: world_01.txt",
                                              t_col = black,
                                              t_scale = 0.16,
                                              is_active = True}
             t_f_fn_base = IBase {place = (50, 130),
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
             s_fn_base = IBase {place = (50, 90),
                               size = (300, 40),
                               action = action_multi [(action_set_world_file ((printf "worlds/world_%02.0f.txt") . (get_slider_val 28))),
                                                      (action_change_text 27 (slider_to_text 28 id "Filename: world_%02.0f.txt")),
                                                      (action_slider 28)],
                                                      -- 4 - текстовое поле, которое зависит от 5-слайдера
                               draw = draw_slider,
                               process = process_none
                              }

------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------  редактор частиц --------------------------------------------------

             b_spawn_base = IBase {place = (50, 65), 
                                   size = (140, 50),
                                   action = (action_click b_spawn_base (action_multi [
                                             action_set_world_action (\_ -> 1),
                                             action_set_buttons_time 0.0 [29],
                                             action_set_buttons_time 1.0 [30]
                                             ])),
                                   draw = draw_button,
                                   process = process_none
                                }
             button_spawn = Button {ibase = b_spawn_base,
                                    b_text = "Spawn",
                                    b_col = (makeColor 0.5 0.5 0.5 1.0),
                                    b_click_col = (makeColor 0.1 0.7 0.1 1.0),
                                    b_text_col = black,
                                    b_time = 0.0,
                                    is_active = False}

             b_erase_base = IBase {place = (210, 65), 
                                   size = (140, 50),
                                   action = (action_click b_erase_base (action_multi [
                                             action_set_world_action (\_ -> 0),
                                             action_set_buttons_time 0.0 [30],
                                             action_set_buttons_time 1.0 [29]
                                             ])),
                                   draw = draw_button,
                                   process = process_none
                                   }
             button_erase = Button {ibase = b_erase_base,
                                    b_text = "Erase",
                                    b_col = (makeColor 0.5 0.5 0.5 1.0),
                                    b_click_col = (makeColor 0.1 0.7 0.1 1.0),
                                    b_text_col = black,
                                    b_time = 1.0,
                                    is_active = False}

---------------------------------------------------------------------------------
             text_field_edit_r     = TextField {ibase = t_f_e_r_base,
                                                t_text = "Brush radius: 10",
                                                t_col = black,
                                                t_scale = 0.15,
                                                is_active = False}
             t_f_e_r_base = IBase {place = (50, 170),
                                   size = (300, 30),
                                   action = action_none,
                                   draw = draw_text_field,
                                   process = process_none
                                  }

             slider_edit_r     = Slider {ibase = s_e_r_base,
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
             s_e_r_base = IBase {place = (50, 130),
                                 size = (300, 40),
                                 action = action_multi [(action_set_world_edit_rad ((* 1) . (get_slider_val 32))),
                                                        (action_change_text 31 (slider_to_text 32 (* 1) "Brush radius: %.0f")),
                                                        (action_slider 32)],
                                 draw = draw_slider,
                                 process = process_none
                                }
---------------------------------------------------------------------------------
             text_field_p_param    = TextField {ibase = t_f_p_p_base,
                                                t_text = "Particle parametres:",
                                                t_col = black,
                                                t_scale = 0.18,
                                                is_active = False}
             t_f_p_p_base = IBase {place = (50, 10),
                                   size = (300, 30),
                                   action = action_none,
                                   draw = draw_text_field,
                                   process = process_none
                                  }

             text_field_edit_m     = TextField {ibase = t_f_e_m_base,
                                                t_text = "Mass: 1.0",
                                                t_col = black,
                                                t_scale = 0.15,
                                                is_active = False}
             t_f_e_m_base = IBase {place = (50, -40),
                                   size = (100, 30),
                                   action = action_none,
                                   draw = draw_text_field,
                                   process = process_none
                                  }

             slider_edit_m     = Slider {ibase = s_e_m_base,
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
             s_e_m_base = IBase {place = (160, -40),
                                 size = (140, 40),
                                 action = action_multi [(action_change_world_spawn_particle 
                                          (\app ent -> (ent {e_mass = (0.1 * (get_slider_val 35 app))}))),
                                                        (action_change_text 34 (slider_to_text 35 (* 0.1) "Mass: %.1f")),
                                                        (action_slider 35)],
                                 draw = draw_slider,
                                 process = process_none
                                }

------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------  переключения между "страницами" интерфейса --------------------------------------
             b_p_1_base = IBase {place = (400, 350), 
                                 size = (150, 50),
                                 action = (action_click b_p_1_base (action_multi [
                                           action_set_page page_save_load,
                                           action_set_buttons_time 0.0 [1],
                                           action_set_buttons_time 1.0 [2, 3, 4]
                                           ])),
                                 draw = draw_button,
                                 process = process_none
                                 }
             button_page_1 = Button {ibase = b_p_1_base,
                                     b_text = "Save/Load",
                                     b_col = (makeColor 0.5 0.5 0.5 1.0),
                                     b_click_col = (makeColor 0.1 0.5 0.1 1.0),
                                     b_text_col = black,
                                     b_time = 0.0,
                                     is_active = True}

             b_p_2_base = IBase {place = (400, 290), 
                                 size = (150, 50),
                                 action = (action_click b_p_2_base (action_multi [
                                           action_set_page page_physics,
                                           action_set_buttons_time 0.0 [2],
                                           action_set_buttons_time 1.0 [1, 3, 4]
                                           ])),
                                 draw = draw_button,
                                 process = process_none
                                 }
             button_page_2 = Button {ibase = b_p_2_base,
                                     b_text = "Physics",
                                     b_col = (makeColor 0.5 0.5 0.5 1.0),
                                     b_click_col = (makeColor 0.1 0.5 0.1 1.0),
                                     b_text_col = black,
                                     b_time = 1.0,
                                     is_active = True}

             b_p_3_base = IBase {place = (400, 230), 
                                 size = (150, 50),
                                 action = (action_click b_p_3_base (action_multi [
                                           action_set_page page_editor,
                                           action_set_buttons_time 0.0 [3],
                                           action_set_buttons_time 1.0 [1, 2, 4]
                                           ])),
                                 draw = draw_button,
                                 process = process_none
                                 }
             button_page_3 = Button {ibase = b_p_3_base,
                                     b_text = "Editor",
                                     b_col = (makeColor 0.5 0.5 0.5 1.0),
                                     b_click_col = (makeColor 0.1 0.5 0.1 1.0),
                                     b_text_col = black,
                                     b_time = 1.0,
                                     is_active = True}

             b_p_4_base = IBase {place = (400, 170), 
                                 size = (150, 50),
                                 action = (action_click b_p_4_base (action_multi [
                                           action_set_page page_world,
                                           action_set_buttons_time 0.0 [4],
                                           action_set_buttons_time 1.0 [1, 2, 3]
                                           ])),
                                 draw = draw_button,
                                 process = process_none
                                 }
             button_page_4 = Button {ibase = b_p_4_base,
                                     b_text = "World",
                                     b_col = (makeColor 0.5 0.5 0.5 1.0),
                                     b_click_col = (makeColor 0.1 0.5 0.1 1.0),
                                     b_text_col = black,
                                     b_time = 1.0,
                                     is_active = True}
---------------------------------------------------------------------------------
             b_bckg = IBase {place = (0, -400), 
                             size = (403, 800),
                             action = action_none,
                             draw = draw_button,
                             process = process_none
                             }
             button_menu_bckg = Button {ibase = b_bckg,
                                        b_text = "",
                                        b_col = (makeColor 0.85 0.85 0.85 1.0),
                                        b_click_col = (makeColor 0.1 0.5 0.1 1.0),
                                        b_text_col = (makeColor 0.1 0.5 0.1 1.0),
                                        b_time = 0.0,
                                        is_active = True}


