module Physics where

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

import BaseClasses

-------------------------------------------------
-- параметры жидкости
-- TODO: возможно их надо приклеить к миру или к отдельным частицам
-- здоровенные числа напрягают
const_k :: Float      -- жёсткость (сжимаемость)
const_k = 5.0 * 1000000000.0

const_p_0 :: Float    -- плотность окружающей среды
const_p_0 = 0.0

const_myu :: Float    -- вязкость
const_myu = 0.0 * 500000000.0

const_sigma :: Float  -- поверхностное натяжение
const_sigma = 4.0 * 100000000.0

const_g :: Float      -- ускорение свободного падения
const_g = 0.3 * 500.0

const_r :: Float
const_r = 0.1         -- отражение от границ мира
-------------------------------------------------

bound_bounce :: Vector -> (Point, Vector) -> (Point, Vector)
bound_bounce size_ ((x, y), (vx, vy)) = ((new_x, new_y), (new_vx, new_vy))
  where
    (wx, wy) = size_
    new_x = if x < 0 then (-const_r * x) else (if x > wx then (wx - const_r * (wx - x)) else x)
    new_y = if y < 0 then (-const_r * y) else (if y > wy then (wy - const_r * (wy - y)) else y)
    new_vx = if (x < 0 || x > wx) then (-const_r * vx) else vx
    new_vy = if (y < 0 || y > wy) then (-const_r * vy) else vy

get_vicinity :: Float -> Point -> Interface -> Interface
get_vicinity r pos world = world {entities = filter check (entities world)
                                  } -- TODO: сделать длину сглаживания зависящей от радиуса частицы
  where
    check = (\ent -> (dist (e_pos ent) pos) <= r)

------------------------------- физика ---------------------------------
------------------------------------------------------------------------

use_force :: Float -> Vector -> Interface -> Point -> Vector
use_force p_i v_i world pos = ((mulSV const_k (f_pressure p_i world pos)) +
                               (mulSV const_myu (f_viscosity v_i world pos)) + 
                               (mulSV const_sigma (f_tension world pos)))


density :: Interface -> Point -> Float
density world pos = sum (map (p_dense (h_smooth world) pos) (entities world)) -- + const_p_0

p_dense :: Float -> Point -> Entity -> Float
p_dense h c (Particle pos _ m _ _ _) = -- if ((dist pos c) > h) then 0.0 else 
                                       m * (ker_density (dist pos c) h)
-- p_dense _ _ _ = 0


get_value :: (Entity -> Float) -> (Float -> Float -> Float) -> Interface -> Point -> Vector
get_value func kernel world pos = sum (map map_f (entities world))
  where map_f = (\ent -> -- if ((dist pos (e_pos ent)) > (h_smooth world)) then (0.0, 0.0) else 
                         (mulSV ((func ent) * (e_mass ent) / (e_dense ent) *
                                (kernel (dist pos (e_pos ent)) (h_smooth world))) 
                         (normalize ((e_pos ent) - pos))))
--get_value _ _ _ _ = 0

get_value_v :: (Entity -> Vector) -> (Float -> Float -> Float) -> Interface -> Point -> Vector
get_value_v func kernel world pos = sum (map map_f (entities world))
  where map_f = (\ent -> -- if ((dist pos (e_pos ent)) > (h_smooth world)) then (0.0, 0.0) else 
                         (mulSV ((e_mass ent) / (e_dense ent) *
                               (kernel (dist pos (e_pos ent)) (h_smooth world))) 
                         (func ent)))
--get_value_v _ _ _ _ = 0



f_pressure :: Float -> Interface -> Point -> Vector
f_pressure p_i world pos = (get_value func ker_nab_poly6 world pos)
  where func = (\ent -> ((p_i + (e_dense ent)) / 2 - const_p_0))

f_viscosity :: Vector -> Interface -> Point -> Vector
f_viscosity v_i world pos = (get_value_v func ker_nab2_poly6 world pos)
  where func = (\ent -> ((e_speed ent) - v_i))

f_tension :: Interface -> Point -> Vector
f_tension world pos = (get_value (\_ -> 1.0) ker_poly6 world pos)

-- магические ядра --
-- здесь мог бы быть поясняющий комментарий, но я не шарю >_< --
ker_poly6 :: Float -> Float -> Float
ker_poly6 r h | (0 <= r && r <= h) = (315 / 64 / pi) * ((h^2 - r^2) ^ 3) / (h^9)
              | otherwise = 0

ker_nab_poly6 :: Float -> Float -> Float
ker_nab_poly6 r h | (0 <= r && r <= h) = (-315 / 64 / pi) * (6 * r * ((h^2 - r^2) ^ 2)) / (h^9)
                  | otherwise = 0

ker_nab2_poly6 :: Float -> Float -> Float
ker_nab2_poly6 r h | (0 <= r && r <= h) = (315 / 64 / pi) * (6 * (h^2 - r^2) * (5 * r^2 - h^2)) / (h^9)
  --(315 / 64 / pi) * (6 * (h^2 - r^2) * (4 * r^2 - (h^2 - r^2))) / (h^9)
                   | otherwise = 0

ker_density :: Float -> Float -> Float
ker_density r h | (0 <= r && r <= h) = (1 - r / h) ^ 2
                | otherwise = 0

------------------------------------------------------------------------

dist :: Point -> Point -> Float
dist (x1, y1) (x2, y2) = ((x1 - x2) ** 2 + (y1 - y2) ** 2) ** 0.5

normalize :: Vector -> Vector
normalize vec | (magV vec == 0) = vec
              | otherwise = mulSV (1.0 / magV vec) vec

------------------------------------------------------------------------
