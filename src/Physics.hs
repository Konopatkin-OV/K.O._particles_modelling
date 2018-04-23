module Physics where

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

import BaseClasses

-------------------------------------------------
-- параметры жидкости
-- TODO: возможно приклеить к отдельным частицам
-- здоровенные числа напрягают

base_consts :: [Float]
base_consts = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
--const_k :: Float      -- жёсткость (сжимаемость)       :: constants[0]
--const_k = 5.0 * 1000000000.0

--const_p_0 :: Float    -- плотность окружающей среды    :: constants[1]
--const_p_0 = 0.0

--const_myu :: Float    -- вязкость                      :: constants[2]
--const_myu = 0.0 * 500000000.0

--const_sigma :: Float  -- поверхностное натяжение       :: constants[3]
--const_sigma = 4.0 * 100000000.0

--const_g :: Float      -- ускорение свободного падения  :: constants[4]
--const_g = 0.3 * 500.0

--const_r :: Float      -- отражение от границ мира      :: constants[5]
--const_r = 0.1         
-------------------------------------------------

-- размер частицы -> константа отражения -> размер мира -> (позиция, скорость)
bound_bounce :: Float -> Float -> Vector -> (Point, Vector) -> (Point, Vector)
bound_bounce r const_r size_ ((x, y), (vx, vy)) = ((new_x, new_y), (new_vx, new_vy))
  where
    (wx, wy) = size_
    new_x = if x - r < 0 then (r + const_r * (r - x)) else (if x + r > wx then (wx - r - const_r * (x + r - wx)) else x)
    new_y = if y - r < 0 then (r + const_r * (r - y)) else (if y + r > wy then (wy - r - const_r * (y + r - wy)) else y)
    new_vx = if (x - r < 0 || x + r > wx) then (-const_r * vx) else vx
    new_vy = if (y - r < 0 || y + r > wy) then (-const_r * vy) else vy

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
  where
    c = (constants world)
    const_k     = c !! 0
    const_myu   = c !! 2
    const_sigma = c !! 3


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
  where 
    func = (\ent -> ((p_i + (e_dense ent)) / 2 - const_p_0))
    const_p_0 = (constants world) !! 1

f_viscosity :: Vector -> Interface -> Point -> Vector
f_viscosity v_i world pos = (get_value_v func ker_nab2_poly6 world pos)
  where func = (\ent -> ((e_speed ent) - v_i))

f_tension :: Interface -> Point -> Vector
f_tension world pos = (get_value (\_ -> 1.0) ker_nab2_poly6 world pos)

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
