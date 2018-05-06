module Physics where

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Debug.Trace

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

-- быстрее + неадекватнее
-- max_vic :: Int
-- max_vic = 1000

-- минимальная диагональ клетки дерева относитально длины сглаживания
const_tree_size :: Float
const_tree_size = 0.1

-- дерево квадрантов для поиска соседей частиц
-- порядок квадрантов: верхний левый, верхный правый, 
--                     нижний левый,  нижний правый
data QuadTree = QNode QuadTree QuadTree QuadTree QuadTree | QLeaf Entity | QEmpty

-- принимает левый нижний угол дерева и верхний правый угол дерева, расстояние сглаживания,
-- новую частицу и дерево, в которое она добавляется
qtr_insert :: (Point, Point) -> Entity -> QuadTree -> QuadTree
qtr_insert _ p_new QEmpty = (QLeaf p_new)
qtr_insert coords p_new (QLeaf p) = new_tree
  where
    ((x_l, y_d), (x_r, y_u)) = coords
    x_m = (x_l + x_r) / 2
    y_m = (y_d + y_u) / 2
    --new_tree = if (dist (x_l, y_d) (x_r, y_u)) < h 
               --then (QLeaf (p_new : p))
               --else split_tree
    new_tree = QNode ul ur dl dr

    old_pos = (e_pos p)
    new_pos = (e_pos p_new)
    -- нашли квадрант со старой точкой
    ul_rect = ((x_l, y_m), (x_m, y_u))
    ur_rect = ((x_m, y_m), (x_r, y_u))
    dl_rect = ((x_l, y_d), (x_m, y_m))
    dr_rect = ((x_m, y_d), (x_r, y_m))

    ul_0 = if (pointInRect ul_rect old_pos) then (QLeaf p) else QEmpty
    ur_0 = if (pointInRect ur_rect old_pos) then (QLeaf p) else QEmpty
    dl_0 = if (pointInRect dl_rect old_pos) then (QLeaf p) else QEmpty
    dr_0 = if (pointInRect dr_rect old_pos) then (QLeaf p) else QEmpty
    -- рекурсивно добавляем новую точку
    ul = if (pointInRect ul_rect new_pos) 
    then qtr_insert ul_rect p_new ul_0 else ul_0

    ur = if (pointInRect ur_rect new_pos)
    then qtr_insert ur_rect p_new ur_0 else ur_0

    dl = if (pointInRect dl_rect new_pos)
    then qtr_insert dl_rect p_new dl_0 else dl_0

    dr = if (pointInRect dr_rect new_pos)
    then qtr_insert dr_rect p_new dr_0 else dr_0

qtr_insert coords p_new (QNode ul ur dl dr) = new_tree 
  where
    ((x_l, y_d), (x_r, y_u)) = coords
    x_m = (x_l + x_r) / 2
    y_m = (y_d + y_u) / 2
    pos = (e_pos p_new)

    ul_rect = ((x_l, y_m), (x_m, y_u))
    ur_rect = ((x_m, y_m), (x_r, y_u))
    dl_rect = ((x_l, y_d), (x_m, y_m))
    dr_rect = ((x_m, y_d), (x_r, y_m))

    new_tree =  if (pointInRect ul_rect pos) then (QNode upd_ul ur dl dr) else (
                if (pointInRect ur_rect pos) then (QNode ul upd_ur dl dr) else (
                if (pointInRect dl_rect pos) then (QNode ul ur upd_dl dr) else (
                if (pointInRect dr_rect pos) then (QNode ul ur dl upd_dr) else 
                (QNode ul ur dl dr))))
    upd_ul = (qtr_insert ul_rect p_new ul)
    upd_ur = (qtr_insert ur_rect p_new ur)
    upd_dl = (qtr_insert dl_rect p_new dl)
    upd_dr = (qtr_insert dr_rect p_new dr)


-- получить список из всех частиц в дереве
qtr_get_all :: QuadTree -> [Entity] -> [Entity]
qtr_get_all QEmpty res = res
qtr_get_all (QLeaf p) res = (p : res)
qtr_get_all (QNode ul ur dl dr) res = (qtr_get_all ul
                                      (qtr_get_all ur
                                      (qtr_get_all dl
                                      (qtr_get_all dr res))))

-- принимает левый нижний угол дерева и верхний правый угол дерева, дерево, расстояние сглаживания,
-- и точку, окрестность которой нужно найти
qtr_get_vicinity :: (Point, Point) -> QuadTree -> Float -> [Entity] -> Point -> [Entity]
qtr_get_vicinity _ QEmpty _ res _ = res
qtr_get_vicinity coords (QLeaf p) h res pos | dist pos (e_pos p) < h = (p : res)
                                            | otherwise = res

qtr_get_vicinity coords (QNode ul ur dl dr) h res pos | d_min > h = res
                                                      | d_max < h = qtr_get_all (QNode ul ur dl dr) res
                                                      | otherwise = new_res
  where
    (d_min, d_max) = distToRect coords pos

    ((x_l, y_d), (x_r, y_u)) = coords
    x_m = (x_l + x_r) / 2
    y_m = (y_d + y_u) / 2
    new_res = --take max_vic (
          (qtr_get_vicinity ((x_l, y_m), (x_m, y_u)) ul h
          (qtr_get_vicinity ((x_m, y_m), (x_r, y_u)) ur h
          (qtr_get_vicinity ((x_l, y_d), (x_m, y_m)) dl h
          (qtr_get_vicinity ((x_m, y_d), (x_r, y_m)) dr h res
           pos) pos) pos) pos)
          -- )

-------------------------------------------------
-------------------------------------------------
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

get_vicinity :: Interface -> Float -> Point -> [Entity]
get_vicinity world r pos = (filter check (entities world))
            --take max_vic (filter check (entities world))
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
p_dense h c (Particle pos _ m _ _ _ _) = -- if ((dist pos c) > h) then 0.0 else 
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

pointInRect :: (Point, Point) -> Point -> Bool
pointInRect ((x_l, y_d), (x_r, y_u)) (x, y) = (x_l <= x) && (x < x_r) && (y_d <= y) && (y < y_u)


dist :: Point -> Point -> Float
dist (x1, y1) (x2, y2) = ((x1 - x2) ** 2 + (y1 - y2) ** 2) ** 0.5

-- возвращает пару: минимальное расстояние до точки прямоугольника,
--                 максимальное расстояние до точки прямоугольника                 
distToRect :: (Point, Point) -> Point -> (Float, Float)
distToRect ((x_l, y_d), (x_r, y_u)) (x, y) = (min_dist, max_dist)
  where max_dist = maximum [dist (x_l, y_d) (x, y),
                            dist (x_l, y_u) (x, y),
                            dist (x_r, y_d) (x, y),
                            dist (x_r, y_u) (x, y)]
        min_dist = if (x < x_l) then
                     (if (y < y_d) then (dist (x_l, y_d) (x, y)) else (
                      if (y < y_u) then (x_l - x) else (
                                        (dist (x_l, y_u) (x, y)))))
                   else (if (x < x_r) then
                     (if (y < y_d) then (y_d - y) else (
                      if (y < y_u) then 0.0 else (
                                        (y - y_u))))
                   else
                     (if (y < y_d) then (dist (x_r, y_d) (x, y)) else (
                      if (y < y_u) then (x - x_r) else (
                                        (dist (x_r, y_u) (x, y))))))

normalize :: Vector -> Vector
normalize vec | (magV vec == 0) = vec
              | otherwise = mulSV (1.0 / magV vec) vec

------------------------------------------------------------------------
