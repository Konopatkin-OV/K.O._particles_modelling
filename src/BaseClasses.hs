module BaseClasses where

import Graphics.Gloss.Interface.IO.Game

const_FPS :: Int
const_FPS = 30

data Application = App             -- объект окна с приложением
  { elems :: [Interface]           -- интерактивные и не очень элементы интерфейса
  , app_scale :: Float                 -- масштаб окна
  , base_size :: (Int, Int)        -- базовый размер окна (для масштабирования)
  , mouse_pos :: (Float, Float)}   -- положение указателя
  deriving Show

-- "базовая часть" элементов интерфейса
-- считаем, что элемент интерфейса - прямоугольник, параллельный осям координат, в который можно кликать мышью
data BaseInterface = IBase
  { place   :: Point                                      -- положение в окне (левый нижний угол)
  , size    :: Vector                                     -- направление на правый верхний угол прямоугольника
  , action  :: (Event -> Application -> IO Application)   -- действие при активации
  , draw    :: (Interface -> Picture)                     -- функция отрисовки
  , process :: (Float -> Interface -> Interface)}         -- функция обработки времени

instance Show BaseInterface where
    show (IBase p s _ _ _) = show ("IBase: ", p, s)

data Interface = Button              -- кнопка
  { ibase       :: BaseInterface     -- базовая часть
  , b_text      :: String            -- надпись на кнопке
  , b_col       :: Color             -- цвет кнопки
  , b_click_col :: Color             -- цвет после клика
  , b_text_col  :: Color             -- цвет текста
  , b_time      :: Float             -- время после последнего клика
  }
  | TextField                        -- поле с текстом
  { ibase       :: BaseInterface
  , t_text      :: String            -- текст
  , t_col       :: Color             -- цвет текста
  , t_scale     :: Float             -- масштаб текста
  }
  | Slider                           
  { ibase       :: BaseInterface     
  , s_indent    :: Vector            -- отступ по краям (для отрисовки)
  , s_min       :: Float             -- минимальное значение
  , s_max       :: Float             -- максимальное значение
  , s_pts       :: Int               -- число делений
  , s_curpt     :: Int               -- текущее деление (нумерация с 0)
--
  , s_col       :: Color             -- цвет поля
  , s_sl_size   :: Float             -- ширина указателя
  , s_sl_col    :: Color             -- цвет указателя
  , s_m_act     :: Bool              -- захват мышью
  }
  | World                            -- интерактивный симулируемый мир
  { ibase       :: BaseInterface     -- базовая часть
  , h_smooth    :: Float             -- константа h - длина сглаживания
  , entities    :: [Entity]          -- сущности в игровом мире
  , back_col    :: Color             -- цвет фона
  , time_speed  :: Float             -- коэффициент времени
  , is_pause    :: Bool              -- пауза
  , constants   :: [Float]           -- волшебные константы для физики
  -- 0: жёсткость (сжимаемость)      ~ k
  -- 1: плотность окружающей среды   ~ p_0
  -- 2: вязкость                     ~ myu
  -- 3: поверхностное натяжение      ~ sigma
  -- 4: ускорение свободного падения ~ g
  -- 5: отражение от границ мира     ~ r
  }
  deriving Show

-- текущее значения для Slider-а
-- min + (max - min) * curpt / pts
s_value :: Interface -> Float
s_value s = (s_min s) + ((s_max s) - (s_min s)) * ((fromIntegral (s_curpt s)) / ((fromIntegral (s_pts s)) - 1))

data Entity = Particle           -- частица
  { e_pos    :: Point            -- положение в мире
  , e_speed  :: Vector           -- скорость
  , e_mass   :: Float            -- масса
  , e_dense  :: Float            -- плотность, пересчитывается автоматически
  , e_radius :: Float            -- размер частицы (для рисования/столкновений)
                                 -- так получилось, что считаем его h / 2 везде...
  , e_color  :: Color            -- цвет частицы
  , e_id     :: Int              -- номер в списке в мире
  }           
  deriving (Eq, Show)

--instance Show Entity where
--  show = (show . e_id)
