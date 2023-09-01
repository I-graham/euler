module P144 where

mapPoint h k =
  where
    m = k / ( 4 * h )
    a = m**2 + 4
    b = 2 * m * (k - h * m)
    c = (m * h)**2 + k**2 - 2 * m * h * k - 100