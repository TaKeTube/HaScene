
import  Linear  hiding (rotate)

eulerMatrix :: V3 Float -> M33 Float
eulerMatrix (V3 a b c) = let
    cosa = cos a
    sina = sin a
    cosb = cos b
    sinb = sin b
    cosc = cos c
    sinc = sin c
    ma = V3 (V3 1       0       0      )
            (V3 0       cosa    (-sina))
            (V3 0       sina    cosa   )
    mb = V3 (V3 cosb    0       sinb   )
            (V3 0       1       0      )
            (V3 (-sinb) 0       cosb   )
    mc = V3 (V3 cosc    (-sinc) 0      )
            (V3 sinc    cosc    0      )
            (V3 0       0       1      )
    in mc !*! mb !*! ma
