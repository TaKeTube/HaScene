module HaRender
    (
        render
    ) where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.MArray
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.List          (sortBy)

import           Control.Lens       hiding (Empty)
import           GHC.Float
import           Linear

import           HaScene
--   show :: Mesh -> String
sortTriangle :: Triangle -> Triangle
sortTriangle (V3 v0 v1 v2) = let
    [v0', v1', v2'] = sortBy (\(V3 _ y1 _) (V3 _ y2 _) -> compare y1 y2) [v0, v1, v2]
    in V3 v0' v1' v2'

-- viewMatrix :: Camera -> M44 Float
-- viewMatrix cam = let
--     rotM = eulerMatrix (_angle cam)
--     invRotM = transpose rotM
--     pos = _pos cam
--     V3 vx vy vz = - (invRotM !* pos)
--     V3 (V3 mxx mxy mxz) (V3 myx myy myz) (V3 mzx mzy mzz) = invRotM
--     in V4   (V4 mxx mxy mxz vx)
--             (V4 myx myy myz vy)
--             (V4 mzx mzy mzz vz)
--             (V4 0   0   0   1 )

viewMatrix :: Camera -> M44 Float
viewMatrix cam = let
    dir = normalize $ _dir cam
    up = normalize $ _up cam
    pos = _pos cam
    lookat = lookAt pos dir up
    -- projection = perspective 60 (16/9) 0.1 50
    in lookat
    -- -- in transpose V4 (V4 (right^._x)   (right^._y)  (right^._z)    0)
    -- --       (V4 (up'^._x)     (up'^._y)    (up'^._z)      0)
    -- --       (V4 (dir^._x)     (dir^._y)    (dir^._z)      0)
    -- --       (V4 (pos^._x)     (pos^._y)    (pos^._z)      1)

-- eulerMatrix :: V3 Float -> M33 Float
-- eulerMatrix (V3 a b c) = let
--     cosa = cos a
--     sina = sin a
--     cosb = cos b
--     sinb = sin b
--     cosc = cos c
--     sinc = sin c
--     ma = V3 (V3 cosa    (-sina) 0      )
--             (V3 sina    cosa    0      )
--             (V3 0       0       1      )
--     mb = V3 (V3 1       0       0      )
--             (V3 0       cosb    (-sinb))
--             (V3 0       sinb    cosb   )
--     mc = V3 (V3 cosc    (-sinc) 0      )
--             (V3 sinc    cosc    0      )
--             (V3 0       0       1      )
--     in mc !*! mb !*! ma

projMatrix :: Float -> Float -> Float -> Float -> M44 Float
projMatrix fov aspectRatio zNear zFar = let
    invTan = 1 / tan (fov / 180 * pi * 0.5)
    k = 1 / (zNear - zFar)
    in V4   (V4 (invTan/aspectRatio) 0       0                0               )
            (V4 0                    invTan  0                0               )
            (V4 0                    0       ((zNear+zFar)*k) (2*zFar*zNear*k))
            (V4 0                    0       1                0               )

transVert :: M44 Float -> V3 Float -> V3 Float
transVert mvpM (V3 x y z) = normalizePoint (mvpM !* V4 x y z 1)

transTriangle :: M44 Float -> Triangle -> Triangle
transTriangle mvpM (V3 v0 v1 v2) =
    V3 (transVert mvpM v0) (transVert mvpM v1) (transVert mvpM v2)

faceCulling :: [Triangle] -> [Triangle]
faceCulling ts = let
    isOut (V3 (V3 x0 y0 _) (V3 x1 y1 _) (V3 x2 y2 _)) = (x1-x0)*(y2-y1) > (x2-x1)*(y1-y0)
    in filter isOut ts

vertShader :: Camera -> Mesh -> [Triangle]
vertShader cam m = let
    viewM = viewMatrix cam
    projM = projMatrix 45 1 (-0.1) (-50)
    mvpM  = projM !*! viewM
    in map (transTriangle mvpM) (_triangles m)

fragShader :: V3 Float -> V3 Float -> Char
fragShader light n = let
    colorMap = ".,-~:;!*=#$@"
    colorLen = length colorMap - 1
    -- idx = round (int2Float colorLen * (max 0 (dot (-light) n)))
    idx = round (int2Float colorLen * (dot (-light) n * 0.5 + 0.5))
    in colorMap !! idx

perspectInterp :: V3 Float -> V3 Float -> V3 Float -> Float
perspectInterp (V3 z0 z1 z2) (V3 u v w) (V3 p0 p1 p2) = let
    a = u / z0
    b = v / z1
    c = w / z2
    zn = 1 / (a + b + c)
    in zn * (a * p0 + b * p1 + c * p2)

berycentric2D :: Float -> Float -> V3 (V3 Float) -> V3 Float
berycentric2D x y (V3 (V3 x0 y0 _) (V3 x1 y1 _) (V3 x2 y2 _)) = let
    u = (x*(y1-y2) + y*(x2-x1) + x1*y2 - x2*y1) / (x0*(y1-y2) + (x2-x1)*y0 + x1*y2 - x2*y1)
    v = (x*(y2-y0) + y*(x0-x2) + x2*y0 - x0*y2) / (x1*(y2-y0) + (x0-x2)*y1 + x2*y0 - x0*y2)
    w = 1 - u - v
    in V3 u v w

rasterize :: Int -> Int -> Triangle -> [((Int, Int), Float, Char)]
rasterize w h t@(V3 v0 v1 v2) = let
    -- sort triangle verts by y
    [V3 xMin yMin zMin, V3 xMid yMid zMid, V3 xMax yMax zMax] =
        sortBy (\(V3 _ y1 _) (V3 _ y2 _) -> compare y1 y2) [v0, v1, v2]
    -- continious coord <-> discrete coord
    dx = 1.0 / int2Float w
    dy = 1.0 / int2Float h
    x2xx x = max 0 $ min (w - 1) $ floor ((x + 1) * 0.5 / dx)
    y2yy y = max 0 $ min (h - 1) $ floor ((y + 1) * 0.5 / dy)
    xx2x xx = dx * (int2Float xx + 0.5) * 2 - 1
    yy2y yy = dy * (int2Float yy + 0.5) * 2 - 1
    -- find bounds of x for a given y
    lerpx x1 y1 x2 y2 y = x1 + (y - y1) * (x2 - x1) / (y2 - y1)
    xBoundVert y
        | y <= yMin = yMin
        | y >= yMax = yMax
        | y >= yMid = lerpx xMax yMax xMid yMid y
        | otherwise = lerpx xMin yMin xMid yMid y
    xBoundEdge y
        | y <= yMin = yMin
        | y >= yMax = yMax
        | otherwise = lerpx xMax yMax xMin yMin y
    xxBound yy = let
        y = yy2y yy
        xVert = xBoundVert y
        xEdge = xBoundEdge y
        xl = min xVert xEdge
        xr = max xVert xEdge
        xxl = x2xx (xl + dx/2)
        xxr = x2xx (xr - dx/2)
        in (xxl, xxr)
    -- perspective-correct interpolation
    interpZ x y = let
        beryCoord = berycentric2D x y t
        in perspectInterp (V3 zMin zMid zMax) beryCoord (V3 zMin zMid zMax)
    -- Just Use Flat Shader
    -- n = normalize $ cross (normalize $ v0-v2) (normalize $ v1-v2)
    n = normalize $ cross v0 v1
    color = fragShader (normalize (V3 0 0 (-1))) n
    -- index range of y
    yyd = y2yy (yMin + dy / 2)
    yyu = y2yy (yMax - dy / 2)
    -- Generate pixels
    in [((xx, yy), interpZ (xx2x xx) (yy2y yy), color) | yy <- [yyd..yyu], let (xxl,xxr) = xxBound yy, xx <- [xxl..xxr]]

render :: Int -> Int -> [Mesh] -> Camera -> String
render w h ms cam = elems $ runSTUArray $ do
    -- Z-buffer for distances
    zbuf <- newArray ((0, 0),(h-1, w-1)) (1.0/0.0) :: ST s (STUArray s (Int,Int) Float)
    -- frame buffer for pixels
    fbuf <- newArray ((0, 0),(h-1, w)) ' '       :: ST s (STUArray s (Int,Int) Char)
    let pixels = concatMap (rasterize w h) $ faceCulling $ concatMap (vertShader cam) ms
    mapM_ (\((xx, yy), z, c) -> do
            z0 <- readArray zbuf (yy,xx)
            if z < z0
            then do
                writeArray zbuf (yy,xx) z
                writeArray fbuf (yy,xx) c
            else do return ()
        ) pixels
    forM_ [0..h-1] $ \j -> do
        writeArray fbuf (j,w) '\n'
    return fbuf