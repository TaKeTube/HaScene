module HaRender
    (
        render
    ) where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.MArray
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.Char          (intToDigit)
import           Data.List          (sortBy)

import           Control.Lens       hiding (Empty)
import           GHC.Float
import           Linear

import           HaScene
--   show :: Mesh -> String

-- Chirality: Right-handed
-- Camera always look at -Z direction no matter which space it is in

getNorm :: Triangle -> V3 Float
getNorm (V3 v0 v1 v2) = normalize $ cross (normalize $ v0 - v1) (normalize $ v0 - v2)

sortTriangle :: Triangle -> Triangle
sortTriangle (V3 v0 v1 v2) = let
    [v0', v1', v2'] = sortBy (\(V3 _ y1 _) (V3 _ y2 _) -> compare y1 y2) [v0, v1, v2]
    in V3 v0' v1' v2'

viewMatrix :: Camera -> M44 Float
viewMatrix cam = let
    z = - (normalize $ _dir cam)
    y = normalize $ _up cam
    x = normalize $ cross y z
    invRotM = V3 x y z
    pos = _pos cam
    V3 vx vy vz = - (invRotM !* pos)
    V3 (V3 mxx mxy mxz) (V3 myx myy myz) (V3 mzx mzy mzz) = invRotM
    in V4   (V4 mxx mxy mxz vx)
            (V4 myx myy myz vy)
            (V4 mzx mzy mzz vz)
            (V4 0   0   0   1 )

projMatrix :: Float -> Float -> Float -> Float -> M44 Float
projMatrix fov aspectRatio zNear zFar = let
    invTan = 1 / tan (fov / 180 * pi * 0.5)
    k = 1 / (zNear - zFar)
    in V4   (V4 (invTan/aspectRatio) 0       0                0                )
            (V4 0                    invTan  0                0                )
            (V4 0                    0       ((zNear+zFar)*k) (-2*zFar*zNear*k))
            (V4 0                    0       1                0                )

transVec3 :: M44 Float -> V3 Float -> V3 Float
transVec3 m (V3 x y z) = let
    V4 x' y' z' _ = m !* V4 x y z 0
    in V3 x' y' z'

transPoint :: M44 Float -> V3 Float -> V3 Float
transPoint m (V3 x y z) = normalizePoint (m !* V4 x y z 1)

transTriangle :: M44 Float -> Triangle -> Triangle
transTriangle mvpM (V3 v0 v1 v2) =
    V3 (transPoint mvpM v0) (transPoint mvpM v1) (transPoint mvpM v2)

faceCulling :: [(Triangle, V3 Float)] -> [(Triangle, V3 Float)]
faceCulling ts = let
    isOut (V3 (V3 x0 y0 _) (V3 x1 y1 _) (V3 x2 y2 _), V3 _ _ z) = z > 0
    in filter isOut ts

vertShader :: Camera -> Mesh -> [(Triangle, V3 Float)]
vertShader cam m = let
    tris = _triangles m
    viewM = viewMatrix cam
    projM = projMatrix 60 1 (-0.1) (-100)
    isFront (V3 (V3 _ _ z0) (V3 _ _ z1) (V3 _ _ z2)) = z0 < 0 && z1 < 0 && z2 < 0
    -- mvpM  = viewM
    trisCamera = filter isFront $ map (transTriangle viewM) tris
    ns = map (normalize.getNorm) trisCamera
    trisProj = map (transTriangle projM) trisCamera
    in zip trisProj ns

fragShader :: V3 Float -> V3 Float -> Char
fragShader light n = let
    colorMap = ".,-~:;!*=#$@"
    -- colorLen = length colorMap - 1
    -- idx = max 0 $ round (int2Float colorLen * max 0 (dot (-light) n))
    colorLen = length colorMap
    idx = min (colorLen - 1) $ max 0 $ round (int2Float colorLen * max 0 (dot (-light) n))
    -- idx = round (int2Float colorLen * (dot (-light) n * 0.5 + 0.5))
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

rasterize :: Int -> Int -> (V3 Float -> Char) -> (Triangle, V3 Float) -> [((Int, Int), Float, Char)]
rasterize w h fShader (t@(V3 v0 v1 v2), n) = let
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
        | y <= yMin = xMin
        | y >= yMax = xMax
        | y >= yMid = lerpx xMax yMax xMid yMid y
        | otherwise = lerpx xMin yMin xMid yMid y
    xBoundEdge y
        | y <= yMin = xMin
        | y >= yMax = xMax
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
    -- apply fragment shader
    color = fShader n
    -- index range of y
    yyd = y2yy (yMin + dy / 2)
    yyu = y2yy (yMax - dy / 2)
    -- generate pixels
    in [((xx, yy), interpZ (xx2x xx) (yy2y yy), color) | yy <- [yyd..yyu], let (xxl,xxr) = xxBound yy, xx <- [xxl..xxr]]

render :: Int -> Int -> [Mesh] -> Camera -> String
render w h ms cam = elems $ runSTUArray $ do
    -- Z-buffer for distances
    zbuf <- newArray ((0, 0),(h-1, w-1)) (-1.0/0.0) :: ST s (STUArray s (Int,Int) Float)
    -- frame buffer for pixels
    fbuf <- newArray ((0, 0),(h-1, w)) ' '       :: ST s (STUArray s (Int,Int) Char)
    -- set fragment shader
    let light = normalize (V3 (-1) (-0.7) (-0.5))
    let viewM = viewMatrix cam
    let light' = normalize $ transVec3 viewM light
    -- rasterize triangles
    let pixels = concatMap (rasterize w h (fragShader light')) $ faceCulling $ concatMap (vertShader cam) ms
    mapM_ (\((xx, yy), z, c) -> do
            z0 <- readArray zbuf (yy,xx)
            if z > z0 && z < 0
            then do
                writeArray zbuf (yy,xx) z
                writeArray fbuf (yy,xx) c
            else do return ()
        ) pixels
    forM_ [0..h-1] $ \j -> do
        writeArray fbuf (j,w) '\n'
    return fbuf
