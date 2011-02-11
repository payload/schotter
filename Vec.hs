module Vec where
import Graphics.Rendering.OpenGL
import qualified Data.Vector as Vector

type Vec = Vector.Vector Double

vec :: Double -> Double -> Double -> Vec
vec x y z = Vector.fromList [x, y, z]

neg a = Vector.map (0-) a
abs a = sqrt $ dot a a
sub a b = Vector.zipWith (-) a b
add a b = Vector.zipWith (+) a b        
mul a b = Vector.map (\e -> e*a) b
dot a b = Vector.sum $ Vector.zipWith (*) a b
(!) = (Vector.!)
map = Vector.map

vecX = vec 1 0 0
vecY = vec 0 1 0
vecZ = vec 0 0 1

vecLeft     v = v `sub` vecX
vecRight    v = v `add` vecX
vecUp       v = v `add` vecY
vecDown     v = v `sub` vecY
vecForward  v = v `add` vecZ
vecBackward v = v `sub` vecZ

vec2Vertex3 v = Vertex3 (v!0) (v!1) (v!2)
vec2Vector3 v = Vector3 (v!0) (v!1) (v!2)
vec2List    v = [v!0, v!1, v!2]
