module Vec where
import Graphics.Rendering.OpenGL
import Data.Vec.Packed
--import qualified Data.Vec as Vec

vecX = Vec3D 1 0 0
vecY = Vec3D 0 1 0
vecZ = Vec3D 0 0 1

vec2Vector3 (Vec3D x y z) = Vector3 x y z
vec2Vertex3 (Vec3D x y z) = Vertex3 x y z
vec2Color4 (Vec3D x y z) a = Color4 x y z a
