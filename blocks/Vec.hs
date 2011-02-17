module Vec where
import Graphics.Rendering.OpenGL

import Data.Vec.Packed
import qualified Data.Vec as Vec

import GHC.Float

vec0 = Vec3F 0 0 0
vecX = Vec3F 1 0 0
vecY = Vec3F 0 1 0
vecZ = Vec3F 0 0 1

vec2Vector3 (Vec3F x y z) = Vector3 x y z
vec2Vertex3 (Vec3F x y z) = Vertex3 x y z
vec2Color4 (Vec3F x y z) a = Color4 x y z a
vec2Normal3 (Vec3F x y z) = Normal3 x y z
vec2Vertex4 (Vec3F x y z) w = Vertex4 x y z w

vec2Vertex3D v = vec3D2Vertex3D $ Vec.map (fromRational . toRational) v
vec3D2Vertex3D (Vec3D x y z) = Vertex3 x y z

vec2Vector3D v = vec3D2Vector3D $ Vec.map (fromRational . toRational) v
vec3D2Vector3D (Vec3D x y z) = Vector3 x y z
