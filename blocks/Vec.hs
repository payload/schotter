module Vec where
import Graphics.Rendering.OpenGL
import Data.Vec.Packed
import GHC.Float

vec0 = Vec3D 0 0 0
vecX = Vec3D 1 0 0
vecY = Vec3D 0 1 0
vecZ = Vec3D 0 0 1

vec2Vector3 (Vec3D x y z) = Vector3 x y z
vec2Vertex3 (Vec3D x y z) = Vertex3 x y z
vec2Color4 (Vec3D x y z) a = Color4 x y z a
vec2Normal3 (Vec3D x y z) = Normal3 x y z

vecD2F (Vec3D x y z) = Vec3F (double2Float x) (double2Float y) (double2Float z)

vec2Color4F v a = f $ vecD2F v
    where f (Vec3F x y z) = Color4 x y z a

vec2Vertex4F v w = f $ vecD2F v
    where f (Vec3F x y z) = Vertex4 x y z w

