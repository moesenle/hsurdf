
module Robotics.Urdf.Types
    (Pose, Vector, RPY, 
     JointType(..),
     Joint(..),
     Shape(..),
     Material(..),
     MaterialData(..),
     Geometry(..),
     Link(..),
     Robot(..),
     jointType
    )
where

import Data.Map.Strict (Map)
    
type Pose = (Vector, RPY)
type Vector = (Float, Float, Float)
type RPY = (Float, Float, Float)

data JointType = Revolute | Continuous | Prismatic
               | Fixed | Floating | Planar
                 deriving (Eq, Show)

data Joint = Joint {
      joint_name :: String,
      joint_type :: JointType,
      joint_origin :: Pose,
      joint_parent :: String,
      joint_child :: String,
      joint_axis :: (Float, Float, Float)
    } deriving (Eq, Show)

data Shape = Box (Float, Float, Float)
           | Cylinder (Float, Float)
           | Sphere Float
           | Mesh (String, Vector)
             deriving (Eq, Show)

data MaterialData = Color (Float, Float, Float, Float)
                  | Texture String
                  | Global
                    deriving (Eq, Show)

data Material = Material {
      material_name :: String,
      material_data :: MaterialData
    } deriving (Eq, Show)

data Geometry = Geometry {
      geometry_origin :: Pose,
      shape :: Shape,
      material :: Maybe Material
    } deriving (Eq, Show)

data Link = Link {
      link_name :: String,
      visual :: Maybe Geometry,
      collision :: Maybe Geometry
    } deriving (Eq, Show)
      
data Robot = Robot {
      robot_name :: String,
      root_link :: Maybe String,
      links :: Map String Link,
      joints :: Map String Joint,
      materials :: Map String Material
    } deriving (Eq, Show)

jointType :: String -> Maybe JointType
jointType n = case n of
      "revolute" -> Just Revolute
      "continuous" -> Just Continuous
      "prismatic" -> Just Prismatic
      "fixed" -> Just Fixed
      "floating" -> Just Floating
      "planar" -> Just Planar
      _ -> Nothing
