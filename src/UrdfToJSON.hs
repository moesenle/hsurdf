{-# LANGUAGE OverloadedStrings #-}

import Robotics.Urdf.Types
import Robotics.Urdf.Parser    
import Data.Aeson
import System.Environment (getArgs, getProgName)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as B (unpack)

instance ToJSON Robot where
    toJSON (Robot n rl ls js ms) =
        object [ "name" .= n,
                 "root" .= rl,
                 "links" .= M.elems ls,
                 "joints" .= M.elems js,
                 "materials" .= M.elems ms
               ]

instance ToJSON Link where
    toJSON (Link n v c) =
        object [ "name" .= n,
                 "visual" .= v,
                 "collision" .= c
               ]

instance ToJSON JointType where
    toJSON Revolute = "revolute"
    toJSON Continuous = "continuous"
    toJSON Prismatic = "prismatic"
    toJSON Fixed = "fixed"
    toJSON Floating = "floating"
    toJSON Planar = "planar"

instance ToJSON Joint where
    toJSON (Joint n jt o p c a) =
        object [ "name" .= n,
                 "type" .= jt,
                 "origin" .= o,
                 "parent" .= p,
                 "child" .= c,
                 "axis" .= a
               ]

instance ToJSON Geometry where
    toJSON (Geometry o s m) =
        object [ "origin" .= o,
                 "shape" .= s,
                 "material" .= m]

instance ToJSON Shape where
    toJSON (Box (x, y, z)) =
        object [ "type" .= String "box",
                 "size" .= [x, y, z]
               ]
    toJSON (Cylinder (r, l)) =
        object [ "type" .= String "cylinder",
                 "radius" .= r,
                 "length" .= l
               ]
    toJSON (Sphere r) =
        object [ "type" .= String "sphere",
                 "radius" .= r
               ]
    toJSON (Mesh (f, s)) =
        object [ "type" .= String "mesh",
                 "filename" .= f,
                 "scale" .= s
               ]

instance ToJSON Material where
    toJSON (Material n d) =
        object $ [ "name" .= n ] ++ (encodeData d)
        where encodeData (Color (r, g, b, a)) = [ "color" .= [r, g, b, a] ]
              encodeData (Texture f) = [ "texture" .= f ]
              encodeData Global = []

main :: IO ()
main = do
  pn <- getProgName
  args <- getArgs
  if length args < 1 then
      putStrLn $ "Usage: " ++ pn ++ " <filename>"
  else
      do
        urdf <- readFile (args !! 0)
        putStrLn $ maybe "could not parse URDF" (B.unpack . encode) (parseUrdf urdf)
