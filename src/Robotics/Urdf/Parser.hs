
module Robotics.Urdf.Parser
    (parseUrdf)
where

import Text.XML.Light.Types
import Text.XML.Light.Input
import Robotics.Urdf.Types
import Data.List (partition, find)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe, mapMaybe)
import Control.Monad (liftM)
import Control.Applicative ((<|>))
import qualified Data.Map.Strict as SM
import qualified Data.Map as M

class StringMapKey a where
    key :: a -> String

data UrdfElement = UrdfElement {
      uName :: Maybe String,
      uType :: String,
      uAttributes :: [(String, String)],
      uChildren :: [UrdfElement]
    }
              
instance StringMapKey Link where
    key = link_name

instance StringMapKey Joint where
    key = joint_name

instance StringMapKey Material where
    key = material_name

listToMap :: (StringMapKey a) => [a] -> SM.Map String a
listToMap = SM.fromList . map (\e -> (key e, e))

parseUrdf :: String -> Maybe Robot
parseUrdf = listToMaybe . mapMaybe parseRobot . parseXML

parseRobot :: Content -> Maybe Robot
parseRobot c = createRobot $ parseUrdfElement c

createRobot :: UrdfElement -> Maybe Robot
createRobot r @ UrdfElement { uType = tp, uChildren = cs }
    | tp == "robot" =
        let js = mapMaybe createJoint cs
            ls = mapMaybe createLink cs
            ms = mapMaybe createMaterial cs
        in do
          n <- uName r
          return Robot { robot_name = n,
                         root_link = findRoot js,
                         links = listToMap ls,
                         joints = listToMap js,
                         materials = listToMap ms
                       }
    | otherwise = Nothing
                   
createLink :: UrdfElement -> Maybe Link
createLink r @ UrdfElement { uType = tp, uChildren = cs }
    | tp == "link" =
        let v = createGeometry =<< getElement "visual" cs
            c = createGeometry =<< getElement "collision" cs
        in do
          n <- uName r
          return Link { link_name = n, visual = v, collision = c }
    | otherwise = Nothing

createJoint :: UrdfElement -> Maybe Joint
createJoint r @ UrdfElement { uType = tp, uChildren = cs }
    | tp == "joint" = do
        n <- uName r
        jtp <- jointType =<< getAttr "type" r
        origin <- (createOrigin =<< getElement "origin" cs)
                  <|> Just ((0.0, 0.0, 0.0), (0.0, 0.0, 0.0))
        axis <- (createAxis =<< getElement "axis" cs) <|> Just (1.0, 0.0, 0.0)
        parent <- getAttr "link" =<< getElement "parent" cs
        child <- getAttr "link" =<< getElement "child" cs
        return Joint { joint_name = n,
                       joint_type = jtp,
                       joint_origin = origin,
                       joint_parent = parent,
                       joint_child = child,
                       joint_axis = axis
                     }
    | otherwise = Nothing

createGeometry :: UrdfElement -> Maybe Geometry
createGeometry UrdfElement { uChildren = cs } =
    let m = createMaterial =<< getElement "material" cs in
    do 
      o <- createOrigin =<< getElement "origin" cs
      s <- createShape =<< getElement "geometry" cs
      return Geometry { geometry_origin = o, shape = s,
                        material = m }

createOrigin :: UrdfElement -> Maybe Pose
createOrigin e = do
  xyz <- parseVec3 =<< getAttr "xyz" e
  rpy <- parseVec3 =<< getAttr "rpy" e
  return (xyz, rpy)

createShape :: UrdfElement -> Maybe Shape
createShape UrdfElement { uChildren = cs } =
    (getElement "box" cs >>= createBox)
    <|> (getElement "cylinder" cs >>= createCylinder)
    <|> (getElement "sphere" cs >>= createSphere)
    <|> (getElement "mesh" cs >>= createMesh)
  where
    createBox b = do
      s <- getAttr "size" b >>= parseVec3
      return $ Box s
    createCylinder c = do
      r <- liftM read $ getAttr "radius" c
      l <- liftM read $ getAttr "length" c
      return $ Cylinder (r, l)

    createSphere s = do
      r <- liftM read $ getAttr "radius" s
      return $ Sphere r

    createMesh m = do
      f <- getAttr "filename" m
      s <- (getAttr "scale" m >>= parseVec3) <|> Just (1.0, 1.0, 1.0)
      return $ Mesh (f, s)

createMaterial :: UrdfElement -> Maybe Material
createMaterial me @ UrdfElement { uType = tp, uChildren = cs }
    | tp /= "material" = Nothing
    | otherwise = do
  n <- uName me
  m <- (getElement "color" cs >>= getAttr "rgba" >>= createColor)
       <|> (getElement "texture" cs >>= getAttr "filename" >>= createTexture)
       <|> Just Global
  return Material { material_name = n, material_data = m }
  where
    createColor c =
        case splitOn " " c of
        [r, g, b, a] -> Just $ Color (read r, read g, read b, read a)
        _ -> Nothing
    createTexture fn = Just $ Texture fn

createAxis :: UrdfElement -> Maybe (Float, Float, Float)
createAxis e = getAttr "xyz" e >>= parseVec3

getElement :: String -> [UrdfElement] -> Maybe UrdfElement
getElement n = find (\e -> uType e == n)

getAttr :: String -> UrdfElement -> Maybe String
getAttr n (UrdfElement { uAttributes = as }) =
    liftM snd $ find (\(n', _) -> n' == n) as

parseVec3 :: String -> Maybe (Float, Float, Float)
parseVec3 s =
    case splitOn " " s of
      [a, b, c] -> Just (read a, read b, read c)
      _ -> Nothing

findRoot :: [Joint] -> Maybe String
findRoot js = root Nothing js $ childToParentMap js
  where
    childToParentMap =
        foldl (\m j -> M.insert (joint_child j) (joint_parent j) m) M.empty
    root r [] _ = r
    root Nothing [j] _ = Just $ joint_parent j
    root r (j : js') jm =
        case M.lookup (joint_parent j) jm of
          Just _ -> root r js' jm
          Nothing -> root (Just $ joint_parent j) js' jm

parseUrdfElement :: Content -> UrdfElement
parseUrdfElement (Elem e) =
    let (name, attrs) = parseAttributes $ elAttribs e in
    UrdfElement {  uName = name,
                   uType = qName $ elName e,
                   uAttributes = attrs,
                   uChildren = map parseUrdfElement $ elContent e
                }
    where
      parseAttributes :: [Attr] -> (Maybe String, [(String, String)])
      parseAttributes as =
          case partition (isAttr "name") $ map parseAttribute as of
            ([], as') -> (Nothing, as')
            ((_, n) : _ , as') -> (Just n, as')
      parseAttribute a = (qName $ attrKey a, attrVal a)
      isAttr n (n', _) = n == n'

parseUrdfElement _ =
    UrdfElement { uName = Nothing, uType = "",
                  uAttributes = [], uChildren = []
                }
