
import Data.Monoid (mempty)
import qualified Data.Map as M
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Robotics.Urdf.Types
import Robotics.Urdf.Parser    

simple_urdf :: String
simple_urdf = "\
\<robot name=\"foo\"> \
\  <link name=\"link1\" /> \
\  <link name=\"link2\" /> \
\  <link name=\"link3\" /> \
\  <joint name=\"joint1\" type=\"revolute\"> \
\    <parent link=\"link1\" /> \
\    <child link=\"link2\" /> \
\  </joint> \
\  <joint name=\"joint2\" type=\"revolute\"> \
\    <parent link=\"link2\" /> \
\    <child link=\"link3\" /> \
\  </joint> \
\</robot>"

complex_urdf :: String
complex_urdf = "\
\<robot name=\"foo\"> \
\  <link name=\"link1\"> \
\    <visual> \
\      <origin xyz=\"1 2 3\" rpy=\"4 5 6\" /> \
\      <geometry> \
\        <mesh filename=\"foo.stl\" /> \
\      </geometry> \
\      <material name=\"the material\"> \
\        <color rgba=\"1 2 3 4\" /> \
\      </material> \
\    </visual> \
\    <collision> \
\      <origin xyz=\"1 2 3\" rpy=\"4 5 6\" /> \
\      <geometry> \
\        <box size=\"1 2 3\" /> \
\      </geometry> \
\    </collision> \
\  </link> \
\  <link name=\"link2\"> \
\    <visual> \
\      <origin xyz=\"1 2 3\" rpy=\"4 5 6\" /> \
\      <geometry> \
\        <cylinder radius=\"2.5\" length=\"1.5\" /> \
\      </geometry> \
\      <material name=\"the other material\" /> \
\    </visual> \
\    <collision> \
\      <origin xyz=\"1 2 3\" rpy=\"4 5 6\" /> \
\      <geometry> \
\        <sphere radius=\"2.5\" /> \
\      </geometry> \
\    </collision> \
\  </link> \
\  <joint name=\"joint1\" type=\"revolute\"> \
\    <origin xyz=\"0.5 1.5 2.5\" rpy=\"3.5 4.5 5.5\" /> \
\    <parent link=\"link1\" /> \
\    <child link=\"link2\" /> \
\  </joint> \
\  <material name=\"the other material\"> \
\    <texture filename=\"tex.png\" /> \
\  </material> \
\</robot>"

main :: IO ()
main = defaultMainWithOpts [
        testCase "parsing of minimal URDF" testMinimalUrdf,
        testCase "parsing of complex URDF" testComplexUrdf,
        testCase "parsing of invalid URDF" testInvalidUrdf
       ] mempty

testMinimalUrdf :: Assertion
testMinimalUrdf =
    case parseUrdf simple_urdf of
      Nothing -> assertFailure $ "Unable to parse URDF: \n" ++ simple_urdf
      Just r -> do
        assertHasLink r "link1"
        assertHasLink r "link2"
        assertHasLink r "link3"
        assertHasJoint r "joint1"
        assertHasJoint r "joint2"
        "foo" @=? robot_name r
        Just "link1" @=? root_link r

testComplexUrdf :: Assertion
testComplexUrdf = return ()

testInvalidUrdf :: Assertion
testInvalidUrdf = case parseUrdf complex_urdf of
      Nothing -> assertFailure $ "Unable to parse URDF: \n" ++ complex_urdf
      Just r -> do
        assertHasLink r "link1"
        assertHasLink r "link2"
        assertHasJoint r "joint1"
        "foo" @=? robot_name r
        Just "link1" @=? root_link r

assertHasLink :: Robot -> String -> Assertion
assertHasLink r n =
    assertMapHas (links r) n $ "Link " ++ n ++ " not found."

assertHasJoint :: Robot -> String -> Assertion
assertHasJoint r n = do
  print "printing joints"
  printMapKeys (joints r)
  assertMapHas (joints r) n $ "Joint " ++ n ++ " not found."

assertMapHas :: M.Map String a -> String -> String -> Assertion
assertMapHas m k msg = if M.member k m then return () else assertFailure msg

printMapKeys :: M.Map String a -> IO ()
printMapKeys m = mapM_ print $ M.foldrWithKey (\k _ r -> k : r) [] m 
