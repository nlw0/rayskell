import Quaternion
import Data.Fixed
import Data.Maybe
import Data.List

data Ray = Ray { origin:: [Double]
               , direction :: [Double]
               }

data Hit = Hit { distance :: Double
               , func :: [Double]
               }
           

iw :: Integer
iw = 1024
ih :: Integer
ih = 798
oc :: [Double]
oc = [(0.5 + ((fromIntegral iw) / (-2.0))), (0.5 + ((fromIntegral ih) / (-2.0))), ((fromIntegral iw) / 2.0)]
tilt:: Double
tilt = 0.6

sky_color = [63, 126, 226]
plane_white = [255, 255, 255]
plane_black = [0, 0, 0]
error_color = [255, 0, 0]

oo1 = [0, -1.5, -0.6]
oo2 = [1.2, -1.5, -0.6]
oo3 = [-1.2, -1.5, -0.6]
rr = 0.6

fmypix = do j <- [0..(ih - 1)]
            k <- [0..(iw - 1)]
            find_ray_color $ vec_from_pixel [j, k]

vec_from_pixel :: [Integer] -> Ray
vec_from_pixel [j, k] = -- [1, 0, 0]
  let cw = [0.0, 0.0, -2.0]
      qq = [(1.0 - (tilt * tilt)) , (tilt * tilt), 0.0, 0.0]
      vv = rotate qq $ vsum [(fromInteger k), (fromInteger j), 0.0] oc
      in Ray cw vv

find_ray_color ray = 
  let tests = test_ray ray
      valid_tests = filter isJust tests
      the_hit = if (null valid_tests)
        then Nothing
        else minimumBy (\(Just (Hit a _)) (Just (Hit b _)) -> compare a b) valid_tests
  in (if (isNothing the_hit)
      then sky_color
      else (func $ fromJust the_hit))

test_ray ray =
  [hit_plane ray]

plane_texture (Ray orig dir) t =
  if (t < 0) then Nothing
  else let px = orig!!0 + t * dir!!0
           py = orig!!1 + t * dir!!1
       in if ((norm [px, py, 0]) > 50) then Nothing
          else if ((((Data.Fixed.mod' px 0.4) < 0.2) && ((Data.Fixed.mod' py 0.4) < 0.2)) ||
                   (((Data.Fixed.mod' px 0.4) > 0.2) && ((Data.Fixed.mod' py 0.4) > 0.2)))
               then Just (fmap id $ (vscale (0.001 / (t * t)) $ fmap fromIntegral plane_white))
               else Just (fmap id $ (vscale (0.001 / (t * t)) $ fmap fromIntegral plane_black))

clip x = if (x < 0) then 0
         else if (x > 255) then 255
              else x

hit_plane :: Ray -> Maybe Hit
hit_plane ray =
  let t = -((origin ray)!!2) / ((direction ray)!!2)
      yaya = plane_texture ray t
    in if (isJust yaya) then Just (Hit 1000.0 (fromJust yaya)) else Nothing

pix = [[1,2],[3,4]]

p6_header = ["P3", ((show iw) ++ " " ++ (show ih)), "255"]

file_contents = unlines $ p6_header ++ [unwords $ map show $ map round $ map clip $ fmypix]

main = writeFile "output.pnm" $ file_contents
