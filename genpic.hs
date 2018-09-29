import Quaternion

iw :: Integer
iw = 10 --24
ih :: Integer
ih = 7 --98
oc :: [Float]
oc = [(0.5 + ((fromIntegral iw) / (-2.0))), (0.5 + ((fromIntegral ih) / (-2.0))), ((fromIntegral iw) / 2.0)]
tilt:: Float
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

vec_from_pixel :: [Integer] -> [Float]
vec_from_pixel [j, k] = -- [1, 0, 0]
  let cw = [0.0, 0.0, -2.0]
      qq = [(1.0 - (tilt * tilt)) , (tilt * tilt), 0.0, 0.0]
      vv = rotate qq $ vsum [(fromInteger k), (fromInteger j), 0.0] oc
      in vv

find_ray_color _ = sky_color


pix = [[1,2],[3,4]]
p6_header = ["P3", ((show iw) ++ " " ++ (show ih)), "255"]

file_contents = unlines $ p6_header ++ [unwords $ map show $ concat pix]

main = writeFile "output.pnm" $ file_contents

