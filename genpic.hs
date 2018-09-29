

pix = [[123,432,2],[354,345,532],[345,34,34]]

iw :: Integer
iw = 1024
ih :: Integer
ih = 798
oc = [(0.5 + ((fromIntegral iw) / (-2.0))), (0.5 + ((fromIntegral ih) / (-2.0))), ((fromIntegral iw) / 2.0)]
tilt = 0.6

p6_header = ["P3", ((show iw) ++ " " ++ (show ih)), "255"]

file_contents = unlines $ p6_header ++ [unwords $ map show $ concat pix]

main = writeFile "output.pnm" $ file_contents

