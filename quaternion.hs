qrot [a, b, c, d] = [
   [(a * a + b * b - c * c - d * d), (2 * b * c - 2 * a * d), (2 * b * d + 2 * a * c)],
   [(2 * b * c + 2 * a * d), (a * a - b * b + c * c - d * d), (2 * c * d - 2 * a * b)],
   [(2 * b * d - 2 * a * c), (2 * c * d + 2 * a * b), (a * a - b * b - c * c + d * d)]]
rotate qq vec = map (\x -> dot x vec) $ qrot qq
reflect dd nn = vsub dd $ vscale (2 * dot dd nn) (normalze nn)

normalze vv = map (\x -> x/(norm vv)) vv
vsum a b = zipWith (+) a b
vsub a b = zipWith (-) a b
vscale s vv = map (s *) vv
dot v w = sum $ zipWith (*) v w
norm v = sqrt $ dot v v

myq = [(1 - (0.1 * 0.1)), (0.1 * 0.1), 0, 0]
myv = [0.0, 1.0, 0.0]
