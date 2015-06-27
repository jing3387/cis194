module Golf where


skips :: [a] -> [[a]]
skips xs = skips' 1 xs
  where skips' :: Int -> [a] -> [[a]]
        skips' m ys
          | m == length xs = [ys]
          | otherwise = takeEveryNth m ys : skips' (m + 1) ys
          where takeEveryNth :: Int -> [a] -> [a]
                takeEveryNth n zs = map snd
                                    (filter (\(x,_) -> x `mod` n == 0) (zip [1..] zs))
