-- 最初のプログラム
double x    = x + x
quadruple x = double (double x)

factorial n = product[1..n]
average ns = sum ns `div` length ns

-- 命名規則
{-
    関数名変数名ともに小文字スタート
    慣習的に、引数がリストの場合は、名前の最後にsをつけ複数の値であることを示す
        数値のリスト -> ns
        任意の値のリスト -> xs
-}

last' xs = head (reverse xs)
init' xs = reverse (drop 1 (reverse xs))

-- 道具箱
-- 条件式
abs' n = if n>= 0 then n else - n

-- ガード
-- 条件が多い場合は、条件式よりも見やすい場合が多い
abs'' n | n >= 0    = n
        | otherwise = -n

-- パターンマッチ
head' :: [a] -> a
head' (x:_) = x -- 関数適用のほうが結合順位が高いので、cons演算子（:）はカッコで囲む必要がある

-- ラムダ
odds n = map(\x -> x * 2 + 1) [0..n-1]

-- 4.8 練習問題
halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (take (halfLength xs) xs, drop (halfLength xs) xs)
        where
            halfLength xs = (length xs) `div` 2

safetail :: [a] -> [a]
safetail [] = []
safetail (_:xs) = xs

or' :: Bool -> Bool -> Bool
or' True True = True
or' True False = True
or' False True = True
or' False False = False

or'' :: Bool -> Bool -> Bool
or'' False False = False
or'' _ _ = True

multi x y z = x * y * z -- カリー化された関数
multi' = (\x -> (\y -> (\z -> (x * y * z)))) -- ラムダ式で表現

