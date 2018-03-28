module Helpers where
--helper functions

import Data.Maybe
import Text.Printf

makeSine :: Int -> Int -> [Double]
makeSine len times 
    | times == 0 = replicate len 1.0
    | otherwise = [sin (2 * pi * (fromIntegral x) * (fromIntegral times) / (fromIntegral len)) | x <- [0..(len-1)]]

makeCosine :: Int -> Int -> [Double]
makeCosine len times 
    | times == 0 = replicate len 0.0
    | otherwise = [cos (2 * pi * (fromIntegral x) * (fromIntegral times) / (fromIntegral len)) | x <- [0..(len-1)]]

mulLists la lb
    | (length la) == (length lb) = [(la!!x) * (lb!!x) | x <- [0..len]]
    where len = (length la) - 1

combineLists :: [Double] -> [Double] -> [(Double, Double)]
combineLists la lb 
    | (length la) == (length lb) = [((la!!x), (lb!!x)) | x <- [0..len]]
    where len = (length la) - 1

pythagorean :: [(Double, Double)] -> [Double]
pythagorean l = [((fst x)**2 + (snd x)**2)**0.5 | x <- l]

total_angle :: (Double, Double) -> Double
total_angle (a, b) 
    | a /= 0 = atan (b / a)
    | otherwise = pi / 2

rectangular_to_polar :: (Double, Double) -> (Double, Double)
rectangular_to_polar a = (b, c)
    where b = ((fst a)**2 + (snd a)**2)**0.5
          c = atan ((snd a) / (fst a))

rmsCalc :: [Double] -> Double
rmsCalc list = sqrt (((sum) (map (** 2) list)) / fromIntegral(length list))

          
dft :: [Double] -> [(Double, Double)]
dft inp = combineLists sineList cosList
    where len = length inp
          mulSine = [makeSine len (truncate x) | x <- [0..(fromIntegral(len) * 0.5)]]
          mulCos = [makeCosine len (truncate x) | x <- [0..(fromIntegral(len) * 0.5)]]
          sineList = [((/(fromIntegral len)) . sum) (mulLists inp x) | x <- mulSine]
          cosList = [((/(fromIntegral len)) . sum) (mulLists inp x) | x <- mulCos]

lineCalc :: [(Double, Double)] -> [Double]
lineCalc points = [a, b, c]
    where x1 = fst (points!!0)
          y1 = snd (points!!0)
          x2 = fst (points!!1)
          y2 = snd (points!!1)
          a = y1 - y2
          b = x2 - x1
          c = ((x1 - x2) * y1) + ((y2 - y1) * x1)

ycalc points x = ((-1) * (points!!2) - (points!!0) * x) / (points!!1)
          
fstLess a it = (fst it) < a
fstMore a it = (fst it) >= a
linearInterpolation :: [(Double, Double)] -> Double -> Double
linearInterpolation array a
    | a <= (fst (head array)) = ycalc (lineCalc [array!!0, array!!1]) a
    | a >= (fst (last array)) = ycalc (lineCalc last_two) a
    | otherwise = ycalc (lineCalc [lower_pair, upper_pair]) a
    where lower_pair = last (filter (fstLess a) array)
          upper_pair = head (filter (fstMore a) array)
          last_two = drop ((length array) - 2) array

arrayLess a arr = (fst arr) < a
arrayMore a arr = (fst arr) > a

biLinearInterpolation :: [(Double, [(Double, Double)])] -> Double -> Double -> Double
biLinearInterpolation graph_data b freq
    | b <= (fst (head ilist)) = ycalc ( lineCalc [ilist!!0, ilist!!1]) b
    | b >= (fst (last ilist)) = ycalc (lineCalc last_two) b
    | otherwise = ycalc (lineCalc [lower_pair, upper_pair]) b
    where lower_pair = last (filter (fstLess b) ilist)
          upper_pair = head (filter (fstMore b) ilist)
          ilist = [( (fst x), linearInterpolation (snd (x)) freq) | x <- graph_data]
          last_two = drop ((length ilist) - 2) ilist

--array helper
makeArray x = [0..(x-1)]

makeList x y = map ((*y) . (/x)) [1..x]

listRange l a b = take (b - a + 1)(drop a l)

inTolerance value expectation percent = (value >= min_val) && (value <= max_val)
    where min_val = (1.0 - percent) * expectation
          max_val = (1.0 + percent) * expectation
getPercentError value expectation = (value - expectation) / expectation
          
findInput t guess goal tolerance
    | abs(closeness) < tolerance = [(result, guess, closeness)]
    | otherwise = (result, guess, closeness): (findInput t (guess / (1 + 0.5 * closeness)) goal tolerance)
    where result = t guess
          closeness = getPercentError result goal
    
engineeringPrint :: Double -> [Char] -> [Char]
engineeringPrint num unit
    | num < 1e-6 = printf "%.3f n" (num / 1e-9) ++ unit
    | num < 1e-3 = printf "%.3f u" (num / 1e-6) ++ unit
    | num < 1 = printf "%.3f m" (num / 1e-3) ++ unit
    | num < 1e3 = printf "%.3f" (num) ++ unit
    | num < 1e6 = printf "%.3f K" (num / 1e3) ++ unit
    | otherwise = printf "%.3f " num ++ unit

rescale inp num = [(minimum inp) + scale * x / actual_num | x <- list]
    where actual_num = num-1
          list = [0..actual_num]
          scale = (maximum inp) - (minimum inp)

headOrEmpty :: [a] -> [a]
headOrEmpty l
    | null l = []
    | otherwise = [head l]

maybeHead :: [a] -> Maybe a
maybeHead l
    | null l = Nothing
    | otherwise = Just (head l)

maybeThing thing it
    | isJust it = thing it
    | otherwise = Nothing
    
lastOrEmpty :: [a] -> [a]
lastOrEmpty l
    | null l = []
    | otherwise = [last l]