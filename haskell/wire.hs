module Wire where

import Units

sortWith :: [a] -> (a -> Double) -> [a]
sortWith [] func = []
sortWith (x:xs) func =
    let smaller = sortWith [a | a <- xs, (func a) <= (func x)] func
        bigger = sortWith [a | a <- xs, (func a) > (func x)] func
    in smaller ++ [x] ++ bigger

--conductor name, resistivity ohms meter, permeability, grams / cc
data Conductor = Conductor [Char] Double Double Double deriving (Show)

conductors = [
    Conductor "Copper" 1.678e-8 0.999994 8.96,
    Conductor "Aluminum" 2.65e-8 1.000022 2.7,
    Conductor "Silver" 1.59e-8 0.99998 10.49
    ]
 
--name, insulation diameter, wire diameter, resistance/meter
data Wire = Wire [Char] Double Double Double Double deriving (Show)

 --http://www.coonerwire.com/magnet-wire/
magnetWire = [
    Wire "8AWG magnet wire"  (inchToMeter 0.1324) (inchToMeter 0.1285) (perMFTtoPerMeter 0.6281) (feetPerLbTolbsPerMeter 19.91),
    Wire "9AWG magnet wire"  (inchToMeter 0.1181) (inchToMeter 0.1144) (perMFTtoPerMeter 0.7925) (feetPerLbTolbsPerMeter 25.13),
    Wire "10AWG magnet wire" (inchToMeter 0.1054) (inchToMeter 0.1019) (perMFTtoPerMeter 0.9987) (feetPerLbTolbsPerMeter 31.68),
    Wire "11AWG magnet wire" (inchToMeter 0.0941) (inchToMeter 0.0907) (perMFTtoPerMeter 1.261)  (feetPerLbTolbsPerMeter 39.92),
    Wire "12AWG magnet wire" (inchToMeter 0.0840) (inchToMeter 0.0808) (perMFTtoPerMeter 1.588)  (feetPerLbTolbsPerMeter 50.18),
    Wire "13AWG magnet wire" (inchToMeter 0.0750) (inchToMeter 0.0720) (perMFTtoPerMeter 2.001)  (feetPerLbTolbsPerMeter 63.25),
    Wire "14AWG magnet wire" (inchToMeter 0.0670) (inchToMeter 0.0641) (perMFTtoPerMeter 2.524)  (feetPerLbTolbsPerMeter 80.80),
    Wire "15AWG magnet wire" (inchToMeter 0.0599) (inchToMeter 0.0571) (perMFTtoPerMeter 3.181)  (feetPerLbTolbsPerMeter 100.50),
    Wire "16AWG magnet wire" (inchToMeter 0.0534) (inchToMeter 0.0508) (perMFTtoPerMeter 4.018)  (feetPerLbTolbsPerMeter 126.70),
    Wire "17AWG magnet wire" (inchToMeter 0.0478) (inchToMeter 0.0453) (perMFTtoPerMeter 5.054)  (feetPerLbTolbsPerMeter 159.70),
    Wire "18AWG magnet wire" (inchToMeter 0.0426) (inchToMeter 0.0403) (perMFTtoPerMeter 6.386)  (feetPerLbTolbsPerMeter 201.20),
    Wire "19AWG magnet wire" (inchToMeter 0.0382) (inchToMeter 0.0359) (perMFTtoPerMeter 8.046)  (feetPerLbTolbsPerMeter 253.20),
    Wire "20AWG magnet wire" (inchToMeter 0.0341) (inchToMeter 0.0320) (perMFTtoPerMeter 10.13)  (feetPerLbTolbsPerMeter 319.50),
    Wire "21AWG magnet wire" (inchToMeter 0.0306) (inchToMeter 0.0285) (perMFTtoPerMeter 12.77)  (feetPerLbTolbsPerMeter 402.70),
    Wire "22AWG magnet wire" (inchToMeter 0.0273) (inchToMeter 0.0253) (perMFTtoPerMeter 16.2)   (feetPerLbTolbsPerMeter 507.60),
    Wire "23AWG magnet wire" (inchToMeter 0.0244) (inchToMeter 0.0226) (perMFTtoPerMeter 20.3)   (feetPerLbTolbsPerMeter 650.00),
    Wire "24AWG magnet wire" (inchToMeter 0.0218) (inchToMeter 0.0201) (perMFTtoPerMeter 25.67)  (feetPerLbTolbsPerMeter 805.50),
    Wire "25AWG magnet wire" (inchToMeter 0.0195) (inchToMeter 0.0179) (perMFTtoPerMeter 32.37)  (feetPerLbTolbsPerMeter 1012.1),
    Wire "26AWG magnet wire" (inchToMeter 0.0174) (inchToMeter 0.0159) (perMFTtoPerMeter 41.02)  (feetPerLbTolbsPerMeter 1276),
    Wire "27AWG magnet wire" (inchToMeter 0.0156) (inchToMeter 0.0142) (perMFTtoPerMeter 51.44)  (feetPerLbTolbsPerMeter 1605),
    Wire "28AWG magnet wire" (inchToMeter 0.0139) (inchToMeter 0.0126) (perMFTtoPerMeter 65.31)  (feetPerLbTolbsPerMeter 2820),
    Wire "29AWG magnet wire" (inchToMeter 0.0126) (inchToMeter 0.0113) (perMFTtoPerMeter 81.21)  (feetPerLbTolbsPerMeter 2538),
    Wire "30AWG magnet wire" (inchToMeter 0.0112) (inchToMeter 0.0100) (perMFTtoPerMeter 103.7)  (feetPerLbTolbsPerMeter 3205),
    Wire "31AWG magnet wire" (inchToMeter 0.0100) (inchToMeter 0.0089) (perMFTtoPerMeter 130.9)  (feetPerLbTolbsPerMeter 4032),
    Wire "32AWG magnet wire" (inchToMeter 0.0091) (inchToMeter 0.0080) (perMFTtoPerMeter 162)    (feetPerLbTolbsPerMeter 5086),
    Wire "33AWG magnet wire" (inchToMeter 0.0081) (inchToMeter 0.0071) (perMFTtoPerMeter 205.7)  (feetPerLbTolbsPerMeter 6369),
    Wire "34AWG magnet wire" (inchToMeter 0.0072) (inchToMeter 0.0063) (perMFTtoPerMeter 261.3)  (feetPerLbTolbsPerMeter 8039),
    Wire "35AWG magnet wire" (inchToMeter 0.0064) (inchToMeter 0.0056) (perMFTtoPerMeter 330.7)  (feetPerLbTolbsPerMeter 10111),
    Wire "36AWG magnet wire" (inchToMeter 0.0058) (inchToMeter 0.0050) (perMFTtoPerMeter 414.8)  (feetPerLbTolbsPerMeter 12690),
    Wire "37AWG magnet wire" (inchToMeter 0.0052) (inchToMeter 0.0045) (perMFTtoPerMeter 512.1)  (feetPerLbTolbsPerMeter 16026),
    Wire "38AWG magnet wire" (inchToMeter 0.0047) (inchToMeter 0.0040) (perMFTtoPerMeter 648.2)  (feetPerLbTolbsPerMeter 20243),
    Wire "39AWG magnet wire" (inchToMeter 0.0041) (inchToMeter 0.0035) (perMFTtoPerMeter 846.6)  (feetPerLbTolbsPerMeter 25445),
    Wire "40AWG magnet wire" (inchToMeter 0.0037) (inchToMeter 0.0031) (perMFTtoPerMeter 1079)   (feetPerLbTolbsPerMeter 31949) 
    ]

--wire, conductor, number of strands
data Cable = Cable [Char] Wire Conductor Double deriving (Show)
    
rcalc (Cable n w c num) = [(p / (0.25 * (diameter2 x) * (diameter2 x) * pi) / resist x) | x <- magnetWire, (Conductor n p r d) <- [conductors!!0]]
dcalc (Cable n w c num) = [(1000 * d * (0.25 * (diameter2 x) * (diameter2 x) * pi) / wire_density x) | x <- magnetWire, (Conductor n p r d) <- [conductors!!0]]

cableOD (Cable _ (Wire _ d _ _ _) _ _ ) = d
cableDensity (Cable _ (Wire _ _ _ _ d) _ _ ) = d
    
wireNotTooLarge x = (diameter1 x) < (diameter1 (magnetWire!!0))
cableNotTooLarge (Cable _ (Wire _ d _ _ _) _ _ ) = d < (diameter1 (magnetWire!!0))

--sort the possible wires based on a list of frequencies and currents for lowest power
sortedWirePowerTable :: [(Double,Double)] -> Conductor -> [Cable]
sortedWirePowerTable a (Conductor n p ur d) = map fst (sortWith tfilt snd)
    where t = (wirePowerTable a (Conductor n p ur d)) ++ (strandedWirePowerTable a (Conductor n p ur d))
          tfilt = filter (cableNotTooLarge . fst) t

makeCable (Wire n d1 d2 w den) c num
 | num == 1 = Cable n (Wire n d1 d2 w den) c num
 | otherwise = Cable (show num ++ " strands of " ++ n) (Wire (show num ++ " strands of " ++ n) ((num**0.5) * d1) ((num**0.5) * d2) (w / num) (num * den)) c num

scalePower inp num = [(fst a, snd a / num) | a <- inp]

strandedWirePowerTable a (Conductor name p ur d) = [(makeCable w (Conductor name p ur d) n,n * power_calc w (scalea n) (Conductor name p ur d)) | w <- magnetWire, n <- [2..50] ]
    where scalea num = scalePower a num

wirePowerTable a (Conductor n p ur d) = [(makeCable w (Conductor n p ur d) 1,power_calc w a (Conductor n p ur d)) | w <- magnetWire ]

power_calc :: Wire -> [(Double, Double)] -> Conductor -> Double
power_calc w a (Conductor n p ur d) = sqrt (sum [x * x | x <- list]) 
    where list = [ resist_frequency w f p ur * a * a | (f,a) <- a]

cablePower (Cable _ w c _) s = power_calc w s c

skinDepth f p ur = sqrt(p/(pi*f*1*4*pi/10000000))

resist_frequency (Wire n d1 d2 r den) f p ur = resist (Wire n d1 d2 r den) / (tube_calc d2 sd)
    where sd = skinDepth f p ur
          area = pi * 0.25 * d2 * d2
          hole_area = pi * 0.25 * (d2-sd) * (d2-sd)

tube_calc od wall
 | od > (2 * wall) = minimum [(area-hole_area)/area, 1]
 | otherwise = 1.0
    where area = pi * 0.25 * od * od
          hole_area = pi * 0.25 * (od-wall) * (od-wall)

diameter1    (Wire _ d _ _ _) = d
diameter2    (Wire _ _ d _ _) = d
resist       (Wire _ _ _ w _) = w
wire_density (Wire _ _ _ _ d) = d

findWeirds :: [Double]  -> [Double]
findWeirds [] = []
findWeirds [_] = []
findWeirds (x:y:ys)
 | x <= y = [] ++ findWeirds (y:ys)
 | otherwise = [x] ++ findWeirds (y:ys)