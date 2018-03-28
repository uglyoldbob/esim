--this block of imports is for chart rendering
import Graphics.Rendering.Chart 
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Control.Lens
--end block

import Data.List
import Data.Maybe

import System.IO
import Text.Printf

import Wire
import Units
import Inductor
import Helpers
import PFC

testlist :: [(Double, Double)]
testlist = [polar_to_rect (pi - x, (r x 5)) | x <- makeList 50 (pi / 2)]
polar_to_rect :: (Double, Double) -> (Double, Double)
polar_to_rect (x, y) = (y * cos x, y * sin x)
r x ir = (ir / cos x) - (ir*ir/((cos x)*(cos x)) - (ir*ir)/(cos x))**0.5

dutyCycleChart = toRenderable layout
  where
    sinusoid2 x = plot_points_style .~ filledCircles 2 (opaque red)
              $ plot_points_values .~ (inVlist (dutyCycleOther x))
              $ plot_points_title .~ (show x) ++ " VAC"
              $ def
    layout = layout_title .~ "Duty Cycle"
           $ layout_plots .~ [toPlot (sinusoid2 x) | x <- inputVoltagesRms]
           $ def

diodeCurrentChart = toRenderable layout
  where
    sinusoid2 = plot_points_style .~ filledCircles 2 (opaque red)
              $ plot_points_values .~ (slist)
              $ plot_points_title .~ "Amps"
              $ def
    sinusoid3 = plot_points_style .~ filledCircles 2 (opaque red)
              $ plot_points_values .~ (rmsList)
              $ plot_points_title .~ "Amps"
              $ def
    slist :: [(Double, Double)]
    slist = [(x, averageDiodeCurrent x) | x <- [0,0.0001..0.02]]
    plist = [averageDiodeCurrent x | x <- [0,0.0001..0.02]]
    rmsList :: [(Double, Double)]
    rmsList = [(x, rmsCalc plist) | x <- [0,0.0001..0.02]]
    layout = layout_title .~ "Diode Current"
           $ layout_plots .~ [toPlot sinusoid2, toPlot sinusoid3]
           $ def
           
inVlist :: [Double] -> [(Double, Double)]
inVlist vl = [(1.0 * fromIntegral(x), vl!!x) | x <- [0..length vl - 1]]


{-|
chart2 = toRenderable layout
  where
    am :: Double -> Double
    am x = (sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))
    sinusoid2 = plot_points_style .~ filledCircles 2 (opaque red)
              $ plot_points_values .~ (testList (manipShape 0.09 0.03) [0.02,0.021..0.060])
              $ plot_points_title .~ "Inner Diameter 0.09 0.03"
              $ def
    sinusoid2a = plot_points_style .~ filledCircles 2 (opaque green)
              $ plot_points_values .~ (testList (manipShape 0.11 0.03) [0.020,0.021..0.060])
              $ plot_points_title .~ "Inner Diameter 0.11 0.03"
              $ def
    sinusoid2b = plot_points_style .~ filledCircles 2 (opaque aqua)
              $ plot_points_values .~ (testList (manipShape 0.12 0.03) [0.020,0.021..0.060])
              $ plot_points_title .~ "Inner Diameter 0.12 0.03"
              $ def
    sinusoid2c = plot_points_style .~ filledCircles 2 (opaque blanchedalmond)
              $ plot_points_values .~ (testList (manipShape 0.13 0.03) [0.020,0.021..0.060])
              $ plot_points_title .~ "Inner Diameter 0.13 0.03"
              $ def
    sinusoid2d = plot_points_style .~ filledCircles 2 (opaque brown)
              $ plot_points_values .~ (testList (manipShape 0.13 0.03) [0.020,0.021..0.060])
              $ plot_points_title .~ "Inner Diameter 0.13 0.03"
              $ def
    sinusoid3 = plot_points_style .~ filledCircles 2 (opaque blue)
              $ plot_points_values .~ (testList (manipShape 0.10 0.03) [0.02,0.021..0.060])
              $ plot_points_title .~ "Inner Diameter 0.10 0.03"
              $ def
    layout = layout_title .~ "Total Power 0.03 thick"
           $ layout_plots .~ [toPlot sinusoid2, toPlot sinusoid2a, toPlot sinusoid2b, toPlot sinusoid2c, toPlot sinusoid2d, toPlot sinusoid3]
           $ def
-}
manipShape a b x = maybeInductorPower (makeInductorFindWire PC40 (ToroidRect x a b) 15.4 1.5e-3 0.1 pfc_signal (conductors!!0)) pfc_signal
manipShape2 a b x = maybeInductorPower (makeInductorFindWire PC40 (ToroidRect a x b) 15.4 1.5e-3 0.1 pfc_signal (conductors!!0)) pfc_signal
manipShape3 a b x = maybeInductorPower (makeInductorFindWire PC40 (ToroidRect a b x) 15.4 1.5e-3 0.1 pfc_signal (conductors!!0)) pfc_signal

notmain = do prettyPrintInductor (fromJust(makeInductorFindD3 KoolMu26 (ToroidRect 0.02 0.15 0.02) 15.4 1.5e-3 0.01 pfc_signal (conductors!!0)))
             prettyPrintInductor (fromJust(makeInductorFindD3 KoolMu40 (ToroidRect 0.02 0.15 0.02) 15.4 1.5e-3 0.01 pfc_signal (conductors!!0)))
             prettyPrintInductor (fromJust(makeInductorFindD3 KoolMu60 (ToroidRect 0.02 0.15 0.02) 15.4 1.5e-3 0.01 pfc_signal (conductors!!0)))
             prettyPrintInductor (fromJust(makeInductorFindD3 KoolMu75 (ToroidRect 0.02 0.15 0.02) 15.4 1.5e-3 0.01 pfc_signal (conductors!!0)))
             prettyPrintInductor (fromJust(makeInductorFindD3 KoolMu90 (ToroidRect 0.02 0.15 0.02) 15.4 1.5e-3 0.01 pfc_signal (conductors!!0)))
             prettyPrintInductor (fromJust(makeInductorFindD3 PC40 (ToroidRect 0.02 0.15 0.02) 15.4 1.5e-3 0.01 pfc_signal (conductors!!0)))

main = do renderableToFile def "duty_cycle.svg" dutyCycleChart
          renderableToFile def "diode_current.svg" diodeCurrentChart
          prettyPrintInductor (stageInductor) pfc_signal

testList :: (Double -> Maybe Double) -> [Double] -> [(Double, Double)]
testList func xin = [(x, fromJust(func x)) | x <- xin, isJust (func x)]

testFunction x 
 | x < 5 = Nothing
 | x < 10 = Just x
 | x < 15 = Nothing
 | x < 20 = Just x
 | otherwise = Nothing

toroidChart a b c d e f mat max_curr targetL toleranceL driver = [inductorOptimumWire mat x max_curr targetL toleranceL driver | x <- (toroidMaker a b c d e f)]
toroidMaker a b c d e f = [ToroidRect (mmToMeter (a*x)) (mmToMeter (b*y)) (mmToMeter (c*z)) | x <- d, y <- e, z <- f, (a*x) < (b*y)]
       
graphMaker :: Material -> Double -> (Double -> Double) -> Double -> Double -> Double -> [[Char]]
graphMaker mat freq func minx maxx steps= [do printf "%.4f, %.4f" (x) (logBase 10 (func (10**x))) | x <- values]
    where values = [(logBase 10 minx) + (x * (logBase 10 (maxx/minx))/steps) | x <- [1..steps]]

data RmsPower = RmsPower Double Double deriving (Show)
  
prettyPrintInductor :: Inductor -> [(Double, Double)] -> IO ()
prettyPrintInductor ind driver = do (putStr . show) (ind_mat ind)
                                    putStr " "
                                    print (ind_shape ind)
                                    (putStr) (engineeringPrint (ind_minL ind) "H")
                                    putStr ", "
                                    (putStr) (engineeringPrint (ind_maxL ind) "H")
                                    putStr ", "
                                    (putStr . show) (ind_turns ind)
                                    putStr " turns, "
                                    (putStr) (printf "%.1f%% full " ((ind_filled ind) * 100))
                                    (putStr . show) (ind_wire ind)
                                    putStr "\n"
                                    (putStr) (printf "%.1f feet " (meterToFeet (ind_wire_length ind)))
                                    putStr "\n"
                                    (putStr) (printf "%.2f pounds " ((cableDensity (ind_wire ind)) * (ind_wire_length ind)))
                                    putStr "\n"
                                    (putStr) (engineeringPrint (totalInductorPower driver ind) "W")
                                    putStr ", "
                                    (putStr) (engineeringPrint (ind_w_pwr driver ind) "W")
                                    putStr " copper, "
                                    (putStr) (engineeringPrint (ind_c_pwr driver ind) "W")
                                    putStr " core"
                                    putStr "\n\n"

power_i2r (f, i) r = RmsPower f (i * i * r);

power (RmsPower _ p) = p
freq (RmsPower f _) = f

total_power t = sqrt (sum [(power x) * (power x) | x <- t])

pfc_signal = [ (60.0, 14.0), (40000.0, 1.4)]
   
lotsToroids = [ToroidRect (mmToMeter (10 * x**1.2)) (mmToMeter (10 * y**1.2)) (mmToMeter (10 * z**1.2)) | x <- [1..20], y <- [1..20], z <- [1..5], x < y]
specialToroids = [ToroidRect (mmToMeter 45.3) (mmToMeter 74.1) (mmToMeter z) | z <- [20..75]]
me = ToroidRect (mmToMeter 14.7) (mmToMeter 26.9) (mmToMeter 11.2)
me2 = ToroidRect (mmToMeter 102.4) (mmToMeter 165.1) (mmToMeter 31.75)
me3 = ToroidRect (mmToMeter 65) (mmToMeter 102) (mmToMeter 20)

testL = head (inductorSort (makeInductorMatShape 15.4 1.5e-3 0.1 pfc_signal (conductors!!0) (PC40, me3)) (totalInductorPower pfc_signal))

--[(calcWireLayerTurnLength me (magnetWire!!4) x) | x <- [1..(fromIntegral (calcWireLayers me (magnetWire!!4)))]]

--sum [(calcWireLayerTurns me (magnetWire!!4) x) | x <- [1..(fromIntegral (calcWireLayers me (magnetWire!!4)))]]

--max wire length
--sum [(calcWireLayerTurnLength me (magnetWire!!4) x) * fromIntegral(calcWireLayerTurns me (magnetWire!!4) x) | x <- [1..(fromIntegral (calcWireLayers me (magnetWire!!4)))]]
