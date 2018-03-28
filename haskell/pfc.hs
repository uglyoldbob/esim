module PFC where

import Data.List
import Data.Maybe

import Helpers
import Inductor
import Wire

{-|
-------------------------------- power supply calculations for FAN9673 pfc controller
---------------------------------------------------------- for FAN9673 pfc controller
-------------------------------- power supply calculations for FAN9673 pfc controller
-}
--given values
inputVoltageRms = [100.0,101.0..240]
inputVoltageRmsMinMax = sort [x * y | x <- inputVoltageRms, y <- [0.85,1,1.1]]
inputVoltagesRms = rescale inputVoltageRmsMinMax 20
inputFrequency = [50.0,60.0]
assumedEfficiency = 0.95
maxOverload = 1.1
switchingFrequency = 40000.0
rippleFactor = 0.1
voltage_pfc = 400.0
outputWattage = 2400.0
hold_time = 15e-3
minimum_hold_voltage = 0.75
outputPowerPercent = [x * 0.01 | x <- [0..100]]

--selected component values
rVIR = 100.0e3
rIAC = 6.0e6    --universal AC input
rM = 7500
vRipple = 0.05

--calculated values
powerInput = outputWattage / assumedEfficiency
fIC = switchingFrequency * 0.13
rRI = 800e6 / switchingFrequency
vVIR = rVIR * 10e-6
iLimit1 = 1.2*1.0208 / rRI
iLimit2 = 1.2*1.03215/ rRI

timeHold = (voltage_pfc**2 - (0.75 * voltage_pfc)**2)*cOUT / (2 * outputWattage)
rCS v = v**2 * 2 * rM / (maxOverload * rIAC * outputWattage / 3)
rILimit v = 1.8 * outputWattage / (3 * assumedEfficiency) * 2**0.5 * (rCS v) / v * 4 / iLimit1
rILimit2 = 1.5 * 0.395 / iLimit2
stageInductance v = (2**0.5 * v * voltage_pfc - 2**0.5 * v) / (rippleFactor * (averageStageCurrent v) * voltage_pfc * switchingFrequency)
stageInductor :: Inductor
stageInductor = fromJust (makeInductorFindD3 PC40 (ToroidRect 0 0.1 0) max_curr targetL 0.05 driver c)
    where max_curr = (peakStageCurrent vmin) + (deltaIL vmin)
          driver = [ (2*(minimum inputFrequency), (peakStageCurrent vmin)), (switchingFrequency, (deltaIL vmin))]
          targetL = stageInductance vmax
          vmin = minimum inputVoltagesRms
          vmax = maximum inputVoltagesRms
          c = (conductors!!0)

cOUT = maximum cOUT_list
cOUT_list = [ripple_spec, hold_spec]
    where ripple_spec = outputWattage / (voltage_pfc * 2 * pi * voltage_pfc * assumedEfficiency)
          hold_spec = (2 * outputWattage * hold_time) / (voltage_pfc**2 - (voltage_pfc * minimum_hold_voltage)**2)

dutyCycle vin = [(voltage_pfc - v) / voltage_pfc | v <- vlist vin]
dutyCycleOther vin = [(v) / voltage_pfc | v <- vlist vin]
vlist vin = [(abs . (*vin) . (*(2**0.5)) . sin) (x * 0.02 * pi) | x <- [0..100]]

averageStageCurrent v = 2**0.5 * outputWattage / (3 * v * assumedEfficiency)
peakStageCurrent v = (averageStageCurrent v) * (1 + rippleFactor * 0.5)
deltaIL v = (2**0.5 * v * voltage_pfc - 2**0.5*v) / ((stageInductance v) * voltage_pfc * switchingFrequency)
vRippleCalc = outputWattage / (voltage_pfc * 2 * pi * (minimum inputFrequency) * cOUT)
averageDiodeCurrent t = (outputWattage / (3 * voltage_pfc)) * (1 - cos(4*pi*(minimum inputFrequency)*t))
rmsDiodeCurrent = rmsCalc [averageDiodeCurrent x | x <- (makeList 1000 (1/(minimum inputFrequency)))]
iCurrent vin = vlist (outputWattage / (vin * assumedEfficiency * 3))

rcs_power vin = i * i * (rCS vin)
    where i = rmsCalc (iCurrent vin)

pfc_diode_power d freq pfcv rmsI vin = (diodeSwitchingLoss d freq pfcv) + (diodePowerLoss d diode_duty rmsI) + (diodeLeakage d duty voltage_pfc)
    where diode_duty = rmsCalc (dutyCycleOther vin)
          duty = rmsCalc (dutyCycle vin)

singleStageLoss ind dio trans cs vin = p_ind + p_dio + p_trans + pcs
    where p_ind = 5
          p_dio = pfc_diode_power dio switchingFrequency voltage_pfc rmsDiodeCurrent vin
          p_trans = transistorPowerLoss trans charge_duty (rmsCalc (iCurrent vin))
          pcs = (charge_duty * (rmsCalc (iCurrent vin)))**2 * (rCS vin)
          diode_duty = rmsCalc (dutyCycleOther vin)
          charge_duty = rmsCalc (dutyCycle vin)

--charging, discharging

--charging portion
--transistor conducts
--diode leaks

--discharging portion
--diode conducts

--IGBT resistance, turn on loss, turn off loss
--ohms, joules, joules
data Transistor = IGBT Double Double Double

--forward voltage, reverse recovery (coulombs), leakage
data Diode = Diode Double Double Double

--watts
transistorSwitchingLoss (IGBT _ lossOn lossOff) freq = freq * lossOn + freq * lossOff
transistorPowerLoss (IGBT r _ _) d a = r * a * a * d
diodeSwitchingLoss (Diode _ a _) freq v = a * freq * v
diodePowerLoss (Diode v _ _) d a = v * a * d
diodeLeakage (Diode _ _ i) d v = v * i * d

igbt_FGH20N60UFDTU = IGBT (1.8 / 20) 0.38e-3 0.26e-3
diode_VS_20ETF06FPPbF = Diode 1.3 1.25e-6 0.1e-3
diode_IDH08G65C6XKSA1 = Diode 1.25 12.2e-9 27.0e-6
diode_list = [diode_VS_20ETF06FPPbF, diode_IDH08G65C6XKSA1]