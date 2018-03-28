module Units where

--to convert various units to meters
inchToMeter x = x * 0.0254
meterToFeet x = x / (12 * 0.0254)
footToMeter x = inchToMeter (x * 12)
cmToMeter x = x * 0.01
mmToMeter x = x * 0.001
perMFTtoPerMeter x = x / 304.8
feetPerLbTolbsPerMeter x = 1 / (footToMeter x)
kgToLb x = 2.20462 * x

gaussToTesla x = x / 10000
oerstedToAtCm x = 250 * x / pi

