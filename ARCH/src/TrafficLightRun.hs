module TrafficLightRun where
import HDL.Hydra.Core.Driver
import TrafficLight

main :: IO ()
main = runTrafficLight1 trafficLight1Data

trafficLight1Data =
    [[1],
      [0],
      [0],
      [0],
      [0],
      [0],
      [0],
      [0],
      [0],
      [0],
      [0],
      [0]]

runTrafficLight1 input = runAllInput input output
    where
        reset = getbit input 0
        (green, amber, red) = trafficLight1 reset
        output =
            [string "reset=", bit reset,
             string "    green=", bit green,
             string " amber=", bit amber,
             string " red=", bit red]