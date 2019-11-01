module TrafficLight where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

trafficLight1 :: CBit a => a -> (a, a, a)
trafficLight1 reset = (green, amber, red)
    where
        state1 = dff (orw [state9, reset])
        state2 = dff state1
        state3 = dff state2
        state4 = dff state3
        state5 = dff state4
        state6 = dff state5
        state7 = dff state6
        state8 = dff state7
        state9 = dff state8
        green = orw [state1, state2, state3]
        amber = orw [state4, state9]
        red = orw [state5, state6, state7, state8]
