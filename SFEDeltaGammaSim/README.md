<div style="margin: 0; padding: 0; text-align: center; border: none;">
<a href="https://quantlet.com" target="_blank" style="text-decoration: none; border: none;">
<img src="https://github.com/StefanGam/test-repo/blob/main/quantlet_design.png?raw=true" alt="Header Image" width="100%" style="margin: 0; padding: 0; display: block; border: none;" />
</a>
</div>

```
Name of Quantlet: SFEDeltaGammaSim

Published in: Statistics of Financial Markets I, SFM1

Description: Calculates performance of delta-gamma hedging for N simulated stock path trajectories

Keywords: delta-gamma, hedging, asset, performance, black-scholes, call, cost, strategy, simulation

See also: SFESLDHConv, SFEStopLossLogic, SFEDeltaHedgeGraph, SFEDeltaHedging,

Author: Kristian Boroz, Awdesch Melzer

Submitted: 2016/12/11

Input: 
- S0: initial stock value
- sg: Volatility
- r: Risk free interest rate
- K1: Strike price call option 1
- K2: Strike price call option 2
- T1: time to maturity option 1
- T2: time to maturity option 2
- a: mean of stock process for simulation
- N: number of simulations (number of trajectories)
- n: number of realisations within stock trajectory

Output: (1) plot of portfolio value, (2) plot of performance relative to stock price change

```
<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/SFE_class_2016/master/SFEDeltaGammaSim/SFEDeltaGammaSim.png" alt="Image" />
</div>

