<div style="margin: 0; padding: 0; text-align: center; border: none;">
<a href="https://quantlet.com" target="_blank" style="text-decoration: none; border: none;">
<img src="https://github.com/StefanGam/test-repo/blob/main/quantlet_design.png?raw=true" alt="Header Image" width="100%" style="margin: 0; padding: 0; display: block; border: none;" />
</a>
</div>

```
Name of Quantlet: SFEStopLossThGraph

Published in: Statistics of Financial Markets I

Description: Generates and plots the path of two stocks and marks the theoretical buying

Keywords: stop-loss, hedging, continuous, asset, black-scholes, call, simulation, cost, delta, strategy

See also: SFEStopLossLogic, SFESLDHPerf, SFESLDHConv, SFEDeltaHedgeGraph, SFSstoploss

Author: Simon Gstöhl, Florian Schulz

Submitted: 2016/12/05

Input: 
- S0_1: Stock price at t = 0 for first path
- S0_2: Stock price at t = 0 for second path
- sig: Volatility
- r: Risk free interest rate
- K: Strike price
- t0: Starting time (1 week = 1/52)
- mat: Maturity
- dt: Time period between steps.

Output: A plot of two simulated stocks with theoretical buy and sell times in comtinuous time within

```
<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/SFE_class_2016/master/SFEStopLossThGraph/SFEStopLossThGraph.png" alt="Image" />
</div>

