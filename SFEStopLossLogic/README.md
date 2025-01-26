<div style="margin: 0; padding: 0; text-align: center; border: none;">
<a href="https://quantlet.com" target="_blank" style="text-decoration: none; border: none;">
<img src="https://github.com/StefanGam/test-repo/blob/main/quantlet_design.png?raw=true" alt="Header Image" width="100%" style="margin: 0; padding: 0; display: block; border: none;" />
</a>
</div>

```
Name of Quantlet: SFEStopLossLogic

Published in: Statistics of Financial Markets I

Description: Generates and plots the path of two stocks and marks the corresponding buying

Keywords: stop-loss, hedging, asset, black-scholes, call, simulation, cost, delta, strategy

See also: SFESLDHPerf, SFESLDHConv, SFEDeltaHedgeGraph, SFEStopLossThGraph, SFSstoploss

Author: Simon Gstöhl, Florian Schulz

Submitted: 2016/12/05

Input: 
- S0: Stock price at t = 0
- sig: Volatility
- r: Risk free interest rate
- K: Strike price
- t0: Starting time (1 week = 1/52)
- mat: Maturity
- dt: Time period between steps.

Output: A plot of two simulated stocks with buy and sell times within the stop-loss hedging strategy

```
<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/SFE_class_2016/master/SFEStopLossLogic/StopLossLogic.png" alt="Image" />
</div>

