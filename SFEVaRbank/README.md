<div style="margin: 0; padding: 0; text-align: center; border: none;">
<a href="https://quantlet.com" target="_blank" style="text-decoration: none; border: none;">
<img src="https://github.com/StefanGam/test-repo/blob/main/quantlet_design.png?raw=true" alt="Header Image" width="100%" style="margin: 0; padding: 0; display: block; border: none;" />
</a>
</div>

```
Name of Quantlet: SFEVaRbank

Published in: Statistics of Financial Markets I

Description: Calculates the RMA and EMA of a portfolio of assets

Keywords: VaR, data visualization, risk, portfolio, RMA, EMA, estimation, qqplot, exceedance, financial, forecast, normal-distribution, time-series

Author: Marta Domagalska, Xiang Gao, Pegah Maham, David Pollack

Submitted: 2016/12/12

Input: 
- h: Time horizon for RMA and EMA
- alpha: Confidence-level of RMA and EMA
- gamma: decay of EMA

Datafile: 
- SFEVaRbank.csv : 'Time-series of the nominal prices of the DAX30, FTSE 100, and an assortment of stocks of other large companies.  Originally this file was named "2004-2014_dax_ftse". (daily periodicity from 05/04 - 05/14)'

Example: A combined plot containing the following: the log returns of a portfolio as points, the VaR using the RMA and EMA methods as lines, and the exceedences of RMA and EMA as rugs and Q-Q plots of the realized profits and losses over the estimated Value-at-Risk for VaR RMA and VaR EMA. (01/2007 - 01/2009)

```
<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/SFE_class_2016/master/SFEVaRbank/EMAoutlierst.png" alt="Image" />
</div>

<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/SFE_class_2016/master/SFEVaRbank/RMAoutlierst.png" alt="Image" />
</div>

<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/SFE_class_2016/master/SFEVaRbank/VaRReliability_EMA.png" alt="Image" />
</div>

<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/SFE_class_2016/master/SFEVaRbank/VaRReliability_RMA.png" alt="Image" />
</div>

<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/SFE_class_2016/master/SFEVaRbank/VaR_LtAbsChange.png" alt="Image" />
</div>

<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/SFE_class_2016/master/SFEVaRbank/VaR_YtPercChange.png" alt="Image" />
</div>

<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/SFE_class_2016/master/SFEVaRbank/VaRqqplotEMA.png" alt="Image" />
</div>

<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/SFE_class_2016/master/SFEVaRbank/VaRqqplotRMA.png" alt="Image" />
</div>

<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/SFE_class_2016/master/SFEVaRbank/VaRqqplot_fattail1.png" alt="Image" />
</div>

<div align="center">
<img src="https://raw.githubusercontent.com/QuantLet/SFE_class_2016/master/SFEVaRbank/VaRqqplot_fattail2.png" alt="Image" />
</div>

