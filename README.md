# NemoCurrentPredGUI
This is a simple R Shiny app that intercts with the SalishSeaCast NEMO model (Soontiens et al. 2016; https://salishsea.eos.ubc.ca/nemo/). This was created by K.J. Palmer of SMRU Consulting on behalf of the Port of Vancouver Frasier Port Authority's Enhancing Cetacean Habitat and Observation Program (https://www.portvancouver.com/environmental-protection-at-the-port-of-vancouver/maintaining-healthy-ecosystems-throughout-our-jurisdiction/echo-program/). The goal of this application is to allow pilots entering or leaving Vancouver to quickly estimate currents throught the slowdown area.

The GUI takes user information on the approximate location of the tidal prediction (12 options) and builds a URL to query the database of predictions assuming 10m depth. It then calculates the magnitude of the current (in knots) and the <b> uncorrected </b> current direction.

Things to consider
1) Forecasted data are available for ~2 days ahead
2) Forecasted data from the model is updated daily around 11:30am Pacific
3) If no data are shown in the plots, it's likely the prediction is too far in advance. Decrease the prediction day by 1
4) Data are sometimes missing from the Salish Seacast Model as the data update. If this happens, please check again after 11:30 am Pacific

Soontiens, N., Allen, S., Latornell, D., Le Souef, K., Machuca, I., Paquin, J.-P., Lu, Y., Thompson, K., Korabel, V., 2016. Storm surges in the Strait of Georgia simulated with a regional model. Atmosphere-Ocean 54 1-21. https://dx.doi.org/10.1080/07055900.2015.1108899
