# probs_test
Freeware app for AEI Premium probabilistic forecasting of futures price

This is a pilot app for AEI's Premium subscription service. It is free and available to approved users.

The app takes user input specifying a futures contract and computes the probability distribution of prices at expiration.

In the pilot, two grain contracts are available: November 2022 Soybeans and December 2022 Corn. The data were collected from BarChart Premier, and are hosted on Jeffrey Young's private Google Sheets account, and were measured weekly starting from May 25 2022 until expiration.

The algorithm takes the below-the-money options premiums (both put and call) and the above-the-money options premiums (both put and call), computes the volatilities (2 + 2 = 4 point estimates), takes the weighted average of the 4 volatilities to obtain the implied volatility of the futures contract. From there, the mean of the price probability distribution is assumed to be the closing spot price, and the standard deviation is computed as the spot price times the implied volatility times the square root of the expiration quotient (days to contract expiration divided by 365.25 days). The mean and standard deviation are transformed into the location and scale parameters of the lognomral distribution.

It is from this distribution that the strike price of interest can be input by the user of the app, and the program will compute the percentile in which that user-defined price falls (i.e., the probability that the contract will expire below that price).
