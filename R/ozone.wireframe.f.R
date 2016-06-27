function()
{
	attach(environmental)
        on.exit(detach(2))

	ozo.m <- loess((ozone^(1/3)) ~ wind * temperature * radiation, parametric = c("radiation", "wind"),
		span = 1, degree = 2)
	w.marginal <- seq(min(wind), max(wind), length = 50)
	t.marginal <- seq(min(temperature), max(temperature), length = 50)
	r.marginal <- seq(min(radiation), max(radiation), length = 4)
	wtr.marginal <- list(wind = w.marginal, temperature = t.marginal, radiation = r.marginal)
	grid <- expand.grid(wtr.marginal)
	assign("fit", predict(ozo.m, grid))
	wireframe(fit ~ wind * temperature | radiation, data = grid, scales = list(arrows = F), xlab = 
		"Wind Speed (mph)", ylab = "Temperature (F)", zlab = "Cube Root Ozone \n(cube root ppb)")
}
