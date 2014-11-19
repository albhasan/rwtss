
obj = wtssClient("http://www.dpi.inpe.br/mds/mds")

expect_that(all.equal(listCoverages(obj),c("MCD43A4","MOD09Q1","MOD13Q1","MYD09Q1","MYD13Q1")), is_true())
expect_that(all.equal(describeCoverages(obj,"MOD09Q1"),list(MOD09Q1=c("evi2","nir","quality","red"))), is_true())

timeseries = getTimeSeries(obj, coverages=c("MOD09Q1","MOD13Q1"), datasets=c("red","nir"), latitude=-12, longitude=-45, from="2004-01-01", to="2004-05-01")
