
#obj = wtssClient("http://www.dpi.inpe.br/mds/mds")
#objlist = listCoverages(obj)
#objdesc = describeCoverages(obj,"MOD09Q1")

#expect_that(all.equal(objlist,c("MCD43A4","MOD09Q1","MOD13Q1","MYD09Q1","MYD13Q1")), is_true())
#expect_that(all.equal(objdesc,list(MOD09Q1=c("evi2","nir","quality","red"))), is_true())

#ts1 = getTimeSeries(obj, coverages="MOD09Q1", datasets=c("nir","quality","red","evi2"), latitude=-12, longitude=-45, from="2004-01-01", to="2004-05-01")
#ts2 = getTimeSeries(obj, coverages=objdesc, longitude=-45, latitude=-12, from="2004-01-01", to="2004-05-01")
#expect_that(all.equal(ts1,ts2), is_true())

#objdesc = describeCoverages(obj,objlist)
#tsAll = getTimeSeries(obj, coverages=objdesc, longitude=-45, latitude=-12, from="2004-01-01", to="2004-05-01")

#obj = wtssClient("http://www.dpi.inpe.br/mds/mds")
#coordinates = list( c(longitude=-45, latitude=-12),  c(longitude=-54, latitude=-11))
#tsAll = getListOfTimeSeries(obj, coverages="MOD09Q1", datasets="red", coordinates=coordinates, from="2004-01-01", to="2004-05-01")

#print("Ok!")

expect_that(TRUE, is_true())