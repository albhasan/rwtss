rwtss
=====

R client for Web Time Series Service (WTSS).


WTSS is a lightweight web service for handling remote sensing imagery as time series. Through a simple and effective representation for time series, this web service can be easily integrated into free and open source tools such as R, Python and web browser (through JavaScript).

A JavaScript client can be found at <a href="http://github.com/gqueiroz/wtss/">http://github.com/gqueiroz/wtss<a>


<h3>Prerequisites:</h3>
<ul>
	<li>Internet access.</li>
	<li><a href="http://git-scm.com/">Git</a>.</li>
	<li><a href="http://www.r-project.org/">R</a>.</li>
	<li><a href="http://www.rstudio.com/">Rstudio</a>.</li>
</ul>


<h3>To use the package:</h3>
<ol>
	<li>Open RStudio</li>
	<li>Install devtools <code>install.packages("devtools")</code></li>
	<li>Load devtools <code>library(devtools)</code></li>
	<li>Install the rwtss package <code>install_github("albhasan/rwtss")</code></li>
	<li>Load the rwtss package <code>library(rwtss)</code></li>
</ol>


<h3>Usage examples:</h3>
<ol>
	<li>Create a connection <code>obj = wtssClient("http://www.dpi.inpe.br/mds/mds")</code></li>
	<li>Get the list of products provided by the service <code>objlist = listCoverages(obj)</code></li>
	<li>Get the description of an specific product <code>objdesc = describeCoverages(obj,"MOD09Q1")</code></li>
	<li>Get a time series <code>ts1 = getTimeSeries(obj, coverages="MOD09Q1", datasets=c("nir","quality","red","evi2"), latitude=-12, longitude=-45, from="2004-01-01", to="2004-05-01")</code></li>
</ol>



<h3>To build the package:</h3>
<ol>
	<li>Clone the project: <code>git clone https//github.com/albhasan/rwtss.git</code>.</li>
	<li>Open Rstudio, go to File - Open Project and pick the file <code>rwtss.Rproj</code>.</li>
	<li>Install the required packages <code>install.packages(c("roxygen2", "testthat"))</code>.</li>
	<li>Go to the <i>Build</i> tab in the upper-right panel and press the button <i>Build & Reload</i>. After this the package is ready to use.</li>
	<li>You can also create a source package: Go to the <i>Build</i> tab, display the menu <i>More</i> and select the option <i>Build Source Package</i>.</li>
</ol> 
