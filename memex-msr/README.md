First, move the msr-data.csv file into this directory.

Second, start R and install the necessary dependencies.
The following code should do it.
```
options("repos"="http://cran.rstudio.com/")
install.packages("itertools")
install.packages("foreach")
install.packages("tm")
install.packages("devtools")
devtools::install_github("bwlewis/IRL")
devtools::install_github("ramnathv/rCharts")
devtools::install_github("tesseradata/datadr")
devtools::install_github("tesseradata/trelliscope")
```

Third, run the script.
```
source("s.r", echo=TRUE)
```

Note:
- I'm getting a sample of 500 documents from 1981-1983 and 2012-2012. This 
will let see how articles have changed.
- There are two displays. One shows all document in a single panel. The other
shows the documents for a given year in 6 different panels.
- I've only added 1 cognostic. There's a lot more we can do with this.
