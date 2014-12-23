Soccer-League-Web-Scraping
==========================
Scraping the data of English Soccer Leagues from a statical webpage.
However for dynamic webpage require webdriver --- RSelenium.

```{r}
source('C:/Users/Scibrokes Trading/Documents/GitHub/englianhu/Soccer-League-Web-Scraping/function/downloadMatch.R')
URL = "http://app.en.gooooal.com/soccer/statistic/standing.do?lid=4"
eng2012 = downloadMatch(URL, year = 2012)
eng2013 = downloadMatch(URL, year = 2013)
```
