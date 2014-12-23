# Simulate to scrape English soccer matches (Static webpage, we need RSelenium package for dynamic webpage)
source('https://github.com/englianhu/Soccer-League-Web-Scraping/blob/master/function/downloadMatch.R')
URL = "http://app.en.gooooal.com/soccer/statistic/standing.do?lid=4"
eng2012 = downloadMatch(URL, year = 2012)
eng2013 = downloadMatch(URL, year = 2013)

