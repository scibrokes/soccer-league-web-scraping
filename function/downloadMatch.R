downloadMatch = function(URL, year, verbose = FALSE, followlocation = TRUE, timeout = 13,
                         ftp.use.epsv = FALSE, ftplistonly = TRUE, ssl.verifypeer = FALSE, useragent = "R"){
  # simulate to scrape English soccer matches
  # URL = "http://app.en.gooooal.com/soccer/statistic/standing.do?lid=4"
  # eng2012 = downloadMatch(URL, year = 2012)
  # eng2013 = downloadMatch(URL, year = 2013)
  require(RCurl, quietly = T)
  require(XML, quietly = T)
  require(stringr, quietly = T)
  options(warn = -1)
  
  # season id, and input url address
  sid = year
  ori.url = paste(URL,"&sid=", sid, sep = "")
  
  # -----------------------------------------------------------
  listLinks = function (URL, pattern = "", relative = FALSE) {
    doc = htmlParse(URL)
    if(is.null(grep("/$", URL)))
      URL = dirname(URL)
    nodes = getNodeSet(doc, "//a[@href]")
    hrefs = sapply(nodes, function(x) xmlGetAttr(x, "href"))
    paste(if(relative)
      ""
          else URL, hrefs[grep(pattern, hrefs)], sep = "")
  }
  # -----------------------------------------------------------
  # download the webpages' url from the website
  lnk = listLinks(ori.url, relative = TRUE)
  lnk = paste("http://app.en.gooooal.com/soccer/statistic/", lnk, sep="")[-1]
  std.url = unique(lnk[regexpr("standing", lnk) > 0])
  
  # filter the id of leagues & cups in website
  all.doc = getURL(std.url, verbose = verbose,
                   followlocation = followlocation, timeout = timeout, ftp.use.epsv = ftp.use.epsv,
                   ftplistonly = ftplistonly, ssl.verifypeer = ssl.verifypeer, useragent = useragent)
  cup.url = std.url[regexpr("Latest Round", all.doc) > 0]
  cid = unlist(str_match_all(cup.url,"lid=[0-9]*[0-9]*[0-9]*[0-9]"))
  league.url = std.url[!std.url %in% cup.url]
  lid = unlist(str_match_all(league.url,"lid=[0-9]*[0-9]*[0-9]*[0-9]"))
  league.url = paste(strsplit(lnk[regexpr("result_fixture", lnk) > 0], lid[1])[[1]][1], lid,
                     strsplit(lnk[regexpr("result_fixture", lnk) > 0], lid[1])[[1]][2], sep = "")
  lcid = unlist(str_match_all(std.url,"lid=[0-9]*[0-9]*[0-9]*[0-9]"))
  rm(std.url)
  
  # get the leagues and cups from the source website url
  league.doc = getURL(league.url, verbose = verbose, followlocation = followlocation,
                     timeout = timeout, ftp.use.epsv = ftp.use.epsv, ftplistonly = ftplistonly,
                     ssl.verifypeer = ssl.verifypeer, useragent = useragent)
  league = str_match_all(league.doc[1], "[^<>]+[a-zA-Z]+[^</a>]")
  regexp1 = as.numeric(regexpr("lid=", unlist(league)) > 0[]) # get the league id
  regexp2 = c(0, regexp1[-length(regexp1)]) #get the league name
  leagueID = data.frame(ID = as.numeric(gsub("lid=", "", lcid)),
                        Name = unlist(league)[(regexp2 > 0) & (regexpr("\\b[A-Z]", unlist(league)) > 0)])
  lgi = as.numeric(gsub("lid=", "", lid))
  leagueID$Cat = ifelse(leagueID$ID %in% lgi, "League", "Cup")
  x = apply(leagueID, 1, function(x) nchar(x['Name']))
  leagueID$Cat = ifelse((substr(leagueID$Name, x-1, x) == "PO"),"League",leagueID$Cat)
  rm(x, league, regexp1, regexp2)
  
  # counting and get the rounds of competition in leagues.
  lRound = str_match_all(league.doc, "Round *[0-9]*[0-9]*[0-9]")
  
  # -------------------------------------------------------------------------------------
  # Data in 2011, "Conference" league only have 45 rounds in this website, as somebody
  #   hacked and edited the data, "Round 33" and "Round 37" contain 18 matches while normal
  #   should be 12 matches per round. These part/section can be deleted if there is no such
  #   human error beyond the following seasons.
  #   lnk$'80' is 'lid=80', means the id of Conference is 80
  if(year == 2011) {
    lRound[[which(regexpr("lid=80",league.url)>0)]] = lRound[[which(regexpr("lid=80",league.url)>0)]][1:45,1]
    lRound[[which(regexpr("lid=80",league.url)>0)]] = matrix(lRound[[which(regexpr("lid=80",league.url)>0)]],
                                                             dimnames=list(names(lRound[[which(regexpr("lid=80",league.url)>0)]]),NULL))
  }
  # -------------------------------------------------------------------------------------
  league.url2 = lapply(lRound, function(x) {paste(rownames(x),
                gsub("Round ", "&roundNum=", x), sep = "")})
  rnd = unlist(lapply(lRound,function(x) {max(as.numeric(gsub("Round ","",x)))}))
  leagueID$Rnd = 0
  lgi = as.numeric(gsub("lid=","",lid))
  leagueID$Rnd[leagueID$ID %in% lgi] = rnd
  rm(lgi, rnd)
  
  # counting and get the rounds of competition in cups.
  cup.doc = all.doc[names(all.doc)%in%cup.url]
  txt = unlist(str_match_all(cup.doc, "[^<>]+[0-9a-zA-Z]+[^</a>]"))
  rg1 = as.numeric(regexpr("option value=",txt) > 0)
  rg2 = c(0, rg1[-length(rg1)])
  cRnd = data.frame(Rid = txt[rg1 == 1], Rname = txt[rg2 == 1])
  cRnd$Rid = as.character(cRnd$Rid)
  cRnd$Rname = as.character(cRnd$Rname)
  
  # delete duplicated match it might due to system/human error
  for(i in rev(seq(cRnd$Rname))) if(i > 1) {
    if(cRnd$Rname[i]==cRnd$Rname[i-1]) { cRnd = cRnd[-i,] } };rm(i)
  
  cRnd = cRnd[(regexpr("[0-9]{4}", cRnd$Rname) < 0),]
  rownames(cRnd) = NULL
  cRnd$lid = ifelse(cRnd$Rname == "Final", 1, 0)
  cRnd$lid[as.numeric(rownames(cRnd[(regexpr("Final",cRnd$Rname) > 0),]))] = 
    leagueID$ID[leagueID$Rnd == 0]
  ic = as.numeric(rownames(cRnd)[cRnd$lid > 0])
  cRnd$lid = rep(cRnd$lid[cRnd$lid > 0], c(ic[1], diff(ic)))
  cRnd$Rid = as.numeric(gsub("[^0-9]", "", cRnd$Rid))
  
  cup.url2 = paste(rep(cup.url, c(ic[1], diff(ic))), "&l=", cRnd$Rid, sep = "")
  splcon = paste(unlist(strsplit(cup.url2, "&sid=")))[seq(1, (length(cup.url2) * 2), 2)]
  cup.url2 = split(cup.url2, splcon)
  names(cup.url2) = NULL
  cRnd = cRnd[order(cRnd$lid),]
  nm1 = unlist(lapply(split(cRnd, cRnd$lid), function(x) {nrow(x)}))
  leagueID = leagueID[order(leagueID$ID),]
  leagueID$Rnd[leagueID$Rnd == 0] = nm1
  rm(txt, splcon, rg1, rg2, ic, nm1)
  
  # get the 90 minutes result from Chinese version webpage.
  # scoring / event time only available in Chinese version
  #    for future In-play odds modelling use.
  cup.res = lapply(seq(cup.url2),function(i) {
    txt = unlist(lapply(seq(strsplit(cup.url2[[i]],year)),function(j){
      paste(strsplit(cup.url2[[i]],year)[[j]][1],year,"&lang=en",
            strsplit(cup.url2[[i]],year)[[j]][2],sep="")}))
    gsub("http://app.en.gooooal.com/soccer/statistic/standing.do",
         "http://app.gooooal.com/cup2.do",txt)})
  attr(cup.res,"names") = unlist(lapply(cup.res, function(x) {str_match_all(
    strsplit(x, "&sid=")[[length(x)]][1], "[0-9]*[0-9]$")}))
  cup.res = cup.res[as.character(sort(as.numeric(names(cup.res))))]
  
  # filter and combine the leagues' and cups' round names
  lnk = c(league.url2, cup.url2)
  nme = lapply(lnk, function(x) {str_match_all(
    strsplit(x, "&sid=")[[length(x)]][1], "[0-9]*[0-9]$")})
  names(lnk) = as.list(unlist(nme))
  lnk = lnk[as.character(sort(as.numeric(names(lnk))))]
  
  nme = lapply(lRound, function(x) {str_match_all(
    strsplit(rownames(x), "&sid=")[[length(x)]][1], "[0-9]*[0-9]$")})
  names(lRound) = as.list(unlist(nme))
  lcRnds = c(lRound, split(cRnd$Rname, cRnd$lid))
  lcRnds = lcRnds[as.character(sort(as.numeric(names(lcRnds))))]
  lcRnds = lapply(lcRnds, function(x) { rownames(x) = NULL; as.character(x) })
  rm(nme, league.url, cup.url, league.url2, cup.url2, lid, lcid, lRound, cRnd)
  
  # scrape the tables from websites
  doc = lapply(lnk,function(x) { getURL(x,
                                        verbose = verbose, followlocation = followlocation, timeout = timeout,
                                        ftp.use.epsv = ftp.use.epsv, ftplistonly = ftplistonly,
                                        ssl.verifypeer = ssl.verifypeer, useragent = useragent)})
  tab = lapply(doc, function(x) {readHTMLTable(htmlParse(x))})
  
  # delete the aggregate table of cup home-away matches
  for(i in rev(seq(tab))) {
    for(j in rev(seq(tab[[i]]))) {
      if(length(tab[[i]][[j]]) > 0) {
        if(names(tab[[i]][[j]])[2] == "Agg.") {
          tab[[i]][j] = NULL
        }
      }
    }
  };rm(i, j)
  
  # due to sometimes website server not stable or whatsoever reason,
  #   check the number af scraped match tables tally with lnks
  num1 = unlist(lapply(seq(tab),function(i) length(tab[[i]])))
  num2 = unlist(lapply(seq(lnk),function(i) length(lnk[[i]])))
  if(any(num1 != num2)) {
    stop(cat('\n','No of Tables =',num1,'\n','No of Links  =',num2),
         "No of Tables not equal No of Links, please run again the
         downloadMatch function with higher timeout value !") }
  rm(num1, num2)
  
  for(i in seq(tab)){ attr(tab[[i]], "names") = lcRnds[[i]] }
  tab = lapply(as.list(seq(tab)), function(i) lapply(as.list(seq(tab[[i]])),
                                                     function(j) { data.frame(Round = ifelse(is.null(tab[[i]][[j]]), list(NULL),
                                                                                             names(tab[[i]][j])), tab[[i]][[j]][1:5])}))
  attr(tab, "names") = as.character(leagueID[order(as.numeric(names(lcRnds))),]$Name)
  for(i in seq(tab)){ attr(tab[[i]],"names") = lcRnds[[i]] }
  
  # delete the rounds of competition which has no matches
  for(i in rev(seq(tab))) {
    for(j in rev(seq(tab[[i]]))) {
      if(length(tab[[i]][[j]]) == 0) tab[[i]][j] = NULL
    }
  };rm(i, j)
  
  # merge all rounds within a league/cup to be one data frame per list. Which
  #       means merge nested lists to be one-level list
  tab = lapply(tab, function(z) { Reduce(function(x, y)
    merge(x, y, all = T, incomparables = NA), z, accumulate = F)})
  
  # get table from Chinese version
  cn.doc = lapply(cup.res, function(x) { getURL(x,
                  verbose = verbose, followlocation = followlocation, timeout = timeout,
                  ftp.use.epsv = ftp.use.epsv, ftplistonly = ftplistonly,
                  ssl.verifypeer = ssl.verifypeer, useragent = useragent)})
  cup.tab = lapply(cn.doc, function(x) { readHTMLTable(htmlParse(x))})
  
  # delete the aggregate table of cup home-away matches (Chinese version)
  for(i in rev(seq(cup.tab))) {
    for(j in rev(seq(cup.tab[[i]]))) {
      if((length(cup.tab[[i]][[j]]) == 0)|(length(cup.tab[[i]][[j]]) == 5)) {
        cup.tab[[i]][j] = NULL
      }
    }
  };rm(i, j)  
  
  for(i in seq(cup.tab)){
    names(cup.tab[[i]]) = lcRnds[names(lcRnds)%in%names(cup.tab)][[i]]
    for(j in seq(cup.tab[[i]])){
      cup.tab[[i]][[j]] = na.omit(cup.tab[[i]][[j]][,c(1,4,6)])      
      names(cup.tab[[i]][[j]]) = c("KO","Score","HT")
      cup.tab[[i]][[j]]$KO = factor(gsub("KO??????",NA,cup.tab[[i]][[j]]$KO))
      cup.tab[[i]][[j]]$Score = factor(cup.tab[[i]][[j]]$Score)
      cup.tab[[i]][[j]]$HT = factor(cup.tab[[i]][[j]]$HT)
      cup.tab[[i]][[j]] = na.omit(cup.tab[[i]][[j]])
      }};rm(i,j)

  # due to sometimes website server not stable or whatsoever reason,
  #   check the number af scraped match tables tally with lnks
  num1 = unlist(lapply(seq(cup.tab),function(i) length(cup.tab[[i]])))
  num2 = unlist(lapply(seq(cup.res),function(i) length(cup.res[[i]])))
  if(any(num1 != num2)) {
    stop(cat('\n','No of Tables =',num1,'\n','No of Links  =',num2),
         "No of Tables not equal No of Links, please run again the
         downloadMatch function with higher timeout value !") }
  rm(num1, num2)
  
  # set a temporary match id to avoid disorder (Chinese version)
  for(i in seq(cup.tab)) {
    for(j in seq(cup.tab[[i]])) {
      cup.tab[[i]][[j]] = na.omit(data.frame(leagueID=i,roundID=j,cup.tab[[i]][[j]]))
      cup.tab[[i]][[j]] = data.frame(cup.tab[[i]][[j]]["leagueID"],cup.tab[[i]][[j]]["roundID"],
                                     matchID=1:nrow(cup.tab[[i]][[j]]),cup.tab[[i]][[j]][c("KO","Score","HT")])
    }
  };rm(i, j)
  
  cup.tab = lapply(cup.tab, function(z) { Reduce(function(x, y)
    merge(x, y, all = T, incomparables = NA), z, accumulate = F)})
  attr(cup.tab,"names") = as.character(leagueID[leagueID$ID %in% names(cup.tab),]$Name)
  
  # due to sometimes website server not stable or whatsoever reason,
  #   check if the length of cup.tab tally with length of cups in tab
  num1 = unlist(lapply(cup.tab,function(x) nrow(x)))
  num2 = unlist(lapply(tab[names(tab)%in%names(cup.tab)],function(x) nrow(x)))
  if(any(num1 != num2)) {
    stop(cat('\n','No of Tables =',num1,'\n','No of Links  =',num2),
         "No of Tables not equal No of Links, please run again the
         downloadMatch function with higher timeout value !") }
  rm(num1, num2)
  
  tab2 = tab
  for(i in seq(tab2)) {
    for(j in seq(cup.tab)) {
        tab2[[i]]$Score = as.character(tab2[[i]]$Score)
      if(names(tab2)[i] == names(cup.tab)[j]) {
        tab2[[i]]$Score = cup.tab[[j]]$Score
      }
    }
  };rm(i, j, cup.tab)
  
  # convert integer Unix Time format to normal date time format
  tab2 = lapply(seq(tab2),function(i) {
    y = as.numeric(unlist(str_match_all(tab2[[i]]$KO, "^([^\\(]+)(\\((.+)\\))?")))
    tab2[[i]]$KO = y[seq(0, length(y), 4)] / 1000
    tab2[[i]]$KO = as.POSIXct(tab2[[i]]$KO, tz = "", origin = "1970-01-01 00:00:00")
    tab2[[i]]$FTHG = as.numeric(gsub("-[0-9]", "", tab2[[i]]$Score))
    tab2[[i]]$FTAG = as.numeric(gsub("[0-9]-", "", tab2[[i]]$Score))
    tab2[[i]]$HTHG = as.numeric(gsub("-[0-9]", "", tab2[[i]]$HT))
    tab2[[i]]$HTAG = as.numeric(gsub("[0-9]-", "", tab2[[i]]$HT))
    tab2[[i]]$Neutral = ifelse(tab2[[i]]$Round == "Final", 1, 0)
    tab2[[i]]$Score = tab2[[i]]$HT = NULL
    names(tab2[[i]]) = gsub("KO", "Date", names(tab2[[i]]))
    tab2[[i]] })
  attr(tab2,"names") = names(tab); rm(tab)
  
  # delete entire league/cup which has no matches
  for(i in rev(seq(tab2))) {
    if(!is.data.frame(tab2[[i]])) tab2[[i]] = NULL
  }; rm(i)
  
  # -------------------------------------------------------------------------------------
  # Data in 2008, "Conference National" replaced by "Conference" to follow other sason's name
  #   can be deleted if there is no such human error beyond the following seasons.
  leagueID$Name = gsub("Conference National$","Conference",leagueID$Name)
  names(tab2) = gsub("Conference National$","Conference",names(tab2))
  for(i in seq(tab2)) {
    tab2[[i]]$Home = gsub("Norwich$","Norwich City",tab2[[i]]$Home)
    tab2[[i]]$Away = gsub("Norwich$","Norwich City",tab2[[i]]$Away)
    tab2[[i]]$Home = factor(tab2[[i]]$Home)
    tab2[[i]]$Away = factor(tab2[[i]]$Away)}
  # -------------------------------------------------------------------------------------
  # team inside leagues
  lg = as.character(subset(leagueID,(Cat=="League")&(regexpr("PO$",Name)<0))$Name)
  team = list()
  team = lapply(seq(tab2),function(i) {
    if(names(tab2)[[i]]%in%lg) {
      team[[i]] = sort(unique(c(as.character(tab2[[i]]$Home),
                                as.character(tab2[[i]]$Away))))}})
  names(team) = names(tab2)
  
  res = list(Table = tab2, leagueID = leagueID, team = team, URL = sid, lnk=lnk, Rounds = lcRnds)
  options(warn = 0)
  return(res) }