library(XML)
library(stringr)

#setwd("C:/Users/VBKobayashi/Documents/publications/waop_submission")

################################
# JOOP Crawler
################################

year1<-seq(1975, 2013)
volume1<-seq(48,86)
year2<-c(2014)
volume2<-c(87)

abst<-"abstract"

parserwiley<-function(weblink){
  rawpage<-htmlTreeParse(weblink, useInternal=TRUE)
  all.links<-xpathSApply(rawpage,"//@href")
  present.abs<-sapply(all.links, str_extract, pattern=abst, USE.NAMES=TRUE)
  all.links.abstract<-all.links[!is.na(present.abs)]
  return(as.vector(all.links.abstract))
}

myfunctionparser<-function(year,volume){
  
  webpage<-character()
  for(i in 1:length(year)){
    for(j in 1:2){
      webpage<-append(webpage,paste("http://onlinelibrary.wiley.com/doi/10.1111/joop.", year[i], ".", volume[i],".", "issue-",j,"/issuetoc", sep=""))
    }
  }
  
  regname<-"joop\\.[[:digit:]]{4}\\.[[:digit:]]{2}\\.issue\\-[1-4]"
  
  for(k in webpage){
    
    getalllinks<-unique(parserwiley(k))
    for(l in 1:length(getalllinks)){
      rawpagejournal<-htmlTreeParse(paste("http://onlinelibrary.wiley.com",getalllinks[l], sep="" ), useInternal=TRUE, encoding="utf-8")
      actualabstract<-xpathSApply(rawpagejournal, "//div[@class='para']",xmlValue)
      if(length(actualabstract!=0)){
        writeLines(actualabstract, paste(str_extract(k,regname),"-",l,".txt", sep=""))
      }
    }
  }
  
}

myfunctionparser(year2, volume2)

#########

for(i in 1:51){
  
weblink=paste("http://www.medi-jobs.de/jobsuche.php?umkreis=30&plzort=&bundesland=deutschland&land=&berufsfeld=3&beruf=104&einsatzgebiet=&jobart=&was=&seite=",i,sep="")
rawpage<-htmlTreeParse(weblink, useInternal=TRUE)
all.links<-xpathSApply(rawpage,"//div[@class='ergebnis-titel']/a")
indv_link<-sapply(all.links, function(alllinks) xmlGetAttr(alllinks, "href"))
for(j in indv_link){
  weblink2<-paste("http://www.medi-jobs.de",j,sep="")
  rawpage2<-htmlTreeParse(weblink2, useInternal=TRUE)
  jobdesc<-xpathSApply(rawpage2,"//div[@id='anzeige']")
  writeLines(xmlValue(jobdesc[[1]]),paste("./german_vacancies/",gsub("/","_",j),".txt", sep=""))
}
}

#########
for(i in paste("?page=", seq(1,551), sep="")){
  
  weblink=paste("http://de.gigajob.com/job/Krankenschwester.html",i,sep="")
  rawpage<-htmlTreeParse(weblink, useInternal=TRUE)
  all.links<-xpathSApply(rawpage,"//ul[@class='search-result gjResults bg grayM']//h3/a | //ul[@class='search-result gjResults bg ']//h3/a")
  indv_link<-sapply(all.links, function(alllinks) xmlGetAttr(alllinks, "href"))
  for(j in indv_link){
    weblink2<-paste("http://de.gigajob.com",j, sep="")
    rawpage2<-htmlTreeParse(weblink2, useInternal=TRUE)
    jobdesc<-xpathSApply(rawpage2,"//div[@class='jobtext']")
    writeLines(xmlValue(jobdesc[[1]]),paste("./german_vacancies/",gsub("[:/.]","_",j),".txt", sep=""))
  }
}
############
indv_link
for(j in indv_link){
  #j=indv_link[1]
  weblink2<-paste("http://avalon.law.yale.edu", substr(j, start=3, stop=50), sep="")
  try(rawpage2<-htmlTreeParse(weblink2, useInternal=TRUE))
  speechpara<-xpathSApply(rawpage2,"//p")
  for(i in speechpara){
    write(xmlValue(i), paste("./presidentspeeches/",gsub("[./]","_",j),".txt", sep=""), append=T)
  }
}


