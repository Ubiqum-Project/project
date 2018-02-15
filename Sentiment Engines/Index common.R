#INDEX COMMON#INDEX COMON DATA BASE
#Library
{
  usePackage <- function(p) 
  {
    if (!is.element(p, installed.packages()[,1]))
      install.packages(p, dep = TRUE)
    require(p, character.only = TRUE)
  }
  
  usePackage("ggplot2")
  usePackage("dplyr")
  usePackage("caret")
  usePackage("readr")
  usePackage("tidytext")
  usePackage("tidyverse")    # data manipulation & plotting
  usePackage("stringr")        # text cleaning and regular expressions
  usePackage("tidytext")
  usePackage("tm")
  usePackage("widyr")
  usePackage("countrycode")
  usePackage("wordnet")
  usePackage("scales")
  usePackage("igraph")
  usePackage("ggraph")
  usePackage("syuzhet")
  usePackage("mlr")
  
}

#DATA ---------------------------------------------------------------------------------
#SOURCE
{
  cleaned <- read_csv("~/Desktop/Bitcoin project/Project bitcoin/cleaned.csv")
  cleaned <- read_csv(gzfile("cleaned_with_dates.csv.gz"))
}
#Other information
{
  #DUMMIES for sources
    #Sources list
    source.name<-levels(factor(cleaned$name))
    #Ratio Table 
    source.ratio<-data.frame(source=source.name)
    source.ratio$ratio<-1

  #SENTIMENT INDICATOR
  Sentiment.list<-c("afinn","bing","syuzhet","nrc")
  #NEGATIVE WORDS
  NegationWords<-c("isnt","cannot","cant","wont","wasnt","ends")
  #### IMPACT WORDS START ####
  {
    ImpactWords <- c("legalize", "banning", "ban", "bans", "regulate", "regulation", "tax", "taxes",
                     "control", "rule", "law", "restriction", "regulating", "ordinance", "regulate",
                     "norm", "policy", "legislation", "limitation", "regularization", 
                     "regularisation", "bylaw", "regulator", "governance", "regulatory",
                     "decree", "provisions", "provision", "standards", "requirements", "controls",
                     "supervision", "regulators", "statute", "statutes", "procedure", "act",
                     "administrative law", "rulemaking", "govern", "decalogue", "metarule", "rulable",
                     "overrule", "governable", "unruled", "state", "limiting", "regulations",
                     "guidelines", "laws", "measures", "procedures", "requiring", "licit",
                     "oversight", "stricter", "requires", "taxation", "restrict", "lawlessness",
                     "restrictions", "tighter", "necessary", "regiment", "policies", "restrictive",
                     "legal", "restricting", "impose", "stringent", "federal", "required",
                     "compliance", "strict", "reform", "prohibit", "violate", "contravention",
                     "penalties", "protection", "blocking", "reducing", "tougher", "safeguards",
                     "enforcement", "enforce", "lawless", "mandatory", "processes", "issues",
                     "specific", "judicially", "mandate", "curfew", "recursion", "jurisdiction",
                     "danelaw", "sedition", "writ", "disciplinarian", "governor", "disapply",
                     "organic process", "game law", "nomy", "penalize", "legislative", "dictate",
                     "rulebreaker", "injunction", "dominant", "formularize", "regency", "extralegal",
                     "delinquent", "yardstick", "authoritarian", "legislate", "metalaw",
                     "clandestinely", "code of conduct", "certiorari", "authoritarianism",
                     "legislator", "rulebound", "oversit", "eviction", "compliant", "impoundment",
                     "imperious", "lawgiver", "social control", "economics", "lawbook",
                     "rule of law", "bewield", "code enforcement", "market", "mandatory",
                     "mandator", "jurisconsult", "preterlegal", "nomic", "rule of evidence",
                     "black letter law", "golden rule", "sociolegal", "blawg", "salic law",
                     "revertible", "lawing", "astrolaw", "intralegal", "legist", "lawmonger",
                     "executive order", "grind rule", "rule out", "gag order", "law of nature",
                     "canon law", "letter of law", "constitutional amendment", 
                     "power of appointment", "court order", "judicial review",
                     "consent decree", "zero tolerance", "point of order", "regulated",
                     "deregulation", "public policy", "order in council", "regulates", 
                     "eminent domain", "governing", "measure length", "special master", 
                     "directive", "legislating", "life estate", "legal system", "bar association",
                     "legal principle", "stand order", "pascal's law", "ordinances", "controlling", 
                     "ohm's law", "pauli exclusion principle", "supervisory", "lawmaking",
                     "statute book", "fundamental law", "power of attorney", "feudal law",
                     "anti authoritarianism", "bylaws", "department of justice", "statute law", 
                     "statute of limitation", "fifth amendment", "natural law", "blue sky law",
                     "policing", "diplomatic immunity", "clause", "hubble's law", "gag law",
                     "blue law", "tax system", "sus law", "international law", "supreme court",
                     "fiduciary relation", "law of land", "penal code", "law and order", "ruling",
                     "unite state code", "united states constitution", "spirit of law", "regula",
                     "judgment in personam", "standardize", "normative", "adjudication",
                     "standardizing", "administering", "prohibition", "interdict", "prohibit",
                     "banish", "forbid", "embargo", "disallow", "outlaw", "proscription",
                     "forbidding", "censor", "ostracize", "forbiddance", "interdiction", 
                     "injunction", "prohibiting", "restriction", "restrict", "banishment",
                     "suspension", "exclusion", "barred", "prohibited", "prohibits", "bar",
                     "criminalize", "decree", "edict", "nix", "bans", "restrictions", "imposed",
                     "banning-order", "criminalise", "enjoinment", "illegalize", "banned",
                     "sanctions", "impose", "rules", "suspend", "restricting", "sanction",
                     "illegal", "regulations", "strict", "stricter", "tariffs", "enforce",
                     "slapped", "guidelines", "abortion", "enforced", "halt", "imposes", "test ban", 
                     "lawless", "illegality", "illegally", "forbidden", "proscribed", "blockade",
                     "prohibitory", "abolition", "closure", "eliminate", "suppression", "prevented",
                     "preclude", "clampdown", "impossibility")
  }
  
}
#INDEX TABLE CREATION-------------------------------------------------------------------
{
  #BASE DATA FRAME : TEXT+TIME+ARTICLE+SOURCE
  {
    Text.art<-tibble(article=seq_along(cleaned$text),source=cleaned$name,text=cleaned$text,time=cleaned$cleaned)
    Text.art$source <- factor(Text.art$source)
    Text.art$time <- as.Date(Text.art$time)
    

    Text.art<-Text.art%>%
      filter(time>=as.Date("2017-11-29"))
  }
  # ---ONE WORD--- TOKENS +++++++++++++++++++++++++++++++
  {
    #ALL Word used list 
    Text.word<-Text.art %>%
      #anti_join(stop_words)%>%
      unnest_tokens(word, text)
    
    #Daily count each word (possible to change the time frame)
    Text.word.Daily.count<-Text.word%>%
      group_by(time)%>%
      count(word, sort = TRUE)
    Text.word.Daily.count<-Text.word.Daily.count[order(as.Date(Text.word.Daily.count$time)),]
    
    #Total words every day
    Text.word.Daily.Total<-Text.word%>%
      group_by(time)%>%
      summarize(nwords = n())
    Text.word.Daily.Total<-Text.word.Daily.Total[order(as.Date(Text.word.Daily.Total$time)),]
    
  }
  # ---BIGRAM--- TOKENS +++++++++++++++++++++++++++++++
  {
    #ALL Bigram used list 
    Text.bigram <- Text.art %>%
      unnest_tokens(bigram, text, token = "ngrams", n = 2)
    
    #ALL Bigram used list word separed
    Text.bigram.separated <- Text.bigram %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    
    #Daily count each bigram (possible to change the time frame)
    Text.bigram.Daily.count<-Text.bigram%>%
      group_by(time)%>%
      count(bigram, sort = TRUE)
    Text.bigram.Daily.count<-Text.bigram.Daily.count[order(as.Date(Text.bigram.Daily.count$time)),]
    
    #Total bigram every day
    Text.bigram.Daily.Total<-Text.bigram%>%
      group_by(time)%>%
      summarize(nbigram = n())
    Text.bigram.Daily.Total<-Text.bigram.Daily.Total[order(as.Date(Text.bigram.Daily.Total$time)),]
  }
  # ---TRIGRAM--- TOKENS +++++++++++++++++++++++++++++++
  {
    #ALL Bigram used list 
    Text.trigram <- Text.art %>%
      unnest_tokens(trigram, text, token = "ngrams", n = 3)
    
    #ALL trigram used list word separated
    Text.trigram.separated <- Text.trigram %>%
      separate(trigram, c("word1", "word2","word3"), sep = " ")
    
    #Word 1 with bigram
    Text.trigram.bigram<- Text.trigram.separated %>%
      unite(bigram,word2, word3, sep = " ")
    
    #Word 3 with bigram
    Text.trigram.bigram.end<- Text.trigram.separated %>%
      unite(bigram,word1, word2, sep = " ")
    
    #Daily count each trigram (possible to change the time frame)
    Text.trigram.Daily.count<-Text.trigram%>%
      group_by(time)%>%
      count(trigram, sort = TRUE)
    Text.trigram.Daily.count<-Text.trigram.Daily.count[order(as.Date(Text.trigram.Daily.count$time)),]
    
    #Total trigram every day
    Text.trigram.Daily.Total<-Text.trigram%>%
      group_by(time)%>%
      summarize(ntrigram = n())
    Text.trigram.Daily.Total<-Text.trigram.Daily.Total[order(as.Date(Text.trigram.Daily.Total$time)),]
  }
  # ---QUADRIGRAM--- TOKENS +++++++++++++++++++++++++++++++ For pure sentiment analysis
  {
    #ALL quatrigram used list 
    Text.quatrigram <- Text.art %>%
      unnest_tokens(quatrigram, text, token = "ngrams", n = 4)
    
    #ALL quatrigram used list word separated
    Text.quatrigram.separated <- Text.quatrigram %>%
      separate(quatrigram, c("word1", "word2","word3","word4"), sep = " ")
    
    #Word 1 with trigram
    Text.quatrigram.trigram<- Text.quatrigram.separated %>%
      unite(trigram,word2, word3,word4, sep = " ")
    
    #Word 3 with bigram
    Text.quatrigram.trigram.end<- Text.quatrigram.separated %>%
      unite(trigram,word1, word2,word3, sep = " ")
    
    #Daily count each quatrigram (possible to change the time frame)
    Text.quatrigram.Daily.count<-Text.quatrigram%>%
      group_by(time)%>%
      count(quatrigram, sort = TRUE)
    Text.quatrigram.Daily.count<-Text.quatrigram.Daily.count[order(as.Date(Text.quatrigram.Daily.count$time)),]
    
    #Total quatrigram every day
    Text.quatrigram.Daily.Total<-Text.quatrigram%>%
      group_by(time)%>%
      summarize(nquatrigram = n())
    Text.quatrigram.Daily.Total<-Text.quatrigram.Daily.Total[order(as.Date(Text.quatrigram.Daily.Total$time)),]
  }
}




