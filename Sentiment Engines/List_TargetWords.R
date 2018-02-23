#LIST of working repertories

#COUNTRIES
TARGET_COUNTRY<-tolower(countrycode_data$country.name.en)
TARGET_COUNTRY<-c(TARGET_COUNTRY,"south-korean","vietnam","usa","unitedstates","europe","americas","asia","southkorea")


#INFLUENCER
TARGET_INFLUENCER.bigram<-c("nick szabo","roger ver","andreas m","m antonopoulos","andreas antonopoulos","vitalik buterin","gavin andresen",
                            "barry silbert","clif high","don tapscott","vincent briatore","nicolas cary","tuur demeester",
                            "emin gün","emin sirer","ameer rosic","charlie shrem","ledgersatus","brian krogsgard","josh olszewicz",
                            "ritchie etwaru","teeka tiwari", "mcafee")
TARGET_INFLUENCER.monogram<-c("LedgerSatus","cryptoyoda","ivanli","crypt0","boxmining","cryptographic","scandinavianlife",
                              "intelliguy","jrcornel","KingsCrown","staticinstance","steempower", "mcafee")

TARGET_INFLUENCER<-c("szabo","ver","antonopoulos","vitalik","buterin","gavin","andresen",
                     "barry","silbert","clif","don","tapscott","briatore","cary","demeester",
                     "gun","sirer","rosic","shrem","ledgersatus","krogsgard","olszewicz",
                     "etwaru","cryptoyoda","tiwari","ivanli","crypt0","boxmining","cryptographic","scandinavianlife",
                     "intelliguy","jrcornel","kingscrown","staticinstance","steempower", "mcafee")

#BITCOIN LINKED

TARGET_BITCOIN<-c(SEARCH_WORD)

#OTHER MARKET INDICATION

TARGET_MKTINDICATOR<-c()
