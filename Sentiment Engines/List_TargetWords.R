#LIST of working repertories

#COUNTRIES
TARGET_COUNTRY<-tolower(countrycode_data$country.name.en)
TARGET_COUNTRY<-c(TARGET_COUNTRY,"south-korean","vietnam","usa","unitedstates","europe","americas","asia","southkorea")


#INFLUENCER
TARGET_INFLUENCER.bigram<-c("nick szabo","roger ver","andreas m","m antonopoulos","andreas antonopoulos","vitalik buterin","gavin andresen",
                            "barry silbert","clif high","don tapscott","vincent briatore","nicolas cary","tuur demeester",
                            "emin gün","emin sirer","ameer rosic","charlie shrem","ledgersatus","brian krogsgard","josh olszewicz",
                            "ritchie etwaru","teeka tiwari")
TARGET_INFLUENCER.monogram<-c("LedgerSatus","CryptoYoda","ivanli","crypt0","boxmining","cryptographic","scandinavianlife",
                              "intelliguy","jrcornel","KingsCrown","staticinstance","steempower")

TARGET_INFLUENCER<-c("nick","szabo","andreas","roger","ver","andreas","antonopoulos","vitalik","buterin","gavin","andresen",
                     "barry","silbert","clif","high","don","tapscott","vincent","briatore","nicolas","cary","tuur","demeester",
                     "emin","gun","sirer","ameer","rosic","charlie","shrem","ledgersatus","brian","krogsgard","josh","olszewicz",
                     "ritchie","etwaru","cryptoyoda","teeka","tiwari","ivanli","crypt0","boxmining","cryptographic","scandinavianlife",
                     "intelliguy","jrcornel","kingscrown","staticinstance","steempower")

