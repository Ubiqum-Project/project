#LIST of working repertories

#COUNTRIES
TARGET_COUNTRY<-tolower(countrycode_data$country.name.en)
TARGET_COUNTRY<-c(TARGET_COUNTRY,"south-korean","vietnam","usa","unitedstates","europe","americas","asia","southkorea")

#### IMPACT WORDS START ####
{
  ImpactWords <- c("legalize", "banning", "ban", "bans", "regulate", "regulation", "tax", "taxes",
                   "control", "rule", "law", "restriction", "regulating", "ordinance", "regulate",
                   "norm", "policy", "legislation", "limitation", "regularization", 
                   "regularisation", "bylaw", "regulator", "governance", "regulatory",
                   "decree", "provisions", "provision", "standards", "requirements", "controls",
                   "supervision", "regulators", "statute", "statutes", "procedure",
                   "administrative law", "rulemaking", "govern", "decalogue", "metarule", "rulable",
                   "overrule", "governable", "unruled", "state", "limiting", "regulations",
                   "guidelines", "laws", "measures", "procedures", "requiring", "licit",
                   "oversight", "stricter", "requires", "taxation", "restrict", "lawlessness",
                   "restrictions", "tighter", "regiment", "policies", "restrictive",
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
                   "rule of law", "bewield", "code enforcement", "mandatory",
                   "mandator", "jurisconsult", "preterlegal", "nomic", "rule of evidence",
                   "black letter law", "golden rule", "sociolegal", "blawg", "salic law",
                   "revertible", "lawing", "astrolaw", "intralegal", "legist", "lawmonger",
                   "executive order", "grind rule", "rule out", "gag order", "law of nature",
                   "canon law", "letter of law", "constitutional amendment", 
                   "power of appointment", "court order", "judicial review",
                   "consent decree", "zero tolerance", "point of order", "regulated",
                   "deregulation", "public policy", "order in council", "regulates", 
                   "eminent domain", "governing", "measure length", "special master", 
                   "directive", "legislating", "life estate", "legal system",
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
                   "prohibitory", "abolition", "closure", "suppression", "prevented",
                   "preclude", "clampdown")
}

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
#INSTITUTION
TARGET_INSTITUTION<-tolower(c("bank","court","government","governments","politic","politics","ceo","chairman","president"
                              ))
approbation.word<-tolower(c("authorize", "accept", "approbation", "approval", "approve", "sanction", "authorise", "noncommittal", 
                       "laudatory", "laconic", "jocular", "favourable", "favorable", "approbative", "cryptic", "approving", 
                       "ratify", "acceptance", "consent", "authorization", "acquiescence", "acquiesce", "acceptation", "admit",
                       "admissible", "approver", "permission", "admission", "acceptant", "permit", "allowance", "unacceptance",
                       "acquiescent", "allowable", "accede", "permissive", "adoption", "licence", "license", "allow", "allows", 
                       "admittable", "unaccepted", "admissive", "admittance", "recognition", "commendation", "agree", "grantee",
                       "grantees", "ratification", "acceptability", "agreement", "approof", "tolerate", "subscribe", "acceptable", 
                       "stipulate", "agreeingness", "warrant", "agreeingly", "treaty", "tolerance", "connivance", "recognize",
                       "recognises", "favour", "submission", "adopt", "nonagreed", "subscription", "receivable", "recognize",
                       "recognizes", "permit", "decriminalize", "legalise", "legitimate", "legitimize", "legitimatize", 
                       "authorize", "law", "decriminalise", "legalization", "victimless crime", "decriminalization",
                       "legislation", "licit", "criminalize", "outlawing", "legislate", "reintroduce", "prohibit",
                       "banning", "outlaw", "repeal", "abolish", "revoke", "overturn", "forbid", "oppose", "restrict", 
                       "discourage", "regulate", "enact", "invalidate", "proscribe", "curtail", "criminalise", "legitimatise",
                       "legitimize", "legitimises", "allow", "countenance", "legalizing", "legal", "legality", "legalized", 
                       "lawful", "monetise", "monetize", "license", "legally", "warrant", "probate",
                       "legalisation", "abortions", "allowable", "eviction"))

#OTHER MARKET INDICATION

TARGET_MKTINDICATOR<-tolower(c("Market", "commodity", "stock exchange", "bank", "foreign exchange market",
                       "FX", "capital market", "security", "nasdaq", "SP", "stocks", "NYSE", "LSE",
                       "New-York","futures", "NewYork", "London", "Paris", "Tokyo", "investor", "bond", "dividend",
                       "stock", "primary market", "secondary market", "IPO", "inflation", "liquidity",
                       "arbitrage", "securities", "bear", "bull", "equities", "equity", "trades", "bearish",
                       "bullish", "assets", "derivative", "derivatives", "volatility", "Eurocurrency", "Dollar", 
                       "Euro", "Yen", "Yuan", "Gold", "Sliver", "VIX", "CAC","FTSE","oil","WTI","Brent", "government bond", "Govies", "wall street", "trend"))

Marketdirection.word<-tolower(c("Highest", "Lowest", "rise", "increase", "go up", "climb", "lift", "ascend", "soar", "surge", "uprise", "grow", "mount", "raise",
                                "arise", "come up", "climb up", "jump", "advance", "heighten", "ascension", "develop", "ascent", "move up", "increment", "get up", 
                                "heave", "rise up", "boost", "emerge", "growth", "gain", "resurrect", "rising", "levitation", "scend", "move", "travel", "change", 
                                "heaving", "swell", "wave", "hike", "upheaval", "ascending", "elevation", "fall", "sink", "decline", "plunge", "fell", "go down",
                                "slump", "cut", "dip", "shed", "decrease", "descend", "drip", "descent", "come down", "teardrop", "dismiss", "fall off", "drop down", 
                                "send away", "send packing", "plummet", "reduction", "shrinkage", "abate", "shorten", "weaken", "contraction", "drop", "shrivel", "slack", 
                                "lessen", "decrement", "fall", "boil down", "dwindle", "depreciation", "minimize", "depreciate", "cutback", "lower", "mitigate", "shrinking",
                                "attrition", "lowering", "amount", "slacken", "depletion"))



