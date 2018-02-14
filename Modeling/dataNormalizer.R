toBeNormalized = read.csv(gzfile("secondary_Predictor_Pull.csv.gz"))

btcPrice = as.data.frame(toBeNormalized$KrakkenPrice)


x1 = 15000

x2 = 15700

x = x2-x1

y = 1

atan((x2-x1)/y)

atan(y/(x2-x1))
atan(y/x)
