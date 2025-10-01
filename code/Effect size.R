library(metafor)
e <- "SOC"
z <- paste(e, "csv", sep = ".")
d <- read.csv(z)
name <- paste(e, "results.txt", sep = "")
sink(name)
v <- c("Yield","Output_value","Net_profit","Output_input_ratio")
for (i in v){
  z <- paste(i, "????", sep = "")
  m1 <- d[, paste(i, "1", sep = "")]
  m2 <- d[, paste(i, "2", sep = "")]
  m_sd1 <- d[, paste(i, "_SD1", sep = "")]
  m_sd2 <- d[, paste(i, "_SD2", sep = "")]
  png_file <- paste(i, ".png", sep = "")
  print(z)
  d2 <- escalc(measure = "ROM", data = d,
               m1i = m2, sd1i = m_sd2, n1i = n2,
               m2i = m1, sd2i = m_sd1, n2i = n1)
  r1 <- rma(yi, vi, data = d2, method = "REML")
  print(r1)
  y <- paste(e, "????", sep = "")
  print(y)
  e1 <- d[, e]
  r2 <- rma(yi, vi, mods = ~e1, data = d2, method = "REML")
  r3 <- rma(yi, vi, mods = ~e1 - 1, data = d2, method = "REML")
  print("��????��")
  print(r2)
  print("??????��")
  print(r3)
}
