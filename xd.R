dset = c(14, 15, 16, 16, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18, 18, 19, 19, 19, 20, 20, 20, 20, 20, 20, 21, 21, 22, 23, 24, 24, 29)

Q1 <- quantile(dset, 0.25)
Q3 <- quantile(dset, 0.75)
IQR <- Q3 - Q1

li <- Q1 - 1.5 * IQR
ls <- Q3 + 1.5 * IQR

atipicos <- dset[dset < li | dset > ls]
print(atipicos)

dset2 = c(14, 15, 16, 16, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18, 18, 19, 19, 19, 20, 20, 20, 20, 20, 20, 21, 21, 22, 23, 24, 24, 19)
print(mean(dset2), median(dset2))