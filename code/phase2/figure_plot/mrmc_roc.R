library(MRMCaov)
library(dplyr)
library(pROC)
library(ggplot2)
data <- read.csv("mrmc_2_roc.csv",header = TRUE)
test_delong <- mrmc(empirical_auc(T_truth,T_read),
                    test = study_device,
                    reader = fixed(Reader),
                    case = study_num,
                    data = data,
                    cov = "jackknife")
print(test_delong,10)
summary(test_delong)

multi_roc <- with(data,{
  roc_curves(N_truth,N_read,
             groups = list(Reader = Reader,Procotol = study_device))
})
tiff(file = "N_reader.tiff", res = 600,width =4500, height = 4500)
plot(multi_roc,emp_points = TRUE)
dev.off()


