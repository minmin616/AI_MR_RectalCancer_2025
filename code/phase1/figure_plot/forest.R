library(compareGroups) 
library(foreign)
library(glue)
library(survminer)
library(survival)
library(dplyr)
library(tidyverse)
library(rms)
library(ggplot2)
library(pROC)
library(caret)
library(e1071)
library(foreign)
library(Matrix)
library(ROCR)
library(rio)   
library(ROCit)
library(maxstat)
setwd("E:\\JDY_FY\\0918")
data <- read.csv("forest.csv",header = TRUE,row.names = 1)
str(data)
indel <- c(1,3:13)
data[indel] <- lapply(data[indel], as.factor)
str(data)

suppressMessages(library(tidyverse))

df <- data %>% 
 mutate(Gender=factor(Gender, levels=c(0,1),labels=c("female","male")),
         Age=ifelse(Age >63,">63","<=63"),
         Age=factor(Age, levels=c(">63","<=63")),
        X654.2=factor(X654.2, levels=c(0,1),labels=c("No","Yes"))
          )

str(df)

conf_matrix<-confusionMatrix(df$Conventional_T_2,df$pT_2)
print(conf_matrix)
conf_matrix<-confusionMatrix(df$Conventional_N_2,df$pN_2)
print(conf_matrix)
conf_matrix<-confusionMatrix(df$Dynamic_T_2,df$pT_2)
print(conf_matrix)
conf_matrix<-confusionMatrix(df$Dynamic_N_2,df$pN_2)
print(conf_matrix)


G_f <-  df[df$Gender=="female", ]
conf_matrix<-confusionMatrix(G_f$Conventional_T_2,G_f$pT_2)
print(conf_matrix)
conf_matrix<-confusionMatrix(G_f$Conventional_N_2,G_f$pN_2)
print(conf_matrix)
conf_matrix<-confusionMatrix(G_f$Dynamic_T_2,G_f$pT_2)
print(conf_matrix)
conf_matrix<-confusionMatrix(G_f$Dynamic_N_2,G_f$pN_2)
print(conf_matrix)

G_m <-  df[df$Gender=="male", ]
conf_matrix<-confusionMatrix(G_m$Conventional_T_2,G_m$pT_2)
print(conf_matrix)
conf_matrix<-confusionMatrix(G_m$Conventional_N_2,G_m$pN_2)
print(conf_matrix)
conf_matrix<-confusionMatrix(G_m$Dynamic_T_2,G_m$pT_2)
print(conf_matrix)
conf_matrix<-confusionMatrix(G_m$Dynamic_N_2,G_m$pN_2)
print(conf_matrix)

A_D <-  df[df$Age==">63", ]
conf_matrix<-confusionMatrix(A_D$Conventional_T_2,A_D$pT_2)
print(conf_matrix)
conf_matrix<-confusionMatrix(A_D$Conventional_N_2,A_D$pN_2)
print(conf_matrix)
conf_matrix<-confusionMatrix(A_D$Dynamic_T_2,A_D$pT_2)
print(conf_matrix)
conf_matrix<-confusionMatrix(A_D$Dynamic_N_2,A_D$pN_2)
print(conf_matrix)

A_G <-  df[df$Age=="<=63", ]
conf_matrix<-confusionMatrix(A_G$Conventional_T_2,A_G$pT_2)
print(conf_matrix)
conf_matrix<-confusionMatrix(A_G$Conventional_N_2,A_G$pN_2)
print(conf_matrix)
conf_matrix<-confusionMatrix(A_G$Dynamic_T_2,A_G$pT_2)
print(conf_matrix)
conf_matrix<-confusionMatrix(A_G$Dynamic_N_2,A_G$pN_2)
print(conf_matrix)

S_N <-  df[df$X654.2=="No", ]
conf_matrix<-confusionMatrix(S_N$Conventional_T_2,S_N$pT_2)
print(conf_matrix)
conf_matrix<-confusionMatrix(S_N$Conventional_N_2,S_N$pN_2)
print(conf_matrix)
conf_matrix<-confusionMatrix(S_N$Dynamic_T_2,S_N$pT_2)
print(conf_matrix)
conf_matrix<-confusionMatrix(S_N$Dynamic_N_2,S_N$pN_2)
print(conf_matrix)

S_Y <-  df[df$X654.2=="Yes", ]
conf_matrix<-confusionMatrix(S_Y$Conventional_T_2,S_Y$pT_2)
print(conf_matrix)
conf_matrix<-confusionMatrix(S_Y$Conventional_N_2,S_Y$pN_2)
print(conf_matrix)
conf_matrix<-confusionMatrix(S_Y$Dynamic_T_2,S_Y$pT_2)
print(conf_matrix)
conf_matrix<-confusionMatrix(S_Y$Dynamic_N_2,S_Y$pN_2)
print(conf_matrix)

# ���ڼ�鷽ʽA
TP_A <- 90    # ������
FP_A <- 10    # ������
TN_A <- 80    # �渺��
FN_A <- 20    # �ٸ���

# ���ڼ�鷽ʽB
TP_B <- 85    # ������
FP_B <- 15    # ������
TN_B <- 75    # �渺��
FN_B <- 25    # �ٸ���

# ����Accuracy
Accuracy_A <- (TP_A + TN_A) / (TP_A + TN_A + FP_A + FN_A)
Accuracy_B <- (TP_B + TN_B) / (TP_B + TN_B + FP_B + FN_B)

# ʹ��prop.test��������������z����
test_result <- prop.test(c(TP_A + TN_A, TP_B + TN_B), 
                         c(TP_A + TN_A + FP_A + FN_A, TP_B + TN_B + FP_B + FN_B), 
                         correct = FALSE)

# ������
cat("Accuracy A:", Accuracy_A, "\n")
cat("Accuracy B:", Accuracy_B, "\n")
cat("zֵ:", test_result$statistic, "\n")
cat("pֵ:", test_result$p.value, "\n")


library(jstable)

res <- TableSubgroupMultiGLM(
  
  # 指定公式，不要乱写！
  formula = Dynamic_N_2 ~ Conventional_N_2, 
  
  # 指定哪些变量有亚�?
  var_subgroups = c("Gender","Age","X654.2"), 
  data = df #指定你的数据
)
res

plot_df <- res[,c("Variable","Count","OR","Lower","Upper","P value",
                  "P for interaction")]
plot_df
#plot_df$` ` <- paste(rep(" ", nrow(plot_df)), collapse = " ")
#plot_df[,2:6] <- apply(plot_df[,2:7],2,as.numeric)
plot_df[,c(2,6,7)][is.na(plot_df[,c(2,6,7)])] <- " "

res2 <- TableSubgroupMultiGLM(
  
  # 指定公式，不要乱写！
  formula = pN_2 ~ Dynamic_N_2, 
  
  # 指定哪些变量有亚�?
  var_subgroups = c("Gender","Age","X654.2"), 
  data = df #指定你的数据
)
res2

plot_df2 <- res2[,c("Variable","Count","OR","Lower","Upper","P value",
                  "P for interaction")]
plot_df2
#plot_df2$` ` <- paste(rep(" ", nrow(plot_df2)), collapse = " ")
#plot_df2[,2:6] <- apply(plot_df2[,2:7],2,as.numeric)
plot_df2[,c(2,6,7)][is.na(plot_df2[,c(2,6,7)])] <- " "

res3 <- TableSubgroupMultiGLM(
  
  # 指定公式，不要乱写！
  formula = pN_2 ~ Conventional_N_2, 
  
  # 指定哪些变量有亚�?
  var_subgroups = c("Age"), 
  data = df #指定你的数据
)
res3
plot_df3 <- res3[,c("Variable","Count","OR","Lower","Upper","P value",
                    "P for interaction")]
plot_df3
plot_df4 <- rbind(plot_df,plot_df3)
plot_df4 <-plot_df4[-c(8),]


plotdata <- cbind(plot_df4,plot_df2)
plotdata$`P value` <- gsub("Conventional_N_21","",plotdata$`P value`)
plotdata$`P value` <- gsub("c","",plotdata$`P value`)
plotdata$`P value` <- gsub(" = ","",plotdata$`P value`)
plotdata$`P value` <- gsub("\\(","",plotdata$`P value`)
plotdata$`P value` <- gsub("\\)","",plotdata$`P value`)
colnames(plotdata)
write.csv(plotdata,"forest_N.csv")

plotdata <- read.csv("forest_N.csv",header = TRUE,row.names = 1)
library(forestploter)
library(grid)
plotdata$"Accuracy(95% CI)" <- paste(rep(" ", 20), collapse = " ")
colnames(plotdata) <- c("Variable", "Count"," pN-        "," pN+        ","S_ACC","C_Lower","C_Upper","D_ACC","D_Lower","D_Upper","z score","p value","Accuracy (95% CI)")
plotdata[,c(2,3,4,11,12)][is.na(plotdata[,c(2,3,4,11,12)])] <- " "
plotdata[,c(5:10)] <- apply(plotdata[,c(5:10)],2,as.numeric)

tm <- forest_theme(
  #base_size = 10,        # 基本字体大小设置�?10
  #base_family = "",
  ci_pch = c(15,18),           # 置信区间点形状设置为15（实心方块）
  ci_col = c("#4575b4","#DC143C"),      # 置信区间颜色设置为黑�?
  ci_fill =  c("#4575b4","#DC143C"),     # 置信区间填充颜色设置为黑�?
  ci_alpha = 0.8,        # 置信区间透明度设置为0.8
  ci_lty = 1,            # 置信区间线型设置为实�?
  ci_lwd = 1,          # 置信区间线宽设置�?1.5
  ci_Theight = 0.2,      # 置信区间横线高度设置�?0.2
  legend_name = "Group",
  legend_position = "bottom",
  legend_value = c("Conventional ","DynaFusion "),
  xaxis_gp = gpar(lwd = 0.6, cex = 1),
  refline_gp = gpar(lwd = 1, lty = "dashed", col = "#4575b4"),
  vertline_lwd =1,   # 垂直线线宽设置为-0.1（隐藏）
  vertline_lty = "dashed",# 垂直线线型设置为虚线
  vertline_col = "grey20",  # 垂直线颜色设置为红色
  summary_fill = "blue", # 汇总部分填充颜色设置为蓝色
  summary_col = "blue",# 汇总部分颜色设置为深蓝�?
  footnote_gp = gpar(fontface = "plain", col = "black")
)

plot_sub <- forest(
  data = plotdata[,c(1,2,3,4,13,11,12)],
  lower = list(plotdata$C_Lower,plotdata$D_Lower),
  upper = list(plotdata$C_Upper,plotdata$D_Upper),
  est =list(plotdata$S_ACC,plotdata$D_ACC) ,
  ci_column = 5,
  xlim = c(0.5,1.0),
  ticks_at=c(0.5,0.7,0.9),#设置X范围，超范围带箭�?
  theme = tm
)

plot_sub

plot_sub <- plot_sub %>% 
  # 指定行加�?
  edit_plot(row = c(1,4,7,10),
            col=c(1),
            gp = gpar(fontface = "bold")) %>%
  # 最上侧画横�?
  add_border(part = c("header")) %>%
  # 最下侧画横�?
  add_border(row= 10) %>%
  # 最后一行灰色背�?
  edit_plot(row = 10, 
            which = "background",
            gp = gpar(fill = "grey"))
tiff(file = "forest_N_0912.tiff", res = 600,width =6000, height = 4500)
plot_sub
dev.off()
