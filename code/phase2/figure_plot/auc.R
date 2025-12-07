library(ggplot2)
library(reshape2)
library(ggstar)
library(tidyverse)
library(patchwork)
library(forcats)
library(ggsci) 
setwd("E:\\JDY_FY\\1206")
dt <- read.csv("N_AUC.csv",header = TRUE)
indel <- c(2,3,6)
dt[indel] <- lapply(dt[indel], as.factor)
dt$Specialty <- fct_inorder(dt$Specialty)
dt$Level <- fct_inorder(dt$Level)
dt$Reader <- fct_inorder(dt$Reader)
unique_readers <- levels(dt$Reader)  # 获取所有唯一的 Reader 值
custom_colors <- setNames(c( "#000080CC","#FF8000CC", "#800000CC", "#008000CC"),
                             unique_readers)

more_unicodes <- c("■","▲","◆","★")
tiff(file = "N_AUC_03_new.tiff", res = 600,width =4500, height = 4500)
ggplot(dt,aes(Convention_AUC_N,DynaFusion_AUC_N))+
  geom_point(aes(shape=Reader,color=Reader),size=4)+
  #geom_text(aes(label = name), hjust = 0.5, vjust = -1, size = 3, fontface = "bold") + 
  scale_color_manual(values = custom_colors) +  
  scale_shape_manual(values = more_unicodes)+
  geom_abline(slope = 1,intercept = 0,lty="dashed")+
  scale_x_continuous(limits = c(0.45,0.75),
                     breaks = seq(0.45,0.75,by=0.1))+
  scale_y_continuous(limits = c(0.45,0.75),
                     breaks = seq(0.45,0.75,by=0.1))+
  theme_bw()+ 
  theme(panel.grid=element_blank(),
        axis.title.x = element_text(size = 16, face = "bold"), # ????x??????????????小
        axis.title.y = element_text(size = 16, face = "bold"), # ????y??????????????小
        axis.text.x = element_text(size = 14, face = "bold"),  # ????x???潭缺?签????????小
        axis.text.y = element_text(size = 14, face = "bold"),
        legend.position = "bottom",  # ??图???贫????撞?
        legend.justification = "center" , # 图??????
        plot.caption = element_text(hjust = 0, vjust = -1, face = "italic", size = 10))
dev.off()
