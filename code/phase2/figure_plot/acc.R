library(ggplot2)
library(reshape2)
library(ggstar)
library(tidyverse)
library(patchwork)
library(transPlotR)
library(rtracklayer)
library(aplot)
setwd("E:\\JDY_FY\\1206")
dt <- read.csv("N_ACC_0108.csv",header = TRUE)
indel <- c(1:3,5,7)
dt[indel] <- lapply(dt[indel], as.factor)
dt$Specialty <- fct_inorder(dt$Specialty)
dt$Level <- fct_inorder(dt$Level)
dt$Reader <- fct_inorder(dt$Reader)

windowsFonts(A=windowsFont("Times New Roman"),
             B=windowsFont("Arial"))
more_unicodes <-c("■", "◆", "▲","★")
tiff(file = "N_ACC_0108.tiff", res = 600,width =4500, height = 4500)
ggplot(dt, aes(x = ACC, y =reorder(name,order), color = group,shape= Reader)) +
    geom_point(aes(x = ACC), size = 6) +
  annotate("rect", xmin = 0.4, xmax = 0.75, ymin = 0, ymax = 6.5, alpha = 0.05, fill = "#FFA50080")+
  annotate("rect", xmin = 0.4, xmax = 0.75, ymin = 6.5, ymax = 15.5, alpha = 0.05, fill = "#FF000040")+
  annotate("rect",  xmin = 0.4, xmax = 0.75, ymin = 15.5, ymax = 20.5, alpha =0.05, fill = "#00FF0040")+
  annotate("rect",  xmin = 0.4, xmax = 0.75, ymin = 20.5, ymax = 23.5, alpha = 0.05, fill = "#0000FF40")+
  labs(x = "ACC_N",
       y = "Specialty",
       color = "Protocol")+
  scale_shape_manual(values = more_unicodes)+
  guides(shape = guide_legend(override.aes = list(size = 4)))+
  geom_path(data=dt,aes(x = ACC, y =name, group = name ),
                                 color="darkgrey",arrow=arrow(angle=30,length=unit(0.1,"in")),
                                 show.legend=FALSE)+
  theme_bw()+
  scale_color_manual(values=c("#436685", "#BF2F24"))+
theme(axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 14, face = "bold"),
      panel.grid=element_blank(),
      legend.position = "bottom",
      legend.justification = "center" ,
      plot.caption = element_text(hjust = 0, vjust = -1, face = "bold", size = 16))

dev.off()