library(patchwork)
library(ggplot2)
library(patchwork)
library(ggplot2)
library(ggpubr)


all_plot <- list()
for (i in 1:13) {
  data_Blautia <- data.frame(x=BW,y=as.numeric(gene_ablusote[i,]))
  model <- lm(y~x,data=data_Blautia)
  conf_interval <- predict(model,interval="confidence")
  
  data_Blautia <- cbind(data_Blautia,conf_interval)
  P <- ggplot()
  p <- ggplot(data_Blautia,aes(x=x,y=y))+geom_point()+geom_smooth(method = "lm")+
    geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.1)+
    theme_bw()+
    theme(panel.background = element_rect(fill = 'transparent'),
          panel.grid = element_blank(),
          axis.ticks.length = unit(0.2,"lines"),
          axis.ticks = element_line(color='black'),
          axis.line = element_line(colour = "black"),
          axis.title.x=element_text(family = "serif",colour='black', size=10,face = "bold"),
          axis.title.y=element_text(family = "serif",colour='black', size=10,face = "bold"),
          axis.text=element_text(family = "serif",colour='black',size=10,face = "bold"),
          plot.title = element_text(family = "serif",size = 10,face = "bold",colour = "black",hjust = 0.5)) +
    labs(x = NULL, y = NULL)+
    stat_cor(label.x = 1600,size = 4,
             aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"),group = 1),
             color = "black",method = "spearman",
             label.x.npc = "left")
  all_plot[[i]]<- p
}

for (i in 1:13) {
  if(i==1){p<-all_plot[[1]]}
  else{
    p <- p +all_plot[[i]]
  }
}

p=p+plot_layout(ncol = 4)#(round(sqrt(k)))
p