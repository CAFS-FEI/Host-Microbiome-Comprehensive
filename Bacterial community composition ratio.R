#######phy
library(openxlsx)
library(ggplot2)
library(ggprism)
library(reshape)
library(ggalluvial)
Phylum_group <- read.xlsx("phy.xlsx",sheet = 1)
Phylum_group <- data.frame(Phylum_group)
group1 <- c(234,	68,	806,	158,	556,	1004,	1464,	19826,	19668,	241)
group2 <- c(73,433,15,117,316,285,2234,265,349,328)
t.test(group1, group2)
df<-data.frame(samples=c('a','b','c','d','e'),
               A=c(0.3,0.25,0.1,0.2,0.15),
               B=c(0.6,0.1,0.05,0.2,0.05),
               C=c(0.4,0.2,0.1,0.15,0.15),
               D=c(0.1,0.2,0.3,0.3,0.1))


Cellratio <- prop.table(x, margin = 2)

x <- array(c(as.numeric(Phylum_group[,2]),as.numeric(Phylum_group[,3])), dim = c(11, 2))


df<-data.frame(samples=as.character(Phylum_group[,1]),
               low=prop.table(x, margin = 2)[,1],
               high=prop.table(x, margin = 2)[,2])

df1 <- melt(df,id.vars = 'samples',measure.vars = c('low','high'))
names(df1)[1:2] <- c("group","X")  
df1$group <- as.factor(df1$group)
df1$group= factor(df1$group,levels = c("Others","Gemmatimonadota","Acidobacteriota",
                                       "Cyanobacteria","Actinobacteriota","Spirochaetota",
                                       "Bacteroidota","Desulfobacterota",
                                       "Fusobacteriota","Firmicutes","Proteobacteria"))

ggplot(df1, aes( x = X,y=100 * value,fill = group))+
  geom_col(position = 'stack', width = 0.6)

ggplot(df1, aes( x = X,y=100 * value,fill = group,
                 stratum = group, alluvium = group))+
  geom_stratum(width = 0.5, color='white')+
  geom_alluvium(alpha = 0.5,
                width = 0.5,
                curve_type = "linear")

ggplot(df1, aes( x = X,y=100 * value,fill = group,
                 stratum = group, alluvium = group))+
  geom_stratum(width = 0.7, color='white')+
  geom_alluvium(alpha = 0.5,
                width = 0.7,
                color='white',
                size = 1,
                curve_type = "linear")+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Samples",y="Relative Abundance(%)",
       fill="group")+
  guides(fill=guide_legend(keywidth = 0.8, keyheight = 0,8)) +
  theme_prism(palette = "candy_bright",
              base_fontface = "plain", 
              base_family = "serif", 
              base_size = 16, 
              base_line_size = 0.8)+
  scale_fill_manual(values = allcolour)+
  theme(axis.title.x=element_text(family = "serif",colour='black', size=12,face = "bold"),
        axis.title.y=element_text(family = "serif",colour='black', size=12,face = "bold"),
        axis.text=element_text(family = "serif",colour='black',size=12,face = "bold"),
        legend.text = element_text(size = 7))

######genues
geneus_group <- read.xlsx("gen.xlsx",sheet = 1)
geneus_group <- data.frame(geneus_group)

group1 <- c(923,1361,31429,2188,19283,2965,1719,1071,2473,3893)
group2 <- c(304,2746,56,168,57799,1387,4622,1290,390,2319)
t.test(group1, group2)
x <- array(c(as.numeric(geneus_group[,2]),as.numeric(geneus_group[,3])), dim = c(28, 2))


df<-data.frame(samples=as.character(geneus_group[,1]),
               low=prop.table(x, margin = 2)[,1],
               high=prop.table(x, margin = 2)[,2])

df1 <- melt(df,id.vars = 'samples',measure.vars = c('low','high'))
names(df1)[1:2] <- c("group","X")  
df1$group <- as.factor(df1$group)
df1$group= factor(df1$group,levels = geneus_group[,1])

ggplot(df1, aes( x = X,y=100 * value,fill = group,
                 stratum = group, alluvium = group))+
  geom_stratum(width = 0.7, color='white')+
  geom_alluvium(alpha = 0.5,
                width = 0.7,
                color='white',
                size = 1,
                curve_type = "linear")+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Samples",y="Relative Abundance(%)",
       fill="group")+
  guides(fill=guide_legend(keywidth = 0.8, keyheight = 0,8)) +
  theme_prism(palette = "candy_bright",
              base_fontface = "plain", 
              base_family = "serif", 
              base_size = 16, 
              base_line_size = 0.8)+
  scale_fill_manual(values = allcolour)+
  theme(axis.title.x=element_text(family = "serif",colour='black', size=12,face = "bold"),
        axis.title.y=element_text(family = "serif",colour='black', size=12,face = "bold"),
        axis.text=element_text(family = "serif",colour='black',size=12,face = "bold"),
        legend.text = element_text(size = 7))


