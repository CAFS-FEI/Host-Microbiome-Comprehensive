data <- read.table("genus_20.txt",header = TRUE,row.names = 1,sep = "\t")
data<- as.matrix(data)
data <- prop.table(data,margin = 1)#
rownames(data) <- group[,1]
group <- read.table("sample.txt",header = FALSE,sep = "\t")
group <- group[-1,]
library(tidyverse)
#data <- data*100
#data <- data %>% filter(apply(data,1,mean) > 1)

data <- t(data)
data1 <- data.frame(data,group$V2)
colnames(data1) <- c(colnames(data),"Group")
data1$Group <- as.factor(data1$Group)

diff <- data1 %>%
  select_if(is.numeric) %>%
  map_df(~ broom::tidy(t.test(. ~ Group,data = data1)), .id = 'var')

#diff$p.value <- p.adjust(diff$p.value,"bonferroni")
#diff <- diff %>% filter(p.value < 2)

#diff1 <- data1 %>%
#select_if(is.numeric) %>%
#map_df(~ broom::tidy(wilcox.test(. ~ Group,data = data1)), .id = 'var')

#diff1$p.value <- p.adjust(diff1$p.value,"bonferroni")
#diff1 <- diff %>% filter(p.value < 0.05)


abun.bar <- data1[,c(diff$var,"Group")] %>%
  gather(variable,value,-Group) %>%
  group_by(variable,Group) %>%
  summarise(Mean = mean(value))


diff.mean <- diff[,c("var","estimate","conf.low","conf.high")]
diff.mean$Group <- c(ifelse(diff.mean$estimate >0,levels(data1$Group)[1],
                            levels(data1$Group)[2]))
diff.mean <- diff.mean[order(diff.mean$estimate,decreasing = TRUE),]

library(ggplot2)
cbbPalette <- c("blue", "red")#c("#E69F00", "#56B4E9")
abun.bar$variable <- factor(abun.bar$variable,levels = rev(diff.mean$var))


p1 <- ggplot(abun.bar,aes(variable,Mean,fill = Group)) +
  scale_x_discrete(limits = levels(diff.mean$var)) +
  coord_flip() +
  xlab("") +
  ylab("Mean proportion (%)") +
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(),
        axis.ticks.length = unit(0.4,"lines"),
        axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(family = "serif", size=10,face = "bold",color='black'),
        axis.text=element_text(family = "serif",size=9,face = "bold",color='black'),
        legend.title=element_blank(),
        legend.text=element_text(size=9,face = "bold",colour = "black",
                                 margin = margin(r = 20)),
        #legend.position = "top",#element_blank()
        #legend.position = c(-1,-0.1),
        legend.position = c(0.6,1.04),
        legend.direction = "horizontal",
        legend.key.width = unit(0.8,"cm"),
        legend.key.height = unit(0.5,"cm"))


for (i in 1:(nrow(diff.mean) - 1))
  p1 <- p1 + annotate('rect', xmin = i+0.5, xmax = i+1.5, ymin = -Inf, ymax = Inf,
                      fill = ifelse(i %% 2 == 0, 'white', 'gray95'))

p1 <- p1 +
  geom_bar(stat = "identity",position = "dodge",width = 0.7,colour = "black") +
  scale_fill_manual(values=cbbPalette)


diff.mean$var <- factor(diff.mean$var,levels = levels(abun.bar$variable))
diff.mean$p.value <- signif(diff$p.value,3)
diff.mean$p.value <- as.character(diff.mean$p.value)
p1                 


p2 <- ggplot(diff.mean,aes(var,estimate,fill = Group)) +
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(),
        axis.ticks.length = unit(0.4,"lines"),
        axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(family = "serif",colour='black', size=10,face = "bold"),
        axis.text=element_text(family = "serif",colour='black',size=10,face = "bold"),
        axis.text.y = element_blank(),
        legend.position = "none",
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(family = "serif",size = 10,face = "bold",colour = "black",hjust = 0.5)) +
  scale_x_discrete(limits = levels(diff.mean$var)) +
  coord_flip() +
  xlab("") +
  ylab("Difference in mean proportions (%)") +
  labs(title="95% confidence intervals")


for (i in 1:(nrow(diff.mean) - 1))
  p2 <- p2 + annotate('rect', xmin = i+0.5, xmax = i+1.5, ymin = -Inf, ymax = Inf,
                      fill = ifelse(i %% 2 == 0, 'white', 'gray95'))

p2 <- p2 +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(0.8), width = 0.5, size = 0.5) +
  geom_point(shape = 21,size = 3) +
  scale_fill_manual(values=cbbPalette) +
  geom_hline(aes(yintercept = 0), linetype = 'dashed', color = 'black')


p3 <- ggplot(diff.mean,aes(var,estimate,fill = Group)) +
  geom_text(aes(y = 0,x = var),label = diff.mean$p.value,
            hjust = 0,fontface = "bold",family = "serif",inherit.aes = FALSE,size = 3) +
  geom_text(aes(x = nrow(diff.mean)/2 +0.5,y = 0.85),label = "P-value ",
            srt = 90,fontface = "bold",family = "serif",size = 3) +
  coord_flip() +
  ylim(c(0,1)) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

library(patchwork)
p <- p1 + p2 + p3 + plot_layout(widths = c(6,5,2))

p       