install.packages("tidyverse")
library(tidyverse)
install.packages("ggrepel")
library(ggrepel)


# Loading in csv file
spr22 <- read_csv("Spr2022.csv")

# Colorblind-friendly palette with black
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
lineVariation <- c("dashed","twodash","longdash","dotdash","solid","dotted","aa","1342","D3")

# Plot looking at alcohol production over time
ggplot(data=spr22,aes(x=day,y=alcohol*100,color=product,linetype=product))+
  geom_line(size=.75)+
  labs(title="24°Plato Fermentation Curve",
       subtitle="Alcohol",
       x="Days",
       y="Alcohol (%)",
       colour="Product",
       linetype="Product")+
  scale_color_manual(values=cbPalette)+
  scale_linetype_manual(values=lineVariation)+
  scale_x_continuous(breaks=seq(0,15,2))

ggsave("alcohol.png",width=8.5,height=5)

# Plot looking at plato reduction over time
ggplot(data=spr22,aes(x=day,y=plato,color=product,linetype=product))+
  geom_line(size=.75)+
  labs(title="24°Plato Fermentation Curve",
       subtitle="Plato",
       x="Days",
       y="°Plato",
       colour="Product",
       linetype="Product")+
  scale_color_manual(values=cbPalette)+
  scale_linetype_manual(values=lineVariation)+
  scale_x_continuous(breaks=seq(0,15,2))+
  scale_y_continuous(breaks=seq(-5,28,2.5))

ggsave("plato.png",width=8.5,height=5)

# Plot looking at pH change over time
ggplot(data=spr22,aes(x=day,y=pH,color=product,linetype=product))+
  geom_line(size=.75)+
  labs(title="24°Plato Fermentation Curve",
       subtitle="pH",
       x="Days",
       y="pH",
       colour="Product",
       linetype="Product")+
  scale_color_manual(values=cbPalette)+
  scale_linetype_manual(values=lineVariation)+
  scale_x_continuous(breaks=seq(0,15,2))

ggsave("pH.png",width=8.5,height=5)