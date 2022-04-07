install.packages("tidyverse")
library(tidyverse)
install.packages("ggrepel")
library(ggrepel)


# Loading in csv file
spr22 <- read_csv("Spr2022.csv")

# Filters to pull historical data in and out.
# Previous Gusmer experiment data listed as V0
csu <- spr22 %>% 
  filter(product %in% c("A1","A2","A3","M1","M2","M3","V1","V2","V3"))
v0 <- spr22 %>% 
  filter(product %in% c("V0"))

# Colorblind-friendly palette and line types
cbPalette <- c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#999999","#666666")
lineVariation <- c("dashed","twodash","longdash","dotdash","solid","dotted","aa","1342","D3","D4")

# Plot looking at alcohol production over time
ggplot(data=spr22,aes(x=day,y=alcohol*100,color=product,linetype=product))+
  geom_smooth(data=v0)+
  geom_line(data=csu,size=.75)+
  labs(title="High Gravity Fermentation Curve",
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
  geom_smooth(data=v0)+
  geom_line(data=csu,size=.75)+
  labs(title="High Gravity Fermentation Curve",
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
  geom_smooth(data=v0)+
  geom_line(data=csu,size=.75)+
  labs(title="High Gravity Fermentation Curve",
       subtitle="pH",
       x="Days",
       y="pH",
       colour="Product",
       linetype="Product")+
  scale_color_manual(values=cbPalette)+
  scale_linetype_manual(values=lineVariation)+
  scale_x_continuous(breaks=seq(0,15,2))

ggsave("pH.png",width=8.5,height=5)