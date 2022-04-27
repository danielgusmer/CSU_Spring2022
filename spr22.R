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
avante <- spr22 %>% 
  filter(product %in% c("A1","A2","A3"))
viva <- spr22 %>% 
  filter(product %in% c("V1","V2","V3"))
merit <- spr22 %>% 
  filter(product %in% c("M1","M2","M3"))

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

# Building plots that show both alcohol and pH on the same plot
ylim.prim <- c(3,8) # pH
ylim.sec <- c(0,18) # alcohol
b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]

# Avante
ggplot(data=avante,aes(day,alcohol*100,color=product,linetype=product))+
  geom_line(aes(y = a + alcohol*100*b),size=.75)+
  geom_line(aes(y = pH),size=.75)+
  labs(title="High Gravity Fermentation Curve",
       subtitle="Renaissance Avante",
       colour="Product",
       linetype="Product")+
  scale_color_manual(values=cbPalette)+
  scale_linetype_manual(values=lineVariation)+
  scale_x_continuous("Days", breaks=seq(0,15,2))+
  scale_y_continuous("pH", sec.axis = sec_axis(~ (. -a)/b, name="Alcohol (%)"))

ggsave("avante.png",width=8.5,height=5)

# Viva
ggplot(data=viva,aes(day,alcohol*100,color=product,linetype=product))+
  geom_line(aes(y = a + alcohol*100*b),size=.75)+
  geom_line(aes(y = pH),size=.75)+
  labs(title="High Gravity Fermentation Curve",
       subtitle="Renaissance Viva",
       colour="Product",
       linetype="Product")+
  scale_color_manual(values=cbPalette)+
  scale_linetype_manual(values=lineVariation)+
  scale_x_continuous("Days", breaks=seq(0,15,2))+
  scale_y_continuous("pH", sec.axis = sec_axis(~ (. -a)/b, name="Alcohol (%)"))

ggsave("viva.png",width=8.5,height=5)

# Merit
ggplot(data=merit,aes(day,alcohol*100,color=product,linetype=product))+
  geom_line(aes(y = a + alcohol*100*b),size=.75)+
  geom_line(aes(y = pH),size=.75)+
  labs(title="High Gravity Fermentation Curve",
       subtitle="Chr. Hansen Merit",
       colour="Product",
       linetype="Product")+
  scale_color_manual(values=cbPalette)+
  scale_linetype_manual(values=lineVariation)+
  scale_x_continuous("Days", breaks=seq(0,15,2))+
  scale_y_continuous("pH", sec.axis = sec_axis(~ (. -a)/b, name="Alcohol (%)"))

ggsave("merit.png",width=8.5,height=5)