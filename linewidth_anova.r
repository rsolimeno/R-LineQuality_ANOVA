### Code by R. Solimeno
# ANOVA for multiple variables
# Line width data from ImageXpert analysis of print targets
#  Desire to compare various runs at different line speeds and throughput volume
#  versus print head position (1 through 4) of 500 nozzles each (TIJ4.0)


lwdata <- read.csv('Linewidth.csv', header = TRUE)

attach(lwdata)

Speed<-factor(Speed)
Throughput<-factor(Throughput)



ResFullMod1<-lm(Head1~Throughput+Speed+Throughput:Speed);ResFullMod1
ResFullMod2<-lm(Head2~Throughput+Speed+Throughput:Speed);ResFullMod2
ResFullMod3<-lm(Head3~Throughput+Speed+Throughput:Speed);ResFullMod3
ResFullMod4<-lm(Head4~Throughput+Speed+Throughput:Speed);ResFullMod4

anova(ResFullMod1)
anova(ResFullMod2)
anova(ResFullMod3)
anova(ResFullMod4)

TukeyHSD(aov(Head1~Throughput), "Throughput", ordered = TRUE)
TukeyHSD(aov(Head2~Throughput), "Throughput", ordered = TRUE)
TukeyHSD(aov(Head3~Throughput), "Throughput", ordered = TRUE)
TukeyHSD(aov(Head4~Throughput), "Throughput", ordered = TRUE)


   layout(matrix(c(1,2,3,4),2,2))
   
   plot(TukeyHSD(aov(Head1~Throughput), "Throughput", ordered = TRUE)) 
    title(sub="Head position 1")
   plot(TukeyHSD(aov(Head2~Throughput), "Throughput", ordered = TRUE))
    title(sub="Head position 2")
   plot(TukeyHSD(aov(Head3~Throughput), "Throughput", ordered = TRUE))
    title(sub="Head position 3")
   plot(TukeyHSD(aov(Head4~Throughput), "Throughput", ordered = TRUE))
    title(sub="Head position 4")
   





