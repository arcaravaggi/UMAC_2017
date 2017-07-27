setwd("C:/Users/Anthony caravaggi/Dropbox/GitHub/UMAC_2017")
library(ggplot2)

sdat <- read.csv(file = "sDAT.csv")

sdat <- sdat[order(sdat$count),]

ggplot(sdat[sdat$cat %in% c("mamm", "mamm.1"),], aes(x = species, y = count))+
  geom_bar(stat = "identity") +
  facet_grid(. ~ cat, scales = "free", space = "free")

mamm <- subset(sdat[sdat$cat %in% c("mamm"),])
mamm.1 <- mamm <- subset(sdat[sdat$cat %in% c("mamm.1"),])
bat <- subset(sdat[sdat$cat %in% c("bat"),])
other <- subset(sdat[sdat$cat %in% c("other"),])

umac.plot <- function(dat,x,y,labx,laby,alt,sav,cfill){
  localenv <- environment()
  ggplot(dat, aes(x = x, y = y))+
    geom_bar(stat = "identity", fill = cfill) + 
    theme_classic() +
    theme(
      text=element_text(family="sans"),
      axis.text.y = element_text(size=12),
      axis.text.x = element_text(angle=alt, hjust=1, size=12),
      axis.title.x = element_text(size=16,face="bold"),
      axis.title.y = element_text(size=16,face="bold")
    ) +
    labs(x=labx,y=laby)
  ggsave(sav, width = 8, height = 6, units = "in")
}

umac.plot(other, x = other$species, 
          y = other$count, 
          labx="Species", laby="Count", alt=45,cfill="#FF6666",sav="test.jpeg")
