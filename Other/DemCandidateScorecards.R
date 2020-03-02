library(tidyverse)
library(reshape2)
library(readxl)

dat <- read_excel(here::here('Other','ScorecardData.xlsx'),sheet = 2) %>%
  as.data.frame()
dat[,-1] <- as.numeric(unlist(dat[,-1]))

# Sloppy scaling
dat$Endorsements <- dat$Endorsements/max(dat$Endorsements)
dat$Root <- dat$Root/max(dat$Root)

dt <- pivot_longer(data = dat,-Candidate) %>%
        mutate(name = recode(name, 'Endorsements'='Endorsement.Index.538'))

png('Scorecard.png',width = 8,height = 6,units = 'in',res = 150)
dt %>% filter(Candidate != "Steyer") %>%
  ggplot(aes(x=name,y=value,colour=value)) +
  geom_point(size=2.5) +
  xlab('Scorecard') +
  ylab('Score') +
  scale_colour_viridis_c(direction = -1) +
  facet_wrap(~Candidate,scales="free_y") +
  theme_classic(base_size=16) +
  theme(axis.text.x = element_text(angle=45,hjust = 1),
        legend.position = 'none',
        strip.background = element_blank()) 
dev.off()
