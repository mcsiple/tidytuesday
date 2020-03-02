library(tidyverse)
library(reshape2)

setwd('~/Dropbox/Fun Side Projects/Scorecard')
dat <- data.frame(stringsAsFactors=FALSE,
              Candidate = c("Warren", "Sanders", "Buttegieg", "Klobuchar", "Steyer",
                            "Bloomburg", "Biden"),
                Sunrise = c(0.855, 0.915, NA, NA, NA, NA, 0.375),
          UrbanRacialEq = c(0.91, 0.88, 0.86, 0.735, 0.675, 0.5525, 0.615),
            Indivisible = c(0.95, 0.89, 0.77, 0.57, 0.75, NA, 0.5),
             Climate350 = c(1, 1, 0.5, 0.5, 1, 0.25, 0.25),
          CenterBiolDiv = c(0.8, 1, 0.4, 0.1, NA, 0.1, 0.2),
           Endorsement.Index.538 = c(95, 68, 50, 60, 2, 108, 287)/287,
                   Root = c(79, 50, 66, 22, 62, 43, 70)/79
       )

dt <- melt(dat)


png('Scorecard.png',width = 8,height = 6,units = 'in',res = 150)
dt %>% filter(Candidate != "Steyer") %>%
  ggplot(aes(x=variable,y=value,colour=-value)) +
  geom_point(size=2.5) +
  xlab('Scorecard') +
  ylab('Score') +
  scale_colour_viridis_c() +
  facet_wrap(~Candidate,scales="free_y") +
  theme_classic(base_size=16) +
  theme(axis.text.x = element_text(angle=45,hjust = 1),
        legend.position = 'none',
        strip.background = element_blank()) 
dev.off()
