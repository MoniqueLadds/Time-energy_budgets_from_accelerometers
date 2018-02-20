winter <- read.csv("data/simualtions/winter.csv")

#combine behaviour proportions
winter$active <- winter$land.Foraging+winter$land.Travelling+
                 winter$surface.Foraging+winter$surface.Travelling+
                 winter$underwater.Foraging+winter$underwater.Travelling
winter$groom <- winter$land.Grooming+winter$surface.Grooming+
                winter$underwater.Grooming
winter$rest <- winter$land.Resting+winter$surface.Resting+
                winter$underwater.Resting
#combine place proportions
winter$water <- winter$underwater.Foraging+winter$underwater.Grooming+
                winter$underwater.Resting+winter$underwater.Travelling+
                winter$surface.Foraging+winter$surface.Grooming+
                winter$surface.Resting+winter$surface.Travelling
winter$land <- winter$land.Foraging+winter$land.Grooming+
               winter$land.Resting+winter$land.Travelling


##plot behaviours
active <- ggplot(winter, aes(active, MJ))+
                geom_point()+
                scale_x_continuous(labels = scales::percent)+
                labs(x = "Percentage time active",
                     y = bquote( 'MJ kg ' ~d^-1))+
                theme_bw()+
                theme(panel.border = element_blank(),
                  axis.title.x = element_text(size=11,vjust=0),
                  axis.title.y = element_text(size=11,vjust=2),
                  axis.text.x = element_text(size=11, colour = "black"),
                  axis.text.y = element_text(size=11, colour = "black"),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  axis.line.x = element_line(colour = "black"),
                  axis.line.y = element_line(colour = "black"),
                  legend.key=element_blank(),
                  legend.position="none")
groom <- ggplot(winter, aes(groom, MJ))+
                geom_point()+
                scale_x_continuous(labels = scales::percent)+
                labs(x = "Percentage time grooming",
                     y = bquote( 'MJ ' ~d^-1))+
                theme_bw()+
                theme(panel.border = element_blank(),
                      axis.title.x = element_text(size=11,vjust=0),
                      axis.title.y = element_text(size=11,vjust=2),
                      axis.text.x = element_text(size=11, colour = "black"),
                      axis.text.y = element_text(size=11, colour = "black"),
                      panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(), 
                      axis.line.x = element_line(colour = "black"),
                      axis.line.y = element_line(colour = "black"),
                      legend.key=element_blank(),
                      legend.position="none")
rest <- ggplot(winter, aes(rest, MJ))+
              geom_point()+
              scale_x_continuous(labels = scales::percent)+
              labs(x = "Percentage time resting",
                   y = bquote( 'MJ ' ~d^-1))+
              theme_bw()+
              theme(panel.border = element_blank(),
                    axis.title.x = element_text(size=11,vjust=0),
                    axis.title.y = element_text(size=11,vjust=2),
                    axis.text.x = element_text(size=11, colour = "black"),
                    axis.text.y = element_text(size=11, colour = "black"),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), 
                    axis.line.x = element_line(colour = "black"),
                    axis.line.y = element_line(colour = "black"),
                    legend.key=element_blank(),
                    legend.position="none")
land <- ggplot(winter, aes(land, MJ))+
              geom_point()+
              scale_x_continuous(labels = scales::percent)+
              labs(x = "Percentage time on land",
                   y = bquote( 'MJ ' ~d^-1))+
              theme_bw()+
              theme(panel.border = element_blank(),
                    axis.title.x = element_text(size=11,vjust=0),
                    axis.title.y = element_text(size=11,vjust=2),
                    axis.text.x = element_text(size=11, colour = "black"),
                    axis.text.y = element_text(size=11, colour = "black"),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(), 
                    axis.line.x = element_line(colour = "black"),
                    axis.line.y = element_line(colour = "black"),
                    legend.key=element_blank(),
                    legend.position="none")
water <- ggplot(winter, aes(water, MJ))+
                geom_point()+
                scale_x_continuous(labels = scales::percent)+
                labs(x = "Percentage time in water",
                     y = bquote( 'MJ ' ~d^-1))+
                theme_bw()+
                theme(panel.border = element_blank(),
                      axis.title.x = element_text(size=11,vjust=0),
                      axis.title.y = element_text(size=11,vjust=2),
                      axis.text.x = element_text(size=11, colour = "black"),
                      axis.text.y = element_text(size=11, colour = "black"),
                      panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(), 
                      axis.line.x = element_line(colour = "black"),
                      axis.line.y = element_line(colour = "black"),
                      legend.key=element_blank(),
                      legend.position="none")

plots <- grid.arrange(active, water, groom, land, rest, nrow = 3)

ggsave(filename = paste("figs/","Figure 4. Sensitivity analysis",".png",sep=""),
       width = 16, height = 14, units = "cm",plots)

summary(lm(winter$MJ~winter$active))
active.lm <- predict(lm(winter$MJ~winter$active), interval = "confidence")
