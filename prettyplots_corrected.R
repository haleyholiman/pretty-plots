#code to plot estimates with confidence intervals
# this is customized for density estimates but can easily be modified for anything!

library(ggplot2)
library(ggpubr) #package for publication ready plots
library(patchwork) #package for easily combining different plots
library(tidyverse) 

#custom theme that I use - modify as needed or use any other ggtheme like theme_minimal(), etc
theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="#091E05"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = margin(0,0,0,0),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}
type <- as.factor(c("Point Count", "PAM")) #Categories you are comparing - my example is for survey method 
type <- fct_relevel(type, "Point Count","PAM")
mgt <- as.factor(c("control","uneven-age"))

est1 <- 5 #estimate for density
est2 <- 8
LCI_1 <- 2 #lower confidence interval for estimate 1
UCI_1 <- 7 #upper confidence interval for estimate 1
LCI_2 <- 6
UCI_2 <- 9

#make a new dataframe
(plot_df<- data.frame(type = type,
                      mgt = mgt,
                      Est = c(est1,est2),
                      LCI = c(LCI_1,LCI_2),
                      UCI = c(UCI_1,UCI_2)))
                      

(plot1 <- ggplot(plot_df, aes(x = type, y = Est)) +  
  geom_point(size = 2, color = "blue") +
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2, color = "blue") +
  labs( x = "Survey Method", y = "Abundance",
        title = "Abundance Estimates by Survey Method") +
  theme_Publication() + #custom theme from above - can replace with any other gg themes 
  labs_pubr() + 
  theme(legend.position = "none", #modify if you do want a legend
        panel.background = element_rect(fill = NULL),
        plot.background = element_rect(fill = NULL)) +
  theme(axis.title = element_text(size = 10),
        title = element_text(size = 9.5),
        axis.text = element_text(size = 10)) +
  ylim(0,10)) #customize this based on your data
  # geom_text(aes(label = round(Est_1, 2)), #this chunk is for adding annotation to the plot
  #           vjust = -1,
  #           hjust = -.3,
  #           color = "#091E05", size = 3,
  #           fontface = "bold")

#IF YOU WANT TO COMBINE MULTIPLE PLOTS --

#pretend I have a second dataframe with different estimates :)
(plot2 <- ggplot(plot_df, aes(x = mgt, y = Est)) +  
    geom_point(size = 2, color = "red") +
    geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2, color = "red") +
    labs( x = "Survey Method", y = "Abundance",
          title = "Abundance Estimates by Management") +
    theme_Publication() + #custom theme from above - can replace with any other gg themes 
    labs_pubr() + 
    theme(legend.position = "none", #modify if you do want a legend
          panel.background = element_rect(fill = NULL),
          plot.background = element_rect(fill = NULL)) +
    theme(axis.title = element_text(size = 10),
          title = element_text(size = 9.5),
          axis.text = element_text(size = 10)) +
    ylim(0,10))

#using patchwork package

#plots side by side 
(plot3 <- plot1 + plot2 +
             plot_layout(axis_titles = "collect", #only works if they have the same x & y axes
                         axes = "collect"))
#plots stacked on each other
(plot4 <- plot1 / plot2 +
    plot_layout(axis_titles = "collect", #only works if they have the same x & y axes
                axes = "collect"))

ggsave("./plots/abundanceplots.png",
       plot = plot3,
       width = 5,
       height = 5,
       units = "in",
       dpi = 300)
