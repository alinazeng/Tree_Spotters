# pie chart experiment
# June-1, 2021
# alina.zeng(at)ubc.c

source("analyses/script_to_plot_observations.R")

# rename column
obs_pheno2 <- obs_pheno %>% rename(Phenophase = pheno_refined)
  
png(filename="pie_observations_of_each_phenophase.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(obs_pheno2, aes(x = Phenophase, y = pheno_obs., colour = Phenophase, fill = Phenophase)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_hline(aes(yintercept = mean(pheno_obs.)),       # Adding a line for mean observation
             colour = "#9A32CD", linetype = "dashed", size=1, show.legend = F) +           # Changing the look of the line                      
  theme_bw() +
  scale_fill_brewer(palette = "PiYG", direction = -1)+   # reverse the order
  scale_color_brewer(palette = "PiYG", direction = -1)+  
  ylab("Number of observations\n") +                             
  xlab("Phenophase")  +
  coord_cartesian(ylim = c(0, 2000))+
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "plain"),                      
        panel.grid = element_blank(),  
        legend.position = "none",
        plot.margin = unit(c(1,1,1,1), units = , "cm"))+
  coord_polar("y", start=0) +theme_void()
dev.off()




png(filename="pie2_observations_of_each_phenophase.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(obs_pheno2, aes(x = Phenophase, y = pheno_obs., colour = Phenophase, fill = Phenophase)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_hline(aes(yintercept = mean(pheno_obs.)),       # Adding a line for mean observation
             colour = "#9A32CD", linetype = "dashed", size=1, show.legend = F) +           # Changing the look of the line                      
  scale_fill_brewer(palette = "PiYG", direction = -1)+   # reverse the order
  scale_color_brewer(palette = "PiYG", direction = -1)+  
  coord_polar("x", start=0) +theme_void()
dev.off()


# Add label position # hmmmm didnt work
obs_pheno <- obs_pheno %>%
  arrange(desc(class)) %>%
  mutate("lab.ypos" = cumsum(pheno_obs.) - 0.5*(pheno_obs.))

png(filename="pie3_observations_of_each_phenophase.png", 
    type="cairo", 
    units="in", 
    width=8, 
    height=6, 
    res=300)
ggplot(obs_pheno2, aes(x = "", y = pheno_obs., fill = Phenophase)) +
  geom_bar(width = 1, stat = "identity") +
  scale_fill_brewer(palette = "PiYG", direction = -1)+
  coord_polar("y", start = 0)+
 # geom_text(aes(y = lab.ypos, label = pheno_obs.), color = "white")+
  theme_void()
dev.off()