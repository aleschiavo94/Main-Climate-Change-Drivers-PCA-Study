{
  GHG_emissions <- read.csv("GHG_emissions.csv", header=F, sep = ";", dec = ","); 
  
  GHG_emissions = GHG_emissions[-c(1,30,31,32,33),];
  
  GHG_emissions$V1 <- factor(GHG_emissions$V1, levels = GHG_emissions$V1[order(GHG_emissions$V2)]);
  
  ggplot(GHG_emissions, aes(x = V1, y = V2)) + 
    theme_bw() + geom_bar(stat = "identity", fill = "#00796B") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    labs(title = "Emissioni gas serra") + xlab("Paese") + 
    ylab("Migliaia di tonnellate");
}
