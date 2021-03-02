#--------------------------Scrpit to generate factor tables----------------------------#
{
  GHG_emissions <- read.csv("GHG_emissions.csv", header=F, sep = ";", dec = ",");
  Primary_Energy_Consumption <- read.csv("Primary_Energy_Consumption.csv", header=F, sep = ";", dec = ",");
  Final_Energy_Consumption <- read.csv("Final_Energy_Consumption.csv", header=F, sep = ";", dec = ",");
  Electricity_Production <- read.csv("Electricity_Production.csv", header=F, sep = ";", dec = ",");
  Energy_Balances <- read.csv("Energy_Balances.csv", header=F, sep = ";", dec = ","); 
  Industry_Production <- read.csv("Industry_Production.csv", header=F, sep = ";", dec = ",");
  Construction_Production <- read.csv("Construction_Production.csv", header=F, sep = ";", dec = ",");
  Forestry_Sawnwood <- read.csv("Forestry_Sawnwood.csv", header=F, sep = ";", dec = ",");
  Land_Use <- read.csv("Land_Use.csv", header=F, sep = ";", dec = ",");
  Passengers_Transported <- read.table("Passengers_Transported.csv", sep = ";", dec = ",");
  Goods_Transported <- read.table("Goods_Transported.csv", sep = ";", dec = ",");
  Waste_Production <- read.table("Waste_Production.csv", sep = ";", dec = ",");
}


{
  Dataset = as.data.frame(matrix(nrow = 33, ncol = 12, 0));
  rownames(Dataset) <- GHG_emissions[,1];
  
  columns_names =  c("Waste_Production", "Primary_Energy_Consumption",  
                     "Final_Energy_Consumption",  "Electricity_Production", 
                     "Energy_Balances", "Industry_Production", "Construction_Production",
                     "Forestry_Sawnwood"," Land_Use", "Passengers_Transported",
                     "Goods_Transported", "GHG_emissions");
  
  colnames(Dataset) = columns_names;
  
  Dataset[,1] = Waste_Production[,2];
  Dataset[,2] = Primary_Energy_Consumption[,2];
  Dataset[,3] = Final_Energy_Consumption[,2];
  Dataset[,4] = Electricity_Production[,2];
  Dataset[,5] = Energy_Balances[,2];
  Dataset[,6] = Industry_Production[,2];
  Dataset[,7] = Construction_Production[,2];
  Dataset[,8] = Forestry_Sawnwood[,2];
  Dataset[,9] = Land_Use[,2];
  Dataset[,10] = Passengers_Transported[,2];
  Dataset[,11] = Goods_Transported[,2];
  Dataset[,12] = GHG_emissions[,2];
  
  #Replacing NA values with zeros
  Dataset[is.na(Dataset)] <- 0;
  
  #Replacing NA values with mean value of the factor 
  for(i in 1:12){
   for(j in 1:33){
      if(Dataset[j,i] == 0)
        Dataset[j,i] = mean(Dataset[,i]);
    }
  }
  
  #Factors Normalization
  for(i in 1:12){
    Dataset[,i]=(Dataset[,i]-mean(Dataset[,i]))/sd(Dataset[,i])
  }
  
}