{
#-------------------------Importazione dei fattori-------------------------------#
  
  #fattore target
  GHG_emissions <- read.csv("GHG_emissions.csv", header=F, sep = ";", dec = ","); 
  #settore energia
  PE_consumption <- read.csv("PE_consumption.csv", header=F, sep = ";", dec = ",");
  #settore trasporti
  AP_transported <- read.csv("AP_transported.csv", header=F, sep = ";", dec = ",");
  GR_transported <- read.csv("GR_transported.csv", header=F, sep = ";", dec = ",");
  #settore rifiuti
  TW_generated <- read.csv("TW_generated.csv", header=F, sep = ";", dec = ",");
  #settore agricoltura
  AA_utilized <- read.csv("AA_utilized.csv", header=F, sep = ";", dec = ",");
  BA_living <- read.csv("BA_living.csv", header=F, sep = ";", dec = ",");
  #settore energia rinnovabile
  EP_renewables <- read.csv("EP_renewables.csv", header=F, sep = ";", dec = ",");
  SE_renewables <- read.csv("SE_renewables.csv", header=F, sep = ";", dec = ",");
  #settore aree verdi
  F_area <- read.csv("F_area.csv", header=F, sep = ";", dec = ",");
  T_occurrences <- read.csv("T_occurrences.csv", header=T, sep = ";", dec = ",");
  T_occurrences <- T_occurrences[,4];
  T_occurrences = as.data.frame(T_occurrences);
  
#----------------------Inizializzazione e popolamento Dataset-------------------#
  #Inizializzazione dataset [33x11]
  Dataset = as.data.frame(matrix(nrow = 33, ncol = 11, 0));
  #Assegnazione nomi righe
  rownames(Dataset) <- GHG_emissions[,1];
  #assegnazione nomi colonne
  columns_names =  c("GHG_emissions",
                     "PE_consumtpion",
                     "AP_transported",
                     "GR_transported",
                     "TW_generated",
                     "AA_utilized",
                     "BA_living",
                     "EP_renewables",
                     "SE_renewables",
                     "F_area",
                     "T_occurrences")
  colnames(Dataset) = columns_names;
  
  #Popolamento tabella 
  Dataset[,1] = GHG_emissions[,2];
  Dataset[,2] = PE_consumption[,2];
  Dataset[,3] = AP_transported[,2];
  Dataset[,4] = GR_transported[,2];
  Dataset[,5] = TW_generated[,2];
  Dataset[,6] = AA_utilized[,2];
  Dataset[,7] = BA_living[,2];
  Dataset[,8] = EP_renewables[,2];
  Dataset[,9] = SE_renewables[,2];
  Dataset[,10] = F_area[,2];
  Dataset[,11] = T_occurrences[,1];
  
  #Esportazione dataset come file csv
  write.csv(Dataset,'Dataset.csv')
  
  #-------------------------------Elaborazione Tabella------------------------------#
  
  #Rimozione stati non membri dell'UE: sono anche gli stati per è presente 
  #il maggior numero di valori NA + rimozione della prima riga: contiene dati aggregati 
  #relativi a tutti gli stati dell'UE insieme. 
  Dataset = Dataset[-c(1,30,31,32,33),]
  
  #Normalizzazione z-score della tabella escudendo valori NA
  for(i in 1:11){
    Dataset[,i]=(Dataset[,i]-mean(Dataset[,i],na.rm=T))/sd(Dataset[,i],na.rm=T)
  }
  
  #Sostituzione dei valori NA con zeri 
  Dataset[is.na(Dataset)] <- 0;
  
  #-----------------------------Visualizzazione dei dati---------------------------#
  
  #-------------------Isogramma emissioni
  
  GHG_emissions <- read.csv("GHG_emissions.csv", header=F, sep = ";", dec = ","); 
  
  GHG_emissions = GHG_emissions[-c(1,30,31,32,33),];
  
  GHG_emissions$V1 <- factor(GHG_emissions$V1, levels = GHG_emissions$V1[order(GHG_emissions$V2)]);
  
  library(ggplot2);
  
  ggplot(GHG_emissions, aes(x = V1, y = V2)) + 
    theme_bw() + geom_bar(stat = "identity", fill = "#00796B") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    labs(title = "Emissioni gas serra") + xlab("Paese") + 
    ylab("Migliaia di tonnellate");
  
  #-----------------Istogramma correlazioni
  
  cor = round(cor(Dataset),2);
  cor = as.data.frame(cor);
  
  names =  c("Emissioni",
             "Conumo Energia",
             "Passeggeri Aerei",
             "Merci Trasportate",
             "Rifiuti Generati",
             "Area Agricoltura",
             "Animali Bovini",
             "Prod. Energia Rinnovabile",
             "Utilizzo Energia Solare",
             "Area Forestale",
             "Numero Alberi");
  
  cor_tmp = matrix(nrow=11,ncol=2,0);
  cor_tmp = as.data.frame(cor_tmp);
  cor_tmp[,1] = as.factor(names);
  cor_tmp[,2] = cor[,1];
  cor_tmp = cor_tmp[-1,];
  
  cor_tmp$V1 <- factor(cor_tmp$V1, levels = cor_tmp$V1[order(cor_tmp$V2)]);
  
  ggplot(cor_tmp, aes(x = V1, y = V2)) + 
    theme_bw() + geom_bar(stat = "identity", fill = "#00796B") +
    geom_text(aes(label=V2), position = position_dodge(width = 0.9), vjust=-0.20) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    xlab("Fattori") + 
    ylab("Correlazione con Emissioni");
  
  #-------------------------------------PCA----------------------------------------#
  
  pca = princomp(Dataset, scores=TRUE);
  #Visualizzo i risultati ottenuti
  summary(pca);
  
  #Istogramma della varianza catturata da ciascuna componente
  plot(pca);
  
  #Plot della varianza spiegata cumulata
  plot(cumsum(pca$sdev^2)/sum(pca$sdev^2), type = "b", ylim = c(0,1), ylab = "% Var spiegata cumulativa", xlab = "Indice Componente");
  axis(1, at = seq(1, 10, by = 1))
  #Linea dell'80% della varianza spiegata
  segments(1,0.8,11,0.8,col="blue");
  
  #Piano principale
  biplot(pca);
  #Zoom su gruppo fattori allineato con prima comp. prncipale
  biplot(pca, expand=2, xlim = c(-0.2,1.3), ylim = c(-0.2,0.2));
  
  #Matrice dei loadings
  loadings(pca);
  
  #Rotazione delle due componenti più influenti
  varimax(loadings(pca)[,1:2]);
  
  #Grafico le osservazioni nel nuovo sistema di riferimento
  plot(pca$scores,pch=20,col="green3");l
  text(pca$scores,labels=as.character(row.names(Dataset)),pos=3)  
  
  #Provo ad usare la prima componente per classificare secondo 
  #la quantità di emissioni: vediamo come ordina i diversi paesi 
  #relativamente alla quantità di emissioni.
  pca.pt = predict(pca);
  sort(pca.pt[,1]);
  #Ottengo l'ordine che mi aspetto più o meno.
  
}