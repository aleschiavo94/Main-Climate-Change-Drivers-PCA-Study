{}
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
}