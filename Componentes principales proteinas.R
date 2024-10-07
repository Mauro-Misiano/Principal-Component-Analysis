library(readr)
library(ggcorrplot)
library(factoextra)
library(psych)
library(ggbiplot)
library(ggrepel)
proteinas <- read_delim("Proteinas.csv", 
                        delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                        trim_ws = TRUE)

#Observamos la base de datos
View(proteinas)

#Convertimos en un dataframe
proteinas <- as.data.frame(proteinas)

#Renombro las filas con los nombres de los países
rownames(proteinas) <- proteinas$Country
proteinas <- proteinas[-1]

#Calculamos y observamos las correlaciones entre las variables
corr <- cor(proteinas)
ggcorrplot(corr)

#Calculamos las componente principales a partir de la matriz
#de covarianzas
pc_prot <- prcomp(proteinas, center = TRUE)

#Analizamos los resultados
summary(pc_prot)

#Comparamos las suma de las varianzas de las variables 
#originales con la suma de los autovalores. 
covarianzas <- cov(proteinas)
tr(covarianzas)
sum((pc_prot$sdev)^2)

#Ahora calculamos las componente principales a partir de la matriz
#de covarianzas
pc_prot <- prcomp(proteinas, center = TRUE, scale. = TRUE)

#Analizamos los resultados
summary(pc_prot)

#Observamos las cargas (loadings) de las variables en cada 
#componente
pc_prot$rotation

#Observamos como se ubican los países en los dos componentes
#principales extraídos. 
biplot(pc_prot, scale = 0, cex = 0.6, col = c("blue4", "brown3")) 

proteinas <- cbind(proteinas, pc_prot$x)

ggplot(proteinas, aes(x = PC1, y = PC2)) + 
  geom_point(color = "blue", size = 3) +
  geom_text(label=row.names(proteinas), 
            nudge_x =0.55, nudge_y = 0.55, 
            check_overlap = T) +
  theme_bw()

