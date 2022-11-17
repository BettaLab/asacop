#### Cargamos las librerías que vamos a utilizar ####

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, # Librería que nos permite trabajar con datos tabulares
               cluster, # Algoritmos para hacer clusters
               factoextra, # Visualización de clusters
               xlsx, # Para importar y exportar formato de excel
               haven, # Para manipular archivos .sav (SPSS)
               ggcorrplot, # Para visualizar correlaciones
               igraph, # Para trabajar con redes o grafos
               networkD3 #Para visualizar redes o grafos utilizando Js (D3.js)
               )

#### Leemos los datos que vamos a utilizar ####

##### Creamos un objeto "datos" #####
data_raw <- read_csv("data/encuestas.csv")

#Miramos los datos que cargamos en memoria
View(data_raw)
glimpse(data_raw)
colnames(data_raw)

#### Preparamos los datos ####
data <- data_raw %>%
  column_to_rownames("id") %>%
  select_if(is.numeric) %>%
  scale()

#Miramos los datos limpios
View(data)


#### K-MEANS ####

##### Seed #####
# Los resultados no serán siempre los mismos. Settear una semilla nos permite
# asegurarnos cierta replicabilidad del trabajo

set.seed(42)

##### ¿Cuantos clusters nos conviene hacer? #####

# Existen distintas formas de medir la cantidad de clusters en un dataset.
# "Silhouette" es una de ellas. Mediante esta función determinamos cuan "bien"
# encaja cada "objeto u observación dentro de un cluster. Valores altos son ideales


fviz_nbclust(data, # Los datos que queremos analizar
             kmeans, # El tipo de clustering que queremos usar
             method = "silhouette", # el método con el que vamos a evaluar
             linecolor = "#862426", # elegimos un color para la linea ;)
             )
##### Clusterizamos #####

# Ahora que sabemos cuantos clusters es el ideal, ya podemos correr el kmeans


clusters <- kmeans(data, # Dataset que queremos clusterizar
                centers = 2, # Cantidad de clusters
                nstart = 25, # Cantidad de configuraciones iniciales
                iter.max = 1000 # Cantidad de iteraciones que le vamos a permitir
                ) 
str(clusters)

##### Visualizamos #####

fviz_cluster(clusters, # Clusters a visualizar
             data = data, # Datos originales
             geom = "point", # ¿Queremos ver texto o puntos?
             shape = 19,
             ellipse.type  = "norm",  # Dibujamos elipses sobre cada cluster
             show.clust.cent = TRUE, # Para ver el centroide de cada cluster
             ggtheme = theme_minimal()) +
  scale_colour_manual(values = c("#862426", "#06aed5")) +  # Paleta de colores bordes
  scale_fill_manual(values = c("#862426", "#06aed5"))   # Paleta de colores relleno

##### Le agregamos la variable "Cluster" a los datos originales #####

data_final <-  data_raw %>%
  mutate(cluster = clusters$cluster)
View(data_final)

##### Exportamos los datos con los resultados de los clusters #####

# En csv
write_csv(data_final, "data/encuestas_con_clusters.csv")

# En Excel
write.xlsx(data_final, "data/encuestas_con_clusters.xlsx",
           sheetName = "encuestas_clusters")

# En SPSS
write_sav(data_final, "data/encuestas_con_clusters.sav")

#### Análisis de los clusters ####

##### ¿Cuál es el cluster kirchnerista? #####

cluster_politica <- data_final %>%
  group_by(cluster) %>%
  summarise(cfk = mean(cfk), mm = mean(mm))
View(cluster_kirchnerista)

##### ¿Cual es el cluster más joven? #####

cluster_edad <- data_final %>%
  group_by(cluster, edad) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n / sum(n), 3))
  
cluster_edad

##### ¿Y por sexo? #####

cluster_sexo <- data_final %>%
  group_by(cluster, sexo) %>%
  summarise(n = n()) %>%
  mutate(freq = round(n / sum(n), 3))

cluster_sexo


#### Visualizaciones con graphos ####
#vemos las columnas
glimpse(data_final)



##### Realizamos una correlación simple entre las columnas #####
data_corr <- cor(data_final[, c(6:20)], method='spearman') %>%
  as.data.frame()

View(data_corr)
glimpse(data_corr)

##### Visualizamos la matriz #####
ggcorrplot(data_corr)


##### Visualizamos la correlación como un grafo#####

graph_corr <- df_corr %>%
  as.data.frame() %>%
  rownames_to_column("id") %>%
  pivot_longer(-id,names_to = "target", values_to = "weight") %>%
  rename(source = id) %>%
  filter(weight >0)

View(df_corr2)

grafo <- simpleNetwork(graph_corr, height="800px", width="800px",        
                   Source = 1,                 # column number of source
                   Target = 2,                 # column number of target
                   linkDistance = 10,          # distance between node. Increase this value to have more space between nodes
                   charge = -900,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
                   fontSize = 14,               # size of the node names
                   fontFamily = "serif",       # font og node names
                   linkColour = "#F0C808",        # colour of edges, MUST be a common colour for the whole graph
                   nodeColour = "#161C1C",     # colour of nodes, MUST be a common colour for the whole graph
                   opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
                   zoom = T                    # Can you zoom on the figure?
)

grafo







