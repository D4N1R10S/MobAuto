library(ggplot2)
library(dplyr)
library(lattice)
library(gplots)

options(scipen = 10000000) 

setwd("C:\\Users\\danie\\OneDrive\\Documentos\\R\\Cases\\MobAuto")

dados = read.csv("car_data_intern.csv", sep = ",", na.strings="NA", 
                 stringsAsFactors=F)


dds <- dados

# Verificando se há dados faltantes
summary(dds)

str(dds)

 # tem, verificar variavel por variavel


#Nomeando as variáveis


colnames(dds) = c('data','marca','modelo',
                  'ano','preco',
                  'km','estado',
                  'cor')

dds[is.na(dds$preco),]$preco = median(dds$preco, na.rm = T)

summary(dds)

#### 1 A ####


# Preço

summary(dds$preco)

#Verificando se há NAs na variável
dds[is.na(dds$preco),] # tem sim

#Tendo pouco NAs substui pela mediana # São 120 NAs, beem menos 1 1% dos dados

dds[is.na(dds$preco),]$preco = median(dds$preco, na.rm = T)


mean(dds$preco)

## COR
summary(dds$cor)

dds[is.na(dds$cor),]$cor # ñ tem


cor = table(dds$cor)
cor


dds2 <- dds



cor = table(dds2$cor)
cor
barplot(cor)

dds2[dds2$preco > 1000000,]$preco = median(dds2$preco)

dds2 %>%
  ggplot(aes(reorder(cor, -preco), preco)) + 
  geom_boxplot()

median(dds2$preco)

#Categorizar

#dds2 é preco categorizado; e dds3 ñ é
dds2 <- dds
dds3 <- dds
summary(dds2$preco)
dds2[is.na(dds2$preco),]$preco = median(dds2$preco, na.rm = T)

dds2[dds2$preco <= 30000,]$preco = 1 #'Menor que 30 mil'

dds2[dds2$preco > 30000 & dds2$preco <= 55000,]$preco = 2#'Entre 30 e 55 mil'

dds2[dds2$preco > 55000 & dds2$preco <= 100000,]$preco = 3#'Entre 55 e 100 mil'

dds2[dds2$preco > 100000 & dds2$preco <= 500000,]$preco = 4 #'Entre 100 e 500 mil'



dds2[dds2$preco > 500000,]$preco = 5 #'500 mil+'


dds2 <- dds2[dds2$cor %in% c('Branco', 'Cinza',
                             'Prata','Preto',
                             'Vermelho'),]





dds3 <- dds3[dds3$cor %in% c('Branco', 'Cinza',
                             'Prata','Preto',
                             'Vermelho'),]

#Corresponde À 95 do banco de dados

preco = table(dds2$cor,dds2$preco)
preco

preco = table(dds3$cor,dds2$preco)
preco

boxplot(preco ~ cor,
        data = dds2, xlab = "Cor",
        ylab = "Dificuldade", 
        frame = FALSE, col = c('white','light gray', 
                               'dark gray', 'yellow', 
                               'red'),outline = T)


boxplot(preco ~ cor,
        data = dds3, xlab = "Cor",
        ylab = "Dificuldade", 
        frame = FALSE, col = c('white','light gray', 
                               'dark gray', 'yellow', 
                               'red'),outline = F)


#H0: Não existem diferenças entre os tempos de reação ao dirigir segundo o tipo de bebida
#consumida

#Ha: Existem diferenças entre os tempos de reação ao dirigir segundo o tipo de bebida consumida

kruskal.test(dds2$preco, dds$cor)
kruskal.test(dds3$preco, dds2$cor)

# H0: as k populações tendem a apresentar valores similares da variável em questão

# Ha: pelo menos duas das k populações tendem a apresentar valores da variável em
#questão diferentes entre si.

library(dunn.test)
dunn.test(dds2$preco, dds3$cor, method="holm")


#Verificar
library(gplots)

plotmeans(preco ~ cor, data = dds3, frame = FALSE, xlab = "Cor",
          ylab = "Preço",main="Mean Plot with 95% CI", 
          col = 'dark blue')




#### 1B ####

count_data <- dds %>% 
  count(marca)
    
      #plotando 
ggplot(count_data[count_data$n > 4000,], 
       aes(x = reorder(marca,-n), y = n )) + 
  
  geom_bar(stat = 'identity', 
           fill = '#073980', color = 'black') +
  
  geom_text(aes(label = n), vjust = -.25) + 
  labs(x = 'Marca', y = 'Quantidade', 
       title = 'As Marcas Mais Vendidas no Brasil') +
 
   theme_minimal()

### Selecionando as 10 Marcas mais Vendidas do Brasil ###

top10 <- dds[dds$marca %in% c('Chevrolet', 'Fiat', 
                              'Ford', 'Honda', 
                              'Hyundai', 'Jeep', 
                              'Nissan', 'Renault', 
                              'Toyota', 'Volkswagen'),]



count_data <- top10 %>% 
  count(marca)

      #plotando
ggplot(count_data, aes(x = reorder(marca,-n), y = n )) + 
  
  geom_bar(stat = 'identity', 
           fill = c('dark blue','dark red','blue',
                    'dark orange','light blue', '100403',
                    'light gray','7f935b','black', 
                    '073980'), color = 'black') +
  
  geom_text(aes(label = n), vjust = -.25) + 
  labs(x = 'Marca', y = 'Quantidade',
       title = 'As 10 Marcas Mais Vendidas no Brasil') +
 
   theme_minimal()


### São Paulo - Marca ###

sp <- dds[dds$estado == 'SP',]



count_data <- sp %>% 
  count(marca)


    #Plotando
ggplot(count_data[count_data$n > 5000,], 
       aes(x = reorder(marca,-n), y = n )) + 
  
  geom_bar(stat = 'identity', 
           fill = c('dark blue','dark red',
                    'blue','dark orange',
                    'light blue', '100403',
                    'light gray','7f935b',
                    'black','073980'),color = 'black') +
  
  geom_text(aes(label = n), vjust = -.25) + 
  labs(x = 'Marca', y = 'Quantidade',
       title = 'As Marcas Mais Vendidas na Concessionária de São Paulo') +
  
  theme_minimal()


### Distrito Federal - Marca ###

df <- dds[dds$estado == 'DF',]


count_data <- df %>% 
  count(marca)


    #Plotando
ggplot(count_data[count_data$n > 600,], 
       aes(x = reorder(marca,-n), y = n )) + 
  
  geom_bar(stat = 'identity', 
           fill = c('dark blue','dark red',
                    'blue','dark orange',
                    'light blue','98856d',
                    'yellow','7f935b',
                    'black','073980'),color = 'black') +
  
  geom_text(aes(label = n), vjust = -.25) + 
  labs(x = 'Marca', y = 'Quantidade', title = 'As Marcas Mais Vendidas na Concessionária do Distrito Federal') +
  theme_minimal()


### Bahia - Marca ###
ba <- dds[dds$estado == 'BA',]


count_data <- ba %>% 
  count(marca)

ggplot(count_data[count_data$n > 100,],
       aes(x = reorder(marca,-n), y = n )) + 
  
  geom_bar(stat = 'identity', 
           fill = c('dark blue','dark red',
                    'blue','dark orange',
                    'light blue', '100403',
                    'light gray','7f935b',
                    'black','073980'), color = 'black') +
  
  
  geom_text(aes(label = n), vjust = -.25) + 
  
  labs(x = 'Marca', y = 'Quantidade', 
       title = 'As Marcas Mais Vendidas na Concessionária da Bahia') +
  
  theme_minimal()


### Rio Grande do Sul - Marca ###


rs <- dds[dds$estado == 'RS',]


count_data <- rs %>% 
  count(marca)

      #Plotando

ggplot(count_data[count_data$n > 360,], 
       aes(x = reorder(marca,-n), y = n )) + 
  
  geom_bar(stat = 'identity', 
           fill = c('dark blue','pink',
                    'dark red','blue',
                    'light blue','100403',
                    'light gray','7f935b',
                    'black','073980'),color = 'black')+
  
  geom_text(aes(label = n), vjust = -.25) + 
  labs(x = 'Marca', y = 'Quantidade', 
       title = 'As Marcas Mais Vendidas na Concessionária do Rio Grande do Sul') +
  
  theme_minimal()

Mempenho = head(dds[order(-dds$km),],5)
Mempenho
barplot(Mempenho$km)


### MODELO ###
summary(dds$modelo)

#Verificando se há NAs na variável
dds[is.na(dds$modelo),] #ñ tem



### Nacional -  Modelo ###
count_data <- dds %>% 
  count(modelo)

ggplot(count_data[count_data$n > 6000,], 
       aes(x = reorder(modelo,-n), y = n )) + 
  
  geom_bar(stat = 'identity',
           fill = c('black', '073980', 
                    'light blue','light blue', 
                    'blue', '7f935b',
                    'dark blue','dark blue',
                    '100403','7f935b'),color = 'black') +
  
  geom_text(aes(label = n), vjust = -.25) + 
  labs(x = 'Marca', y = 'Quantidade', 
       title = 'Os Modelos mais Vendidos Nacionalmente') +
 
   theme_minimal()
  


### São Paulo - Modelo ###
sp <- dds[dds$estado == 'SP',]


count_data <- sp %>% 
  count(modelo)


      #plotando
ggplot(count_data[count_data$n > 2800,], 
       aes(x = reorder(modelo,-n), y = n )) + 
  
  geom_bar(stat = 'identity', 
           fill = 'dark blue', color = 'black') +
 
  geom_text(aes(label = n), vjust = -.25) + 
  labs(x = 'Marca', y = 'Quantidade',
       title = 'As Marcas Mais Vendidas na Concessionária do Rio Grande do Sul') +
  
  theme_minimal()


### Distrito Federal - Modelo ###
df <- dds[dds$estado == 'DF',]


count_data <- df %>% 
  count(modelo)

ggplot(count_data[count_data$n > 310,], 
       aes(x = reorder(modelo,-n), y = n )) + 
  
  geom_bar(stat = 'identity', 
           fill = 'dark blue', color = 'black') +
  
  geom_text(aes(label = n), vjust = -.25) + 
  labs(x = 'Marca', y = 'Quantidade',
       title = 'As Modelos Mais Vendidas na Região de Distrito Federal') +
 
   theme_minimal()


### Bahia- Modelo ###
ba <- dds[dds$estado == 'BA',]


count_data <- ba %>% 
  count(modelo)

ggplot(count_data[count_data$n > 110,], 
       aes(x = reorder(modelo,-n), y = n )) + 
  
  geom_bar(stat = 'identity', 
           fill = 'dark blue', color = 'black') +
 
  geom_text(aes(label = n), vjust = -.25) + 
  labs(x = 'Marca', y = 'Quantidade', 
       title = 'As Marcas Mais Vendidas na Concessionária do Rio Grande do Sul') +
  
  theme_minimal()


### Rio Grande do Sul - Modelo ###
rs <- dds[dds$estado == 'RS',]


count_data <- rs %>% 
  count(modelo)

ggplot(count_data[count_data$n > 220,],
       aes(x = reorder(modelo,-n), y = n )) + 
  
  geom_bar(stat = 'identity', 
           fill = 'dark blue', color = 'black') +
 
   geom_text(aes(label = n), vjust = -.25) + 
  labs(x = 'Marca', y = 'Quantidade',
       title = 'As Marcas Mais Vendidas na Concessionária do Rio Grande do Sul') +
  
  theme_minimal()


#### 2A ####

summary(dds$data)

dds$data <- as.character(dds$data)


dds[dds$data %in% c('2020-10-01','2020-10-02','2020-10-03',
                           '2020-10-04','2020-10-05','2020-10-06',
                           '2020-10-07','2020-10-08','2020-10-09',
                           '2020-10-10','2020-10-11','2020-10-12',
                           '2020-10-13','2020-10-14','2020-10-15',
                           '2020-10-16','2020-10-17','2020-10-18',
                           '2020-10-19','2020-10-20','2020-10-21',
                           '2020-10-22','2020-10-23','2020-10-24',
                           '2020-10-25','2020-10-26','2020-10-27',
                           '2020-10-28','2020-10-29', '2020-10-30',
                           '2020-10-31') ,]$data = 'Outubro'


dds[dds$data %in% c('2020-11-01','2020-11-02','2020-11-03',
                           '2020-11-04','2020-11-05','2020-11-06',
                           '2020-11-07','2020-11-08','2020-11-09',
                           '2020-11-10','2020-11-11','2020-11-12',
                           '2020-11-13','2020-11-14','2020-11-15',
                           '2020-11-16','2020-11-17','2020-11-18',
                           '2020-11-19','2020-11-20','2020-11-21',
                           '2020-11-22','2020-11-23','2020-11-24',
                           '2020-11-25','2020-11-26','2020-11-27',
                           '2020-11-28','2020-11-29','2020-11-30'
                           ) ,]$data = 'Novembro'


novo[novo$data %in% c('2020-12-01','2020-12-02','2020-12-03',
                           '2020-12-04','2020-12-05','2020-12-06',
                           '2020-12-07','2020-12-08','2020-12-09',
                           '2020-12-10','2020-12-11','2020-12-12',
                           '2020-12-13','2020-12-14','2020-12-15',
                           '2020-12-16','2020-12-17','2020-12-18',
                           '2020-12-19','2020-12-20','2020-12-21',
                           '2020-12-22','2020-12-23','2020-12-24',
                           '2020-12-25','2020-12-26','2020-12-27',
                           '2020-12-28','2020-12-29','2020-12-30',
                           '2020-12-31') ,]$data = 'Dezembro'



#### 2B #####


cor <- cor.test(dds$km, dds$preco, method="kendall")
cor


plot(dds$km, dds$preco)



mob <- dds[dds$estado == c('SP', 'DF', 'BA', 'RS'),]

#Faixa de preços

summary(mob$preco)
summary(mob$km)

# o valor de 70000 escolhido olhando pro 3 quartil da variável
mob %>%
  filter(preco < 70000 & km < 300000) %>%
  ggplot(aes(x = km, y = preco))+
  labs(x = 'Kilometros (km)', y = 
        'Preço', title = 'Gráfico de Dispersão entre as variáveis Km e Preço')+
  geom_point() + 
  geom_smooth() +
  theme_minimal()

#correlações
cors <- cor.test(mob$km, mob$preco, method="spearman")
cors




sp <- dds[dds$estado == 'SP' &
            dds$modelo %in% c('Onix','HB20','Prisma',
                          'Ka', 'Sandero', 'Corolla', 
                          'HB20S','Fox', 'Renegade', 
                          'Fit'),]

summary(sp$km)
summary(sp$preco)

sp %>%
  filter(preco < 70000 & km < 300000) %>%
  ggplot(aes(x = km, y = preco))+
  labs(x = 'Kilometros (km)', y = 
         'Preço', title = 'Gráfico de Dispersão entre as variáveis Km e Preço')+
  geom_point() + 
  geom_smooth() +
  theme_minimal()


cor <- cor.test(sp$km, sp$preco, method="spearman")
cor



df <- dds[dds$estado == 'DF' & dds$modelo %in% c('HB20','Gol','Onix',
                                             'Cruze','Palio','Ka',
                                             'Sandero','Corolla','Voyage',
                                             'UP!'),]

summary(df$preco)
summary(df$km)

df %>%
  filter(preco < 70000 & km < 300000) %>%
  ggplot(aes(x = km, y = preco))+
  labs(x = 'Kilometros (km)', y = 
         'Preço', title = 'Gráfico de Dispersão entre as variáveis Km e Preço')+
  geom_point() + 
  geom_smooth() +
  theme_minimal()



cor <- cor.test(df$km, df$preco, method="spearman")




ba <- dds[dds$estado == 'BA' & dds$modelo %in% c('HB20','Onix','Ka',
                                             'Sandero','HB20S','Kwid',
                                             'Argo','Gol','Renegade',
                                             'EcoSport'),]

summary(ba$km)
summary(ba$preco)


ba %>%
  filter(preco < 70000 & km < 300000) %>%
  ggplot(aes(x = km, y = preco))+
  labs(x = 'Kilometros (km)', y = 
         'Preço', title = 'Gráfico de Dispersão entre as variáveis Km e Preço')+
  geom_point() + 
  geom_smooth() +
  theme_minimal()


cor <- cor.test(ba$km, ba$preco, method="spearman")
cor

boxplot(ba$km, col = 'blue', 
        main = 'Boxplot de km',outline = F)

summary(ba$km)





rs <- dds[dds$estado == 'RS' & dds$modelo %in% c('Onix','HB20','Ka',
                                             'Sandero','Prisma','Logan',
                                             'Gol', 'HB20S','Uno','Corolla'),]

rs %>%
  
  filter(preco < 70000 & km < 300000) %>%
  
  ggplot(aes(x = km, y = preco))+
  labs(x = 'Kilometros (km)', y = 
         'Preço', title = 'Gráfico de Dispersão entre as variáveis Km e Preço')+
  geom_point() + 
  geom_smooth() +
  theme_minimal()


cor <- cor.test(rs$km, rs$preco, method="spearman")
cor



boxplot(preco + km ~ estado, col = 'blue', data = mob, outline = F)







kendall <- cor.test(rs$data | rs$preco, method="kendall")
kendall




