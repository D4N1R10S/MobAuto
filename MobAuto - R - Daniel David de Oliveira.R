library(ggplot2)
library(dplyr)
library(lattice)
library(gplots)
library(dunn.test)

options(scipen = 10000000) #Tirar a notação científica dos gráficos

setwd("C:\\Users\\danie\\OneDrive\\Documentos\\R\\Cases\\MobAuto")

dados = read.csv("car_data_intern.csv", 
                 sep = ",", na.strings="NA",
                 stringsAsFactors=T)

#criando um data.frame espelho
dds <- dados


#Nomeando as variáveis
colnames(dds) = c('data','marca','modelo',
                  'ano','preco',
                  'km','estado',
                  'cor')

dds[!complete.cases(dds),]

summary(dds$preco)


#Substituindo os NAs pela mediana
dds[is.na(dds$preco),]$preco = median(dds$preco, na.rm = T)


#### 1A ####
preco <- dds

#Corresponde a 95% do banco de dados
preco <- preco[preco$cor %in% c('Branco', 'Cinza',
                                'Prata','Preto',
                                'Vermelho'),]


#H0: A cor do carro não importa em seu preço
#Ha: A cor do carro importa em seu preço

kruskal.test(preco$preco, preco$cor)


library(dunn.test)
dunn.test(preco$preco, preco$cor, method="holm")


#removendo fatores não usados
preco$cor = factor(preco$cor)

#boxplot da variável preço e cor
boxplot(preco ~ cor,
        data = preco, xlab = "Cor",
        ylab = "Preço",
        frame = FALSE, col = c('white','light gray', 
                               'dark gray', 'yellow', 
                               'red'),outline = F)



#Função Plotmeans
plotmeans(preco ~ cor, data = preco, frame = FALSE, xlab = "Cor",
          ylab = "Preço",main="Mean Plot with 95% CI", 
          col = 'dark blue')


#### 1B ####
#Verificando se há NAs na variável
dds[is.na(dds$marca),] 

count_data <- dds %>% 
  count(marca)

#plotando as maiores marcas vendidas nacionalmente

ggplot(count_data[count_data$n > 4000,], 
       aes(x = reorder(marca,-n), y = n )) + 
  
  geom_bar(stat = 'identity', 
           fill = '#073980', color = 'black') +
  
  geom_text(aes(label = n), vjust = -.25) + 
  labs(x = 'Marca', y = 'Quantidade', 
       title = 'As Marcas Mais Vendidas no Brasil') +
  
  theme_minimal()


# Selecionando as 10 Marcas mais Vendidas do Brasil 

#criando data.freme top10
top10 <- dds[dds$marca %in% c('Chevrolet', 'Fiat', 
                              'Ford', 'Honda', 
                              'Hyundai', 'Jeep', 
                              'Nissan', 'Renault', 
                              'Toyota', 'Volkswagen'),]


#### Nacional - Marca ####
count_data <- top10 %>% 
  count(marca)

#plotando as 10 marmas mais vendidas nacionalmente
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



#### Nacional -  Modelo ####
count_data <- dds %>% 
  count(modelo)

#Plotando os 10 modelos mais vendidos Nacionalmente
ggplot(count_data[count_data$n > 6000,], 
       aes(x = reorder(modelo,-n), y = n )) + 
  
  geom_bar(stat = 'identity',
           fill = c('black', '073980', 
                    'light blue','light blue', 
                    'blue', '7f935b',
                    'dark blue','dark blue',
                    '100403','7f935b'),color = 'black') +
  
  geom_text(aes(label = n), vjust = -.25) + 
  labs(x = 'Modelo', y = 'Quantidade', 
       title = 'Os Modelos mais Vendidos Nacionalmente') +
  
  theme_minimal()






#### São Paulo - Marca ####
sp <- dds[dds$estado == 'SP',]

count_data <- sp %>% 
  count(marca)


#Plotando as marcas mais vendidas em SP
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


##### São Paulo - Modelo ####

count_data <- sp %>% 
  count(modelo)


#plotando os modelos mais vendidos em SP
ggplot(count_data[count_data$n > 2800,], 
       aes(x = reorder(modelo,-n), y = n )) + 
  
  geom_bar(stat = 'identity', 
           fill = c('black', 'dark orange',
                    '073980', 'light blue',
                    'light blue', 'blue',
                    'dark blue', 'dark blue',
                    '100403', '7f935b'),
           
           
           color = 'black') +
  
  geom_text(aes(label = n), vjust = -.25) + 
  labs(x = 'Modelos', y = 'Quantidade',
       title = 'Os Modelos mais Vendidos na Região de São Paulo') +
  
  theme_minimal()





#### Distrito Federal - Marca ####

df <- dds[dds$estado == 'DF',]


count_data <- df %>% 
  count(marca)


#Plotando as marcas mais vendidas em DF
ggplot(count_data[count_data$n > 600,], 
       aes(x = reorder(marca,-n), y = n )) + 
  
  geom_bar(stat = 'identity', 
           fill = c('dark blue','dark red',
                    'blue','dark orange',
                    'light blue','98856d',
                    'yellow','7f935b',
                    'black','073980'),color = 'black') +
  
  geom_text(aes(label = n), vjust = -.25) + 
  labs(x = 'Marca', y = 'Quantidade', title = 'As Marcas Mais Vendidas na Região de Distrito Federal') +
  theme_minimal()



#### Distrito Federal - Modelo ####

count_data <- df %>% 
  count(modelo)

#Plotandos os modelos mais vendidos em DF
ggplot(count_data[count_data$n > 310,], 
       aes(x = reorder(modelo,-n), y = n )) + 
  
  geom_bar(stat = 'identity', 
           fill = c('black', 'dark blue',
                    '073980', 'light blue',
                    'blue', 'blue',
                    'dark red', '7f935b',
                    '073980', '073980'),
           color = 'black') +
  
  geom_text(aes(label = n), vjust = -.25) + 
  labs(x = 'Modelos', y = 'Quantidade',
       title = 'Os Modelos Mais Vendidas na Região de Distrito Federal') +
  
  theme_minimal()





#### Bahia - Marca ####
ba <- dds[dds$estado == 'BA',]


count_data <- ba %>% 
  count(marca)

#Plotando as marcas mais vendidas na BA
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
       title = 'As Marcas Mais Vendidas na Região da Bahia') +
  
  theme_minimal()





#### Bahia- Modelo ####



count_data <- ba %>% 
  count(modelo)

#Plotando os modelos mais vendidos na BA
ggplot(count_data[count_data$n > 110,], 
       aes(x = reorder(modelo,-n), y = n )) + 
  
  geom_bar(stat = 'identity', 
           fill = c('dark red', 'blue',
                    '073980', 'light blue',
                    'light blue', 'blue',
                    '7f935b', 'dark blue',
                    '100403', '7f935b'), 
           color = 'black') +
  
  geom_text(aes(label = n), vjust = -.25) + 
  labs(x = 'Modelo', y = 'Quantidade', 
       title = 'Os Modelos Mais Vendidas na Região da Bahia') +
  
  theme_minimal()





#### Rio Grande do Sul - Marca ####

count_data <- rs %>% 
  count(marca)

#Plotando as marcas mais vendidas em RS
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
       title = 'As Marcas Mais Vendidas na Região do Rio Grande do Sul') +
  
  theme_minimal()





#### Rio Grande do Sul - Modelo ####

count_data <- rs %>% 
  count(modelo)

#Plotando os modelos mais vendidos em RS
ggplot(count_data[count_data$n > 220,],
       aes(x = reorder(modelo,-n), y = n )) + 
  
  geom_bar(stat = 'identity', 
           fill = c('black', '073980',
                    'light blue', 'light blue',
                    'blue', '7f935b',
                    'dark blue', 'dark blue',
                    '7f935b', 'dark red'), 
           color = 'black') +
  
  geom_text(aes(label = n), vjust = -.25) + 
  labs(x = 'Modelo', y = 'Quantidade',
       title = 'Os Modelos Mais Vendidas na Região do Rio Grande do Sul') +
  
  theme_minimal()





#### 2 ####

#verificando a variável data
summary(dds$data)

dds$data <- as.character(dds$data)

#Modificando a Variável data nos dias de venda para o mês que foi vendido

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


dds[dds$data %in% c('2020-12-01','2020-12-02','2020-12-03',
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






#### 2) Região de São Paulo ####
sp <- dds[dds$estado == 'SP' &
            dds$modelo %in% c('Onix','HB20','Prisma',
                              'Ka', 'Sandero'),]
#Sumários
summary(sp$km)
summary(sp$preco)


#Gráfico de Dispersão - Preço e KM em São Paulo
sp %>%
  filter(preco < 70000 & km < 300000) %>%
  ggplot(aes(x = km, y = preco))+
  labs(x = 'Kilometros (km)', y = 
         'Preço', title = 'Gráfico de Dispersão - Preço e KM em São Paulo')+
  geom_point() + 
  geom_smooth() +
  theme_minimal()

#Correlação de Spearman entre Preço e KM em SP
cor <- cor.test(sp$km, sp$preco, method="spearman")
cor

#Teste Kruskal entre preço e data em SP
kruskal.test(sp$preco, sp$data)

#dunn.test entre preço e data em SP
dunn.test(sp$preco, sp$data,method="holm")

#Boxplot da variação de preço dos modelos em meses em SP
sp %>%
  filter(preco < 70000) %>%
  ggplot(aes(x = modelo,y = preco, color = data))+
  labs(x = 'Modelo', y = 'Preço', title = 'Boxplot da variação de preço dos modelos em meses em SP')+
  geom_boxplot() + 
  theme_minimal()





#### 2) Distrito Federal ####
df <- dds[dds$estado == 'DF' & dds$modelo %in% c('HB20','Gol','Onix',
                                                 'Cruze','Palio'),]

#Sumários
summary(df$preco)
summary(df$km)

# Gráfico de Dispersão - Preço e KM no Distrito Federal
df %>%
  filter(preco < 70000 & km < 300000) %>%
  ggplot(aes(x = km, y = preco))+
  labs(x = 'Kilometros (km)', y = 
         'Preço', title = 'Gráfico de Dispersão - Preço e KM no Distrito Federal')+
  geom_point() + 
  geom_smooth() +
  theme_minimal()

#Correlação de Spearman entre Preço e KM em DF
cor <- cor.test(df$km, df$preco, method="spearman")
cor

#Teste Kruskal entre preço e data em DF
kruskal.test(df$preco, df$data)

#dunn.test entre preço e data em DF
dunn.test(df$preco, df$data,method="holm")

#Boxplot da variação de preço dos modelos em meses em DF
df %>%
  filter(preco < 70000) %>%
  ggplot(aes(x = modelo,y = preco, color = data))+
  labs(x = 'Modelo', y = 'Preço', title = 'Boxplot da variação de preço dos modelos em meses no DF')+
  geom_boxplot() + 
  theme_minimal()



#### 2) Bahia ####
ba <- dds[dds$estado == 'BA' & dds$modelo %in% c('HB20','Onix','Ka',
                                                 'Sandero','HB20S'),]

#Sumários
summary(ba$km)
summary(ba$preco)

#Gráfico de Dispersão entre as variáveis Km e Preço
ba %>%
  filter(preco < 70000 & km < 300000) %>%
  ggplot(aes(x = km, y = preco))+
  labs(x = 'Kilometros (km)', y = 
         'Preço', title = 'Gráfico de Dispersão entre as variáveis Km e Preço')+
  geom_point() + 
  geom_smooth() +
  theme_minimal()

#Correlação de Spearman entre Preço e KM na BA
cor <- cor.test(ba$km, ba$preco, method="spearman")
cor

#Teste Kruskal entre preço e data na BA
kruskal.test(ba$preco, ba$data)


#Boxplot da variação de preço dos modelos em meses na BA
ba %>%
  filter(preco < 70000) %>%
  ggplot(aes(x = modelo,y = preco, color = data))+
  labs(x = 'Modelo', y = 'Preço', title = 'Boxplot da variação de preço dos modelos em meses na BA')+
  geom_boxplot() + 
  theme_minimal()


#### 2) Rio Grande do Sul ####
rs <- dds[dds$estado == 'RS' & dds$modelo %in% c('Onix','HB20','Ka',
                                                 'Sandero','Prisma'),]
#Sumários
summary(rs$preco)
summary(rs$km)

#Gráfico de Dispersão entre as variáveis Km e Preço
rs %>%
  
  filter(preco < 70000 & km < 300000) %>%
  
  ggplot(aes(x = km, y = preco))+
  labs(x = 'Kilometros (km)', y = 
         'Preço', title = 'Gráfico de Dispersão entre as variáveis Km e Preço')+
  geom_point() + 
  geom_smooth() +
  theme_minimal()

#Correlação de Spearman entre Preço e KM na BA
cor <- cor.test(rs$km, rs$preco, method="spearman")
cor

##Teste Kruskal entre preço e data em RS
kruskal.test(rs$preco, rs$data)

#dunn.test entre preço e data em RS
dunn.test(rs$preco, rs$data,method="holm")


#Boxplot da variação de preço dos modelos em meses na BA
rs %>%
  filter(preco < 70000) %>%
  ggplot(aes(x = modelo,y = preco, color = data))+
  labs(x = 'Modelo', y = 'Preço', title = 'Boxplot da variação de preço dos modelos em meses na BA')+
  geom_boxplot() + 
  theme_minimal()

