#######################################################################
########### GGPLOT2: uma introdução à gramática de gráficos ###########
########### Mahayana Godoy (UFRN)                           ###########
########### Setembro de 2019                                 ##########
#######################################################################

## Preparação para acompanhar este workshop {-}

# Instale o `R`: https://cran.r-project.org/

# Instale o RStudio: https://www.rstudio.com/products/rstudio/download/

# Baixe o material do curso: http://github.com/mahayanag/linguisticaR

# Acompanhe o tutoriaR com o arquivo LinguisticaR.html

# Enquanto você vê a apresentação do curso, vá baixando os pacotes que serão usados no Workshop

## Baixar pacotes usados no curso
 
install.packages("ggplot2")
install.packages("dplyr")


## PARTE PRÁTICA: EXPLORANDO O CONJUNTO DE DADOS

# criando o conjunto de dados chamado linguistas.total com todos os dados da planilha linguistas.csv

linguistas.total = read.csv("linguistas.csv")


# Use a função head() para ver as linhas iniciais do conjunto de dados conjunto de dados

head(linguistas.total)

## Use a função str() para conhecer o conjunto de dados

str(linguistas.total)

# Clique no conjunto na área Environment para abrir o conjunto de dados em forma de planilha


# usando a função unique para ver todos os valores únicos da coluna *instituicao* no conjunto de dados

unique(linguistas.total$instituicao)

# usando a função unique para ver todos os valores únicos da coluna *status* no conjunto de dados



# MANIPULANDO O CONJUNTO DE DADOS: dplyr

# carregando o pacote dplyr

library(dplyr) 


# selecionando apenas as colunas de interesse para o projeto

linguistas.total%>%
  select(genero, idade, altura, peso, tamanho.pe, instituicao, dialeto, deslocamento, status, esporte)



# selecionando apenas participantes que indicaram que trabalham/estudam na Unicamp *OU* na UFMG *OU* na UFRN

# Marcador | indica "ou"

linguistas.total%>%
  filter(instituicao == "UFRN" | instituicao == "UFMG" | instituicao == "Unicamp")



# Selecionando participantes de todas as universidades diferentes de Outra

linguistas.total%>%
  filter(instituicao != "Outra")


# Selecionando as colunas de interesse
# Selecionando participantes de instituições diferentes de Outra
# Selecionando participantes com tamanho de calçado menor que 50

linguistas.total%>%
  select(genero, idade, altura, peso, tamanho.pe, instituicao, dialeto, deslocamento, status, esporte)%>%
  filter(instituicao != "Outra")%>%
  filter(tamanho.pe < 50)



## Criando conjunto de dados

linguistas.edit = linguistas.total%>%
  select(genero, idade, altura, peso, tamanho.pe, instituicao, dialeto, deslocamento, status, esporte)%>%
  filter(instituicao != "Outra")%>%
  filter(tamanho.pe < 70)%>%
  droplevels()

### Treinando 1

# Olhe os códigos abaixo sem rodá-los. 

# Tente descobrir que código gerará uma mensagem de erro e o motivo dessa mensagem.

# Código 1
linguistas.total%>%
  select(genero, idade, altura, esporte, tamanho.pe, status)%>%
  filter(instituicao != "UFMG")

# Código 2
linguistas.total%>%
  select(genero, idade, altura, instituicao, esporte, status)%>%
  filter(instituicao != "UFMG")


# TABELAS

# Criando tabela com as médias de altura e tamanho de pé

linguistas.edit%>%
summarise(media_altura = mean(altura),
          media_pe     = mean(tamanho.pe))


# Criando tabela agrupando os participantes por instituição e para extração das médias de altura e tamanho de pé e do número de participantes

linguistas.edit%>%
  group_by(instituicao)%>%
  summarise(media_altura = mean(altura),
            media_pe     = mean(tamanho.pe),
            qtdd         = n())


# Criando tabelas agrupando participantes por instituição e gênero para saber o n de cada grupo

linguistas.edit%>%
  group_by(instituicao, genero)%>%
  summarise(qtdd = n())


## TREINANDO 2

# Olhe os códigos abaixo, tente adivinhar qual seria seu output e crie uma descrição para eles

# Código 1
linguistas.total%>%
  filter(instituicao == "Unicamp")%>%
  group_by(genero)%>%
  summarize(media = mean(altura))

# Código 2
linguistas.total%>%
  group_by(status, deslocamento)%>%
  summarise(qtt = n())
 

#################################
#################################
# EXPLORANDO DADOS GRAFICAMENTE #
#################################
#################################

# Carregando o pacote

library(ggplot2)

# sintaxe básica

# ggplot(dados, aes(x = variável_preditora, y = variável_resposta))


## EXEMPLOS PRÁTICOS

## GRÁFICOS DE DISPERSÃO

# criando a estrutura básica do nosso gráfico

ggplot(linguistas.edit, aes(x = altura, y = tamanho.pe))


# adicionando a camada de geometria, indicando geometria de pontos

ggplot(linguistas.edit, aes(x = altura, y = tamanho.pe))+
  geom_point()


# modificando os elementos introduzidos pela camada geometria

# o valor de alpha vai de 0 (completamente transparente) a 1 (completamente opaco)

ggplot(linguistas.edit, aes(x = altura, y = tamanho.pe))+
  geom_point(size = 3, alpha = 0.3) 



# agrupando dados por nova variável na camada de geometria por meio de cor

ggplot(linguistas.edit, aes(x = altura, y = tamanho.pe))+
  geom_point(size = 3, alpha = 0.3, aes(color = genero))



# agrupando dados por nova variável na camada de geometria por meio de formato

ggplot(linguistas.edit, aes(x = altura, y = tamanho.pe))+
  geom_point(size = 3, alpha = 0.3, aes(shape = genero))



# PRÁTICA: gráficos de dispersão
 
## Leia o código abaixo e tente adivinhar que tipo de gráfico ele construiria. Depois rode o código e veja se você acertou.

 ggplot(linguistas.edit, aes(x = altura, y = peso))+
   geom_point(size = 3, alpha = 0.6, colour = "red")
 


## Leia o código abaixo e tente descobrir porque ele retorna um código de erro. Arrume o código.
 
 ggplot(linguistas.edit, aes(x = altura, y = peso))+
   geom_point(size = 2, alpha = 0.6, colour = genero)
 
## Crie um gráfico que demonstre a relação entre idade e peso, de modo a identificar se a idade é uma variável que influencia o peso de uma pessoa. Indique também, no gráfico, se a pessoa faz ou não atividade física, pois isso pode ser uma variável importante que influencia a relação entre peso e idade (pode ser que haja uma relação, mas essa relação some para quem faz atividade física)


## BOXPLOTS

# Criando um boxplot

ggplot(linguistas.edit, aes(x = genero, y = altura))+
  geom_boxplot()


# customizando o gráfico; note que é possível informar a cor por sua notação RGB

ggplot(linguistas.edit, aes(x = genero, y = altura))+
  geom_boxplot(alpha = 0.4, size = 4, color = "#8bd5c4")



# indicando preenchimento da geometria pela variável genero

ggplot(linguistas.edit, aes(x = genero, y = altura))+
  geom_boxplot(aes(fill = instituicao))


# Boxplot mostrando altura para cada gênero; um painel por instituição

ggplot(linguistas.edit, aes(x = genero, y = altura))+
  geom_boxplot()+
  facet_wrap(~ instituicao)



# criar gráficos nos 6 agrupamentos do cruzamento de instituição x esporte

# determinal número de colunas = 2 nos painéis do facet para facilitar comparação (o default seria 3)

ggplot(linguistas.edit, aes(x = genero, y = altura))+
  geom_boxplot()+
  facet_wrap(~ instituicao + esporte, ncol = 2)



# Criando gráficos de dispersão com painéis diferentes para cada gênero

ggplot(linguistas.edit, aes(x = altura, y = tamanho.pe))+
  geom_point(size = 3, alpha = 0.3)+
  facet_wrap(~ genero)



## PRÁTICA: Boxplot

# Leia o código abaixo e tente adivinhar que tipo de gráfico ele construiria. Depois rode o código e veja se você acertou.

 ggplot(linguistas.edit, aes(x = esporte, y = peso))+
   geom_boxplot(aes(fill = genero))
 

# Tente escrever o código que reproduz o gráfico visto no arquivo HTML (para a cor, use "light blue").


# Crie um gráfico um boxplot que compare a idade das pessoas que fazem ou não fazem atividade física e tente pintar o gráfico de uma cor de sua escolha.



## GRÁFICO DE BARRAS

# Gráfico de barra indicando frequência absoluta por instituição

ggplot(linguistas.edit, aes(x = instituicao))+
  geom_bar()


# gráfico de instituição x dialeto: frequência absoluta

ggplot(linguistas.edit, aes(x = instituicao))+
  geom_bar(aes(fill = dialeto))


# Gráfico com frequência relativa das ocorrência de instituição x dialeto

ggplot(linguistas.edit, aes(x = instituicao)) +  
  geom_bar(aes(fill = dialeto, y = (..count..)/sum(..count..)))
  

# Gráfico com % das ocorrências de instituição x dialeto

ggplot(linguistas.edit, aes(x = instituicao)) +  
  geom_bar(aes(fill = dialeto, y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels=scales::percent)
  


# Gráfico com % das ocorrências de instituição x dialeto com barra dodged

ggplot(linguistas.edit, aes(x = instituicao)) +  
  geom_bar(position = "dodge", aes(fill = dialeto, y = (..count..)/sum(..count..)))+
  scale_y_continuous(labels=scales::percent)
  


# Gráfico para comparação entre % das taxas de dialeto em cada instituição

ggplot(linguistas.edit, aes(x = instituicao, fill = dialeto)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill")+
  scale_y_continuous(labels=scales::percent)
  


## PRÁTICA: gráfico de barra

# Leia o código abaixo e tente adivinhar que tipo de gráfico ele construiria. Depois rode o código e veja se você acertou.

 ggplot(linguistas.edit, aes(x = instituicao, fill = deslocamento))+
   geom_bar(aes(y = (..count..)/sum(..count..)))+
   facet_wrap(~ status)
 


# Leia o código abaixo e tente descobrir porque ele não retorna o que descreve sua descrição. Arrume o código.

 # Gráfico de barra mostrando a proporção (em %) de pessoas que faz atividade física em cada uma das instituições; cada instituição tomada individualmente em 100%
 
 ggplot(linguistas.edit, aes(x = instituicao)) +
   geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill")+
   scale_y_continuous(labels=scales::percent)
 
## Crie um gráfico que mostre se gênero prevê a preferência por fazer ou não atividade física; como há mais mulheres que homens na amostra, normalize o gráfico para que cada barra some 100%, facilitando a comparação.

## PERSONALIZAÇÃO DE GRÁFICOS

# Temas

ggplot(linguistas.edit, aes(x = altura, y = tamanho.pe))+
  geom_point(size = 4, alpha = 0.4, aes(color=genero))



ggplot(linguistas.edit, aes(x = altura, y = tamanho.pe))+
  geom_point(size = 4, alpha = 0.4, aes(color=genero))+
  theme_bw()



ggplot(linguistas.edit, aes(x = altura, y = tamanho.pe))+
  geom_point(size = 4, alpha = 0.4, aes(color=genero))+
  theme_minimal()



ggplot(linguistas.edit, aes(x = altura, y = tamanho.pe))+
  geom_point(size = 4, alpha = 0.4, aes(color=genero))+
  theme_dark()

# PALETA DE CORES

#para gráficos de barra

ggplot(linguistas.edit, aes(x = instituicao, fill = deslocamento))+
  geom_bar()


ggplot(linguistas.edit, aes(x = instituicao, fill = deslocamento))+
  geom_bar()+
  scale_fill_brewer(palette = "Dark2")


ggplot(linguistas.edit, aes(x = instituicao, fill = deslocamento))+
  geom_bar()+
  scale_fill_brewer(palette = "Reds")


ggplot(linguistas.edit, aes(x = instituicao, fill = deslocamento))+
  geom_bar()+
  scale_fill_brewer(palette = "Pastel1")

# para gráficos de dispersão

ggplot(linguistas.edit, aes(x = altura, y = tamanho.pe))+
  geom_point(size = 4, alpha = 0.4, aes(color=genero))


ggplot(linguistas.edit, aes(x = altura, y = tamanho.pe))+
  geom_point(size = 4, alpha = 0.7, aes(color=genero))+
  scale_colour_brewer(palette = "Reds")


ggplot(linguistas.edit, aes(x = altura, y = tamanho.pe))+
  geom_point(size = 4, alpha = 0.7, aes(color=genero))+
  scale_colour_brewer(palette = "Pastel1")


## LEGENDAS

ggplot(linguistas.edit, aes(x = altura, y = tamanho.pe))+
  geom_point(size = 4, alpha = 0.4, aes(color=genero))+
  labs(x = "Altura (em cm)", y = "Tamanho do calçado", color = "Gênero", title = "Relação entre altura e tamanho do pé")+
  theme_bw()


ggplot(linguistas.edit, aes(x = instituicao, fill = deslocamento))+
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill")+
  scale_y_continuous(labels=scales::percent)+
  scale_fill_brewer(palette = "Dark2")+
  labs(x = "Instituição de Ensino", y = "Porcentagem dos deslocamentos", fill = "Tipo de deslocamento entre casa e universidade")+
  theme_bw()


ggplot(linguistas.edit, aes(x = instituicao, fill = deslocamento))+
  geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill")+
  scale_y_continuous(labels=scales::percent)+
  scale_fill_brewer(palette = "Dark2")+
  labs(x = "Instituição de Ensino", y = "Porcentagem dos deslocamentos", fill = "Tipo de deslocamento \n entre casa e \n universidade")+
  theme_bw()


## UNINDO DPLYR E GGPLOT

linguistas.edit%>%
  filter(genero=="Masculino")%>%
  ggplot(., aes(x=instituicao, y=tamanho.pe))+
  geom_boxplot(fill = "lightblue")+
  labs(x = "Instituição", y = "Tamanho do Calçado", title  = "Distribuição do tamanho do calçado de linguistas do gênero \n masculino em três universidades")+
  theme_bw()
