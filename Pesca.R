
library(tidyverse)
library(lubridate)
library(stringr)
library(DBI)

#Dados (.Renviron)
dbicon <-  DBI::dbConnect(RPostgres::Postgres(),
                          db = Sys.getenv('POSTGRES_DB'),
                          host = Sys.getenv('POSTGRES_HOST'),
                          port = Sys.getenv('POSTGRES_PORT'),
                          user = Sys.getenv('POSTGRES_USER'),
                          password = Sys.getenv('POSTGRES_PASSWORD'))

sql <- 'SELECT * FROM "tblPesca"'
df_maturity <- DBI::dbGetQuery(dbicon, sql)


DBI::dbDisconnect(dbicon)


#nós selecionamos, filtramos e processamos os preditores que vamos utilizar na análise:
maturity_tbl <- df_maturity %>%
  select(Data, Ano, Mes, Trimestre, Matura, Sexo, MaturaH1, SexoH1, Total_lenght,  Especie,Peso_total) %>%
  set_names(c("date", "ano", "mes", "trimestre", "matura", "sexo", "maturaH1",
              "sexoH1", "comprimento", "specie", "peso")) 

#NAs
maturity_tbl <- maturity_tbl[complete.cases(maturity_tbl),]
colSums(is.na(maturity_tbl))


maturity_tbl <- maturity_tbl %>% mutate(sexo = ifelse(sexoH1 %in% "", sexo, sexoH1))
maturity_tbl <- maturity_tbl %>% mutate(matura = ifelse(maturaH1 %in% "", 
                                                        substr(matura,start=1,stop=1), 
                                                        maturaH1))
maturity_tbl$day <- gsub("(.*)[-]", "", maturity_tbl$date)

maturity_tbl <- maturity_tbl %>%
  select(date, ano, mes, trimestre, comprimento, sexo, matura, specie, peso)

# Alterar alguns formatos
maturity_tbl <- maturity_tbl %>% 
  mutate(matura = as.factor(matura),
         specie = as.factor(trimws(specie)))



# 'IND', 'H' e ''
print("Frequência de sexo=('', 'IND' ou 'H') em os tipos de matura")
with(maturity_tbl[maturity_tbl$sexo %in% c('','IND','H'),], table(matura))

maturity_tbl <- maturity_tbl %>% 
  filter(!sexo %in% c('IND','H',''))

# matura=('I','',' ')
maturity_tbl <- maturity_tbl %>% 
  filter(!matura %in% c('I','',' '))

maturity_tbl$matura <- droplevels(maturity_tbl$matura)
print("Removemos os níveis matura = (' ','I'). Obs frequência com comprimento = 0") 
with(maturity_tbl[maturity_tbl$comprimento==0,], table(matura))


#lcat2
maturity_tbl <- maturity_tbl %>% mutate(lcat2=lencat(comprimento,w=2))

#Filtramos as observações de la especie "GOR" e a variável "specie"
maturity_tbl <- maturity_tbl %>% filter(specie=='GORAZ') %>% select(-specie)

#Filtramos as observações en anos irracional
maturity_tbl <- maturity_tbl %>% filter(ano>1900)

# o comprimento = 0, convertê-los para NA
maturity_tbl$comprimento[maturity_tbl$comprimento==0] <- NA

# Construímos a variável dicotômica madura que agrupa peixes com maturidade 0,2 
# como imaturos e 1,3,4,5 como maduros.
maturity_tbl$mature <- NA
maturity_tbl[maturity_tbl$matura %in% c(1,3,4,5),'mature'] <- 1
maturity_tbl[maturity_tbl$matura %in% c(0,2),'mature'] <- 0
maturity_tbl$mature <- factor(maturity_tbl$mature)

maturity_tbl[maturity_tbl$sexo %in% c("M"),'sexo'] <- 1
maturity_tbl[maturity_tbl$sexo %in% c("F"),'sexo'] <- 0
maturity_tbl$sexo <- factor(maturity_tbl$sexo)


## Análise exploratória de dados


#### Distribuição de comprimentos entre maduros e imaturos:


# comprimento by sexo distribution
cols <- c("0" = "red", "1" = "blue")
ggplot(data=na.omit(maturity_tbl), aes(x=comprimento,fill= mature)) +
  geom_histogram(position="identity",alpha=0.5)+
  scale_fill_manual(values = cols, breaks = c("0", "1"),labels = c("Inmature", "Mature")) +
  labs(x = "Comprimento", y = "Frequência", fill = "Mature") +
  theme(legend.position="top") +
  ggtitle("Distribuição do comprimento em espécie de Goraz") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("comprimento_sexo.png")


#### Distribuição do comprimento entre maduros e imaturos e entre os sexos:

# comprimento by sexo distribution
cols <- c("0" = "red", "1" = "blue")
sex_names <- c(
  `0` = 'Female',
  `1` = 'Male'
)

ggplot(data=na.omit(maturity_tbl), aes(x=comprimento,  fill= mature)) +
  geom_histogram(position="identity",alpha=0.5)+
  scale_fill_manual(values = cols, breaks = c("0", "1"),labels = c("Inmature", "Mature")) +
  facet_grid(.~sexo, labeller = as_labeller(sex_names))+
  labs(x = "Comprimento", y = "Frequência", fill = "Mature") +
  theme(legend.position="top") +
  ggtitle("Distribuição do comprimento por sexo em espécie de Goraz") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("comprimento_matura_sexo.png")



#### Box-plot comprimento por sexo:

cols <- c("0" = "red", "1" = "blue")

ggplot(maturity_tbl, aes(x=sexo, y=comprimento, color=sexo)) +
  geom_boxplot(fill='white') +
  scale_color_manual(values = cols, breaks = c("0", "1"),labels = c("Female", "Male")) +
  scale_x_discrete(name="Sexo",labels=c("0" = "Female", '1'= 'Male')) + 
  ggtitle("Distribuição do comprimento por sexo em espécie de Goraz") +
  xlab("Sexo") + ylab("Comprimento") + labs(color = "Sexo") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("bp_comprimento_sexo.png")



#### Distribuição do comprimento por sexo e matura:

ggplot(na.omit(maturity_tbl), aes(x=mature, y=comprimento, color=sexo)) +
  geom_boxplot() +
  scale_color_manual(values = cols, breaks = c("0", "1"),labels = c("Female", "Male")) +
  scale_x_discrete(name="Matura",labels=c("0" = "Inmature", '1'= 'Mature')) + 
  xlab("Matura") + ylab("Comprimento") + labs(color = "Sexo") +
  ggtitle("Length by sex and matura in GOR specie") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("bp_comprimento_matura_sexo.png")


#### Comprimento ao longo dos anos por sexo:

ggplot(na.omit(maturity_tbl), aes(x=ano, y=comprimento, col=sexo)) +
  geom_point() +
  scale_color_manual(values = cols, breaks = c("0", "1"),labels = c("Female", "Male")) +
  ggtitle("Comprimento por ano e sexo em espécie Goraz") +
  xlab("Ano") + ylab("Comprimento") + labs(color = "Sexo") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("ts_comprimento_ano_sexo.png")



#### Heterogeneidade ao longo do tempo por sexo:

#Heterogeneity along the time
maturity_tbl %>%
  group_by(ano,sexo) %>%
  summarise(comp_mean = mean(comprimento,na.rm = T)) %>%
  left_join(maturity_tbl,by=c("ano","sexo")) %>%
  ggplot(data = ., 
         aes(x = ano, y = comprimento)) +
  geom_point() +
  geom_line(aes(x = ano, y = comp_mean), col = "blue") +
  facet_grid(.~sexo,labeller = as_labeller(sex_names))+
  scale_x_continuous(labels = as.character(maturity_tbl$ano), 
                     breaks = maturity_tbl$ano) +
  labs(x = "Trimestre", y = "Comprimento") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Comprimento médio ao longo dos trimestres para a espécie de Goraz") +
  theme(plot.title = element_text(hjust = 0.5))



## Modeling


  
### Primeiro modelo

#### A matura em relação ao comprimento
mylogit1 <- glm(mature ~ comprimento, data = maturity_tbl, family = "binomial")
summary(mylogit1)


#Intervalos de confiança para parâmetros do modelo por meio de reamostragem
bcL1 <- car::Boot(mylogit1,R=100)
confint(bcL1)


#Intervalos de confiança baseados na distribuição normal (odds)
## odds ratios and 95% CI
print("odds ratios and 95% CI for males")
exp(cbind(OddsRatio = coef(mylogit1), confint(mylogit1)))

#Intervalos de confiança para a probabilidade prevista de maturação dos peixes 
#de 30cm:
cf <- coef(mylogit1)
bcL <- car::bootCase(mylogit1, B=100)
predP <- function(cf,x) exp(cf[1]+cf[2]*x)/(1+exp(cf[1]+cf[2]*x))
p30 <- apply(bcL,1,predP,x=30)
quantile(p30,c(0.025,0.975))



p <- 0.5
b1 <- unname(cf[1])
b2 <- unname(cf[2])
y <- log(p/(1-p))
print(paste("Comprimento para 50% de prob de ser matura em machos:",(round(((y - b1)/b2),3))))


# Classificador de regressão logística para machos
x <- seq(-170,170,0.1)
p <- exp(cf[1]+cf[2]*x)/(1+exp(cf[1]+cf[2]*x))
plot(p ~ x, col = "orange", pch = "|", ylim = c(0, 1),cex=0.25,
     main = "Using Logistic Regression for Classification") 
abline(h = 0, lty = 3)  
abline(h = 1, lty = 3)  
abline(h = 0.5, lty = 2) 
abline(v = -b1 / b2, lwd = 2)
abline(v =  (y - b1)/b2, lwd= 2, col = "green")


print(paste("Comprimento para 50% prob de ser matura em machos:", round((-b1/b2),3)))



### Segundo modelo:

#### A matura em relação ao intervalo de comprimento, sexo e a interação entre esses

mylogit2 <- glm(mature ~ comprimento*sexo, data = maturity_tbl, family = "binomial")
summary(mylogit2)

# Seleção de preditores
drop1(mylogit2,~.,test="Chisq")

# Intervalos de confiança
bcL2 <- car::Boot(mylogit2,R=100)
confint(bcL2)


cf <- coef(mylogit2)
x <- seq(-170,170,0.1)
#females
pfem <- exp(cf[1]+cf[2]*x)/(1+exp(cf[1]+cf[2]*x))
#males
pmas <- exp(cf[1]+cf[3]+(cf[2]+cf[4])*x)/(1+exp(cf[1]+cf[3]+(cf[2]+cf[4])*x))


plot(pfem ~ x,type="l",lwd=2,xlab="Comprimento",ylab="Proportion Mature", col = "red") + 
  lines(pmas ~ x,type="l",lwd=2,col="blue") 
legend("bottomright",c("Female","Male"),lwd=2,col=c("red","blue"),bty="n")
abline(h = 0, lty = 3)
abline(h = 1, lty = 3)
abline(h = 0.5, lty = 2)
abline(v = - cf[1] / cf[2], lwd = 2, col = "red")
abline(v = - (cf[1]+cf[3]) / (cf[2]+cf[4]), lwd = 2, col="blue")


print(paste("Comprimento para 50% prob de ser matura en females:", round(- cf[1] / cf[2],3)))

print(paste("Comprimento para 50% prob de ser matura en males:", round((- (cf[1]+cf[3]) / (cf[2]+cf[4])),3)))


### Terceiro modelo:

#### A matura em relação ao intervalo de comprimento, sexo, ano e a interação entre esses.



mylogit3 <- glm(mature ~ comprimento*sexo*trimestre,
                data = maturity_tbl, family = "binomial")
summary(mylogit3)

# Seleção de preditores
drop1(mylogit3,~.,test="Chisq")

AIC(mylogit1,mylogit2,mylogit3)

# Compute predicted probabilities
df <- maturity_tbl %>% na.omit()

m1_prob <- predict(mylogit1, df)
m2_prob <- predict(mylogit2, df)
m3_prob <- predict(mylogit3, df)

# Compute AUC metrics for cv_model1 and cv_model3
perf1 <- prediction(m1_prob, df$mature) %>%
  performance(measure = "tpr", x.measure = "fpr")
perf2 <- prediction(m2_prob, df$mature) %>%
  performance(measure = "tpr", x.measure = "fpr")
perf3 <- prediction(m3_prob, df$mature) %>%
  performance(measure = "tpr", x.measure = "fpr")


# Plot ROC curves for cv_model1 and cv_model3

ROCR::plot(perf1, col = "black")
ROCR::plot(perf2,add=T,col = "blue")
ROCR::plot(perf3,add=T,col = "red")

plot_colors <- c("black","blue","red")
legend(x = "bottom",inset = 0,
       legend = c("Model1", "Model2", "Model3"), 
       col=plot_colors, lwd=7, cex=.7, horiz = TRUE)

