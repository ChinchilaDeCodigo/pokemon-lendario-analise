#### Script para identificar o que torna um Pokémon Lendário ####

library(RColorBrewer)
library(GGally)
library(tibble)
library(dplyr)
library(ggrepel)
library(forcats)
library(tree)

# Montando o dataset
data <- read.csv(file.choose())
df <- data.frame(data)
print(df)

# Análise e medições
mean(df$HP)

df$Legendary <- ifelse(df$Legendary == "False", "Non-Legendary", "Legendary")

# Mudando Legendary de num para factor
df$Legendary <- as.factor(df$Legendary)

hp <- df$HP
summary(hp)

table(df$Legendary)
prop.table((table(df$Legendary)))

table(df$Type.1)
prop.table(table(df$Type.1))

table(df$Type.2)
prop.table(table(df$Type.2))

# Mudando o tipo de Type.1 e Type.2 de chr para factor
df$Type.1 <- as.factor(df$Type.1)
df$Type.2 <- as.factor(df$Type.2)

print(df)

coul <- brewer.pal(9,"Set1")
coul2 <- brewer.pal(8,"Set2")

hist(hp, main=paste("Histograma de HP"), xlab="HP")
hist(df$Attack, main=paste("Histograma de Ataque"), xlab="Ataque")
hist(df$Defense, main=paste("Histograma de Defesa"), xlab="Defesa")

barplot(prop.table(table(df$Type.1)),las = 2, col=coul )
barplot(prop.table(table(df$Type.2)), las = 2, col=coul2)


#Filtrando somente lendários
only_legendary <- df[df$Legendary == "Legendary",]
barplot(prop.table(table(only_legendary$Type.1)),las = 2, col=coul)
barplot(prop.table(table(only_legendary$Type.2)), las = 2, col=coul2)

table(only_legendary$Type.1)
prop.table(table(only_legendary$Type.1))

table(only_legendary$Type.2)
prop.table(table(only_legendary$Type.2))

ggpairs(data = only_legendary, columns = 6:11)

df %>% 
 ggpairs(columns = c("HP", "Attack", "Defense", "Sp..Atk", "Sp..Def",
                     "Speed"), mapping = aes(color = Legendary))

### com as observações, as correlações mais fortes
### são Defense vs Sp..Def e Sp..Def vs Sp..Atk

ggplot(df, aes(x=Defense, y=Sp..Def, color=Legendary)) + 
  geom_point() +
  geom_text_repel(data = filter(df, Defense >= 200 | Sp..Def >= 200), 
            aes(label=Name), size = 4)

ggplot(df, aes(x=Sp..Def, y=Sp..Atk, color=Legendary)) + 
  geom_point() +
  geom_text_repel(data = filter(df, Sp..Def >= 200 | Sp..Atk >= 175), 
                  aes(label=Name), size = 4)

#Boxplots
df %>%
  mutate(class = fct_reorder(Legendary, Defense, .fun='median')) %>%
  ggplot(aes(x=Legendary, y=Defense, fill=Legendary)) + 
  geom_boxplot()

df %>%
  mutate(class = fct_reorder(Legendary, Sp..Def, .fun='median')) %>%
  ggplot(aes(x=Legendary, y=Sp..Def, fill=Legendary)) + 
  geom_boxplot()

df %>%
  mutate(class = fct_reorder(Legendary, Sp..Atk, .fun='median')) %>%
  ggplot(aes(x=Legendary, y=Sp..Atk, fill=Legendary)) + 
  geom_boxplot()

#Teste t de Student
nonlegendary <- df[df$Legendary != "Legendary", ]
defNon <- nonlegendary[,8]
defLeg <- only_legendary[,8]
t.test(defNon, defLeg)

spdefNon <- nonlegendary[, 10]
spdefLeg <- only_legendary[, 10]
t.test(spdefNon, spdefLeg)

spatkNon <- nonlegendary[, 9]
spatkLeg <- only_legendary[ ,9]
t.test(spatkNon, spatkLeg)

## Usando árvore de decisão para modelo
set.seed(1)
train <- sample(1:nrow(df), nrow(df)/3)
test <- -train

train_data <- df[train, ]
test_data <- df[test, ]
legendary_test <- ifelse(df$Legendary == "Legendary", 
                         "Legendary", "Non-Legendary")
legendary_data <- legendary_test[test]

decisionTree <- tree(Legendary ~ HP + Attack + Defense + Sp..Atk + Sp..Def + 
                       Speed, train_data)
summary(decisionTree)

predictionTree <- predict(decisionTree, test_data, type="class")
table(predictionTree, legendary_data)
accuracy = mean(predictionTree == legendary_data)
accuracy
#91% de sucesso

plot(decisionTree)
text(decisionTree, pretty = 0)
title(main = "Árvore de Classificação Sem Poda")

#Podando a árvore
cv_tree = cv.tree(decisionTree, FUN=prune.misclass)
plot(cv_tree$size, cv_tree$dev, type = "b",
     xlab="Tamanho da árvore",
     ylab="Taxa de Erro",
     main="Validação Cruzada: Erro vs Tamanho")

#Menor erro foi com tamanho 4
pruned_model = prune.misclass(decisionTree, best = 4)
plot(pruned_model)
text(pruned_model, pretty = 0)
title(main = "Árvore de Classificação Com Poda")

prunePredictionTree <- predict(pruned_model, test_data,
                               type="class")
accuracyPrune <- mean(prunePredictionTree == legendary_data)
accuracyPrune
#91% de sucesso, igual ao da árvore sem poda