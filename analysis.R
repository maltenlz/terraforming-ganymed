library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(forcats)
raw <- read.csv("Terraforming Runde - Tabellenblatt1.csv")
reshaped_data = list()
for(i in 1:5){
  temp = raw[,c(1, 3+(i-1),8+(i-1),13+(i-1))]
  colnames(temp) = c("Spiel", "Spieler", "Punkte", "Konzern")
  reshaped_data[[i]] =temp
}

data  = do.call('rbind', reshaped_data)

data = data %>% filter(!is.na(Punkte)) %>% group_by(Spiel) %>% mutate(Victory = as.numeric(Punkte == max(Punkte, na.rm =T)),
                                                                      points_relative = Punkte/max(Punkte, na.rm =T)) %>% ungroup() %>% data.frame()


ggplot(data  %>% group_by(Konzern) %>% summarise(Siegwahrscheinlichkeit = mean(Victory), n = n()) %>% mutate(Konzern = fct_reorder(Konzern, Siegwahrscheinlichkeit, .desc = TRUE)) %>%
                 mutate(sd = sqrt(Siegwahrscheinlichkeit*(1-Siegwahrscheinlichkeit)/n)),
                 aes(x = Konzern, y = Siegwahrscheinlichkeit, fill = Siegwahrscheinlichkeit)) + 
                 geom_bar(stat = "identity")  + 
                 theme_ipsum(axis_text_size = 12, axis_title_size = 14)+ 
                 theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
                 scale_fill_gradient(low = "lightgreen", high = "darkgreen", na.value = NA) + 
                 geom_errorbar(aes(ymin=Siegwahrscheinlichkeit-sd, ymax=Siegwahrscheinlichkeit+sd), width=.2, position=position_dodge(.9))

ggplot(data  %>% group_by(Konzern) %>% summarise(Punkte_rel = mean(points_relative), sd = sd(points_relative)/sqrt(n()))  %>% mutate(Konzern = fct_reorder(Konzern, Punkte_rel, .desc = TRUE)),
                 aes(x = Konzern, y = Punkte_rel, fill = Punkte_rel)) + 
                 geom_bar(stat = "identity")  + 
                 theme_ipsum(axis_text_size = 12, axis_title_size = 14)+ 
                 theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
                 scale_fill_gradient(low = "lightgreen", high = "darkgreen", na.value = NA) + 
                 geom_errorbar(aes(ymin= Punkte_rel-sd, ymax= ifelse(Punkte_rel+sd>=1, 1, Punkte_rel+sd)), width=.2, position=position_dodge(.9))

#ggplot(data  %>% group_by(Spieler) %>% summarise(win_pct = mean(Victory)), aes(x = Spieler, y = win_pct)) + geom_bar(stat = "identity")

ggplot(data  %>% group_by(Spieler) %>% summarise(Siegwahrscheinlichkeit = mean(Victory), n = n()) %>% mutate(Spieler = fct_reorder(Spieler, Siegwahrscheinlichkeit, .desc = TRUE)) %>%
                 mutate(sd = sqrt(Siegwahrscheinlichkeit*(1-Siegwahrscheinlichkeit)/n)),
                 aes(x = Spieler, y = Siegwahrscheinlichkeit, fill = Siegwahrscheinlichkeit)) + 
                 geom_bar(stat = "identity")  + 
                 theme_ipsum(axis_text_size = 12, axis_title_size = 14)+ 
                 theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
                 scale_fill_gradient(low = "lightgreen", high = "darkgreen", na.value = NA) + 
                 geom_errorbar(aes(ymin=Siegwahrscheinlichkeit-sd, ymax=Siegwahrscheinlichkeit+sd), width=.2, position=position_dodge(.9))

ggplot(data  %>% group_by(Spieler) %>% summarise(Punkte_rel = mean(points_relative), sd = sd(points_relative)/sqrt(n()))  %>% mutate(Spieler = fct_reorder(Spieler, Punkte_rel, .desc = TRUE)),
                 aes(x = Spieler, y = Punkte_rel, fill = Punkte_rel)) + 
                 geom_bar(stat = "identity")  + 
                 theme_ipsum(axis_text_size = 12, axis_title_size = 14)+ 
                 theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
                 scale_fill_gradient(low = "lightgreen", high = "darkgreen", na.value = NA) + 
                 geom_errorbar(aes(ymin= Punkte_rel-sd, ymax= ifelse(Punkte_rel+sd>=1, 1, Punkte_rel+sd)), width=.2, position=position_dodge(.9))


#### Beta Regression model auf Punkte_rel. Erstmal simpel: Konzern und Spieler als Einfluß
library(betareg)
data = data %>% group_by(Spieler) %>% mutate(n_s = n()) %>% ungroup() %>% group_by(Konzern) %>% mutate(n_k = n()) %>% ungroup() %>% filter((n_s > 2) & (n_k > 1))

data$Spieler = as.factor(as.character(data$Spieler))
data$Konzern = as.factor(as.character(data$Konzern))

####
x_design     = model.matrix(~. -1, data = data[,c("Spieler", "Konzern", "points_relative", "Spiel")])
x_design[,colnames(x_design) == "points_relative"] = x_design[,colnames(x_design) == "points_relative"]-0.01
### Patrick und Unmi als Referenzkategorien
#w        =  1/sqrt(rev(x_design[,colnames(x_design) == "Spiel"]))
x_design = x_design[,!(colnames(x_design) %in% c("Spieler", "Konzern", "SpielerOskar", "Spiel"))]

betamod = betareg(points_relative ~., data = data.frame(x_design), link = "logit")

summod = summary(betamod)


betaframe = data.frame(Coeff = names(summod$coefficients$mean[,1]), Strength= summod$coefficients$mean[,1], sd = summod$coefficients$mean[,2])
betaframe = rbind(betaframe, data.frame(Coeff = c("SpielerOskar", "KonzernArcadian"), Strength=c(0, 0), sd = c(0, 0)))

ggplot(betaframe %>% filter(grepl("Spieler", Coeff)) %>% mutate(Spieler = gsub("Spieler", "", Coeff))%>% mutate(Spieler = fct_reorder(Spieler, Strength, .desc = TRUE)),
                     aes(x = Spieler, y = Strength, fill = Strength)) + 
                     geom_bar(stat = "identity")  + 
                     theme_ipsum(axis_text_size = 12, axis_title_size = 14)+ 
                     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
                     scale_fill_gradient(low = "pink", high = "darkred", na.value = NA)+ 
                     geom_errorbar(aes(ymin= Strength-sd, ymax= Strength+sd), width=.2, position=position_dodge(.9))



ggplot(betaframe %>% filter(grepl("Konzern", Coeff)) %>% mutate(Konzern = gsub("Konzern", "", Coeff))%>% mutate(Konzern = fct_reorder(Konzern, Strength, .desc = TRUE)),
                     aes(x = Konzern, y = Strength, fill = Strength)) + 
                     geom_bar(stat = "identity")  + 
                     theme_ipsum(axis_text_size = 12, axis_title_size = 14)+ 
                     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
                     scale_fill_gradient(low = "pink", high = "darkred", na.value = NA)+ 
                     geom_errorbar(aes(ymin= Strength-sd, ymax= Strength+sd), width=.2, position=position_dodge(.9))


#### Komplexer mit Konzern und spieler als einfluß und dummy für gegner
#### Idee für mit Gegnern: 
# Matrix, welche als Einträge enthält: 1 an der Stelle für den Spieler -1 an Stelle von den Gegnern
# data  = do.call('rbind', reshaped_data)
# data = data %>% filter(!is.na(Punkte)) %>% group_by(Spiel) %>% mutate(Victory = as.numeric(Punkte == max(Punkte, na.rm =T)),
#                                                                       points_relative = Punkte/max(Punkte, na.rm =T),
#                                                                       n_player = n()) %>% ungroup() %>% filter(n_player == 4) %>% data.frame()
# 
# prepare_player_contrast = function(data){
#   games_un        = unique(data$Spiel)
#   players_un      = unique(data$Spieler)
#   player_contrast = matrix(0, ncol=length(players_un), nrow = nrow(data))
#   colnames(player_contrast) = paste0("Spieler", players_un)
#   counter         = 0
#   for(i in 1:length(games_un)){
#     temp = data %>% filter(Spiel == games_un[i])
#     for(k in 1:nrow(temp)){
#       counter = counter+1
#       player_contrast[counter, match(temp$Spieler[k], players_un)]   = 1
#       player_contrast[counter, match(temp$Spieler[-k], players_un)]  = -1
#     }
#   }
#   player_contrast
# }
# 
# prepare_corp_contrast = function(data){
#   games_un      = unique(data$Spiel)
#   corp_un       = unique(data$Konzern)
#   corp_contrast = matrix(0, ncol=length(corp_un), nrow = nrow(data))
#   colnames(corp_contrast) = paste0("Konzern", corp_un)
#   counter         = 0
#   for(i in 1:length(games_un)){
#     temp = data %>% filter(Spiel == games_un[i])
#     for(k in 1:nrow(temp)){
#       counter = counter+1
#       corp_contrast[counter, match(temp$Konzern[k], corp_un)]   = 1
#       corp_contrast[counter, match(temp$Konzern[-k], corp_un)]  = -1
#     }
#   }
#   corp_contrast
# }
# 
# 
# betamod  = betareg(points_relative ~., data = data.frame(x_design)[,-c(4,19)], link = "logit")
# 
# summod = summary(betamod)
# 
# betaframe = data.frame(Coeff = names(summod$coefficients$mean[,1]), Strength=summod$coefficients$mean[,1], sd = summod$coefficients$mean[,2])
# betaframe = rbind(betaframe, data.frame(Coeff = c("SpielerOskar"), Strength=c(0), sd = c(0)),
#                              data.frame(Coeff = c("KonzernUnmi"),  Strength=c(0), sd = c(0)))
# 
# ggplot(betaframe %>% filter(grepl("Spieler", Coeff)) %>% mutate(Spieler = gsub("Spieler", "", Coeff))%>% mutate(Spieler = fct_reorder(Spieler, Strength, .desc = TRUE)),
#        aes(x = Spieler, y = Strength, fill = Strength)) + 
#        geom_bar(stat = "identity")  + 
#        theme_ipsum(axis_text_size = 12, axis_title_size = 14)+ 
#        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#        scale_fill_gradient(low = "pink", high = "darkred", na.value = NA)+ 
#        geom_errorbar(aes(ymin= Strength-sd, ymax= Strength+sd), width=.2, position=position_dodge(.9))
# 
# 
# 
# ggplot(betaframe %>% filter(grepl("Konzern", Coeff)) %>% mutate(Konzern = gsub("Konzern", "", Coeff))%>% mutate(Konzern = fct_reorder(Konzern, Strength, .desc = TRUE)),
#        aes(x = Konzern, y = Strength, fill = Strength)) + 
#        geom_bar(stat = "identity")  + 
#        theme_ipsum(axis_text_size = 12, axis_title_size = 14)+ 
#        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#        scale_fill_gradient(low = "pink", high = "darkred", na.value = NA)+ 
#        geom_errorbar(aes(ymin= Strength-sd, ymax= Strength+sd), width=.2, position=position_dodge(.9))
# 
