randomize_settings = function(prelude    = 2/5,
                              colonies   = 1/5,
                              venus      = 3/5,
                              turmoil    = 4/5,
                              big_box    = 1,
                              players    = NULL,
                              boardprobs = c(0.45, 0.2, 0.35)){
colors = sample(c("yellow", "red", "green", "blue", "white")[1:length(players)])
print(paste0(players, ": ", colors))
print(paste0("Prelude: ", ifelse(rbinom(1, 1, prelude) == 1, "Yes", "No")))
print(paste0("Colonies: ", ifelse(rbinom(1, 1, colonies) == 1, "Yes", "No")))
print(paste0("Venus: ", ifelse(rbinom(1, 1, venus) == 1, "Yes", "No")))
print(paste0("Turmoil: ", ifelse(rbinom(1, 1, turmoil) == 1, "Yes", "No")))
print(paste0("Big Box: ", ifelse(rbinom(1, 1, big_box) == 1, "Yes", "No")))
print(paste0("On Board: ", sample(1:3,1, prob = boardprobs)))
}

randomize_settings(players = c("Julian", "Niklas", "Martin", "Malte", "Patrick"))

#### alternativ: Wahrsacheinlichkeitsverteilung �ber die Anzahl Erweiterungen, dann ziehen mit Wahrscheinlichkeiten
