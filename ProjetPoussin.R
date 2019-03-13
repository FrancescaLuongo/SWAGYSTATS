setwd("/Users/laurefont/EPFL/AppliedBiostats/SWAGYSTATS/Data")
Database <- read.table("tree-moisture.txt", header = FALSE)

#print(tapply(my_data, list(tree.moisture$Species, tree.moisture$Branch_location), mean))

# Categorical variables (nominal Species,Branch,ordinal Location,ordinal Transpiration) and Numerical (Moisture)
# Multivariate data (measurement on different variables)
# balanced groups : every sample is different from the next

names(Database) <- c("Species", "Branch_Species", "Branch_Locations", "Transpiration_Types", "Dry")

Database$Species <- factor(Database$Species,
                           levels = c(1,2,3,4),
                           labels = c("Lobolly Pine","Shortleaf Pine", "Yellow Poplar","Red Gum"))

Database$Branch_Species <- factor(Database$Branch_Species,
                                  levels = c(1,2,3,4,5),
                                  labels = c(1,2,3,4,5))

Database$Branch_Locations <- factor(Database$Branch_Locations,
                                    levels = c(1,2,3),
                                    labels = c("Central", "Distal", "Proximal"))

Database$Transpiration_Types <- factor(Database$Transpiration_Types,
                                       levels = c(1,2),
                                       labels = c("Rapid", "Slow"))

#tree.moisture$Species <- factor(c("Lobolly Pine","Shortleaf Pine", "Yellow Poplar","Red Gum"))
#tree.moisture$Branch_Locations <- factor(c("Central", "Distal", "Proximal"))
#tree.moisture$Transpiration_Types <- factor(c("Rapid", "Slow"))

str(Database)

tapply(Database$Dry, list(Database$Species, Database$Transpiration_Types, Database$Branch_Locations), mean) 
tapply(Database$Dry, list(Database$Species, Database$Transpiration_Types, Database$Branch_Locations), sd) 
plot.design(Database) 

# Modele additif
moisture.aov <- aov(Dry ~ Species+Branch_Species+Branch_Locations+Transpiration_Types, data=Database)
summary(moisture.aov)
coef(moisture.aov)
options("contrasts")
# Réduction de RSS divisée par le nombre de paramètres donc comme branch species a plus de levels, sa SS aura une 
# plus petite valeur

moisture.aov <- aov(Dry ~ Species*Branch_Locations*Transpiration_Types, data=Database)
summary(moisture.aov)

moisture.aov <- aov(Dry ~ Species+Branch_Locations+Transpiration_Types+Species:Transpiration_Types, data=Database)
summary(moisture.aov)

layout(matrix(1:4,ncol=2)) 
plot(moisture.aov)

interaction.plot(Database$Species, Database$Transpiration_Types, Database$Dry)
interaction.plot(Database$Species, Database$Branch_Locations, Database$Dry)
interaction.plot(Database$Transpiration_Types, Database$Branch_Locations, Database$Dry)

# facteur = variable linéaire
# Mettre en facteur pour dire que chaque groupe a une valeur différente
# Si on a una variable qui prend des valeurs continues : on ajuste une pente pour faire passer la droite par 
# les points
# Autrement si facteur discret : prédit moyenne pour chaque groupe
# Si relation linéaire entre les variables, dangereux (on veut relation linéaire entre variable et le Y)

#model + coeff + ANOVA + analyse exploratoire + CCL
