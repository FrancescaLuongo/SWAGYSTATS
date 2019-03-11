#V1 1-4 is the species
#V2 1-5 is branches within species
#V3 1-3 Branch location
#V4 1-2 Transpiration type
#V5 moisture content (10x%of dry weight of Sample)

#Steps: We want to predict moisture content
#1) Check same variance => ANOVA condition
#2) Check if the datset is balanced or unbalanced
#3) Check if additive (no interaction) model is sufficient single model analysis

#) Questions TP:
# Interaction plot? Dafuk?
# Anova of a single model or anova of multiple models => what is better? or do we do both? 

#Questions: 
#3) What is an interaction? species and transpiration type?
#4) Branches: real branches or branches of species?  

########## DATA EXPLORATION: VARIANCE CHECK
tree.moisture
tapply(tree.moisture$V5, list(tree.moisture$V1, tree.moisture$V2, tree.moisture$V3, tree.moisture$V4), mean) 
tapply(tree.moisture$V5, list(tree.moisture$V1, tree.moisture$V2, tree.moisture$V3, tree.moisture$V4), sd) 
plot.design(tree.moisture) 
##
boxplot(V5~V1, data=tree.moisture)
boxplot(V5~V2, data=tree.moisture)
boxplot(V5~V3, data=tree.moisture)
boxplot(V5~V4, data=tree.moisture)

##### ANOVA
#Do we want interaction between all? does it make sense to have interaction 
#between branches of one species with another specie? 
moisture.model <- aov(V5 ~ V1*V2*V3*V4, data=tree.moisture) 
summary(moisture.model)
coef(moisture.model)

#### INTERACTION PLOT
#does order matter? 
interaction.plot(tree.moisture$V1, tree.moisture$V2, tree.moisture$V5)
interaction.plot(tree.moisture$V2, tree.moisture$V3, tree.moisture$V5)
interaction.plot(tree.moisture$V3, tree.moisture$V4,  tree.moisture$V5)
interaction.plot(tree.moisture$V4, tree.moisture$V1,  tree.moisture$V5)
