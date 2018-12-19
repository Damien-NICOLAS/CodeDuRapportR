#Libraire : ggplot/ dplyr

#Import des données depuis le CSV
file = read.csv("bgg_db_1806.csv")

#On retire les deux colonnes qui nous sont inutiles : bgg_url and images_url 
file_bis = file[,-2]
file_ter = file_bis[,-13]

=SI(ESTNUM(CHERCHE("Role Playing";P2));1;0)
#Le dataset est prêt pour les manipulations
file = file_ter

file








#Quelles sont les mécaniques de jeu cette année : 
#On commence par prendre uniquement le jeu de cette année
lesJeuxDeCetteAnnee = filter(file, year == 2018)

toutelesMecaniques = lesJeuxDeCetteAnnee["mechanic"]

mechanic = toutelesMecaniques$mechanic
class(mechanic)




