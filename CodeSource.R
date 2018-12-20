#Librairies : ggplot/ dplyr 

#Import des données depuis le CSV
file = read.csv("bgg_db_1806.csv")

#On retire les deux colonnes qui nous sont inutiles : bgg_url and images_url 
file_bis = file[,-2]
file_ter = file_bis[,-13]

#Le dataset est prêt pour les manipulations
file = file_ter

#Avant de commencer, on crée un data.frame avec toute les mécaniques de jeu.
AllMechanics = c("Action / Movement Programing",
                 "Campaign / Battle Card Driven", 
                 "Commodity / Speculation", 
                 "Area Control / Area Influence",
                 "Area Movement",
                 "Area Enclosure",
                 "Area Impulse",
                 "Acting",
                 "Action Point Allowance System",
                 "Auction / Bidding",
                 "Betting/Wagering",
                 "Card Drafting", 
                 "Co-operative Play", 
                 "Deck / Pool Building",
                 "Dice Rolling", 
                 "Hex and Counter", 
                 "Line Drawing",
                 "Point to point Movement",
                 "Paper and Pencil", 
                 "Pattern Recognition", 
                 "Set Collection",
                 "Secret Unit Deployment",
                 "Simulation", 
                 "Stock Holding",
                 "Take that", 
                 "Trading", 
                 "Trick Taking",
                 "Tile Placement", 
                 "Time Track",
                 "Grid Movement",
                 "Hand Management", 
                 "Memory",
                 "Modular Board", 
                 "Partnerships",
                 "Pattern Buildind",
                 "Pick-up and Deliver", 
                 "Player Elimination",
                 "Press your Luck",
                 "Role Playing",
                 "Roll / Spin and Move", 
                 "Route/ Network Building",
                 "Simultaneous Action Selection",
                 "Variable Phase Order", 
                 "Storytelling",
                 "Variable Player Powers",
                 "Voting", 
                 "Worker Placement", 
                 "none")

#Combien il y a t-il de jeu par an ?
#On calcule les effectifs par ans avec la fonction "table"
nbreDeJeuParAns = table(file$year)
nbreDeJeuParAnsDF = as.data.frame(nbreDeJeuParAns)

#On plot ensuite le graphique 
ggplot(nbreDeJeuParAnsDF, aes(x = Var1, y = Freq))+ geom_bar(stat="identity") + geom_boxplot() + ggtitle("Evolution of the number of board games edited each year") + xlab("Year") + ylab("Number of game") + theme(plot.title = element_text(size=20),axis.text.x  = element_text(angle=90, vjust=0.5))


#Quelles sont les mécaniques de jeu cette année ? 
#On commence par prendre uniquement le jeu de cette année
lesJeuxDeCetteAnnee = filter(file, year == 2018)

#On crée ensuite un data.frame avec tout a somme de toutes les mecaniques sur cette année
NbreDeMecPour2018 = c(sum(lesJeuxDeCetteAnnee$Action...Movement.Programming),
                      sum(lesJeuxDeCetteAnnee$Campaign...Battle.Card.Driven),
                      sum(lesJeuxDeCetteAnnee$Commodity.Speculation), 
                      sum(lesJeuxDeCetteAnnee$Area.Control...Area.Influence),
                      sum(lesJeuxDeCetteAnnee$Area.Movement),   
                      sum(lesJeuxDeCetteAnnee$Area.Enclosure),  
                      sum(lesJeuxDeCetteAnnee$Area.Impulse),    
                      sum(lesJeuxDeCetteAnnee$Acting),
                      sum(lesJeuxDeCetteAnnee$Action.Point.Allowance.System),
                      sum(lesJeuxDeCetteAnnee$Auction.Bidding),  
                      sum(lesJeuxDeCetteAnnee$Betting.Wagering), 
                      sum(lesJeuxDeCetteAnnee$Card.Drafting),  
                      sum(lesJeuxDeCetteAnnee$Co.operative.Play),
                      sum(lesJeuxDeCetteAnnee$Deck...Pool.Building), 
                      sum(lesJeuxDeCetteAnnee$Dice.Rolling),          
                      sum(lesJeuxDeCetteAnnee$Hex.and.Counter),  
                      sum(lesJeuxDeCetteAnnee$Line.Drawing),
                      sum(lesJeuxDeCetteAnnee$Point.to.Point.Movement), 
                      sum(lesJeuxDeCetteAnnee$Paper.and.Pencil),      
                      sum(lesJeuxDeCetteAnnee$Pattern.Recognition),
                      sum(lesJeuxDeCetteAnnee$Set.Collection),    
                      sum(lesJeuxDeCetteAnnee$Secret.Unit.Deployment),  
                      sum(lesJeuxDeCetteAnnee$Simulation),            
                      sum(lesJeuxDeCetteAnnee$Stock.Holding),
                      sum(lesJeuxDeCetteAnnee$Take.That),    
                      sum(lesJeuxDeCetteAnnee$Trading), 
                      sum(lesJeuxDeCetteAnnee$Trick.Taking),
                      sum(lesJeuxDeCetteAnnee$Tile.Placement),
                      sum(lesJeuxDeCetteAnnee$Time.Track), 
                      sum(lesJeuxDeCetteAnnee$Grid.Movement), 
                      sum(lesJeuxDeCetteAnnee$Hand.Management),
                      sum(lesJeuxDeCetteAnnee$Memory),
                      sum(lesJeuxDeCetteAnnee$Modular.Board), 
                      sum(lesJeuxDeCetteAnnee$Partnerships),
                      sum(lesJeuxDeCetteAnnee$Pattern.Building),
                      sum(lesJeuxDeCetteAnnee$Pick.up.and.Deliver),
                      sum(lesJeuxDeCetteAnnee$Player.Elimination), 
                      sum(lesJeuxDeCetteAnnee$Press.Your.Luck),
                      sum(lesJeuxDeCetteAnnee$Role.Playing), 
                      sum(lesJeuxDeCetteAnnee$Roll...Spin.and.Move),
                      sum(lesJeuxDeCetteAnnee$Route.Network.Building),
                      sum(lesJeuxDeCetteAnnee$Simultaneous.Action.Selection),
                      sum(lesJeuxDeCetteAnnee$Variable.Phase.Order), 
                      sum(lesJeuxDeCetteAnnee$Storytelling), 
                      sum(lesJeuxDeCetteAnnee$Variable.Player.Powers),
                      sum(lesJeuxDeCetteAnnee$Voting),
                      sum(lesJeuxDeCetteAnnee$Worker.Placement),
                      sum(lesJeuxDeCetteAnnee$none))

#On relie les mécaniques de jeu avec leur nombres. 
mechanicsIn2018 = data.frame(AllMechanics,   NbreDeMecPour2018 )

#On plot ensuite le graphe
ggplot(mechanicsIn2018, aes(x = AllMechanics, y = NbreDeMecPour2018))+ geom_bar(stat="identity") + geom_boxplot() + ggtitle("Plot of the game mechanics present in the 62 games of 2018") + xlab("Game mecanics") + ylab("Number of game") + theme(plot.title = element_text(size=20),axis.text.x  = element_text(angle=90, vjust=0.5))

#Quelles sont les 5 mécaniques de jeu les plus utilisées ces 5 dernières années ? 
#filter sur une des 5 dernieres années
# on fait la somme des jeu mecaniques
# on rank sur les meca  et on prend seulement les 5 premiers
#on plot

#Quelles les 5 mécaniques de jeu les plus populaires ces 5 dernières années ?
#filter sur une des 5 dernieres années
# on fait une moyenne des rank sur les mécaniques de jeu grâce aux colonnes de Damien
# on prend seulement les 5 premiers
# on plot



