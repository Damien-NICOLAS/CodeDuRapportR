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

#On prend tout les jeux de l'année 2018
lesJeuxDeLAnnee2018 = filter(file, file$year == 2018)

#On fait la moyenne des ranks des jeux : 

actionMovementPrograming2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Action...Movement.Programming == 1)

MecRating2018 = ddply(actionMovementPrograming2018, .(year), summarize,
                      mechanic = "Action / Movement Programming",
                      meanRank = round(mean(avg_rating), 2))

campaignBattleCardDriven2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Campaign...Battle.Card.Driven== 1)

temp = ddply(campaignBattleCardDriven2018, .(year), summarize,
             mechanic = "Campaign / Battle Card Driven",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)

commoditySpeculation2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Commodity.Speculation== 1)

temp = ddply(commoditySpeculation2018, .(year), summarize,
             mechanic = "Commodity Speculation",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)

areaControlAreaInfluence2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Area.Control...Area.Influence== 1)

temp = ddply(areaControlAreaInfluence2018, .(year), summarize,
             mechanic = "Area Control/ Area Influence",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)

areaMovement2018= filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Area.Movement== 1)

temp = ddply(areaMovement2018, .(year), summarize,
             mechanic = "Area Movement",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)
areaEnclosure2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Area.Enclosure== 1)

temp = ddply(areaEnclosure2018, .(year), summarize,
             mechanic = "Area Enclosure",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)

areaImpulse2018= filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Area.Impulse== 1)



temp = ddply(areaImpulse2018, .(year), summarize,
             mechanic = "Area Impulse",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)
acting2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Acting== 1)

temp = ddply(acting2018, .(year), summarize,
             mechanic = "Acting",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)

actionPointAllowanceSystem2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Action.Point.Allowance.System== 1)

temp = ddply(actionPointAllowanceSystem2018, .(year), summarize,
             mechanic = "Action Point Allowance System",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


auctionBidding2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Auction.Bidding== 1)

temp = ddply(auctionBidding2018, .(year), summarize,
             mechanic = "Auction/Bidding",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)

bettingWagering2018= filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Betting.Wagering== 1)

temp = ddply(bettingWagering2018, .(year), summarize,
             mechanic = "Betting / Wagering",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


cardDrafting2018= filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Card.Drafting== 1)

temp = ddply(cardDrafting2018, .(year), summarize,
             mechanic = "Card Drafing",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


cooperativePlay2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Co.operative.Play== 1)

temp = ddply(cooperativePlay2018, .(year), summarize,
             mechanic = "Cooperative Play",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)



deckPoolBuilding2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Deck...Pool.Building== 1)

temp = ddply(deckPoolBuilding2018, .(year), summarize,
             mechanic = "Deck/Pool Building",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


diceRolling2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Dice.Rolling== 1)

temp = ddply(diceRolling2018, .(year), summarize,
             mechanic = "Dice Rolling",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


hexAndCounter2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Hex.and.Counter== 1)

temp = ddply(hexAndCounter2018, .(year), summarize,
             mechanic = "Hex and Counter",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


lineDrawing2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Line.Drawing== 1)

temp = ddply(lineDrawing2018, .(year), summarize,
             mechanic = "Line Drawing",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


pointToPointMovement2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Point.to.Point.Movement== 1)

temp = ddply(pointToPointMovement2018, .(year), summarize,
             mechanic = "Point to Point movement",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


paperAndPencil2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Paper.and.Pencil== 1)

temp = ddply(paperAndPencil2018, .(year), summarize,
             mechanic = "Paper and Pencil",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


patternRecognition2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Pattern.Recognition== 1)

temp = ddply(patternRecognition2018, .(year), summarize,
             mechanic = "Pattern Recognition",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


setCollection2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Set.Collection== 1)

temp = ddply(setCollection2018, .(year), summarize,
             mechanic = "Set Collection",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)

secretUnitDeployment2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Secret.Unit.Deployment== 1)

temp = ddply(secretUnitDeployment2018, .(year), summarize,
             mechanic = "Secret Unit Deployment",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


simulation2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Simulation== 1)

temp = ddply(simulation2018, .(year), summarize,
             mechanic = "Simulation",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


stockHolding2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Stock.Holding== 1)

temp = ddply(stockHolding2018, .(year), summarize,
             mechanic = "Stock Holding",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


takeThat2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Take.That== 1)

temp = ddply(takeThat2018, .(year), summarize,
             mechanic = "Take That",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)



trading2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Trading== 1)

temp = ddply(trading2018, .(year), summarize,
             mechanic = "Trading",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


trickTaking2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Trick.Taking== 1)

temp = ddply(trickTaking2018, .(year), summarize,
             mechanic = "Trick Taking",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


tilePlacement2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Tile.Placement== 1)

temp = ddply(tilePlacement2018, .(year), summarize,
             mechanic = "Tile Placement",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


timeTrack2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Time.Track== 1)

temp = ddply(timeTrack2018, .(year), summarize,
             mechanic = "Time Track",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


gridMovement2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Grid.Movement== 1)

temp = ddply(gridMovement2018, .(year), summarize,
             mechanic = "Grid Movement",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


handManagement2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Hand.Management== 1)

temp = ddply(handManagement2018, .(year), summarize,
             mechanic = "Hand Management",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


memory2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Memory== 1)


temp = ddply(memory2018, .(year), summarize,
             mechanic = "Memory",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)

modularBoard2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Modular.Board== 1)

temp = ddply(modularBoard2018, .(year), summarize,
             mechanic = "Modular Board",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)

partnerships2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Partnerships== 1)

temp = ddply(partnerships2018, .(year), summarize,
             mechanic = "Partnersips",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


patternBuilding2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Pattern.Building== 1)

temp = ddply(patternBuilding2018, .(year), summarize,
             mechanic = "Pattern Building",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


pickUpAndDeliver2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Pick.up.and.Deliver== 1)

temp = ddply(pickUpAndDeliver2018, .(year), summarize,
             mechanic = "Pick Up and Deliver",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)



playerElimination2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Player.Elimination== 1)

temp = ddply(playerElimination2018, .(year), summarize,
             mechanic = "Player Elimination",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


pressYourLuck2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Press.Your.Luck== 1)

temp = ddply(pressYourLuck2018, .(year), summarize,
             mechanic = "Press Your Luck",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


rolePlaying2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Role.Playing== 1)

temp = ddply(rolePlaying2018, .(year), summarize,
             mechanic = "Role Playing",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


rollSpinAndMove2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Roll...Spin.and.Move== 1)

temp = ddply(rollSpinAndMove2018, .(year), summarize,
             mechanic = "Roll / Spin and Move",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)

routeNetworkBuilding2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Route.Network.Building== 1)

temp = ddply(routeNetworkBuilding2018, .(year), summarize,
             mechanic = "Route/Network building",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


simultaneousActionSelection2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Simultaneous.Action.Selection== 1)

temp = ddply(simultaneousActionSelection2018, .(year), summarize,
             mechanic = "Simultaneous Action Selection",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


variablePhaseOrder2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Variable.Phase.Order== 1)

temp = ddply(variablePhaseOrder2018, .(year), summarize,
             mechanic = "Variable Phase Order",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


storytelling2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Storytelling== 1)

temp = ddply(storytelling2018, .(year), summarize,
             mechanic = "Storytelling",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


variablePlayersPowers2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Variable.Player.Powers== 1)


temp = ddply(variablePlayersPowers2018, .(year), summarize,
             mechanic = "Variable Players Powers",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)

voting2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Voting== 1)



temp = ddply(voting2018, .(year), summarize,
             mechanic = "Voting",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


workerPlacement2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$Worker.Placement== 1)


temp = ddply(workerPlacement2018, .(year), summarize,
             mechanic = "Worker Placement",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


none2018 = filter(lesJeuxDeLAnnee2018, lesJeuxDeLAnnee2018$none == 1)

temp = ddply(none2018, .(year), summarize,
             mechanic = "none",
             meanRank = round(mean(avg_rating), 2))

MecRating2018 = rbind(MecRating2018, temp)


#On ordonne le tableau nouvellement crée en foncion du rank : 

MecRating2018 = MecRating2018[order(-MecRating2018$meanRank),] 

#On selectionne les 5 premiers : 
top5MecsRating2018 = MecRating2018[1:5,]


#Même chose pour l'année 2017
#
#

#On prend tout les jeux de l'année 2018
lesJeuxDeLAnnee2017 = filter(file, file$year == 2017)

#On fait la moyenne des ranks des jeux : 

actionMovementPrograming2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Action...Movement.Programming == 1)

MecRating2017 = ddply(actionMovementPrograming2017, .(year), summarize,
                      mechanic = "Action / Movement Programming",
                      meanRank = round(mean(avg_rating), 2))

campaignBattleCardDriven2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Campaign...Battle.Card.Driven== 1)

temp = ddply(campaignBattleCardDriven2017, .(year), summarize,
             mechanic = "Campaign / Battle Card Driven",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)

commoditySpeculation2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Commodity.Speculation== 1)

temp = ddply(commoditySpeculation2017, .(year), summarize,
             mechanic = "Commodity Speculation",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)

areaControlAreaInfluence2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Area.Control...Area.Influence== 1)

temp = ddply(areaControlAreaInfluence2017, .(year), summarize,
             mechanic = "Area Control/ Area Influence",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)

areaMovement2017= filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Area.Movement== 1)

temp = ddply(areaMovement2017, .(year), summarize,
             mechanic = "Area Movement",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)
areaEnclosure2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Area.Enclosure== 1)

temp = ddply(areaEnclosure2017, .(year), summarize,
             mechanic = "Area Enclosure",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)

areaImpulse2017= filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Area.Impulse== 1)



temp = ddply(areaImpulse2017, .(year), summarize,
             mechanic = "Area Impulse",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)
acting2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Acting== 1)

temp = ddply(acting2017, .(year), summarize,
             mechanic = "Acting",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)

actionPointAllowanceSystem2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Action.Point.Allowance.System== 1)

temp = ddply(actionPointAllowanceSystem2017, .(year), summarize,
             mechanic = "Action Point Allowance System",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


auctionBidding2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Auction.Bidding== 1)

temp = ddply(auctionBidding2017, .(year), summarize,
             mechanic = "Auction/Bidding",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)

bettingWagering2017= filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Betting.Wagering== 1)

temp = ddply(bettingWagering2017, .(year), summarize,
             mechanic = "Betting / Wagering",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


cardDrafting2017= filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Card.Drafting== 1)

temp = ddply(cardDrafting2017, .(year), summarize,
             mechanic = "Card Drafing",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


cooperativePlay2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Co.operative.Play== 1)

temp = ddply(cooperativePlay2017, .(year), summarize,
             mechanic = "Cooperative Play",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)



deckPoolBuilding2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Deck...Pool.Building== 1)

temp = ddply(deckPoolBuilding2017, .(year), summarize,
             mechanic = "Deck/Pool Building",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


diceRolling2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Dice.Rolling== 1)

temp = ddply(diceRolling2017, .(year), summarize,
             mechanic = "Dice Rolling",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


hexAndCounter2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Hex.and.Counter== 1)

temp = ddply(hexAndCounter2017, .(year), summarize,
             mechanic = "Hex and Counter",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


lineDrawing2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Line.Drawing== 1)

temp = ddply(lineDrawing2017, .(year), summarize,
             mechanic = "Line Drawing",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


pointToPointMovement2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Point.to.Point.Movement== 1)

temp = ddply(pointToPointMovement2017, .(year), summarize,
             mechanic = "Point to Point movement",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


paperAndPencil2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Paper.and.Pencil== 1)

temp = ddply(paperAndPencil2017, .(year), summarize,
             mechanic = "Paper and Pencil",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


patternRecognition2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Pattern.Recognition== 1)

temp = ddply(patternRecognition2017, .(year), summarize,
             mechanic = "Pattern Recognition",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


setCollection2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Set.Collection== 1)

temp = ddply(setCollection2017, .(year), summarize,
             mechanic = "Set Collection",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)

secretUnitDeployment2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Secret.Unit.Deployment== 1)

temp = ddply(secretUnitDeployment2017, .(year), summarize,
             mechanic = "Secret Unit Deployment",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


simulation2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Simulation== 1)

temp = ddply(simulation2017, .(year), summarize,
             mechanic = "Simulation",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


stockHolding2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Stock.Holding== 1)

temp = ddply(stockHolding2017, .(year), summarize,
             mechanic = "Stock Holding",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


takeThat2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Take.That== 1)

temp = ddply(takeThat2017, .(year), summarize,
             mechanic = "Take That",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)



trading2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Trading== 1)

temp = ddply(trading2017, .(year), summarize,
             mechanic = "Trading",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


trickTaking2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Trick.Taking== 1)

temp = ddply(trickTaking2017, .(year), summarize,
             mechanic = "Trick Taking",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


tilePlacement2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Tile.Placement== 1)

temp = ddply(tilePlacement2017, .(year), summarize,
             mechanic = "Tile Placement",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


timeTrack2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Time.Track== 1)

temp = ddply(timeTrack2017, .(year), summarize,
             mechanic = "Time Track",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


gridMovement2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Grid.Movement== 1)

temp = ddply(gridMovement2017, .(year), summarize,
             mechanic = "Grid Movement",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


handManagement2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Hand.Management== 1)

temp = ddply(handManagement2017, .(year), summarize,
             mechanic = "Hand Management",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


memory2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Memory== 1)


temp = ddply(memory2017, .(year), summarize,
             mechanic = "Memory",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)

modularBoard2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Modular.Board== 1)

temp = ddply(modularBoard2017, .(year), summarize,
             mechanic = "Modular Board",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)

partnerships2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Partnerships== 1)

temp = ddply(partnerships2017, .(year), summarize,
             mechanic = "Partnersips",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


patternBuilding2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Pattern.Building== 1)

temp = ddply(patternBuilding2017, .(year), summarize,
             mechanic = "Pattern Building",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


pickUpAndDeliver2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Pick.up.and.Deliver== 1)

temp = ddply(pickUpAndDeliver2017, .(year), summarize,
             mechanic = "Pick Up and Deliver",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)



playerElimination2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Player.Elimination== 1)

temp = ddply(playerElimination2017, .(year), summarize,
             mechanic = "Player Elimination",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


pressYourLuck2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Press.Your.Luck== 1)

temp = ddply(pressYourLuck2017, .(year), summarize,
             mechanic = "Press Your Luck",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


rolePlaying2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Role.Playing== 1)

temp = ddply(rolePlaying2017, .(year), summarize,
             mechanic = "Role Playing",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


rollSpinAndMove2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Roll...Spin.and.Move== 1)

temp = ddply(rollSpinAndMove2017, .(year), summarize,
             mechanic = "Roll / Spin and Move",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)

routeNetworkBuilding2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Route.Network.Building== 1)

temp = ddply(routeNetworkBuilding2017, .(year), summarize,
             mechanic = "Route/Network building",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


simultaneousActionSelection2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Simultaneous.Action.Selection== 1)

temp = ddply(simultaneousActionSelection2017, .(year), summarize,
             mechanic = "Simultaneous Action Selection",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


variablePhaseOrder2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Variable.Phase.Order== 1)

temp = ddply(variablePhaseOrder2017, .(year), summarize,
             mechanic = "Variable Phase Order",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


storytelling2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Storytelling== 1)

temp = ddply(storytelling2017, .(year), summarize,
             mechanic = "Storytelling",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


variablePlayersPowers2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Variable.Player.Powers== 1)


temp = ddply(variablePlayersPowers2017, .(year), summarize,
             mechanic = "Variable Players Powers",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)

voting2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Voting== 1)



temp = ddply(voting2017, .(year), summarize,
             mechanic = "Voting",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


workerPlacement2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$Worker.Placement== 1)


temp = ddply(workerPlacement2017, .(year), summarize,
             mechanic = "Worker Placement",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


none2017 = filter(lesJeuxDeLAnnee2017, lesJeuxDeLAnnee2017$none)

temp = ddply(none2017, .(year), summarize,
             mechanic = "none",
             meanRank = round(mean(avg_rating), 2))

MecRating2017 = rbind(MecRating2017, temp)


MecRating2017 = MecRating2017[order(-MecRating2017$meanRank),] 
top5MecsRating2017 = MecRating2017[1:5,]




#On fait encore la me ch

lesJeuxDeLAnnee2016 = filter(file, file$year == 2016)                   



actionMovementPrograming2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Action...Movement.Programming == 1)

MecRating2016 = ddply(actionMovementPrograming2016, .(year), summarize,
                      mechanic = "Action / Movement Programming",
                      meanRank = round(mean(avg_rating), 2))

campaignBattleCardDriven2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Campaign...Battle.Card.Driven== 1)

temp = ddply(campaignBattleCardDriven2016, .(year), summarize,
             mechanic = "Campaign / Battle Card Driven",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)

commoditySpeculation2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Commodity.Speculation== 1)

temp = ddply(commoditySpeculation2016, .(year), summarize,
             mechanic = "Commodity Speculation",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)

areaControlAreaInfluence2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Area.Control...Area.Influence== 1)

temp = ddply(areaControlAreaInfluence2016, .(year), summarize,
             mechanic = "Area Control/ Area Influence",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)

areaMovement2016= filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Area.Movement== 1)

temp = ddply(areaMovement2016, .(year), summarize,
             mechanic = "Area Movement",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)
areaEnclosure2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Area.Enclosure== 1)

temp = ddply(areaEnclosure2016, .(year), summarize,
             mechanic = "Area Enclosure",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)

areaImpulse2016= filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Area.Impulse== 1)



temp = ddply(areaImpulse2016, .(year), summarize,
             mechanic = "Area Impulse",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)
acting2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Acting== 1)

temp = ddply(acting2016, .(year), summarize,
             mechanic = "Acting",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)

actionPointAllowanceSystem2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Action.Point.Allowance.System== 1)

temp = ddply(actionPointAllowanceSystem2016, .(year), summarize,
             mechanic = "Action Point Allowance System",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


auctionBidding2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Auction.Bidding== 1)

temp = ddply(auctionBidding2016, .(year), summarize,
             mechanic = "Auction/Bidding",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)

bettingWagering2016= filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Betting.Wagering== 1)

temp = ddply(bettingWagering2016, .(year), summarize,
             mechanic = "Betting / Wagering",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


cardDrafting2016= filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Card.Drafting== 1)

temp = ddply(cardDrafting2016, .(year), summarize,
             mechanic = "Card Drafing",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


cooperativePlay2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Co.operative.Play== 1)

temp = ddply(cooperativePlay2016, .(year), summarize,
             mechanic = "Cooperative Play",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)



deckPoolBuilding2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Deck...Pool.Building== 1)

temp = ddply(deckPoolBuilding2016, .(year), summarize,
             mechanic = "Deck/Pool Building",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


diceRolling2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Dice.Rolling== 1)

temp = ddply(diceRolling2016, .(year), summarize,
             mechanic = "Dice Rolling",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


hexAndCounter2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Hex.and.Counter== 1)

temp = ddply(hexAndCounter2016, .(year), summarize,
             mechanic = "Hex and Counter",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


lineDrawing2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Line.Drawing== 1)

temp = ddply(lineDrawing2016, .(year), summarize,
             mechanic = "Line Drawing",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


pointToPointMovement2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Point.to.Point.Movement== 1)

temp = ddply(pointToPointMovement2016, .(year), summarize,
             mechanic = "Point to Point movement",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


paperAndPencil2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Paper.and.Pencil== 1)

temp = ddply(paperAndPencil2016, .(year), summarize,
             mechanic = "Paper and Pencil",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


patternRecognition2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Pattern.Recognition== 1)

temp = ddply(patternRecognition2016, .(year), summarize,
             mechanic = "Pattern Recognition",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


setCollection2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Set.Collection== 1)

temp = ddply(setCollection2016, .(year), summarize,
             mechanic = "Set Collection",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)

secretUnitDeployment2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Secret.Unit.Deployment== 1)

temp = ddply(secretUnitDeployment2016, .(year), summarize,
             mechanic = "Secret Unit Deployment",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


simulation2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Simulation== 1)

temp = ddply(simulation2016, .(year), summarize,
             mechanic = "Simulation",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


stockHolding2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Stock.Holding== 1)

temp = ddply(stockHolding2016, .(year), summarize,
             mechanic = "Stock Holding",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


takeThat2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Take.That== 1)

temp = ddply(takeThat2016, .(year), summarize,
             mechanic = "Take That",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)



trading2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Trading== 1)

temp = ddply(trading2016, .(year), summarize,
             mechanic = "Trading",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


trickTaking2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Trick.Taking== 1)

temp = ddply(trickTaking2016, .(year), summarize,
             mechanic = "Trick Taking",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


tilePlacement2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Tile.Placement== 1)

temp = ddply(tilePlacement2016, .(year), summarize,
             mechanic = "Tile Placement",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


timeTrack2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Time.Track== 1)

temp = ddply(timeTrack2016, .(year), summarize,
             mechanic = "Time Track",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


gridMovement2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Grid.Movement== 1)

temp = ddply(gridMovement2016, .(year), summarize,
             mechanic = "Grid Movement",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


handManagement2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Hand.Management== 1)

temp = ddply(handManagement2016, .(year), summarize,
             mechanic = "Hand Management",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


memory2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Memory== 1)


temp = ddply(memory2016, .(year), summarize,
             mechanic = "Memory",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)

modularBoard2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Modular.Board== 1)

temp = ddply(modularBoard2016, .(year), summarize,
             mechanic = "Modular Board",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)

partnerships2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Partnerships== 1)

temp = ddply(partnerships2016, .(year), summarize,
             mechanic = "Partnersips",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


patternBuilding2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Pattern.Building== 1)

temp = ddply(patternBuilding2016, .(year), summarize,
             mechanic = "Pattern Building",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


pickUpAndDeliver2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Pick.up.and.Deliver== 1)

temp = ddply(pickUpAndDeliver2016, .(year), summarize,
             mechanic = "Pick Up and Deliver",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)



playerElimination2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Player.Elimination== 1)

temp = ddply(playerElimination2016, .(year), summarize,
             mechanic = "Player Elimination",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


pressYourLuck2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Press.Your.Luck== 1)

temp = ddply(pressYourLuck2016, .(year), summarize,
             mechanic = "Press Your Luck",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


rolePlaying2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Role.Playing== 1)

temp = ddply(rolePlaying2016, .(year), summarize,
             mechanic = "Role Playing",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


rollSpinAndMove2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Roll...Spin.and.Move== 1)

temp = ddply(rollSpinAndMove2016, .(year), summarize,
             mechanic = "Roll / Spin and Move",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)

routeNetworkBuilding2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Route.Network.Building== 1)

temp = ddply(routeNetworkBuilding2016, .(year), summarize,
             mechanic = "Route/Network building",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


simultaneousActionSelection2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Simultaneous.Action.Selection== 1)

temp = ddply(simultaneousActionSelection2016, .(year), summarize,
             mechanic = "Simultaneous Action Selection",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


variablePhaseOrder2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Variable.Phase.Order== 1)

temp = ddply(variablePhaseOrder2016, .(year), summarize,
             mechanic = "Variable Phase Order",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


storytelling2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Storytelling== 1)

temp = ddply(storytelling2016, .(year), summarize,
             mechanic = "Storytelling",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


variablePlayersPowers2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Variable.Player.Powers== 1)


temp = ddply(variablePlayersPowers2016, .(year), summarize,
             mechanic = "Variable Players Powers",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)

voting2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Voting== 1)



temp = ddply(voting2016, .(year), summarize,
             mechanic = "Voting",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


workerPlacement2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$Worker.Placement== 1)


temp = ddply(workerPlacement2016, .(year), summarize,
             mechanic = "Worker Placement",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


none2016 = filter(lesJeuxDeLAnnee2016, lesJeuxDeLAnnee2016$none)

temp = ddply(none2016, .(year), summarize,
             mechanic = "none",
             meanRank = round(mean(avg_rating), 2))

MecRating2016 = rbind(MecRating2016, temp)


MecRating2016 = MecRating2016[order(-MecRating2016$meanRank),] 

top5MecsRating2016 = MecRating2016[1:5,]

#
#
#On fait le même chose pour l'année 2015
#
#
#
#
lesJeuxDeLAnnee2015 = filter(file, file$year == 2015)                   



actionMovementPrograming2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Action...Movement.Programming == 1)

MecRating2015 = ddply(actionMovementPrograming2015, .(year), summarize,
                      mechanic = "Action / Movement Programming",
                      meanRank = round(mean(avg_rating), 2))

campaignBattleCardDriven2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Campaign...Battle.Card.Driven== 1)

temp = ddply(campaignBattleCardDriven2015, .(year), summarize,
             mechanic = "Campaign / Battle Card Driven",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)

commoditySpeculation2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Commodity.Speculation== 1)

temp = ddply(commoditySpeculation2015, .(year), summarize,
             mechanic = "Commodity Speculation",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)

areaControlAreaInfluence2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Area.Control...Area.Influence== 1)

temp = ddply(areaControlAreaInfluence2015, .(year), summarize,
             mechanic = "Area Control/ Area Influence",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)

areaMovement2015= filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Area.Movement== 1)

temp = ddply(areaMovement2015, .(year), summarize,
             mechanic = "Area Movement",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)
areaEnclosure2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Area.Enclosure== 1)

temp = ddply(areaEnclosure2015, .(year), summarize,
             mechanic = "Area Enclosure",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)

areaImpulse2015= filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Area.Impulse== 1)



temp = ddply(areaImpulse2015, .(year), summarize,
             mechanic = "Area Impulse",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)
acting2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Acting== 1)

temp = ddply(acting2015, .(year), summarize,
             mechanic = "Acting",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)

actionPointAllowanceSystem2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Action.Point.Allowance.System== 1)

temp = ddply(actionPointAllowanceSystem2015, .(year), summarize,
             mechanic = "Action Point Allowance System",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


auctionBidding2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Auction.Bidding== 1)

temp = ddply(auctionBidding2015, .(year), summarize,
             mechanic = "Auction/Bidding",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)

bettingWagering2015= filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Betting.Wagering== 1)

temp = ddply(bettingWagering2015, .(year), summarize,
             mechanic = "Betting / Wagering",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


cardDrafting2015= filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Card.Drafting== 1)

temp = ddply(cardDrafting2015, .(year), summarize,
             mechanic = "Card Drafing",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


cooperativePlay2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Co.operative.Play== 1)

temp = ddply(cooperativePlay2015, .(year), summarize,
             mechanic = "Cooperative Play",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)



deckPoolBuilding2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Deck...Pool.Building== 1)

temp = ddply(deckPoolBuilding2015, .(year), summarize,
             mechanic = "Deck/Pool Building",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


diceRolling2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Dice.Rolling== 1)

temp = ddply(diceRolling2015, .(year), summarize,
             mechanic = "Dice Rolling",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


hexAndCounter2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Hex.and.Counter== 1)

temp = ddply(hexAndCounter2015, .(year), summarize,
             mechanic = "Hex and Counter",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


lineDrawing2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Line.Drawing== 1)

temp = ddply(lineDrawing2015, .(year), summarize,
             mechanic = "Line Drawing",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


pointToPointMovement2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Point.to.Point.Movement== 1)

temp = ddply(pointToPointMovement2015, .(year), summarize,
             mechanic = "Point to Point movement",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


paperAndPencil2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Paper.and.Pencil== 1)

temp = ddply(paperAndPencil2015, .(year), summarize,
             mechanic = "Paper and Pencil",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


patternRecognition2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Pattern.Recognition== 1)

temp = ddply(patternRecognition2015, .(year), summarize,
             mechanic = "Pattern Recognition",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


setCollection2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Set.Collection== 1)

temp = ddply(setCollection2015, .(year), summarize,
             mechanic = "Set Collection",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)

secretUnitDeployment2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Secret.Unit.Deployment== 1)

temp = ddply(secretUnitDeployment2015, .(year), summarize,
             mechanic = "Secret Unit Deployment",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


simulation2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Simulation== 1)

temp = ddply(simulation2015, .(year), summarize,
             mechanic = "Simulation",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


stockHolding2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Stock.Holding== 1)

temp = ddply(stockHolding2015, .(year), summarize,
             mechanic = "Stock Holding",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


takeThat2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Take.That== 1)

temp = ddply(takeThat2015, .(year), summarize,
             mechanic = "Take That",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)



trading2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Trading== 1)

temp = ddply(trading2015, .(year), summarize,
             mechanic = "Trading",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


trickTaking2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Trick.Taking== 1)

temp = ddply(trickTaking2015, .(year), summarize,
             mechanic = "Trick Taking",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


tilePlacement2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Tile.Placement== 1)

temp = ddply(tilePlacement2015, .(year), summarize,
             mechanic = "Tile Placement",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


timeTrack2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Time.Track== 1)

temp = ddply(timeTrack2015, .(year), summarize,
             mechanic = "Time Track",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


gridMovement2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Grid.Movement== 1)

temp = ddply(gridMovement2015, .(year), summarize,
             mechanic = "Grid Movement",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


handManagement2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Hand.Management== 1)

temp = ddply(handManagement2015, .(year), summarize,
             mechanic = "Hand Management",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


memory2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Memory== 1)


temp = ddply(memory2015, .(year), summarize,
             mechanic = "Memory",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)

modularBoard2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Modular.Board== 1)

temp = ddply(modularBoard2015, .(year), summarize,
             mechanic = "Modular Board",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)

partnerships2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Partnerships== 1)

temp = ddply(partnerships2015, .(year), summarize,
             mechanic = "Partnersips",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


patternBuilding2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Pattern.Building== 1)

temp = ddply(patternBuilding2015, .(year), summarize,
             mechanic = "Pattern Building",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


pickUpAndDeliver2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Pick.up.and.Deliver== 1)

temp = ddply(pickUpAndDeliver2015, .(year), summarize,
             mechanic = "Pick Up and Deliver",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)



playerElimination2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Player.Elimination== 1)

temp = ddply(playerElimination2015, .(year), summarize,
             mechanic = "Player Elimination",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


pressYourLuck2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Press.Your.Luck== 1)

temp = ddply(pressYourLuck2015, .(year), summarize,
             mechanic = "Press Your Luck",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


rolePlaying2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Role.Playing== 1)

temp = ddply(rolePlaying2015, .(year), summarize,
             mechanic = "Role Playing",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


rollSpinAndMove2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Roll...Spin.and.Move== 1)

temp = ddply(rollSpinAndMove2015, .(year), summarize,
             mechanic = "Roll / Spin and Move",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)

routeNetworkBuilding2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Route.Network.Building== 1)

temp = ddply(routeNetworkBuilding2015, .(year), summarize,
             mechanic = "Route/Network building",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


simultaneousActionSelection2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Simultaneous.Action.Selection== 1)

temp = ddply(simultaneousActionSelection2015, .(year), summarize,
             mechanic = "Simultaneous Action Selection",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


variablePhaseOrder2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Variable.Phase.Order== 1)

temp = ddply(variablePhaseOrder2015, .(year), summarize,
             mechanic = "Variable Phase Order",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


storytelling2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Storytelling== 1)

temp = ddply(storytelling2015, .(year), summarize,
             mechanic = "Storytelling",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


variablePlayersPowers2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Variable.Player.Powers== 1)


temp = ddply(variablePlayersPowers2015, .(year), summarize,
             mechanic = "Variable Players Powers",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)

voting2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Voting== 1)



temp = ddply(voting2015, .(year), summarize,
             mechanic = "Voting",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


workerPlacement2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$Worker.Placement== 1)


temp = ddply(workerPlacement2015, .(year), summarize,
             mechanic = "Worker Placement",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


none2015 = filter(lesJeuxDeLAnnee2015, lesJeuxDeLAnnee2015$none)

temp = ddply(none2015, .(year), summarize,
             mechanic = "none",
             meanRank = round(mean(avg_rating), 2))

MecRating2015 = rbind(MecRating2015, temp)


MecRating2015 = MecRating2015[order(-MecRating2015$meanRank),] 

top5MecsRating2015 = MecRating2015[1:5,]


#
#
#On fait le même chose pour l'année 2014
#
#
#
#
lesJeuxDeLAnnee2014 = filter(file, file$year == 2014)                   



actionMovementPrograming2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Action...Movement.Programming == 1)

MecRating2014 = ddply(actionMovementPrograming2014, .(year), summarize,
                      mechanic = "Action / Movement Programming",
                      meanRank = round(mean(avg_rating), 2))

campaignBattleCardDriven2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Campaign...Battle.Card.Driven== 1)

temp = ddply(campaignBattleCardDriven2014, .(year), summarize,
             mechanic = "Campaign / Battle Card Driven",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)

commoditySpeculation2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Commodity.Speculation== 1)

temp = ddply(commoditySpeculation2014, .(year), summarize,
             mechanic = "Commodity Speculation",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)

areaControlAreaInfluence2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Area.Control...Area.Influence== 1)

temp = ddply(areaControlAreaInfluence2014, .(year), summarize,
             mechanic = "Area Control/ Area Influence",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)

areaMovement2014= filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Area.Movement== 1)

temp = ddply(areaMovement2014, .(year), summarize,
             mechanic = "Area Movement",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)
areaEnclosure2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Area.Enclosure== 1)

temp = ddply(areaEnclosure2014, .(year), summarize,
             mechanic = "Area Enclosure",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)

areaImpulse2014= filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Area.Impulse== 1)



temp = ddply(areaImpulse2014, .(year), summarize,
             mechanic = "Area Impulse",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)
acting2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Acting== 1)

temp = ddply(acting2014, .(year), summarize,
             mechanic = "Acting",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)

actionPointAllowanceSystem2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Action.Point.Allowance.System== 1)

temp = ddply(actionPointAllowanceSystem2014, .(year), summarize,
             mechanic = "Action Point Allowance System",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


auctionBidding2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Auction.Bidding== 1)

temp = ddply(auctionBidding2014, .(year), summarize,
             mechanic = "Auction/Bidding",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)

bettingWagering2014= filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Betting.Wagering== 1)

temp = ddply(bettingWagering2014, .(year), summarize,
             mechanic = "Betting / Wagering",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


cardDrafting2014= filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Card.Drafting== 1)

temp = ddply(cardDrafting2014, .(year), summarize,
             mechanic = "Card Drafing",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


cooperativePlay2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Co.operative.Play== 1)

temp = ddply(cooperativePlay2014, .(year), summarize,
             mechanic = "Cooperative Play",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)



deckPoolBuilding2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Deck...Pool.Building== 1)

temp = ddply(deckPoolBuilding2014, .(year), summarize,
             mechanic = "Deck/Pool Building",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


diceRolling2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Dice.Rolling== 1)

temp = ddply(diceRolling2014, .(year), summarize,
             mechanic = "Dice Rolling",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


hexAndCounter2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Hex.and.Counter== 1)

temp = ddply(hexAndCounter2014, .(year), summarize,
             mechanic = "Hex and Counter",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


lineDrawing2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Line.Drawing== 1)

temp = ddply(lineDrawing2014, .(year), summarize,
             mechanic = "Line Drawing",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


pointToPointMovement2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Point.to.Point.Movement== 1)

temp = ddply(pointToPointMovement2014, .(year), summarize,
             mechanic = "Point to Point movement",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


paperAndPencil2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Paper.and.Pencil== 1)

temp = ddply(paperAndPencil2014, .(year), summarize,
             mechanic = "Paper and Pencil",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


patternRecognition2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Pattern.Recognition== 1)

temp = ddply(patternRecognition2014, .(year), summarize,
             mechanic = "Pattern Recognition",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


setCollection2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Set.Collection== 1)

temp = ddply(setCollection2014, .(year), summarize,
             mechanic = "Set Collection",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)

secretUnitDeployment2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Secret.Unit.Deployment== 1)

temp = ddply(secretUnitDeployment2014, .(year), summarize,
             mechanic = "Secret Unit Deployment",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


simulation2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Simulation== 1)

temp = ddply(simulation2014, .(year), summarize,
             mechanic = "Simulation",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


stockHolding2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Stock.Holding== 1)

temp = ddply(stockHolding2014, .(year), summarize,
             mechanic = "Stock Holding",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


takeThat2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Take.That== 1)

temp = ddply(takeThat2014, .(year), summarize,
             mechanic = "Take That",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)



trading2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Trading== 1)

temp = ddply(trading2014, .(year), summarize,
             mechanic = "Trading",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


trickTaking2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Trick.Taking== 1)

temp = ddply(trickTaking2014, .(year), summarize,
             mechanic = "Trick Taking",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


tilePlacement2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Tile.Placement== 1)

temp = ddply(tilePlacement2014, .(year), summarize,
             mechanic = "Tile Placement",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


timeTrack2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Time.Track== 1)

temp = ddply(timeTrack2014, .(year), summarize,
             mechanic = "Time Track",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


gridMovement2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Grid.Movement== 1)

temp = ddply(gridMovement2014, .(year), summarize,
             mechanic = "Grid Movement",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


handManagement2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Hand.Management== 1)

temp = ddply(handManagement2014, .(year), summarize,
             mechanic = "Hand Management",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


memory2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Memory== 1)


temp = ddply(memory2014, .(year), summarize,
             mechanic = "Memory",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)

modularBoard2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Modular.Board== 1)

temp = ddply(modularBoard2014, .(year), summarize,
             mechanic = "Modular Board",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)

partnerships2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Partnerships== 1)

temp = ddply(partnerships2014, .(year), summarize,
             mechanic = "Partnersips",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


patternBuilding2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Pattern.Building== 1)

temp = ddply(patternBuilding2014, .(year), summarize,
             mechanic = "Pattern Building",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


pickUpAndDeliver2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Pick.up.and.Deliver== 1)

temp = ddply(pickUpAndDeliver2014, .(year), summarize,
             mechanic = "Pick Up and Deliver",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)



playerElimination2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Player.Elimination== 1)

temp = ddply(playerElimination2014, .(year), summarize,
             mechanic = "Player Elimination",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


pressYourLuck2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Press.Your.Luck== 1)

temp = ddply(pressYourLuck2014, .(year), summarize,
             mechanic = "Press Your Luck",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


rolePlaying2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Role.Playing== 1)

temp = ddply(rolePlaying2014, .(year), summarize,
             mechanic = "Role Playing",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


rollSpinAndMove2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Roll...Spin.and.Move== 1)

temp = ddply(rollSpinAndMove2014, .(year), summarize,
             mechanic = "Roll / Spin and Move",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)

routeNetworkBuilding2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Route.Network.Building== 1)

temp = ddply(routeNetworkBuilding2014, .(year), summarize,
             mechanic = "Route/Network building",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


simultaneousActionSelection2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Simultaneous.Action.Selection== 1)

temp = ddply(simultaneousActionSelection2014, .(year), summarize,
             mechanic = "Simultaneous Action Selection",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


variablePhaseOrder2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Variable.Phase.Order== 1)

temp = ddply(variablePhaseOrder2014, .(year), summarize,
             mechanic = "Variable Phase Order",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


storytelling2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Storytelling== 1)

temp = ddply(storytelling2014, .(year), summarize,
             mechanic = "Storytelling",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


variablePlayersPowers2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Variable.Player.Powers== 1)


temp = ddply(variablePlayersPowers2014, .(year), summarize,
             mechanic = "Variable Players Powers",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)

voting2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Voting== 1)



temp = ddply(voting2014, .(year), summarize,
             mechanic = "Voting",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


workerPlacement2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$Worker.Placement== 1)


temp = ddply(workerPlacement2014, .(year), summarize,
             mechanic = "Worker Placement",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


none2014 = filter(lesJeuxDeLAnnee2014, lesJeuxDeLAnnee2014$none)

temp = ddply(none2014, .(year), summarize,
             mechanic = "none",
             meanRank = round(mean(avg_rating), 2))

MecRating2014 = rbind(MecRating2014, temp)


MecRating2014 = MecRating2014[order(-MecRating2014$meanRank),] 

top5MecsRating2014 = MecRating2014[1:5,]




#
#
#On fait le même chose pour l'année 2013
#
#
#
#
lesJeuxDeLAnnee2013 = filter(file, file$year == 2013)                   



actionMovementPrograming2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Action...Movement.Programming == 1)

MecRating2013 = ddply(actionMovementPrograming2013, .(year), summarize,
                      mechanic = "Action / Movement Programming",
                      meanRank = round(mean(avg_rating), 2))

campaignBattleCardDriven2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Campaign...Battle.Card.Driven== 1)

temp = ddply(campaignBattleCardDriven2013, .(year), summarize,
             mechanic = "Campaign / Battle Card Driven",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)

commoditySpeculation2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Commodity.Speculation== 1)

temp = ddply(commoditySpeculation2013, .(year), summarize,
             mechanic = "Commodity Speculation",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)

areaControlAreaInfluence2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Area.Control...Area.Influence== 1)

temp = ddply(areaControlAreaInfluence2013, .(year), summarize,
             mechanic = "Area Control/ Area Influence",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)

areaMovement2013= filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Area.Movement== 1)

temp = ddply(areaMovement2013, .(year), summarize,
             mechanic = "Area Movement",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)
areaEnclosure2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Area.Enclosure== 1)

temp = ddply(areaEnclosure2013, .(year), summarize,
             mechanic = "Area Enclosure",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)

areaImpulse2013= filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Area.Impulse== 1)



temp = ddply(areaImpulse2013, .(year), summarize,
             mechanic = "Area Impulse",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)
acting2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Acting== 1)

temp = ddply(acting2013, .(year), summarize,
             mechanic = "Acting",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)

actionPointAllowanceSystem2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Action.Point.Allowance.System== 1)

temp = ddply(actionPointAllowanceSystem2013, .(year), summarize,
             mechanic = "Action Point Allowance System",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


auctionBidding2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Auction.Bidding== 1)

temp = ddply(auctionBidding2013, .(year), summarize,
             mechanic = "Auction/Bidding",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)

bettingWagering2013= filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Betting.Wagering== 1)

temp = ddply(bettingWagering2013, .(year), summarize,
             mechanic = "Betting / Wagering",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


cardDrafting2013= filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Card.Drafting== 1)

temp = ddply(cardDrafting2013, .(year), summarize,
             mechanic = "Card Drafing",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


cooperativePlay2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Co.operative.Play== 1)

temp = ddply(cooperativePlay2013, .(year), summarize,
             mechanic = "Cooperative Play",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)



deckPoolBuilding2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Deck...Pool.Building== 1)

temp = ddply(deckPoolBuilding2013, .(year), summarize,
             mechanic = "Deck/Pool Building",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


diceRolling2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Dice.Rolling== 1)

temp = ddply(diceRolling2013, .(year), summarize,
             mechanic = "Dice Rolling",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


hexAndCounter2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Hex.and.Counter== 1)

temp = ddply(hexAndCounter2013, .(year), summarize,
             mechanic = "Hex and Counter",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


lineDrawing2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Line.Drawing== 1)

temp = ddply(lineDrawing2013, .(year), summarize,
             mechanic = "Line Drawing",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


pointToPointMovement2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Point.to.Point.Movement== 1)

temp = ddply(pointToPointMovement2013, .(year), summarize,
             mechanic = "Point to Point movement",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


paperAndPencil2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Paper.and.Pencil== 1)

temp = ddply(paperAndPencil2013, .(year), summarize,
             mechanic = "Paper and Pencil",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


patternRecognition2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Pattern.Recognition== 1)

temp = ddply(patternRecognition2013, .(year), summarize,
             mechanic = "Pattern Recognition",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


setCollection2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Set.Collection== 1)

temp = ddply(setCollection2013, .(year), summarize,
             mechanic = "Set Collection",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)

secretUnitDeployment2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Secret.Unit.Deployment== 1)

temp = ddply(secretUnitDeployment2013, .(year), summarize,
             mechanic = "Secret Unit Deployment",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


simulation2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Simulation== 1)

temp = ddply(simulation2013, .(year), summarize,
             mechanic = "Simulation",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


stockHolding2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Stock.Holding== 1)

temp = ddply(stockHolding2013, .(year), summarize,
             mechanic = "Stock Holding",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


takeThat2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Take.That== 1)

temp = ddply(takeThat2013, .(year), summarize,
             mechanic = "Take That",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)



trading2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Trading== 1)

temp = ddply(trading2013, .(year), summarize,
             mechanic = "Trading",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


trickTaking2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Trick.Taking== 1)

temp = ddply(trickTaking2013, .(year), summarize,
             mechanic = "Trick Taking",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


tilePlacement2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Tile.Placement== 1)

temp = ddply(tilePlacement2013, .(year), summarize,
             mechanic = "Tile Placement",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


timeTrack2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Time.Track== 1)

temp = ddply(timeTrack2013, .(year), summarize,
             mechanic = "Time Track",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


gridMovement2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Grid.Movement== 1)

temp = ddply(gridMovement2013, .(year), summarize,
             mechanic = "Grid Movement",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


handManagement2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Hand.Management== 1)

temp = ddply(handManagement2013, .(year), summarize,
             mechanic = "Hand Management",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


memory2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Memory== 1)


temp = ddply(memory2013, .(year), summarize,
             mechanic = "Memory",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)

modularBoard2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Modular.Board== 1)

temp = ddply(modularBoard2013, .(year), summarize,
             mechanic = "Modular Board",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)

partnerships2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Partnerships== 1)

temp = ddply(partnerships2013, .(year), summarize,
             mechanic = "Partnersips",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


patternBuilding2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Pattern.Building== 1)

temp = ddply(patternBuilding2013, .(year), summarize,
             mechanic = "Pattern Building",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


pickUpAndDeliver2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Pick.up.and.Deliver== 1)

temp = ddply(pickUpAndDeliver2013, .(year), summarize,
             mechanic = "Pick Up and Deliver",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)



playerElimination2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Player.Elimination== 1)

temp = ddply(playerElimination2013, .(year), summarize,
             mechanic = "Player Elimination",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


pressYourLuck2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Press.Your.Luck== 1)

temp = ddply(pressYourLuck2013, .(year), summarize,
             mechanic = "Press Your Luck",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


rolePlaying2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Role.Playing== 1)

temp = ddply(rolePlaying2013, .(year), summarize,
             mechanic = "Role Playing",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


rollSpinAndMove2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Roll...Spin.and.Move== 1)

temp = ddply(rollSpinAndMove2013, .(year), summarize,
             mechanic = "Roll / Spin and Move",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)

routeNetworkBuilding2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Route.Network.Building== 1)

temp = ddply(routeNetworkBuilding2013, .(year), summarize,
             mechanic = "Route/Network building",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


simultaneousActionSelection2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Simultaneous.Action.Selection== 1)

temp = ddply(simultaneousActionSelection2013, .(year), summarize,
             mechanic = "Simultaneous Action Selection",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


variablePhaseOrder2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Variable.Phase.Order== 1)

temp = ddply(variablePhaseOrder2013, .(year), summarize,
             mechanic = "Variable Phase Order",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


storytelling2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Storytelling== 1)

temp = ddply(storytelling2013, .(year), summarize,
             mechanic = "Storytelling",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


variablePlayersPowers2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Variable.Player.Powers== 1)


temp = ddply(variablePlayersPowers2013, .(year), summarize,
             mechanic = "Variable Players Powers",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)

voting2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Voting== 1)



temp = ddply(voting2013, .(year), summarize,
             mechanic = "Voting",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


workerPlacement2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$Worker.Placement== 1)


temp = ddply(workerPlacement2013, .(year), summarize,
             mechanic = "Worker Placement",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


none2013 = filter(lesJeuxDeLAnnee2013, lesJeuxDeLAnnee2013$none)

temp = ddply(none2013, .(year), summarize,
             mechanic = "none",
             meanRank = round(mean(avg_rating), 2))

MecRating2013 = rbind(MecRating2013, temp)


MecRating2013 = MecRating2013[order(-MecRating2013$meanRank),] 

top5MecsRating2013 = MecRating2013[1:5,]

#On fait un tableau récapitulatif : 
top5AllYears = rbind(top5MecsRating2013,top5MecsRating2014, top5MecsRating2015, top5MecsRating2016, top5MecsRating2017, top5MecsRating2018)


ggplot(top5AllYears, aes(x = year, y = meanRank, fill = mechanic )) + geom_bar( stat="identity", position = "dodge") 







