library(shiny)
library(dplyr)
library(lubridate)

ui <- fluidPage( 
        titlePanel("Elo score for boardgaming"),
        tabsetPanel( 
                
                tabPanel("The App",
                         
                        sidebarLayout( sidebarPanel(
                              textInput(inputId = "the.site", "The google sheet" , value = "The url for the published csv file"),
                              numericInput(inputId = "the.year", "The Year, (0 = all years)", value = 0),
                              numericInput(inputId = "k", "k value", value = 16),
                              numericInput(inputId = "default.score", "Default score", value = 2000),
                              #numericInput(inputId = "the.game", "Basic score for the game", value = 2000),
                              actionButton("goButton", "Calculate Score")
                                      ),
                                        mainPanel(
                                                verticalLayout( 
                                                verbatimTextOutput("descript"),
                                                tableOutput("score.changing"),
                                                verbatimTextOutput("descript2"),
                                                tableOutput("final.score")
                                                )
                                                
                                                
                                        )
                                       )
                        ), 
                              

                tabPanel("Help", 
                         verticalLayout(
                                 tags$h1("Elo explained"),
                                 "This shiny app uses Elo to calculate score for each player. The first players will start with the _default.score_, every player joining after that will start with the average score of current players.",
                                 tags$p("When players go up against the game (co-op) the score of the game is always the average of the current players score."),
                                 tags$h2("the inputs"),
                                 
                                 tags$h3("The google sheet"),
                                 "Here one must input the direction to the google sheet after it has been published to the web as csv and set up correctly. The way it should be set up can be found in the next tab.",
                                 "when publishing google sheets one must go to file and from there publish to the web as csv. This is not the same as sharing a google sheet.",
                                 
                                 tags$h3("The year"),
                                 "Which year to filter out. it is only possible to filter one year. If this is left as 0, the whole sheet (all years) will be used for calculations",
                                 
                                 tags$h3("k value"),
                                 "K value decides the increments on who much players increase in score when winning or losing. most common numbers are 16 and 32.",
                                 
                                 tags$h3("Default value"),
                                 "the default value determines what score the first players will start with. Most common is 2000",
                                 
                                 #tags$h3("Basic score for the game"),
                                 #"This determines what the score is for the game when it goes up against players. It defaults on 2000 but perhaps I will make it adaptive in the future or offer different varieties.",
                                 tags$br(),
                                 "If you want to see something specific email me at -- noi at logn dot is"
                                
                         )
                )
                # object 3
                         
                         ),
                tabPanel("Google sheet setup") 
                              
               )
        


        
       
        
        
        



server <- function(input, output) {
        observeEvent(input$goButton, {
        
                
                
                
               
                        
                        
                        ## Main function
                        ### inniheldur the.year = 0, k = 16, default.score = 2000, the.game = 2000
                        
                        ###TEST ### k <- 16
        #(print("1"))
                        DF <- read.csv(input$the.site, na.strings=c("","NA"))
####TEST#### DF <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTRlgvBwmSBPtL_WSGW4TN8qLe1dYpySU2i8h4R08PPjm7UygsNEdD3L1cqksA0ISF23mI_NgEK_SGQ/pub?output=csv", na.strings=c("","NA"))
                        
                        #### score listi over time
                        
                        ### fix the date
                        DF$Dagsetning <- as.Date(DF$Dagsetning, format="%m/%d/%Y")
                        
                        
                        #subsetting the database if year is not set to default
                        the.year <- input$the.year
####TEST the.year <- 0
                        the.text <- "Score and changes in score over all time (sorted by score)"
        #(print("2"))
                        if(the.year != 0){
                                DF <- (DF[year(DF$Dagsetning) == the.year,])
                                the.text <- paste0("Score and changes in score in the year ", the.year, "sorted by score")
                        }
                        
                        ##Clean up all NA players and remove from
                        DF <- Filter(function(x)!all(is.na(x)), DF)
                        
                        DF.scores.hist <- data.frame(names(DF[,5:length(DF)]))
                        
                        ### Búa til grunn scorelista með NA sem stig
                        DF.scores <- data.frame(names(DF[,5:length(DF)]), rep(NA,length(DF[,5:length(DF)])))
                        
        #(print("3"))                        
                        
                        colnames(DF.scores) <- (c("names", "score"))
                        colnames(DF.scores.hist) <- ("names")
                        
                        
                        # Set the default beginning score
                        default.score <- input$default.score
###TEST### default.score <- 2000
                        
                
                        ###  wins / loses / number of games / win percentages
                        #samset <- data.frame(matrix(nrow= nrow(DF.scores)-1, ncol=5))
                        
                        samset <- as.data.frame(apply(DF[6:length(DF)-1], 2, function(x) length(which(x == "Win"))))
                        samset <- merge(samset,as.data.frame(apply(DF[6:length(DF)-1], 2, function(x) length(which(x == "Lose")))),by="row.names",all.x=TRUE)
                        colnames(samset) <- c("names","wins", "losses")
                        samset$total.games <- samset[,2] + samset[,3] 
                        samset$ratio <- round(samset$wins/samset$losses, 2)
                        samset$percentage.won <- round(samset$wins/(samset$wins+samset$losses)*100) 
                        
        #(print("4"))                        
                       
                        
                        
                        #DF <- DF[DF$Spilið != "Lose",]
                        #rownames(DF) <- seq(length=nrow(DF))
                        
                        
                        the.game <- 2000
###TEST the.game <- 2000
                        
                        score.results <- data.frame()
                        score.diff <- data.frame()
                        
        #(print("5"))                   
                        elo <- function(winner.score, loser.score, k = input$k) {
                                ## Transform rating
                                winner.score.simp <- 10^(winner.score/400) 
                                loser.score.simp <- 10^(loser.score/400)
                                
                                ### calculate expected rating
                                exp.rating.winner <- winner.score.simp/(winner.score.simp+loser.score.simp)
                                exp.rating.loser <- loser.score.simp/(winner.score.simp+loser.score.simp)
                                
                                ### assuming 1 for winning and 0 for losing
                                ### calculating updated elo rating
                                ### k defaults at 32
                                winner.score <- round(winner.score + k*(1 - exp.rating.winner), 0)
                                loser.score <- round(loser.score + k*(0 - exp.rating.loser), 0)
                                win.lose.result <- c(winner.score, loser.score)
                                #print(winner.score)
                                #print(loser.score)
                                
                                return(win.lose.result)
                        }
                        
                        DF$Dagsetning <- as.character(DF$Dagsetning)
                        
                        for (i in 1:nrow(DF)) {
                                #print(paste0("this is loop number: ", i))
                                won.list  <- names(DF)[which(DF[i,] == "Win", arr.ind = T)[, "col"]]
                                lost.list <- names(DF)[which(DF[i,] == "Lose", arr.ind = T)[, "col"]]
                                print(won.list)
                                print(lost.list)
                                #print(paste0("the won.list: ", won.list))
                                #print(paste0("The lost.list: ", lost.list))
                                
                                all.players <- append(won.list, lost.list)
                                
                                
                                
                                #print(paste0("all.players: ", all.players))
                                
                                ### gefa 2000 ef allir í hópnum ef first players s.s alfyrsta spilun
        
                                
                               
        if(all(is.na(DF.scores$score))){
                DF.scores$score <- with(DF.scores,ifelse(names %in% all.players & is.na(score),yes=default.score,no=score))
                print(DF.scores)
        }
        if(!all(is.na(DF.scores$score))){
                # Hér er gert meðaltal af núverandi spilurum
                #Sækja skor frá fyrri leikjum.
                
                DF.scores$score <- round(with(DF.scores,ifelse(names %in% all.players & is.na(score),yes=mean(na.omit(DF.scores$score)),no=score)),0)
                print(DF.scores)
                #DF.scores$score <- 2000
        }
                #stig spilsins er alltaf gert að meðaltalinu.
                                the.game <- round(mean(DF.scores[DF.scores$names %in% all.players,]$score))               
                                
                                

                #print(paste0("current scores: ", DF.scores))
                                # prepare comparing score
                                # first get the average of the loser to pit against winner
                                loser.mean <- round(mean(DF.scores$score[DF.scores$names %in% lost.list]),0)
                        
                                #print(paste0("the loser.mean: ", loser.mean))
                                # than the average of the winners to pit against losers
                                winner.mean <- round(mean(DF.scores$score[DF.scores$names %in% won.list]),0)
                                
                                #print(paste0("the winner.mean: ", winner.mean))
                                # than each of the winners score is returned
                                # Hér er allt reiknað og því skilað til baka í gagnagrunninn
                                # Hér eru bara sigurvegarar
                                temp <- DF.scores
                                temp$score <- NA
                                
                                for (e in won.list){
                                        temp$score[temp$names == e] <- round(elo(DF.scores$score[DF.scores$names == e], loser.mean)[1],0)  
                                        #print(round(elo(DF.scores$score[DF.scores$names == e], loser.mean)[1],0))
                                        #DF.scores$score[DF.scores$names == e] <- new.score.win
                                }
                                for (f in lost.list){
                                        temp$score[temp$names == f] <- round(elo(winner.mean, DF.scores$score[DF.scores$names == f])[2],0)
                                        #print(round(elo(DF.scores$score[DF.scores$names == f], winner.mean)[2],0))
                                        #DF.scores$score[DF.scores$names == f] <- new.score.lose
                                        #print(paste0("new score lose: ", new.score.lose))   
                                }
                                
                                DF.scores$score <- ifelse(is.na(temp$score), DF.scores$score, DF.scores$score <- temp$score)
                                
                                # resetta spilið
                                DF.scores$score[DF.scores$names == "Spilið"] <- the.game
                                
                                #Making history chart
                                DF.scores.hist <- cbind(DF.scores.hist, DF.scores$score)
                                colnames(DF.scores.hist)[length(DF.scores.hist)] <- as.character(DF[i,2])
                                
                                #print(loser.mean, winner.mean)
                        }
                        
                        
                        # Hér er reiknað hve mikið fólk hefur hækkað yfir tímabilið
                        for (i in 2:nrow(DF.scores.hist)-1){
                                
                                all.list <- which(!is.na(DF.scores.hist[i,]))
                                fsl <- DF.scores.hist[i,all.list[2]]
                                scl <- DF.scores.hist[i,length(DF.scores.hist)]
                                #score.results <- rbind(score.results, scl)
                                score.diff <- rbind(score.diff, (scl-fsl))
                                score.results <- rbind(score.results, scl)
                                
                        }
                        
                        score.results <- cbind(score.results, score.diff)
                        score.results <- cbind(DF.scores[1:nrow(DF.scores.hist)-1,1], score.results)
                        colnames(score.results) <- (c("names", "score", "score.change"))
                        
                        # Sorter eftir stigum
                        score.results <- score.results[order(-score.results$score),]
                        
                        # breyta INF í 0 ef INF er til staðar í ratio
                        if(any(is.infinite(samset$ratio)) == TRUE){
                                print("TRUE")
                                samset[!is.finite(samset$ratio),]$ratio <- 0
                        }
                        
                        
                        #sorterað eftir W/L ratio
                        samset <- samset[order(-samset$ratio),]
                        
                        final.print <- ifelse(the.year == 0, "fyrir öll árin. Heildar samantekt leikja. Sorterað eftir W/L ratio", paste0("fyrir árið ", the.year, ". Samantekt leikja. Sorterað eftir W/L ratio"))
                        
                        
                        output$score.changing <- renderTable(score.results)
                        output$descript <- renderPrint(the.text)
                        output$descript2 <- renderPrint(final.print)
                        output$final.score <- renderTable(samset)
                        
                      
                
                })
                
       
        
}


shinyApp(ui = ui, server = server)


