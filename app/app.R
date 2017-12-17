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
                              numericInput(inputId = "the.game", "Basic score for the game", value = 2000),
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
                              

                tabPanel("Help", "contents"),
                tabPanel("Google sheet setup", "contents") 
                              
               )
        
        )


        
       
        
        
        



server <- function(input, output) {
        observeEvent(input$goButton, {
        
                
                
                
               
                        
                        
                        ## Main function
                        ### inniheldur the.year = 0, k = 16, default.score = 2000, the.game = 2000, min.game = 3
                        
                        
                        
                        DF <- read.csv(input$the.site)
                        #### score listi over time
                        DF.scores.hist <- data.frame(names(DF[,5:length(DF)]))
                        
                        ### Búa til grunn scorelista með NA sem stig
                        DF.scores <- data.frame(names(DF[,5:length(DF)]), rep(NA,length(DF[,5:length(DF)])))
                        
                        
                        
                        colnames(DF.scores) <- (c("names", "score"))
                        colnames(DF.scores.hist) <- ("names")
                        
                        
                        # Set the default beginning score
                        default.score <- input$default.score
                        
                
                        ###  wins / loses / number of games / win percentages
                        #samset <- data.frame(matrix(nrow= nrow(DF.scores)-1, ncol=5))
                        
                        samset <- as.data.frame(apply(DF[6:length(DF)-1], 2, function(x) length(which(x == "Win"))))
                        samset <- merge(samset,as.data.frame(apply(DF[6:length(DF)-1], 2, function(x) length(which(x == "Lose")))),by="row.names",all.x=TRUE)
                        colnames(samset) <- c("names","wins", "losses")
                        samset$total.games <- samset[,2] + samset[,3] 
                        samset$ratio <- round(samset$wins/samset$losses, 2)
                        samset$percentage.won <- round(samset$wins/(samset$wins+samset$losses)*100) 
                        
                        
                        DF$Dagsetning <- as.Date(DF$Dagsetning, "%m/%d/%Y")
                        
                        
                        #DF <- DF[DF$Spilið != "Lose",]
                        #rownames(DF) <- seq(length=nrow(DF))
                        
                        
                        
                        
                        
                        #subsetting the database if year is not set to default
                        the.year <- input$the.year
                        the.game <- input$the.game
                        
                        score.results <- data.frame()
                        the.text <- "Changes in score over all time."
                        if(the.year != 0){
                                DF <- (DF[year(DF$Dagsetning) == the.year,])
                                the.text <- paste0("changes in score in the year ", the.year)
                        }
                        
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
                                won.list  <- names(DF)[which(DF[i,] == "Win", arr.ind = T)[, "col"]]
                                lost.list <- names(DF)[which(DF[i,] == "Lose", arr.ind = T)[, "col"]]
                                
                                all.players <- append(won.list, lost.list)
                                ### gefa 2000 ef allir í hópnum ef first players s.s alfyrsta spilun
                                if(all(is.na(DF.scores$score)))
                                {
                                        DF.scores$score <- with(DF.scores,ifelse(names %in% all.players & is.na(score),yes=default.score,no=score))
                                }
                                if(!all(is.na(DF.scores$score)))        
                                {
                                        # Hér er gert meðaltal af núverandi spilurum
                                        DF.scores$score <- with(DF.scores,ifelse(names %in% all.players & is.na(score),yes=mean(na.omit(DF.scores$score)),no=score))
                                }
                                
                                # prepare comparing score
                                # first get the average of the loser to pit against winner
                                loser.mean <- mean(DF.scores$score[DF.scores$names %in% lost.list])
                                # than the average of the winners to pit against losers
                                winner.mean <- mean(DF.scores$score[DF.scores$names %in% won.list])
                                # than each of the winners score is returned
                                # Hér er allt reiknað og því skilað til baka í gagnagrunninn
                                # Hér eru bara sigurvegarar
                                for (e in won.list){
                                        new.score.win <- elo(DF.scores$score[DF.scores$names == e], loser.mean)[1]
                                        DF.scores$score[DF.scores$names == e] <- new.score.win
                                }
                                for (e in lost.list){
                                        new.score.lose <- elo(DF.scores$score[DF.scores$names == e], winner.mean)[2]
                                        DF.scores$score[DF.scores$names == e] <- new.score.lose      
                                }
                                # resetta spilið
                                DF.scores$score[DF.scores$names == "Spilið"] <- the.game
                                
                                #Making history chart
                                DF.scores.hist <- cbind(DF.scores.hist, DF.scores$score)
                                colnames(DF.scores.hist)[length(DF.scores.hist)] <- as.character(DF[i,2])
                                
                                
                        }
                        
                        
                        # Hér er reiknað hve mikið fólk hefur hækkað yfir tímabilið
                        for (i in 2:nrow(DF.scores.hist)-1){
                                
                                all.list <- which(!is.na(DF.scores.hist[i,]))
                                fsl <- DF.scores.hist[i,all.list[2]]  
                                scl <- DF.scores.hist[i,length(DF.scores.hist)]
                                score.results <- rbind(score.results,(fsl-scl))
                        }
                        
                        score.results <- cbind(DF.scores[1:nrow(DF.scores.hist)-1,1], score.results)
                        colnames(score.results) <- (c("names", "score.change"))
                        
                        # Sorter eftir stigum
                        score.results <- score.results[order(-score.results$score.change),]
                        
                        # breyta INF í 0
                        samset[!is.finite(samset$ratio),]$ratio <- 0
                        
                        #sorterað eftir W/L ratio
                        samset <- samset[order(-samset$ratio),]
                        
                        output$score.changing <- renderTable(score.results)
                        output$descript <- renderPrint(the.text)
                        output$descript2 <- renderPrint("óháð ári. Heildar samantekt leikja")
                        output$final.score <- renderTable(samset)
                        
                        print("Hér birtist breytingar á skorum yfir þann tíma sem var valinn:")
                        print(score.results)
                        print("")
                        #print(DF.scores.hist)
                        print("hér er samantekt á skorinu hjá öllum. óháð ári:")
                        print(samset)
                
                })
                
       
        
}


shinyApp(ui = ui, server = server)


