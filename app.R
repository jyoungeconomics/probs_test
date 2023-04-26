require("shiny");require("tidyverse");
require("ggplot2");require("gsheet");
require("fredr");require("dplyr")

# source("C:/Users/jyoung6/Desktop/Consulting/AEI/2023 Projects/Pricing Distributions/apps/probs/helpers1.R")

for (mooky in 1:1){
  
  # https://towardsdatascience.com/implied-volatility-in-r-assess-options-risk-correctly-70fe36843474
  gBSM <- function(S, X, sigma, r, q, ttm, type){
    #S = stock price
    #X = strike price
    #sigma = volatility
    #r = risk free interest rate
    #q = dividend yield
    #ttm = time to maturity in days
    #type = option type
    
    b <- r - q
    t <- ttm/365.25
    
    d1 <- (log(S / X) + (b + sigma ^ 2 / 2) * t) / (sigma * sqrt(t))
    d2 <- d1 - sigma * sqrt(t)
    
    if(type == "call"){
      price <- S * exp((b - r) * t) * pnorm(d1) - X * exp(-r * t) * pnorm(d2)
    }else if (type == "put"){
      price <-  (X * exp(-r * t) * pnorm(-d2) - S * exp((b - r) * t) * pnorm(-d1))
    }
    
    return(price)
  }
  
  volOptimFun <- function(sigma, price, S, K, r, q, ttm, type){
    crossprod(price - gBSM(S, K, sigma, r, q, ttm, type))
  }
  
  roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
    if(length(x) != 1) stop("'x' must be of length 1")
    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
  }
  
  goobus <- function(mydata,z,details=F){
    z <- ifelse(z>100,z/100,z)
    temp <- plnorm(q = z,meanlog = mydata$MOO,sdlog = mydata$SIG)
    if(details==F){
      return(cbind.data.frame("date"=mydata$Date[length(temp)],
                              "ppp"=temp[length(temp)]))
    }else(return(cbind.data.frame("date"=mydata$Date,
                                  "ppp"=temp)))
  }
  
  #
  #
  #

  #
  #
  #
  #
  
  fong <- "soybeans"
  if(fong=="corn"){doodoo <- "December '22 Corn"}else{doodoo <- "November '22 Soybeans"}
  if(fong=="corn"){
    URL. <- 'https://docs.google.com/spreadsheets/d/1XjBUc0D79pmzaEUEh2qU9QOYXZuwSzOjiRjIkAjxQII/edit?usp=sharing'}else{
      URL. <- 'https://docs.google.com/spreadsheets/d/1Nm2g6_k3tjy0O9SBOgK5OrgymUN4shZADyh_MWaQtkk/edit?usp=sharing'}
  DDD1 <- gsheet2tbl(url = URL.)
  DDD1$Date <- as.Date(DDD1$Date, "%m-%d-%Y")
  DDD1$Expiration <- as.Date(DDD1$Expiration, "%m-%d-%Y")
  ###################################
  #get the 1-year T-bill yield as the riskless rate
  fredr_set_key("df934772ec9a9d2bc1b4b10bd2c3fbd2")
  d <- fredr(
    series_id = "DGS1",
    observation_start = min(DDD1$Date),
    observation_end = max(DDD1$Date)
  )
  #risk-free interest rate
  DDD1$RRR <- 0.01*as.numeric(as.character(d[d$date%in%DDD1$Date,]$value));rm(d)
  #time to expiration as a percent of the year
  DDD1$t <- as.numeric(DDD1$Expiration-DDD1$Date)
  #placeholder column for implied volatility
  DDD1$IV <- NA -> DDD1$SIG -> DDD1$MOO
  
  ####################
  #
  # lapply(DDD1$`Spot Price`,function(x){-1*x}) ##replace loop one day in the future
  #
  ####################
  
  for(i in 1:nrow(DDD1)){
    IVC1 <- optimize(volOptimFun, interval = c(0, 1), 
                     price = DDD1$`Call Below`[i], 
                     S = DDD1$`Spot Price`[i], 
                     K = DDD1$`Strike Below`[i], 
                     r = DDD1$RRR[i],q = 0,
                     ttm = DDD1$t[i]+1, type = "call")$minimum
    IVC2 <- optimize(volOptimFun, interval = c(0, 1), 
                     price = DDD1$`Call Above`[i], 
                     S = DDD1$`Spot Price`[i], 
                     K = DDD1$`Strike Above`[i], 
                     r = DDD1$RRR[i],q = 0,
                     ttm = DDD1$t[i]+1, type = "call")$minimum
    IVP1 <- optimize(volOptimFun, interval = c(0, 1), 
                     price = DDD1$`Put Below`[i], 
                     S = DDD1$`Spot Price`[i], 
                     K = DDD1$`Strike Below`[i], 
                     r = DDD1$RRR[i],q = 0,
                     ttm = DDD1$t[i]+1, type = "put")$minimum
    IVP2 <- optimize(volOptimFun, interval = c(0, 1), 
                     price = DDD1$`Put Above`[i], 
                     S = DDD1$`Spot Price`[i], 
                     K = DDD1$`Strike Above`[i], 
                     r = DDD1$RRR[i],q = 0,
                     ttm = DDD1$t[i]+1, type = "put")$minimum
    W1 <- (abs(DDD1$`Strike Below`[i]-DDD1$`Spot Price`[i]))/(abs(DDD1$`Strike Below`[i]-DDD1$`Spot Price`[i])+abs(DDD1$`Strike Above`[i]-DDD1$`Spot Price`[i]))
    W2 <- (abs(DDD1$`Strike Above`[i]-DDD1$`Spot Price`[i]))/(abs(DDD1$`Strike Below`[i]-DDD1$`Spot Price`[i])+abs(DDD1$`Strike Above`[i]-DDD1$`Spot Price`[i]))
    IVC <- W1*IVC1+W2*IVC2
    IVP <- W1*IVP1+W2*IVP2
    DDD1$IV[i] <- (IVC+IVP)/2
    sigma <- (DDD1$IV[i]*sqrt(DDD1$t[i]/365.25)*DDD1$`Spot Price`[i]/100) #calculate the standard deviation
    mu <- DDD1$`Spot Price`[i]/100 #convert units from cents to dollars, store spot price in a list
    #https://stats.stackexchange.com/questions/572987/convert-from-log-normal-distribution-to-normal-distribution
    DDD1$SIG[i] <- sqrt(log(sigma/(mu^2)+1)) #scale parameter of lognormal
    DDD1$MOO[i] <- log(mu)-(DDD1$SIG[i]^2)/2 #location parameter of lognormal
  }
  
  #
  #
  #
  #
  #
  #
  
  fong <- "corn"
  if(fong=="corn"){doodoo <- "December '22 Corn"}else{doodoo <- "November '22 Soybeans"}
  if(fong=="corn"){
    URL. <- 'https://docs.google.com/spreadsheets/d/1XjBUc0D79pmzaEUEh2qU9QOYXZuwSzOjiRjIkAjxQII/edit?usp=sharing'}else{
      URL. <- 'https://docs.google.com/spreadsheets/d/1Nm2g6_k3tjy0O9SBOgK5OrgymUN4shZADyh_MWaQtkk/edit?usp=sharing'}
  DDD2 <- gsheet2tbl(url = URL.)
  DDD2$Date <- as.Date(DDD2$Date, "%m-%d-%Y")
  DDD2$Expiration <- as.Date(DDD2$Expiration, "%m-%d-%Y")
  ###################################
  #get the 1-year T-bill yield as the riskless rate
  fredr_set_key("df934772ec9a9d2bc1b4b10bd2c3fbd2")
  d <- fredr(
    series_id = "DGS1",
    observation_start = min(DDD2$Date),
    observation_end = max(DDD2$Date)
  )
  #risk-free interest rate
  DDD2$RRR <- 0.01*as.numeric(as.character(d[d$date%in%DDD2$Date,]$value));rm(d)
  #time to expiration as a percent of the year
  DDD2$t <- as.numeric(DDD2$Expiration-DDD2$Date)
  #placeholder column for implied volatility
  DDD2$IV <- NA -> DDD2$SIG -> DDD2$MOO
  
  ####################
  #
  # lapply(DDD2$`Spot Price`,function(x){-1*x}) ##replace loop one day in the future
  #
  ####################
  
  for(i in 1:nrow(DDD2)){
    IVC1 <- optimize(volOptimFun, interval = c(0, 1), 
                     price = DDD2$`Call Below`[i], 
                     S = DDD2$`Spot Price`[i], 
                     K = DDD2$`Strike Below`[i], 
                     r = DDD2$RRR[i],q = 0,
                     ttm = DDD2$t[i]+1, type = "call")$minimum
    IVC2 <- optimize(volOptimFun, interval = c(0, 1), 
                     price = DDD2$`Call Above`[i], 
                     S = DDD2$`Spot Price`[i], 
                     K = DDD2$`Strike Above`[i], 
                     r = DDD2$RRR[i],q = 0,
                     ttm = DDD2$t[i]+1, type = "call")$minimum
    IVP1 <- optimize(volOptimFun, interval = c(0, 1), 
                     price = DDD2$`Put Below`[i], 
                     S = DDD2$`Spot Price`[i], 
                     K = DDD2$`Strike Below`[i], 
                     r = DDD2$RRR[i],q = 0,
                     ttm = DDD2$t[i]+1, type = "put")$minimum
    IVP2 <- optimize(volOptimFun, interval = c(0, 1), 
                     price = DDD2$`Put Above`[i], 
                     S = DDD2$`Spot Price`[i], 
                     K = DDD2$`Strike Above`[i], 
                     r = DDD2$RRR[i],q = 0,
                     ttm = DDD2$t[i]+1, type = "put")$minimum
    W1 <- (abs(DDD2$`Strike Below`[i]-DDD2$`Spot Price`[i]))/(abs(DDD2$`Strike Below`[i]-DDD2$`Spot Price`[i])+abs(DDD2$`Strike Above`[i]-DDD2$`Spot Price`[i]))
    W2 <- (abs(DDD2$`Strike Above`[i]-DDD2$`Spot Price`[i]))/(abs(DDD2$`Strike Below`[i]-DDD2$`Spot Price`[i])+abs(DDD2$`Strike Above`[i]-DDD2$`Spot Price`[i]))
    IVC <- W1*IVC1+W2*IVC2
    IVP <- W1*IVP1+W2*IVP2
    DDD2$IV[i] <- (IVC+IVP)/2
    sigma <- (DDD2$IV[i]*sqrt(DDD2$t[i]/365.25)*DDD2$`Spot Price`[i]/100) #calculate the standard deviation
    mu <- DDD2$`Spot Price`[i]/100 #convert units from cents to dollars, store spot price in a list
    #https://stats.stackexchange.com/questions/572987/convert-from-log-normal-distribution-to-normal-distribution
    DDD2$SIG[i] <- sqrt(log(sigma/(mu^2)+1)) #scale parameter of lognormal
    DDD2$MOO[i] <- log(mu)-(DDD2$SIG[i]^2)/2 #location parameter of lognormal
  }
  
  #
  #
  #
  #
  #
  
  # contract.select.c <- data.frame("Crop"=c("Corn","Whatevs"),
  #                                 "Contract"=c("Nah","December 2022"))
  # contract.select.s <- data.frame("Crop"=c("NOOO","Soybeans"),
  #                                 "Contract"=c("November 2022","Meh"))
  
  # contracts <- data.frame("Crop1"=c("Corn","Corn","Soybeans","Soybeans"),
  #                         "Contract1"=c("Nah","December 2022","November 2022","Meh"))
  
  DDD1$Crop1 <- "Soybeans";DDD1$Contract1 <- "November 2022"
  DDD2$Crop1 <- "Corn";DDD2$Contract1 <- "December 2022"
  DDD <- rbind.data.frame(DDD2,DDD1)
  
  rm(list = ls()[!ls() %in% c("DDD","goobus","roundUpNice")])
  
  # rm(list = ls()[!ls() %in% c("DDD1","DDD2","goobus","roundUpNice",
  #                             "contracts")])
}

# Define UI for application that takes two vars and returns text
ui <- fluidPage(
  # Sidebar with two var filters
  sidebarLayout(
    sidebarPanel(
      img(src = "http://aei.ag/wp-content/uploads/2023/04/AEI-Premium-Watermark.png",
          height = 175, width = 350), #have to first run app externally for image to show
      selectInput(inputId = "crop123",
                  label = "Select Crop:",
                  choices = unique(DDD$Crop1)
                  ),
      width = 3,
      uiOutput("STRIKE"),
      uiOutput("contract123")
    ),
    # Print text
    mainPanel(
      br(),
      br(),
      plotOutput("line",width = "110%",height = 750)
    )
  )
)

# Server logic: subset DDD based on input, print output
server <- function(input, output, session) {
  
  output$contract123 <- renderUI({
    cropz <- DDD %>% 
      filter(Crop1 %in% input$crop123) %>% 
      pull(Contract1) %>% 
      unique()
    selectInput("contract123", "Select Futures Contract", cropz)
  })
  
  output$STRIKE <- renderUI({
    cropz <- DDD %>% 
      filter(Crop1 %in% input$crop123) %>% 
      pull(`Strike Above`) %>% 
      unique()
    numericInput(inputId = "STRIKE",
                 label = "Enter a Price of Interest",
                 value = 0.01*cropz[1],min = 0,max = 100)
  })
    
  output$line <- renderPlot({
    ggplot(goobus(mydata = DDD %>% filter(Crop1 == input$crop123,
                                          Contract1 == input$contract123),
                  z = input$STRIKE,details = T),aes(x=`date`, y=`ppp`))+
      theme_minimal()+
      annotation_custom(rasterGrob(image_read("http://aei.ag/wp-content/uploads/2023/04/AEI-Premium-Watermark.png"),
                                   interpolate = T), 
                        xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
      theme(panel.grid.minor.x=element_blank(),
            panel.grid.major.x=element_blank(),
            panel.grid.minor.y = element_line(size = 1.1,color="grey80"),
            panel.grid.major.y = element_line(size = 1.1,color="grey80"))+
      ggtitle(label = paste0("Probability of ",input$contract123," ",input$crop123,
                             " less than $",sprintf("%0.2f",round(input$STRIKE,2))," at expiration: \n",
                             "Weekly Estimates from ",min(goobus(mydata = DDD %>% filter(Crop1 == input$crop123, Contract1 == input$contract123), z = input$STRIKE,details = T)$date),
                             " --- ",max(goobus(mydata = DDD %>% filter(Crop1 == input$crop123, Contract1 == input$contract123), z = input$STRIKE,details = T)$date),
                             " \n(Source: BarChart, AEI Calculations)"),
              subtitle = waiver())+
      theme(plot.title = element_text(hjust = 0.5,size = 27.5),
            legend.title = element_text(hjust = 0,size = 17.5),
            legend.text = element_text(hjust = 0.5,size = 17.5))+
      theme(axis.title.y = element_text(size = 20))+
      theme(axis.title.x = element_text(size = 20,color="black"))+
      geom_line(aes(color="forestgreen"),size=1.5) + 
      theme(plot.title = element_text(hjust = 0.5,size = 27.5),
            legend.title = element_text(hjust = 0,size = 17.5),
            legend.text = element_text(hjust = 0.5,size = 13),
            legend.position="none")+
      scale_color_manual(values="forestgreen")+
      geom_point(data=goobus(mydata = DDD %>% filter(Crop1 == input$crop123, Contract1 == input$contract123), z = input$STRIKE,details = T),aes(x=date, y=`ppp`),
                 colour="black", size=5,shape=15)+
      scale_x_continuous(breaks = unique(goobus(mydata = DDD %>% filter(Crop1 == input$crop123, Contract1 == input$contract123), z = input$STRIKE,details = T)$date)[seq(1,length(unique(goobus(mydata = DDD %>% filter(Crop1 == input$crop123, Contract1 == input$contract123), z = input$STRIKE,details = T)$date)),by=5)])+
      scale_y_continuous(breaks = seq(roundUpNice(max(0,min(goobus(mydata = DDD %>% filter(Crop1 == input$crop123, Contract1 == input$contract123), z = input$STRIKE,details = T)$`ppp`,na.rm = T)-0.2)),
                                      roundUpNice(min(1,(max(goobus(mydata = DDD %>% filter(Crop1 == input$crop123, Contract1 == input$contract123), z = input$STRIKE,details = T)$`ppp`,na.rm = T)))),
                                      by=0.1),
                         limits = c(roundUpNice(max(0,min(goobus(mydata = DDD %>% filter(Crop1 == input$crop123, Contract1 == input$contract123), z = input$STRIKE,details = T)$`ppp`,na.rm = T)-0.2)),
                                    roundUpNice(min(1,(max(goobus(mydata = DDD %>% filter(Crop1 == input$crop123, Contract1 == input$contract123), z = input$STRIKE,details = T)$`ppp`,na.rm = T))))))+
      labs(x="Date",y=paste0("Pr[x < $",input$STRIKE,"/bu at expiration]"))+
      theme(axis.text=element_text(size=15),
            axis.title=element_text(size=25,face="bold.italic",color="black"))
  })

}

# Run the application
shinyApp(ui = ui, server = server)