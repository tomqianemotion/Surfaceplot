#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
load('surface.RData')
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("surface plot"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    
    column(2,
      sliderInput("rele",
                  "Relevance",
                  min = 0,
                  max = 9,
                  value = 0),
      sliderInput("cong",
                  "Congruence",
                  min = 0,
                  max = 9,
                  value = 0),
      sliderInput("self",
                  "Self-accountability",
                  min = 0,
                  max = 9,
                  value = 5),
      sliderInput("other",
                  "Other-accountability",
                  min = 0,
                  max = 9,
                  value = 5),
      sliderInput("futu",
                  "Future expectancy",
                  min = 0,
                  max = 9,
                  value = 5),
      sliderInput("pfcp",
                  "Problem-focused coping",
                  min = 0,
                  max = 9,
                  value = 5),
      sliderInput("afcp",
                  "Accomodative-focused coping",
                  min = 0,
                  max = 9,
                  value = 5),
      sliderInput("phideg",
                  "Colatitude",
                  min = 0,
                  max = 60,
                  value = 30),
      sliderInput("thetadeg",
                  "Rotation",
                  min = -90,
                  max = 0,
                  value = -45)
    ),
    
    
    # Show a plot of the generated distribution
    column(10,
      plotOutput("surface")
 
    )
  )
)

# Define server logic required to draw a picture
server <- function(input, output) {
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    as.data.frame(input$rele,input$cong,input$self,input$other,input$futu,input$pfcp,input$afcp,input$phideg,input$thetadeg)
  })
  
  output$surface <- renderPlot({
    w=c(1:9)
    rel=NULL
    con=NULL
    sel=NULL
    oth=NULL
    fut=NULL
    pfc=NULL
    afc=NULL
    
    if (input$rele==0){
      rel=w
    }else{
      rel=input$rele
    }
    if (input$cong==0){
      con=w
    }else{
      con=input$cong
    }
    if (input$self==0){
      sel=w
    }else{
      sel=input$self
    }
    if (input$other==0){
      oth=w
    }else{
      oth=input$other
    }
    if (input$futu==0){
      fut=w
    }else{
      fut=input$futu
    }
    if (input$pfcp==0){
      pfc=w
    }else{
      pfc=input$pfcp
    }
    if (input$afcp==0){
      afc=w
    }else{
      afc=input$afcp
    }
    
    ang=angp[rel,con,sel,oth,fut,pfc,afc]
    anx=anxp[rel,con,sel,oth,fut,pfc,afc]
    bor=borp[rel,con,sel,oth,fut,pfc,afc]
    cal=calp[rel,con,sel,oth,fut,pfc,afc]
    cha=chap[rel,con,sel,oth,fut,pfc,afc]
    fea=feap[rel,con,sel,oth,fut,pfc,afc]
    fru=frup[rel,con,sel,oth,fut,pfc,afc]
    gra=grap[rel,con,sel,oth,fut,pfc,afc]
    gui=guip[rel,con,sel,oth,fut,pfc,afc]
    hap=happ[rel,con,sel,oth,fut,pfc,afc]
    hop=hopp[rel,con,sel,oth,fut,pfc,afc]
    int=intp[rel,con,sel,oth,fut,pfc,afc]
    pri=prip[rel,con,sel,oth,fut,pfc,afc]
    reg=regp[rel,con,sel,oth,fut,pfc,afc]
    ###relief uses a differnt name because of conflict with rel(evanec)
    relf=relp[rel,con,sel,oth,fut,pfc,afc]
    res=resp[rel,con,sel,oth,fut,pfc,afc]
    sad=sadp[rel,con,sel,oth,fut,pfc,afc]
    sur=surp[rel,con,sel,oth,fut,pfc,afc]
    
    phid=input$phideg
    thetad=input$thetadeg
    
    par(mfrow=c(3,6),mar=c(1,1,1,1))    
    persp(gui,phi =  phid, theta = thetad,
          xlab = "former", ylab = "latter",zlab="",zlim = c(-2,10),
          main = "guilt",cex=1,cex.lab=1.5,cex.axis=1.5,cex.main=1.8
    )
    persp(reg,phi =  phid, theta = thetad,
          xlab = "former", ylab = "latter",zlab="",zlim = c(-2,10),
          main = "regret",cex=1,cex.lab=1.5,cex.axis=1.5,cex.main=1.8
    )
    
    persp(anx,phi =  phid, theta = thetad,
          xlab = "former", ylab = "latter",zlab="",zlim = c(-2,10),
          main = "anxiety",cex=1,cex.lab=1.5,cex.axis=1.5,cex.main=1.8
    )
    persp(ang,phi =  phid, theta = thetad,
          xlab = "former", ylab = "latter",zlab="",zlim = c(-2,10),
          main = "anger",cex=1,cex.lab=1.5,cex.axis=1.5,cex.main=1.8
    )
    persp(bor,phi =  phid, theta = thetad,
          xlab = "former", ylab = "latter",zlab="",zlim = c(-2,10),
          main = "bore",cex=1,cex.lab=1.5,cex.axis=1.5,cex.main=1.8
    )
    persp(fea,phi =  phid, theta = thetad,
          xlab = "former", ylab = "latter",zlab="",zlim = c(-2,10),
          main = "fear",cex=1,cex.lab=1.5,cex.axis=1.5,cex.main=1.8
    )
    persp(fru,phi =  phid, theta = thetad,
          xlab = "former", ylab = "latter",zlab="",zlim = c(-2,10),
          main = "frustration",cex=1,cex.lab=1.5,cex.axis=1.5,cex.main=1.8
    )
    persp(res,phi =  phid, theta = thetad,
          xlab = "former", ylab = "latter",zlab="",zlim = c(-2,10),
          main = "resignation",cex=1,cex.lab=1.5,cex.axis=1.5,cex.main=1.8
    )
    persp(sad,phi =  phid, theta = thetad,
          xlab = "former", ylab = "latter",zlab="",zlim = c(-2,10),
          main = "sad",cex=1,cex.lab=1.5,cex.axis=1.5,cex.main=1.8
    )
    persp(cal,phi =  phid, theta = thetad,
          xlab = "former", ylab = "latter",zlab="",zlim = c(-2,10),
          main = "calm",cex=1,cex.lab=1.5,cex.axis=1.5,cex.main=1.8
    )
    
    persp(cha,phi =  phid, theta = thetad,
          xlab = "former", ylab = "latter",zlab="",zlim = c(-2,10),
          main = "challenge",cex=1,cex.lab=1.5,cex.axis=1.5,cex.main=1.8
    )
    persp(gra,phi =  phid, theta = thetad,
          xlab = "former", ylab = "latter",zlab="",zlim = c(-2,10),
          main = "gratitude",cex=1,cex.lab=1.5,cex.axis=1.5,cex.main=1.8
    )
    persp(hap,phi =  phid, theta = thetad,
          xlab = "former", ylab = "latter",zlab="",zlim = c(-2,10),
          main = "happy",cex=1,cex.lab=1.5,cex.axis=1.5,cex.main=1.8
    )
    persp(hop,phi =  phid, theta = thetad,
          xlab = "former", ylab = "latter",zlab="",zlim = c(-2,10),
          main = "hope",cex=1,cex.lab=1.5,cex.axis=1.5,cex.main=1.8
    )
    persp(int,phi =  phid, theta = thetad,
          xlab = "former", ylab = "latter",zlab="",zlim = c(-2,10),
          main = "interest",cex=1,cex.lab=1.5,cex.axis=1.5,cex.main=1.8
    )
    persp(pri,phi =  phid, theta = thetad,
          xlab = "former", ylab = "latter",zlab="",zlim = c(-2,10),
          main = "pride",cex=1,cex.lab=1.5,cex.axis=1.5,cex.main=1.8
    )
    persp(relf,phi =  phid, theta = thetad,
          xlab = "former", ylab = "latter",zlab="",zlim = c(-2,10),
          main = "relief",cex=1,cex.lab=1.5,cex.axis=1.5,cex.main=1.8
    )
    persp(sur,phi =  phid, theta = thetad,
          xlab = "former", ylab = "latter",zlab="",zlim = c(-2,10),
          main = "surprise",cex=1,cex.lab=1.5,cex.axis=1.5,cex.main=1.8
    )
    
    
    
  },  height=800,width = 1200)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

