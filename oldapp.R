#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#mo
#  
#

library("shiny")
library("data.table")
library("gtools")
library("htmltools")
library("htmlwidgets")
library("lubridate")
library("RCurl")
library("readxl")
library("RSQLite")
library("shiny")
library("shinyjs")
library("sqliter")
library("stringr")
con = dbConnect(SQLite(), dbname="posdata.sqlite")
items <- as.data.table(dbReadTable(con,"items"))
tb_data <- as.data.table(dbReadTable(con,"tabs"))
dbDisconnect(con)
if(length(tb_data$tabident) > 0){
  initident <- tb_data$tabident[length(tb_data$tabident)]+1
}else{
  initident <- 1
}
<<<<<<< HEAD
pginit <- 1
=======

>>>>>>> 0934efa6d0b226a80bf2fb4330ed36021058dc2c
#print("tb_data")
print(initident)


#print(orderid)
lastcatslistord <- ""
fl_lst <- unique(items$category)
categorylist <- fl_lst[which(!fl_lst== "Sides" & !fl_lst == "Modifier")]
print(categorylist)
counter <- 0
typeslist <- c("retail","food","alcohol","non-tax","create new")
<<<<<<< HEAD

shinyApp(
      shinyUI(
      #  uiOutput("navigate")
        uiOutput("current")
=======
#tabs <- data.table("Tab Name"= character(),"Amount" = numeric(),"Items" = list(),"Item Cost"= list(), "Taxes" = numeric(),"saved card" = character(),"Number in Party" = integer(),"Time Tab Opened" = numeric(), "Open" = logical())
#opentablst <- tabs$`Tab Name`[tabs$Open == TRUE]
#lngthtabs <- length(opentablst)
#tabbuttons <- unlist(lapply(1:lngthtabs,function(i) paste("button",i,sep="")))
#tabchoices <- for(i in 1:lngthtabs){
#  opentablst
#}
# Define UI for application that draws a histogram
shinyApp(
      shinyUI(
        uiOutput("navigate")
      #  uiOutput("current")
>>>>>>> 0934efa6d0b226a80bf2fb4330ed36021058dc2c
      ),

shinyServer(function(input, output, session) {
  output$current <- renderUI({
<<<<<<< HEAD
    uiOutput(stateof$currentpage)
=======
    uiOutput(currentpage$data)
>>>>>>> 0934efa6d0b226a80bf2fb4330ed36021058dc2c
  })
  
  output$navigate <- renderUI({
    fixedPage(
      navbarPage(p("POS"),id="navipage",
                 tabPanel("order", value = "orderpage"),
                 tabPanel("tabs",value = "tabpage"),
                 navbarMenu(
                   "tables",
                   tabPanel("all",uiOutput('tablepage'))),
                 navbarMenu(
                   "functions",
                   tabPanel("add item",uiOutput("additempage")),
                   tabPanel("add category",uiOutput('addcatpage')),
                   tabPanel("initialize",uiOutput('initpage'))         
                 ),
<<<<<<< HEAD
                 selected = NULL#cpagedatanullcheck()
      )
      
      #uiOutput(stateof$currentpage)
      #uiOutput("action_panel"),
      #uiOutput("status_panel")
    )
  })
  
  cpagedatanullcheck <- reactive({
    if(is.null(stateof$currentpage)){ 
      return("orderpage")
      }else{
        return(stateof$currentpage)
      }
  })
  clearacttb <- reactive({
    ordrchc$data <- NULL
    ordrprc$data <- NULL
    acttbcontainer$tabident = NULL
    acttbcontainer$name = NULL
    acttbcontainer$itms = NULL
    acttbcontainer$time = NULL
    acttbcontainer$itms$price = NULL
  })
  output$status_panel <- renderUI({
    absolutePanel(
      wellPanel(
      textOutput("statusprint")
    ),bottom = "2%",height="20%",right="2%",width="25%"
    )
  })
  output$action_panel <- renderUI({
    absolutePanel(
      wellPanel(
        textOutput("actionprint")
      ),bottom = "8%",height="20%",right="2%",width="25%"
    )
  })
  printout <- reactiveValues(status = NULL, action = NULL)
  output$statusprint <- renderText({
    stateof$lastpage
  })
  output$actionprint <- renderText({
    stateof$currentpage
  })
  output$buttons4items <- renderUI({
    radioButtons("catslistord","Category", c(categorylist,""),selected = lastcatslistord)
  })
  output$orderactbuttons <- renderUI({
    tagList(
      actionButton("svcard","Save Card"),
    actionButton("mktogo","TO GO"),
    actionButton("sndord","Send")
    )
  }
  )
  primarychoices <- reactiveValues(panel = "buttons4items")
  lowerpnlbuttons <- reactiveValues(panel = "orderactbuttons")
=======
                 selected = cpagedatanullcheck()
      ),
      uiOutput(currentpage$data)
      
    )
  })
  cpagedatanullcheck <- reactive({
    if(is.null(currentpage$data)){ 
      return("orderpage")
      }else{
        return(currentpage$data)
      }
  })
>>>>>>> 0934efa6d0b226a80bf2fb4330ed36021058dc2c
  output$orderpage <- renderUI({
    
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6,
<<<<<<< HEAD
                 wellPanel( 
                   uiOutput(ordchcpnl$panel)
                  )  
                 ),
          column(6,
                wellPanel(
                  uiOutput(primarychoices$panel)
                 )
                )
        ),
        fluidRow(
          column(6),
          column(6,
               wellPanel(
                 uiOutput(lowerpnlbuttons$panel)
                )
               ) 
        ),
        width = 6
      ),
=======
                 absolutePanel( 
                   uiOutput(ordchcpnl$panel),
                   top="5%",height = "100%",left="5%",width="50%"
                 )  
                 ),
          column(6,
          wellPanel(
         radioButtons("catslistord","Category", c(categorylist,""),selected = lastcatslistord)
        ))),
        fluidRow(
            column(6),
            column(6,
          wellPanel(
          
            actionButton("nmtab","Name Tab")
          ,
         
            actionButton("svcard","Save Card")
          ,
         
            actionButton("mktogo","TO GO")
          ,
          
            actionButton("sndord","Send")
          )) 
        ),width = 6
          ),
>>>>>>> 0934efa6d0b226a80bf2fb4330ed36021058dc2c
      mainPanel(
        fixedRow(
          column(3,"Time : "),
          column(4,textOutput("ordertime"))
        ),
        #fixedRow(column(4,textOutput("orderident"))),
        fluidRow(
          column(6,
                 
                 absolutePanel(
                   wellPanel(
                   if(!is.null(ordrchc$data)){
                     uiOutput("orderpanel")
                  }),
                   top="5%",height = "100%",left="5%",width="100%"
                 )
                 
          ),
          column(2),
          column(4,
                 fluidRow("Tab Name"),
                 fluidRow(
                   h4(textInput("tbname",NULL,value = paste("quick sell ",orderid(),sep=""),placeholder = paste("quick sell ",orderid(),sep="")))
                   
                  # column(4,"Tab Name :"),
                   
                 ),
                 fluidRow("Total"),
                 fluidRow(
                   h2(textOutput("ordertotal")))
          )
          )
       ,width = 6
        
      ),
      position = "right"
    )
  })
<<<<<<< HEAD
  output$testbutton <- renderUI({
    if(!input$catslistord == ""){
      actionButton("nmtab","Name Tab")
      }
  })
=======
  
>>>>>>> 0934efa6d0b226a80bf2fb4330ed36021058dc2c
  output$tabpage <- renderUI({
    
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          uiOutput("mytabs")
<<<<<<< HEAD
        ),
        fluidRow(
          if(pgnum$testlst > 20){actionButton("prv_butt","Prev",width = "50px")}
        ),
        fluidRow(
          if(pgnum$testlst > 20){actionButton("nxt_butt","Next",width = "50px")}
=======
>>>>>>> 0934efa6d0b226a80bf2fb4330ed36021058dc2c
        ),
        width=3
      ),
      uiOutput("showtabpanel")
    )
  })
  output$additempage <- renderUI({
<<<<<<< HEAD
=======
    
>>>>>>> 0934efa6d0b226a80bf2fb4330ed36021058dc2c
    sidebarLayout(
      sidebarPanel("Categories",
                   uiOutput("sdbrpnl")
      ),
      mainPanel(
        fixedRow(
          column(6,actionButton("edititm","Edit"),actionButton("delitm","Del"),actionButton("additm","Add"),actionButton("viewitm","View"),actionButton("rfrshtbls","Refresh"))
        ),
        fixedRow(
          uiOutput("categorypanel")
        ),
        fixedRow(
          uiOutput("itemspanel")
        )
      )
    )
  })
  
  output$addcatpage <- renderUI({
    
    typeslist <- categories_dt$type
    
    sidebarLayout(
      sidebarPanel("Categories",
                   wellPanel(
                     textOutput("categlist")
                     #lapply(1:length(categorylist), function(i) {
                     #radioButtons("catslist","Add to :", categorylist),
                     #}),
                     #actionButton("addcat","Add Category")
                     
                   )
                   # tabsetPanel(
                   # tabPanel(
                   #   )
                   #)
      ),
      mainPanel(
        fluidRow(
          inputPanel(
            textInput("catname","Category Name"),
            selectInput("typedrop","Type",c(typeslist,"add type")),
            actionButton("pressme","submit")
          )
        )
      )
    )
  })
  ordrchc <- reactiveValues(data = NULL)
  ordrprc <- reactiveValues(data = NULL)
  ordrid <- reactiveValues(data = NULL)
  v <- reactiveValues(counter = initident)
  sdbrdata <- reactiveValues(data = NULL)
  pnldata <- reactiveValues(data = NULL)
  currentpage <- reactiveValues(data = NULL)
  delit <- reactiveValues(data = NULL)
  addit <- reactiveValues(data = NULL)
  cancelit <- reactiveValues(data = NULL)
  confirmit <- reactiveValues(data = NULL)
  orderid <- reactive({
    v$counter
    return(v$counter)
  })
  ordchcrfrsh <- reactive({
    ordrchc$data <- NULL
    return(ordrchc$data)
  })
  ordprcrfrsh <- reactive({
    ordrprc$data <- NULL
    return(ordrchc$data)
  })
  ord_time <- reactive({
   odt <- format(Sys.time(), "%X")
    return(odt)
  })
  ord_tbl <- reactive({
    odtbl <- data.table(name = ordrchc$data, price = ordrprc$data)
    return(odtbl)
  })
  ord_ttl <- reactive({
<<<<<<< HEAD
    odttl <-sum(as.numeric(ordrprc$data))
=======
    odttl <-sum(as.integer(ordrprc$data))
>>>>>>> 0934efa6d0b226a80bf2fb4330ed36021058dc2c
    return(odttl)
  })
  tbs_dt <- reactive({
    con = dbConnect(SQLite(), dbname="posdata.sqlite")
    tbs <- as.data.table(dbReadTable(con,"tabs"))
    dbDisconnect(con)
    tbsrtn <- tbs[which(tbs$name == input$panelotab)]
    return(tbsrtn)
  })
<<<<<<< HEAD
  acttbcontainer <- reactiveValues(name = NULL , time = NULL, total = NULL, open = NULL, tabident = NULL, itms = NULL)
  tbnm <- reactiveValues(name = NULL)
  makeacttb <- reactive({
    con = dbConnect(SQLite(), dbname="posdata.sqlite")
    tbs <- as.data.table(dbReadTable(con,"tabs"))
    acttbcontainer$name = tbnm$name
    print(acttbcontainer$name)
    acttbcontainer$time = tbs$time[which(tbs$name == input$panelotab)]
    print(acttbcontainer$time)
    acttbcontainer$total = as.numeric(tbs$total[which(tbs$name == input$panelotab)])
    print(acttbcontainer$total)
    acttbcontainer$open = as.logical(tbs$open[which(tbs$name == input$panelotab)])
    print(acttbcontainer$open)
    acttbcontainer$tabident = as.integer(tbs$tabident[which(tbs$name == input$panelotab)])
    print(acttbcontainer$tabident)
    dbDisconnect(con)
    acttbcontainer$itms = getitms()
    return(acttbcontainer)
  })
  observeEvent(input$panelotab,{
    if(!input$panelotab == "select tab"){
      tbnm$name <- input$panelotab
    makeacttb()
    #
    }
  })
  getitms <- reactive({
    con = dbConnect(SQLite(), dbname="posdata.sqlite")
    gt_itms <- as.data.table(dbReadTable(con,acttbcontainer$name))
=======
  getitms <- reactive({
    con = dbConnect(SQLite(), dbname="posdata.sqlite")
    gt_itms <- as.data.table(dbReadTable(con,input$panelotab))
>>>>>>> 0934efa6d0b226a80bf2fb4330ed36021058dc2c
    dbDisconnect(con)
    return(gt_itms)
  })
  makedroplist <- reactive({
    con = dbConnect(SQLite(), dbname="posdata.sqlite")
    items <- as.data.table(dbReadTable(con,"items"))
    dbDisconnect(con)
    rtn <- items$name[which(items$category == input$catslist)]
    print(rtn)
    return(rtn)
    
  })
  output$nmfrmpanel <- renderText(
    input$panelotab
  )
  output$stimye <- renderText({
    timesish <- tbs_dt()
    print(timesish$time)
  })
  output$ordertotal <- renderText({
    ord_ttl()
  })
  output$ordertime <- renderText({
   ord_time()
  })
  output$orderident <- renderText({
    ordval <- orderid()
    ordval
  })
  output$tabtotal <- renderText({
    timesish <- tbs_dt()
<<<<<<< HEAD
    as.numeric(timesish$total)
=======
    timesish$total
>>>>>>> 0934efa6d0b226a80bf2fb4330ed36021058dc2c
  })
  
  output$ordertbl <- renderTable({
    ord_tbl()
  })
  output$orderoutput <- renderTable(
    as.data.table(ordrchc$data)
  )
  output$tabitems <- renderTable({
    getitms()
  })
  observeEvent(input$navipage,{
<<<<<<< HEAD
    stateof$lastpage <<- stateof$currentpage
    stateof$currentpage <<- input$navipage
    print(stateof$currentpage)
    return(stateof$currentpage)
=======
    currentpage$data <<- input$navipage
    print(currentpage$data)
    return(currentpage$data)
>>>>>>> 0934efa6d0b226a80bf2fb4330ed36021058dc2c
  })
  observeEvent(input$sndord,{
    con = dbConnect(SQLite(), dbname="posdata.sqlite")
    
    nameident <- input$tbname
    qry0 <- paste("INSERT INTO tabs (name, time, total, open, tabident) VALUES ('",nameident,"','",ord_time(),"','",as.numeric(ord_ttl()),"','",1,"','",as.integer(orderid()),"');",sep = "")
    rs0 <- dbSendQuery(con,qry0)
    dbHasCompleted(rs0)
    #qry1 <- paste("INSERT INTO master_itms (name, price, tabident) VALUES ('",input$tbname,"','",ord_time,"','",ord_ttl,"','",1,"','",orderid,"');",sep = "")
    dbWriteTable(con, nameident, ord_tbl())
    ordrchc$data <- NULL
    ordrprc$data <- NULL
    tabslst$open <- NULL
    v$counter  <<- v$counter +1
    dbDisconnect(con)
  })
  observeEvent(input$updord,{
    con = dbConnect(SQLite(), dbname="posdata.sqlite")
    
<<<<<<< HEAD
    nameident <- acttbcontainer$name
    updordtbl <- ord_tbl()
    tmp_ttl <- sum(c(as.numeric(acttbcontainer$itms$price),as.numeric(ordrprc$data)))
=======
    nameident <- addtoordervars$name
    updordtbl <- ord_tbl()
    tmp_ttl <- sum(c(as.numeric(addtoordervars$itms$price),as.numeric(ordrprc$data)))
>>>>>>> 0934efa6d0b226a80bf2fb4330ed36021058dc2c
    print(tmp_ttl)
    #qry1 <- paste("INSERT INTO `",nameident,"`(`name`,`price`) VALUES ('",updordtbl$name,"','",updordtbl$price,"');",sep = "")
    #rs1 <- dbSendQuery(con,qry1)
    #dbHasCompleted(rs1)
   
    
    dbWriteTable(con, nameident, ord_tbl(), append = TRUE)
    dbDisconnect(con)
    con = dbConnect(SQLite(), dbname="posdata.sqlite")
<<<<<<< HEAD
    qry2 <- paste("UPDATE `tabs` SET `total`= '",as.numeric(tmp_ttl),"' WHERE `tabident`='",as.integer(acttbcontainer$tabident),"';",sep = "")
    print(qry2)
    rs2 <- dbSendQuery(con,qry2)
    dbHasCompleted(rs2)
    #ordrchc$data <- NULL
    #ordrprc$data <- NULL
    #acttbcontainer$tabident = NULL
    #acttbcontainer$name = NULL
    #acttbcontainer$itms = NULL
    #acttbcontainer$time = NULL
    #acttbcontainer$itms$price = NULL
    clearacttb()
    tabslst$open <- NULL
    stateof$lastpage <<- stateof$currentpage
    stateof$currentpage <- "orderpage"
    dbDisconnect(con)
  })
  
=======
    qry2 <- paste("UPDATE `tabs` SET `total`= '",as.numeric(tmp_ttl),"' WHERE `tabident`='",as.integer(addtoordervars$tabid),"';",sep = "")
    print(qry2)
    rs2 <- dbSendQuery(con,qry2)
    dbHasCompleted(rs2)
    ordrchc$data <- NULL
    ordrprc$data <- NULL
    addtoordervars$tabid = NULL
    addtoordervars$name = NULL
    addtoordervars$itms = NULL
    addtoordervars$time = NULL
    addtoordervars$prcs = NULL
    tabslst$open <- NULL
    currentpage$data <- "orderpage"
    dbDisconnect(con)
  })
>>>>>>> 0934efa6d0b226a80bf2fb4330ed36021058dc2c
  observeEvent(input$sidebox, {    
    if(!input$sidebox == ""){
      
        
        ordrchc$data <- c(ordrchc$data,input$sidebox)
        #ord_dt <- data.table(name = items$name[which(items$name == ordrchc$data)],price = items$price[which(items$name == ordrchc$data)])
      
      return(ordrchc$data)}
  })
  observeEvent(input$sidebox, {    
    if(!input$sidebox == ""){
      
<<<<<<< HEAD
        sditminfo <- items[which(items$name == input$sidebox),]
        print(sditminfo)
        priceinfo <- as.numeric(sditminfo$price)
        ordrprc$data <- c(as.numeric(ordrprc$data),priceinfo)
=======
        itminfo <- items[which(items$name == input$sidebox),]
        print(itminfo)
        priceinfo <- itminfo$price
        ordrprc$data <- c(ordrprc$data,priceinfo)
>>>>>>> 0934efa6d0b226a80bf2fb4330ed36021058dc2c
        ordchcpnl$panel <<- "choicespanel"
        ordchcpnl$data <<- NULL
        #ord_dt <- data.table(name = items$name[which(items$name == ordrchc$data)],price = items$price[which(items$name == ordrchc$data)])
      print(ordchcpnl$panel)
      return(ordrprc$data)}
  })
  observeEvent(input$idbox, {    
    if(!input$idbox == ""){
      if(is.null(ordrchc$data)){
        ordrchc$data <- input$idbox
        #ord_dt <- data.table(name = items$name[which(items$name == ordrchc$data)],price = items$price[which(items$name == ordrchc$data)])
      }else{
        
        ordrchc$data <- c(ordrchc$data,input$idbox)
        #ord_dt <- data.table(name = items$name[which(items$name == ordrchc$data)],price = items$price[which(items$name == ordrchc$data)])
      }
      return(ordrchc$data)}
  })
  observeEvent(input$idbox, {    
    if(!input$idbox == ""){
      if(is.null(ordrprc$data)){
        itminfo <- items[which(items$name == input$idbox),]
        print(itminfo)
<<<<<<< HEAD
        print(itminfo$sides)
=======
>>>>>>> 0934efa6d0b226a80bf2fb4330ed36021058dc2c
        if(!itminfo$sides=="none"){
          ordchcpnl$panel <<- "sidespanel"
          ordchcpnl$data <<- itminfo
        }
        priceinfo <- itminfo$price
        
        ordrprc$data <- priceinfo
        #ord_dt <- data.table(name = items$name[which(items$name == ordrchc$data)],price = items$price[which(items$name == ordrchc$data)])
      }else{
        itminfo <- items[which(items$name == input$idbox),]
        print(itminfo)
<<<<<<< HEAD
        print(itminfo$sides)
=======
>>>>>>> 0934efa6d0b226a80bf2fb4330ed36021058dc2c
        
        if(!itminfo$sides=="none"){
          ordchcpnl$panel <<- "sidespanel"
          ordchcpnl$data <<- itminfo
        }
        priceinfo <- itminfo$price
        ordrprc$data <- c(ordrprc$data,priceinfo)
        #ordchcpnl$panel <<- "choicespanel"
        #ordchcpnl$data <<- NULL
        #ord_dt <- data.table(name = items$name[which(items$name == ordrchc$data)],price = items$price[which(items$name == ordrchc$data)])
      }
      print(ordchcpnl$panel)
      return(ordrprc$data)}
  })
  observeEvent(input$edititm, {    
    pnldata$data <- "edititmpanel"
    print("pnldata$data")
    print(pnldata$data)
    
  })
  observeEvent(input$additm, {    
    pnldata$data <- "additmpanel"
    
  })
  observeEvent(input$delitm, {    
    pnldata$data <- "delitmpanel"
    
  })
  observeEvent(input$viewitm, {    
    pnldata$data <- "defaultitmpanel"
    
  })
  output$orderpanel <- renderUI({
    
      tableOutput("ordertbl")
    
  })
 ordchcpnl <- reactiveValues(panel = "choicespanel",data = NULL)
 
  output$choicespanel <- renderUI({
    if(!input$catslistord == ""){
      con = dbConnect(SQLite(), dbname="posdata.sqlite")
    items <- as.data.table(dbReadTable(con,"items"))
    dbDisconnect(con)
    catchoice <- input$catslistord
    itmslist <- items$name[which(items$category == catchoice & items$active == 1)]
    itmsidents <- items$itmident[which(items$category == catchoice & items$active == 1)]
    radioButtons("idbox",catchoice,c(itmslist,""),selected= "",inline=FALSE)}
  })
  
  output$sidespanel <- renderUI({
    tblname <- paste(ordchcpnl$data$sides,"_side",sep="")
    print(tblname)
    con = dbConnect(SQLite(), dbname="posdata.sqlite")
    sides_tmp <<- as.data.table(dbReadTable(con,tblname))
    dbDisconnect(con)
    sideitmslist <- sides_tmp$name[which(sides_tmp$active == 1)]
    #itmsidents <- items$itmident[which(items$category == catchoice & items$active == 1)]
    h4(radioButtons("sidebox","Sides",c(sideitmslist,""),selected= "",inline=FALSE))
  })
  
  output$modifiespanel <- renderUI({
    con = dbConnect(SQLite(), dbname="posdata.sqlite")
    items <- as.data.table(dbReadTable(con,"items"))
    dbDisconnect(con)
    catchoice <- input$catslistord
    itmslist <- items$name[which(items$category == catchoice & items$active == 1)]
    itmsidents <- items$itmident[which(items$category == catchoice & items$active == 1)]
    h4(radioButtons("idbox",catchoice,c(itmslist,""),selected= "",inline=TRUE))
  })
  
  addtoordervars <- reactiveValues(tabid = NULL,name = NULL,itms = NULL,time = NULL,prcs = NULL)
  observeEvent(input$addtotab,{
<<<<<<< HEAD
    #tmptbs <- tbs_dt()
    #gt_itms_dt <- getitms()
    #addtoordervars$tabid <- tmptbs$tabident
    #addtoordervars$name <- input$panelotab 
    #addtoordervars$itms <- gt_itms_dt
    #print(str(addtoordervars$itms$price))
    #addtoordervars$prcs <- gt_itms_dt$price
    stateof$lastpage <<- stateof$currentpage
    stateof$currentpage <- "addtotabpage"
  })
  stateof <- reactiveValues(paymentlist = NULL,currentpage = "orderpage",lastpage = NULL)
  observeEvent(input$closetab,{
    printout$status <- paste(print("this"))
    pymntpnl$panel <- NULL
    clssdbar$panel <- "pymntslistpanel"
    stateof$paymentlist <- NULL
    #addtoordervars$prcs <- gt_itms_dt$price
    stateof$lastpage <<- stateof$currentpage
    stateof$currentpage <- "closetabpage"
  })
  
  output$currtabident <- renderText({
    acttbcontainer$tabident
  })
  output$currtabname <- renderText({
    acttbcontainer$name
  })
  output$currtabtbl <- renderTable({
    acttbcontainer$itms
=======
    tmptbs <- tbs_dt()
    gt_itms_dt <- getitms()
    addtoordervars$tabid <- tmptbs$tabident
    addtoordervars$name <- input$panelotab 
    addtoordervars$itms <- gt_itms_dt
    print(str(addtoordervars$itms$price))
    #addtoordervars$prcs <- gt_itms_dt$price
    currentpage$data <- "addtotabpage"
  })
  output$currtabident <- renderText({
    addtoordervars$tabid
  })
  output$currtabname <- renderText({
    addtoordervars$name
  })
  output$currtabtbl <- renderTable({
    addtoordervars$itms
>>>>>>> 0934efa6d0b226a80bf2fb4330ed36021058dc2c
  })
  output$addtotabpage <- renderUI({
    print(ordchcpnl$panel)
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6,
                 absolutePanel( 
                   uiOutput(ordchcpnl$panel),
                   top="5%",height = "100%",left="5%",width="50%"
                 )  
          ),
          column(6,
                 wellPanel(
                   radioButtons("catslistord","Category", c(categorylist,""),selected = lastcatslistord)
                 ))),fluidRow(
                   column(6),
                   column(6,
                          wellPanel(
                            
                            actionButton("updord","Send")
                            
                          )) 
                 ),
                fluidRow(),
                fluidRow(),width = 6
      ),
      
      mainPanel(
        fixedRow(
          column(3,"Time : "),
          column(4,textOutput("ordertime"))
        ),
        #fixedRow(column(4,textOutput("orderident"))),
        fluidRow(
          column(7,
                 
                 absolutePanel(
                   wellPanel(
                     tableOutput("currtabtbl"),
                     if(!is.null(ordrchc$data)){
                       uiOutput("orderpanel")
                     }),
                   top="5%",height = "100%",left="5%",width="100%"
                 )
                 
          ),
          column(1),
          column(4,
                 fluidRow("Tab Name"),
                 fluidRow(
                   h4(textOutput("currtabname"))
                   
                   # column(4,"Tab Name :"),
                   
                 ),
                 fluidRow("Total"),
                 fluidRow(
<<<<<<< HEAD
                   h2(paste("$",sum(c(as.integer(acttbcontainer$itms$price),as.integer(ordrprc$data))),sep=""))),
=======
                   h2(paste("$",sum(c(as.integer(addtoordervars$itms$price),as.integer(ordrprc$data))),sep=""))),
>>>>>>> 0934efa6d0b226a80bf2fb4330ed36021058dc2c
                 fluidRow("Tab ID"),
                 fluidRow(
                   h4(textOutput("currtabident"))
                   
                   # column(4,"Tab Name :"),
                   
                 )
          )
          ),
        width = 6
        
        
      ),
      position = "right"
    )
  })
<<<<<<< HEAD
  observeEvent(input$paymentlist,{
    if(!input$paymentlist == ""){
      pymntpnl$panel <- "amountpnl"
    }
    stateof$paymentlist <- input$paymentlist
    print(pymntpnl$panel)
    #return(ordchcpnl$panel)
  })
  observeEvent(input$payout,{
    acttbcontainer
  })
  output$amountpnl <- renderUI({
    exactamt <- getclosetotal()
    h2(numericInput("cashamt",NULL,exactamt,min=.01,max=exactamt+1000,step = .01))
    #radioButtons("cashlist",NULL,c(exactamt,""),selected= exactamt,inline=FALSE)
  })
  
  output$pymntslistpanel <- renderUI({
    
    radioButtons("paymentlist",NULL,c("Cash","MasterCard","Visa","Discover","AmEx","Gift Card"),selected=checkifpymntnull() ,inline=FALSE)
  })
 
  checkifpymntnull <- reactive({
    if(is.null(stateof$paymentlist)){ 
      return("")
    }else{
      return(stateof$paymentlist)
    }
  })
  
  output$catslistpanel <- renderUI({
    
    #radioButtons("catslistord","Category", c(categorylist,""),selected = lastcatslistord)
  })
  sdbar <- reactiveValues(panel = "catslistpanel")
  clssdbar <- reactiveValues(panel = "pymntslistpanel")
  pymntpnl <- reactiveValues(panel = NULL)
  output$closetabpage <- renderUI({
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6,
                 absolutePanel( 
                   uiOutput(pymntpnl$panel),
                   top="5%",height = "100%",left="5%",width="50%"
                 )  
          ),
          column(6,
                 wellPanel(
                   uiOutput(clssdbar$panel)
                   
                 ))),fluidRow(
                   column(6),
                   column(6,
                          wellPanel(
                            
                            actionButton("payout","Close")
                            
                          )) 
                 ),
        fluidRow(),
        fluidRow(),width = 6
      ),
      
      mainPanel(
        fixedRow(
          column(3,"Time : "),
          column(4,textOutput("ordertime"))
        ),
        #fixedRow(column(4,textOutput("orderident"))),
        fluidRow(
          column(7,
                 
                 absolutePanel(
                   wellPanel(
                     tableOutput("currtabtbl"),
                     if(!is.null(ordrchc$data)){
                       uiOutput("orderpanel")
                     }),
                   top="5%",height = "100%",left="5%",width="100%"
                 )
                 
          ),
          column(1),
          column(4,
                 fluidRow("Tab Name"),
                 fluidRow(
                   h4(textOutput("currtabname"))
                   
                   # column(4,"Tab Name :"),
                   
                 ),
                 fluidRow("Total"),
                 fluidRow(
                   h4("$"),
                   h4(textOutput("showclosetotal"))),
                 fluidRow("Tab ID"),
                 fluidRow(
                   h4(textOutput("currtabident"))
                   
                   # column(4,"Tab Name :"),
                   
                 )
          )
        ),
        width = 6
        
        
      ),
      position = "right"
    )
    
  })
  output$showclosetotal <- renderText(
    getclosetotal()
  )
  
  getclosetotal <- reactive({
    return(sum(c(as.numeric(acttbcontainer$itms$price))))
    
  })
  output$showtabpanel <- renderUI({
    if(!input$panelotab == "select tab"){
      mainPanel(
      fixedRow(
        column(4,h2(textOutput("nmfrmpanel"))),
        column(4),
        column(4,
          
            #tabsetPanel("Add to Order",uiOutput('addtotab')),
            #tabsetPanel("Close Tab",uiOutput('closetab'))
            actionButton("addtotab","Add to Order",width = '150px'),
            actionButton("closetab","Close Tab",width = '150px')
          
        )
      ),
      fixedRow(
        column(6,"Time :",p(textOutput("stimye")))
      ),
      fixedRow(
        column(4,"Ordered Items :"),
        column(2),
        column(6,tableOutput("tabitems"))
      ),
      fixedRow(
        column(6,"Total :",p(textOutput("tabtotal")))
      ),
      fluidRow(
        
      ),
      position = "left",
      fluid = TRUE
  )
    }
  })
  tabslst <- reactiveValues(open = NULL)
  opentablst <- reactive({
    con = dbConnect(SQLite(), dbname="posdata.sqlite")
    tbs <<- as.data.table(dbReadTable(con,"tabs"))
    dbDisconnect(con)
    tabslst$open <<- tbs$name[which(tbs$open == 1)]
    return(tabslst$open)
  })
  pgnum <- reactiveValues(pglist = 1,testlst = 1,testlist = NULL)
  observeEvent(input$nxt_butt,{
    pgnum$tablist <<- pgnum$pglist + 1
    return(pgnum$pglist)
  })
  observeEvent(input$prv_butt,{
    pgnum$pglist <<- pgnum$pglist - 1
    return(pgnum$pglist)
=======
  
  output$showtabpanel <- renderUI({
    if(!input$panelotab == "select tab"){
      mainPanel(
      fixedRow(
        column(4,h2(textOutput("nmfrmpanel"))),
        column(4),
        column(4,
          
            #tabsetPanel("Add to Order",uiOutput('addtotab')),
            #tabsetPanel("Close Tab",uiOutput('closetab'))
            actionButton("addtotab","Add to Order",width = '150px'),
            actionButton("closetab","Close Tab",width = '150px')
          
        )
      ),
      fixedRow(
        column(6,"Time :",p(textOutput("stimye")))
      ),
      fixedRow(
        column(4,"Ordered Items :"),
        column(2),
        column(6,tableOutput("tabitems"))
      ),
      fixedRow(
        column(6,"Total :",p(textOutput("tabtotal")))
      ),
      fluidRow(
        
      ),
      position = "left",
      fluid = TRUE
  )
    }
  })
  tabslst <- reactiveValues(open = NULL)
  opentablst <- reactive({
    con = dbConnect(SQLite(), dbname="posdata.sqlite")
    tbs <<- as.data.table(dbReadTable(con,"tabs"))
    dbDisconnect(con)
    tabslst$open <<- tbs$name[which(tbs$open == 1)]
    return(tabslst$open)
>>>>>>> 0934efa6d0b226a80bf2fb4330ed36021058dc2c
  })
  
  output$mytabs = renderUI({
<<<<<<< HEAD
    pgnum$testlist <- opentablst()
    pgnum$testlst <- length(pgnum$testlist)
    if(pgnum$testlst>20){
      sm <- ((pgnum$pglist - 1) * 20) + 1 
      lg <- (pgnum$pglist * 20)
    }else{
      sm <- 1
    lg <- pgnum$testlst
    }
    
    h4(radioButtons("panelotab",NULL,c("select tab",pgnum$testlist[sm:lg]),inline=TRUE,width = '100px'))
    
=======
    
     
    h4(radioButtons("panelotab",NULL,c("select tab",opentablst()),inline=TRUE,width = '100px'))
>>>>>>> 0934efa6d0b226a80bf2fb4330ed36021058dc2c
  #  if(length(opentablst) < 4){
  #  nTabs <- length(opentablst)
  #  }else{
  #    nTabs <- 3
  #  }
  # tlits <- lapply(1:nTabs,function(i) paste(opentablst[[i]]))
  #  myTabs <- lapply(tlits, tabPanel)
  #  do.call(tabsetPanel, c(myTabs,id="panelotab"))
  #  
  })
  
  output$defaultsdbrpanel <- renderUI({
    wellPanel(
      #lapply(1:length(categorylist), function(i) {
      radioButtons("catslist","Category", c(categorylist,""),selected = "")
      #}),
      
    )
  })  
  output$refreshsdbrpanel <- renderUI({
    wellPanel(
      #lapply(1:length(categorylist), function(i) {
      radioButtons("catslist","Category", c(categorylist,""),selected = "")
      #}),
      
    )
  })  
  output$sdbrpnl <- renderUI({
    if(is.null(sdbrdata$data)){
      sdbrdata$data <-"defaultsdbrpanel"
    }
    uiOutput(sdbrdata$data)
  })  

  
  output$initpage <- renderUI({
    sidebarLayout(
      sidebarPanel(p("Initialize all tables"),
        wellPanel(p("1) Enter tax rate for all tax codes")
            )
        ),
      mainPanel(
        fluidRow(
          wellPanel(
            numericInput("retailtax","Retail Tax Rate",value= 0, min = 0, max = 10,step = .25),
            numericInput("foodtax","Food Tax Rate",value= 0,min = 0, max = 10,step = .25),
            numericInput("alctax","Alcohol Tax Rate",value= 0,min = 0, max = 10,step = .25),
            numericInput("othertax","Other Tax Rate",value= 0,min = 0, max = 10,step = .25),
            actionButton("taxesbutton","Submit"),
            uiOutput("aftertaxconditional")
          )
        )
      )
    )
  })
  aftertax <- eventReactive(input$taxesbutton, {
    h2("Food Categories")
    
  })
  output$aftertaxconditional <- renderUI({
    aftertax()
      
   
    })
  output$categlist <- renderText(
    #load("categorylist"),
    
   # unlist(lapply(1:length(categorylist), function(i)  categorylist[[i]]))
    categorylist
    
  )

  output$categorypanel <- renderUI({
    con = dbConnect(SQLite(), dbname="posdata.sqlite")
    items <- as.data.table(dbReadTable(con,"items"))
    dbDisconnect(con)
    if(!input$catslist == "none"){
      inputPanel(
        selectInput("item_selecta","Items",makedroplist()),
       fixedRow(
         column(4,h2(input$catslist))
       )
       
       )
      }
    
  })
  output$itemnameout <- renderText(
    input$item_selecta
  )
 
  
  output$delitmpanel <- renderUI({
    item_stats <- items[which(items$name == input$item_selecta)]
    wellPanel(
      fixedRow(
        p("Are you sure you want to delete the following record?")
      ),
      fixedRow(
      textOutput("itemnameout")
      ),
      fixedRow(
        column(4,actionButton("cnfmbutton","Confirm"),actionButton("cnclbutton","Cancel"))
      )
      
    )
  })
  output$itemspanel <- renderUI({
    if(is.null(pnldata$data)){
      pnldata$data <-"defaultitmpanel"
    }
    uiOutput(pnldata$data)
  })  
  output$defaultitmpanel <- renderUI({
    con = dbConnect(SQLite(), dbname="posdata.sqlite")
    items <- as.data.table(dbReadTable(con,"items"))
    dbDisconnect(con)
    item_stats <- items[which(items$name == input$item_selecta)]
    stat_names <- names(item_stats)
    wellPanel(
      lapply(1:length(stat_names), function(i) {
        fixedRow(
          column(4,h4(stat_names[[i]])),
          column(4,h4(item_stats[[i]]))
        )
      })
    )
  })  
  output$edititmpanel <- renderUI({
    item_stats <- items[which(items$name == input$item_selecta)]
    stat_names <- names(item_stats)
    wellPanel(
      
        fixedRow(
          column(4,h4("name ")),
          column(4,textInput("nameedt",NULL,value = item_stats[[1]], placeholder = item_stats[[1]]))
          ),
        fixedRow(
          column(4,h4("price ")),
          column(4,numericInput("priceedt",NULL,value = item_stats[[2]], min = 0,step = .05))
          ),
        fixedRow(
          column(4,h4("modifier ")),
          column(4,textInput("modedt",NULL,value = item_stats[[3]], placeholder = item_stats[[3]]))
        ),
        fixedRow(
          column(4,h4("sides ")),
          column(4,textInput("sidesedt",NULL,value = item_stats[[4]],placeholder = item_stats[[4]]))
        ),
        fixedRow(
          column(4,h4("active ")),
          column(4,checkboxInput("actedt",NULL,if(!is.na(item_stats[[5]])){value =item_stats[[5]]}else{value =FALSE}))
        ),
        fixedRow(
          column(4,h4("category")),
          column(4,h4(if(is.na(item_stats[[6]])){"none"}else{item_stats[[6]]}))
        ),
        fixedRow(
          column(4,h4("tax group ")),
          column(4,h4(items$taxgrp[which(items$category == item_stats[[6]])][1]))
        ),
        fixedRow(
          column(4,h4("item id ")),
          column(4,h4(item_stats[[8]]))
        ),
        fixedRow(
          column(4,h4()),
          column(4,actionButton("saveedt","Save"))
        )
    )
    })
  output$additmpanel <- renderUI({
    con = dbConnect(SQLite(), dbname="posdata.sqlite")
    items <- as.data.table(dbReadTable(con,"items"))
    dbDisconnect(con)
    item_stats <- items[which(items$name == input$item_selecta)]
    nxtvalid <- items$itmident[length(items$itmident)]+1
    print("nxtvalid")
    print(nxtvalid)
    wellPanel(
      
      fixedRow(
        column(4,h4("name ")),
        column(4,textInput("nameadd",NULL))
      ),
      fixedRow(
        column(4,h4("price ")),
        column(4,numericInput("priceadd",NULL,value=0, min = 0,step = .05))
      ),
      fixedRow(
        column(4,h4("modifier ")),
        column(4,textInput("modadd",NULL))
      ),
      fixedRow(
        column(4,h4("sides ")),
        column(4,textInput("sidesadd",NULL))
      ),
      fixedRow(
        column(4,h4("active ")),
        column(4,checkboxInput("actadd",NULL))
      ),
      fixedRow(
        column(4,h4("category")),
        column(4,selectInput("addtocat",NULL,categorylist,selected = input$catslist))
      ),
      fixedRow(
        column(4,h4("tax group ")),
        column(4,selectInput("addtotax",NULL,typeslist))
      ),
      fixedRow(
        column(4,h4("item id ")),
        column(4,h4(nxtvalid))
        
      ),
      fixedRow(
        column(4,h4()),
        column(4,actionButton("saveadd","Save"))
      )
    )
  })
  observeEvent(input$cnfmbutton,{
    itm_stats <- items[which(items$name == input$item_selecta)]
    
    qry <- paste("DELETE FROM `items` WHERE `itmident`='",itm_stats$itmident,"';",sep="")
    rs <- dbSendQuery(con,qry)
    dbHasCompleted(rs)
    print("items")
    print(tail(items))
    #items <- as.data.table(dbReadTable(con,"items"))
    rm(qry,itm_stats,rs)
  })
  observeEvent(input$cnclbutton,{
    pnldata$data <- "defaultitmpanel"
  })
  observeEvent(input$saveadd,{
    con = dbConnect(SQLite(), dbname="posdata.sqlite")
    items <- as.data.table(dbReadTable(con,"items"))
    nameset <- items$name
    print(unlist(nameset))
    print("nameshit")
    print(input$nameadd)
    if(input$nameadd %in% nameset){
      p("duplicate entry")
      #pnldata$data <- "defaultitmpanel"
    }else{
    nxtvalid <- items$itmident[length(items$itmident)]+1
    print("nxtvalid")
    print(nxtvalid)
    ident <- nxtvalid
    if(input$actadd == TRUE){
      addact <- 1
    }else if(input$actadd == FALSE){
      addact <- 0
    }
    qry <- paste("INSERT INTO items (name, price, modifier,sides,active,category,taxgrp,itmident) VALUES ('",input$nameadd,"','",input$priceadd,"','",input$modadd,"','",input$sidesadd,"','",addact,"','",input$addtocat,"','",input$addtotax,"','",ident,"');",sep = "")
    rs <- dbSendQuery(con,qry)
    dbHasCompleted(rs)
    #items <- as.data.table(dbReadTable(con,"items"))
    print("items")
    print(tail(items))
    print("items line")
    print(items[which(items$name == input$item_selecta)])
    rm(qry)
    sdbrdata$data <-"refreshsdbrpanel"
    dbDisconnect(con)
    }
    #items$name[as.integer(nxtvalid())] <- input$nameadd
    #items$price[as.integer(nxtvalid())] <- input$priceadd
    #items$modifier[as.integer(nxtvalid())] <- input$modadd
    #items$sides[as.integer(nxtvalid())] <- input$sidesadd
    #items$category[as.integer(nxtvalid())] <- input$addtocat
    #items$active[as.integer(nxtvalid())] <- input$actadd
    #dbWriteTable(con,"items",items,overwrite=TRUE)  
    })
  
  observeEvent(input$rfrshtbls, {    
    items <- as.data.table(dbReadTable(con,"items"))
    dbDisconnect(con)
    print("items")
    print(tail(items))
    
    pnldata$data <- "defaultitmpanel"
    
  })
#output$typeconditional <- 
  
  observeEvent(input$taxesbutton,{
    
    taxesdt <- data.table(taxident = c("tax1","tax2","tax3","tax4","tax5"),name=c("retail","food","alcohol","other","non"),rate=c(input$retailtax,input$foodtax,input$alctax,input$othertax,0))
    if(dbExistsTable(posdb,"taxesdt")){
      dbRemoveTable(posdb,"taxesdt")
    }
    dbWriteTable(posdb, "taxesdt", taxesdt)
    dbDisconnect(posdb)
    taxesdone <- 1
    
  })
  
  observeEvent(input$pressme,{
    
    typeslist <- categories_dt$type
    #load("categorylist")
    if(categorylist == ""){
      categories_dt <- data.table(names = input$catname,type = input$typedrop)
      write.table(categories_dt,"categories_dt")
      categorylist <- categories_dt$names
      save(categorylist,file="categorylist")
      }else{
      categorylist <- c(categorylist,input$catname)
      typeslist <- c(typeslist,input$typedrop)
      categories_dt <- data.table(names = categorylist,type = typeslist)
      write.table(categories_dt,"categories_dt")
      save(categorylist,file="categorylist")
      }
    print(categorylist)
  }
  )

})
)
