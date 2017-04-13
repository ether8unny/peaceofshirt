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
con = dbConnect(SQLite(), dbname="peaceofshirt/posdata.sqlite")
items <- as.data.table(dbReadTable(con,"items"))
tb_data <- as.data.table(dbReadTable(con,"tabs"))
dbDisconnect(con)
if(length(tb_data$tabident) > 0){
  initident <- tb_data$tabident[length(tb_data$tabident)]+1
}else{
  initident <- 1
}
pginit <- 1
#print("tb_data")
print(initident)
counter <- 0
typeslist <- c("retail","food","alcohol","non-tax","create new")


#print(orderid)
lastcatslistord <- ""
fl_lst <- unique(items$category)
categorylist <- fl_lst[which(!fl_lst== "Sides" & !fl_lst == "Modifier")]


shinyApp(
  shinyUI(
    uiOutput("navigate")
    #  uiOutput("current")
  ),
  
  
  shinyServer(function(input, output, session) {
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
                   selected = stateof$currentpage
        ),
        
        uiOutput(stateof$currentpage)
       # uiOutput("action_panel"),
      #  uiOutput("status_panel")
      )
    })
   stateof <- reactiveValues(paymentlist = NULL,currentpage = "orderpage",lastpage = NULL)
    ordrchc <- reactiveValues(data = NULL)
    observeEvent(input$navipage,{
      clearacttb()
      stateof$lastpage <<- stateof$currentpage
      stateof$currentpage <<- input$navipage
    #  print(stateof$currentpage)
      #return(stateof$currentpage)
    })
    observeEvent(stateof$currentpage,{
      primarychoices$panel <- switch(stateof$currentpage,
                    orderpage = "buttons4items",
                    addtotabpage = "buttons4items",
                    tabpage = "tabslistpnl",
                    closetabpage = "pymntslistpanel"
      )
      secondarychoices$panel <- switch(stateof$currentpage,
                                       orderpage = "itmchoicespanel",
                                       addtotabpage = "itmchoicespanel",
                                       tabpage = "tabsactbuttons",
                                       closetabpage ="amountpnl"
        
      )
      lowerpnlbuttons$panel <- switch(stateof$currentpage,
                                      orderpage = "orderactbuttons",
                                      addtotabpage = "updorderbuttons",
                                      tabpage = "tabslistbuttonspnl",
                                      closetabpage = "closetabbuttons")
    })
    
    output$tabsactbuttons <- renderUI({
      wellPanel(
        fluidRow(
          actionButton("addtotab","Add to Order",width = '100px')
          ),
        fluidRow(
        actionButton("closetab","Close Tab",width = '100px')
        )
      )
    })
    output$closetabbuttons <- renderUI({
      wellPanel(
        fluidRow(
          actionButton("payout","Close",width = '100px')
        )
      )
    })
    output$updorderbuttons <- renderUI({
      wellPanel(
        fluidRow(
          actionButton("updord","Send",width = '100px')
        )
      )
    })
    output$buttons4items <- renderUI({
      wellPanel( 
      radioButtons("catslistord","Category", c(categorylist,""),selected = lastcatslistord)
      )
    })
    output$orderactbuttons <- renderUI({
      wellPanel( 
      actionButton("sndord","Send")
      )
    })
    getitems <- reactive({
      con = dbConnect(SQLite(), dbname="peaceofshirt/posdata.sqlite")
      itms <- as.data.table(dbReadTable(con,"items"))
      dbDisconnect(con)
      return(itms)
    })
    v <- reactiveValues(counter = initident)
    output$itmchoicespanel <- renderUI({
      if(!input$catslistord == ""){
        items <- getitems()
        catchoice <- input$catslistord
        itmslist <- items$name[which(items$category == catchoice & items$active == 1)]
        itmsidents <- items$itmident[which(items$category == catchoice & items$active == 1)]
        wellPanel(
        radioButtons("idbox",catchoice,c(itmslist,""),selected= "",inline=FALSE)
        )}
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
    clearacttb <- reactive({
      ordrchc$data <- NULL
      ordrprc$data <- NULL
      acttbcontainer$tabident = NULL
      acttbcontainer$name = NULL
      acttbcontainer$itms = NULL
      acttbcontainer$time = NULL
      acttbcontainer$itms$price = NULL
    })
    observeEvent(input$updord,{
      con = dbConnect(SQLite(), dbname="peaceofshirt/posdata.sqlite")
      
      nameident <- acttbcontainer$name
      updordtbl <- ord_tbl()
      tmp_ttl <- sum(c(as.numeric(acttbcontainer$itms$price),as.numeric(ordrprc$data)))
      print(tmp_ttl)
      dbWriteTable(con, nameident, ord_tbl(), append = TRUE)
      dbDisconnect(con)
      con = dbConnect(SQLite(), dbname="peaceofshirt/posdata.sqlite")
      qry2 <- paste("UPDATE `tabs` SET `total`= '",as.numeric(tmp_ttl),"' WHERE `tabident`='",as.integer(acttbcontainer$tabident),"';",sep = "")
      print(qry2)
      rs2 <- dbSendQuery(con,qry2)
      dbHasCompleted(rs2)
      clearacttb()
      tabslst$open <- NULL
      stateof$lastpage <<- stateof$currentpage
      stateof$currentpage <- "orderpage"
      dbDisconnect(con)
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
      as.numeric(timesish$total)
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
    output$orderpanel <- renderUI({
      
      tableOutput("ordertbl")
      
    })
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
      if(!is.null(ordrchc$data)){
      odtbl <- data.table(name = ordrchc$data, price = ordrprc$data)
      return(odtbl)}
    })
    ord_ttl <- reactive({
      odttl <-sum(as.numeric(ordrprc$data))
      return(odttl)
    })
    observeEvent(input$tbname,{
      acttbcontainer$name <- input$tbname
    })
    observeEvent(input$sndord,{
      con = dbConnect(SQLite(), dbname="peaceofshirt/posdata.sqlite")
      
      nameident <- acttbcontainer$name
      qry0 <- paste("INSERT INTO tabs (name, time, total, open, tabident) VALUES ('",nameident,"','",ord_time(),"','",as.numeric(ord_ttl()),"','",1,"','",as.integer(orderid()),"');",sep = "")
      rs0 <- dbSendQuery(con,qry0)
      dbHasCompleted(rs0)
      #qry1 <- paste("INSERT INTO master_itms (name, price, tabident) VALUES ('",input$tbname,"','",ord_time,"','",ord_ttl,"','",1,"','",orderid,"');",sep = "")
      dbWriteTable(con, nameident, ord_tbl())
      acttbcontainer$name <- NULL
      ordrchc$data <- NULL
      ordrprc$data <- NULL
      tabslst$open <- NULL
      v$counter  <<- v$counter +1
      dbDisconnect(con)
    })
    output$mainorderpanel <- renderUI({
      tagList(
      fixedRow(
        column(3,"Time : "),
        column(4,textOutput("ordertime"))
      ),
      #fixedRow(column(4,textOutput("orderident"))),
      fluidRow(
        column(8,
               
               
                 wellPanel(
                   if(!is.null(ordrchc$data)){
                     uiOutput("orderpanel")
                   })
                 
               
        ),
        
        column(4,
               fluidRow("Tab Name"),
               fluidRow(
                 h4(textInput("tbname",NULL,value = if(is.null(acttbcontainer$name)){paste("quick sell ",orderid(),sep="")}else{acttbcontainer$name},placeholder = if(is.null(acttbcontainer$name)){paste("quick sell ",orderid(),sep="")}else{acttbcontainer$name}))
                 
                 # column(4,"Tab Name :"),
                 
               ),
               fluidRow("Total"),
               fluidRow(
                 h2(textOutput("ordertotal")))
        )
      )
      )
      
    })
    observeEvent(input$idbox, {    
      if(!input$idbox == ""){
        if(is.null(ordrprc$data)){
          items <- getitems()
          itminfo <- items[which(items$name == input$idbox),]
          print(itminfo)
          print(itminfo$sides)
          if(!itminfo$sides=="none"){
            secondarychoices$panel <<- "sidespanel"
            ordchcpnl$data <<- itminfo
          }
          priceinfo <- itminfo$price
          
          ordrprc$data <- priceinfo
          #ord_dt <- data.table(name = items$name[which(items$name == ordrchc$data)],price = items$price[which(items$name == ordrchc$data)])
        }else{
          items <- getitems()
          itminfo <- items[which(items$name == input$idbox),]
          print(itminfo)
          print(itminfo$sides)
          
          if(!itminfo$sides=="none"){
            secondarychoices$panel <<- "sidespanel"
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
    primarychoices <- reactiveValues(panel = NULL)
    secondarychoices <- reactiveValues(panel = "")
    lowerpnlbuttons <- reactiveValues(panel = "orderactbuttons")
    ordchcpnl <- reactiveValues(panel = NULL)
    stateof <- reactiveValues(currentpage = "orderpage")
    ordrprc <- reactiveValues(data = NULL)
    #orderpage
    output$orderpage <- renderUI({
      
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(6,
                   
                     uiOutput(primarychoices$panel)
                     
            ),
            column(6,
                  
                     uiOutput(secondarychoices$panel)
                   
            )
          ),
          fluidRow(
            column(6,
                 
              uiOutput(lowerpnlbuttons$panel)
              ),
            column(6)
             
          ),
          width = 6
        ),
        mainPanel(
          uiOutput("mainorderpanel"),width = 6
          
            
          
          
          
        
      ),
      position = "left"
      )
    })
    #tabpage
    acttbcontainer <- reactiveValues(name = NULL , time = NULL, total = NULL, open = NULL, tabident = NULL, itms = NULL)
    tbnm <- reactiveValues(name = NULL)
    makeacttb <- reactive({
      con = dbConnect(SQLite(), dbname="peaceofshirt/posdata.sqlite")
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
      acttbcontainer$itms = getitems()
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
      con = dbConnect(SQLite(), dbname="peaceofshirt/posdata.sqlite")
      gt_itms <- as.data.table(dbReadTable(con,acttbcontainer$name))
      dbDisconnect(con)
      return(gt_itms)
    })
    output$tabslistbuttonspnl <- renderUI({
      wellPanel(
        fluidRow(
          if(pgnum$testlst > 20){actionButton("prv_butt","Prev",width = "50px")}
        ),
        fluidRow(
          if(pgnum$testlst > 20){actionButton("nxt_butt","Next",width = "50px")}
        )
      )
    }) 
    pgnum <- reactiveValues(pglist = 1,testlst = 1,testlist = NULL)
    tabslst <- reactiveValues(open = NULL)
    opentablst <- reactive({
      con = dbConnect(SQLite(), dbname="peaceofshirt/posdata.sqlite")
      tbs <<- as.data.table(dbReadTable(con,"tabs"))
      dbDisconnect(con)
      tabslst$open <<- tbs$name[which(tbs$open == 1)]
      return(tabslst$open)
    })
    output$sidespanel <- renderUI({
      tblname <- paste(ordchcpnl$data$sides,"_side",sep="")
      print(tblname)
      con = dbConnect(SQLite(), dbname="peaceofshirt/posdata.sqlite")
      sides_tmp <<- as.data.table(dbReadTable(con,tblname))
      dbDisconnect(con)
      sideitmslist <- sides_tmp$name[which(sides_tmp$active == 1)]
      #itmsidents <- items$itmident[which(items$category == catchoice & items$active == 1)]
      h4(radioButtons("sidebox","Sides",c(sideitmslist,""),selected= "",inline=FALSE))
    })
    observeEvent(input$sidebox, {    
      if(!input$sidebox == ""){
        
        
        ordrchc$data <- c(ordrchc$data,input$sidebox)
        #ord_dt <- data.table(name = items$name[which(items$name == ordrchc$data)],price = items$price[which(items$name == ordrchc$data)])
        
        return(ordrchc$data)}
    })
    observeEvent(input$sidebox, {    
      if(!input$sidebox == ""){
        
        sditminfo <- items[which(items$name == input$sidebox),]
        print(sditminfo)
        priceinfo <- as.numeric(sditminfo$price)
        ordrprc$data <- c(as.numeric(ordrprc$data),priceinfo)
        secondarychoices$panel <<- "itmchoicespanel"
        ordchcpnl$data <<- NULL
        #ord_dt <- data.table(name = items$name[which(items$name == ordrchc$data)],price = items$price[which(items$name == ordrchc$data)])
        print(ordchcpnl$panel)
        return(ordrprc$data)}
    })
    output$tabslistpnl = renderUI({
      pgnum$testlist <- opentablst()
      pgnum$testlst <- length(pgnum$testlist)
      if(pgnum$testlst>20){
        sm <- ((pgnum$pglist - 1) * 20) + 1 
        lg <- (pgnum$pglist * 20)
      }else{
        sm <- 1
        lg <- pgnum$testlst
      }
      wellPanel(
        fluidRow(
      h5(radioButtons("panelotab",NULL,c("select tab",pgnum$testlist[sm:lg]),inline=TRUE,width = '100px'))
        ),
      width=3
      )
    })
    output$tabpage <- renderUI({
      
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(6,
                   
                   uiOutput(primarychoices$panel)
                   
            ),
            column(6,
                   
                   uiOutput(secondarychoices$panel)
                   
            )
          ),
          fluidRow(
            column(6,
                   wellPanel(
                     uiOutput(lowerpnlbuttons$panel))
            ),
            column(6)
            
          ),
          width = 6
          
        ),
        mainPanel(
          uiOutput("showtabpanel"),width=6
        )
      )
    })
    observeEvent(input$panelotab,{
      if(!input$panelotab == "select tab"){
      
        }
    })
    tbs_dt <- reactive({
      con = dbConnect(SQLite(), dbname="peaceofshirt/posdata.sqlite")
      tbs <- as.data.table(dbReadTable(con,"tabs"))
      dbDisconnect(con)
      tbsrtn <- tbs[which(tbs$name == input$panelotab)]
      return(tbsrtn)
    })
    output$showtabpanel <- renderUI({
      if(!input$panelotab == "select tab"){
        tagList(
          fixedRow(
            column(6,h2(textOutput("nmfrmpanel"))),
            column(4,"Time :",p(textOutput("stimye")))
          ),
          fixedRow(
           # column(4,"Ordered Items :"),
            
            column(6,tableOutput("tabitems"))
          ),
          fixedRow(
            column(6,"Total :",p(textOutput("tabtotal")))
          )
        )
      }
    })
    #addtoorderpage
    addtoordervars <- reactiveValues(tabid = NULL,name = NULL,itms = NULL,time = NULL,prcs = NULL)
    observeEvent(input$addtotab,{
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
    
    observeEvent(input$closetab,{
      pymntpnl$panel <- NULL
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
    })
    output$addtotabpage <- renderUI({
     
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(6,
                   
                   uiOutput(primarychoices$panel)
                   
            ),
            column(6,
                   
                   uiOutput(secondarychoices$panel)
                   
            )
          ),
          fluidRow(
            column(6,
                   
                     uiOutput(lowerpnlbuttons$panel))
           ,
            column(6)
            
          ),
          width = 6
        ),
        
        mainPanel(
          uiOutput("mainaddtotabpnl"),
          width = 6
          
          
        ),
        position = "left"
      )
    })
    output$mainaddtotabpnl <- renderUI({
      tagList(
      fixedRow(
        column(3,"Time : "),
        column(4,textOutput("ordertime"))
      ),
      #fixedRow(column(4,textOutput("orderident"))),
      fluidRow(
        column(7,
               
               
               wellPanel(
                 tableOutput("currtabtbl"),
                 if(!is.null(ordrchc$data)){
                   uiOutput("orderpanel")
                 })
               
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
                 h2(paste("$",sum(c(as.integer(acttbcontainer$itms$price),as.integer(ordrprc$data))),sep=""))),
               fluidRow("Tab ID"),
               fluidRow(
                 h4(textOutput("currtabident"))
                 
                 # column(4,"Tab Name :"),
                 
               )
        )
      )
      )
    })
    closingdata <- reactiveValues(tab_id = NULL,tab_name = NULL,total = NULL,time_order = NULL,time_close = NULL,
                                  MC_pay = NULL,Visa_pay = NULL, AMC_pay = NULL, DISC_pay = NULL,cash_pay = NULL,GC_pay = NULL,
                                  MC_amt = NULL,Visa_amt = NULL, AMC_amt = NULL, DISC_amt = NULL,cash_amt = NULL,GC_amt = NULL)
    transaction_data
    output$payout <- renderUI({
      con = dbConnect(SQLite(), dbname="peaceofshirt/posdata.sqlite")
      qry2 <- paste("UPDATE `tabs` SET `open`= '0' WHERE `tabident`='",as.integer(acttbcontainer$tabident),"';",sep = "")
      print(qry2)
      rs2 <- dbSendQuery(con,qry2)
      dbHasCompleted(rs2)
      
      
    })
    output$amountpnl <- renderUI({
      exactamt <- getclosetotal()
      wellPanel(
      h2(numericInput("cashamt",NULL,exactamt,min=.01,max=exactamt+1000,step = .01))
      )
      #radioButtons("cashlist",NULL,c(exactamt,""),selected= exactamt,inline=FALSE)
    })
    
    output$pymntslistpanel <- renderUI({
      wellPanel(
      radioButtons("paymentlist",NULL,c("Cash","MasterCard","Visa","Discover","AmEx","Gift Card"),selected="Cash" ,inline=FALSE)
      )
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
                 
                 uiOutput(primarychoices$panel)
                 
          ),
          column(6,
                 
                 uiOutput(secondarychoices$panel)
                 
          )
        ),
        fluidRow(
          column(6,
                 
                 uiOutput(lowerpnlbuttons$panel))
          ,
          column(6)
          
        ),
        width = 6
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
        position = "left"
      )
      
    })
    output$showclosetotal <- renderText(
      getclosetotal()
    )
    
    getclosetotal <- reactive({
      return(sum(c(as.numeric(acttbcontainer$itms$price))))
      
    })
    #closetabpage
    
    
    
})


)