source("read.fasta.R")
source("show.table.nonR.R")
source("show.table.Red.R")
source("ResCount.R")
source("formula.R")
source("report.MassTable.nonR.R")
source("report.CalcTable.nonR.R")
source("report.CalcTable.R.R")
source("report.item.nonR.R")
source("report.item.R.R")
source("Split.glycans.R")
source("Report.MassTable.R.R")
source("EC.280.R")
source("Addspace.R")
source("EC.residue.R")
source("PLOTLY.R")
source("harnessNewUI.R")
source("harnessNewUI_NR.R")
library(OrgMassSpecR)
library(knitr)
library(kableExtra)
library(dichromat)
library(plotly)
library(ggplot2)


shinyServer(function(input, output) {
  
  PRO<-reactive({ 
    validate(
      need(input$input.file$datapath !="", "")
    )
    read.fasta(isolate(input$input.file[['datapath']]))
     })
  
  observeEvent(input$input.file,{
   output$lc.seq<-renderText({as.character(PRO()$LC[2])})
  })
  
  
   output$hc.seq<-renderText({as.character(PRO()$HC[2])})
  

   observeEvent(input$input.file, {
     output$hc.count<-renderValueBox({
       valueBox(value=tags$p(paste(as.character(PRO()$HC[1]), "  amino acids", sep=""), style="font-size: 50%"), subtitle = tags$p(strong(em(span("Heavy chain", style="font-family:'3ds'"))), style="font-size: 130%"), icon=icon("angle-double-left"), color = "yellow")})
   })
  
   
   observeEvent(input$input.file, {
     output$lc.count<-renderValueBox({
       valueBox( value=tags$p(paste(as.character(PRO()$LC[1]), "  amino acids", sep=""),style="font-size: 50%"), subtitle = tags$p(strong(em(span("Light chain", style="font-family:'3ds'"))), style="font-size: 130%"), icon=icon("angle-left"), color = "light-blue")})
   })
   

 observeEvent(input$input.file, {
   output$Hc.count<-function(){
   knitr::kable(ResCount(as.character(PRO()$HC[2])), format="html",caption = "Heavy Chain Residue Count") %>% kable_styling(bootstrap_options = c("striped", "condensed", "responsive", "hover")) %>% row_spec(c(1,2), bold = T,color="white",background = "#F77807")
   }
 })
   
   
 observeEvent(input$input.file, {
   output$Lc.count<-function(){
     knitr::kable(ResCount(as.character(PRO()$LC[2])), format="html",caption = "Light Chain Residue Count") %>% kable_styling(bootstrap_options = c("striped", "condensed", "responsive", "hover")) %>%
row_spec(c(1,2), bold = T,color="white",background = "#3088BE")
   }
 })
 
 observeEvent(input$input.file,{
   output$termi<-renderText('Terminal Modifications')
 })

   
   output$Nterm.hc<-renderUI({
     if(substr(as.character(PRO()$HC[2]),1,1) %in% c("Q","E")){
       radioButtons("Nterm.hc", label="Heavy chain N-term pE", choices=list("Yes","No"), selected = "Yes", inline=TRUE)
     } else {
       NULL}
   })
   
   output$Cterm.hc<-renderUI({
     if(substr(as.character(PRO()$HC[2]), as.numeric(as.character(PRO()$HC[1]))-1, as.numeric(as.character(PRO()$HC[1]))) =="GK") {
       radioButtons("Cterm.hc", label="Heavy chain C-term K Loss", choices=list("Yes","No"), selected="Yes", inline=TRUE)
     } else{
       NULL}
   })
   
   output$Nterm.lc<-renderUI({
     if(substr(as.character(PRO()$LC[2]),1,1) %in% c("Q","E")){
       radioButtons("Nterm.lc", label="Light chain N-term pE", choices=list("Yes","No"), selected = "Yes", inline=TRUE)
     } else {
       NULL}
   })
   
   output$hinge.ds<-renderUI({
     if(input$IgG.type!="IgG2"){
       numericInput("hinge.ds", label="Hinge", value=2, min=0, max=20, step=1, width="60%")
     }
     else if(input$IgG.type=="IgG2"){
       numericInput("hinge.ds", label="Hinge", value=4, min=0, max=20, step=1, width="60%")
     }
   })
   
   output[['HC-HC.ds']]<-renderUI({
     if(input$IgG.type=="Bispecifics"| input$IgG.type=="Fusion protein"){
       numericInput("HC-HC.ds", label="intra HC-HC", value=14, min=0, max=15, step=1,width="60%")
     }
     else if(input$IgG.type!="Bispecifics" | input$IgG.type!="Fusion protein"){
       numericInput("HC-HC.ds", label="intra HC-HC", value=8, min=0, max=15, step=1,width="60%")
     }
   })

   output$OtherMod<-renderUI({
       
       if(is.null(input$Cterm.hc)){
         selectInput("OtherMod", label=span("Pre-defined", style="font-size:10pt"), choices = list("none","+Lys", "+Glycation","Oxidation", "Water-Loss","NH3-Loss","Deamidation","Cysteinylation"), selected="none") 
       }
       else if(input$Cterm.hc == "Yes"){
         selectInput("OtherMod", label=span("Pre-defined", style="font-size:10pt"), choices = list("none","+Lys", "+Glycation","Oxidation", "Water-Loss","NH3-Loss","Deamidation","Cysteinylation"), selected="none") 
       } 
       else if(input$Cterm.hc == "No"){
         selectInput("OtherMod", label=span("Pre-defined", style="font-size:10pt"), choices = list("none", "+Glycation","Oxidation", "Water-Loss","NH3-Loss","Deamidation","Cysteinylation"), selected="none") 
       }
       
     })
     
  output$cust.ptm<-renderUI({
    if(input$c.ptm=="Yes"){
      textInput("cust.ptm", label=span("Please enter formula...", style="font-size:10pt"), value = "", placeholder = "for example, C6H12O6", width = "90%")
    } else {NULL}
    
  })
  
  output$red.cust.fml<-renderUI({
    if(input$red.c.ptm=="Yes"){
      textInput("red.cust.fml", label=span("Please enter formula...", style="font-size:10pt"), value = "", placeholder = "for example, C6H12O6", width = "90%")
    } else {NULL}
    
  })
  
  
  output$c.Howmany<-renderUI({
    if(input$c.ptm=="Yes"){
    numericInput("c.Howmany", label=span("# per antibody?",style="font-size:10pt"), value=0, min=0, max=100, step=1, width = "70%")
    } else {NULL}
    
  })
  
  output$red.Where2<-renderUI({
    if(input$red.c.ptm=="Yes"){
      selectInput("red.Where2", label=span("on which chain?",style="font-size:10pt"), choices = list("HC", "LC"), selected = "HC", width="60%")
    } else {NULL}
    
  })
  
  output$red.Howmany2<-renderUI({
    if(input$red.c.ptm=="Yes"){
      numericInput("red.Howmany2", label=span("# per chain?",style="font-size:10pt"), value=0, min=0, max=100, step=1, width = "70%")
    } else {NULL}
    
  })
  

    output$red.OtherMod<-renderUI({

        
        if(is.null(input$Cterm.hc)){
          selectInput("red.OtherMod", label=span("Pre-defined", style="font-size:10pt"), choices = list("none","+Lys", "+Glycation","Oxidation", "Water-Loss","NH3-Loss","Deamidation", "Cysteinylation"), selected="none") 
        }
        else if(input$Cterm.hc == "Yes"){
          selectInput("red.OtherMod", label=span("Pre-defined", style="font-size:10pt"), choices = list("none","+Lys", "+Glycation","Oxidation", "Water-Loss","NH3-Loss","Deamidation","Cysteinylation"), selected="none") 
        } 
        else if(input$Cterm.hc == "No"){
          selectInput("red.OtherMod", label=span("Pre-defined", style="font-size:10pt"), choices = list("none", "+Glycation","Oxidation", "Water-Loss","NH3-Loss","Deamidation","Cysteinylation"), selected="none") 
        }
        
      })
      

   
  Tables<-reactive({

      show.table.nonR(PRO(), input$Nterm.hc, input$Nterm.lc, input$Cterm.hc, input$glyco,input$OtherMod, input$Howmany, input$c.ptm, input$cust.ptm, input$c.Howmany  ,input$hinge.ds, input$`HC-LC.ds`,input$`HC-HC.ds`, input$`LC-LC.ds`, input$unR.hc, input$unR.lc, EXTRA()) 
      
    
  })
  
  Tables.Red<-reactive({ show.table.Red(PRO(), input$Nterm.hc, input$Nterm.lc, input$Cterm.hc, input$glyco.Red,input$red.OtherMod,input$red.Howmany,input$red.Where,  input$red.c.ptm, input$red.cust.fml, input$red.Where2, input$red.Howmany2, input$unR.hc, input$unR.lc, EXTRAR())})
  
  observeEvent(input$input.file, {
    output$poly.mass.lc<-renderValueBox({
      valueBox( value=tags$p(paste(Tables()$LC.ms, "  Da", sep=""), style="font-size: 70%"), subtitle = tags$p(strong(em(span("mass", style="font-family:'3ds'"))), style="font-size: 130%"), icon=icon("star-half-empty"), color = "light-blue")})
  })

  observeEvent(input$input.file,{
    output$poly.mass.hc<-renderValueBox({
      valueBox( value=tags$p(paste(Tables()$HC.ms, "  Da", sep=""), style="font-size: 70%"), subtitle = tags$p(strong(em(span("mass", style="font-family:'3ds'"))), style="font-size: 130%"), icon=icon("star-half-o"), color = "yellow")})
  })


  observeEvent(input$input.file,{
    output$tb1<-renderValueBox({
      valueBox( value=tags$div(
        HTML(paste("C", tags$sub(Tables()$LC.fml[["C"]]),"H", tags$sub(Tables()$LC.fml[["H"]]), "N",tags$sub(Tables()$LC.fml[["N"]]),"O",tags$sub(Tables()$LC.fml[["O"]]),"S",tags$sub(Tables()$LC.fml[["S"]]), sep="")), style="font-size: 70%"), subtitle = tags$p(strong(em(span("Light chain polypeptide formula", style="font-family:'3ds'"))), style="font-size: 130%"), icon=icon("github"), color = "light-blue")})
  })
  
  
  observeEvent(input$input.file,{
    output$tb2<-renderValueBox({
      valueBox( value=tags$div(
        HTML(paste("C", tags$sub(Tables()$HC.fml[["C"]]),"H", tags$sub(Tables()$HC.fml[["H"]]), "N",tags$sub(Tables()$HC.fml[["N"]]),"O",tags$sub(Tables()$HC.fml[["O"]]),"S",tags$sub(Tables()$HC.fml[["S"]]), sep="")), style="font-size: 70%"), subtitle = tags$p(strong(em(span("Heavy chain polypeptide formula", style="font-family:'3ds'"))), style="font-size: 130%"), icon=icon("github"), color = "yellow")})
  })
  
  
  observeEvent(input$input.file,{
    output$tb22<-renderValueBox({
      valueBox( value=tags$div(
        HTML(paste("C", tags$sub(Tables()$mAb.fml[["C"]]),"H", tags$sub(Tables()$mAb.fml[["H"]]), "N",tags$sub(Tables()$mAb.fml[["N"]]),"O",tags$sub(Tables()$mAb.fml[["O"]]),"S",tags$sub(Tables()$mAb.fml[["S"]]), sep="")), style="font-size: 70%"), subtitle = tags$p(strong(em(span("Whole Ab formula", style="font-family:'3ds'"))), style="font-size: 130%"), icon=icon("android"), color = "purple")})
  })
  
  observeEvent(input$input.file,{
    output$NR.mass<-renderValueBox({
      valueBox( value=tags$p(paste(Tables()$mAb.ms, "  Da", sep=""), style="font-size: 70%"), subtitle = tags$p(strong(em(span("Whole Ab mass", style="font-family:'3ds'"))), style="font-size: 130%"), icon=icon("paper-plane-o"), color = "purple")})
  })
  
  
  observeEvent(input$input.file, {
    output$tbhc<-function(){
      knitr::kable(Tables.Red()$HC, format="html",caption = "Heavy Chain", align = "c") %>% kable_styling(bootstrap_options = c("striped", "condensed", "responsive", "hover"))  %>% row_spec(c(1), bold = T,color="white",background = "#F77807")
    }
  })
  
  observeEvent(input$input.file, {
    output$tblc<-function(){
      knitr::kable(Tables.Red()$LC, format="html",caption = "Light Chain", align = "c") %>% kable_styling(bootstrap_options = c("striped", "condensed", "responsive", "hover")) %>% row_spec(c(1), bold = T,color="white",background = "#3088BE")

    }
  })
  

  observeEvent(input$input.file,{
    output$note<-renderText('NOTE: The "Fully reduced" formula and mass assume all disulfide bonds have been reduced; whereas the "Partially reduced" formula and mass correspond to Heavy Chain and Light Chain with un-reduced intra-chain disulfide bonds, which are typically retained even in reducing environment until denaturant (e.g., urea) is added.')
  })
  
  observeEvent(input$input.file,{
    output$note02<-renderText('NOTE: Calculations of polypeptide formulas and masses only depend on the provided amino acid sequences, no PTMs of any forms is considered.')
  })
  
  observeEvent(input$input.file,{
    output$note03<-renderText('NOTE: Moving cursor onto each bar to see the exact values.')
  })
  
  observeEvent(input$input.file,{
    output$note04<-renderText(paste0('EC = ', format(EC.poly(), scientific = FALSE), " /(m cm)"))
  })

  ECAA<-reactive({EC.residue(PRO()$HC[2], PRO()$LC[2])})
  
  output$EC.plot<-renderPlotly({ ## the plotly plot generation
    PLOTLY(ECAA())
  })


  
# ====================== code beblow is for insert/remove UI of non-reduced Ab N-glycans.
  
  xx<-reactiveValues(val=NULL)
  yy<-reactiveValues(val=NULL)

  observeEvent(input$add, ignoreInit = TRUE, {
   btn<-input$add
    id<-paste0("newUI", btn)
    
    insertUI(
      selector = "#placeholder", ui = div( id=id,
                                           fluidRow(
                                             column(width = 6,
                                                    selectInput(paste0("ui", btn), label="Addtional glycans",choices = list("Aglycosylated","Deglycosylated",'G0', "G0-GN", "G1", "G2","G0F","G1F","G2F", "M5", "M6" ),selected = "Aglycosylated", width="100%")      ),
                                             column(width = 6,
                                                    selectInput(paste0("num", btn), label="# of sites", choices = list("1","2","3","4","5","6","7","8"), selected = "2", width="60%")
                                                    )),
                                          fluidRow(
                                            column(width=6,
                                                   actionButton(paste0('rmv', btn), "Remove")
                                          )))
     
    )
xx$val<-c(xx$val, btn)
yy$val<-c(yy$val, btn)

  observeEvent(input[[paste0('rmv',btn)]], {
  
    removeUI(
      selector = paste0("#newUI", btn)
    )
    xx$val[btn]<-"X"
    yy$val[btn]<-"Y"
    
  })

  })
  newval1<-reactive({do.call("c", lapply(xx$val, function(x) {input[[paste0("ui", x)]]}))  })## collection of all newly added glycans
  newval2<-reactive({do.call("c", lapply(yy$val, function(x) {input[[paste0("num", x)]]})) }) ## collection of # of sites for each newly added glycans 

  EXTRA<-reactive(harnessNewUI_NR(newval1(), newval2()))
 
  # ====================== code above is for insert/remove UI of non-reduced Ab N-glycans. 
  
  # ====================== code below is for insert/remove UI of reduced Ab N-glycans.
  
  aa<-reactiveValues(val=NULL)
  bb<-reactiveValues(val=NULL)
  
  observeEvent(input$add.red, ignoreInit = TRUE, {
    btnR<-input$add.red
    id<-paste0("newUIR", btnR)
    
    insertUI(
      selector = "#placeholder2", ui = div( id=id,
                                           fluidRow(
                                             column(width = 6,
                                                    selectInput(paste0("uiR", btnR), label="Addtional",choices = list("Aglycosylated","Deglycosylated",'G0', "G0-GN", "G1", "G2","G0F","G1F","G2F", "M5", "M6" ),selected = "Aglycosylated", width="100%")      ),
                                             column(width = 6,
                                                    selectInput(paste0("numR", btnR), label="# of sites", choices = list("1","2","3","4","5","6","7","8"), selected = "2", width="60%")
                                             )),
                                           fluidRow(
                                             column(width=6,
                                                    actionButton(paste0('rmvR', btnR), "Remove")
                                             )))
      
    )
    aa$val<-c(aa$val, btnR)
    bb$val<-c(bb$val, btnR)
    
    observeEvent(input[[paste0('rmvR',btnR)]], {
      
      removeUI(
        selector = paste0("#newUIR", btnR)
      )
      aa$val[btnR]<-"X"
      bb$val[btnR]<-"Y"
      
    })
    
  })
  newval3<-reactive({do.call("c", lapply(aa$val, function(x) {input[[paste0("uiR", x)]]}))  })## collection of all newly added glycans
  newval4<-reactive({do.call("c", lapply(bb$val, function(x) {input[[paste0("numR", x)]]})) }) ## collection of # of sites for each newly added glycans 

  EXTRAR<-reactive(harnessNewUI(newval3(), newval4()))
 
  # ====================== code above is for insert/remove UI of reduced Ab N-glycans.
  


## ========================================= Reporting-associated codes==============================  
  
  
  output$r.HCnterm<-renderUI({
    if(substr(as.character(PRO()$HC[2]),1,1) %in% c("Q","E")){
      selectizeInput("r.HCnterm", label="Heavy chain pE", choices=list("Yes","No"), multiple = TRUE,selected = "Yes")  } else {
      NULL}
  })
  
  output$r.LCnterm<-renderUI({
    if(substr(as.character(PRO()$LC[2]),1,1) %in% c("Q","E")){
      selectizeInput("r.LCnterm", label="Light chain pE", choices=list("Yes","No"), multiple = TRUE,selected = "No")      } else {
      NULL}
  })
  
  
  
  output$r.HCcterm<-renderUI({
    if(substr(as.character(PRO()$HC[2]), as.numeric(as.character(PRO()$HC[1]))-1, as.numeric(as.character(PRO()$HC[1]))) =="GK") {
      selectizeInput("r.HCcterm", label=NULL, choices=list("cleaved"="Yes","attached"="No"), multiple=TRUE,selected = c("Yes","No"))  } else{
      NULL}
  })

  output$ngly<-renderUI({
    if(input$mol.format!="Fusion protein"){
    div(
  selectizeInput("ngly",NULL, choices = list("Degly",'G0/G0-GN', "G0/G0","G0F/G0F", "G0/G1", "G1/G1", "G1/G2","G0F/G1F", "G1F/G1F","G2F/G2F","G1F/G2F" ,"G0-GN/G0-GN", "G2/G2"), multiple = TRUE,selected = c( "Degly","G0F/G0F")),
  hr(),
  p('NOTE: select all glycans that apply; to deselect, click the one and press "backspace" button on keyboard.')
    )} else {
      div(
    selectizeInput("ngly",NULL, choices = list("Degly"), multiple = TRUE,selected = c( "Degly")),
        hr(),
        p('NOTE: other glycan forms will not be reported for fusion proteins, owing to high heterogeneity of glycans.'),
    hr(),
    
     numericInput("FP.ngly", label = span(strong("# glycan sites/heavy chain"), style="font-size:10pt"), value=4, min=0, max=10, step=1)
      )
} 
  
  })
  
  output$nonR.DS.total<-renderUI({
    if(input$mol.format=="Fusion protein" | input$mol.format=="Bispecifics"){
  sliderInput("nonR.DS.total", NULL, value=22, min=0, max=50, step=1, ticks = FALSE)
    } else {
  sliderInput("nonR.DS.total", NULL, value=16, min=0, max=50, step=1, ticks = FALSE)
    }
  })
  
  output$EC.ADC.MA<-renderUI({
    if(input$withADC == "Yes"){
      numericInput("EC.ADC.MA", label="Payload molar absorptivity", value=0, min=0, max=100000, step=1)  } else {
        NULL}
  })
  
  output$EC.ADC.n<-renderUI({
    if(input$withADC == "Yes"){
      numericInput("EC.ADC.n", label="# of payload?", value=0, min=0, max=50, step=1)  } else {
        NULL}
  })

  n.glycan.site<-reactive({ 
    if(input$mol.format!="Fusion protein" | is.null(input$FP.ngly)){1} else{input$FP.ngly}})
  
  ## Below GL, CMA, and r.table take care of the non-reduced reporting calc.##
  ## A function to add one space in the peptide sequence for every y residues.
  
  report.HCseq<-reactive({Addspace(as.character(PRO()$HC[2]),10)})  
  report.LCseq<-reactive({Addspace(as.character(PRO()$LC[2]),10)})  
  ## add one space every 10 amino acids, so the LaTex can wrap the long sequence.
  
  GL<-reactive({ report.item.nonR(input$r.HCnterm, input$r.LCnterm, input$r.HCcterm, input$ngly, input$nonR.DS.total )  })
  CMA<-reactive({Calc.Mass.all.nonR(as.character(PRO()$HC[2]),as.character(PRO()$LC[2]), GL(), input$mol.format, input$FP.ngly)  })
  r.table<-reactive({Report.MassTable.nonR(CMA())  })
  
  GL0<-reactive({ report.item.nonR("No", "No", "No", "Agly", input$nonR.DS.total )  })
  CMA0<-reactive({Calc.Mass.all.nonR(as.character(PRO()$HC[2]),as.character(PRO()$LC[2]), GL0(), input$mol.format, input$FP.ngly)  })
  r.table0<-reactive({Report.MassTable.nonR(CMA0())  }) 
  
  
  ## Below GL_R, CMA_R, r.table_R take care of the reduced reporting calc.##
  GL_R<-reactive(({report.item.R(input$r.HCnterm, input$r.LCnterm, input$r.HCcterm, input$ngly, input$R.DS.HC, input$R.DS.LC)  }))
  CMA_R<-reactive({Calc.Mass.all.R(as.character(PRO()$HC[2]), as.character(PRO()$LC[2]), GL_R(), input$mol.format, input$FP.ngly) })
  r.table_R<-reactive({lapply(CMA_R(),Report.MassTable.R)})
  
  
  GL_R0<-reactive(({report.item.R("No", "No", "No", "Agly", 0, 0)  }))
  CMA_R0<-reactive({Calc.Mass.all.R(as.character(PRO()$HC[2]), as.character(PRO()$LC[2]), GL_R0(), input$mol.format, input$FP.ngly) })
  r.table_R0<-reactive({lapply(CMA_R0(),Report.MassTable.R)})
  
  
  
  RC_HC<-reactive({ResCount(as.character(PRO()$HC[2]))})
  RC_LC<-reactive({ResCount(as.character(PRO()$LC[2]))})
  
  EC.poly<-reactive({EC.280(as.character(PRO()$HC[2]), as.character(PRO()$LC[2]), "No",0, 0)})
  EC<-reactive({EC.280(as.character(PRO()$HC[2]), as.character(PRO()$LC[2]), input$withADC,input$EC.ADC.MA, input$EC.ADC.n)})
  

  NAM<-reactive({as.character(input$mol.name)})
  your.name<-reactive({as.character(input$creator.name)})
  
  adc.note<-reactive({
    if(input$withADC=="No"){NULL
      }else{paste0(" and ", input$EC.ADC.n, " payloads, with molar absorptivity of each payload being ", input$EC.ADC.MA, " /(m cm)" ) }
    
  })

  output$Report<-downloadHandler(
    filename = function(){
      paste(paste("Report_", Sys.Date(), sep=""), "html", sep=".")
    },
    content = function(file){
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      ## Set up parameters to pass to Rmd document
      
      params <- list(a = r.table(),
                     b = r.table_R()$HC.fullR,
                     c = r.table_R()$HC.withDSB,
                     d = r.table_R()$LC.fullR,
                     e = r.table_R()$LC.withDSB,
                     f = report.HCseq(),
                     g = report.LCseq(),
                     h = RC_HC(),
                     i = RC_LC(),
                     j = as.numeric(as.character(PRO()$HC[1])),
                     k = as.numeric(as.character(PRO()$LC[1])),
                     l = format(EC(), scientific = FALSE),
                     m = NAM(),
                     o = paste0(your.name(), "    ", Sys.Date()),
                     p = input$mol.format,
                     q = n.glycan.site(),
                     A = r.table0(),
                     B = r.table_R0()$HC.fullR,
                     D = r.table_R0()$LC.fullR,
                     E = ECAA()$Counts[1],
                     H = ECAA()$Counts[2],
                     I = ECAA()$Counts[3],
                     M = adc.note()) 
                    
                     
                     
                     
      
      
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
      
    }
    
    
    
  )
  
  
  
  
  
  
  
  
  
  
  
}
)
