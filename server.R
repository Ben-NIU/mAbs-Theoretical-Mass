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
library(OrgMassSpecR)
library(knitr)
library(kableExtra)

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
       valueBox(value=tags$p(paste(as.character(PRO()$HC[1]), "  amino acids", sep=""), style="font-size: 50%"), subtitle = tags$p("Heavy Chain", style="font-size: 120%"), icon=icon("angle-double-left"), color = "yellow")})
   })
  
   
   observeEvent(input$input.file, {
     output$lc.count<-renderValueBox({
       valueBox( value=tags$p(paste(as.character(PRO()$LC[1]), "  amino acids", sep=""),style="font-size: 50%"), subtitle = tags$p("Light Chain", style="font-size: 120%"), icon=icon("angle-left"), color = "light-blue")})
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
 
   

   
   output$Nterm.hc<-renderUI({
     if(substr(as.character(PRO()$HC[2]),1,1) %in% c("Q","E")){
       radioButtons("Nterm.hc", label="HC N-terminal PyroE", choices=list("Yes","No"), selected = "Yes", inline=TRUE)
     } else {
       NULL}
   })
   
   output$Cterm.hc<-renderUI({
     if(substr(as.character(PRO()$HC[2]), as.numeric(as.character(PRO()$HC[1]))-1, as.numeric(as.character(PRO()$HC[1]))) =="GK") {
       radioButtons("Cterm.hc", label="HC C-terminal Lys Loss", choices=list("Yes","No"), selected="Yes", inline=TRUE)
     } else{
       NULL}
   })
   
   output$Nterm.lc<-renderUI({
     if(substr(as.character(PRO()$LC[2]),1,1) %in% c("Q","E")){
       radioButtons("Nterm.lc", label="LC N-terminal PyroE", choices=list("Yes","No"), selected = "Yes", inline=TRUE)
     } else {
       NULL}
   })
   
   output$Others<-renderUI({
     if( is.null(input$Cterm.hc)){
   selectInput("Other", label="Other Mod.", choices = list("none","+1Lys", "+1Hex","+2Lys","+2Hex", "+1Lys/+1Hex", "+2Lys/+1Hex"), selected="none", width="100%") 
     } 
     else if(input$Cterm.hc == "Yes"){
       selectInput("Other", label="Other Mod.", choices = list("none","+1Lys", "+1Hex","+2Lys","+2Hex", "+1Lys/+1Hex", "+2Lys/+1Hex"), selected="none", width="100%") 
     } 
     else if(input$Cterm.hc == "No"){
       selectInput("Other", label="Other Mod.", choices = list("none", "+1Hex","+2Hex"), selected="none", width="100%") 
     } 
   })
   
   
   
   output$hinge.ds<-renderUI({
     if(input$IgG.type=="IgG1" | input$IgG.type=="IgG4"){
       numericInput("hinge.ds", label="Hinge", value=2, min=0, max=20, step=1, width="100%")
     }
     else if(input$IgG.type=="IgG2"){
       numericInput("hinge.ds", label="Hinge", value=4, min=0, max=20, step=1, width="100%")
     }
   })
   
  observeEvent(input$AORB,{ 
   output$OtherMod<-renderUI({
     if(input$AORB=="Customized"){
       
       textInput("OtherMod", label=span("customizd Mod.", style="font-size:10pt"), value = "", placeholder = "H2O")       
       
     } else {
       
       if(is.null(input$Cterm.hc)){
         selectInput("OtherMod", label=span("pre-defined Mod", style="font-size:10pt"), choices = list("none","+Lys", "+Glycation","Oxidation", "Water-Loss","NH3-Loss","Deamidation","Cysteinylation"), selected="none") 
       }
       else if(input$Cterm.hc == "Yes"){
         selectInput("OtherMod", label=span("pre-defined Mod", style="font-size:10pt"), choices = list("none","+Lys", "+Glycation","Oxidation", "Water-Loss","NH3-Loss","Deamidation","Cysteinylation"), selected="none") 
       } 
       else if(input$Cterm.hc == "No"){
         selectInput("OtherMod", label=span("pre-defined Mod", style="font-size:10pt"), choices = list("none", "+Glycation","Oxidation", "Water-Loss","NH3-Loss","Deamidation","Cysteinylation"), selected="none") 
       }
       
     }
     
   })
  })
   
  
  
  observeEvent(input$red.AORB,{ 
    output$red.OtherMod<-renderUI({
      if(input$red.AORB=="Customized"){
        
        textInput("red.OtherMod", label=span("customizd Mod.", style="font-size:10pt"), value = "", placeholder = "H2O")       
        
      } else {
        
        if(is.null(input$Cterm.hc)){
          selectInput("red.OtherMod", label=span("pre-defined Mod", style="font-size:10pt"), choices = list("none","+Lys", "+Glycation","Oxidation", "Water-Loss","NH3-Loss","Deamidation", "Cysteinylation"), selected="none") 
        }
        else if(input$Cterm.hc == "Yes"){
          selectInput("red.OtherMod", label=span("pre-defined Mod", style="font-size:10pt"), choices = list("none","+Lys", "+Glycation","Oxidation", "Water-Loss","NH3-Loss","Deamidation","Cysteinylation"), selected="none") 
        } 
        else if(input$Cterm.hc == "No"){
          selectInput("red.OtherMod", label=span("pre-defined Mod", style="font-size:10pt"), choices = list("none", "+Glycation","Oxidation", "Water-Loss","NH3-Loss","Deamidation","Cysteinylation"), selected="none") 
        }
        
      }
      
    })
  })
   
   
   
  Tables<-reactive({ show.table.nonR(PRO(), input$Nterm.hc, input$Nterm.lc, input$Cterm.hc, input$glyco,input$AORB,input$OtherMod, input$Howmany,input$hinge.ds, input$`HC-LC.ds`,input$`HC-HC.ds`, input$`LC-LC.ds`, input$unR.hc, input$unR.lc) })
  
  Tables.Red<-reactive({ show.table.Red(PRO(), input$Nterm.hc, input$Nterm.lc, input$Cterm.hc, input$glyco.Red, input$red.AORB,input$red.OtherMod,input$red.Howmany, input$red.Where, input$unR.hc, input$unR.lc)})
  
  observeEvent(input$input.file, {
    output$poly.mass.lc<-renderValueBox({
      valueBox( value=tags$p(paste(Tables()$LC.ms, "  Da", sep=""), style="font-size: 70%"), subtitle = tags$p("Light Chain", style="font-size: 130%"), icon=icon("star-half-empty"), color = "light-blue")})
  })

  observeEvent(input$input.file,{
    output$poly.mass.hc<-renderValueBox({
      valueBox( value=tags$p(paste(Tables()$HC.ms, "  Da", sep=""), style="font-size: 70%"), subtitle = tags$p("Heavy Chain", style="font-size: 130%"), icon=icon("star-half-o"), color = "yellow")})
  })

  output$tb1<-renderTable({
    Tables()$LC.fml}, caption=NULL,digits=0, align="c")
  
  output$tb2<-renderTable({
    Tables()$HC.fml}, caption=NULL,digits=0, align="c")

  output$tb22<-renderTable({
    Tables()$mAb.fml}, caption=NULL, digits=0, align="c")
  
  observeEvent(input$input.file,{
    output$NR.mass<-renderValueBox({
      valueBox( value=tags$p(paste(Tables()$mAb.ms, "  Da", sep=""), style="font-size: 70%"), subtitle = tags$p("Whole mAb", style="font-size: 130%"), icon=icon("paper-plane-o"), color = "purple")})
  })
  
  
  observeEvent(input$input.file, {
    output$tbhc<-function(){
      knitr::kable(Tables.Red()$HC, format="html",caption = "Heavy Chain", align = "c") %>% kable_styling(bootstrap_options = c("striped", "condensed", "responsive", "hover")) %>% row_spec(c(1), bold = T,color="white",background = "#F77807")
    }
  })
  
  observeEvent(input$input.file, {
    output$tblc<-function(){
      knitr::kable(Tables.Red()$LC, format="html",caption = "Light Chain", align = "c") %>% kable_styling(bootstrap_options = c("striped", "condensed", "responsive", "hover")) %>% row_spec(c(1), bold = T,color="white",background = "#3088BE")
    }
  })
  


  observeEvent(input$input.file,{
    output$note<-renderText('The "Theoretical" composition and mass assume all disulfide bonds have been reduced; whereas the "Expected" composition and mass correspond to Heavy Chain and Light Chain with un-reduced intra-chain disulfide bonds; typically, 3 un-reduced disulfide bonds for heavy chain and 2 for light chain.')
  })
    
  output$instruction<-renderText("The inputs here are to determine the totality of disulfide bonds within the whole molecule (i.e., for typical mAbs 2 heavy chain and 2 light chain together). Hinge region is where the two half-antibody are connected. Typically for IgG1 and IgG4, there are 2 disulfide bonds; and for IgG2, there are 4 disulfide bonds.")
  
## ========================================= Reporting-associated codes==============================  
  
  
  output$r.HCnterm<-renderUI({
    if(substr(as.character(PRO()$HC[2]),1,1) %in% c("Q","E")){
      checkboxGroupInput("r.HCnterm", label="HC pE", choices=list("Yes","No"), selected = NULL, inline=FALSE)  } else {
      NULL}
  })
  
  output$r.LCnterm<-renderUI({
    if(substr(as.character(PRO()$LC[2]),1,1) %in% c("Q","E")){
      checkboxGroupInput("r.LCnterm", label="LC pE", choices=list("Yes","No"), selected = NULL, inline=FALSE)      } else {
      NULL}
  })
  
  
  
  output$r.HCcterm<-renderUI({
    if(substr(as.character(PRO()$HC[2]), as.numeric(as.character(PRO()$HC[1]))-1, as.numeric(as.character(PRO()$HC[1]))) =="GK") {
      checkboxGroupInput("r.HCcterm", label=NULL, choices=list("cleaved"="Yes","attached"="No"), selected = NULL, inline = FALSE)  } else{
      NULL}
  })

  
  
  output$EC.ADC.MA<-renderUI({
    if(input$withADC == "Yes"){
      numericInput("EC.ADC.MA", label="Payload molar absorptivity", value=0, min=0, max=100000, step=1)  } else {
        NULL}
  })
  
  output$EC.ADC.n<-renderUI({
    if(input$withADC == "Yes"){
      numericInput("EC.ADC.n", label="#. payload?", value=0, min=0, max=50, step=1)  } else {
        NULL}
  })

  ## A function to add one space in the peptide sequence for every y residues.
  
  Addspace<-function(x, y){
    aa<-strsplit(as.character(x), split = "")[[1]]
    l.aa<-length(aa)
    sec<-l.aa %/% y
    if(l.aa<=y){
      return(as.character(x))
    } else {  
      
      S<-NULL
      for(i in 0:sec){
        
        AA<-aa[(1+(i*y)):(y+(i*y))]
        AA<-AA[!is.na(AA)]
        AA[length(AA)+1]<-" "
        S0<-paste(AA, collapse = "")
        S<-c(S,S0)
        
      }
      return(paste(S, collapse = ""))
      
    }
  }
  
  ## Below GL, CMA, and r.table take care of the non-reduced reporting calc.##
  ## A function to add one space in the peptide sequence for every y residues.
  
  report.HCseq<-reactive({Addspace(as.character(PRO()$HC[2]),10)})  
  report.LCseq<-reactive({Addspace(as.character(PRO()$LC[2]),10)})  
  ## add one space every 10 amino acids, so the LaTex can wrap the long sequence.
  
  GL<-reactive({ report.item.nonR(input$r.HCnterm, input$r.LCnterm, input$r.HCcterm, input$ngly, input$nonR.DS.total )  })
  CMA<-reactive({Calc.Mass.all.nonR(as.character(PRO()$HC[2]),as.character(PRO()$LC[2]), GL())  })
  r.table<-reactive({Report.MassTable.nonR(CMA())  })
  
  ## Below GL_R, CMA_R, r.table_R take care of the reduced reporting calc.##
  GL_R<-reactive(({report.item.R(input$r.HCnterm, input$r.LCnterm, input$r.HCcterm, input$ngly, input$R.DS.HC, input$R.DS.LC)  }))
  CMA_R<-reactive({Calc.Mass.all.R(as.character(PRO()$HC[2]), as.character(PRO()$LC[2]), GL_R()) })
  r.table_R<-reactive({lapply(CMA_R(),Report.MassTable.R)})
  
  
  RC_HC<-reactive({ResCount(as.character(PRO()$HC[2]))})
  RC_LC<-reactive({ResCount(as.character(PRO()$LC[2]))})
  
  
  EC<-reactive({EC.280(as.character(PRO()$HC[2]), as.character(PRO()$LC[2]), input$withADC,input$EC.ADC.MA, input$EC.ADC.n)})
  
  
  W_count<-reactive({sum(strsplit(as.character(PRO()$HC[2]), split = "")[[1]] %in% "W")*2 + sum(strsplit(as.character(PRO()$LC[2]), split = "")[[1]] %in% "W")*2})
  Y_count<-reactive({sum(strsplit(as.character(PRO()$HC[2]), split = "")[[1]] %in% "Y")*2 + sum(strsplit(as.character(PRO()$LC[2]), split = "")[[1]] %in% "Y")*2})
  C_count<-reactive({sum(strsplit(as.character(PRO()$HC[2]), split = "")[[1]] %in% "C") + sum(strsplit(as.character(PRO()$LC[2]), split = "")[[1]] %in% "C")})
  
  observeEvent(input$input.file,{
   output$fluoroAA<-renderText(paste("Trp:",W_count()," ; Tyr:", Y_count(), " ; Cystines:", C_count(), sep=""))
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
                     l = format(EC(), scientific = FALSE))
                    
                     
                     
                     
      
      
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
      
    }
    
    
    
  )
  
  
  
  
  
  
  
  
  
  
  
}
)
