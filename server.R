source("read.fasta.R")
source("show.table.nonR.R")
source("show.table.Red.R")
library(OrgMassSpecR)

shinyServer(function(input, output) {
  
  PRO<-reactive({ 
    validate(
      need(input$input.file$datapath !="", "")
    )
    read.fasta(input$input.file[['datapath']])
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
   
   output$Nterm.hc<-renderUI({
     if(substr(as.character(PRO()$HC[2]),1,1) %in% c("Q","E")){
       radioButtons("Nterm.hc", label="N-terminal PyroE", choices=list("Yes","No"), selected = "Yes", inline=TRUE)
     } else {
       NULL}
   })
   
   output$Cterm.hc<-renderUI({
     if(substr(as.character(PRO()$HC[2]), as.numeric(as.character(PRO()$HC[1]))-5, as.numeric(as.character(PRO()$HC[1]))) =="SLSPGK") {
       radioButtons("Cterm.hc", label="C-terminal Lys Loss", choices=list("Yes","No"), selected="Yes", inline=TRUE)
     } else{
       NULL}
   })
   
   output$Nterm.lc<-renderUI({
     if(substr(as.character(PRO()$LC[2]),1,1) %in% c("Q","E")){
       radioButtons("Nterm.lc", label="N-terminal PyroE", choices=list("Yes","No"), selected = "Yes", inline=TRUE)
     } else {
       NULL}
   })
   
   
   
   
  Tables<-reactive({ show.table.nonR(PRO(), input$Nterm.hc, input$Nterm.lc, input$Cterm.hc, input$glyco, input$hinge.ds, input$`HC-LC.ds`,input$`HC-HC.ds`, input$`LC-LC.ds`, input$unR.hc, input$unR.lc) })
  
  Tables.Red<-reactive({ show.table.Red(PRO(), input$Nterm.hc, input$Nterm.lc, input$Cterm.hc, input$glyco.Red, input$unR.hc, input$unR.lc)})
  
  observeEvent(input$input.file, {
    output$poly.mass.lc<-renderValueBox({
      valueBox( value=tags$p(paste(Tables()$LC$'Mass (Da)', "  Da", sep=""), style="font-size: 70%"), subtitle = tags$p("Light Chain", style="font-size: 130%"), icon=icon("star-half-empty"), color = "green")})
  })

  observeEvent(input$input.file,{
    output$poly.mass.hc<-renderValueBox({
      valueBox( value=tags$p(paste(Tables()$HC$'Mass (Da)', "  Da", sep=""), style="font-size: 70%"), subtitle = tags$p("Heavy Chain", style="font-size: 130%"), icon=icon("star-half-o"), color = "maroon")})
  })
  
  observeEvent(input$input.file,{
    output$poly.mass.mAb<-renderValueBox({
      valueBox( value=tags$p(paste(Tables()$mAb$'Mass (Da)'[2], "  Da", sep=""), style="font-size: 70%"), subtitle = tags$p("Whole mAb", style="font-size: 130%"), icon=icon("paper-plane-o"), color = "purple")})
  })
  
  output$tb1<-renderTable({
    Tables()$LC[,1:5]}, caption=NULL,digits=0, align="c")
  
  output$tb2<-renderTable({
    Tables()$HC[,1:5]}, caption=NULL,digits=0, align="c")
  
  output$tb3<-renderTable({
    Tables()$mAb[,1:5][2,]}, caption=NULL,digits=0, align="c")
    
  output$tb22<-renderTable({
    Tables()$mAb[,1:5][1,]}, caption=NULL, digits=0, align="c")
  
  observeEvent(input$input.file,{
    output$NR.mass<-renderValueBox({
      valueBox( value=tags$p(paste(Tables()$mAb$'Mass (Da)'[1], "  Da", sep=""), style="font-size: 70%"), subtitle = tags$p("Whole mAb", style="font-size: 130%"), icon=icon("paper-plane-o"), color = "purple")})
  })
  
  output$tblc<-renderTable({
    Tables.Red()$LC}, caption="Light Chain", caption.placement = getOption("xtable.caption.placement", "top"), digits = 1,align="c")
  output$tbhc<-renderTable({
    Tables.Red()$HC}, caption="Heavy Chain", caption.placement = getOption("xtable.caption.placement", "top"), digits = 1, align="c")

  observeEvent(input$input.file,{
    output$note<-renderText('The "Theoretical" composition and mass assume all disulfide bonds have been reduced; whereas the "Expected" composition and mass correspond to Heavy Chain with 3 un-reduced intra-disulfide bonds, and Light Chain with 2 un-reduced intra-disulfide bond.')
  })
    
  
}
)
