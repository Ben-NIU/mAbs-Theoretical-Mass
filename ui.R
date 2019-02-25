library(shinydashboard)

dbHeader<- dashboardHeader(title=span("mAbs Intact Mass Calculator", style="font-family:'rockwell extra bold';color:white; font-size:14pt"), titleWidth = 400,
     
                           tags$li(a(href = "https://www.astrazeneca.com/", img(src='AZ_white.png', title = "AstraZeneca Home", height = "30px"), style = "padding-top:10px; padding-bottom:10px;"), class = "dropdown")
                           )

dashboardPage(skin="purple",
dbHeader,
  dashboardSidebar(
    sidebarMenu(
      fileInput("input.file", label=span("Input FASTA here", style="font-family:'calibri'; font-size:12pt")),
      hr(),
 
      menuItem("non-Reduced MS", icon=icon("sitemap"), tabName = "nonRed"),
      menuItem("Reduced MS", icon=icon("cubes"), tabName = "Red"),
      menuItem("Polypeptide", icon=icon("paw"), tabName="poly"),
      menuItem("Reporting", icon=icon("check"), tabName = "Report"),
      hr(),
      
        uiOutput("Nterm.hc"),
        uiOutput("Cterm.hc"),
      br(),
        uiOutput("Nterm.lc")
     
      
      )
    ),
    
  
  dashboardBody(
    tags$style(HTML("

                              
                              .box.box-solid.box-primary>.box-header {
                              color:#fff;
                              background:#666666
                              }
                              
                              .box.box-solid.box-primary{
                              border-bottom-color:#666666;
                              border-left-color:#666666;
                              border-right-color:#666666;
                              border-top-color:#666666;
                              }
                              
                              ")),
    
    tabItems(
      tabItem(tabName = "poly",
      
    fluidRow(
   
      box(title="Show Sequences", status="primary", solidHeader = TRUE, collapsible = TRUE, width=12,
          fluidRow(
            column(width=9,
            p(verbatimTextOutput("hc.seq"))),
            column(width=3,
            fluidRow(
              br(),
            valueBoxOutput("hc.count", width=12)))),
          fluidRow(
            column(width=9,
            p(verbatimTextOutput("lc.seq"))),
            column(width=3,
             fluidRow(
               br(),
            valueBoxOutput("lc.count", width=12))))
          
      )),
  
    
    fluidRow(
      box(title="Polypeptide Stats", status="primary", solidHeader = TRUE, collapsible = TRUE, width=9,
         fluidRow(
           column(width=4,
            tableOutput("tb2")),
           column(width=4, 
          valueBoxOutput("poly.mass.hc", width=10))),
         hr(),
         fluidRow(
           column(width=4,
             tableOutput("tb1")),
           column(width=4,
          valueBoxOutput("poly.mass.lc", width=10))),
         hr()
         
      )),
    fluidRow(
      box(title="Residue Count", status="primary", solidHeader = TRUE, collapsible = TRUE, width=12,
          fluidRow(
            column(width=12,
                   tableOutput("Hc.count"))),
          fluidRow(
            column(width=12,
                   tableOutput("Lc.count"))
            
          )
          )
      
      
    )
      ),
    
    tabItem(tabName = "nonRed",
    
    fluidRow(          
      column(width=5,
      box(title="Disulfide Bonds", status="primary", solidHeader = TRUE, collapsible = TRUE, width = "100%",
        fluidRow(
          column(width=12,
                 selectInput("IgG.type", label="IgG Types", choices = list("IgG1", "IgG2", "IgG4"), selected = "IgG1", width="50%")
                 )
        ),  
        fluidRow(
          column(width=12,
          verbatimTextOutput("instruction"))
        ),
        fluidRow(
          column(width=6,
                 uiOutput("hinge.ds")),
          column(width=6,
                 numericInput("HC-HC.ds", label="intra HC-HC", value=8, min=0, max=15, step=1,width="100%"))),
        
        fluidRow(
          column(width=6,
                 numericInput("LC-LC.ds", label="intra LC-LC", value=4, min=0, max=8, step=1, width="100%")),
          column(width=6,
                 numericInput("HC-LC.ds", label="inter HC-LC", value=2, min=0, max=4, step=1, width="100%")))
        ),
      
      box(title="N-Glycans  &  Others", status="primary", solidHeader = TRUE, collapsible = TRUE, width="100%",
        fluidRow(
          column(width=6,
          selectInput("glyco", label="N-Glycan Status of both HC", choices = list("Aglycosylated","Deglycosylated",'G0/G0', "G0/G0-GN","G0F/G0F", "G0/G1","G1/G1", "G1/G2","G1F/G1F","G0F/G1F","G2F/G2F", "G0-GN/G0-GN", "G1F/G2F", "G2/G2"), selected = "Aglycosylated", width="100%")),
          column(width=6,
                 fluidRow(
                   column(width=12,
                          radioButtons("AORB", label="Others", choices = list("Pre-defined", "Customized"), selected = "Pre-defined"))
                   
                 ),           
                 fluidRow(
                   column(width=12,
                          uiOutput("OtherMod")
                   )),
             
                 fluidRow(
                   column(width=12,
                          numericInput("Howmany", label=span("how many per mAb?",style="font-size:10pt"), value=0, min=0, max=100, step=1, width = "70%"))
                   
                 )
                 
                 
                 
                       
                 )
          )
      )),
        
      column(width=7,
      box(title="Formula & Mass", status="primary", solidHeader = TRUE, collapsible = TRUE,width="100%",
          fluidRow(
            column(width=12,
            tableOutput("tb22"))),
          fluidRow(
            column(width=12,
            valueBoxOutput("NR.mass")))
      )))),
    
    tabItem(tabName = "Red",
      fluidRow(      
      column(width=4,
             
       box(title="UnReduced intra-chain Disuldife bonds", status="primary", solidHeader = TRUE, collapsible = TRUE,width="100%",
          fluidRow(
            column(width=12,
                 numericInput("unR.hc", "Heavy chain", value=3, min=0, max=6, step=1))),
          fluidRow(
            column(width=12,
      
                 numericInput("unR.lc", "Light chain", value=2, min=0, max=4, step=1)))
          ),
       box(title="N-Glycans", status="primary", solidHeader = TRUE, collapsible = TRUE, width="100%",
            fluidRow(
            column(width=12,
     
                 selectInput("glyco.Red", label="N-Glycan Status of 1 HC", choices = list("Aglycosylated","Deglycosylated",'G0', "G0F", "G1", "G1F", "G2F","G0-GN", "G2","M5","M6"), selected = "Aglycosylated")))
             ),
       box(title="Other PTMs", status = "primary", solidHeader = TRUE, collapsible = TRUE, width="100%",
           fluidRow(
             column(width=6,
                    radioButtons("red.AORB", label=span("Others", style="font-size:1-pt"), choices = list("Pre-defined", "Customized"), selected = "Pre-defined")),
             column(width=6,
                    selectInput("red.Where", label=span("which chain?",style="font-size:10pt"), choices = list("HC", "LC"), selected = "HC", width="50%"))
             
           ),
           hr(),
           fluidRow(
             column(width=6,
                    uiOutput("red.OtherMod")
             ),
             column(width=6,
                  numericInput("red.Howmany", label=span("how many per Chain?",style="font-size:10pt"), value=0, min=0, max=100, step=1, width = "70%"))
                    

           )
           
           )
       
       
       
       
       ),
      
      column(width=8,
       box(title="Formula & Mass", status="primary", solidHeader = TRUE, collapsible = TRUE, width="100%",
            fluidRow(
              column(width=8,
              tableOutput("tbhc"))
              ),       
            fluidRow(
              column(width=8,
              tableOutput("tblc"))
              ),
            fluidRow(
              column(width=12,
             verbatimTextOutput("note")))
             
          )) )       
          
    ),
    
    tabItem(tabName = "Report",
            fluidRow(
              column(width=12,
                     box(title="Interactive Report generation", status = "primary", solidHeader = TRUE, collapsible = TRUE, width="100%",
                         fluidRow(
                           column(width=12,
                                  downloadButton("Report", "Generate report")
                           ) 
                           
                         ),
                  
                         hr(),
                         
                         fluidRow(
                           column(width=12,
                                  box(title=span("Items included in Masses/Compositions", style="font-family:'Comic Sans MS'; color:black;font-size:12pt"), status = "primary", width="100%",
                                      column(width=2,
                                      box(title=span(strong("N-term Cyclization"), style="font-size:10pt"), background = "teal", width="100%",
                                             fluidRow(
                                               column(width=12,
                                                     uiOutput("r.HCnterm") 
                                                      )),
                                             fluidRow(
                                               column(width=12,
                                                    uiOutput("r.LCnterm")
                                                    )  
                                                      )
                                               
                                             )
                                               
                                               
                                               
                                             ),
                                      column(width=2,
                                        box(title=span(strong("HC C-term Lys"), style="font-size:10pt"), background = "red", width = "100%",
                                            
                                          column(width=12,
                                                 uiOutput("r.HCcterm")
                                                 
                                                 )  
                                            
                                            )     

                                             ),
                                      column(width=2,
                                          box(title=span(strong("HC N-glycan"), style="font-size:10pt"), background = "light-blue",width="100%",
                                          column(width=12,
                                                 checkboxGroupInput("ngly",NULL, choices = list("Agly","Degly",'G0/G0-GN', "G0/G0","G0F/G0F", "G0/G1", "G1/G1", "G1/G2","G0F/G1F", "G1F/G1F","G2F/G2F","G1F/G2F" ,"G0-GN/G0-GN", "G2/G2"), selected = "Agly")
                                                 )    
                                              
                                              
                                              )   
                                             
                                             ),
                                      column(width=3,
                                             box(title=span(strong("Disulfide bonds (DSB)-nonReduced form"), style="font-size:10pt"), background="olive", width="100%",
                                                 fluidRow(
                                                   column(width=12,
                                                          numericInput("nonR.DS.total", "total# of DSB", value=16, min=0, max=100, step=1)
                                                          ))),
                                                   hr(),
                                              box(title = span(strong("Unreduced Disulfide bonds (DSB) in Reduced form"), style="font-size:10pt"), background="olive", width = "100%",
                                                  fluidRow(
                                                    column(width=6,
                                                           numericInput("R.DS.HC", "HC", value=3, min=0, max=50, step=1)
                                                
                                                  )),
                                                  fluidRow(
                                                    column(width=6,
                                                           numericInput("R.DS.LC", "LC", value=2, min=0, max=50, step=1)
                                                    
                                                  ))
                                                   
                                                    
                                                  ) 
                                                   
                                                 ),
                                      column(width=3,
                                        box(title=span(strong("Extinction Coefficient (EC) Input"), style="font-size:10pt"), background = "maroon", width="100%", 
                                        fluidRow(column(width=12,
                                                    verbatimTextOutput("fluoroAA")    
                                                        ))    
                                        ),
                                        
                                        hr(),
                                        box(title = span(strong("Payload-associated EC properties"), style="font-size:10pt"), background = "maroon", width = "100%",
                                            
                                        fluidRow(column(width=12,
                                                      radioButtons("withADC", "Payload affects EC?", choices = list("Yes","No"), selected = "No", inline = TRUE )
                                                      )),
                                                  
                                         fluidRow(
                                            column(width=10,
                                                   uiOutput("EC.ADC.MA"))),
                                         fluidRow(
                                           column(width=6,
                                                  uiOutput("EC.ADC.n")
                                         )   
                                            
                                          )))  
                                            
                                            
                                            )      
                                             
                                             
                                      )
                                      
                                                 )
                                             )
   
                                    
                                             
                                             )
                                  
                                  
                                  
                                  )
                           
                           
                           
                           
                           
                         )
                         
                         )
                     
                     )
              
              
            )
            
            
            
            
            
            
            
            
            

  
    
    
  

          
          
          
          
          