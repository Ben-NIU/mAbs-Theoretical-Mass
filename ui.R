library(shinydashboard)
library(plotly)
dbHeader<- dashboardHeader(title=span("Ab-IntactMS", style="font-family:'eras bold itc';color:white; font-size:18pt"), titleWidth = 230,
     
                           tags$li(a(href = "https://www.astrazeneca.com/", img(src='AZ_white.png', title = "AstraZeneca Home", height = "30px"), style = "padding-top:10px; padding-bottom:10px;"), class = "dropdown")
                           )

dashboardPage(skin="purple",
dbHeader,
  dashboardSidebar(
    sidebarMenu(
      fileInput("input.file", label=span("FASTA input here", style="font-family:'forte'; font-size:12pt")),
      hr(),
 
      menuItem(strong(em(span("Whole Antibody", style="font-size:12pt; font-family: '3ds'"))), icon=icon("sitemap"), tabName = "nonRed"),
      menuItem(strong(em(span("Subunits (reduced)", style="font-size:12pt; font-family: '3ds'"))), icon=icon("cubes"), tabName = "Red"),
      menuItem(strong(em(span("Polypeptide Stats", style="font-size:12pt; font-family: '3ds'"))), icon=icon("paw"), tabName="poly"),
      menuItem(strong(em(span("Create Report", style="font-size:12pt; font-family: '3ds'"))), icon=icon("check"), tabName = "Report"),
      hr(),


      textOutput("termi"),
      tags$head(tags$style("#termi{font-family: 'forte'; font-size:12pt; text-indent: 1em;}")),
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
   
      box(title=span(strong("Sequence Display"), style="font-family:century gothic"), status="primary", solidHeader = TRUE, collapsible = TRUE, width=12,
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
      box(title=span(strong("Polypeptide Formula and Mass"), style="font-family:century gothic"), status="primary", solidHeader = TRUE, collapsible = TRUE, width=7,
         fluidRow(
           column(width=12,
            valueBoxOutput("tb2", width=8),
           
          valueBoxOutput("poly.mass.hc", width=4))),
         hr(),
         fluidRow(
           column(width=12,
            valueBoxOutput("tb1", width = 8),
           
          valueBoxOutput("poly.mass.lc", width=4))),
         hr(),
         textOutput("note02"),
         tags$head(tags$style("#note02{font-style: italic; color: #666666}"))
         
      ),
      box(title=span(strong("Extinction Coefficient (EC)"), style="font-family:century gothic"), status="primary", solidHeader = TRUE, collapsible = TRUE, width=5,
      fluidRow(
        column(width=6,
              plotlyOutput("EC.plot")
               ),
        column(width=6,
               br(),
               textOutput("note03"),
               hr(),
               textOutput("note04"),
               tags$head(tags$style("#note03{font-style: italic; color: #666666}")),
               tags$head(tags$style("#note04{font-style: italic; color: #666666}"))
               )
      )

      )),
    
    fluidRow(
      box(title=span(strong("Residue Composition"), style="font-family:century gothic"), status="primary", solidHeader = TRUE, collapsible = TRUE, width=12,
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
    column(width=8,
      fluidRow(     
      column(width=6,
             
      box(title=span("Disulfide bonds", style="font-family:century gothic"), status="primary", solidHeader = TRUE, collapsible = TRUE, width = "100%",
        fluidRow(
          column(width=12,
                 selectInput("IgG.type", label="Antibody Types", choices = list("IgG1", "IgG2", "IgG4", "Bispecifics", "Fusion protein"), selected = "IgG1", width="50%")
                 )
        ),  
        fluidRow(
          column(width=10,
          p("The inputs here are to determine", strong("the totality of disulfide bonds"), "within the whole antibody.", align="justify"))
        ),
        fluidRow(
          column(width=6,
                 uiOutput("hinge.ds")),
          column(width=6,
                 uiOutput("HC-HC.ds"))),
        
        fluidRow(
          column(width=6,
                 numericInput("LC-LC.ds", label="intra LC-LC", value=4, min=0, max=8, step=1, width="60%")),
          column(width=6,
                 numericInput("HC-LC.ds", label="inter HC-LC", value=2, min=0, max=4, step=1, width="60%")))
        )),
      
      column(width=6,
             box(title=span("Chemical Modficiations", style="font-family:century gothic"), status="primary", solidHeader = TRUE, collapsible = TRUE, width="100%",  
                 fluidRow(
                   column(width=6,
                          uiOutput("OtherMod")),
                   
                   column(width=6,
                          numericInput("Howmany", label=span("# per antibody?",style="font-size:10pt"), value=0, min=0, max=100, step=1, width = "70%")
                   )),
                 hr(),
                 fluidRow(
                   column(width=6,
                          radioButtons("c.ptm",span("Create", em(span("customized PTM?", style="color: red"))), choices = list("Yes","No"), inline = TRUE, selected = "No")
                   ),
                   column(width=6,
                          uiOutput("c.Howmany")
                   )),
                 fluidRow(
                   column(width=12,
                          uiOutput("cust.ptm")
                   )
                   
                 )
             )

             )))),
    
    fluidRow(

      column(width=4, 
             box(title=span("N-glycosylation", style="font-family:century gothic"), status="primary", solidHeader = TRUE, collapsible = TRUE, width="100%",
                 fluidRow(
                   column(width=10,
                          p("Default assumption is there are only, in total, 2 glycan sites for each antibody.")
                          )
                 ),
                 
                 fluidRow(
                   column(width=8,
                          selectInput("glyco", label="N-Glycan Status of both heavy chains", choices = list("Aglycosylated","Deglycosylated",'G0/G0', "G0/G0-GN","G0F/G0F", "G0/G1","G1/G1", "G1/G2","G1F/G1F","G0F/G1F","G2F/G2F", "G0-GN/G0-GN", "G1F/G2F", "G2/G2"),selected = "Aglycosylated", width="100%")
                   ) 
),
                  hr(),
               fluidRow(
                 column(width=11,
                        p('To add more glycans/sites, click "Add glycan" below, specify identity of each addtional glycan and number of sites to be added. Click "Remove" to remove any added sites/glycans.', align = "justify")
                        )
               ),
              fluidRow(
                column(width=10,
                       actionButton("add", "Add glycan"),
                       tags$div(id='placeholder'))
              )
               
             )),
      column(width=8,
             box(title=span(strong("Mass and Formula"), style="font-family:century gothic"), status="warning", solidHeader = TRUE, collapsible = TRUE,width="100%",
                 fluidRow(
                   valueBoxOutput("NR.mass", width=4),
                   valueBoxOutput("tb22", width = 6)))
             ))
   

  ),
    
    tabItem(tabName = "Red",
   fluidRow(
    column(width=10,
      fluidRow(      
      column(width=4,
             
       box(title=span("Disulfide bonds", style="font-family:century gothic"), status="primary", solidHeader = TRUE, collapsible = TRUE,width="100%",
           fluidRow(
          column(width=12,
                    p("There are ", strong("unreduced intra-chain disulfide bonds"), "within the heavy chain and light chain, respectively, even in a reducing environment. Inputs below define the # of such disulfide bonds of each chain.", align="justify"))
           ),
          fluidRow(
            column(width=10,
                 sliderInput("unR.hc", "Heavy chain",  min=0, max=10, value=3,step=1, width = "90%"))),
          fluidRow(
          
            column(width=10,
      
                 sliderInput("unR.lc", "Light chain", value=2, min=0, max=6, step=1, width = "90%"))),
          fluidRow(
            column(width=12,
                   p("For typical mAbs, the", strong('#'), "of unreduced intra-chain disulfide bonds is", strong("3 for heavy chain, and 2 for light chain;"), "other formats of antibody might have different # of unreduced intra-chain disulfide bonds.",align="justify"))
          )
          )
         ),
      
      column(width=8,
             box(title=span("Chemical Modficiations", style="font-family:century gothic"), status = "primary", solidHeader = TRUE, collapsible = TRUE, width="100%",
                 fluidRow(
                   column(width=4,
                          uiOutput("red.OtherMod")),
                   column(width=4,
                          selectInput("red.Where", label=span("on which chain?",style="font-size:10pt"), choices = list("HC", "LC"), selected = "HC", width="60%")),
                   column(width=4,
                          numericInput("red.Howmany", label=span("# per chain?",style="font-size:10pt"), value=0, min=0, max=100, step=1, width = "70%")
                          )
                   
                 ),
                 hr(),
                 fluidRow(
                   column(width=4,
                          radioButtons("red.c.ptm",span("Create", em(span("customized PTM?", style="color: red"))), choices = list("Yes","No"), inline = TRUE, selected = "No")
                   ),
                   column(width=4,
                        uiOutput("red.Where2")
                          ),
                   column(width=4,
                          uiOutput("red.Howmany2"))
                   
                   
                 ),
                 fluidRow(
                   column(width=10,
                          uiOutput("red.cust.fml"))
                 )
                 
             )
             
             )
       ))),
      fluidRow(
       column(width=10,
              fluidRow(
        column(width=4,
        box(title=span("N-glycosylation", style="font-family:century gothic"), status="primary", solidHeader = TRUE, collapsible = TRUE, width="100%",
            fluidRow(
              column(width=11,
                     p("Default assumption is, heavy chain has only 1 glycan site; light chain has none.")
              )
            ),
            
            fluidRow(
              column(width=12,
                     
                     selectInput("glyco.Red", label="N-Glycan Status of heavy chain", choices = list("Aglycosylated","Deglycosylated",'G0', "G0F", "G1", "G1F", "G2F","G0-GN", "G2","M5","M6"), selected = "Aglycosylated", width = "60%"))),
            hr(),
            fluidRow(
              column(width=11,
                     p('Click "Add glycan" to specify the identity and number of each additional glycans on heavy chain; click "Remove" to remove any added sites/glycans.', align = "justify")
              )
            ),
            fluidRow(
              column(width=12,
                     actionButton("add.red", "Add glycan"),
                     tags$div(id='placeholder2'))
            )
        )
        
      ),

      column(width=8,
       box(title=span(strong("Mass and Formula"), style="font-family:century gothic"), status="danger", solidHeader = TRUE, collapsible = TRUE, width="100%",
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
             textOutput("note"),
             tags$head(tags$style("#note{font-style: italic; color: #666666}"))
             ))
             
          )) )       
          
    ))),
    
    tabItem(tabName = "Report",
            fluidRow(
              column(width=12,
                     box(title=span("Interactive Report Generation", style="font-family:century gothic"), status = "primary", solidHeader = TRUE, collapsible = TRUE, width="100%",

                         fluidRow(
                           column(width=4,
                               textInput("mol.name", span("Step1: Input name/code of antibody", style="font-family:'Comic Sans MS'; color: #666666; font-size:10pt"), value="", placeholder = "for example: NISTmAb")
                                  
                                  ),
                           column(width=3,
                                textInput("creator.name", span("Step2: Input your name", style="font-family:'Comic Sans MS'; color: #666666; font-size:10pt"), value="")
                                  ),
                           column(width=3,
                                selectizeInput("mol.format", span("Step3: Select antibody format", style="font-family:'Comic Sans MS'; color: #666666; font-size:10pt"), choices = list("IgG1", "IgG2", "IgG4", "Bispecific", "Fusion protein"), selected = "IgG1", width = "70%" )  
                                  )
                         ),
                        
                         fluidRow(
                           column(width=9,
                                  box(title=strong(span("Step4: Define PTMs settings to be included", style="font-family:'Comic Sans MS'; color:#666666;font-size:11pt")), status = "primary", width="100%",
                                      column(width=3,
                                      box(title=span(strong("N-term pE"), style="font-size:10pt"), background = "purple", width="100%",
                                             fluidRow(
                                               column(width=12,
                                                     uiOutput("r.HCnterm") 
                                                      )),
                                             fluidRow(
                                               column(width=12,
                                                    uiOutput("r.LCnterm")
                                                    )  
                                                      ),
                                          hr(),
                                          p('NOTE: upon uploading of FASTA, software detects N-terminal residues on heavy and light chain automatically.')
                                               
                                             )
                                               
                                             ),
                                      column(width=3,
                                        box(title=span(strong("Heavy chain C-term K"), style="font-size:10pt"), background = "red", width = "100%",
                                            
                                          column(width=12,
                                                 uiOutput("r.HCcterm")),
                                                 hr(),
                                          p('NOTE: upon uploading of FASTA, software detects C-terminal K on heavy chain automatically.')
                                                 
                                                
                                            
                                            )     

                                             ),
                                      column(width=3,
                                          box(title=span(strong("Heavy chain glycosylation"), style="font-size:10pt"), background = "light-blue",width="100%",
                                          column(width=12,
                                          uiOutput("ngly")       
                                          )
                                         
                                              )   
                                             
                                             ),
                                      column(width=3,
                                             box(title=span(strong("Whole Ab disulfide bonds total"), style="font-size:10pt"), background="olive", width="100%",
                                                 fluidRow(
                                                   column(width=12,
                                                          uiOutput("nonR.DS.total")
                                                          ))),
                                                   hr(),
                                              box(title = span(strong("Unreduced intra-chain disulfide bonds in subunits when reduced"), style="font-size:10pt"), background="olive", width = "100%",
                                                  fluidRow(
                                                    column(width=6,
                                                           numericInput("R.DS.HC", "HC", value=3, min=0, max=50, step=1)
                                                
                                                  )),
                                                  fluidRow(
                                                    column(width=6,
                                                           numericInput("R.DS.LC", "LC", value=2, min=0, max=50, step=1)
                                                    
                                                  ))
   
                                                  ) 
                                                 )
                                      )),
                                      column(width=3,
                                             box(title=strong(span("Step5: (ADC Only) ADC-related EC", style="font-family:'Comic Sans MS'; color:#666666;font-size:11pt")), status = "primary", width="100%",
                                                fluidRow(
                                                  column(width=12,
                                                         box(title=span(strong("ADC payloads contribute to EC?"), style="font-size:10pt"), background="orange", width="100%",
      
                                                        fluidRow(column(width=12,
                                                                        radioButtons("withADC", NULL, choices = list("Yes","No"), selected = "No", inline = TRUE )
                                                        )),
                                                        
                                                        fluidRow(
                                                          column(width=10,
                                                                 uiOutput("EC.ADC.MA"))),
                                                        fluidRow(
                                                          column(width=6,
                                                                 uiOutput("EC.ADC.n")
                                                          )   
                                                          
                                                        ))
                                                    )
                                                 
                                                 )
)  
                                            
                                            
                                            )),
hr(),
              fluidRow(
                  column(width=9,
                         p(strong(span("Step6: Click!", style="font-family:'Comic Sans MS'; color:#666666;font-size:11pt"))),
                        downloadButton("Report", "Generate report")
              )
           
  
  ))
                                  
                                             
                                             
                                      )
                                      
                                                 )
                                             )
   
                                    
                                             
                                             )
                                  
                                  
                                  
                                  )
                           
                           
                           
                           
                           
                         )
                         
                         
                     
                     
              
              
            
            
            
            
            
            
            
            
            
            

  
    
    
  

          
          
          
          
          