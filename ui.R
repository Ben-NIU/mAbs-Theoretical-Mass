library(shinydashboard)

dashboardPage(skin="purple",
  dashboardHeader(title="mAbs Intact Mass Calculator", titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      fileInput("input.file", label=span("Input FASTA here", style="font-family:'calibri'; font-size:12pt")),
      hr(),
      menuItem("Polypeptide", icon=icon("paw"), tabName="poly"),
      menuItem("non-Reduced MS", icon=icon("sitemap"), tabName = "nonRed"),
      menuItem("Reduced MS", icon=icon("cubes"), tabName = "Red"),
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
            tableOutput("tb1")),
           column(width=4, 
          valueBoxOutput("poly.mass.lc", width=10))),
         hr(),
         fluidRow(
           column(width=4,
             tableOutput("tb2")),
           column(width=4,
          valueBoxOutput("poly.mass.hc", width=10))),
         hr(),
         fluidRow(
           column(width=4,
              tableOutput("tb3")),
           column(width=4,
            valueBoxOutput("poly.mass.mAb", width=10)))
      ))
      ),
    
    tabItem(tabName = "nonRed",
    
    fluidRow(          
      column(width=5,
      box(title="Disulfide Bonds Status of Monomer", status="primary", solidHeader = TRUE, collapsible = TRUE, width = "100%",
        fluidRow(
          column(width=12,
          verbatimTextOutput("instruction"))
        ),
        fluidRow(
          column(width=6,
                 selectInput("hinge.ds", label="Hinge", choices=c(0,1,2,3,4), selected=2, width="100%")),
          column(width=6,
                 numericInput("HC-HC.ds", label="intra HC-HC", value=4, min=0, max=15, step=1,width="100%"))),
        fluidRow(
          column(width=6,
                 selectInput("LC-LC.ds", label="intra LC-LC", choices=c(0,1,2,3), selected=2, width="100%")),
          column(width=6,
                 selectInput("HC-LC.ds", label="inter HC-LC", choices=c(0,1,2,3), selected=1, width="100%"))),
        fluidRow(
          column(width=6,
          selectInput("glyco", label="N-Glycan", choices = list("none","Deglycosylated",'G0/G0', "G0F/G0F", "G1/G1", "G1F/G1F","G0F/G1F","G2F/G2F", "G0-GN/G0-GN", "G1F/G2F"), selected = "G0", width="100%"))
          
        ))),
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
             
       box(title="# of UnReduced intra-chain Disuldife bonds", status="primary", solidHeader = TRUE, collapsible = TRUE,width="100%",
          fluidRow(
            column(width=12,
                 numericInput("unR.hc", "Heavy chain", value=3, min=0, max=6, step=1))),
          fluidRow(
            column(width=12,
      
                 numericInput("unR.lc", "Light chain", value=2, min=0, max=4, step=1))),
            fluidRow(
            column(width=12,
     
                 selectInput("glyco.Red", label="N-Glycan", choices = list("none","Deglycosylated",'G0', "G0F", "G1", "G1F", "G2F","G0-GN"), selected = "G0")))
             )),
      
      column(width=8,
       box(title="Formula & Mass", status="primary", solidHeader = TRUE, collapsible = TRUE, width="100%",
            fluidRow(
              column(width=8,
              tableOutput("tblc"))
              ),       
            fluidRow(
              column(width=8,
              tableOutput("tbhc"))
              ),
            fluidRow(
              column(width=12,
             verbatimTextOutput("note")))
             
          )) )       
          
    )
  
    
    
  )
)
)
          
          
          
          
          