shinyUI(fluidPage(

    div(style="color:white", textOutput("login")),

    conditionalPanel("output.login == 0",
                     column(3, offset = 4,
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            
                            wellPanel(
                                
                                textInput("login", "Login:"),
                                passwordInput("pswd", "Password:"),
                                actionButton("btnLogin","Login")
                                
                            ) # wellPanel
                     ) # column
    ),
    
    conditionalPanel("output.login == 1",
    
    titlePanel("Leaflets Monitoring Report"),
    br(),
    wellPanel(
    
    fluidRow(
            
      column(3,
             selectInput("selectCategory", label = "Select Category:",
                         choices = sort(c("", unique(dataTL$SubCategory))) 
                        )
            ), #column 
        
      column(3,
        dateRangeInput('dateRange',
                       label = 'Date range:',
                       start = min(dataTL$Start), 
                       end = max(dataTL$End),
                       min = min(dataTL$Start), 
                       max = max(dataTL$End), 
                       format = "dd-mm-yyyy", 
                       startview = "month", 
                       weekstart = 0, 
                       language = "en", 
                       separator = " to ")
       ), #column
        
     column(3,
        selectInput("selectChannel", label = "Select Channel:",
                    choices = sort(c("", unique(dataTL$Channel))),
                    selected = c("Food", "Drug"),
                    multiple = TRUE)
      ), # column

        column(3,
        conditionalPanel("input.btnAdvanced % 2 == 1",
        selectInput("checkGroup", label = "Select Chain:",
                           choices = sort(c("", unique(dataTL$Retailer))),
                           selected = 1, multiple = TRUE)
            )) # column
       
       ), # fluidRow
    
    fixedRow(
        
        column(4,
        actionButton("btnBuild", "Build Report"),
        actionButton("btnAdvanced", "Advanced")
        ) # column
        
        ) # fixedRow
   
    ), # wellPanel 

    conditionalPanel("input.btnBuild > 0",
    
# 1st section of output

    h4("Total Number of Advertisement Blocks & Average Discount Over Time"),
    verbatimTextOutput("summary1"),
    plotOutput("plot1", hover = hoverOpts(id = "plot_hover")),
    br(),

# 2ns section of output    
 
    h4("Density of Promotional Campaigns"),
    verbatimTextOutput("summary2"),
    uiOutput("plot2ui"),
    br(),
    
# 3rd section of output

    h4("Brands' Performance Over the Selected Period"),
    DT::dataTableOutput("table"),
    br(),
    
# 4th section of output

    h4("List of the Promotional Campaigns for Selected Period"),
    DT::dataTableOutput("table2"),
    br()
    
    ) # conditionalPanel 
    )
))