shinyServer(function(input, output, session) {

# Data loading and transformations    

    DF = eventReactive(input$btnBuild, {
        
    validate(
          need(input$selectCategory != "", "Help message: Please select a category you want to analyse."),
          need(difftime(input$dateRange[2], input$dateRange[1]) >= 0 ||
                   !is.null(input$dateRange[2]), 
                "Help message: Start-date cannot be later than end-date. Please select correct date range"),
            need(input$selectChannel != "", "Help message: Please select a channel you want to analyse. Multiple channels are allowed.")
        )
        
        dataSubset = 
            
            if(input$btnBuild == 0 || is.null(input$btnBuild)){return()}
                input$btnBuild
                isolate({
                    if ((input$dateRange[2] >= input$dateRange[1]) &
                        (input$selectCategory !="") &
                        (input$selectChannel != "")) {
            
                  
            if (!is.null(input$checkGroup) & input$btnAdvanced %% 2) {
                                    
            dataTL[SubCategory == input$selectCategory &  
                       input$dateRange[1] <= End &
                       input$dateRange[2] >= Start &
                       Channel %in% input$selectChannel &
                      Retailer %in% input$checkGroup
                       , , drop = FALSE]
            } # if
                        else {
                            dataTL[SubCategory == input$selectCategory &  
                                       input$dateRange[1] <= End &
                                       input$dateRange[2] >= Start &
                                       Channel %in% input$selectChannel
                                   , , drop = FALSE]
                        }
                    }
                })
        })

# 1st plot
    
     output$plot1 = renderPlot({
         if(input$btnBuild == 0 || is.null(input$btnBuild)){return()}
         input$btnBuild
         isolate({
             
         validate(
             need(difftime(input$dateRange[2], input$dateRange[1]) >= 0 &
                      !is.null(input$dateRange[2]), 
                  "Chart1 help message: Start-date cannot be later than end-date. Please select correct date range."),
             need(nrow(DF())>0, "Chart1 help message: No data to buid report on. Please try other options.")
             ) 
         
        chartDates = seq(input$dateRange[1], input$dateRange[2], by="1 day")
        
        a1 = outer(DF()$Start, chartDates, function(x,y) x<=y)
        a2 = outer(DF()$End, chartDates, function(x,y) x>=y)
        a3=a1*a2
        a4 <<-colSums(a3)
        a5 = a3*DF()$Discount
        a5[a5==0] = NA
        a6 = colMeans(a5, na.rm = TRUE)
        a6[a6 == "NaN"] = 0
        
        chartDates = as.data.frame(chartDates, origin = "1970-01-01")
        chartDates <<- cbind(chartDates, a4, a6)
       
        ggplot(data = chartDates) + 
            geom_area(aes(chartDates, a4), fill = "#2154EB", alpha = 0.6) + 
            geom_area(aes(chartDates, a6), fill = "grey", alpha = 0.75) +
            scale_x_date(date_breaks = "1 week",
                     labels = date_format("%d %m %y"),
                     limits = c(input$dateRange[1], input$dateRange[2]),
                     expand = c(0,0)) + 
            scale_y_continuous(breaks = pretty_breaks()) +
            labs(x = "Date", y = "Ads (blue) & Discount (grey)") + 
            theme(plot.title = element_text(size=20, hjust=0)) +
            theme(axis.title = element_text(size=12, face = "bold", hjust = 1)) +
            theme(panel.background = element_rect(fill = NA),
                  panel.border = element_rect(fill = NA, linetype = "dotted", colour = "grey"),
                  panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
                  panel.grid.minor = element_blank(),
                  axis.text.x = element_text(angle = 90, hjust = 1),
                  axis.text = element_text(size = 12))
         })
    })
    
# 2nd plot
     
     output$plot2ui <- renderUI({
         
         if(input$btnBuild == 0 || is.null(input$btnBuild)){return()}
         input$btnBuild
         isolate({
             
             validate(
                 need(difftime(input$dateRange[2], input$dateRange[1]) >= 0 &
                          !is.null(input$dateRange[2]), 
                      "Chart2 help message: Start-date cannot be later than end-date. Please select correct date range."),
                 need(nrow(DF())>0, "Chart2 help message: No data to buid report on. Please try other options.")
             )
             
         plotOutput("plot2", height = paste0((20 + 180*(length(unique(DF()$Brand))^-1.19776))*length(unique(DF()$Brand)),"px"), hover = hoverOpts(id = "plot_hover"))
     }) 
    })
     
    output$plot2 = renderPlot({
        
        if(input$btnBuild == 0 || is.null(input$btnBuild)){return()}
        input$btnBuild
        isolate({
        
        ggplot(data = DF(), aes(colour = Retailer), scale = "column") + 
            geom_segment(aes(x=Start, xend=End, y=Brand, yend=Brand), size=2, alpha=1) +
            scale_x_date(limits = c(min(DF()$Start), max(DF()$End)), 
                         breaks = date_breaks(width = "1 week"),
                         labels = date_format("%d %m %y"),
                         expand = c(0,0)) +
            labs(x = "Date", y = "Brand") + 
            theme(plot.title = element_text(size=20, hjust=0)) +
            theme(axis.title = element_text(size=12, face = "bold", hjust = 1)) +
            theme(panel.background = element_rect(fill = NA),
                  panel.grid.minor = element_blank(),
                  axis.text = element_text(size = 12),
                  axis.text.x = element_text(angle = 90, hjust = 1),
                  panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
                  panel.grid.minor = element_blank(),
                  legend.position="top",
                  legend.text = element_text(size = 12),
                  legend.key = element_blank()) +
            guides(colour = guide_legend(nrow = 1, 
                                         title.theme = element_text(size=12, angle = 0, face ="bold")))

        })
         
    })

    #2nd table It is important to run this function before changing DF()
    
    output$table2 = DT::renderDataTable(DT::datatable({
        
        if(input$btnBuild == 0 || is.null(input$btnBuild)){return()}
        input$btnBuild
        isolate({
            
            validate(
                need(nrow(DF())>0, "Table1 help message: No data to buid report on. Please try another options.")
            )
            
            DF()[ ,    
                  .(SubCategory = SubCategory, Start = Start, 
                    End = End, Description = Promo, Chain = Retailer),    
                  by = Brand][order(Brand)]
            
        })   
    }))

# 1st table
    
    output$table = DT::renderDataTable(DT::datatable({
       
        if(input$btnBuild == 0 || is.null(input$btnBuild)){return()}
        input$btnBuild
        isolate({
        
        validate(
        need(nrow(DF())>0, "Table1 help message: No data to buid report on. Please try another options.")
    )
        
        DF()[input$dateRange[1] > Start, Start := input$dateRange[1]]
        DF()[input$dateRange[2] < End, End := input$dateRange[2]] 
            
        all = DF()[, .N]
        WeightedAll = DF()[, sum(Weight)]
        allDays = DF()[, as.numeric(sum(End - Start + 1))]
        WeightedAllDays = DF()[,as.numeric(sum((End - Start + 1)*Weight))]
        
        # check days
        
        DF()[ ,    
           .(AvgDiscount = round(mean(Discount, na.rm = TRUE),1),
             Actions = .N,
             AvgAction = round(as.numeric(sum(End - Start + 1))/.N, 1),
             ShareInBlocks = round(.N/all*100, 1),
             WeightedSIB = round(sum(Weight/WeightedAll*100), 1),
             Days = as.numeric(sum(End - Start + 1)),
             ShareInDays = round(as.numeric(sum(End - Start + 1))/allDays*100, 1), 
             WeightedSID = round(as.numeric(sum((End - Start + 1)*Weight/WeightedAllDays*100)), 1)),
           by = Brand][order(-ShareInDays)]

        })   
    }))

    output$summary1 = renderText({
        
        if (nrow(DF()) == 0) return(NULL)

        point = nearPoints(chartDates, input$plot_hover, threshold = 200, maxpoints = 1)
        if (nrow(point) == 0 || is.null(point)) return(NULL)
        isolate({
        paste("Min:", min(a4),
              "Max:", max(a4) ,
              "Date:", as.Date(input$plot_hover$x, "1970-01-01"),
              "Number of Ads:", chartDates[chartDates$chartDates == as.character(as.Date(input$plot_hover$x, "1970-01-01")), 2])
        })
              
    })
    
    output$summary2 = renderText({
        
        req(nrow(DF())>0)
        
        point = nearPoints(DF(), input$plot_hover, threshold = 5, maxpoints = 50)
        if (length(point) == 0) return(NULL)
 
        paste("Brand:", point$Brand,
              "Retailer:", point$Retailer ,
              "Durtion:", point$Start, "to", point$End,
              "Description:", point$Promo)
    })

    output$login = reactive({
        input$btnLogin
        isolate(
        if ((input$login == "123") & (input$pswd == "123")) {
            logged = TRUE 
        }
        else {
            logged = FALSE
        }
    )
    })
})

