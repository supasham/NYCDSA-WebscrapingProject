################################################################################
# Shameer SUkha  - Webscraping project
# server.R
################################################################################

# Main function
shinyServer(function(input, output, session){
  
  # Server functions for Dashboard item
  #####################################
  
  # Helper function to create text for InfoBoxes
  get_infobox_text <- function(boxtext, boxval, boxsub) {
    
    return(list(boxtext, boxval, boxsub))
  }
  
  
  
  # First 4 Infoboxes 
  #############################################################################
  output$infoBox11 <- renderInfoBox({
    ans1 <- df %>% select(., lcbo_name) %>% summarise(., total=n())
    ans2 <- df %>% select(., container) %>% unique(.)
    ans2 <- paste0(ans2$container,collapse="s, ")
    tot = paste0(formatC(as.numeric(ans1$total), format="f", digits=0, big.mark=","))
    
    box11 <- get_infobox_text("Total Red Wine Items", tot, ans2)

    infoBox(box11[1], value = box11[2], subtitle = box11[3],
            icon = icon("wine-glass"), fill = FALSE,
            color = "red", width = 3
            )
  })

  
  
  
  output$infoBox12 <- renderInfoBox({
    ans1 = df %>% select(., lcbo_name, price) %>% arrange(., desc(price))
    price = paste0("$", formatC(as.numeric(ans1[1,2]), format="f", digits=0, big.mark=","))
    
    box12 <- get_infobox_text("Most Expensive Wine", price, ans1[1,1])

    infoBox(box12[1], value = box12[2], subtitle = box12[3],
            icon = icon("coins"), fill = FALSE,
            color = "olive", width = 3
            )
  })

  output$infoBox13 <- renderInfoBox({
    ans1 <- df %>% select(., lcbo_region, lcbo_country) %>% 
      group_by(., lcbo_country, lcbo_region) %>%
      summarise(., total=n()) %>% 
      arrange(., desc(total))
    
    box13 <- get_infobox_text("Most Popular Region", ans1[1,1], ans[1,2])

    infoBox(box13[1], value = box13[2], subtitle = box13[3],
            icon = icon("globe-americas"), fill = FALSE,
            color = "yellow", width = 3
            )
  })

  output$infoBox14 <- renderInfoBox({
    ans1 <- df %>% select(., lcbo_name, viv_name, num_reviews) %>% 
      arrange(., desc(num_reviews))
    tot = paste0(formatC(as.numeric(ans1[1,3]), format="f", digits=0, big.mark=","))
    
    box14 <- get_infobox_text("Most Vivino Reviews", tot, ans1[1,2])

    infoBox(box14[1], value = box14[2], subtitle = box14[3],
            icon = icon("thumbs-up"), fill = FALSE,
            color = "light-blue", width = 3
            )
  })
  #############################################################################
  
  # Second 4 Infoboxes
  #############################################################################
  output$infoBox21 <- renderInfoBox({
    ans1 = df %>% select(., size.mL) %>% filter(., size.mL==750 ) %>% summarise(., total=n())
    tot = paste0(formatC(as.numeric(ans1), format="f", digits=0, big.mark=","))
    
    box21 <- get_infobox_text("Total Bottles", tot, "750ml bottles")

    infoBox(box21[1], value = box21[2], subtitle = box21[3],
            icon = icon("wine-bottle"), fill = FALSE,
            color = "maroon", width = 3
            )
  })
  
  output$infoBox22 <- renderInfoBox({
    ans1 = df %>% filter(., size.mL==750 ) %>% select(., lcbo_name, price) %>% arrange(., price)
    price = paste0("$", formatC(as.numeric(ans1[1,2]), format="f", digits=2, big.mark=","))
    
    box22 <- get_infobox_text("Least Expensive Bottle", price, ans1[1,1])

    infoBox(box22[1], value = box22[2], subtitle = box22[3],
            icon = icon("coins"), fill = FALSE,
            color = "green", width = 3
            )
  })

  output$infoBox23 <- renderInfoBox({
    ans1 <- df %>% select(., lcbo_region, lcbo_country) %>% 
      group_by(., lcbo_country, lcbo_region) %>%
      summarise(., total=n()) %>% 
      arrange(., desc(total))
    
    box23 <- get_infobox_text("Second Most Popular Region", ans1[2,1], ans1[2,2])

    infoBox(box23[1], value = box23[2], subtitle = box23[3],
            icon = icon("globe-europe"), fill = FALSE,
            color = "orange", width = 3
            )
  })

  output$infoBox24 <- renderInfoBox({
    ans1 <- df %>% select(., lcbo_name, viv_name, score) %>% 
      arrange(., desc(score))
    
    box24 <- get_infobox_text("Highest Rating", ans1[1,3], ans1[1,2])

    infoBox(box24[1], value = box24[2], subtitle = box24[3],
            icon = icon("star"), fill = FALSE,
            color = "blue", width = 3
            )
  })
  #############################################################################
  
  # Charts for dashboard
  #############################################################################
  
  # Create helper functions to set options for Google charts 
  #############################################################################
  my_options <- list(width="780", height="430",
                     vAxis="{title:'Price (CAD)'}",
                     hAxis="{title:'Average wine rating'}",
                     hAxis.gridlines = "{color: '#333'}",
                     chartArea = "{left:70,top:30,width:'75%',height:'85%'}",
                     explorer = "{actions:['dragToZoom','rightClickToReset']}")
  
  # Main Scatter Chart 
  #############################################################################
  
  
  
  
  
  
  output$scat11 <- renderGvis({
    
    
      
      numrev = input$slider11
      pricemin = input$slider12[1]
      pricemax = input$slider12[2]
      
      
      
      cht11 <- df %>%
        filter(., (viv_name != "missed")) %>%
        filter(., container == "bottle") %>%
        filter(., fwscore4 > 80) %>% 
        filter(., num_reviews > numrev) %>%
        filter(., (price > pricemin) & (price < pricemax)) %>% 
        mutate(., ttname = paste(paste(paste(lcbo_name,price,sep=":$"),size.mL,sep=":"),vintage,sep="mL:")) %>% 
        select(., AvgRating=score, Price=price, country=lcbo_country, ttname)
      
      if (dim(cht11)[1]==0){
        cht11 <- data.frame(AvgRating=0, Price=0, country="None", ttname="None")
      }
      
      tmp <- cht11 %>% 
        group_by(., country) %>% 
        summarise(., total=n()) %>% 
        arrange(., desc(total))
      
      
    observe({  
      
      checkvarnames <-c(tmp$country)
      checklist<- setNames(as.list(seq(1,length(checkvarnames))),checkvarnames)
      
      updateCheckboxGroupInput(
        session, 
        inputId = "checkbox13",
        choices = checklist,
        selected = input$checkbox13
        )
    })
      
      
      dt <- cht11[,c("AvgRating", "Price")]
      
      for (icount in tmp$country){
        dt[icount] <- ifelse(cht11$country==icount, dt$Price, NA)
        dt[paste(icount,"html","tooltip",sep=".")] <- cht11$ttname
      }
      dt$Price <- NULL
      
    
    
    

    gvisScatterChart(dt,options=my_options)
    
    
  })
  
  # Charts 1: Rating boxplots by country 
  #############################################################################
  output$ggdistbox <- renderPlot({
      numrev = input$slidernumrev
      
      ggdistbox <- df %>%
        filter(., (viv_name != "missed")) %>%
        filter(., container == "bottle") %>%
        filter(., size.mL == 750) %>% 
        filter(., fwscore4 > 80) %>% 
        filter(., num_reviews > numrev) %>%  
        ggplot(., aes(x = reorder(lcbo_country, score, FUN = median),
                      y = score, fill = lcbo_country)) +
        geom_boxplot(alpha = 0.5) +
        coord_flip() +
        labs(x = "lcbo_country", y = "Average Rating") +
        theme_light() +
        theme(legend.position = "none")
      
      ggdistbox
      
    })
  
  
  
  # Create helper functions to set options for Google charts 
  #############################################################################
  # set_my_options = function(maxtenor){
  #   list(
  #     legend = "none",
  #     seriesType = "bars",
  #     series = "[{targetAxisIndex: 0},{targetAxisIndex: 1, type:'scatter'}]",
  #     vAxes = "[{title:'delta'}, {title:'CDS level'}]",
  #     hAxis.gridlines = "{color: '#333'}",
  #     hAxis = paste0("{title:'tenor(yrs)', minValue:1, maxValue:",
  #                    as.character(maxtenor),
  #                    ",ticks: [",
  #                    paste(as.character(c(1:maxtenor)), collapse=","),
  #                    "]}"),
  #     chartArea = "{height: 'automatic'}",
  #     explorer = "{actions:['dragToZoom','rightClickToReset']}"
  #     )
  # }
  
  # First 4 Infoboxes 
  #############################################################################
  # output$scat11 <- renderGvis({
  #   cht11 <- avgdata %>% 
  #     filter(., (Sector == "Consumer Cycl.") & 
  #              (Rating == "BB") & 
  #              (Region == "North America") & 
  #              (Tenor<=10)) %>%
  #     transmute(., Tenor, 
  #               CDS.change = round(sprdchanges,1), 
  #               CDS.today = round(ParSpreadMid.t,1))
  #   
  #   gvisComboChart(cht11, xvar="Tenor",
  #                  yvar=c("CDS.change", "CDS.today"),
  #                  options=set_my_options(10))
  # })
  # 
  # output$scat12 <- renderGvis({
  #   cht12 <- avgdata %>% 
  #     filter(., (Sector == "Consumer Non-Cycl.") & 
  #              (Rating == "BB") & 
  #              (Region == "North America") & 
  #              (Tenor<=10)) %>% 
  #     transmute(., Tenor,
  #               CDS.change = round(sprdchanges,1),
  #               CDS.today = round(ParSpreadMid.t,1))
  #   
  #   gvisComboChart(cht12, xvar="Tenor",
  #                  yvar=c("CDS.change", "CDS.today"),
  #                  options=set_my_options(10))
  # })
  # 
  # output$scat13 <- renderGvis({
  #   cht13 <- avgdata %>% 
  #     filter(., (Sector == "Financials") &
  #              (Rating == "AA") &
  #              (Region == "North America") &
  #              (Tenor<=10)) %>%
  #     transmute(., Tenor,
  #               CDS.change = round(sprdchanges,1),
  #               CDS.today = round(ParSpreadMid.t,1))
  #   
  #   gvisComboChart(cht13, xvar="Tenor",
  #                  yvar=c("CDS.change", "CDS.today"),
  #                  options=set_my_options(10))
  # })
  # 
  # output$scat14 <- renderGvis({
  #   cht14 <- avgdata %>% 
  #     filter(., (Sector == "Oil & Gas") & 
  #              (Rating == "A") & 
  #              (Region == "North America") & 
  #              (Tenor<=10)) %>%
  #     transmute(., Tenor,
  #               CDS.change = round(sprdchanges,1),
  #               CDS.today = round(ParSpreadMid.t,1))
  #   
  #   gvisComboChart(cht14, xvar="Tenor",
  #                  yvar=c("CDS.change", "CDS.today"),
  #                  options=set_my_options(10))
  # })
  # #############################################################################
  # 
  # # Second 4 Infoboxes 
  # #############################################################################
  # output$scat21 <- renderGvis({
  #   cht21 <- avgdata %>% 
  #     filter(., (Sector == "Consumer Cycl.") &
  #              (Rating == "B") &
  #              (Region == "North America") &
  #              (Tenor<=10)) %>% 
  #     transmute(., Tenor,
  #               CDS.change = round(sprdchanges,1),
  #               CDS.today = round(ParSpreadMid.t,1))
  #   
  #   gvisComboChart(cht21, xvar="Tenor",
  #                  yvar=c("CDS.change", "CDS.today"),
  #                  options=set_my_options(10))
  # })
  # 
  # output$scat22 <- renderGvis({
  #   cht22 <- avgdata %>% 
  #     filter(., (Sector == "Consumer Non-Cycl.") &
  #              (Rating == "B") &
  #              (Region == "North America") &
  #              (Tenor<=10)) %>% 
  #     transmute(., Tenor, 
  #               CDS.change = round(sprdchanges,1), 
  #               CDS.today = round(ParSpreadMid.t,1))
  #   
  #   gvisComboChart(cht22, xvar="Tenor",
  #                  yvar=c("CDS.change", "CDS.today"),
  #                  options=set_my_options(10))
  # })
  # 
  # output$scat23 <- renderGvis({
  #   cht23 <- avgdata %>% 
  #     filter(., (Sector == "Financials") &
  #              (Rating == "A") &
  #              (Region == "North America") &
  #              (Tenor<=10)) %>% 
  #     transmute(., Tenor,
  #               CDS.change = round(sprdchanges,1),
  #               CDS.today = round(ParSpreadMid.t,1))
  #   
  #   gvisComboChart(cht23, xvar="Tenor",
  #                  yvar=c("CDS.change", "CDS.today"),
  #                  options=set_my_options(10))
  # })
  # 
  # output$scat24 <- renderGvis({
  #   cht24 <- avgdata %>% 
  #     filter(., (Sector == "Oil & Gas") &
  #              (Rating == "BBB") &
  #              (Region == "North America") &
  #              (Tenor<=10)) %>% 
  #     transmute(., Tenor,
  #               CDS.change = round(sprdchanges,1),
  #               CDS.today = round(ParSpreadMid.t,1))
  #   
  #   gvisComboChart(cht24, xvar="Tenor",
  #                  yvar=c("CDS.change", "CDS.today"),
  #                  options=set_my_options(10))
  # })
  #############################################################################
  
  
  # Server functions for first Charts item
  ########################################
  # output$chart11 <- renderGvis({
  #   chtdata11 <- avgdata %>%
  #     filter(., (Sector == input$sectorselected1) &
  #              (Rating == input$ratingselected1) &
  #              (Region == input$regionselected1)) %>%
  #     transmute(., Tenor,
  #               CDS.change = round(sprdchanges, 1),
  #               CDS.today = round(ParSpreadMid.t, 1))
  #   
  #   gvisComboChart(chtdata11, xvar = "Tenor", yvar = c("CDS.change", "CDS.today"),
  #                  options = set_my_options(30))
  # })
  # 
  # output$chart12 <- renderGvis({
  #   chtdata12 <- avgdata %>%
  #     filter(., (Sector == input$sectorselected2) &
  #              (Rating == input$ratingselected2) &
  #              (Region == input$regionselected2)) %>%
  #     transmute(., Tenor,
  #               CDS.change = round(sprdchanges, 1),
  #               CDS.today = round(ParSpreadMid.t, 1))
  #   
  #   gvisComboChart(chtdata12, xvar = "Tenor", yvar = c("CDS.change", "CDS.today"),
  #                  options = set_my_options(30))
  # })
  #############################################################################
  
  
  # Server functions for second Charts item
  #########################################
  
  # # Create own color palettes for heatmaps, red as negative and blue as positive
  # redpal <- c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d')
  # redpal <- rev(redpal)
  # bluepal <- c('#f7fbff','#deebf7','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#08519c','#08306b')
  # colors <- c(redpal, bluepal)
  # 
  # # Create helper function to creat ggplot heatmaps for factors
  # makefactorplots <- function(df, yvar, colorpalette) {
  #   ggplot(df, aes(x = reorder(as.character(Tenor), Tenor, FUN = max),
  #                  y = reorder(Category, Factor, FUN = mean))) +
  #     geom_tile(aes(fill = Factor)) +
  #     scale_fill_gradientn(colors = colorpalette,
  #                          breaks = seq(minfactor / 100, maxfactor / 100, by = 0.01),
  #                          labels = paste0(as.character(c(minfactor:maxfactor)), "%"),
  #                          limits = c(minfactor / 100, maxfactor / 100)
  #                          ) +
  #     labs(x = "Tenor", y = yvar) +
  #     theme_light() +
  #     facet_grid(. ~ Class)
  # }
  # 
  # # Create heatmaps for all factors
  # output$ggall <- renderPlot({
  #   ggall <- makefactorplots(classdf, "All Factors", colors)
  #   ggall
  # })
  # 
  # output$ggregion <- renderPlot({
  #   ggregion <- makefactorplots(regiondf, "Region", colors)
  #   ggregion
  # })
  # 
  # output$ggsector <- renderPlot({
  #   ggsec <- makefactorplots(sectordf, "Sector", colors)
  #   ggsec
  # })
  # 
  # output$ggrating <- renderPlot({
  #   ggrating <- makefactorplots(ratingdf, "Rating", colors)
  #   ggrating
  # })
  #############################################################################
  
  
  # Server functions for third Charts item
  #########################################
  # output$ggdistbox <- renderPlot({
  #   bvar1 = input$radio31
  #   bvar2 = input$radio32
  #   
  #   ggdistbox <- avgdata %>%
  #     filter(., (Rating != "No Rating") & (Rating != "CCC")) %>%
  #     filter(., Tenor == bvar2) %>%
  #     ggplot(., aes(x = reorder(get(bvar1), ParSpreadMid.t, FUN = mean),
  #                   y = ParSpreadMid.t, fill = get(bvar1))) +
  #     geom_boxplot(alpha = 0.5) + 
  #     coord_flip() +
  #     labs(x = bvar1, y = "CDS spread") +
  #     theme_light() +
  #     theme(legend.position = "none")
  #   
  #   ggdistbox
  # })
  # 
  # output$ggdistdensity <- renderPlot({
  #   dvar1 = input$radio33
  #   dvar2 = input$radio34
  #   
  #   ggdistdensity <- avgdata %>%
  #     filter(., (Rating != "No Rating") & (Rating != "CCC")) %>%
  #     filter(., Tenor == dvar2) %>%
  #     ggplot(., aes(x = ParSpreadMid.t, fill = get(dvar1))) +
  #     geom_density(alpha = 0.25) +
  #     labs(x = "CDS Spread", y = "Frequency", fill = dvar1) +
  #     theme_light()
  #   
  #   ggdistdensity
  # })
  #############################################################################
  
  
  # Server functions for data table item
  #########################################
  output$table <- DT::renderDataTable({
    datatable(factortbl, rownames = FALSE, filter = "top") %>%
      formatRound('score', digits = 2) %>%
      formatRound(c('size.mL','critic_score','num_reviews'), digits = 0) %>%
       formatCurrency(c('price'), currency = "$", digits = 0) %>% 
       formatCurrency(c('unit_price'), currency = "$", digits = 2) %>% 
      formatStyle("score", background = "skyblue", fontWeight = 'bold')
  })
  #############################################################################
  
  
})