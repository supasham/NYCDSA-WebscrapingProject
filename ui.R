################################################################################
# Shameer SUkha  - Webscraping project - NYCDSA 17 December 2020
# ui.R
################################################################################

# Main function to generate dashboard
shinyUI(dashboardPage(title = "Red Wine Dashboard", skin = "green",
                      
                      dashboardHeader(title = "Red Wine Analysis"),
                      dashboardSidebar(
                        sidebarUserPanel("User", image ="LCBO.jpg"),
                        sidebarMenu(
                          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")
                                   ),
                          menuItem("Charts", icon = icon("bar-chart-o"),
                                    menuSubItem("Rating by Country", tabName = "charts2"),
                                    menuSubItem("Price per mL by Country", tabName = "charts3"),
                                    menuSubItem("Consumer vs Critic", tabName = "charts4")
                                    ),
                          menuItem("Data", tabName = "data", icon = icon("database")
                                   )
                          )
                        ),
                      dashboardBody(
                        tabItems(
                          # Use infoBoxes and charts to show the most important information
                          tabItem(tabName = "dashboard",
                                  
                                  fluidRow(
                                    infoBoxOutput("infoBox11", width=3),
                                    infoBoxOutput("infoBox12", width=3),
                                    infoBoxOutput("infoBox13", width=3),
                                    infoBoxOutput("infoBox14", width=3)
                                    ),
                                  fluidRow(
                                    infoBoxOutput("infoBox21", width=3),
                                    infoBoxOutput("infoBox22", width=3),
                                    infoBoxOutput("infoBox23", width=3),
                                    infoBoxOutput("infoBox24", width=3)
                                    ),
                                  fluidRow(
                                    box(sliderInput("slider11", label = h4("Minimum number of reviews"), min = 0, 
                                                    max = max_reviews, value = 1),
                                        width = 3
                                    ),
                                    box(sliderInput("slider12", label = h4("Price range (C$)"), min = 0, 
                                                    max = max_price, value = c(0,max_price)),
                                        width = 3
                                    ),
                                    box(radioButtons("radio13", label = "Select price type:",
                                                     choices = list("Price per bottle", "Price per mL"),
                                                     selected = "Price per bottle"),
                                        width = 3
                                        )
                                    ),
                                  fluidRow(
                                    box(htmlOutput("scat11"), width = 6, height = 500
                                    )
                                    
                                  )
                          ),
                          tabItem(tabName = "charts2",
                                  fluidRow(
                                    box(sliderInput("slider21", label = h3("Minimum number of reviews"), min = 0,
                                                    max = max_reviews, value = 1000),
                                        width = 3
                                        ),
                                    box(plotOutput("ggdist21"), width = 6, height = 480
                                        )
                                    )
                                  
                                  ),
                          tabItem(tabName = "charts3",
                                  fluidRow(
                                    box(plotOutput("ggdist31"), width = 6, height = 480
                                    ),
                                    box(sliderInput("slider31", label = h3("Maximum bottle price"), min = 0,
                                                    max = max_price, value = 10000),
                                        width = 3
                                    ),
                                    box(radioButtons("radio31", label = "Select price type:",
                                                     choices = list("Price per bottle", "Price per mL", "Log price per bottle", "Log price per mL"),
                                                     selected = "Price per bottle"),
                                        width = 3
                                    )
                                  )
                                  ),
                          tabItem(tabName = "charts4",
                                  fluidRow(
                                    box(sliderInput("slider41", label = h4("Minimum number of reviews"), min = 0, 
                                                    max = max_reviews, value = 1000),
                                        width = 3
                                    ),
                                    box(sliderInput("slider42", label = h4("Price range (C$)"), min = 0, 
                                                    max = max_price, value = c(0,max_price)),
                                        width = 3
                                    ),
                                    
                                  ),
                                  fluidRow(
                                    box(htmlOutput("scat41"), width = 6, height = 500
                                    )
                                    
                                  )
                                  ),
                          tabItem(tabName = "data",
                                  # Create data table of factor changes 
                                  fluidRow(box(DT::dataTableOutput("table"), width = 12))
                                  )
                          )
                        )
                      )
)

