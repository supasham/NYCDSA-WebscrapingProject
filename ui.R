################################################################################
# Shameer SUkha  - Webscraping project - NYCDSA 17 December 2020
# ui.R
################################################################################

# Main function to generate dashboard
shinyUI(dashboardPage(title = "Red Wine Dashboard", skin = "green",
                      # Put the data of the latest data as title
                      dashboardHeader(title = "Red Wine Analysis"),
                      dashboardSidebar(
                        sidebarUserPanel("User", image ="LCBO.jpg"),
                        sidebarMenu(
                          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")
                                   ),
                          # menuItem("Charts", icon = icon("bar-chart-o"),
                          #          menuSubItem("Custom Average Charts", tabName = "charts1"),
                          #          menuSubItem("Factor Charts", tabName = "charts2"),
                          #          menuSubItem("Distribution Charts", tabName = "charts3")
                          #          ),
                          menuItem("Data", tabName = "data", icon = icon("database")
                                   )
                          )
                        ),
                      dashboardBody(
                        tabItems(
                          # Use infoBoxes and charts to show the most important information
                          tabItem(tabName = "dashboard",
                                  # Create 4 fluid rows of 4 items each for the 8 most material variables
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
                                  # fluidRow(
                                  #   box(title = "Cons. Cyclical : BB", status = "primary",
                                  #       solidHeader = TRUE, htmlOutput("scat11"), width=3),
                                  #   box(title = "Cons. Non-Cycl. : BB", status = "primary",
                                  #       solidHeader = TRUE, htmlOutput("scat12"), width=3),
                                  #   box(title = "Financials : AA", status = "primary",
                                  #       solidHeader = TRUE, htmlOutput("scat13"), width=3),
                                  #   box(title = "Oil & Gas : A", status = "primary",
                                  #       solidHeader = TRUE, htmlOutput("scat14"), width=3)
                                  #   ),
                                  # fluidRow(
                                  #   box(title = "Cons. Cyclical : B", status = "primary",
                                  #       solidHeader = TRUE, htmlOutput("scat21"), width=3),
                                  #   box(title = "Cons. Non-Cycl. : B", status = "primary",
                                  #       solidHeader = TRUE, htmlOutput("scat22"), width=3),
                                  #   box(title = "Financials : A", status = "primary",
                                  #       solidHeader = TRUE, htmlOutput("scat23"), width=3),
                                  #   box(title = "Oil & Gas : BBB", status = "primary",
                                  #       solidHeader = TRUE, htmlOutput("scat24"), width=3)
                                  #   )
                                  ),
                          # tabItem(tabName = "charts1",
                          #         # Create first chart sub-item that will allow user to choose own factors to plot
                          #         fluidRow(box(title = "Term Structure 1", status = "primary", solidHeader = TRUE,
                          #                      selectizeInput("regionselected1", "Select Region to Display",
                          #                                     regionchoice),
                          #                      selectizeInput("sectorselected1", "Select Sector to Display",
                          #                                     sectorchoice),
                          #                      selectizeInput("ratingselected1", "Select Rating to Display",
                          #                                     ratingchoice),
                          #                      width = 3
                          #                      ),
                          #                  box(title = "Term Structure 1", status = "primary", solidHeader = TRUE,
                          #                      htmlOutput("chart11"), width = 6, height = 300
                          #                      )
                          #                  ),
                          #         br(),
                          #         br(),
                          #         fluidRow(box(title = "Term Structure 2", status = "info", solidHeader = TRUE,
                          #                      selectizeInput("regionselected2", "Select Region to Display",
                          #                                     regionchoice),
                          #                      selectizeInput("sectorselected2", "Select Sector to Display",
                          #                                     sectorchoice),
                          #                      selectizeInput("ratingselected2","Select Rating to Display",
                          #                                     ratingchoice),
                          #                      width = 3
                          #                      ),
                          #                  box(title = "Term Structure 2", status = "info", solidHeader = TRUE,
                          #                      htmlOutput("chart12"), width = 6, height = 300
                          #                      )
                          #                  )
                          #         ),
                          # tabItem(tabName = "charts2",
                          #         # Create second chart sub-item consisting of heatmaps of the major factors
                          #         fluidRow(box(plotOutput("ggall")),
                          #                  box(plotOutput("ggsector"))
                          #                  ),
                          #         fluidRow(box(plotOutput("ggregion")),
                          #                  box(plotOutput("ggrating"))
                          #                  )
                          #         ),
                          # tabItem(tabName = "charts3",
                          #         # Create third chart sub-item consisting of distribution plots of various factors
                          #         fluidRow(box(title = "Factor", status = "primary", solidHeader = TRUE,
                          #                      radioButtons("radio31", label = "Please select:",
                          #                                   choices = list("Region", "Sector", "Rating"),
                          #                                   selected = "Region"),
                          #                      width = 2),
                          #                  box(plotOutput("ggdistbox")),
                          #                  box(title = "Select tenor:", status = "primary", solidHeader = TRUE,
                          #                      radioButtons("radio32", label = "Please select:",inline = TRUE,
                          #                                   choices = tenorchoice, selected = tenorchoice[6]),
                          #                      width = 2, height = 200),
                          #                  ),
                          #         fluidRow(box(title = "Factor", status = "primary", solidHeader = TRUE,
                          #                      radioButtons("radio33", label = "Please select:",
                          #                                   choices = list("Region", "Sector", "Rating"),
                          #                                   selected = "Region"),
                          #                      width = 2),
                          #                  box(plotOutput("ggdistdensity")),
                          #                  box(title = "Select tenor:", status = "primary", solidHeader = TRUE,
                          #                      radioButtons("radio34", label = "Please select:", inline = TRUE,
                          #                                   choices = tenorchoice, selected = tenorchoice[6]),
                          #                      width = 2, height = 200),
                          #                  )
                          #         ),
                          tabItem(tabName = "data",
                                  # Create data table of factor changes 
                                  fluidRow(box(DT::dataTableOutput("table"), width = 12))
                                  )
                          )
                        )
                      )
)

