# ui.R --> ui file for Shiny App

#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("DT")

library(shiny)
library(shinydashboard)
library(DT)

ui <- 
  dashboardPage(
    dashboardHeader(title = "Duke Co-Curriculars",
                    dropdownMenu(type = "messages",
                                 messageItem(
                                   from = "Catalyst",
                                   message = "We would love to have you. Please come to our interest meeting on the 1st!"
                                 ),
                                 messageItem(
                                   from = "New User",
                                   message = "How do I get started?",
                                   icon = icon("question"),
                                   time = "13:45"
                                 )
                    ),
                    dropdownMenu(type = "notifications",
                                 notificationItem(
                                   text = "5 new organizations recommended today",
                                   icon("users")
                                 ),
                                 notificationItem(
                                   text = "Profile completion at 86%",
                                   icon = icon("exclamation-triangle"),
                                   status = "warning"
                                 )
                    ),
                    dropdownMenu(type = "tasks", badgeStatus = "success",
                                 taskItem(value = 90, color = "green",
                                          "Past Activities"
                                 ),
                                 taskItem(value = 17, color = "aqua",
                                          "Interests"
                                 )
                    )
    ),
    dashboardSidebar(
      sidebarSearchForm(textId = "searchOrgs", buttonId = "searchButton", 
                        label = "Search..."),
      sidebarMenuOutput("menu"),
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Favorites", tabName = "favorites", icon= icon("star")),
        menuItem("Widgets", tabName = "widgets", icon = icon("th"), 
                 badgeLabel = "new", badgeColor = "green"),
        menuItem("About Us", tabName = "about", icon = icon("address-card"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",                   
                h2("Dashboard"),
                #fluidRow(
                #  h3("Welcome!")
                #),
                fluidRow(
                  # User Profile Box
                  # Include follow up questions about Bass Connections, Data+ etc.?
                  box(title = "User Profile", status = "primary", width = 12,
                      solidHeader = TRUE, collapsible = TRUE,
                      column(width = 4,
                             textInput("netid", label = h4("Net ID"), placeholder = "Ex. abc123"),
                             selectInput("major", label = h4("Major(s)"),
                                         choices = list("AMES" = "ames",
                                                        "VMS" = "vms"),
                                         multiple = TRUE),
                             textInput("year", label = h4("Graduation Year"), placeholder = "Ex. 2020")
                      ),
                      column(width = 8,
                             selectInput("yr1prog", label = h4("Programs - Year 1"),
                                         choices = list("Bass Connections" = 1,
                                                        "Blueprint" = 2),
                                         multiple = TRUE),
                             selectInput("yr2prog", label = h4("Programs - Year 2"),
                                         choices = list("Bass Connections" = 1,
                                                        "Blueprint" = 2),
                                         multiple = TRUE),
                             selectInput("yr3prog", label = h4("Programs - Year 3"),
                                         choices = list("Bass Connections" = 1,
                                                        "Blueprint" = 2),
                                         multiple = TRUE),
                             selectInput("yr4prog", label = h4("Programs - Year 4"),
                                         choices = list("Bass Connections" = 1,
                                                        "Blueprint" = 2),
                                         multiple = TRUE)
                      )
                  )
                )#,
                #fluidRow(
                  # Info Boxes - can use InputBoxOutput
                  #infoBox(title = "Co-Curriculars", 
                  #        value = 60, 
                  #        subtitle = "We represent 60 different co-curricular activities at Duke",
                  #        icon = icon("list-alt"),
                  #        color = "light-blue",
                  #        width = 4
                  #),
                  #infoBox(title = "Approval Rating", 
                  #        value = 100, 
                  #        icon = icon("thumbs-up"),
                  #        color = "green",
                  #        width = 4
                  #)
                #)
        ),
        tabItem(tabName = "favorites",
                h2("Favorites")),
        tabItem(tabName = "widgets",
                h2("Widgets"),
                # Create Content-Based Recommender Widget
                box(title = "Recommender", status = "primary",
                    solidHeader = TRUE, width = 12, collapsible = TRUE, 
                    column(width = 4,
                           selectInput("programs", label = h3("Co-Curricular Activities"), 
                                       choices = list("Bass Connections" = 1, 
                                                      "Blueprint" = 2, 
                                                      "BOW" = 3, 
                                                      "Catalyst" = 4, 
                                                      "Co-Lab Grants" = 5, 
                                                      "Co-Lab Roots" = 6,
                                                      "Data+" = 7,
                                                      "DataFest" = 8,
                                                      "Duke Engineers for International Development (DEID)" = 9,
                                                      "Design for America" = 10,
                                                      "Domath" = 11,
                                                      "Duke Conservation Tech" = 12,
                                                      "Duke Electric Vehicles" = 13,
                                                      "Duke Energy Club" = 14,
                                                      "Duke In Chicago" = 15,
                                                      "Duke In Silicon Valley" = 16, 
                                                      "Duke Mobile App Development" =17,
                                                      "Duke Motorsports" = 18,
                                                      "Duke Robotics" = 19,
                                                      "Duke Startup Challenge" = 20,
                                                      "Enable" = 21,
                                                      "Engineering World Health" = 22,
                                                      "FEMMES" = 23,
                                                      "HackDuke" = 24,
                                                      "Ideate" = 25,
                                                      "Institute of Electrical and Electronics Engineers (IEEE)" = 26, 
                                                      "Innovation Co-Lab Studio" = 27,
                                                      "MUSER" = 28,
                                                      "Project Edge" = 29,
                                                      "Project Search" = 30,
                                                      "Smart Home" = 31,
                                                      "SPIRE" = 32,
                                                      "The Cube" = 33,
                                                      "Wiring With Women" = 34,
                                                      "Women in Computing" = 35),
                                       multiple = TRUE),
                           # Include clarifying text ----
                           helpText("Note: Select all the co-curricular programs you have participated 
                                    at Duke from the drop-down menu.")
                           ),
                    column(width = 8,
                           DT::dataTableOutput("table")
                    )
                    ),
                # Create a Filtering Widget
                box(title = "Interests", status = "primary",
                    solidHeader = TRUE, width = 12, collapsible = TRUE,
                    selectInput("interests", label = h3("Interests"),
                                choices = list("Art" = "a"),
                                multiple = TRUE)
                )
      ),
      tabItem(tabName = "about",
              h2("About Us"),
              p("
                "),
              h3("Contact Information"),
              p("Director: Paul Bendich"),
              p("Project Team Manager: Lindsay Berry"),
              p("Project Team Members: Alec Ashforth, Brooke Keene, Vincent Liu, Dezmanique Martin")
              )
      )
      )
    )

shinyUI(ui)