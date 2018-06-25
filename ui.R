# ui.R --> ui file for Shiny App

#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("DT")
library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(googlesheets)

# Load Data from Google Sheets
# List of majors
gs_maj <- gs_title("Majors")
maj_list <- gs_read_csv(gs_maj, col_names = TRUE)
maj_choice <- as.list(maj_list$Abbreviations)
names(maj_choice) <- maj_list$Majors

# List of activities
gs_prog <- gs_title("DukeGroups_Tech")
prog_list <- gs_read_csv(gs_prog, col_names = TRUE)
prog_choice <- as.list(c(1:nrow(prog_list)))
names(prog_choice) <- prog_list$CoCurriculars

header <-  
  dashboardHeader(
    title = "Duke Co-Curriculars",
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
  )

sidebar <- 
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
  )

body <- 
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",                   
              h2("Dashboard"),
              h3("Welcome!"),
              fluidRow(
                # User Profile Box
                # Include follow up questions about Bass Connections, Data+ etc.?
                box(title = "User Profile", status = "primary", width = 12,
                    solidHeader = TRUE, collapsible = TRUE,
                    column(width = 4,
                           textInput("netid", label = h4("Net ID"), placeholder = "Ex. abc123"),
                           selectInput("major", label = h4("Major(s)"),
                                       choices = maj_choice,
                                       multiple = TRUE),
                           textInput("year", label = h4("Graduation Year"), placeholder = "Ex. 2020")
                    ),
                    column(width = 8,
                           selectInput("yr1prog", label = h4("Programs - Year 1"),
                                       choices = prog_choice,
                                       multiple = TRUE),
                           selectInput("yr2prog", label = h4("Programs - Year 2"),
                                       choices = prog_choice,
                                       multiple = TRUE),
                           selectInput("yr3prog", label = h4("Programs - Year 3"),
                                       choices = prog_choice,
                                       multiple = TRUE),
                           selectInput("yr4prog", label = h4("Programs - Year 4"),
                                       choices = prog_choice,
                                       multiple = TRUE)
                    ),
                    actionButton("submit", "Submit")
                )
              )
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
                                     choices = prog_choice,
                                     multiple = TRUE),
                         # Include clarifying text ----
                         helpText("Note: Select all the co-curricular programs you have participated 
                                  at Duke from the drop-down menu.")
                  ),
                  column(width = 8,
                         DT::dataTableOutput("table")
                  )
              )
              # Create a Filtering Widget
              #box(title = "Interests", status = "primary",
              #    solidHeader = TRUE, width = 12, collapsible = TRUE,
              #    selectInput("interests", label = h3("Interests"),
              #                choices = list("Art" = "a"),
              #                multiple = TRUE)
              #)
      ),
      tabItem(tabName = "about",
              h2("About Us"),
              p("We are a team of Duke undergraduate students currently working on a 
                Data+ project in collaboration with Duke's Office of Informational 
                Technology (O.I.T.). Duke University is an exciting and ever-changing 
                institution, but as Duke's massive co-curricular environment grows it 
                can become more difficult for undergraduates to navigate this 
                complicated academic and extracurricular landscape. Therefore, our 
                goal is to create an 'e-advisor' tool that will help students determine
                which co-curricular activities are well-suited for their interests. In
                order to accomplish this, we hope to gather data that will allow us to
                track a student's co-curricular 'pathway,' a map of the various 
                activities the student was involved in during their time at Duke. With
                this data, we plan to improve the current recommendation system running
                behind this initial version of our 'e-advisor.'"),
              p("Thank you for helping us with this task! 
              If you have any questions, please feel free to contact us."),
              h3("Contact Information"),
              p("Director: Paul Bendich"),
              p("Project Team Manager: Lindsay Berry"),
              p("Project Team Members: Alec Ashforth, Brooke Keene, Vincent Liu, Dezmanique Martin")
      )
    )
  )

ui <- dashboardPage(header, sidebar, body)

shinyUI(ui)