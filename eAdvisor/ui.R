library(shiny)
library(shinyjs)
library(shinydashboard)
library(googlesheets)

# Access Data from Googlesheets
# List of majors
gs_maj <- gs_key("1LqaHYZixDr4aMYq64wojMF5AYDVOsD83IWlX-ayDc0c")
maj_list <- gs_read_csv(gs_maj, col_names = TRUE)
maj_choice <- as.list(maj_list$Abbreviations)
names(maj_choice) <- maj_list$Majors

# List of activities
gs_prog <- gs_key("1HkqN1ISgHevSjYQw5XJlkAMQao61GMktFl9CO4PqMJY")
prog_list <- gs_read_csv(gs_prog, col_names = TRUE)
prog_choice <- as.list(prog_list$Code)
names(prog_choice) <- prog_list$CoCurriculars

# Pop-up Messages
js_thanks <- 'Shiny.addCustomMessageHandler("thanks", function(message) {alert(message);});'
js_check <- 'Shiny.addCustomMessageHandler("check", function(message) {alert(message);});'
js_exists <- 'Shiny.addCustomMessageHandler("exists", function(message) {alert(message);});'
js_record <- 'Shiny.addCustomMessageHandler("noRecord", function(message) {alert(message);});'

header <-  
  dashboardHeader(
    title = "Duke University's Co-Curricular E-Advisor",
    titleWidth = 400
  )

sidebar <- 
  dashboardSidebar(
    width = 400,
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
              p("As we all know, Duke University can be a difficult environment to navigate, especially 
                with its countless opportunities and resources. Therefore, we have compiled a list of over 
                150 different co-curricular programs, although we hope to eventually  include all Duke activities. 
                With the information we have gathered regarding these different organizations, we have deveoloped
                various algorithms that will recommend certain co-curricular programs based on a student's
                interests and previous participation. Feel free to explore our website and test out our
                widgets."),
              p("If you would like to help us improve our system, please fill out the User Profile
                below or add your organization to our database. If you would like to learn more about our 
                project or provide feedback, please visit the \"About Us\" tab"),
              tags$head(tags$script(HTML(js_thanks))),
              tags$head(tags$script(HTML(js_check))),
              tags$head(tags$script(HTML(js_exists))),
              useShinyjs(),
              div(
                box(title = "User Profile", status = "primary", width = 12,
                    solidHeader = TRUE, collapsible = TRUE,
                    # Include clarifying text
                    helpText("Please enter your information into all of the following fields."),
                    textOutput("check"),
                    column(width = 4,
                           textInput("netid", label = h4("Net ID"), placeholder = "Ex. abc123"),
                           selectInput("major", label = h4("Major(s)"),
                                       choices = maj_choice,
                                       multiple = TRUE),
                           helpText("*Please select up to 3 majors. If you are unsure, simply select \"Undeclared\"."),
                           textInput("year", label = h4("Admit Year"), placeholder = "Ex. 2016, if Class of 2020"),
                           helpText("*Please type the year in which you matriculated at Duke.")
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
                           selectInput("yr4prog", label = h4("Programs - Year 4+"),
                                       choices = prog_choice,
                                       multiple = TRUE),
                           helpText("*Please select the programs that you have participated in during each year 
                                    you have been at Duke. Summer programs are counted under the academic year
                                    you finished directly prior to the program. If you have not completed a specific
                                    year yet, please select \"NA\".")
                    ),
                    actionButton("submit", "Submit")
                )
              )
      ),
      tabItem(tabName = "favorites",
              h2("Favorites")),
      tabItem(tabName = "widgets",
              h2("Widgets"),
              p("Discover new Duke co-curricular activities with the widgets below! 
                If you have already completed your user profile, you are able to use
                our Co-Curricular Recommender. If you have participated in only one
                or two co-curricular activities here at Duke, we would recommend that
                you begin by using the Find Similar Co-Curriculars Widget."),

              # Hybrid Recommender Widget
              tags$head(tags$script(HTML(js_record))),
              useShinyjs(),
              div(
                box(title = "Co-Curricular Recommender", status = "primary",
                    solidHeader = TRUE, width = 12, collapsible = TRUE, 
                    column(width = 4,
                           # Include clarifying text
                           helpText("If you have already completed your user profile, you are ready
                                    to receive recommendations! Please enter your Duke NetID 
                                    below which you used to create your profile."),
                           textInput("recID", label = h3("Enter your NetID"), placeholder = "Ex. abc123"),
                           actionButton("recGo", "Recommend!")
                           ),
                    column(width = 8,
                           tableOutput("table")
                    ),
                    collapsed = TRUE
                    )
              ),
              # Jaccard Similarity Recommender Widget
              box(title = "Find Similar Co-Curriculars", status = "primary",
                  solidHeader = TRUE, width = 12, collapsible = TRUE,
                  column(width = 4,
                         helpText("Enter the co-curricular program for which you would like to
                                  see similar activities."),
                         selectInput("recProg", label = h3("Enter a Program"),
                                     choices = prog_choice),
                         actionButton("recGo2", "Recommend!")
                         ),
                  column(width = 8,
                         tableOutput("table2")
                  ),
                  collapsed = TRUE
              )
              ),
      tabItem(tabName = "about",
              h2("About Us"),
              p("We are a team of Duke undergraduate students currently working on a", 
                a("Data+", href = "https://bigdata.duke.edu/data"), "project in collaboration 
                with Duke's Office of Information Technology (O.I.T.). Duke University is an 
                exciting and ever-changing institution, but as Duke's massive co-curricular 
                environment grows it can become more difficult for undergraduates to navigate 
                this complicated academic and extracurricular landscape. Therefore, our 
                goal is to create an 'e-advisor' tool that will help students determine
                which co-curricular activities are well-suited for their interests. In
                order to accomplish this, we hope to gather data that will allow us to
                track a student's co-curricular 'pathway,' a map of the various 
                activities the student was involved in during their time at Duke. With
                this data, we plan to improve the current recommendation system running
                behind this initial version of our 'e-advisor.'"),
              p("Thank you for helping us with this task! 
                If you have any questions, please visit our ", 
                a("website", href = "https://bigdata.duke.edu/projects/co-curricular-technology-pathways-e-advisor"), 
                " or feel free to contact us ."),
              h3("Contact Information"),
              p("Data+ Director: Paul Bendich"),
              p("Project Manager: Lindsay Berry"),
              p("Project Team Members: Alec Ashforth, Brooke Keene, Vincent Liu, Dezmanique Martin"),
              p("Project Clients: Michael Faber, Evan Levine")
              )
      )
      )

ui <- dashboardPage(header, sidebar, body)

shinyUI(ui)