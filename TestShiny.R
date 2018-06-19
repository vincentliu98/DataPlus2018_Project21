# Build a website using R Shiny
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("shinyjs")
#install.packages("DT")
#install.packages("rsconnect")
#install.packages("tidyverse")
#install.packages("tm")

library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(rsconnect)
library(tidyverse)
library(tm)

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
      sidebarMenuOutput("menu"),
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Widgets", tabName = "widgets", icon = icon("th"), 
                 badgeLabel = "new", badgeColor = "green"),
        menuItem("About Us", tabName = "about", icon = icon("address-card"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",
                h2("Dashboard"),
                h3("Complete User Profile"),
                textInput("netid", label = h4("Net ID"), placeholder = "Ex. abc123"),
                selectInput("major", label = h4("Major(s)"),
                            choices = list("AMES" = "ames",
                                           "VMS" = "vms"),
                            multiple = TRUE),
                textInput("year", label = h4("Graduation Year"), placeholder = "Ex. 2020")
        ),
        tabItem(tabName = "widgets",
                h2("Widgets"),
                # Create a dropdown menu
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
                         at Duke from the drop-down menu."),
                DT::dataTableOutput("table")
                ),
        tabItem(tabName = "about",
                h2("About Us"),
                p("We are a Data+ Project."),
                
                h3("Contact Information"),
                p("Paul Bendich")
        )
      )
    )
  )


server <- function(input, output) {
  output$user_programs <- renderPrint({ input$programs })

  output$table <- DT::renderDataTable({
    #ContentBasedRec.R
        # Load Data from Excel (in csv format)
    programs_tags <- read_csv("/Users/brookekeene/Documents/Duke University/Data+/Project_21/Tag_Words.csv")
    
    # Create and Label a Document Term Matrix

    corpus <- Corpus(VectorSource(programs_tags$`Tag Words`))
    programs_df <- as.matrix(DocumentTermMatrix(corpus))
    
    program_names = programs_tags[c(1)]            # Column of Program Names
    row.names(programs_df) <- apply(program_names, MARGIN = 1, FUN = paste0)
    
    # Determine DF and IDF vectors
    DF = colSums(programs_df)                      # Document Frequency 
    total_tags = rowSums(programs_df)              # Total number of tags for a program
    N = nrow(program_names)                        # Total number of documents
    IDF = log10(N/DF)                              # Inverse Document Frequency
    
    # Create a data frame for a student's participation using their input
    students_df = data.frame(matrix(0,N,1))
    row.names(students_df) <- apply(program_names, MARGIN = 1, FUN = paste0)
    student_progs <- input$programs
    for(i in student_progs) {
      i = strtoi(i)
      students_df[i,1] = 1
    }
    
    # Normalize Data
    norm_prog = sweep(programs_df, 1, sqrt(total_tags), "/")
    
    # Create matrix of student profiles
    stud_profiles = matrix(NA, nrow = ncol(students_df), ncol = ncol(norm_prog))
    for(i in 1:ncol(students_df)) {
      stud_profiles[i,] = apply(norm_prog, 2, function(x){t(x)%*%students_df[,i]})
    }
    
    # Create matrix of weighted scores of tags for each program
    weighted_scores = t(apply(norm_prog, 1, function(x){x*IDF}))
    
    # Create matrix of student predictions based on weighted scores
    stud_predictions = matrix(NA, nrow = nrow(programs_df), ncol = ncol(students_df))
    row.names(stud_predictions) <- apply(program_names, MARGIN = 1, FUN = paste0)
    for(c in 1:ncol(students_df)) {
      for(r in 1:nrow(programs_df)) {
        stud_predictions[r,c] = sum(stud_profiles[c,]*weighted_scores[r,])
      }
    }
    
    # Factor in Programs that Students Have Already Participated in
    for(i in 1:N) {
      if(students_df[i,] == 1) {
        stud_predictions[i] = -1 # change to -1
      }
    }
    #stud_predictions = stud_predictions[-which(stud_predictions %in% -1)]
    DT::datatable(stud_predictions)
  })
}

shinyApp(ui = ui, server = server)



