#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# For the proper code working, setwd("") has to point to the dirrectory where this file is located, 
# and this dirrectory has to contain the folder "data" with the necessary ".csv" tables. 


library(shiny)

library(data.table)
library(plotly)

library(magrittr)
library(stringr)

setwd("")

get_years <- function(file_pattern){ # prihlasky, zajemci
  
  fls <- list.files(path = "./data/",
                    pattern = ".csv",
                    full.names = FALSE)
  
  fls_years <- fls[grep(pattern = file_pattern,
                        x = fls)] %>% 
    strsplit("-") 
  
  output_list <- list()
  
  for(i in 1:length(fls_years)){
    output_list <- c(output_list, as.numeric(fls_years[[i]][2]))
  }
  output_vector <- unlist(output_list)
  return(output_vector)
}


# get_years("prih")
# get_years("zaj")


import_table <- function(file_pattern, file_year){
  fls <- list.files(path = "./data",
                    pattern = ".csv",
                    full.names = TRUE)
  
  
  fls <- fls[grep(pattern = file_pattern,
                  x = fls)]
  fls <- fls[grep(pattern = file_year,
                  x = fls)]
  
  dta <- fread(fls)
  
  dta$`Program wrap` <- str_wrap(dta$`Program název`, width = 12)
  return(dta)
}

plot_student_program <- function(table_filtered, plot_title){
  
  
  dta <- table_filtered[, .(n_students = .N),
                        by = .(program = `Program wrap`)]
  
  dta$abbrProg <- abbreviate(dta$program, minlength = 1)
  
  
  plot_ly(dta, x = ~abbrProg, y = ~n_students, type = "bar", color = ~program) %>%
    layout(
      title = paste0("<em><b>", plot_title, "</b></em>"), 
      plot_bgcolor = "#d9d9d9", 
      xaxis = list(title = 'Program'), 
      yaxis = list(title = 'Students number'), 
      legend = list(
        title=list(text='<b> Programs </b>')
      )
    )
  
}










ui <- fluidPage(
  
  titlePanel("Project"),
  
  tabsetPanel(
    id = "nav",
    tabPanel("Applied", 
             
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year_slider_applied", 
                             "Year:",
                             min = min(get_years("prih")),
                             max = max(get_years("prih")),
                             value = max(get_years("prih")),
                             animate = animationOptions(loop = TRUE)
                 ), width = 3
               ),
               
               # Show a plot
               mainPanel(
                 plotlyOutput("plot_applied", height = "600px"), width = 9
               ),
             )),
    tabPanel("Admited", 
             
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year_slider_admited", 
                             "Year:",
                             min = min(get_years("prih")),
                             max = max(get_years("prih")),
                             value = max(get_years("prih")),
                             animate = animationOptions(loop = TRUE)
                 ), width = 3
               ),
               
               # Show a plot
               mainPanel(
                 plotlyOutput("plot_admited"), width = 9
               ),
             )),
    tabPanel("Rejected", 
             
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year_slider_rejected", 
                             "Year:",
                             min = min(get_years("prih")),
                             max = max(get_years("prih")),
                             value = max(get_years("prih")),
                             animate = animationOptions(loop = TRUE)
                 ), width = 3
               ),
               
               # Show a plot
               mainPanel(
                 plotlyOutput("plot_rejected"), width = 9
               ),
             )),
    
    
  )
)





admitedF <- function(year){
  dta <- import_table("prih", year)
  admited <- (dta[`Rozh. o přijetí - název` == "10 - Přijat"])
}
rejectedF <- function(year){
  dta <- import_table("prih", year)
  rejected <- dta[`Rozh. o přijetí - název` != "10 - Přijat"]
  
}
appliedF <- function(year){
  dta <- import_table("prih", year)
}





server <- function(input, output){
  
  output$plot_admited <- renderPlotly(
    
    plot_student_program(admitedF(input$year_slider_admited), "Admited")
  )
  
  output$plot_rejected <- renderPlotly(
    
    plot_student_program(rejectedF(input$year_slider_rejected), "Rejected")
  )
  
  output$plot_applied <- renderPlotly(
    
    plot_student_program(appliedF(input$year_slider_applied), "Applied")
  )
}


shinyApp(ui, server)

