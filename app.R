

library(shiny)

library(slam)
library(Rglpk)

ui <- fluidPage(

  
  titlePanel(title="Optimize taxable income using linear optimization method"),
  sidebarLayout(
    sidebarPanel(
      textInput("Gross_Salary","Monthly Total Income","82500"),
      shiny::submitButton(text = "Apply")
    ),mainPanel(
      tableOutput("odf")
      # p("Optimam Taxable Income (Basic): "), textOutput("Taxable"),
      # p("House Rent: "), textOutput("House_Rent"),
      # p("Medical: "), textOutput("Medical"),
      # p("Conveyance: "), textOutput("Convey")
    )
  )
)

# Define server logic required to draw a histogram
server <-   function(input,output){
  
  output$odf<-renderTable({
    obj <- c(1, -1,-1,-1)
    mat <- matrix(c(-0.5,-0.1,1.5,0.1,0.5,1.1,0.5,0.1), nrow = 2)
    dir <- c("<=", "<=")
    rhs <- c(0, 0)
    types <- c("C", "C","C","C")
    max <- FALSE
    gs<- as.integer(input$Gross_Salary)
    bounds <- list(lower = list(ind = c(1L,2L,3L,4L), val = c(gs,1000,1000,500)),
                   upper = list(ind = c(1L,2L,3L, 4L), val = c(gs,25000,5000,2500)))
    r<-Rglpk_solve_LP(obj=obj, mat=mat, dir=dir, rhs=rhs, bounds=bounds, types=types, max=max)
    
    Total_Income<- c(r$solution[1])
    Obtimam_Taxable_Income<- c(r$optimum)
    House_Rent<- c( r$solution[2])
    Medical<- c(r$solution[3])
    Conveyance<- c(r$solution[4])
    data.frame(Total_Income,Obtimam_Taxable_Income,House_Rent,Medical,Conveyance)   
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
