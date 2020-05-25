library(shiny)
library(readr)
require(deSolve)
require(pracma)
require(ggplot2)
require(dplyr)

#-----------------------------DATA------------------------#
Data <- read_csv("Data.csv")

suceptibles <- (Data$Population - Data$`New infections of HIV/AIDS (new cases of HIV infection)`)
infectados <- ((Data$`Number of people living with HIV (x10) (tens of people living with HIV)`*10) + Data$`New infections of HIV/AIDS (new cases of HIV infection)`)
total <- (suceptibles+infectados)


#--------------------------------------UI-------------------#
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("VIH Simulator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            selectInput(inputId = "pais", label = strong("Indice de paises"),
                        choices = unique(Data$Pais),
                        selected = "Colombia"),
            
            selectInput(inputId = "poblacion", label = strong("Indice de tipo de poblacion"),
                        choices = c("Infectados", "Susceptibles"),
                        selected = "Infectados"),
            
            selectInput(inputId = "metodo", label = strong("Indice de metodos"),
                        choices = c("Runge-Kutta", "Adams"),
                        selected = "Runge-Kutta"),
            
            sliderInput("alfa",
                        "Valor de alfa:",
                        min = 0,
                        max = 0.01,
                        value = 0),
            
            sliderInput("inicial",
                        "Poblacion infectada inicial:",
                        min = round(min(infectados),0),
                        max = round(max(infectados),0),
                        value = 9000),
            sliderInput("inicialS",
                        "Poblacion suceptible inicial:",
                        min = round(min(suceptibles),0),
                        max = round(max(suceptibles),0),
                        value = 2)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput(outputId = "linePlot"),
           tableOutput(outputId = "table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    choose_poblacion <- reactive({
        
        alfa <- input$alfa
        
        if(input$poblacion == "Infectados"){
            func <- function(x, y){
                return (alfa*x*y)
            }
            func
        }else{
            func <- function(x, y){
                return (-alfa*x*y)
            }
            func
        }
    })
    
    update_table <- reactive({
        alfa <- input$alfa
        yInicial <-input$inicial
        func <- choose_poblacion()
        res <-  choose_method()
        
        df <- data.frame((res$x+1989), res$y, "experimental")
        names(df)<- c("anio", "infectados", "tipo")
        
        
        DataFiltered <- (Data %>% filter(Pais == input$pais))
        infectados <- choose_dataP() 
            
        df2 <- data.frame((res$x+1989), infectados, "Exacto")
        names(df2)<- c("anio", "infectados", "tipo")
        
        dfd <- rbind(df, df2)
    })
    
    
    choose_method <- reactive({
        
        alfa <- input$alfa
        yInicial <-input$inicial
        func <- choose_poblacion()
        
        if(input$poblacion == "Susceptibles"){
            yInicial <- input$inicialS
        }
        
        if(input$metodo == "Runge-Kutta"){
            res <- rk4(func, 1, 28, yInicial, 27)
            res
        }else{
            res <- abm3pc(func, 1, 28, yInicial, 27)
            res
        }
    })
    
    choose_dataP <- reactive({
        
        DataFiltered <- (Data %>% filter(Pais == input$pais))
        infectados <- ((DataFiltered$`Number of people living with HIV (x10) (tens of people living with HIV)`*10) + DataFiltered$`New infections of HIV/AIDS (new cases of HIV infection)`)
        suceptibles <- (DataFiltered$Population - infectados)
        
        if(input$poblacion == "Infectados"){
            infectados
        }else{
            suceptibles
        }
    })
    
    output$linePlot <- renderPlot({
        # generate bins based on input$bins from ui.R

        #func <- choose_poblacion()
        res <-  choose_method()
        
        df <- data.frame((res$x+1989), res$y, "experimental")
        names(df)<- c("anio", "infectados", "tipo")
        
        DataFiltered <- (Data %>% filter(Pais == input$pais))
        infectados <- choose_dataP()
        df2 <- data.frame((res$x+1989), infectados, "Exacto")
        names(df2)<- c("anio", "infectados", "tipo")
        
        dfd <- rbind(df, df2)
        
        p <- ggplot(dfd, aes(x = anio, y = infectados, colour = tipo)) 
        p <- p +geom_point(size = 3) + facet_grid(tipo ~ .) + theme_light()
        p
    })
    
    output$table <- renderTable({
        
        res <-  choose_method()
        
        infectadosExactos <- (update_table() %>% filter(tipo == "Exacto"))
        infectadosExperi <- (update_table() %>% filter(tipo == "experimental"))
        
        errorAbs <- abs(infectadosExactos$infectados - infectadosExperi$infectados)
        errorRel <- abs(infectadosExactos$infectados - infectadosExperi$infectados)/infectadosExactos$infectados
        
        error <- data.frame((res$x+1989), infectadosExactos$infectados, infectadosExperi$infectados, errorAbs, errorRel)
        names(error) <- c("anio", paste(input$poblacion,"reales (tomados de Global Health Data Exchange)", sep = " "), paste(input$poblacion, "experimentales (calculados)", sep = " "), "error absoluto", "error relativo")
        error
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
