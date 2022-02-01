#-----
# Title: Developing an interactive dashboard with R shinydashboard
# Author: Lucas Augusto Pereira
# Date: 08_d31_2021
#-----

# Loading packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(here)
library(plotly)
library(highcharter)

# Importing .csv with our data about sales ----------------------------------------------------------------
dash.data <- readr::read_delim(here::here("app.data.csv"), delim = ";")


# Data wrangling-------------------------------------------------------------------------------------------

# Getting the total value of sales for year 2019
sales.total <- dash.data %>% 
    dplyr::select(valor) %>%
    dplyr::summarise(
        valor.total = sum(valor)) %>%
    print(valor)

# Getting the month with most sales (for KPI and barplot)
sales.month <- dash.data %>%
    dplyr::select(mes, valor) %>%
    dplyr::group_by(mes) %>%
    dplyr::summarise(
        valor.mes = sum(valor)) 

# ordering values
sales.month.desc <- sales.month %>%
    dplyr::arrange(desc(valor.mes)) %>% 
  print(valor.mes)

# doing a tibble with the months extended as name not number
sales.month.tb <- tibble(
    mes.ex = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
            "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"),
    valor = sales.month$valor.mes)

# Getting the store with most sales
sales.store <- dash.data %>%
    dplyr::select(filial, valor) %>%
    dplyr::group_by(filial) %>%
    dplyr::summarise(
        valor.filial = sum(valor)) %>%
    dplyr::arrange(desc(valor.filial)) %>%
    print(valor.filial)


# Getting de values by store--------------------------------------------------


# Getting the month with most sales by store (store 1)
sales.month.store1 <- dash.data %>%
  dplyr::select(mes, filial, valor) %>%
  dplyr::group_by(mes, filial) %>%
  dplyr::summarise(
    valor.mes = sum(valor)) %>% 
  dplyr::filter(filial == "filial_1")

# doing a tibble with the months extended as name not number
month.store1.tb <- tibble(
  mes.ex = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
             "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"),
  valor = sales.month.store1$valor.mes)


# Getting the month with most sales by store (store 2)
sales.month.store2 <- dash.data %>%
  dplyr::select(mes, filial, valor) %>%
  dplyr::group_by(mes, filial) %>%
  dplyr::summarise(
    valor.mes = sum(valor)) %>% 
  dplyr::filter(filial == "filial_2")

# doing a tibble with the months extended as name not number
month.store2.tb <- tibble(
  mes.ex = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
             "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"),
  valor = sales.month.store2$valor.mes)


# Getting the month with most sales by store (store 3)
sales.month.store3 <- dash.data %>%
  dplyr::select(mes, filial, valor) %>%
  dplyr::group_by(mes, filial) %>%
  dplyr::summarise(
    valor.mes = sum(valor)) %>% 
  dplyr::filter(filial == "filial_3")

# doing a tibble with the months extended as name not number
month.store3.tb <- tibble(
  mes.ex = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
             "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"),
  valor = sales.month.store3$valor.mes)

# getting the quantity of products sold by category and by store-------------------------------------------

# store 1
prd.store1 <- dash.data %>% 
  dplyr::filter(filial == "filial_1") %>% 
  dplyr::group_by(categoria) %>% 
  dplyr::count(categoria, sort = TRUE)

# store 2
prd.store2 <- dash.data %>% 
  dplyr::filter(filial == "filial_2") %>% 
  dplyr::group_by(categoria) %>% 
  dplyr::count(categoria, sort = TRUE)

# store 3
prd.store3 <- dash.data %>% 
  dplyr::filter(filial == "filial_3") %>% 
  dplyr::group_by(categoria) %>% 
  dplyr::count(categoria, sort = TRUE)


# User Interface for the application-----------------------------------------------------------------------
ui <- dashboardPage(
    skin = "blue",

    dashboardHeader(
        title = "Receita Anual - 2019",
        titleWidth = 300
    ),

    dashboardSidebar(
        width = 200,
        sidebarMenu(

            menuItem("Informações gerais", tabName = "tabOne", icon = icon("list-alt")
            ),

            menuItem("Filial 1", tabName = "tabTwo", icon = icon("store")
            ),

            menuItem("Filial 2", tabName = "tabThree", icon = icon("store")
            ),

            menuItem("Filial 3", tabName = "tabFour", icon = icon("store")
            )

        )
    
    ),

    dashboardBody(
        tabItems(
            tabItem("tabOne",
                fluidRow(
                    valueBox(
                    "R$ 204.920,00", "Receita do ano", icon = icon("cash-register"), color = "teal"
                    ),
                    valueBox(
                    "R$ 46.500,00", "Mês com maior receita: Novembro", icon = icon("calendar"), color = "blue"
                    ),
                    valueBox(
                    "R$ 80.750,00", "Filial com maior receita: Filial 2", icon = icon("store"), color = "teal"
                    )
                ),
                fluidRow(
                    box(
                        width = 7,
                        plotlyOutput(
                            "p.all",
                            height = 500
                        )
                    ),
                    box(
                        width = 5,
                        plotlyOutput(
                            "p.store",
                            height = 500)
                    )
                )
            ),
          tabItem("tabTwo",
                  fluidRow(
                    valueBox(
                    "R$ 55.770,00", "Receita anual da filial 1", icon = icon("cash-register"), color = "teal"
                    ),
                    valueBox(
                      "R$ 11.200,00", "Mês com maior receita: Dezembro", icon = icon("calendar"), color = "blue"
                    ),
                    valueBox(
                      "R$ 400,00", "Mês com menor receita: Agosto", icon = icon("calendar"), color = "teal"
                    )
                  ),
                  fluidRow(
                      box(
                          width = 7,
                          plotlyOutput(
                          "p.year.store1",
                          height = 500
                          )
                    ),
                      box(
                          width = 5,
                          plotlyOutput(
                          "p.cate.store1",
                          height = 500)
                      )
                    )
                  ),
          tabItem("tabThree",
                  fluidRow(
                    valueBox(
                      "R$ 80.750,00", "Receita anual da filial 2", icon = icon("cash-register"), color = "teal"
                    ),
                    valueBox(
                      "R$ 24.500,00", "Mês com maior receita: Março", icon = icon("calendar"), color = "blue"
                    ),
                    valueBox(
                      "R$ 500,00", "Mês com menor receita: Julho", icon = icon("calendar"), color = "teal"
                    )
                  ),
                  fluidRow(
                    box(
                        width = 7,
                        plotlyOutput(
                        "p.year.store2",
                        height = 500
                        )
                    ),
                    box(
                        width = 5,
                        plotlyOutput(
                        "p.cate.store2",
                        height = 500
                        )
                    )
                  )
          ),
          tabItem("tabFour",
                  fluidRow(
                   valueBox(
                     "R$ 68.400,00", "Receita anual da filial 3", icon = icon("cash-register"), color = "teal"
                   ),
                   valueBox(
                     "R$ 23.800,00", "Mês com maior receita: Novembro", icon = icon("calendar"), color = "blue"
                   ),
                   valueBox(
                     "R$ 600,00", "Mês com menor receita: Junho", icon = icon("calendar"), color = "teal"
                   )
                  ),
                  fluidRow(
                    box(
                      width = 7,
                      plotlyOutput(
                      "p.year.store3",
                      height = 500
                      )
                    ),
                    box(
                        width = 5,
                        plotlyOutput(
                        "p.cate.store3",
                        height = 500
                        )
                    )
                  )
          )
        )
    )
)





# Server logic required for the application------------------------------------------------------------------
server <- function(input, output) { 

# tabOne
    output$p.all <- renderPlotly({
        #The default order will be alphabetized unless specified as below:
        sales.month.tb$mes.ex <- factor(sales.month.tb$mes.ex, 
                                        levels = sales.month.tb[["mes.ex"]]
                                        )
        p <- plot_ly(sales.month.tb, x = ~mes.ex, y = ~valor, type = "bar",
                marker = list(color = 'rgb(158,202,225)',
                              line = list(color = 'rgb(8,48,107)',
                                          width = 1.5
                                          )
                              )
                )
        p <- p %>% 
          layout(title = "Receita Mensal (2019)",
                 xaxis = list(title = ""),
                 yaxis = list(title = "Receita (R$)")
                 )
    })


    output$p.store <- renderPlotly({

        q <- plot_ly(sales.store, x = ~valor.filial, y = c("Filial 2", "Filial 3", "Filial 1"), type = "bar",
                orientation = 'h', marker = list(color = 'rgb(58,200,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5
                            )
                          )
                )
        q <- q %>%
            layout(title = "Receita Por Filial",
                 xaxis = list(title = "Receita"),
                 yaxis = list(title = "", categoryorder = "total ascending")
                 )
    })
# end tabOne

# tabTwo    
    output$p.year.store1 <- renderPlotly({
        month.store1.tb$mes.ex <- factor(month.store1.tb$mes.ex, 
                                        levels = month.store1.tb[["mes.ex"]]
                                        )
        
        r <- plot_ly(month.store1.tb, x = ~mes.ex, y = ~valor, type = "bar",
                     marker = list(color = 'rgb(158,202,225)',
                                   line = list(color = 'rgb(8,48,107)',
                                               width = 1.5
                                              )
                                  )
                    )
        r <- r %>% 
          layout(title = "Receita Mensal - Filial 1 (2019)",
                 xaxis = list(title = ""),
                 yaxis = list(title = "Receita (R$)")
          )
    })
    
    
    output$p.cate.store1 <- renderPlotly({
      
        c <- plot_ly(prd.store1, x = ~n, y = c("Computador", "Smartphone", "Notebook", "Impressora"), type = "bar",
                     orientation = 'h', marker = list(color = 'rgb(58,200,225)',
                                                      line = list(color = 'rgb(8,48,107)', width = 1.5))
        )
        c <- c %>%
          layout(title = "Produtos Vendidos Por Categoria",
                 xaxis = list(title = "Quantidade"),
                 yaxis = list(title = "", categoryorder = "total ascending")
          )
    })
# end tabTwo    

# tabThree   
    output$p.year.store2 <- renderPlotly({
      month.store2.tb$mes.ex <- factor(month.store2.tb$mes.ex, 
                                       levels = month.store2.tb[["mes.ex"]]
                                      )
      
      j <- plot_ly(month.store2.tb, x = ~mes.ex, y = ~valor, type = "bar",
                   marker = list(color = 'rgb(158,202,225)',
                                 line = list(color = 'rgb(8,48,107)',
                                             width = 1.5
                                 )
                   )
      )
      j <- j %>% 
        layout(title = "Receita Mensal - Filial 2 (2019)",
               xaxis = list(title = ""),
               yaxis = list(title = "Receita (R$)")
        )
    })
    
    output$p.cate.store2 <- renderPlotly({
      
      v <- plot_ly(prd.store2, x = ~n, y = c("Computador", "Smartphone", "Notebook", "Impressora"), type = "bar",
                   orientation = 'h', marker = list(color = 'rgb(58,200,225)',
                                                    line = list(color = 'rgb(8,48,107)', width = 1.5))
      )
      v <- v %>%
        layout(title = "Produtos Vendidos Por Categoria",
               xaxis = list(title = "Quantidade"),
               yaxis = list(title = "", categoryorder = "total ascending")
        )
      
    })
# end tabThree
        
    
# tabFour
    output$p.year.store3 <- renderPlotly({
      
      month.store3.tb$mes.ex <- factor(month.store3.tb$mes.ex, 
                                       levels = month.store3.tb[["mes.ex"]]
      )
      
      k <- plot_ly(month.store3.tb, x = ~mes.ex, y = ~valor, type = "bar",
                   marker = list(color = 'rgb(158,202,225)',
                                 line = list(color = 'rgb(8,48,107)',
                                             width = 1.5
                                 )
                   )
      )
      k <- k %>% 
        layout(title = "Receita Mensal - Filial 3 (2019)",
               xaxis = list(title = ""),
               yaxis = list(title = "Receita (R$)")
        )
    })
    
    output$p.cate.store3 <- renderPlotly({
      
      a <- plot_ly(prd.store3, x = ~n, y = c("Notebook", "Smartphone", "Computador", "Impressora"), type = "bar",
                   orientation = 'h', marker = list(color = 'rgb(58,200,225)',
                                                    line = list(color = 'rgb(8,48,107)', width = 1.5))
      )
      a <- a %>%
        layout(title = "Produtos Vendidos Por Categoria",
               xaxis = list(title = "Quantidade"),
               yaxis = list(title = "", categoryorder = "total ascending")
        )
    })
}




# Run the application----------------------------------------------------------------------------------------- 
shinyApp(ui = ui, server = server)