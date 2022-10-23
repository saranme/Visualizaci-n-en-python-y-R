# Análisis gráfico de los cultivos en la Unión Europea

library(tidyverse)
library(eurostat)
library(rvest)
library(knitr)
library(data.table)
library(shiny)
library(ggplot2)
library("ggrepel")
library(stringr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  ########## Carga de datos  ############
  # carga de datasets de Eurostat
  toc <- get_eurostat_toc()
  # carga de precios de cultivoos
  prices <- get_eurostat("apri_ap_crpouta", time_format = "date", type="label")
  price <- filter(prices, values != 0)
  # carga de cantidad producida de cultivos
  production <- get_eurostat("apro_cpsh1", time_format = "date", type="label")
  prod <- production %>% filter(strucpro == 'Harvested production in EU standard humidity (1000 t)')
  
  ########## Limpieza de datos  ############
  # limpieza de dataset de precios de cultivos
  # 1 conversión a una única moneda: Euro
  euro <- function(x, output) {
    if (x['currency'] != 'Euro') {
      if (x['geo'] == 'Bulgaria') { 
        A = round(as.numeric(x[5]) * 0.51133,2)}
      else if (x['geo'] == 'Latvia') { 
        A = round(as.numeric(x[5]) * 1.54,2)}
      else if (x['geo'] == 'Czechia') { 
        A = round(as.numeric(x[5]) * 0.040444697,2)}
      else if (x['geo'] == 'Croatia') { 
        A = round(as.numeric(x[5]) * 0.1322239,2)}
      else if (x['geo'] == 'Poland') { 
        A = round(as.numeric(x[5]) * 0.2181165,2)}
      else if (x['geo'] == 'Romania') { 
        A = round(as.numeric(x[5]) * 0.20222646,2)}
      else if (x['geo'] == 'Sweden') { 
        A = round(as.numeric(x[5]) * 0.095152994,2)}
      else if (x['geo'] == 'Lithuania') { 
        A = round(as.numeric(x[5]) * 0.28962002,2)}
      else if (x['geo'] == 'Hungary') { 
        A = round(as.numeric(x[5]) * 0.0025290582,2)}
      else if (x['geo'] == 'United Kingdom') {
        A = round(as.numeric(x[5]) * 1.174087,2)}
      else if (x['geo'] == 'Kosovo (under United Nations Security Council Resolution 1244/99)') {
        A = round(as.numeric(x[5]) * 0.0085094766,2)}
      else { A = as.numeric(x[5])}
    }
    else {A = as.numeric(x[5])}
    return(A)
  }
  price$values <- apply(price,1,euro)
  
  # 2. filtrado de los cultivos para analizar: los cinco más caros y los cinco más económicos
  price_2020 <- filter(price, time == as.Date('2020-01-01'))
  top_price_2020 <- price_2020 %>% group_by(prod_veg) %>% summarise(mean = mean(values))
  price2 <-top_price_2020[order(top_price_2020$mean, decreasing = TRUE),]
  price3 <- filter(price, prod_veg %in% c(# expensive ones
    "Raspberries - prices per 100 kg",
    
    "Hops : all varieties - prices per 100 kg",
    
    "Extra virgin olive oil - prices per 100 litres",
    "Lampante virgin olive oil - prices per 100 litres",
    "Virgin olive oil - prices per 100 litres",
    "Virgin olive oil - fine - prices per 100 litres" ,
    "Ordinary virgin olive oil - semi-fine/corrente - prices per 100 litres",
    
    "Strawberries in the open - prices per 100 kg",
    "Walnuts - prices per 100 kg",
    
    # cheapest ones
    "Sorghum - prices per 100 kg",
    "Maize - prices per 100 kg",
    "Rye - prices per 100 kg",
    
    "Malting barley - prices per 100 kg",
    "Feed barley - prices per 100 kg",
    "Barley - prices per 100 kg",
    
    "Triticale - prices per 100 kg"
  ))
  
  
  # 3. reemplazo de nombres de cultivos por nombres cultivos genéricos
  price3$prod_veg[price3$prod_veg== "Raspberries - prices per 100 kg"] <- "Raspberries"
  
  price3$prod_veg[price3$prod_veg== "Hops : all varieties - prices per 100 kg"] <- "Hops"
  
  price3$prod_veg[price3$prod_veg== "Extra virgin olive oil - prices per 100 litres"] <- "Olive oil"
  price3$prod_veg[price3$prod_veg== "Lampante virgin olive oil - prices per 100 litres"] <- "Olive oil"
  price3$prod_veg[price3$prod_veg== "Virgin olive oil - prices per 100 litres"] <- "Olive oil"
  price3$prod_veg[price3$prod_veg== "Virgin olive oil - fine - prices per 100 litres"] <- "Olive oil"
  price3$prod_veg[price3$prod_veg== "Ordinary virgin olive oil - semi-fine/corrente - prices per 100 litres"] <- "Olive oil"
  
  price3$prod_veg[price3$prod_veg== "Strawberries in the open - prices per 100 kg"] <- "Strawberries"
  
  price3$prod_veg[price3$prod_veg== "Walnuts - prices per 100 kg"] <- "Walnuts"
  
  
  price3$prod_veg[price3$prod_veg== "Sorghum - prices per 100 kg"] <- "Sorghum"
  price3$prod_veg[price3$prod_veg== "Maize - prices per 100 kg"] <- "Maize"
  price3$prod_veg[price3$prod_veg== "Rye - prices per 100 kg"] <- "Rye"
  
  price3$prod_veg[price3$prod_veg== "Barley - prices per 100 kg"] <- "Barley"
  price3$prod_veg[price3$prod_veg== "Feed barley - prices per 100 kg"] <- "Barley"
  price3$prod_veg[price3$prod_veg== "Malting barley - prices per 100 kg"] <- "Barley"
  
  price3$prod_veg[price3$prod_veg== "Triticale - prices per 100 kg"] <- "Triticale"
  
  # 4. agrupar a los cultivos con el mismo nombre genérico
  price4 <- price3 %>%  group_by(prod_veg, geo, time) %>% summarize(values = mean(values), .groups = 'drop')
  # 5. renombre de columnas
  price3 <- price4 %>% rename(crops=prod_veg, prices=values)
  
  # limpieza de dataset de cantidades de cultivos producidos
  # 1. filtrado de fechas para tener las mismas en ambos datasets
  prod2 <- filter(prod, (time != as.Date("2022-01-01")))
  prod <- filter(prod2, (time != as.Date("2021-01-01")))
  # 2. filtrado de cultivos seleccionados 
  prod3 <- filter(prod, crops %in% c(# expensive ones
    "Raspberries",
    "Hops",
    "Olives for oil",
    "Strawberries",
    "Walnuts",
    
    # cheapest ones
    "Sorghum",
    "Green maize",
    "Rye",
    "Barley",
    "Triticale"
  ))
  
  
  # 3. reemplazo de nombres de cultivoos por cultivos genéricos
  prod3$crops[prod3$crops== "Olives for oil"] <- "Olive oil"
  prod3$crops[prod3$crops== "Green maize"] <- "Maize"
  
  # 4. selección de variables del dataset
  prod2 <- prod3[c("crops","geo","time","values")]
  
  # 5. renombramiento de columnas
  prod3 <- prod2 %>% rename(production=values)
  
  ########## Creación de dataframe final  ############
  # unión de datasets en un único dataframe
  df<- merge(price3,prod3, by= c("crops","geo","time"))
  df$geo[df$geo== "Germany (until 1990 former territory of the FRG)"] <- "Germany"
  df$geo[df$geo== "Kosovo (under United Nations Security Council Resolution 1244/99)"] <- "Kosovo"
  # eliminación de nulos
  df_ <- na.omit(df) 
  
  ########## Dataframe con datos de 2020  ############
  df_2020 <- filter(df_, time == as.Date('2020-01-01'))
  ########## Dataframe de la producción total y el precio medio de cada cultivo  ############
  crops_2020 <- df_2020 %>% group_by(crops) %>% summarise(total_production = sum(production), mean_price = mean(prices))
  stats <- data.table(crops_2020)
  
  ########## tab1 ############
  # variable para ejecutar el zoom 
  ranges <- reactiveValues(x = NULL, y = NULL)
  # primera gráfica 
  output$plot1 <- renderPlot({
    ########## Plot data
    first_plot <- 
      ggplot(stats, aes(x= mean_price, y= total_production,color = crops, label=crops)) +
        geom_point(aes(size = total_production),
                   show.legend= FALSE) + 
        geom_label_repel(fill = "white", 
                         xlim = c(-Inf, Inf), 
                         ylim = c(-Inf, Inf),
                         show.legend = FALSE) +
      
      xlab("Average price in € per 100 kg (or l)") +
      ylab('Quantity produced in tons') +
      ggtitle(paste0("Quantity and average price\n per crop in 2020")) +
      scale_y_continuous(expand=expansion(mult=c(0.5,0.4)),labels = scales::comma) +
      scale_x_continuous(expand=expansion(mult=c(0.2,0.2))) +
      
      theme(panel.background = element_rect(fill='transparent'),
            panel.grid.major = element_line(size = 0.1, 
                                            linetype = 'solid',
                                            colour = "grey"), 
            panel.grid.minor = element_line(size = 0.1, 
                                            linetype = 'solid',
                                            colour = "grey"),
            axis.title.x = element_text(margin=margin(30,0,0,0)),
            axis.title.y = element_text(margin=margin(0,20,0,0)),
            plot.title = element_text(face ="bold", size = 15, hjust = 0.5, margin = margin(20,10,20,0)))
    
    first_plot
  })
  
  # segunda gráfica
  output$plot2 <- renderPlot({
    ########## Plot data
    first_plot <- 
      ggplot(stats, aes(x= mean_price, y= total_production,color = crops, label=crops)) +
        geom_point(aes(size = total_production),
                   show.legend= FALSE) + 
        geom_label_repel(fill = "white", 
                         xlim = c(-Inf, Inf), 
                         ylim = c(-Inf, Inf),
                         show.legend = FALSE) +
      
      xlab('Average price in € per 100 kg (or l)') +
      ylab('Quantity produced in tons') +
      ggtitle(paste0("Quantity and average price\n per crop in 2020")) +
      scale_y_continuous(expand=expansion(mult=c(0.5,0.4)),labels = scales::comma) +
      scale_x_continuous(expand=expansion(mult=c(0.2,0.2))) +
      theme(panel.background = element_rect(fill='transparent'),
            panel.grid.major = element_line(size = 0.1, 
                                            linetype = 'solid',
                                            colour = "grey"), 
            panel.grid.minor = element_line(size = 0.1, 
                                            linetype = 'solid',
                                            colour = "grey"),
            axis.title.x = element_text(margin=margin(30,0,0,0)),
            axis.title.y = element_text(margin=margin(0,20,0,0)),
            plot.title = element_text(face ="bold", size = 15, hjust = 0.5, margin = margin(20,10,20,0)))
    
    first_plot + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE)
  })
  
  # ejecutando zoom
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  ########## tab2  ############
  # tercera gráfica
  output$plot3 <- renderPlot({
    # dataframe de cada cultivo
    df_crop <- filter(df_, crops == input$Crop)
    stats2 <- data.table(df_crop)
    # asignación a cada cultivo su país con mayor producción
    top_prod_countries <- ifelse(input$Crop %in% c('Barley','Olive oil'), 'Spain',
                                 ifelse(input$Crop == 'Walnuts', 'Romania',
                                        ifelse(input$Crop %in% c('Maize','Hops'), 'Germany',
                                               ifelse(input$Crop %in% c('Strawberries','Rye','Triticale','Raspberries'),'Poland',
                                                      ifelse(input$Crop == 'Sorghum','Hungary','Romania')
                                               )
                                        )
                                 )
    )
    
    # dataframe con cuartiles de la cantidad producida de los cultivos 
    data_quantiles <- stats2[, 
                             list(
                               quant.25 = quantile(production, probs = 0.25, na.rm = TRUE, names = TRUE)[[1]], 
                               quant.50 = quantile(production, probs = 0.50, na.rm = TRUE, names = TRUE)[[1]], 
                               quant.75 = quantile(production, probs = 0.75, na.rm = TRUE, names = TRUE)[[1]]), by = list(time)
    ]
    
    ########## Plot data
    # colores para la leyenda de la gráfica
    colors <- c(top_prod_countries='blue',"Trend"='grey')
    
    evol <- ggplot(data = data_quantiles, aes(x = time)) + 
      # pintar el rango intercuartílico de la cantidad producida del cultivo
      geom_ribbon(aes(ymin = quant.25, ymax = quant.75, fill = 'Trend'), alpha=0.3) +
      # pintar el país con mayor producción del cultivo en 2020
      geom_line(data = stats2[geo == top_prod_countries], aes(x = time, y = production, fill=top_prod_countries), size = 1, alpha=0.4, color="blue")+
      
      # ajuste del color para el Trend y el país
      scale_fill_manual(values = c("blue","grey"), breaks = c(top_prod_countries,"Trend"))+

      theme_bw()+theme(panel.grid.major=element_line(size=0.3, colour='grey92'))+
      
      guides(fill=guide_legend(title=NULL))+
      
      xlab("") +
      ylab('Total production in tons') +
      ggtitle(paste0("Evolution of the amount produced of ", input$Crop)) +
      
      theme(axis.title.x = element_blank(),
            axis.title.y = element_text(margin=margin(0,30,0,0)),
            panel.border = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            plot.title = element_text(face ="bold", size = 15, hjust = 0.5, margin = margin(30,10,0,0)))

    # condición: posibilidad de elegir mostrar la evolución del cultivo en otro país si tiene producción
    if ((input$selectcountry == "Compare with another country") & (stats2 %>% group_by(input$secondCountry) %>% summarise(sum(production))) > 0) 
      evol+
      geom_line(data = stats2[geo == str_to_title(input$secondCountry)], aes(x = time, y = production, color=geo)) + labs(col="")
    else evol
  })
  
  # cuarta gráfica
  output$plot4 <- renderPlot({
    # dataframe de cada cultivo
    df_crop <- filter(df_, crops == input$Crop)
    df_crop_2020 <- filter(df_crop, time == as.Date('2020-01-01'))
    
    # asignación a cada cultivo su país con mayor producción
    top_prod_countries <- ifelse(input$Crop %in% c('Barley','Olive oil'), 'Spain',
                                 ifelse(input$Crop == 'Walnuts', 'Romania',
                                        ifelse(input$Crop %in% c('Maize','Hops'), 'Germany',
                                               ifelse(input$Crop %in% c('Strawberries','Rye','Triticale','Raspberries'),'Poland',
                                                      ifelse(input$Crop == 'Sorghum','Hungary','Romania')
                                               )
                                        )
                                 )
    )
    # creación de nueva variable: cuánto difiere el precio en cada país respecto al precio en el país con más producción
    my_fun <- function(x) {
      ifelse(x <= 0, 9999999999,
      x - filter(df_crop_2020, geo == top_prod_countries)$price
            )
    }
    df_crop_2020$price_diff_ro <- sapply(df_crop_2020[,4], my_fun)

    ########## Plot gráfica
    ggplot(filter(df_crop_2020, (geo != top_prod_countries) & (price_diff_ro < 9999999999)), aes(x=geo, y=price_diff_ro)) +
      geom_segment( aes(x=geo, xend=geo, y=0, yend=price_diff_ro), color="grey") +
      geom_point( color='orange', size=4) +
    
    xlab("") +
    ylab("Price difference in € for 100 kg (or l)") +
    ggtitle(paste0(input$Crop," price difference in 2020 between each country and ", top_prod_countries)) +
      
    theme_light() +  theme(
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin=margin(40,0,0,0)),
        axis.title.y = element_text(margin=margin(0,30,0,0)),
        plot.title = element_text(face ="bold", size = 15, hjust = 0.5, margin = margin(40,10,20,0)),
        axis.text.x = element_text(angle = 45)
      )
  }) 
  
})


