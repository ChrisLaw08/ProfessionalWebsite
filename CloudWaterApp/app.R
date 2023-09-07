 
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(latex2exp)
library(glue)
library(ggtext)

# Read in the data ####



clouddata<- vroom::vroom("AllCloudandMetData.csv")%>%
  rename(pH = LABPH, Na = Sodium, Ca = CA, Mg = MG, Cl = CL)
CloudParams <- tribble(
  ~name, ~symbol, ~label,
  "NO3", "NO<sub>3</sub><sup>-</sup>", "NO<sub>3</sub><sup>-</sup> (μeq L<sup>-1</sup>)",
  "SO4", "SO<sub>4</sub><sup>2-</sup>", "SO<sub>4</sub><sup>2-</sup> (μeq L<sup>-1</sup>)",
  "pH", "pH", 'pH',
  "SPCOND", "SPCOND", "SPCOND (μS L<sup>-1</sup>)",
  'NH4', "NH<sub>4</sub><sup>+</sup>", "NH<sub>4</sub><sup>+</sup> (μeq L<sup>-1</sup>)",
  "TOC", "TOC", "TOC (μmolC L<sup>-1</sup>)",
  "K", "K<sup>+</sup>", "K<sup>+</sup> (μeq L<sup>-1</sup>)",
  "Ca", "Ca<sup>2+</sup>", "Ca<sup>2+</sup> (μeq L<sup>-1</sup>)",
  "Mg", "Mg<sup>2+</sup>", "Mg<sup>2+</sup> (μeq L<sup>-1</sup>)",
  "Na", "Na<sup>+</sup>", "Na<sup>+</sup> (μeq L<sup>-1</sup>)",
  "Cl", "Cl<sup>-</sup>", "Cl<sup>-</sup> (μeq L<sup>-1</sup>)"
)

get_name <- \(s) subset(CloudParams, symbol == s)$name
get_label <- \(s) subset(CloudParams, symbol == s)$label
get_symbol<- \(s) subset(CloudParams, symbol == s)$symbol

#CloudParms<-c('SO4', "NO3", 'pH', "SPCOND", "NH4", "TOC", "K", "Ca", "Mg", "Na", "Cl")

colorselect<-c("SO4"= 'red', "NO3"="blue", "NH4" = "orange", "TOC" = 'forest green',
          "Ca" = "purple", 'K' = 'green', 'SPCOND'= 'white', "pH" = 'grey',
          'Mg'= 'lightblue', 'Na' = 'lightgreen', 'Cl' = "cyan")

# Define UI for application that draws a histogram
#prod_codes <- setNames(products$prod_code, products$title)
ui <- fluidPage(
  withMathJax(),
  fluidRow(column(
    6,
    selectizeInput(inputId = "Cloud_Concentrations",
      label = "Choose a Cloud Analyte",
      choices = CloudParams$symbol,
      options = list(render = I('{
          item: function(item, escape) {
            return "<div>" + item.value + "</div>";
          },
          option: function(item, escape) {
            return "<div>" + item.value + "</div>";
          }
        }')
      )
    )
  )),
  fluidRow(plotlyOutput(outputId = "cloudplot")),
  theme = bslib::bs_theme(bootswatch = "darkly"),
  
)
server <- function(input, output) {
  thematic::thematic_shiny()
  selected <- clouddata
  sumdata <- selected %>%
      select(Year,CloudParams$name)%>%
      mutate(Cl = case_when(Year == 2018~ Cl/5,
                            Year == 2019 ~Cl/2,
                            TRUE ~ Cl))|>
      group_by(Year)|>
      summarise(across(where(is.numeric), 
                                        list(median = median, QuantMin = ~quantile(., 0.25, na.rm = TRUE),
                                             QuantMax = ~quantile(., 0.75, na.rm = TRUE)),
                                        na.rm = TRUE,
                                        .names = "{.col}.{.fn}"))
      

    
  output$cloudplot <-renderPlotly({ # nolint
    if(input$Cloud_Concentrations == 'pH' | input$Cloud_Concentrations == "SPCOND"){
    ggplotly(ggplot(data = sumdata,aes(x = Year, color = paste0(input$CloudConcentrations, ".median"), 
               fill = paste0(input$Cloud_Concentrations, '.median')))+
      geom_ribbon(aes_string(ymin = paste0(input$Cloud_Concentrations, ".QuantMin"),
                  ymax = paste0(input$Cloud_Concentrations, ".QuantMax")),
                  alpha = 0.4)+
     # geom_smooth(aes_string(y = paste0(input$Cloud_Concentrations, ".median")), # nolint
    #              alpha = 0.4, method = 'gam', color = 'blue')+
      geom_line(aes_string(y = paste0(input$Cloud_Concentrations, ".median")), size=3)+
      geom_point(aes_string(y = paste0(input$Cloud_Concentrations, ".median")), 
                 color = 'black',size =3, shape =21)+
      geom_smooth(aes_string(y = paste0(input$Cloud_Concentrations, ".median")), size=1,
                  method = 'gam', linetype = 'dashed')+  
      scale_color_manual(values =colorselect[[input$Cloud_Concentrations]])+
      scale_fill_manual(values =colorselect[[input$Cloud_Concentrations]])+
      scale_x_continuous(breaks = seq(1994, 2022, 4))+
    #  ylab(ylabels())+
      labs(title =paste0("Median Cloud Water Analyte: ", get_name(input$Cloud_Concentrations)),
           y = get_label(input$Cloud_Concentrations))+
      theme(title = element_text(size =20),
            axis.text = element_text(size = 20),
            legend.position = "none", axis.title = element_markdown()),
    tooltip = c("x","y"))%>%
        highlight("plotly_selected")
        #layout(legend ="")
        
    }
    else{
      
        ggplotly(ggplot(data = sumdata,aes(x = Year, color = paste0(input$Cloud_Concentrations, ".median"), 
                   fill = paste0(input$Cloud_Concentrations, '.median')))+
        geom_ribbon(aes_string(ymin = paste0(get_name(input$Cloud_Concentrations), ".QuantMin"),
                               ymax = paste0(get_name(input$Cloud_Concentrations), ".QuantMax")),
                    alpha = 0.4)+
        # geom_smooth(aes_string(y = paste0(input$Cloud_Concentrations, ".median")),
        #              alpha = 0.4, method = 'gam', color = 'blue')+
        geom_line(aes_string(y = paste0(get_name(input$Cloud_Concentrations), ".median")), size=3)+
        geom_point(aes_string(y = paste0(get_name(input$Cloud_Concentrations), ".median")), color = 'black',
                   size =3, shape =21)+
        geom_smooth(aes_string(y = paste0(get_name(input$Cloud_Concentrations), ".median")), size=1,
                    method = 'gam', linetype = 'dashed')+
        scale_color_manual(values =colorselect[[get_name(input$Cloud_Concentrations)]])+
        scale_fill_manual(values =colorselect[[get_name(input$Cloud_Concentrations)]])+
        scale_x_continuous(breaks = seq(1994, 2022, 4))+
       # ylab(ylabels())+
        labs(title =paste0("Median Cloud Water Analyte: ", get_symbol(input$Cloud_Concentrations)),
             y = get_label(input$Cloud_Concentrations))+
        theme(title = element_markdown(size =20),
              axis.text = element_text(size = 20), legend.title = element_blank(),
              legend.position = "none", axis.title = element_markdown()),
        tooltip = c('x','y'))|>
        highlight("plotly_selected")
       # layout(yaxis = list(ylabels()))
    }  
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)



