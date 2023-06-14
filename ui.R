################################################################################
# Define UI for DVP app
################################################################################
library(ggiraph)

ui <- fixedPage(
  titlePanel(
    h2(strong('Alcohol Consumption Across The Globe:'), 
       br(strong(em('Cause and Effect'))),
       align = 'center')),
  
  fluidRow(
    style = 'border-left: 4px double black;
             border-top: 4px double black;
             border-right: 4px double black',
    column(
      width = 12, 
      h4('This project looks at a topic of general interest: Alcohol consumption.
         Alcohol beverages are luxury goods that many of us put on the dining 
         table on a regular basis. On the other hand, alcohol consumption is 
         known to have some adverse effects human health. In this project, 
         visualisation techniques are utilised to explore several causative 
         factors of alcohol consumption as well as various aspects in human life 
         that are likely to be affected by alcohol consumption.'
         )
    )),
  
  fluidRow(
    style = 'border-left: 4px double black;
             border-top: 1px solid black;
             border-right: 4px double black',
    column(
      width = 12, 
      h3(strong(em('Cause:'))),
      h4(br(),
         'Three potential causative factors that might affect alcohol consumption 
         (i.e., literacy level, alcohol production, and unemployment rate) among 
         people are examined. Geographical region and the major religion of a 
         country are also among the factors that are likely to influence alcohol 
         consumption.'
         ),
      h5(br(strong(em('Units:')))),
      h6(br('Consumption: [ litres per capita ]'),
         br('Literacy Level: [ % of people ages >= 15 ]'),
         br('Production: [ x 1000 tonne / year ]'),
         br('Unemployment Rate: [ % of total labour force ]'))
      )
    ),
  
  fluidRow(
    style = 'border-left: 4px double black;
             border-right: 4px double black',
    column(
      style = 'margin-top: 20px;
               margin-left: 30px',
      width = 3,
      radioButtons("colorCause",
                   "Colour code bubbles by:",
                   choices = c(
                     "Region", 
                     "Major Religion"),
                   selected = "Region")),
    
    column(
      style = 'margin-top: 20px;
               margin-left: 50 px',
      align = 'center',
      width = 5,
      sliderInput(
        "yearCause", 
        'Year:',
        min = 2000, 
        max = 2018,
        value = 2000,
        step = 5,
        sep = "",
        animate = animationOptions(interval = 3000, loop = TRUE),
        width = '85%')),
    
    column(
      style = 'margin-top: 20px;
               margin-left: 50px',
      width = 3,
      radioButtons("cause",
                   "Cause:",
                   choices = c(
                     "Literacy Level", 
                     "Production", 
                     "Unemployment Rate"),
                   selected = "Literacy Level"))
    ),
  fluidRow(
    style = 'border-left: 4px double black;
             border-right: 4px double black;
             border-bottom: 1px solid black',
    column(
      style = 'margin-top: 60px;
               margin-bottom: 10px',
      width = 4, 
      align = 'left',
      girafeOutput("scatterPlotCause")),
    
    column(
      style = 'margin-bottom: 10px',
      width = 8,
      align = 'right',
      girafeOutput("mapCause"))
    ),
  
  fluidRow(
    style = 'border-left: 4px double black;
             border-right: 4px double black',
    column(
      width = 12, 
      h3(br(strong(em('Effect:')))),
      h4(br(),
         'Four aspects of human life, namely life expectancy, life satisfaction, 
         death from liver cirrhosis, and traffic accidents are anticipated to be 
         affected by alcohol consumption. The trendlines in the scatterplots are 
         intended to give a clearer idea about how alcohol consumption affected 
         these aspects. Although the data on traffic crashes are also presented, 
         viewers should bear in mind that this dataset is not representative of 
         the countries in the world as it consists of only 50 countries which are 
         rather closely located. Hence, this part of the visualisation should 
         serve as reference only.'),
      h5(br(strong(em('Units:')))),
      h6(br('Consumption: [ litres per capita ]'),
         br('Life Expectancy: [ years ]'),
         br('Life Satisfaction: [ in Cantril Ladder ]'),
         br('Liver Cirrhosis Deaths: [ % (alcohol-attributable fractions) ]'),
         br('Traffic Crashes : [ % of all traffic crashes ]'),
         br('FAQ | World Happiness Report (n.d.): “The Cantril
            ladder: it asks respondents to think of a ladder, with the best 
            possible life for them being a 10 and the worst possible life being 
            a 0. They are then asked to rate their own current lives on that 0 
            to 10 scale.”'),
         br('FAQ | The World Happiness Report. (n.d.). Retrieved September 9, 
            2022, from https://worldhappiness.report/faq/'))
      ),
    
    column(
      style = 'margin-top: 20px;
               margin-left: 30px',
      width = 3,
      radioButtons("colorEffect",
                   "Colour code bubbles by:",
                   choices = c(
                     "Region", 
                     "Major Religion"),
                   selected = "Region"
                   )
      ),
    column(
      style = 'margin-top: 20px;
               margin-left: 50 px',
      align = 'center',
      width = 5,
      sliderInput(
        "yearEffect", 
        'Year:',
        min = 2000, 
        max = 2018,
        value = 2000,
        step = 5,
        sep = "",
        animate = animationOptions(interval = 3000, loop = TRUE),
        width = '85%'
      )
    ),
    column(
      style = 'margin-top: 20px;
               margin-left: 50px',
      width = 3,
      radioButtons(
        inputId = "effect",
        label = "Effect:",
        choices = c(
          "Life Expectancy", 
          "Life Satisfaction", 
          "Liver Cirrhosis Deaths",
          "Traffic Crashes"),
        selected = "Life Expectancy")
    )
  ),
  
  fluidRow(
    style = 'border-left: 4px double black;
             border-right: 4px double black;
             border-bottom: 4px double black',
    column(
      style = 'margin-top: 60px;
               margin-bottom: 10px',
      width = 4, 
      align = 'left',
      girafeOutput("scatterPlotEffect")),
    
    column(
      style = 'margin-bottom: 10px',
      width = 8,
      align = 'right',
      girafeOutput("mapEffect"))
    )
  )
