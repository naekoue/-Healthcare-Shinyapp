shinyUI(
  dashboardPage(
    dashboardHeader(title = "Chronic Conditions in the US",titleWidth = 300),
    dashboardSidebar(width = 300,
                     sidebarUserPanel("Nillia Ekoue"),
                     sidebarMenu(
                       menuItem("Overview", tabName = "intro", icon = icon("info")),
                       menuItem("Chronic Conditions by State", tabName = "map", icon = icon("map")),
                       selectizeInput("selected","Conditions Costs / ER visists / Readmissions",
                                      Conditions.Costs.ER.Readmissions),
                       menuItem("Dyads", tabName = "firstdata", icon = icon("stethoscope")),
                       selectizeInput("selected", "Conditions Costs / ER visists / Readmissions",
                                      Conditions.Costs.ER.Readmissions),
                       menuItem("Charts", tabName = "seconddata", icon = icon("line-chart")),
                       selectizeInput("dyads","Dyads Type",Dyads.Type),
                       selectizeInput("costprevalence","Cost or Prevalence", cost.prevalence),
                       menuItem('Further Exploration', tabName = 'further', icon = icon("medkit")))),
    dashboardBody(tabItems(
      tabItem(tabName = 'intro',
              fluidRow(
                h2("Chronic Conditions in the US."),
                #fluidRow(  
                  h3('Overview:') ,
                       h4('According to the CMS, the national health spending growth
                           is projected to average 5.7 percent, from 4.8
                              percent in 2019, and reach nearly $6.0 trillion by 2027. 
                              Chronic and mental health conditions account for 90% of the annual healthcare expenses.
                        Chronic diseases are defined broadly as conditions that last 1 year or 
                         more and require ongoing medical attention or limit activities
                        of daily living or both. 
                          "6 in 10 adults in the US have a chronic disease,
                          4 in 10 adults in the US have two or more."(CDC.gov)'),
                       h3('The Dataset:'),
                       h4('Prevalence and Medicare utilization and spending are presented
                              for 21 chronic conditions, FY 2017.')
                #)
              ,
                HTML(
                  paste('<br/>', '<br/>','<br/>','<br/>','<br/>','<br/>','<br/>','<br/>','<br/>',
                        h5("Email: naekoue@gmail.com \n\n" ),h5("www.linkedin.com/in/nilliaekoue"))
                ))),
      tabItem(tabName = "map",
              fluidRow(infoBoxOutput("maxBox"),
                       infoBoxOutput("minBox"),
                       infoBoxOutput("avgBox")),
              tabsetPanel(tabPanel("Tab1",htmlOutput("map", height = 500)), 
                          tabPanel("Tab2", htmlOutput("bar", height = 400)),
                          tabPanel("Tab3", htmlOutput("scatter", height = 400)),
                          tabPanel("Tab4",DT::dataTableOutput("table")))),
      
      tabItem(tabName = "firstdata",
              tabsetPanel(
                tabPanel("Tab5", htmlOutput("bar2", height = 300)),
                tabPanel("Tab6", htmlOutput("bar3", height = 300),htmlOutput("bar8", height = 300)),
                tabPanel("Tab7", htmlOutput("bar4", height = 300),htmlOutput("bar5", height = 300)),
                tabPanel("Tab8", htmlOutput("bar6", height = 300),htmlOutput("bar7", height = 300))
                )),
      tabItem(tabName = "seconddata",
              tabsetPanel(
                tabPanel("Tab9", plotOutput("plot4", height = 400)),
                tabPanel("Tab10", plotOutput("plot5", height = 300),plotOutput("plot6", height = 300)),
                tabPanel("Tab11", plotOutput("plot7", height = 300),
                                          plotOutput("plot8", height = 300)))),
      tabItem(tabName = 'further',
              fluidPage(
                tags$pre(h3("\t Going Forward")),
                column(width = 10, offset = 2, h4('1. Expand the analysis of Chronic Conditions Spending to cover 2007-2017.'),
                       h4('2. Analyze healthcare costs associated with chronic conditions hospital readmissions .'),
                       h4('3. Evaluate hospital and long term care facilities geographical presence in zones 
with high risk Medicare beneficiaries.'))))
    ))
  )) 
