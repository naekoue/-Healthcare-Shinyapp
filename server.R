 shinyServer(function(input, output) {
   
   output$map <- renderGvis({
     gvisGeoChart(spending2017,"State",input$selected,
                  options=list(region="US", displayMode="regions", 
                               resolution="provinces",
                               width="auto", height="auto", title ="Chronic Conditions Costs and Prevalence"))
   })
   output$hist <- renderGvis(
     gvisHistogram(spending2017[,input$selected, drop=FALSE]))
   
   output$table <- DT::renderDataTable({
     datatable(spending2017, rownames=FALSE) %>% 
       formatStyle(input$selected,  
                   background="skyblue", fontWeight='bold')
   }) 
   
   #spendinorder<-reactive({spending2017%>%arrange(desc(input$selected))})
   
   output$bar <- renderGvis({gvisBarChart(spending2017,x = 'State', y = input$selected,
                                          options=list(title='Chronic Conditions Spending per State per capita'))})
   
   output$scatter <- renderGvis({ gvisScatterChart(spending2017[,c("Alcohol.Abuse.Readmissions", "Alcohol.Abuse.ED.Visits")],
                                                   options = list(width="600px", height="300px",
                                                                  title="Hospital Readmissions and Emergency Department Visits", 
                                                                  hAxis="{title:'Hospital Readmissions '}",
                                                                  vAxis="{title:'Emergency Department Visits'}"))})   
   output$table2 <- DT::renderDataTable({
     datatable(Dyads2017, rownames=FALSE) %>% 
       formatStyle(input$dyads,  
                   background="skyblue", fontWeight='bold')
   })
   
   output$table3 <- DT::renderDataTable({
     datatable(Dyads1, rownames=FALSE) %>% 
       formatStyle(input$dyads,  
                   background="skyblue", fontWeight='bold')
   })
   
   output$bar2 <- renderGvis({gvisBarChart(Dyads2017, x = 'Dyad.Label', y = 'All.Beneficiaries.Spending' ,
                                           options=list(title='Dyads combinations'))})
   output$bar3 <- renderGvis({
     gvisBarChart(Top10Dyads, x= 'Dyad.Label', y='All.Beneficiaries.Spending' ,
                  option=list(title="Most Expensive Dyads All Beneficiaries Spending"))})
   
   output$bar4 <- renderGvis({
     gvisBarChart(Top10DyadsM, x= 'Dyad.Label', y='Males.Spending' ,
                  option=list(title="Top 10 Most Expensive Dyads Males Spending"))})
   output$bar5 <- renderGvis({
     gvisBarChart(Top10DyadsF, x= 'Dyad.Label', y='Females.Spending' ,
                  option=list(title="Top 10 Most Expensive Dyads Females Spending"))})
   
   output$bar6 <- renderGvis({
     gvisBarChart(Top10Dyadsalt, x= 'Dyad.Label', y='At.Least.65.Years.Spending' ,
                  option=list(title="Top 10 Most Expensive Dyads At Least 65 Years Spending"))})
   
   output$bar7 <- renderGvis({
     gvisBarChart(Top10Dyadsls, x= 'Dyad.Label', y='Less.than.65.Years.Spending' ,
                  option=list(title="Top 10 Most Expensive Dyads Less than 65 Years Spending"))})
   
   output$bar8 <- renderGvis({
     gvisBarChart(Low10Dyads, x= 'Dyad.Label', y='All.Beneficiaries.Spending' ,
                  option=list(title="Least Expensive Dyads All Beneficiaries Spending"))})
   
   
   
   #dc1%>%filter(variables == input$selected)%>%group_by(State)%>%
   # ggplot(aes(x=Year,y=value))+geom_line()})
   
  # output$plot1 = renderPlot({ ggplot(spending2017,(aes_string(x = reorder(State, -input$selected), y = input$selected))) + geom_bar(stat="identity") + 
   #    theme(axis.text.x=element_text(angle=60,hjust=1,vjust=0.5)) + coord_flip()+ ggtitle('Chronic Conditions Spending per State per capita')
   #})  
   
   output$plot3 <-renderPlot({Top10expensive<-spending2017%>%select(State,input$selected)%>%
     arrange(desc(input$selected))%>% top_n(10, input$selected)%>%ggplot(aes_string(x = Top10expensive[,1], y =Top10expensive[,2]))+
     geom_col(fill ='green') + ggtitle("Top10")})
   
   output$plot4 =renderPlot({Dyads1%>%filter(Dyad.Label %in%c("Stroke.and.Hepatitis.(Chronic.Viral.B.&.C)",
                                                              "Atrial.Fibrillation.and.HIV.AIDS",
                                                              "Stroke.and.HIV.AIDS",
                                                              "Atrial.Fibrillation.and.Hepatitis.(Chronic.Viral.B.&.C)",
                                                              "Heart.Failure.and.HIV.AIDS") & variables =='All.Beneficiaries.Spending')%>%
       group_by(Dyad.Label)%>%ggplot(aes(x=Year,y=value))+geom_line(aes(color = Dyad.Label))+ 
       ggtitle('Top 5 Most Expensive Dyads')+ ylab('All Beneficiaries Spending')+ scale_x_continuous(breaks = 2007:2017)
   })   
   
   output$plot5 =renderPlot({Dyads1%>%filter(Dyad.Label %in%c("Stroke.and.Hepatitis.(Chronic.Viral.B.&.C)",
                                                              "Atrial.Fibrillation.and.Hepatitis.(Chronic.Viral.B.&.C)",
                                                              "Stroke.and.Schizophrenia.Other.Psychotic.Disorders",
                                                              "Atrial.Fibrillation.and.Schizophrenia.Other.Psychotic.Disorders",
                                                              "Heart.Failure.and.Hepatitis.(Chronic.Viral.B.&.C)") & variables =='Males.Spending')%>%
       group_by(Dyad.Label)%>%ggplot(aes(x=Year,y=value))+geom_line(aes(color = Dyad.Label))+ ggtitle('Top 5 Most Expensive Dyads')+ ylab('Males.Spending')+ scale_x_continuous(breaks = 2007:2017)
   })
   
   output$plot6 =renderPlot({Dyads1%>%filter(Dyad.Label %in%c("Atrial.Fibrillation.and.HIV.AIDS",
                                                              "Stroke.and.HIV.AIDS",
                                                              "Stroke.and.Hepatitis.(Chronic.Viral.B.&.C)",
                                                              "Heart.Failure.and.HIV.AIDS",
                                                              "Stroke.and.Drug.Substance.Abuse") & variables =='Females.Spending')%>%
       group_by(Dyad.Label)%>%ggplot(aes(x=Year,y=value))+geom_line(aes(color = Dyad.Label))+ ggtitle('Top 5 Most Expensive Dyads')+ ylab('Females.Spending')+ scale_x_continuous(breaks = 2007:2017)
   })
   
   output$plot7 =renderPlot({Dyads1%>%filter(Dyad.Label %in%c("Alzheimer.s.Disease.Dementia.and.Atrial.Fibrillation",
                                                              "Atrial.Fibrillation.and.Hepatitis.(Chronic.Viral.B.&.C)",
                                                              "Heart.Failure.and.Stroke",
                                                              "Alzheimer.s.Disease.Dementia.and.Heart.Failure",
                                                              "Stroke.and.Atrial.Fibrillation") & variables =='Less.than.65.Years.Spending')%>%
       group_by(Dyad.Label)%>%ggplot(aes(x=Year,y=value))+geom_line(aes(color = Dyad.Label))+ ggtitle('Top 5 Most Expensive Dyads')+ ylab('Less.than.65.Years.Spending')+ scale_x_continuous(breaks = 2007:2017)
   })
   
   output$plot8 =renderPlot({Dyads1%>%filter(Dyad.Label %in%c("Stroke.and.Hepatitis.(Chronic.Viral.B.&.C)",
                                                              "Stroke.and.HIV.AIDS",
                                                              "Stroke.and.Schizophrenia.Other.Psychotic.Disorders",
                                                              "Stroke.and.Drug.Substance.Abuse",
                                                              "Atrial.Fibrillation.and.HIV.AIDS") & variables =='At.Least.65.Years.Spending')%>%
       group_by(Dyad.Label)%>%ggplot(aes(x=Year,y=value))+geom_line(aes(color = Dyad.Label))+ ggtitle('Top 5 Most Expensive Dyads')+ ylab('At.Least.65.Years.Spending')+ scale_x_continuous(breaks = 2007:2017)
     
   })
   low10 <-reactive({Dyads2017%>%select(Dyad.Label,input$costprevalence)%>%
       arrange(desc(input$costprevalence))
     low10 <- tail(low10,10)
     
     low10$Dyad.Label <- factor(low10$Dyad.Label, levels = low10$Dyad.Label[order(low10$costprevalence ==input$costprevalence)])})
   output$plot9 = renderPlot({low10()%>% ggplot()+
       geom_bar(aes(x=Dyad.Label,y=input$costprevalence,color = input$costprevalence),stat="identity")+ 
       theme(axis.text.x=element_text(angle=60,hjust=1,vjust=0.5)) + coord_flip()})
   
   state5 = reactive({top5s<-spending2017%>%select(State,input$selected)%>%
     arrange(desc(input$selected))%>%top_n(5, input$selected)
   top5s
   })
   
   output$plot2  = renderPlot({ggplot(state5(),aes(x=State,y=input$selected,fill=input$selected)+geom_bar(stat="identity")+ 
                                        theme(axis.text.x=element_text(angle=60,hjust=1,vjust=0.5)) + coord_flip()
                                      +ggtitle("Top5 Most Expensive States"))})
   
   output$maxBox <- renderInfoBox({
     max_value <- max(spending2017[,input$selected])
     max_state <- 
       spending2017$State[spending2017[,input$selected]==max_value]
     infoBox(max_state, max_value, icon = icon("arrow-up"))})
   
   output$minBox <- renderInfoBox({
     min_value <- min(spending2017[,input$selected])
     min_state <- 
       spending2017$State[spending2017[,input$selected]==min_value]
     infoBox(min_state, min_value, icon = icon("arrow-down"))})
   
   output$avgBox <- renderInfoBox({
     avg_value<- spending2017[1,input$selected]
     avg_national<-spending2017$State[spending2017[1,input$selected]==spending2017[1,input$selected]][1]
     infoBox(avg_national, avg_value, icon = icon("arrows-alt-h"))})
 })
