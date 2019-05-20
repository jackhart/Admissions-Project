library(shiny)
library(here)
library(ggplot2)
library(scales)
library(dplyr)
library(shinythemes)
library(magrittr)
library(reshape2)

# Data Setup ------------------------------------------------------------------------------------------------------------------

#**make sure you're in the correct directory
getwd() #**set up so your working directory is the repository

#Import Data
AlumnData <- read.csv(here("ScholarshipApplicationDataFilePretend.csv"))

table(AlumnData$State)
sum(table(AlumnData$CollegeRegion))

# Analysis ------------------------------------------------------------------------------------------------------------------
table(AlumnData$Matriculating, AlumnData$OfferOfAdmissionExtended)

table(AlumnData$Major)

#t-test on GPA
subset_matriculated <- AlumnData %>% filter(OfferOfAdmissionExtended == 'YES') %>%
  filter(Matriculating == 'YES')
subset_NonMatriculated <- AlumnData %>% filter(OfferOfAdmissionExtended == 'YES') %>%
  filter(Matriculating == 'NO')
t.test(subset_matriculated$GPA, subset_NonMatriculated$GPA)

#t-test on GRE
t.test(subset_matriculated$GRE, subset_NonMatriculated$GRE)

#chi-squared test by major -- counts too small
subset_accepted <- AlumnData %>% filter(OfferOfAdmissionExtended == 'YES')
chisq.test(table(subset_accepted$Matriculating, subset_accepted$Major))

#prop test on all majors
MajorsMatric <- subset_accepted %>% count(Matriculating, Major) %>% 
  dcast(Major ~ Matriculating, value.var = 'n') %>% 
  filter(!Major %in% c('Accounting', 'Physics', 'Management', 'Finance'))
total_col = apply(MajorsMatric[,-c(1)], 2, sum)

prop_test_results <- data.frame()
for(i in 1:nrow(MajorsMatric)){
  temp1 <- as.list(unname(MajorsMatric[i,-1]))
  temp <- prop.test(x = c(temp1[[1]], temp1[[2]]), n = total_col[1:2])
  prop_test_results <- rbind(prop_test_results, 
                             data.frame("Major" = as.character(MajorsMatric$Major[i]), temp$statistic,
                                        temp$parameter, "p.value" = temp$p.value))
}
rownames(prop_test_results) <- NULL
prop_test_results <- prop_test_results %>% set_colnames(c("Major", "X-Squared", "df","p-value"))


#chi-squared test with gender
chisq.test(table(subset_accepted$Matriculating, subset_accepted$Gender))


# Functions -------------------------------------------------------------------------------------------------------------
createPanel <- function(name, body) {
  obj <- tabPanel(name, br(),
                  fluidRow(paste0(name, " Distribution"),
                           #Description of results
                           column(3, wellPanel(HTML(
                             paste(
                               paste0("<h1>Matriculated VS Non-Matriculated ", name, "</h1>"),
                               "<p>",body, "</p>",
                               sep = "" ))
                           )),
                           #Inputs for different plots
                           column(3, wellPanel(
                             radioButtons(paste0(name,"plots"), h3("Plots"),
                                          choices = list("Histogram of Counts" = paste0(name,'hist'), "Density Plot" = paste0(name,'dense'),
                                                         "Boxplots" = paste0(name,'box')),selected = paste0(name,'hist')) ),
                             conditionalPanel(
                               condition = paste0("input.",name,"plots == '", name, "hist'"),
                               wellPanel(sliderInput(inputId = paste0(name,'bins'),
                                                     label = "Number of bins:",
                                                     min = 2,
                                                     max = 20,
                                                     step = 2,
                                                     value = 10) )),
                             wellPanel(
                               checkboxGroupInput(inputId = paste0(name,"matriculation"), 
                                                  h3("Matriculation Status"), 
                                                  choices = list("Matriculated" = "YES", 
                                                                 "Non-Matriculated" = "NO"),
                                                  selected = c("YES", "NO"))
                             ) ),
                           
                           #The actual plots
                           column(6, conditionalPanel(
                             condition = paste0("input.",name,"plots == '", name, "hist'"),
                             plotOutput(outputId = paste0("histogram",name)))
                           ),
                           column(6, conditionalPanel(
                             condition = paste0("input.",name,"plots == '", name, "dense'"),
                             plotOutput(outputId = paste0("density",name)))
                           ),
                           column(6, conditionalPanel(
                             condition = paste0("input.",name,"plots == '", name, "box'"),
                             plotOutput(outputId = paste0("boxplot",name)))
                           )
                  ))
  
  return(obj)
}

createPaneldiscrete <- function(name, body) {
  obj <- tabPanel(name, br(),
                  fluidRow(paste0(name, " Two-Way Table"),
                           #Description of results
                           column(4, wellPanel(HTML(
                             paste(
                               paste0("<h1>Matriculated VS Non-Matriculated ", name, "</h1>"),
                               "<p>",body, "</p>",
                               sep = "" ))
                           )),
                           #Inputs for different plots
                           column(2, wellPanel(
                             radioButtons(paste0(name,"plots"), h3("Plots"),
                                          choices = list("Two-Way Count Table" = paste0(name,'count'),
                                                         "Two-Way Proportions Table" = paste0(name,'prop')),selected = paste0(name,'count')) 
                           )),
                           
                           #The actual plots
                           column(6, conditionalPanel(
                             condition = paste0("input.",name,"plots == '", name, "count'"),
                             plotOutput(outputId = paste0("count",name)))
                           ),
                           column(6, conditionalPanel(
                             condition = paste0("input.",name,"plots == '", name, "prop'"),
                             plotOutput(outputId = paste0("prop",name)))
                           )
                  ))
  
  return(obj)
}






createhist <- function(name, matriculation, inp_bins, subsetAdmit){
  #create dataframe
  dfplot <- subsetAdmit() %>% filter(Matriculating %in% matriculation)
  
  if(length(matriculation) == 2){
    ggplot(data = dfplot) + geom_histogram(aes(x = eval(parse(text = name)), fill = Matriculating), bins = inp_bins, position = 'identity', alpha = .6) + theme_minimal() + 
      labs(x = name, y = "Count", title = "", fill = "") + scale_x_continuous(breaks = pretty_breaks()) +
      scale_fill_manual(labels = c("Did Not Matriculate", "Matriculated"), values = c("#F79857", "#61A9B0"))
  } else {
    
    ggplot(data = dfplot) + geom_histogram(aes(x = eval(parse(text = name)), fill = Matriculating), bins = inp_bins) + theme_minimal() + 
      labs(x = name, y = "Count", title = "", fill = "") + scale_x_continuous(breaks = pretty_breaks()) +
      scale_fill_manual(labels = c("Did Not Matriculate", "Matriculated"), values = c("#F79857", "#61A9B0")) + theme(legend.position = "none")
  }
}


createdens <- function(name, matriculation, subsetAdmit){
  #create dataframe
  dfplot <- subsetAdmit() %>% filter(Matriculating %in% matriculation)
  if(length(matriculation) == 2){
    ggplot(data = dfplot) + geom_density(aes(x = eval(parse(text = name)), fill = Matriculating), alpha = .6, color = "grey40") +
      theme_minimal() + scale_x_continuous(breaks = pretty_breaks()) + labs(x = name, y = "Density", title = "", fill = "") +
      scale_fill_manual(labels = c("Did Not Matriculate", "Matriculated"), values = c("#F79857", "#61A9B0"))
  } else{
    ggplot(data = dfplot) + geom_density(aes(x = eval(parse(text = name)), fill = Matriculating), color = "grey40") +
      theme_minimal() + scale_x_continuous(breaks = pretty_breaks()) + labs(x = name, y = "Density", title = "", fill = "") +
      scale_fill_manual(labels = c("Did Not Matriculate", "Matriculated"), values = c("#F79857", "#61A9B0")) + theme(legend.position = "none")
  }
}


createbox <- function(name, matriculation, subsetAdmit){
  #create dataframe
  dfplot <- subsetAdmit() %>% filter(Matriculating %in% matriculation)
  
  if(length(matriculation) == 2){
    ggplot(data = dfplot) + geom_boxplot(aes(x = Matriculating, y = eval(parse(text = name)), fill = Matriculating), color = "grey40") +
      theme_minimal() + scale_y_continuous(breaks = pretty_breaks()) + labs(y = name, x = "", title = "", fill = "") +
      scale_fill_manual(values = c("#F79857", "#61A9B0")) + theme(legend.position = "none") +
      scale_x_discrete(labels = c("Did Not Matriculate", "Matriculated"))
    
  } else{
    label <- ifelse(matriculation == 'YES', "Matriculated", "Did Not Matriculate")
    ggplot(data = dfplot) + geom_boxplot(aes(x = "", y = eval(parse(text = name))), fill = "#F79857", color = "grey40") +
      theme_minimal() + scale_y_continuous(breaks = pretty_breaks()) + labs(y = name, x = "", title = "", fill = "") +
      scale_x_discrete(labels = label)
  }
}

createcount <- function(name, subset){
  temp <- subset %>% count(variable) %>% arrange(desc(n))
  dfplot <- subset %>% mutate(variable = factor(as.character(variable), as.character(temp$variable)))
  
  ggplot(dfplot, aes(variable, Matriculating)) +
    geom_jitter(aes(color = Matriculating), size = 4, show.legend=FALSE) +
    geom_vline(xintercept=seq(1.5, length(unique(dfplot$variable))-0.5, 1), lwd=1, colour="grey") +
    geom_hline(yintercept=seq(1.5, length(unique(dfplot$Matriculating))-0.5, 1), lwd=1, colour="grey") +
    scale_y_discrete(labels = c("Did Not Matriculate", "Matriculated")) +
    scale_color_manual(values = c("#F79857", "#61A9B0")) + theme_minimal() +
    theme(axis.text.x = element_text(hjust = 1, angle = 45))
}

createprop <- function(name, subset){
  dfplot <- subset %>% count(variable, Matriculating) %>% group_by(variable) %>%
    mutate(Percent = n / sum(n)*100) %>% ungroup()
  temp <- dfplot %>% filter(Matriculating == "YES") %>% arrange(desc(Percent))
  dfplot <- dfplot %>% mutate(variable = factor(as.character(variable), unique(as.character(temp$variable))))
  
  label = paste(round(dfplot$Percent), replicate(length(dfplot$Percent),'%'), sep = "")
  
  #create n values for plot 
  temp <- dfplot %>% group_by(variable) %>% summarise(Total = sum(n)) 
  major.labels = paste(temp$variable, replicate(nrow(temp),'\n n = '), temp$Total, sep = "")
  
  ggplot(dfplot, aes(variable, Matriculating, fill = Percent)) + 
    geom_tile(colour = 'grey30', size = 2) + geom_text(aes(label=label)) +
    theme_minimal() + theme(axis.text.x = element_text(hjust = 1, angle = 45), legend.position = "none") +
    labs(y = '', x = '') +
    scale_fill_gradientn(colours = c('#cdf1f9', '#059abd'), values = c(0,1)) +
    scale_x_discrete(labels=major.labels) +
    scale_y_discrete(labels = c("Did Not Matriculate", "Matriculated"))
  
}



# Text for pages ----------------------------------------------------------------------------

textGPA <- "Students who matriculated (ended up enrolling) tended to have lower GPAs, on average.  The mean GPA for matriculated students was about 3.54, while the non-matriculated students had mean GPAs of 3.67.  A t-test found that the mean GPAs of matriculated and non-matriculated students were different at a statistically significant level (t = -4.1945, df = 179, p-value = ~4.2x10^-5).  Meaning, there is strong evidence indicating that accepted students who did not enroll had higher average GPAs than accepted students who did enroll."
textGRE <- "Both types of students had highly skewed distributions for GRE scores.  However, accepted students who did not end up enrolling had, on average, higher GRE scores.  The mean GRE score for matriculated students was about 166.23, while the mean score for non-matriculaed students was 167.91.  A t-test found that the mean GRE scores of matriculated and non-matriculated students were different at a statistically significant level (t = -2.604, df = 141, p-value = .0004). "
textMajor <- "In the visual with dots, each dot represents an accepted student, and the color/position of that dot indicates whether they matriculated.  That visual depicts a wide variety of counts for each undergraduate major; because some majors had small counts, it's not possible to run a chi-squared significance test on this entire table.  However, proportions tests were run to see if the percentages of matriculated and non-matriculated students from each individual major were different.  (Refer to the second visual to see the percentages by major.)  No major had statistically significant differences in their proportions of matriculated students.  The only major that got close to having a significant difference was the "Other" major category, which had a p-value of 0.07."
textGender <- "In the visual with dots, each dot represents an accepted student, and the color/position of that dot indicates whether they matriculated.  This visual indicates that there were relatively consistent counts within each category.  The next visual shows the proportions by gender.  About 70% of men who were accepted matriculated into the program, while only 61% of women did.  However, a chi-squared test run on the counts used to make these tables did not find a significant difference.  This means that although proportionally more men matriculated in this sample, this difference is not significant. "
textCitz <- "insert here"
# Shiny App ui ------------------------------------------------------------------------------------------------------------------

ui <- navbarPage("Project", selected = "Exploritory Analysis", collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"),
                 #Main Nav bar Panels
                 tabPanel("Exploritory Analysis",
                          fluidPage(
                            #nav bar for matriculation page
                            tabsetPanel(
                              createPanel("GPA", textGPA),
                              createPanel("GRE", textGRE),
                              createPaneldiscrete("Major", textMajor),
                              createPaneldiscrete("Gender", textGender),
                              createPaneldiscrete("Dom_Int", textCitz),
                              tabPanel("Location")
                            ))),
                 tabPanel("Models"),
                 tabPanel("Conclusions")
)



server <- function(input, output) {
  
  subsetAdmit <- reactive({
    AlumnData %>% filter(OfferOfAdmissionExtended == 'YES') 
  })
  
  
  #Create histogram objects
  output$histogramGPA <- renderPlot({createhist("GPA", input$GPAmatriculation, input$GPAbins, subsetAdmit)})
  output$histogramGRE <- renderPlot({createhist("GRE", input$GREmatriculation, input$GREbins, subsetAdmit)})
  #Create Density Objects
  output$densityGPA <- renderPlot({createdens("GPA", input$GPAmatriculation, subsetAdmit)})
  output$densityGRE <- renderPlot({createdens("GRE", input$GREmatriculation, subsetAdmit)})
  #Create Boxplot Objects
  output$boxplotGPA <- renderPlot({createbox("GPA", input$GPAmatriculation, subsetAdmit)})
  output$boxplotGRE <- renderPlot({createbox("GRE", input$GREmatriculation, subsetAdmit)})
  #create count jitter objects
  output$countMajor <-  renderPlot({ inputdf <- subsetAdmit() %>% dplyr::select(Major, Matriculating) %>% 
    set_colnames(c("variable", "Matriculating"))
  createcount("Major", inputdf)})
  output$countGender <-  renderPlot({ inputdf <- subsetAdmit() %>% dplyr::select(Gender, Matriculating) %>% 
    set_colnames(c("variable", "Matriculating"))
  createcount("Gender", inputdf)})
  output$countDom_Int <-  renderPlot({ inputdf <- subsetAdmit() %>% dplyr::select(Dom_Int, Matriculating) %>% 
    set_colnames(c("variable", "Matriculating"))
  createcount("Dom_Int", inputdf)
  })
  #create proportions tile objects
  output$propMajor <- renderPlot({ inputdf <- subsetAdmit() %>% dplyr::select(Major, Matriculating) %>% 
    set_colnames(c("variable", "Matriculating"))
  createprop("Major", inputdf)
  })
  
  output$propGender <- renderPlot({ inputdf <- subsetAdmit() %>% dplyr::select(Gender, Matriculating) %>% 
    set_colnames(c("variable", "Matriculating"))
  createprop("Gender", inputdf)
  })
  output$propDom_Int <- renderPlot({ inputdf <- subsetAdmit() %>% dplyr::select(Dom_Int, Matriculating) %>% 
    set_colnames(c("variable", "Matriculating"))
  createprop("Dom_Int", inputdf)
  })
  
}



shinyApp(ui, server)






