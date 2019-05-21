library(shiny)
library(here)
library(ggplot2)
library(scales)
library(dplyr)
library(shinythemes)
library(magrittr)
library(reshape2)
library(pROC)
library(randomForest)
library(glmnet)
library(shinyBS)


# Data Setup ------------------------------------------------------------------------------------------------------------------

#**make sure you're in the correct directory
getwd() #**set up so your working directory is the repository

#Import Data
AlumnData <- read.csv(here("ScholarshipApplicationDataFilePretend.csv"), fileEncoding="UTF-8-BOM")

# Models and CV Tests -------------------------------------------------------------------------------------

# Run random forest model ==============================================================================

#prep data
#clean subset with all majors and new region variable
subset_accepted <- AlumnData %>% filter(OfferOfAdmissionExtended == 'YES')
subset_accepted_cleaned3 <- subset_accepted %>% dplyr::select(-c("OfferOfAdmissionExtended", "TOEFL","CollegeName")) %>%
  mutate(CollegeRegion = as.character(CollegeRegion)) %>% mutate(State = as.character(State))

subset_accepted_cleaned3$CollegeRegion[subset_accepted_cleaned3$CollegeRegion == 'USA' &
                                         !is.na(subset_accepted_cleaned3$State)] = 
  subset_accepted_cleaned3$State[subset_accepted_cleaned3$CollegeRegion == 'USA' & !is.na(subset_accepted_cleaned3$State)]

subset_accepted_cleaned3$CollegeRegion <-ifelse(subset_accepted_cleaned3$CollegeRegion %in% c("Connecticut", "Maine", "Pennsylvania", "New Jersey", "Pittsburgh",
                                                                                              "New York", "Massachusetts"), "New England", ifelse(subset_accepted_cleaned3$CollegeRegion %in% 
                                                                                                                                                    c("DC", "Delaware", "Virginia", "Maryland"), "DMV", ifelse(
                                                                                                                                                      subset_accepted_cleaned3$CollegeRegion %in% c("Canada", "China", "India", "Korea",
                                                                                                                                                                                                    "Scotland", "Singapore", "Spain", "UK"), "International", ifelse(
                                                                                                                                                                                                      subset_accepted_cleaned3$CollegeRegion %in% c("Georgia", "Alabama",
                                                                                                                                                                                                                                                    "Florida", "North Carolina", "Tennessee"), "South",  ifelse(
                                                                                                                                                                                                                                                      subset_accepted_cleaned3$CollegeRegion == "USA", "USA","West"))  )) )
subset_accepted_cleaned3 <- subset_accepted_cleaned3 %>% mutate(CollegeRegion = as.factor(CollegeRegion)) %>%
  dplyr::select(-c("State")) %>% na.omit(.)


#10-fold cv
set.seed(45)
folds <- sample(rep(1:10, length.out = nrow(subset_accepted_cleaned3)), size = nrow(subset_accepted_cleaned3), replace = F)
accuracies_rf <- c()
for(x in 1:10){
  model <- randomForest(Matriculating ~ ., data = subset_accepted_cleaned3[folds != x,], ntree = 50)
  preds <- unname(predict(model,  subset_accepted_cleaned3[folds == x,]))

  conf <- table(preds, subset_accepted_cleaned3[folds == x,]$Matriculating)
  acc <- sum(diag(conf))/sum(conf)
  accuracies_rf <- rbind(accuracies_rf, acc)
  
}

#Box plot of accuracies
dfplot_rf_cv <- data.frame(accuracy = unname(accuracies_rf[,1]))
rf_accuracies_gg <- ggplot(data = dfplot_rf_cv) + geom_boxplot(aes(y = accuracy), fill = "palegreen3", color = "grey40") +
  theme_minimal() + scale_y_continuous(breaks = pretty_breaks()) + labs(y = "Accuracy", x = "", title = "", fill = "") +
  theme(axis.text.x = element_blank())

#Hold-out 70%-30%
set.seed(2343)
model_rf_holdout <- randomForest(Matriculating ~ ., data = subset_accepted_cleaned3[!folds %in% c(1,2,3),], ntree = 50)
preds_rf_holdout <- unname(predict(model_rf_holdout,  subset_accepted_cleaned3[folds %in% c(1,2,3),], type = "prob")[,1])

#what variables were most important
#varImpPlot(model_rf_holdout)

dfplot_rf_holdout <- data.frame("pred" = preds_rf_holdout, "cover" = subset_accepted_cleaned3[folds %in% c(1,2,3),]$Matriculating)
thresholdholdoutdf <- ggplot(data = dfplot_rf_holdout) + geom_histogram(aes(x = pred, y =..density.., fill = cover),
                                       position = "identity", bins = 10, alpha = .5) + labs(y = "Density", x = "Threshold/Probability", fill = "") +
                      scale_fill_manual(labels = c("Did Not Matriculate", "Matriculated"), values = c("#F79857", "#61A9B0"))
#roc plot
#plot(roc(dfplot_rf_holdout$cover, dfplot_rf_holdout$pred))


#Logisitic Model -- Not great

#10-fold cv
set.seed(55)
folds <- sample(rep(1:10, length.out = nrow(subset_accepted_cleaned3)), size = nrow(subset_accepted_cleaned3), replace = F)
accuracieslog <- c()
for(x in 1:10){
  model <- glm(data = subset_accepted_cleaned3[folds != x,], Matriculating ~ ., family = "binomial")
  preds <- unname(predict(model,  subset_accepted_cleaned3[folds == x,], type = "response")) 
  preds <- as.factor(ifelse(preds > .5, "YES", "NO"))
  conf <- table(preds, subset_accepted_cleaned3[folds == x,]$Matriculating)
  acc <- sum(diag(conf))/sum(conf)
  accuracieslog <- rbind(accuracieslog, acc)
}
#Box plot of accuracies
dfplot_log_cv <- data.frame(accuracy = unname(accuracieslog[,1]))
log_accuracies_gg <- ggplot(data = dfplot_log_cv) + geom_boxplot(aes(y = accuracy), fill = "palegreen3", color = "grey40") +
  theme_minimal() + scale_y_continuous(breaks = pretty_breaks()) + labs(y = "Accuracy", x = "", title = "", fill = "") +
  theme(axis.text.x = element_blank())



#Hold-out 70%-30%
set.seed(33)
model_log_holdout <- glm(data = subset_accepted_cleaned3[!folds %in% c(1,2,3),], Matriculating ~ ., family = "binomial")
preds_log <- unname(predict(model_log_holdout,  subset_accepted_cleaned3[folds %in% c(1,2,3),], type = "response"))
dfplot_log_holdout <- data.frame("pred" = preds_log, "cover" = subset_accepted_cleaned3[folds %in% c(1,2,3),]$Matriculating)

thresholdholdoutlog <- ggplot(data = dfplot_log_holdout) + geom_histogram(aes(x = pred, y =..density.., fill = cover),
                                                                        position = "identity", bins = 10, alpha = .5) + labs(y = "Density", x = "Threshold/Probability", fill = "") +
  scale_fill_manual(labels = c("Did Not Matriculate", "Matriculated"), values = c("#F79857", "#61A9B0"))

#roc plot
#plot(roc(dfplot_log_holdout$cover, dfplot_log_holdout$pred))







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
    scale_y_discrete(labels = c("Did Not Matriculate", "Matriculated")) + labs(y = '', x = '') +
    scale_color_manual(values = c("#F79857", "#61A9B0")) + theme_minimal() +
    theme(axis.text.x = element_text(hjust = 1, angle = 45), text = element_text(size = 15))
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
    theme_minimal() + theme(axis.text.x = element_text(hjust = 1, angle = 45), legend.position = "none",
      text = element_text(size = 15)) + labs(y = '', x = '') +
    scale_fill_gradientn(colours = c('#cdf1f9', '#059abd'), values = c(0,1)) +
    scale_x_discrete(labels=major.labels) +
    scale_y_discrete(labels = c("Did Not Matriculate", "Matriculated"))
  
}



# Text for pages ----------------------------------------------------------------------------

textGPA <- "Students who matriculated (ended up enrolling) tended to have lower GPAs, on average.  The mean GPA for matriculated students was about 3.54, while the non-matriculated students had mean GPAs of 3.67.  A t-test found that the mean GPAs of matriculated and non-matriculated students were different at a statistically significant level (t = -4.1945, df = 179, p-value = ~4.2x10^-5).  Meaning, there is strong evidence indicating that accepted students who did not enroll had higher average GPAs than accepted students who did enroll."
textGRE <- "Both types of students had highly skewed distributions for GRE scores.  However, accepted students who did not end up enrolling had, on average, higher GRE scores.  The mean GRE score for matriculated students was about 166.23, while the mean score for non-matriculaed students was 167.91.  A t-test found that the mean GRE scores of matriculated and non-matriculated students were different at a statistically significant level (t = -2.604, df = 141, p-value = .0004). "
textMajor <- "In the visual with dots, each dot represents an accepted student, and the color/position of that dot indicates whether they matriculated.  That visual depicts a wide variety of counts for each undergraduate major; because some majors had small counts, it's not possible to run a chi-squared significance test on this entire table.  However, proportions tests were run to see if the percentages of matriculated and non-matriculated students from each individual major were different.  (Refer to the second visual to see the percentages by major.)  No major had statistically significant differences in their proportions of matriculated students.  The only major that got close to having a significant difference was the 'Other' major category, which had a p-value of 0.07."
textGender <- "The first visual indicates that there were relatively consistent counts of accepted students within each gender category.  The next visual shows the proportions by gender.  About 70% of men who were accepted matriculated into the program, while only 61% of women did.  However, a chi-squared test run on the counts used to make these tables did not find a significant difference.  This means that although proportionally more men matriculated in this sample, this difference is not significant. "
textCitz <- "The proportions of matriculated and non-matriculated students by original citizenship are very similar to those found for gender.  As with gender, the differences in matriculation of accepted students by original citizenship was not found to be statistically significant.  Nonetheless, in this sample larger proportions of international students did not matriculate."
textLoc <- "Since the data had a wide variety of school locations, a new feature was created based off of the State and Region variables of a student's previous school.  This new feature has a domain of New England, DMV, South, West, International, and USA/Unknown.  The USA/Unknown variable was used when a graduate was indicated as going to school in the US, but did not have any state data (there were only 4 cases of this).  These visuals show that the majority of accepted students who went to schools in international locations did not matriculate.  Additionally, the majority of accepted students who went to school in the DMV (DC-Maryland-Virginia) ended up matriculating.  A chi-squared test was run on the counts used to create these visuals (excluding USA/Unknown and South due to low counts).  The differences in the number of students matriculating by location was statistically significant  (X-squared = 9.82, df = 3, p-value = 0.0200). "
text_rf <- "Both models were run on a subset of the data that included the features Gender, Dom_Int, Major, GRE, GPA, and the Region variable (discussed in the exploratory section).  This subset of that data also only included graduates where were accepted, because the point is to try to classify whether or not students matriculated.  Models were testing using 10-fold CV, and some of the following plots were creating from a model using hold-out (70% of data was trained on, 30% was used for testing).
<br><br>
 For all parameters (such as the number of trees, and the sample of features selected per tree), the forest model performed poorly.  It appeared to be too flexible, creating unuseful classifications that performed poorly on testing data.  As you can see in the histogram of predicted probabilities (for a class), given the actual class of the data, the classifier is not doing a good job of dividing the classes correctly.  The ROC curve also shows that the classifier performs worse than chance at some thresholds.
<br><br>
However, this model does create an interesting visualization: the variable importance plot.  This plot shows which variables were best at splitting the data into homogeneous groups (i.e. which variables were good at classifying).  It appears GPA, GRE, Major, and the region a student previously went to school in are all very important variables in determining whether or not a student will matriculate.
"
text_log <- "Both models were run on a subset of the data that included the features Gender, Dom_Int, Major, GRE, GPA, and the Region variable (discussed in the exploratory section).  This subset of that data also only included graduates where were accepted, because the point is to try to classify whether or not students matriculated.  Models were testing using 10-fold CV, and some of the following plots were creating from a model using hold-out (70% of data was trained on, 30% was used for testing).
<br><br>
Using a logistic regression turns out to be the way to classify matriculated students for this dataset.  The median accuracy was about 0.7 (seen in the box plot of accuracies found in 10-Fold CV).  The ROC curve shows that the classifier isn't great, but is performing better than chance for.  Additionally, the histogram shows the predicted probabilities (of either matriculated or non-matriculated) calculated on the testing data in hold-out for each actual class value.  Meaning, the histogram plots how well the classifier is separating the matriculated and non-matriculated students.  Generally, it's not doing a great job, but it does look like the classes are separated, somewhat.
"
text_conc <- "The exploratory analysis showed that there were significant differences between the GPAs, GREs, and Regions for matriculated and non-matriculated students.  These features appeared to be the most significant and important in the logistic and random forest models.
<br><br>
Although the model performances left a lot to be desired, their performances are still quite compelling.  This analysis worked with a relatively small sample and with data that was edited for security purposes.  A school with direct access to all admissions data would increase the number of features in the analysis along with overall observations.  This would clearly increase the performance of the classifiers.  Since the classifiers created were performing moderately well with a limited amount of data and features, this indicates classifiers could be improved with more data. 
<br><br>
The analysis shows that creating a model to determine whether a an accepted student will matriculate is possible.  Determining what characteristics influence whether an accepted student will matriculate could help increase efficiency of the college admissions process.  For example, classifier results could pinpoint which students should be targeted with merit scholarships.  Students with probabilities close to 0.5 for both matriculation and non-matriculation, for instance, may be perfect candidates for scholarships.  This could help attract more of the students who would have otherwise decided not to matriculate.
"
# Shiny App ui ------------------------------------------------------------------------------------------------------------------

ui <- navbarPage("Project", id = "project", selected = "Exploritory Analysis", collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"),
                 #Main Nav bar Panels
                 tabPanel("Exploritory Analysis",
                          fluidPage(
                            #nav bar for matriculation page
                            tabsetPanel(
                              createPanel("GPA", textGPA),
                              createPanel("GRE", textGRE),
                              createPaneldiscrete("Major", textMajor),
                              createPaneldiscrete("Gender", textGender),
                              createPaneldiscrete("Citizenship", textCitz),
                              createPaneldiscrete("Location", textLoc)
                            ))),
                 tabPanel("Models",
                          fluidPage(
                            #nav bar for models
                            tabsetPanel(
                              tabPanel("Logistic Model", br(),
                                       fluidRow("Logistic Model Summary",
                                                #CV test plots
                                                column(6, wellPanel(HTML(
                                                  paste("<h1>Logistic Model Summary</h1>",
                                                        "<p>",text_log, "</p>",
                                                        sep = "" ))),
                                                  HTML("<h3>Threshold by Class</h3>"),
                                                  wellPanel(plotOutput(outputId = "thresholdclasslog"))  ),
                                                
                                                column(6, HTML("<h3>10-Fold CV Accuracies</h3>"),
                                                       wellPanel(plotOutput(outputId = "logcvtest")),
                                                       HTML("<h3>Hold-Out Testing ROC Curve</h3>"),
                                                       wellPanel(plotOutput(outputId = "logholdout")))
                                       )),
                              tabPanel("Random Forest Model", br(),
                                       fluidRow("Random Forest Summary",
                                                #CV test plots
                                                column(6, wellPanel(HTML(
                                                  paste("<h1>Random Forest Summary</h1>",
                                                    "<p>",text_rf, "</p>",
                                                    sep = "" ))),
                                                  HTML("<h3>Variable Importance Plot</h3>"),
                                                  wellPanel(plotOutput(outputId = "rfvariableimp")),
                                                  HTML("<h3>Threshold by Class</h3>"),
                                                  wellPanel(plotOutput(outputId = "thresholdclass"))  ),
                                                
                                                column(6, HTML("<h3>10-Fold CV Accuracies</h3>"),
                                                       wellPanel(plotOutput(outputId = "rfcvtest")),
                                                       HTML("<h3>Hold-Out Testing ROC Curve</h3>"),
                                                       wellPanel(plotOutput(outputId = "rfholdout")))
                                        )) ))),
                 tabPanel("Conclusions", br(),
                          fluidPage(fluidRow("Logistic Model Summary",
                                             #CV test plots
                                             column(12, wellPanel(HTML(
                                               paste("<h1>Final Remarks</h1>",
                                                     "<p>",text_conc, "</p>",
                                                     sep = "" ))))
                                    ) ) )
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
  output$countCitizenship <-  renderPlot({ inputdf <- subsetAdmit() %>% dplyr::select(Dom_Int, Matriculating) %>% 
    set_colnames(c("variable", "Matriculating"))
  createcount("Citizenship", inputdf)
  })
  output$countLocation <-  renderPlot(
    { inputdf <- subsetAdmit() %>% dplyr::select(CollegeRegion, State, Matriculating) %>% 
                  mutate(CollegeRegion = as.character(CollegeRegion)) %>% mutate(State = as.character(State))
    #create new region variable
    inputdf$CollegeRegion[inputdf$CollegeRegion == 'USA' & !is.na(inputdf$State)] <- 
      inputdf$State[inputdf$CollegeRegion == 'USA' & !is.na(inputdf$State)]
    inputdf$CollegeRegion <- ifelse(inputdf$CollegeRegion %in% c("Connecticut", "Maine", "Pennsylvania", "New Jersey", "Pittsburgh",
                                        "New York", "Massachusetts"), "New England", ifelse(inputdf$CollegeRegion %in%
                                        c("DC", "Delaware", "Virginia", "Maryland"), "DMV", ifelse(inputdf$CollegeRegion %in% 
                                        c("Canada", "China", "India", "Korea", "Scotland", "Singapore", "Spain", "UK"), "International",
                                        ifelse(inputdf$CollegeRegion %in% c("Georgia", "Alabama", "Florida", "North Carolina", "Tennessee"), "South",
                                        ifelse(inputdf$CollegeRegion == "USA", "USA - Unknown State","West"))  )) )
   
  inputdf <- inputdf %>% dplyr::select(CollegeRegion, Matriculating) %>% set_colnames(c("variable", "Matriculating"))
  createcount("Location", inputdf)
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
  output$propCitizenship <- renderPlot({ inputdf <- subsetAdmit() %>% dplyr::select(Dom_Int, Matriculating) %>% 
    set_colnames(c("variable", "Matriculating"))
  createprop("Citizenship", inputdf)
  })
  output$propLocation <- renderPlot(
    { inputdf <- subsetAdmit() %>% dplyr::select(CollegeRegion, State, Matriculating) %>% 
      mutate(CollegeRegion = as.character(CollegeRegion)) %>% mutate(State = as.character(State))
    #create new region variable
    inputdf$CollegeRegion[inputdf$CollegeRegion == 'USA' & !is.na(inputdf$State)] <- 
                          inputdf$State[inputdf$CollegeRegion == 'USA' & !is.na(inputdf$State)]
    inputdf$CollegeRegion <- ifelse(inputdf$CollegeRegion %in% c("Connecticut", "Maine", "Pennsylvania", "New Jersey", "Pittsburgh",
                                                                 "New York", "Massachusetts"), "New England", ifelse(inputdf$CollegeRegion %in%
                                                                                                                       c("DC", "Delaware", "Virginia", "Maryland"), "DMV", ifelse(inputdf$CollegeRegion %in% 
                                                                                                                                                                                    c("Canada", "China", "India", "Korea", "Scotland", "Singapore", "Spain", "UK"), "International",
                                                                                                                                                                                  ifelse(inputdf$CollegeRegion %in% c("Georgia", "Alabama", "Florida", "North Carolina", "Tennessee"), "South",
                                                                                                                                                                                         ifelse(inputdf$CollegeRegion == "USA", "USA - Unknown State","West"))  )) )
    inputdf <- inputdf %>% dplyr::select(CollegeRegion, Matriculating) %>% set_colnames(c("variable", "Matriculating"))
    createprop("Location", inputdf)
    })
  
  #Random Forest Results
  output$rfvariableimp <- renderPlot({varImpPlot(model_rf_holdout)})
  output$rfcvtest <- renderPlot({rf_accuracies_gg})
  output$rfholdout <- renderPlot({plot(roc(dfplot_rf_holdout$cover, dfplot_rf_holdout$pred))})
  output$thresholdclass <- renderPlot({thresholdholdoutdf})
  
  #Logistic Regression Results
  output$logcvtest <- renderPlot({log_accuracies_gg})
  output$logholdout <- renderPlot({plot(roc(dfplot_log_holdout$cover, dfplot_log_holdout$pred))})
  output$thresholdclasslog <- renderPlot({thresholdholdoutlog})
  
  
  observeEvent(input$project, {
    if(input$project == "Exploritory Analysis"){
      showModal(modalDialog(
        title = "Project Introduction",
        HTML("Colleges may know what types of students they're offering admissions to, but what students are actually enrolling/matriculating?  This is an analysis of Georgetown's admissions data, looking specifically at the students accepted into Georgetown.  Determining what characteristics influence whether an accepted student will matriculate could help the efficiency of the college admissions process.  For example, classifier results could 
pinpoint which students should be targeted with merit scholarships.  Students with probabilities close to 0.5 for both matriculation and non-matriculation, for instance, may be perfect candidates for scholarships. <br><br>
 The first section contains a thorough exploratory analysis of some key features in the dataset.  More specifically, feature characteristics were examined for accepted students by students' matriculation statuses.  Then, various models were created to determine how well the data can be classified by matriculated and non-matriculated students."),
        size = 'm',
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  

}





shinyApp(ui, server)




