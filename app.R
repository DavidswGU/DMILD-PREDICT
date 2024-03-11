library(shiny)
library(ranger)
library(randomForest)
library(tidymodels)
library(caret)
library(dplyr)
library(MLmetrics)
library(tune)
library(parsnip)
library(bonsai)
library(shapr)
library(party)
ui <- fluidPage(
    titlePanel("DMILD-PREDICT: Predictions for 180-day mortality in DM-ILD patients"),
    tabsetPanel(type = "tabs",
                tabPanel("Usage Instructions", 
                         fluidRow(
                           column(6,offset = 0.5,
                                  helpText("Instructions for Basic Usage"),
                                  htmlOutput("help")
                           ),
                           column(4,offset = 1,
                                  imageOutput("logo1")
                           )
                         )
                ),
                tabPanel("Prediction",
                         fluidRow(
                           column(4,
                                  helpText("Imaging Diagnosis"),
                                  checkboxInput("NSIP","Non-specific Interstitial Pneumonia"),
                                  checkboxInput("DAD","Diffusal Alveolar Damage"),
                           ),
                           column(4,
                                  helpText("Laboratory Test"),
                                  numericInput("CD4","CD4+ Lymphocyte (cell/uL)",value = 0,step=0.01),
                                  numericInput("CD8","CD8+ Lymphocyte (cell/uL)",value = 0,step=0.01),
                                  numericInput("CD3","CD3+ Lymphocyte (cell/uL)",value =  0,step=0.01),
                                  numericInput("LY","Total Lymphocyte Count (*10^9/L)",value = 0,step=0.01),
                                  numericInput("NE_pct","Neutrophil (% of all WBC)",value = 0,step=0.01),
                                  numericInput("NE","Neutrophil Count (*10^9/L)",value = 0,step=0.01),
                                  numericInput("PCT","Procalcitonin (ng/mL)",value = 0,step=0.01),
                                  numericInput("ALB","Albumin (g/L)",value = 0,step=0.01),
                                  checkboxInput("MDA5","MDA5 Antibody Positive")
                           ),
                           column(4,
                                  helpText("Clinical Manifestation"),
                                  numericInput("PO2","PaO2 (mmHg)",value = 0,step=0.01),
                                  checkboxInput("RPILD","Rapid progressive interstitial lung disease"),
                                  actionButton("calculate","Predict",width = 200),
                                  htmlOutput("predictions")
                                  )
                         )
                ),
                tabPanel("Visualization", 
                         plotOutput("shap"),
                         htmlOutput("abbr"),
                         actionButton("visualize","SHAP visualization"),
                         htmlOutput("progress")
                ),
                tabPanel("Study Data",
                         dataTableOutput("traindata"),
                         htmlOutput("abbr2"),
                         fluidRow(
                           column(3,
                                  downloadButton("download","Download data"),
                           ),
                           column(4,
                                  actionButton("switchdv","Switch discovery/validation cohort"),
                                  htmlOutput("cohort")
                           )
                         )
                )
    )
)

server <- function(input, output, session) {
  set.seed(58)
  load(file = "rf.model.RData")
  train.data.select <- read.csv(file = "DM_ILD_train.csv")
  test.data.select <- read.csv(file = "DM_ILD_validation.csv")
  cohort.current <- reactiveVal(train.data.select)
  NSIP <- reactive({
    temp <- "No"
    if(input$NSIP==T) temp="Yes"
    temp
  })
  DAD <- reactive({
    temp <- "No"
    if(input$DAD==T) temp="Yes"
    temp
  })
  MDA5 <- reactive({
    temp <- "No"
    if(input$MDA5==T) temp="Yes"
    temp
  })
  RPILD <- reactive({
    temp <- "No"
    if(input$RPILD==T) temp="Yes"
    temp
  })  
  data1 <- reactive({
    data.frame(
      NSIP=NSIP(),
      DAD=DAD(),
      RPILD=RPILD(),
      MDA5=MDA5(),
      NE_pct=input$NE_pct,
      LY=input$LY,
      N_L=(input$NE)/(input$LY),
      PCT=input$PCT,
      ALB=input$ALB,
      CD3=input$CD3,
      CD4=input$CD4,
      CD8=input$CD8,
      PO2=input$PO2,
      surv="Survived"
    )
  })
  
  predicting <- eventReactive(input$calculate,{
        data.recipe <- recipes::recipe(surv~.,data = train.data.select)%>%
          recipes::step_string2factor(all_nominal())%>%
          prep()
        test.prediction <- rf.model.final%>%parsnip::fit(formula = surv~.,
                                                         data = bake(data.recipe,new_data=train.data.select)
        )%>%
          predict(new_data=bake(data.recipe,new_data=data1()),type="prob")
        pDeath <- test.prediction$.pred_Deceased
        mortality <- paste("180-day mortality (%):",100*round(pDeath,4))
        predict <- HTML(mortality)
    predict 
  })
  
  visualization <- eventReactive(input$visualize,{
    data.recipe <- recipes::recipe(surv~.,data = train.data.select)%>%
      recipes::step_string2factor(all_nominal())%>%
      prep()
    data.recipe$var_info$type[[13]] <- c("double","numeric")
    predict_model <- function(model,newdata){
      test.prediction <-predict(model,as.data.frame(newdata),type="prob")
      test.prediction <- test.prediction$.pred_Deceased
    }
    rf.fit <- rf.model.final%>%fit(formula = surv~.,
                                   data = bake(data.recipe,new_data=train.data.select)
    )
    train.data.baked <- bake(data.recipe,new_data=train.data.select)
    train.data.baked$PO2 <- as.double(train.data.baked$PO2)
    test.data.baked <- bake(data.recipe,new_data=data1())
    test.data.baked$NE_pct <- as.double(test.data.baked$NE_pct)
    test.data.baked$LY <- as.double(test.data.baked$LY)
    test.data.baked$N_L <- as.double(test.data.baked$N_L)
    test.data.baked$PCT <- as.double(test.data.baked$PCT)
    test.data.baked$ALB <- as.double(test.data.baked$ALB)
    test.data.baked$CD3 <- as.double(test.data.baked$CD3)
    test.data.baked$CD4 <- as.double(test.data.baked$CD4)
    test.data.baked$CD8 <- as.double(test.data.baked$CD8)
    test.data.baked$PO2 <- as.double(test.data.baked$PO2)
    p0=0
    explanation.rf <- explain(
      model=rf.fit,
      x_explain=test.data.baked[,-14],
      x_train=train.data.baked[,-14],
      approach="ctree",
      prediction_zero=p0,
      predict_model=predict_model,
      n_batches=1
    )
    plot1 <- plot(explanation.rf, bar_plot_phi0 = FALSE,col = c("#e66f51","#8ab07d"),pch=20)
    plot1
  })
  
  observeEvent(input$switchdv,{
    if(identical(cohort.current(),train.data.select)){
      cohort.current(test.data.select)
    }else{
      cohort.current(train.data.select)
    }
  })
  
  abbr <- eventReactive(input$visualize,{
    text <- HTML("Abbreviations: RPILD rapidly progressive interstitial lung disease; 
         DAD diffuse alveolar damage; 
         NE_pct percentage of neutrophil; 
         N_L neutrophil to lymphocyte ratio;
         LY lymphocyte count;
         NSIP nonspecific interstitial pneumonia; 
         PCT procalcitonin;
         MDA5 anti-melanoma differentiation-associated gene 5;
         PO2 arterial oxygen pressure;
         ALB albumin.")
    text
  })
  
  output$logo1 <- renderImage({
    list(src="Picture1.png")
  },deleteFile = FALSE)
  
  output$abbr <- renderUI(abbr())
  
  output$shap <- renderPlot(visualization())
  
  output$predictions <- renderUI(predicting())
  
  output$help <- renderUI({
    HTML(paste("1. Predicting outcomes for cases using DMILD-PREDICT",
              "   Enter the values for all predictors in Imaging Diagnosis, Laboratory Test and Clinical Manifestation.", 
              "   Press Predict button for probability of 180-day mortality.",
              "2. Visualizing the prediction",
              "   Enter the values for all predictors in Imaging Diagnosis, Laboratory Test and Clinical Manifestation.",
              "   Press SHAP Visualization (this may take several minutes)",
              "Please contact davidgu@ccmu.edu.cn if you encounter any problem.",
      sep = '<br/>'))
  })
  
  output$traindata <- renderDataTable(cohort.current())
  
  output$abbr2 <- renderUI({
    HTML("Abbreviations: NSIP nonspecific interstitial pneumonia;
    DAD diffuse alveolar damage;
    RPILD rapidly progressive interstitial lung disease;
    MDA5 anti-melanoma differentiation-associated gene 5;
    NE_pct percentage of neutrophil;
    LY lymphocyte count;
    N_L neutrophil to lymphocyte ratio; 
    PCT procalcitonin;
    ALB albumin;     
    PO2 arterial oxygen pressure;
    surv 180-day survival.")
  })
  
  output$cohort <- renderUI({
    if(identical(cohort.current(),train.data.select)){
      HTML("Now showing: Discovery cohort")
    }else{
      HTML("Now showing: Validation cohort")
    }
  })
  
  output$download <- downloadHandler(filename = function(){
    if(identical(cohort.current(),train.data.select)){
      return("DM_ILD_Discovery.csv")
    }else{
      return("DM_ILD_Validation.csv")
    }
  },
  content = function(file){
    write.csv(cohort.current(),file) 
  }
                 )
}

# Run the application 
shinyApp(ui = ui, server = server)
