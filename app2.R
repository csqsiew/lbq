# warwick version: https://csqsiew.shinyapps.io/warwick_lbq_pat/ 
# city u version: https://csqsiew.shinyapps.io/lbq 

# Example of a Survey using the ShinyPsych package
#
# Code sections:
#   - Section 0: Load Libraries
#   - Section A: assign external values
#   - Section B: Define overall layout
#   - Section C: Define reactive values
#   - Section D: Page layouts
#   - Section F: Event (e.g. button) actions
#       - Section F1: Page navigation button
#       - Section F2: Event Control
#   - Section G: Save Data

# Section 0: Load Libraries ====================================================

library(shiny)
library(shinyjs)
library(ShinyPsych)
library(stringr)


# Section A: assign external values ============================================

# Dropbox directory to save data
outputDir <- "ShinyPsych/lbq"

# Vector with page ids used to later access objects
idsVec <- c("Instructions", "Survey", "Demographics", "Pregoodbye", "Goodbye") 

# create page lists for the instructions and the last page
instructions.list <- createPageList(fileName = "Instruction.txt",
                                    globId = "Instructions", defaulttxt = F) # set defaulttxt to false
survey.list <- createPageList(fileName = "Language1.txt",
                              globId = "Survey", defaulttxt = F)

demographics.list <- createPageList(fileName = "Language2_1.txt", globId = 'Demographics', defaulttxt = F)
demographics.list2 <- createPageList(fileName = "Language2_2.txt", globId = 'Demographics', defaulttxt = F)
demographics.list3 <- createPageList(fileName = "Language2_3.txt", globId = 'Demographics', defaulttxt = F)
demographics.list4 <- createPageList(fileName = "Language2_4.txt", globId = 'Demographics', defaulttxt = F)

pregoodbye.list <- createPageList(fileName = "Pregoodbye.txt",  globId = 'Pregoodbye', defaulttxt = F)

goodbye.list <- createPageList(fileName = "Goodbye.txt",  globId = 'Goodbye', defaulttxt = F)


# Section B: Define overall layout =============================================

ui <- fixedPage(
  
  # App title
  title = "LBQ",
  uiOutput("MainAction"),
  
  # For Shinyjs functions
  useShinyjs(),
  
  # include appropriate css and js scripts
  includeScriptFiles()
  
)

server <- function(input, output, session) {
  
  output$MainAction <- renderUI( {
    PageLayouts()
    
  })
  
  # Section C: Define Reactive Values ==========================================
  
  # CurrentValues controls page setting such as which page to display
  CurrentValues <- createCtrlList(firstPage = "Instructions", # id of the first page
                                  globIds = idsVec,           # ids of pages for createPage
                                  complCode = F) #,           # create a completion code
  # complName = "Survey")    # first element of completion code
  
  # Section D: Page Layouts ====================================================
  
  PageLayouts <- reactive({
    
    # insert created completion code that it can later be displayed
    #goodbye.list <- changePageVariable(pageList = goodbye.list, variable = "text",
    #                                   oldLabel = "completion.code",
    #                                   newLabel = CurrentValues$completion.code)
    
    # display instructions page
    if (CurrentValues$page == "Instructions") {
      
      return(
        # create html logic of instructions page
        createPage(pageList = instructions.list,
                   pageNumber = CurrentValues$Instructions.num,
                   globId = "Instructions", ctrlVals = CurrentValues)
      )}
    
    # display survey page
    if (CurrentValues$page == "Survey") {
      
      return(
        # create html logic of instructions page
        createPage(pageList = survey.list,
                   pageNumber = CurrentValues$Survey.num,
                   globId = "Survey", ctrlVals = CurrentValues)
      )}
    
    
    ### Language

    store <- unlist(str_extract_all(input$Survey_l2, "[A-Za-z]+"))
    
    if(length(store) == 4) {
      demographics.list <- demographics.list4
    } else if(length(store) == 3) {
      demographics.list <- demographics.list3
    } else if(length(store) == 2) {
      demographics.list <- demographics.list2
    } else if(length(store) == 1) {
      demographics.list <- demographics.list
    } else {
      demographics.list <- demographics.list4
    }
    
    demographics.list <- changePageVariable(pageList = demographics.list, variable = 'text',
                                            oldLabel = 'x[1]', newLabel = store[1])
    demographics.list <- changePageVariable(pageList = demographics.list, variable = 'text',
                                            oldLabel = 'x[2]', newLabel = store[2])
    demographics.list <- changePageVariable(pageList = demographics.list, variable = 'text',
                                            oldLabel = 'x[3]', newLabel = store[3])
    demographics.list <- changePageVariable(pageList = demographics.list, variable = 'text',
                                            oldLabel = 'x[4]', newLabel = store[4])
    
    
    if (CurrentValues$page == "Demographics"){
      
      return(
        createPage(pageList = demographics.list, pageNumber = CurrentValues$Demographics.num,
                    globId = "Demographics", ctrlVals = CurrentValues)
      )}
    
    # test 
    if (CurrentValues$page == "Pregoodbye") {
      
      return(
        # create html logic of instructions page
        createPage(pageList = pregoodbye.list,
                   pageNumber = CurrentValues$Pregoodbye.num,
                   globId = "Pregoodbye", ctrlVals = CurrentValues)
      )}
    
    goodbye.list <- changePageVariable(pageList = goodbye.list, variable = 'text',
                                            oldLabel = 'id_code', 
                                       newLabel = input$Survey_partid)
    
    # P5) Goodbye
    if (CurrentValues$page == "Goodbye") {
      
      return(
        createPage(pageList = goodbye.list, pageNumber = CurrentValues$Goodbye.num,
                   globId = "Goodbye", ctrlVals = CurrentValues, continueButton = FALSE)
      )}
    
  })
  
  
  # Section F: Event (e.g.; button) actions ======================================
  
  # Section F1: Page Navigation Buttons ----------------------
  
  
  observeEvent(input[["Instructions_next"]],{
    nextPage(pageId = "Instructions", ctrlVals = CurrentValues, nextPageId = "Survey",
             pageList = instructions.list, globId = "Instructions")
  })
  
  observeEvent(input[["Survey_next"]],{
    nextPage(pageId = "Survey", ctrlVals = CurrentValues,
             nextPageId = "Demographics", pageList = survey.list,
             globId = "Survey")
  })
  
  observeEvent(input[["Demographics_next"]],{
    nextPage(pageId = "Demographics", ctrlVals = CurrentValues,
             nextPageId = "Pregoodbye", pageList = demographics.list,
             globId = "Demographics")
  })
  

  # Section F2: Event Control ----------------------
  
  
  # Make sure answers are selected
  observeEvent(reactiveValuesToList(input),{
    
    onInputEnable(pageId = "Instructions", ctrlVals = CurrentValues,
                  pageList = instructions.list, globId = "Instructions",
                  inputList = input, charNum = 1)
    
    onInputEnable(pageId = "Survey", ctrlVals = CurrentValues,
                  pageList = survey.list, globId = "Survey",
                  inputList = input, charNum = 1)
    
    onInputEnable(pageId = "Demographics", ctrlVals = CurrentValues, ######## to ensure that inputs are enabled for all 4 versions 
                  pageList = demographics.list, globId = "Demographics",
                  inputList = input, charNum = 1)
    onInputEnable(pageId = "Demographics", ctrlVals = CurrentValues,
                  pageList = demographics.list2, globId = "Demographics",
                  inputList = input, charNum = 1)
    onInputEnable(pageId = "Demographics", ctrlVals = CurrentValues,
                  pageList = demographics.list3, globId = "Demographics",
                  inputList = input, charNum = 1)
    onInputEnable(pageId = "Demographics", ctrlVals = CurrentValues,
                  pageList = demographics.list4, globId = "Demographics",
                  inputList = input, charNum = 1)

  })
  
  # Section G: Save data =========================================================
  
  observeEvent(input[["Pregoodbye_next"]], {(
    
    # Create progress message
    withProgress(message = "Saving data...", value = 0, {
      
      incProgress(.25)
      
      # Create a list to save data
      data.list <- list(  
        #"id" = input$Instructions_workerid,
        #"code" = completion.code,
        "partid" = input$Survey_partid,
        "age" = input$Survey_age,
        "gender" = input$Survey_gender,
        "edu" = input$Survey_edu,
        "hear1" = input$Survey_hear1,
        "hear2" = input$Survey_hear2,
        "vision1" = input$Survey_vision1,
        "vision2" = input$Survey_vision2,
        "native" = input$Survey_l1,
        "lang" = input$Survey_l2,
        "aoa1" = input$Demographics_aoa1,
        "aoa2" = input$Demographics_aoa2,
        "aoa3" = input$Demographics_aoa3,
        "aoa4" = input$Demographics_aoa4,
        "y1" = input$Demographics_y1,
        "y2" = input$Demographics_y2,
        "y3" = input$Demographics_y3,
        "y4" = input$Demographics_y4,
        "c1" = input$Demographics_c1,
        "c2" = input$Demographics_c2,
        "c3" = input$Demographics_c3,
        "c4" = input$Demographics_c4,
        "test1" = input$Demographics_test1,
        "test2" = input$Demographics_test2,
        "toefl" = input$Demographics_toefl,
        "toefl_listen" = input$Demographics_toefl_listen,
        "toefl_speak" = input$Demographics_toefl_speak,
        "ielts" = input$Demographics_ielts,
        "ielts_listen" = input$Demographics_ielts_listen,
        "ielts_speak" = input$Demographics_ielts_speak,
        "othertest" = input$Demographics_othertest,
        "listen1" = input$Demographics_listen1,
        "listen2" = input$Demographics_listen2,
        "listen3" = input$Demographics_listen3,
        "listen4" = input$Demographics_listen4,
        "speak1" = input$Demographics_speak1,
        "speak2" = input$Demographics_speak2,
        "speak3" = input$Demographics_speak3,
        "speak4" = input$Demographics_speak4,
        "accent" = input$Demographics_accent,
        "percent1" = input$Demographics_percent1,
        "percent2" = input$Demographics_percent2,
        "percent3" = input$Demographics_percent3,
        "percent4" = input$Demographics_percent4,
        "instruct1" = input$Demographics_instruct1,
        "instruct2" = input$Demographics_instruct2,
        "instruct3" = input$Demographics_instruct3,
        "instruct4" = input$Demographics_instruct4
      )
      
      saveData(data.list, location = "dropbox", outputDir = outputDir,
               partId = 'LBQ_')
      
      CurrentValues$page <- "Goodbye"
      
    })
    
  )})
  
}

# Create app!
shinyApp(ui = ui, server = server)
