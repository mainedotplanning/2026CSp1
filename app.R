# 2026 Bridge Scoping Reviews

# fix the zoom to bridge location - done
# update the output google doc link for new page - done
# clear responses when switching projects - done
# review the list of bridges to make sure they are all in here - done - added large culverts

# create the highway projects app
# see if expressway roadway context can be added

#rsconnect::writeManifest()

library(shiny)
library(shinyjs)
library(shinyBS)
library(tidyverse)
library(bslib)
library(DT)
library(leaflet)
library(sf)
library(googlesheets4)
library(jsonlite)
library(htmltools)
library(googledrive)


######
#-------------------------------
# Global Options

# Google Sheets authentication for shiny.io server

gs4_auth(cache = ".secrets", email = "kathryn.grond@maine.gov")

response_sheet = c("https://docs.google.com/spreadsheets/d/11F4HfNUOtYcFETGcO9y8KnXnIpzWjsApN4O7emppA_Y/")

# load shapefiles

load("app_shapefiles_2026.RData")
load("app_datafiles_2026.RData")

# Mandatory fields
fieldsMandatory <- c("checklist_person")
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}


fieldsAll <- c("context_agree", "context_other", "multi_context", "context_AT", "users_AT", 
               "srl_review",  "hcl_review", "ka_review","bp_review",  "safety_comment", 
               "facility_review", "ada_review", "transit_review",  "speed_review", "speed_comment", "landuse",
               "future_landuse", "future_plans", "road_works", "env_constraint", "comp_plan",    
               "plan_comments", "MPO", "MPO_comments", "RPO", "RPO_comments", "Muni", "Muni_comments", 
               "CSelements_comments", "checklist_person", "date")



responsesDir <- file.path("responses")
epochTime <- function() {
  as.integer(Sys.time())
}

# define CSS formatting
appCSS <-
  ".mandatory_star { color: red; }
   #error { color: red; }"



#-------------------------------
# UI

ui <- function(request) {
  
  fluidPage(
    shinyjs::useShinyjs(), #call the package shinyjs in the ui
    shinyjs::inlineCSS(appCSS), # adds CSS formatting to the ui

    
    tags$head(
      tags$style(type = "text/css", 
                         "html, body {width:100%;height:100%}",
                         ".leaflet .legend i{
                   width: 10px;
                   height: 10px;
                   margin-top: 4px;
                   }
                   "
    ),
      tags$style(HTML("
      .panel-heading {
        background-color: #4CAF50 ; 
        color: white ;
        
        
      }
    "))
    ),

    # -----
    
    page_navbar(title = "Complete Streets Scoping Review - 2026 Bridges",bg = "#476F84FF",
                 
                sidebar = sidebar(id = "sidebar",  width = "40%", bg = "#FAECCFFF", #"#A4BED5FF",
                                  h3( "Project Filters"),
                                  #p("It can take up to 30 seconds to load all the dashboard elements"),
                                  accordion(multiple=FALSE, 
                                            accordion_panel(
                                              "Step 1 Select Reviewer", icon = bsicons::bs_icon("sliders"),
                                              class = "primary",
                                              
                                              selectInput(
                                                "var",
                                                label = "Select Reviewer",
                                                choices = 
                                                  c("Claire", "Dakota", "Jarod", "Matt", "Mike")
                                              )
                                            ),
                                            
                                            accordion_panel(
                                              "Step 2 Select Project", icon = bsicons::bs_icon("sliders"),
                                              
                                              # Response table
                                              DT::dataTableOutput("table1")
                                            ),
                                            accordion_panel(
                                              "Step 3 Saved Responses", icon = bsicons::bs_icon("menu-app"),
                                              
                                              p("Saved responses can be edited directly in google sheets: "),
                                              HTML('<a href=response_sheet 
                     target="_blank">Completed Responses Table</a>'),
                                              br(),
                                              p(" or you can view saved responses below:"),
                                              
                                              # Completed Table
                                              DT::dataTableOutput("table2"),
                                              br(),
                                              DT::dataTableOutput("table3"),
                                              
                                            )
                                  )
                                  
                ),
                        
                        
      nav_panel("Checklist", 
                
                fluidPage(
                  shinyjs::useShinyjs(),
                  shinyjs::inlineCSS(appCSS),
                  #titlePanel("Checklist"),
                  
                  # Form Page
                  
                  div(
                    id = "form",
                    br(),
                    
                    card(
                      title = "Project Details",
                      collapsible = TRUE,
                      
                      h4("Project Details"),
                      br(),
                      
                      DT::dataTableOutput("projectDetails"),
                      
                      br(),
                      
                      leafletOutput("contextmap", width = "100%", height = "450px"),
                      
                    ),
                    
                    
                    br(),
                    
                    card(
                      title = "Project Context Review",
                      collapsible = FALSE,
                      h5("Project Context Review"),
                      
                      radioButtons("context_agree", "1. Do you agree with the Roadway Context Classification listed in the map and table above?", 
                                   choices = list("Yes" = "Yes", "No" = "No" ,"Not Sure" = "Not Sure", "Multiple Context Classifications" = "Multiple Context Classifications"), 
                                   selected ="",width = "75%"),
                      
                      textAreaInput("context_other", "  If you think there is an issue with the context classification, please specify:","",
                                    width = "75%", cols = 200, rows = 3),
                      
                      textAreaInput("multi_context", "  If the project spans multiple road contexts, please list them here:","",
                                    width = "75%", cols = 200, rows = 3),
                      
                      textAreaInput("context_AT", "2. Identify any active transportation destinations near the project, e.g. schools, trails, stores, tourism destinations.","",
                                    width = "75%", cols = 200, rows = 3),
                      
                      textAreaInput("users_AT", "3. Identify current road users expected in this location.","",
                                    width = "75%", cols = 200, rows = 3),
                      
                    ),

   
                    
                    br(),
                    
                    card(
                      title = "Project Safety Context Review",
                      collapsible = FALSE,
                      h5("Project Safety Context Review"),
                      p("Please check the following map for safety data, including HCL, SRL and 10 year crash data."),
                      
                      leafletOutput("safetymap", width = "100%", height = "450px"),
                      
                      br(),
                      
                      radioButtons("srl_review", "4. Is the project near a Safety Review Location?", 
                                   choices = list("Yes"= "Yes", "No"= "No", "Not Sure"= "Not Sure"), 
                                   selected ="", width = "75%"),
                      
                      # textAreaInput("SRL_comment", "  If yes, please specify the safety concern after consulting with the Safety Office.","",
                      #               width = "75%", cols = 200, rows = 3),
                      
                      radioButtons("hcl_review", "5. Is the project near a High Crash Location?", 
                                   choices = list("Yes"= "Yes", "No"= "No", "Not Sure"= "Not Sure"), 
                                   selected ="", width = "75%"),
                      
                      radioButtons("ka_review", "6. Are there Serious Injury or Fatal Crashes in the project area?", 
                                   choices = list("Yes"= "Yes", "No"= "No", "Not Sure"= "Not Sure"), 
                                   selected ="", width = "75%"),
                      
                      radioButtons("bp_review", "7. Are there Bicycle or Pedestrian Crashes in the project area?", 
                                   choices = list("Yes"= "Yes", "No"= "No", "Not Sure"= "Not Sure"), 
                                   selected ="", width = "75%"),
                      
                      textAreaInput("safety_comment", "8. Record any safety concerns from the crash data and from discussions with the Safety Office.", "", 
                                    width = "75%", cols = 200, rows = 3),
                      
                    ),

                    br(),
                    h4("If the roadway context is Rural and there are no safety concerns or 
                       active transportation destinations, you have completed the review.
                       (scroll down to submit the form)"),  
                    br(),
                    h4("Otherwise, please continue with the checklist below."),
                    
                    br(),
                    
                    card(
                      title = "Planning Context Review",
                      collapsible = TRUE,
                      h5("Planning Context Review"),
                      
                      textAreaInput("facility_review", "9. Identify the current condition of any existing complete streets elements or facilities.", "", 
                                    width = "75%", cols = 200, rows = 3),
                      
                      textAreaInput("ada_review", "10. Identify any connectivity or ADA compliance concerns with any existing complete streets elements or facilities.", "", 
                                    width = "75%", cols = 200, rows = 3),
                      
                      textAreaInput("transit_review", "11. Identify any transit routes, facilities or stops near the project location.", "", 
                                    width = "75%", cols = 200, rows = 3),

                      radioButtons("speed_review", "12. Does the current posted speed exceed the appropriate speed range for the roadway context classification? Please consult the table below.", 
                                   choices = list("Yes" = "Yes", "No" = "No", "Not Sure" = "Not Sure"), 
                                   selected = "", width = "75%"),
                      
                      textAreaInput("speed_comment", "If the speed exceed the guidelines, please record recommendations from traffic and engineering.", "", 
                                    width = "75%", cols = 200, rows = 3),  
                
                      img(src='speed_limit_table.png', align = "center", width = "75%"),

                      
                      textAreaInput("landuse", "13. What are the existing land use types adjacent to the project?", "", 
                                    width = "75%", cols = 200, rows = 3),
                      
                      textAreaInput("future_landuse", "14. Do the municipal plans or comprehensive plans identify future land use and development?", "", 
                                    width = "75%", cols = 200, rows = 3),
                      
                      textAreaInput("future_plans","15. Do the municipal plans or comprehensive plans mention new active transportation facilities in or near the project area?","", 
                                    width = "75%", cols = 200, rows = 3),
                      
                      textAreaInput("road_works", "16. Identify recent and planned project candidates or paving in/near the project area.", "", 
                                    width = "75%", cols = 200, rows = 3),
                      
                      textAreaInput("env_constraint","17. Are there potential environmental or right of way constraints? for example: wetlands, historic properties, 4f properties, ledges, etc.","", 
                                    width = "75%", cols = 200, rows = 3),

                      radioButtons("comp_plan", "18. Review of Municipal Plans, Comprehensive Plans or other relevant local documents.", 
                                   choices = list("Completed" = "Completed", "In progress" = "In progress" , "Not Applicable" = "Not Applicable"), selected ="",width = "75%"),
                      
                      textAreaInput("plan_comments", "19. Please include any other relevant notes pertaining to the local plan review.","",
                                    width = "75%", cols = 200, rows = 5),
                      
                    ),
                    
                    card(
                      title = "Engagement and Outreach",
                      collapsible = TRUE,
                      h5("Engagement and Outreach"),
                      
                      actionButton("showModal", "Notes on municipal outreach"),
                      
                      radioButtons("MPO", "20. Notify relevant Metropolitan Planning Organization (MPO) staff of the potential project.", 
                                   choices = list("Complete" = "Complete", "In Progress" = "In Progress" ,  "Not Applicable" = "Not Applicable"), 
                                   selected ="", width = "75%"),
                      
                      textAreaInput("MPO_comments", "21. Record MPO staff engagement and comments:","",
                                    width = "75%", cols = 200, rows = 3),
                      
                      radioButtons("RPO", "22. Notify relevant Regional Planning Organization (RPO) staff of the potential project.", 
                                   choices = list("Complete" = "Complete", "In Progress" = "In Progress" , "Not Applicable" = "Not Applicable"), 
                                   selected ="", width = "75%"),
                      
                      textAreaInput("RPO_comments", "23. Record RPO Staff engagement and comments:","",
                                    width = "75%", cols = 200, rows = 3),
                      
                      radioButtons("Muni", "24. Notify relevant Municipal staff of the potential project.", 
                                   choices = list("Complete" = "Complete", "In Progress" = "In Progress" ,  "Not Applicable" = "Not Applicable"), 
                                   selected ="", width = "75%"),
                      
                      textAreaInput("Muni_comments", "25. Record Municipal staff engagement and comments:","",
                                    width = "75%", cols = 200, rows = 3),
                      
                      textAreaInput("CSelements_comments", "26. Record the Complete Streets Elements, to be included in PE, that are appropriate for the project's scope and context.","",
                                    width = "75%", cols = 200, rows = 6),
                      
                    ),
      
                    h5("Ready to save your answers?"),
                    
                    selectInput("checklist_person",
                                label = "", labelMandatory("Completed by: "), width = "75%",
                                choices =  c("Claire",  "Dakota", "Jarod", "Matt", "Mike")
                    ),
                    
                    dateInput("date", 
                              label = "Choose a date:", 
                              value = Sys.Date(), 
                              min = "2026-01-01", 
                              max = "2026-12-31"),
                    
                    actionButton("submit", "Save", class = "btn-primary"),
                    
                    br(),
                    
                    shinyjs::hidden(
                      span(id = "submit_msg", "Submitting..."),
                      div(id = "error",
                          div(br(), tags$b("Error: "), span(id = "error_msg"))
                      )
                    )
                  ),
                  
                  # Thank you message page
                  
                  shinyjs::hidden(
                    div(
                      id = "thankyou_msg",
                      h3("Thanks, your response was submitted successfully!"),
                      actionLink("submit_another", "Start another checklist")
                    )
                  ))
                ),
      
      nav_panel("Elements By Scope", 
                
                img(src='AppendixA_Consideration By Scope Matrix_3.25.25.png', align = "center"),
                
      ),
      
      nav_panel("Elements By Context", 
                
                img(src='AppendixB_Complete Streets Elements By Context_3.25.25_Page_1.png', align = "center"),
                
      )   
    ), 
    
    
    tagQuery(bsModal("myModal", "Municipal Engagement Process", "showModal", 
            size = "large", 
            p("In accordance with the 2024 MaineDOT Complete Streets Policy and implementation guidance, 
            MaineDOT staff will be utilizing a checklist to assist in scoping of most MaineDOT infrastructure 
            projects by evaluating project area context, community needs, project location characteristics, 
            and municipal plans. Part of this checklist process involves outreach to relevant municipalities 
            based on each project being scoped via this process. The following process should be considered 
            the standard operating procedure for engaging with municipalities in Maine during project scoping. 
            Some cases will require more in depth engagement that will be dictated by specific project scope needs, 
            project area considerations, or other factors involved in complex projects beyond what is outlined in 
            this process."),
            br(),
            p("1.	Complete the Project Scoping Checklist with all available relevant information. Information 
            that is missing or requires further coordination can be added later if needed."),
            p("2.	Once the Scoping Checklist is substantially complete, and all relevant project scope and 
            background information is known, begin engagement with relevant municipalities. It may be beneficial 
            to bundle engagement with municipalities to include engagement regarding all planned MaineDOT projects being scoped, 
              if multiple projects are being scoped in those municipalities at the same time.  "),
            p("3.	Municipal engagement should be directed to Municipal staff. Depending on the organization of 
            each municipality, that staff may include the Town/City Manager, Public Works Director, Planner, 
            Select Board/Council Chairperson, Clerk, or other authorized municipal official. This engagement 
            may be conducted via email and/or virtual or in-person coordination as 
              dictated by the needs of the project and the preferences of those involved.  "),
            p("4.	When conducting this project scoping engagement, MaineDOT should include the following:  "),
            p(" i.	Overview of intended project scope, potential schedule, location, project history, and other 
              relevant past or planned MaineDOT efforts. "),
            p(" ii.	Overview of any notable outcomes from Project Scoping Checklist (safety concerns, scope 
            recommendations, questions about local plans or local use of the transportation system, etc.)"),
            p(" iii.	Does the municipality have any written plans, policies, or documents regarding transportation, 
            land use, development, or other local needs relevant to the project that MaineDOT is not aware of?"),
            p("iv. Does the municipality have any concerns regarding existing conditions of the project area that 
            MaineDOT should be aware of? (This could be relating to safety, drainage, public use, maintenance, or 
            other conditions specific to the project.)"),
            p("v.	At this time, based on the project overview and Project Scoping Checklist, does the municipality 
            have any additional concerns or requests relating to the future design and implementation of the project? 
            (MaineDOT should remind the Municipality that most projects will have standard public engagement 
            opportunities as the design and project development process progresses.)"),
            p(" 3.	Capture any concerns, questions, and requests received from the Municipality and update the
              Project Scoping Checklist as needed with that information. Any items that need to be addressed later 
              in the design process should be noted so Project Development can address those when appropriate. ")
    ))$find("button")$addAttrs("style" = "display:none;")$allTags()
           
    )
    
}

# Server
  
  server <- function(input, output, session) {
    
    # activate mandatory fields
    
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)

      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
    
    # Set reactive value to table filtering
    v <- reactiveValues()
    v$s <- 1 
    v$w <- 1
    v$n <- 1
    

    
    # Reactive expression to filter project list table in sidebar
    filteredList <- reactive({
      filteredList <- projectlist[projectlist$`Checklist Completed By` == input$var,1:2]  
    })

    output$table1 <- renderDataTable({
      datatable(filteredList(), selection = 'single', options = list(dom = 't'), rownames = FALSE)
    })

    
    observeEvent(input$showModal, {
      toggleModal(session, "myModal", "open")
    })
    
    output$textOutput <- renderText({
      paste("You entered:", input$textInput)
    })
    
    
    
    
    # Filter attribute tables in the Project checklist based on the row selected
    observeEvent(input$table1_rows_selected,{
      if(!is.null(input$table1_rows_selected)){
        v$s <- input$table1_rows_selected
        PD1 <- filteredList()[v$s,]
        PD2 <- filteredList()[v$s,]
        v$w <- PD1$WIN
        v$n <- PD1$`Project Title`
        shinyjs::reset("form")   
        updateRadioButtons(session, "context_agree", selected = character(0))
        updateRadioButtons(session, "srl_review", selected = character(0))
        updateRadioButtons(session, "hcl_review", selected = character(0))
        updateRadioButtons(session, "ka_review", selected = character(0))
        updateRadioButtons(session, "bp_review", selected = character(0))
        updateRadioButtons(session, "speed_review", selected = character(0))
        updateRadioButtons(session, "MPO", selected = character(0))
        updateRadioButtons(session, "RPO", selected = character(0))
        updateRadioButtons(session, "Muni", selected = character(0))

       }
    
        output$projectDetails <- DT::renderDataTable({
          PD1 <- filteredList()[v$s,]
          PD1 <- projectdetails[projectdetails$WIN == PD1$WIN, ]
          PD1 <- pivot_longer(PD1, cols = 1:8, names_to = "Attribute", values_to = " Project Details")
          datatable(PD1, rownames = FALSE, options = list(dom = 't'))
        }) 
        
        output$projectDetails2 <- DT::renderDataTable({
          PD2 <- filteredList()[v$s,]
          PD2 <- projectdetails2[projectdetails2$WIN == PD2$WIN, ]
          PD2 <- pivot_longer(PD2, cols = 1:4, names_to = "Safety Attribute", values_to = "Details")
          datatable(PD2[-1,], rownames = FALSE, options = list(dom = 't'))
        }) 
    })
    
    observe({
      sidebar_toggle(
        id = "sidebar",
        open = is.null(input$table1_rows_selected)
      )
    })
 
  # Map Palettes and Legend Icons  

       
 pal <- colorNumeric(
      palette = colorRampPalette(c("darkgreen", "yellow", "darkorange"))(10),
      domain = NULL)
 
 cc.pal <- colorFactor(palette = c("Rural Town"="#76EE00", "Suburban"="#FF7F50", "Urban"="#CD2626", "Village"="#00B2EE"), 
                       domain = clip_context$context_cl)

 
 cc.pal.leg <- colorFactor(palette = c("Rural"="darkgreen", "Rural Town"="#76EE00", "Suburban"="#FF7F50", "Urban"="#CD2626", "Village"="#00B2EE"), 
                           domain = NULL)
 
 
 
 # set legend features
 colors <- c("blue") #, "white", "blue", "white", "blue", "red")
 labels <- c("Bridge") # , "empty_square", "big_square", "empty_circle", "filled_circle", "big_circle")
 sizes <- c(10) #, 20, 30, 10, 20, 30)
 shapes <- c("circle") #, "square", "square", "circle", "circle", "circle")
 borders <- c("blue") #, "blue", "black", "blue", "blue", "black")
 
 addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, opacity = 0.8){
   
   make_shapes <- function(colors, sizes, borders, shapes) {
     shapes <- gsub("circle", "50%", shapes)
     shapes <- gsub("square", "0%", shapes)
     paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
   }
   make_labels <- function(sizes, labels) {
     paste0("<div style='display: inline-block;height: ", 
            sizes, "px;margin-top: 4px;line-height: ", 
            sizes, "px;'>", labels, "</div>")
   }
   
   legend_colors <- make_shapes(colors, sizes, borders, shapes)
   legend_labels <- make_labels(sizes, labels)
   
   return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity))
 }
 
 

#----  
  # Add map with Crash data layers
    output$safetymap <- renderLeaflet({

        my_map <- leaflet() %>%
        setView(lng = -69.060, lat = 45.259, zoom = 7) %>%
        addProviderTiles(provider = providers$CartoDB.Positron, group = "Light Grey",
                         options = providerTileOptions(minZoom = 6, maxZoom = 17)) %>%
        addProviderTiles(provider = providers$Esri.WorldImagery, group = "Satellite",
                         options = providerTileOptions(minZoom = 6, maxZoom = 17)) %>%
        addProviderTiles(provider = providers$OpenStreetMap, group = "Open Street Map",
                         options = providerTileOptions(minZoom = 6, maxZoom = 17))  %>%
        addLayersControl(position = "topleft", options = layersControlOptions(collapsed = FALSE),
                         baseGroups = c( "Light Grey", "Open Street Map","Satellite"),
                         overlayGroups = c("K+A Crashes","Bike Ped Crashes","High Crash Locations", "Safety Review Locations","Bridges")) %>% #"Speed Limit",
         #hideGroup("Speed Limit") %>%
          addCircleMarkers(data = HCL_n,
                           lng = ~HCL_n$lon,
                           lat = ~HCL_n$lat,
                           fillColor = "magenta",
                           fillOpacity = 0.5,
                           radius = 5,
                           stroke = TRUE,
                           color = "magenta",
                           weight = 3,
                           popup = ~paste0("<b>Total Crashes: </b>", as.character(HCL_n$totalcrash)),
                           group = "High Crash Locations") %>%
          addPolylines(data = HCL_s,
                       color = "magenta",
                       popup = ~paste0("<b>Total Crashes: </b>", as.character(HCL_s$totalcrash)),
                       weight = 5,
                       group = "High Crash Locations"
          )  %>%
          addCircleMarkers(data = RSL_n,
                           lng = ~RSL_n$lon,
                           lat = ~RSL_n$lat,
                           fillColor = "purple",
                           fillOpacity = 0.5,
                           radius = 5,
                           stroke = TRUE,
                           color = "purple",
                           weight = 2,
                           popup = ~paste0("<b>Safety Review Location</b>","<br>","<b> Review Year: </b>", as.character(RSL_n$review_yr)),
                           group = "Safety Review Locations")%>%
          addPolylines(data = RSL_s,
                       color = "purple",
                       weight = 5,
                       popup = ~paste0("<b>Safety Review Location</b>","<br>","<b>Review Year: </b>", as.character(RSL_s$rev_yr)),
                       group = "Safety Review Locations") %>%
          addCircleMarkers(data = crash_10_KA,
                           lng = ~crash_10_KA$lon,
                           lat = ~crash_10_KA$lat,
                           fillColor = "darkgreen",
                           fillOpacity = 0.5,
                           radius = 5,
                           stroke = TRUE,
                           color = "darkgreen",
                           weight = 2,
                           popup = paste0("<b>Accident Date: </b>",crash_10_KA$accident_d, "<br>",
                                          "<b>Type of Crash: </b>", crash_10_KA$type_of_cr, "<br>",
                                          "<b>Type of Location: </b>",  crash_10_KA$type_of_lo, "<br>",
                                          "<b>Injury Type: </b>",crash_10_KA$injury_lev
                                          
                           ),
                           group = "K+A Crashes") %>%
          addCircleMarkers(data = crash_10_BP,
                           lng = ~crash_10_BP$lon,
                           lat = ~crash_10_BP$lat,
                           fillColor = "aquamarine",
                           fillOpacity = 0.5,
                           radius = 5,
                           stroke = TRUE,
                           color = "aquamarine",
                           weight = 2,
                           popup = paste0("<b>Accident Date: </b>",crash_10_BP$accident_d, "<br>",
                                          "<b>Type of Crash: </b>", crash_10_BP$type_of_cr, "<br>",
                                          "<b>Type of Location: </b>", crash_10_BP$type_of_lo,"<br>",
                                          "<b>Injury Type: </b>",crash_10_BP$injury_lev
                           ),
                           group = "Bike Ped Crashes") %>% 
          addCircleMarkers(data = WP_bridge,
                         lng = ~WP_bridge$lon,
                         lat = ~WP_bridge$lat,
                         fillColor = "blue",
                         fillOpacity = 0.7,
                         radius = 8,
                         stroke = TRUE,
                         color = "blue",
                         weight = 2,
                         label = ~WP_bridge$descriptio,
                         labelOptions = labelOptions(noHide = FALSE),
                         popup = ~paste0("Project WIN: ",
                                         as.character(WP_bridge$WIN), "<br>",
                                         "<b> Workplan scope: </b>",
                                         as.character(WP_bridge$resource_a), ", ",
                                         as.character(WP_bridge$scope)
                                          ),
                         group = "Bridges")  %>%
          addLegend(position = "topright", 
                    colors = c("darkgreen", "aquamarine", "purple", "magenta","blue" ), 
                    labels = c("K+A Crash", "Bike Ped  Crash", "Safety Review Location", "High Crash Location", "Bridges"), 
                    title = "Legend")


      my_map

     })
    
  
    # Add map with Context Classification layer
    output$contextmap <- renderLeaflet({
      
      my_map2 <- leaflet() %>% 
        setView(lng = -69.060, lat = 45.259, zoom = 7) %>%
        addProviderTiles(provider = providers$OpenStreetMap, group = "Open Street Map",
                         options = providerTileOptions(minZoom = 6, maxZoom = 17))  %>%
        addProviderTiles(provider = providers$CartoDB.Positron, group = "Light Grey", 
                         options = providerTileOptions(minZoom = 6, maxZoom = 17))%>%
        addProviderTiles(provider = providers$Esri.WorldImagery, group = "Satellite", 
                         options = providerTileOptions(minZoom = 6, maxZoom = 17)) %>%
         addLayersControl(position = "topleft",
                         baseGroups = c("Open Street Map", "Light Grey", "Satellite"),
                         overlayGroups = c("Bridge Project","Roadway Context"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addPolylines(data = diff_speed,
                     color = "darkgreen",
                     weight = 5,
                     opacity = 1,
                     popup = ~paste0("<b>Roadway Context Classification: Rural </b>"),
                     group = "Roadway Context") %>%
        addPolylines(data = clip_context,
                         color = ~cc.pal(clip_context$context_cl),
                         weight = 5,
                         opacity = 1,
                         popup = ~paste0("<b>Roadway Context Classification: </b>",
                                         as.character(context_cl)),
                         group = "Roadway Context") %>%
        addCircleMarkers(data = WP_bridge,
                         lng = ~WP_bridge$lon,
                         lat = ~WP_bridge$lat,
                         fillColor = "blue",
                         fillOpacity = 0.7,
                         radius = 8,
                         stroke = TRUE,
                         color = "blue",
                         weight = 2,
                         label = ~WP_bridge$descriptio,
                         labelOptions = labelOptions(noHide = FALSE),
                         popup = ~paste0("Project WIN: ",
                                         as.character(WP_bridge$WIN), "<br>",
                                         "<b> Workplan scope: </b>",
                                         as.character(WP_bridge$resource_a), ", ",
                                         as.character(WP_bridge$scope)
                                          ),
                         group = "Bridge Project")   %>%
        addLegend(
          position = "topright",
          pal = cc.pal.leg,
          values = c("Rural","Rural Town", "Suburban", "Urban", "Village"), 
          title = "Roadway Context",
          opacity = 1)%>%
        addLegendCustom(colors, labels, sizes, shapes, borders)
        

      my_map2
      
    })   
    
    observe( {
      selected_location <- WP_bridge[WP_bridge$WIN == v$w, ]
      leafletProxy("contextmap") %>%
        setView(lng = selected_location$lon, lat = selected_location$lat, zoom = 14)
      leafletProxy("safetymap") %>%
        setView(lng = selected_location$lon, lat = selected_location$lat, zoom = 14)
    })
    
#----    
    deets <- reactiveVal(data.frame(WIN = character(), `Project Title` = character(), timestamp = character()))
    
    
    humanTime <- function() format(Sys.time(), "%m/%d/%Y %H:%M")
    
    
    # Observe the update button
    observeEvent(input$submit, {
      current_deets <- deets()
      if (nrow(current_deets) > 0) {
        deets(current_deets[-nrow(current_deets), ])
      }
      new_deets <- data.frame(WIN =  v$w, `Project Title` = v$n, Reviewer = input$checklist_person , timestamp = humanTime())
      deets(rbind(deets(), new_deets))
      
    })
  
    # Save checklist responses 

    formData <- reactive({
      data <- lapply(fieldsAll, function(x) input[[x]])
      data <- t(data)
      data
    })

    saveData <- function(data) {
      # The data must be a dataframe rather than a named vector
      data <- data %>% as.list() %>% data.frame()
      data <- cbind(deets(), data)
      
      # Add the data as a new row
      sheet_append("11F4HfNUOtYcFETGcO9y8KnXnIpzWjsApN4O7emppA_Y", data) 
      
    }
    
    # action to take when submit button is pressed
    observeEvent(input$submit, {
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")

      
      tryCatch({
        saveData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")

      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
        shinyjs::enable("submit")
        shinyjs::hide("submit_msg")
      })
    })
    
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      #updateRadioButtons(session, id = "context_agree", selected = character(0))
      shinyjs::hide("thankyou_msg")
    })
    
    # Set reactive value to table filtering
    vv <- reactiveValues()
    vv$s <- NULL 
    vv$w <- NULL
    
    
    loadData <- function() {
      # Read the data
      data <- read_sheet("11F4HfNUOtYcFETGcO9y8KnXnIpzWjsApN4O7emppA_Y") 
      data[] <-lapply(data, as.character)
      data$WIN <- as.character(data$WIN)
      data$timestamp <- as.character(data$timestamp)
      data = data[data$checklist_person == input$var,1:35]
      data
    }
    

    # Adding view saved data button to project table
    
    output$table2 = renderDataTable({
      DT=loadData()
      DT = DT %>% dplyr::select(WIN, `Project Title`)
      DT[["Actions"]]<-
        paste0('
              <div class="btn-group" role="group" aria-label="Basic example">
              <button type="button" class="btn btn-info view" id=view_',1:nrow(DT),'>View Saved</button>
              </div>
              ')
      datatable(DT, selection = "single", rownames = FALSE, escape=F, options = list(dom = 't', pageLength = 35, escape=F))
    })
 
    observeEvent(input$table2_rows_selected,{
      if(!is.null(input$table2_rows_selected)){
        vv$s <- input$table2_rows_selected
        PD2 <- loadData()[vv$s,]
        vv$w <- PD2$WIN
      }

      # Show the previous responses
      output$table3 <- DT::renderDataTable({
        DT3 = loadData()
        DT3 <- DT3 %>% filter(WIN == vv$w)
        DT3 <- pivot_longer(DT3, names_to = "Questions", values_to = "Responses", cols = 1:35)
        datatable( DT3 , rownames = FALSE, escape=F, options = list(dom = 't', pageLength = 35) )
      })
    })

  
    observe({
      sidebar_toggle(
        id = "sidebar",
        open = !is.null(input$submit_another)
      )
    })
  
  }


shinyApp(ui, server)





