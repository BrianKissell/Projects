library(shiny)

################################################################################
# User Interface Setup
################################################################################

shinyUI(
  ui <- fluidPage(
    # Set up shiny theme
    theme = shinytheme("cyborg"),
    # Use custom css code
    tags$head(tags$style(type = 'text/css',".shiny-input-panel{background-color: black}")),
    tags$head(
      tags$style(
        HTML('#sidebar {background-color: #fcf9f5;}
        
        body, label, input, button, select {font-family: "Arial";}
        
        #timeline .vis-timeline {border: 2px solid black;font-size: 12pt;background: #ffffff;}
                 
        #timeline .vis-timeline {border: 2px solid black;font-size: 12pt; background: #fcf4e6;}
        
        #timeline .vis-item {
          border-color: #ebcb73;
          background-color: orange;
          font-size: 10pt;
          color: black;
          box-shadow: 5px 5px 20px rgba(128,128,128, 0.5);
          }
        
        #timeline .vis-item,

        #timeline .vis-item.vis-line {border-width: 3px;}
        
        #timeline .vis-item.vis-dot {
          border-width: 10px;
          border-radius: 10px;
          }
        
        #timeline .vis-item.vis-selected {
          border-color: green;
          background-color: lightgreen;
          }
        
        #timeline .vis-background .vis-minor.vis-odd {background: #ffd375;}
        
        #timeline .vis-time-axis .vis-text {
          color: black;
          padding-top: 10px;
          padding-left: 10px;
          }
        
        #timeline .vis-time-axis .vis-grid.vis-minor {
          border-width: 2px;
          border-color: orange;
          }

        #timeline .vis-time-axis .vis-grid.vis-major {
          border-width: 2px;
          border-color: #f2c063;
          }'
             )
        )
      ),
    
    # Create the title for the application
    titlePanel("Brian Kissell: Portfolio"),
    
    # Allow the application to have tabs
    tabsetPanel(
            
      ##########################################################################
      # Tab 1 - My Research
      ##########################################################################
            
      # For the first tab, include general research details
      tabPanel(
        "Research Experience",
        sidebarLayout(
          sidebarPanel(
            # Provide a photo of me
            img(src = "photo_of_brian_kissell_sqr.jpg", width = "100%"),
            # Add option to select different studies
            selectInput(
              "study",
              "Read more about selected projects",
              c("Introduction: Brian Kissell",
                "A Multi-Site Preregistered Paradigmatic Test of the Ego Depletion Effect",
                "Collection and Analysis of SBIR Awards Data",
                "Construction of the Perceived Threat to Identity Scale",
                "Experimental Evaluation of the Perceived Threat to Identity Scale",
                "The Influence of Overvaluation on Narcissism",
                "Has Psychology Always Been so Ideologically Biased? A Historical Argument for Ideological Diversity"
                )
              )
            ),
          
          # Include details on some selected studies
          mainPanel(
            conditionalPanel(
              condition  = "input.study == 'Introduction: Brian Kissell'",
              img(src = "intro_image.png",width = "100%"),
              ),
            conditionalPanel(
              condition  = "input.study == 'Construction of the Perceived Threat to Identity Scale'",
              img(src = "construction_of_the_perceived_threat_to_identity_scale.png", width = "100%"),
              ),
            conditionalPanel(
              condition  = "input.study == 'A Multi-Site Preregistered Paradigmatic Test of the Ego Depletion Effect'",
              img(src = "a_multi_site_preregistered_paradigmatic_test_of_the_ego_depletion_effect.png", width = "100%"),
              ),
            conditionalPanel(
              condition  = "input.study == 'Collection and Analysis of SBIR Awards Data'",
              img(src = "collection_and_analysis_of_sbir_awards_data.png", width = "100%"),
              ),
            conditionalPanel(
              condition  = "input.study == 'Experimental Evaluation of the Perceived Threat to Identity Scale'",
              img(src = "experimental_evaluation_of_the_perceived_threat_to_identity_scale.png", width = "100%"),
              ),
            conditionalPanel(
              condition  = "input.study == 'Has Psychology Always Been so Ideologically Biased? A Historical Argument for Ideological Diversity'",
              img(src = "has_psychology_always_been_so_ideologically_biased.png", width = "100%"),
              ),
            conditionalPanel(
              condition  = "input.study == 'The Influence of Overvaluation on Narcissism'",
              img(src = "the_influence_of_overvaluation_on_narcissism.png", width = "100%"),
              ),
            )
          ),
        # Create a timeline for my research
        h6("Timeline for Brian Kissell's Projects: Hover Over Item to See the Title"),
        fluidRow(id="sidebar", timevisOutput("timeline"))
      ),
            
      ################################################################################
      # Tab 2 - Data Visualization
      ################################################################################

      # Create a tab for data visualization
      tabPanel(
        "Data Visualization",
        img(src = "data_visualization.png", width = "100%"),
        h4("Example: Meta-Analysis"),
        sidebarLayout(
          # Add option to select between biased assimilation and polarization
          sidebarPanel(
            selectInput(
              "meta_analysis_selector",
              "Select between biased assimilation and polarization",
              c("Biased Assimilation" = "g_biased_assimilation",  "Polarization"= "g_polarization")
              ),
            # Add option to select between moderators
            selectInput(
              "moderator_selector",
              "Choose a specific moderator variable",
              c("Topic" = "topic", "Sample Selection" = "sample_selection", "Composite Measure" = "is_composite", "Extremity of Belief" = "extremity_level_of_sample")
              ),
            plotOutput("meta_regression_plot", height = "240px")
            ),
          mainPanel(
            # Add Forest Plot
            plotOutput("forest_plot", height = "500px")
            )
        ),
        verticalLayout(
          # Add p-curve images
          h6("P-Curve for Biased Assimilation and Attitude Polarization"),
          splitLayout(
            img(src = "p_curve_biased_assimilation.jpg", width = "100%"),
            img(src = "p_curve_polarization.jpg", width = "100%")
          )
        ),
        
        # Add Covid-19 Gifs
        h3("Animated Visualizations: Four Time Series Maps on the Progression of Covid-19"),
        splitLayout(
          img(src = "daily_deaths.gif", width = "100%"),
          img(src = "daily_cases.gif", width = "100%")
          ),
        splitLayout(
          img(src = "daily_death_percentage.gif", width = "100%"),
        img(src = "daily_cases_percentage.gif", width = "100%")
        )
        ),
            
      ################################################################################
      # Tab 3 - Natural Language Processing
      ################################################################################

      # Create a tab for natural language processing
      tabPanel(
        "Natural Language Processing",
        img(src = "natural_language_processing.png", width = "100%"),
        h4("Example: Exploratory Application"),
        sidebarLayout(
          # Add option to customize text
          sidebarPanel(
            textInput("text1", "Enter the first text", ""),
            textInput("text2", "Enter the second text", ""),
            p("or"),
            # Add text from the Moore Lab website
            selectInput(
              "selected_text1", 
              "Select First Text", 
              c("Moore Lab - Eye-Tracking" = "eye_tracking",
                "Moore Lab - Galvanic Skin Response" = "galvanic_skin_response",
                "Moore Lab - Electroencephalography" = "electroencephalography",
                "Moore Lab - Facial Analysis" = "facial_analysis",
                "Moore Lab - Implicit Response Test" =  "implicit_response_test",
                "Moore Lab - Respondent Interview" = "respondent_interview"
                )
              ),
            selectInput(
              "selected_text2",
              "Select Second Text",
              c(
                "Moore Lab - Galvanic Skin Response" = "galvanic_skin_response",
                "Moore Lab - Electroencephalography" = "electroencephalography",
                "Moore Lab - Facial Analysis" = "facial_analysis",
                "Moore Lab - Implicit Response Test" =  "implicit_response_test",
                "Moore Lab - Respondent Interview" = "respondent_interview",
                "Moore Lab - Eye-Tracking" = "eye_tracking"
                )
              ),
            # Add option to change the number of N-grams
            selectInput(
              "n_gram_number", 
              "Please select the number of Ngrams you want included",
              c(1,2,3)
              )
            ),
          
          mainPanel(
            #Include the plots
            splitLayout(
              verticalLayout(
                h6("Comparison Plot"),
                plotOutput("comparison_plot", height = "250px", width = "100%")
                ),
              verticalLayout(
                h6("Commonality Plot"),
                plotOutput("commonality_plot", height = "250px", width = "100%")
                )
              ),
            splitLayout(
              verticalLayout(
                h6("TF-IDF Plot"),
                plotOutput("tf_idf_plot", height = "250px")
                ),
              verticalLayout(
                h6("Top N-Grams"),
                plotOutput("word_plot", height = "250px")
                )
              )
            )
          ),
        plotOutput("sentiment_plot")
        ),

      ################################################################################
      # Tab 4 - Data Collection, Data Scraping, and Psychometrics
      ################################################################################
      
      tabPanel(
        "Additional Skills",
        sidebarLayout(
          sidebarPanel(
            # Add option to select a skill
            selectInput(
              "skills_selector",
              "Find more details on specific skills",
              c("Data Collection",
                "Data Wrangling",
                "Psychometrics and Statistics")
                )
              ),
          mainPanel(

            )
          ),
        # Add image depending on which is selected
        conditionalPanel(
          condition  = "input.skills_selector == 'Data Collection'",
          img(src = "data_collection.png", width = "100%"),
        ),
        conditionalPanel(
          condition  = "input.skills_selector == 'Data Wrangling'",
          img(src = "data_wrangling.png", width = "100%"),
        ),
        conditionalPanel(
          condition  = "input.skills_selector == 'Psychometrics and Statistics'",
          img(src = "psychometrics_and_applied_statistics.png", width = "100%"),
        )
      )
    )
  )
)
