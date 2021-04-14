library(shiny)

################################################################################
# Server Setup
################################################################################

shinyServer(function(input, output) {

################################################################################
# Tab 1 - My Research
################################################################################
        # Create title for the selected study
        output$research_title <- renderUI({
            HTML(as.character(h4(input$study)))
        })
        
        # This is code that can directly add the description of the studies, but I instead went with images.
        # output$research_experience <- renderUI({
        #     if(input$study == "Introduction: Brian Kissell"){
        #         html <- HTML("")
        #     } 
        #     if(input$study == "Overvaluation and Narcissism"){
        #         html <-HTML(as.character(h4("Data Collection: Gather Relevant and Valuable Data")))
        #     }
        #     
        #     if(input$study == "Construction of the perceived threat to identity scale"){
        #         html <- HTML(as.character(p("A validated measure of perceived threat to one’s identity, which functions across heterogeneous populations, did not yet exist, and thus I constructed, and validated, a new measure. I recruited 573 participants from Amazon Mechanical Turk and had them answer questions related to their perceptions of threat to their identity and the importance of their identities, along with eight additional measures. These were the measures of importance of identity, right-wing authoritarianism, authoritarian parenting preferences, social dominance orientation, conservative political attitudes, the need for closure scale, the cognitive reflection test, and a measure of conspiracy mentality. To validate the new scale, I evaluated the descriptive statistics, non-parametric and parametric (i.e., item response theory) tests, factor analyses, reliability analyses, and the descriptive statistics of the final measure. The result was the ten-item perceived threat to identity scale, which has very good psychometric properties. I then explored the relationships between the perceived threat to identity scale and the other measures and found that each measure was correlated with perceived threat to identity. I also utilized regression and structural equation modeling, and found a strong relationship between three latent variables (i.e., intuitive cognitive processing, identity, and right-wing ideology).")))
        #     }
        #     
        #     if(input$study == "Experimental evaluation of the perceived threat to identity scale"){
        #         html <- HTML(as.character(p("In 2019, I created and validated a new measure of perceived threat to identity. To further validate this measure, I recruited 196 participants from a midwestern university. Participants were presented with a new induction technique (i.e., letters which were designed to be threatening to members of particular groups). After the presentation of the induction materials, participants completed the perceived threat to identity scale, along with measures of importance of identity, conservative political beliefs, conspiracy mentality, need for closure, the cognitive reflection test, and measures of belief in real and fake news. I did not find that the induction technique increased perceived threat to identity. However, I did find a significant interaction for need for closure, in which the religious and non-religious threat conditions led to higher scores for religious participants and lower scores for non-religious participants, as compared to the control group. I also found that religious participants scored significantly higher in importance of identity, need for closure, and conservative political attitudes than non-religious participants. In this project, I thus created a new tool that can be used in research on the topic of threat, and provided an example of how this measure can be used to ensure that researchers are indeed inducing threat with their induction techniques.")))
        #     }      
        #     
        #     html
        # })
        
        # Create timeline visualization
        output$timeline <- renderTimevis({
            timevis(research_projects_df)
        })

        ################################################################################
        # Tab 2 - Data Visualization
        ################################################################################        
        
        # Create forest plot
        output$forest_plot <- renderPlot({
            create_forest_plot(d1, effect = {{input$meta_analysis_selector}}, moderator = {{input$moderator_selector}})
        })
        
        # Create meta-regression plot
        output$meta_regression_plot <- renderPlot({
            create_meta_regression_plot(d1, {{input$moderator_selector}})
        })


################################################################################
# Tab 3 - Natural Language Processing
################################################################################        
        # Select the first text
        first_text <- reactive({
            which_text1(input$text1, input$selected_text1)
        })
        
        # Select the second text
        second_text <- reactive({
            which_text2(input$text2, input$selected_text2)
        })
        
        # Create DTM
        created_dtm_m <- reactive({
            create_dtm_m(text1 = first_text(), text2 = second_text(), ngrams = input$n_gram_number)
        })
            
        # Create Ngram wordplot
        output$word_plot <- renderPlot({
            plot_words(dtm_m = created_dtm_m(), color = "White", title = "")
        })
        
        # Create Comparison plot
        output$comparison_plot <- renderPlot({
            par(mar=rep(0, 4))
            plot.new()
            commonality_comparison_plots(
                text1 = first_text(), 
                name_a = "First Text",
                text2 = second_text(),
                name_b = "Second Text",
                type = "Comparison")
        })
        
        # Create Commonality plot
        output$commonality_plot <- renderPlot({
            par(mar=rep(0, 4))
            plot.new()
            commonality_comparison_plots(
                text1 = first_text(), 
                name_a = "First Text",
                text2 = second_text(),
                name_b = "Second Text",
                type = "Commonality")
        })
        
        # Extract the sentiment data
        sentiment_data <- reactive({
            extract_data_from_both_texts(first_text(), second_text())
        })
        
        # Create plots with the sentiment data
        output$sentiment_plot <- renderPlot({
            prep <- sentiment_data() %>%
                select(docname, care, fairness, loyalty, authority, sanctity, mfd_total, starts_with("nrc_"), polarity) %>%
                pivot_longer(cols = -docname, names_to = "foundation", values_to = "score")
            prep$foundation <- factor(prep$foundation, labels = c("MFD: Authority", "MFD: Care", "MFD: Fairness", "MFD: Loyalty", "MFD: Sanctity", "MFD: Total", "NRC: Anger", "NRC: Anticipation", "NRC: Disgust", "NRC: Fear", "NRC: Joy", "NRC: Negative", "NRC: Positive", "NRC: Sadness", "NRC Surprise", "NRC: Trust", "AFINN: Polarity"))
            prep %>% ggplot(aes(x = docname, y = score, fill = foundation)) +
                geom_col(position = "dodge") +
                labs(title = "Sentiment and Moral Foundation Scores", x = "Text", y = "Score")+
                scale_fill_discrete(name = "Sentiment/Foundation", labels = c("MFD: Authority", "MFD: Care", "MFD: Fairness", "MFD: Loyalty", "MFD: Sanctity", "MFD: Total", "NRC: Anger", "NRC: Anticipation", "NRC: Disgust", "NRC: Fear", "NRC: Joy", "NRC: Negative", "NRC: Positive", "NRC: Sadness", "NRC Surprise", "NRC: Trust", "AFINN: Polarity")) +
                scale_x_discrete(name = "Text", labels = c("Text 1", "Text 2"))+
                facet_wrap(~foundation, scales = "free", nrow = 3) +    
                dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
                theme(plot.title = element_text(family = "Fira Sans Condensed"),
                      plot.background = element_rect(fill = "grey10"),
                      panel.background = element_blank(),
                      panel.grid.major = element_line(color = "grey30", size = 0.2),
                      panel.grid.minor = element_line(color = "grey30", size = 0.2),
                      legend.background = element_blank(),
                      axis.ticks = element_blank(),
                      legend.key = element_blank(),
                      legend.position = "bottom")
        })
        
        # Create a tf_idf plot
        output$tf_idf_plot <- renderPlot({
            get_keywords(first_text(), second_text())
        })
        ################################################################################
        # Tab 4 - Additional skills
        ################################################################################
        

        
})
