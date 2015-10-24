


shinyUI(navbarPage("Bioequivalence v0.3",
                   
                   tabPanel("Data upload",
                            sidebarLayout(
                                sidebarPanel(
                                    fileInput('file1', 'Choose .txt file',
                                              accept=c('text/csv', 
                                                       'text/comma-separated-values,text/plain', 
                                                       '.csv', '.txt')),
                                    helpText("Tab separated data with comma as decimal point")
                                ),
                                mainPanel(
                                    tags$h2("Data upload"),
                                    
                                    "This app computes main results required for
                                    bioequivalence evaluation (standart 2x2x2 
                                    crossover design). Your dataset should contains:",
                                    
                                    tags$br(),
                                    
                                    tags$div(
                                        "subj - randomization number;", 
                                        tags$br(),
                                        "seq - sequence (1 - RT, 2 - TR);",
                                        tags$br(),
                                        " prd - period;",
                                        tags$br(),
                                        "drug - test (T) or reference (R) formulation;",
                                        tags$br(),
                                        "time - time of blood sampling;",
                                        tags$br(),
                                        "conc - concentration of analyte."
                                    ),
                                    
                                    tags$hr(),
                                    
                                    "Demo dataset can be dowloaded",
                                    tags$a("here", 
                                           href="https://dl.dropboxusercontent.com/u/50959048/testdata.txt",
                                           target="_blank"), " (right click - save as...).",
                                    
                                    tags$hr(),
                                    
                                    "Undetectable concentrations for first time
                                    points should be entered as 0;

                                    for last points - as NA (missed values).
                                    "
                                )
                            )
                   ),

                   tabPanel("Source data",
                            dataTableOutput("table.all")
                   ),                   
                                      
                   tabPanel("Plots",
                            sidebarLayout(
                                sidebarPanel(
                                    radioButtons("plotType", 
                                                 "Choose type",
                                                 c("Both"="both", 
                                                   "T"="test",
                                                   "R"="ref",
                                                   "Both (log)"="both_log", 
                                                   "T (log)"="test_log",
                                                   "R (log)"="ref_log")
                                    )
                                ),
                                mainPanel(
                                    plotOutput("plot")
                                )
                            )
                   ),
                   
                   tabPanel("Results",
                            tags$h4("Cmax"),
                            tableOutput("table1"),
                            tags$hr(),
                            
                            tags$h4("AUC(0-t)"),
                            tableOutput("table2"),
                            tags$hr(),
                            
                            tags$h4("90% CI"),
                            tableOutput("table3")
                   ),
                   
                   navbarMenu("...",
                              tabPanel("Manual",
                                       "The most impotant note: current app 
                                       exactly reproduces results were obtained
                                       with WinNonlin software. Mixed models are
                                       realized in nlme package, all plots created
                                       with ggplot2."
                              ),
                              tabPanel("About",
                                       tags$br(),
                                       tags$a("Source on GitHub", 
                                              href="https://github.com/statist-bhfz/bioeq_en",
                                              target="_blank"),
                                       tags$br(),
                                       "(с) Andrey Ogurtsov, 2015",
                                       tags$br(),
                                       "mailto: ogurtsov.a.b@gmail.com",
                                       tags$br(),
                                       tags$a("Blog", 
                                         href="http://biostat-r.blogspot.com",
                                         target="_blank")
                              )
                   )
))

