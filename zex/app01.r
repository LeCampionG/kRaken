library(shinythemes)
library(shiny)
library(knitr)
library(visNetwork)

options(shiny.maxRequestSize = 30*1024^2)

ui<-fluidPage(theme = shinytheme("yeti"),
              navbarPage("Coiff'R - Grégoire Le Campion",
                         tabPanel("A propos",
                                  #code Html
                                  h2(strong("Bienvenue sur l'application Penn boultouz !!!")),
                                  p(strong("Penn boultouz"),"vous permet d'explorer, réaliser et visualiser des réseaux."),
                                  p("Si vous ne savez pas qui sont Pearson, Spearman et Kendall, ou que vous souhaitez un bref rappel sur le calcul des corrélations et ce qu'il implique, avant d'utiliser cette application vous pouvez lire le tutoriel sur la réalisation de corrélations qui est hébergé sur la plateforme du pôle ARD :", strong(a("OUVRIR", href="http://ouvrir.passages.cnrs.fr/wp-content/uploads/2019/04/rapp_correlation.html")),"."),
                                  br(),
                                  h3(strong("Citation")), 
                                  p("Dans l'éventualité où vous utiliseriez cette application dans le cadre d'une publication, vous pouvez citer cet outil comme ceci :"),
                                  p(strong("Le Campion G. ", a("Coiff'R: un outil pour produire, visualiser, explorer et analyser des réseaux.", href="https://analytics.huma-num.fr/Gregoire.LeCampion/Mon_nom_est_Pearson/")," Pôle ARD UMR 5319 UMR Passages. 2019."))
                                  
                         ),
                         
                         tabPanel("Import Table des liens",
                                  sidebarLayout(
                                    ############################################################
                                    # 1. Le menu de gauche, présentant les options de l'import #
                                    ############################################################
                                    sidebarPanel(
                                      h5("Exemple de table de liens"),
                                      downloadButton("downloadexliens", "Télécharger exemple Grey's Anatomy"),
                                      downloadButton("downloadex2liens", "Télécharger exemple Game of Thrones"),
                                      tags$hr(),
                                      fileInput("file1", "Charger un fichier CSV",
                                                multiple = FALSE,
                                                accept = c("text/csv",
                                                           "text/comma-separated-values,text/plain", "text/semicolon-separated-values,text/plain" ,
                                                           ".csv")),
                                      h5(helpText("Le poid des fichier est limité à 30Mb")),
                                      tags$hr(),
                                      h5(helpText("Ajuster les options suivantes en fonction de votre fichier importé")),
                                      # Pour déterminer si la première ligne correspond aux en-tête de colonne
                                      checkboxInput("header", "1ere ligne comme en-tête", TRUE),
                                      #déterminer le séparateur de champ
                                      radioButtons("sep", "Séparateur de champ",
                                                   choices = c(Comma = ",",
                                                               Semicolon = ";",
                                                               Tab = "\t"),
                                                   selected = ","),
                                      #déterminer séparateur de texte
                                      radioButtons("quote", "Séparateur de texte",
                                                   choices = c("Aucun" = "",
                                                               "Guillemet double" = '"',
                                                               "Guillemet simple" = "'"),
                                                   selected = '"'),
                                      #Choix du mode de visualistaion
                                      radioButtons("disp", "Visualiser",
                                                   choices = c("Uniquement les 1eres lignes" = "head",
                                                               "Ensemble des données"= "all"),
                                                   selected = "head")
                                    ),
                                    #########################################################
                                    # 2. Panneau principal pour visualiser données chargées #
                                    #########################################################
                                    mainPanel(tabsetPanel(
                                      tabPanel("Liens",
                                               uiOutput("tb1")
                                      )
                                    )
                                    ))),
                         tabPanel("Import Table des Noeuds",
                                  sidebarLayout(
                                    ############################################################
                                    # 1. Le menu de gauche, présentant les options de l'import #
                                    ############################################################
                                    sidebarPanel(
                                      h5("Exemple de table de noeuds"),
                                      downloadButton("downloadexnoeuds", "Télécharger exemple Grey's Anatomy"),
                                      downloadButton("downloadex2noeuds", "Télécharger exemple Game of Thrones"),
                                      tags$hr(),
                                      fileInput("file2", "Charger un fichier CSV",
                                                multiple = FALSE,
                                                accept = c("text/csv",
                                                           "text/comma-separated-values,text/plain", "text/semicolon-separated-values,text/plain" ,
                                                           ".csv")),
                                      h5(helpText("Le poid des fichier est limité à 30Mb")),
                                      tags$hr(),
                                      h5(helpText("Ajuster les options suivantes en fonction de votre fichier importé")),
                                      # Pour déterminer si la première ligne correspond aux en-tête de colonne
                                      checkboxInput("header2", "1ere ligne comme en-tête", TRUE),
                                      #déterminer le séparateur de champ
                                      radioButtons("sep2", "Séparateur de champ",
                                                   choices = c(Comma = ",",
                                                               Semicolon = ";",
                                                               Tab = "\t"),
                                                   selected = ","),
                                      #déterminer séparateur de texte
                                      radioButtons("quote2", "Séparateur de texte",
                                                   choices = c("Aucun" = "",
                                                               "Guillemet double" = '"',
                                                               "Guillemet simple" = "'"),
                                                   selected = '"'),
                                      #Choix du mode de visualistaion
                                      radioButtons("disp2", "Visualiser",
                                                   choices = c("Uniquement les 1eres lignes" = "head",
                                                               "Ensemble des données"= "all"),
                                                   selected = "head")
                                    ),
                                    #########################################################
                                    # 2. Panneau principal pour visualiser données chargées #
                                    #########################################################
                                    mainPanel(tabsetPanel(
                                      tabPanel("Noeuds",
                                               uiOutput("tb2")
                                      )
                                    )
                                    ))),
                         tabPanel("Explo",
                                  mainPanel(visNetworkOutput("visnet"))),
                         tabPanel("Réseau",
                                  sidebarLayout(
                                    #############################################################
                                    # 3. Le menu de gauche, présentant les options de l'analyse #
                                    #############################################################
                                    sidebarPanel(# 1.2. Choix de la  variable à prédire :
                                      # 1.3. Paramètres de l'analyse :
                                      h5("Paramètres réseau"),
                                      radioButtons("directed", label="Votre réseau est-il dirigé ?", choices=list("Oui"="TRUE","Non"="FALSE"), selected="FALSE"),
                                      selectizeInput('select_layout', "Algorithm de spacialisation du réseau", choices = c("nicely","stress", "fr", "kk", "drl", "lgl","tree", "sugiyama", "star", "circle", "dh", "gem", "graphopt", "grid", "mds", "sphere", "randomly"), multiple=FALSE, selected="nicely"),
                                      radioButtons("advance", " Paramètres avancés", choices = c("Non", "Oui"), selected = "Oui"),
                                      conditionalPanel(condition="input.advance == 'Oui'",
                                                       selectizeInput('posleg', "Position de la légende", choices = c("En-bas"="bottom", "A droite"="right", "A gauche"="left", "Au-dessus"="top", "Pas de légende"="none"), multiple=FALSE, selected="bottom"),
                                                       tags$hr(),
                                                       h5("Paramètres noeuds"),
                                                       selectizeInput("typecoul", label="Définition couleur des noeuds", choices=list("Attributs noeuds","Type de noeuds", "Communautés réseaux"), selected=""),
                                                       conditionalPanel(condition="input.typecoul == 'Attributs noeuds'",
                                                                        uiOutput("select_coul"),
                                                                        selectizeInput('select_palette', "Quelle palette de couleur", choices = c("Set3","Paired","Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG"), multiple=FALSE, selected="Set3")),
                                                       conditionalPanel(condition="input.typecoul == 'Communautés réseaux'",
                                                                        selectizeInput("comm", label="Définition de l'algorithm de création de communauté", choices=list("Louvain"="cluster_louvain", "Walktrap"="cluster_walktrap", "Spinglass"="cluster_spinglass", "Leading eigen"="cluster_leading_eigen", "Edge betweenness"="cluster_edge_betweenness", "Fast greedy"="cluster_fast_greedy", "Label prop"="cluster_label_prop"), selected="")),
                                                       conditionalPanel(condition="input.typecoul == 'Type de noeuds'",
                                                                        selectizeInput("typenode", label="Type de noeuds", choices=list("node_is_center", "node_is_cut", "node_is_sink", "node_is_source", "node_is_isolated", "node_is_universal", "node_is_simplical", "node_is_adjacent"), selected="")),
                                                       uiOutput("select_shape"),
                                                       selectizeInput("typesize", label="Définition taille des noeuds", choices=list("Taille fixe", "Attributs noeuds", "Indicateurs analyse de réseaux"), selected=""),
                                                       #conditionalPanel(condition="input.typesize == 'Taille fixe'",
                                                       #sliderInput("size1", label="Taille des noeuds", value=1, min=1, max=50, step=1)),
                                                       conditionalPanel(condition="input.typesize == 'Attributs noeuds'",
                                                                        uiOutput("select_size2"),
                                                                        sliderInput("slidernode1", "Intervalle taille des noeuds",min = 1 , max = 50, value = c(3, 8))),
                                                       conditionalPanel(condition="input.typesize == 'Indicateurs analyse de réseaux'",
                                                                        selectizeInput("size3", label="Définition indicateur pour la taille des noeuds", choices=list("Degré"="centrality_degree", "Betweenness"="centrality_betweenness", "Closeness"="centrality_closeness"), selected=""),
                                                                        sliderInput("slidernode2", "Intervalle taille des noeuds",min = 1 , max = 50, value = c(3, 8))),
                                                       tags$hr(),
                                                       h5("Paramètres Labels"),
                                                       selectizeInput("typefont", label="Police des labels", choices=list("Times", "Helvetica", "Courier", "AvantGarde", "Palatino", "Bookman"), selected="Times"),
                                                       selectizeInput("fontface", label="caractéristique police", choices=list("plain", "bold", "italic", "bold.italic"), selected="plain"),
                                                       selectizeInput("typefontsize", label="Filtrer les labels", choices=list("Pas de filtre", "Attributs noeuds", "Indicateurs analyse de réseaux"), selected=""),
                                                       #conditionalPanel(condition="input.typesize == 'Taille fixe'",
                                                       #sliderInput("size1", label="Taille des noeuds", value=1, min=1, max=50, step=1)),
                                                       conditionalPanel(condition="input.typefontsize == 'Attributs noeuds'",
                                                                        uiOutput("select_fontsize2"),
                                                                        sliderInput("sliderlabel1", "Valeur filtre",min = 1 , max = 100, value = 1, step=1)),
                                                       conditionalPanel(condition="input.typefontsize == 'Indicateurs analyse de réseaux'",
                                                                        selectizeInput("fontsize3", label="Définition indicateur pour filtrer les labels", choices=list("Degré"="centrality_degree", "Betweenness"="centrality_betweenness", "Closeness"="centrality_closeness"), selected=""),
                                                                        sliderInput("sliderlabel2", "Valeur filtre",min = 0 , max = 100, value = 0, step=1)),
                                                       tags$hr(),
                                                       h5("Paramètres Liens"),
                                                       selectizeInput("sensedge", label="Indiquer le sens du lien", choices=list("Ne pas indiquer", "Indiquer"), selected="Ne pas indiquer"),
                                                       uiOutput("select_sizeedge"),
                                                       sliderInput("slideredge", "Intervalle épaisseur liens",min = 0.1 , max = 10, value = c(0.2, 3), step=0.1))
                                    ),
                                    
                                    ############################################################################
                                    # 4. Le panneau principal, pour l'affichage des résultats Arbre de décision#
                                    ############################################################################
                                    mainPanel(
                                      tabPanel("Réseau",
                                               plotOutput("Reseau", height="900px"),
                                               radioButtons(
                                                 inputId = "filetype_reseau",
                                                 label = "Quel format d'image :",
                                                 inline = TRUE,
                                                 choices = list("PDF", "PNG","SVG")),
                                               downloadButton(outputId = "downloadreseau", label = "Télécharger Reseau")
                                      )
                                    )
                                  )
                         ),
                         tabPanel("Export noeuds",
                                  mainPanel(DT::dataTableOutput("table_noeuds"))),
                         
                         tabPanel("Ego-Réseau",
                                  sidebarLayout(
                                    #############################################################
                                    # 3. Le menu de gauche, présentant les options de l'analyse #
                                    #############################################################
                                    sidebarPanel(# 1.2. Choix de la  variable à prédire :
                                      # 1.3. Paramètres de l'analyse :
                                      h5("Paramètres réseau"),
                                      uiOutput("select_name"),
                                      selectizeInput('egoselect_layout', "Algorithm de spacialisation du réseau", choices = c("nicely","stress", "fr", "kk", "drl", "lgl","tree", "sugiyama", "star", "circle", "dh", "gem", "graphopt", "grid", "mds", "sphere", "randomly"), multiple=FALSE, selected="nicely"),
                                      selectizeInput('egoposleg', "Position de la légende", choices = c("En-bas"="bottom", "A droite"="right", "A gauche"="left", "Au-dessus"="top", "Pas de légende"="none"), multiple=FALSE, selected="bottom"),
                                      tags$hr(),
                                      h5("Paramètres noeuds"),
                                      selectizeInput("egotypecoul", label="Définition couleur des noeuds", choices=list("Attributs noeuds", "Communautés réseaux"), selected=""),
                                      conditionalPanel(condition="input.egotypecoul == 'Attributs noeuds'",
                                                       uiOutput("egoselect_coul")),
                                      conditionalPanel(condition="input.egotypecoul == 'Communautés réseaux'",
                                                       selectizeInput("egocomm", label="Définition de l'algorithm de création de communauté", choices=list("Louvain"="cluster_louvain", "Walktrap"="cluster_walktrap", "Spinglass"="cluster_spinglass", "Leading eigen"="cluster_leading_eigen", "Edge betweenness"="cluster_edge_betweenness", "Fast greedy"="cluster_fast_greedy", "Label prop"="cluster_label_prop"), selected="")),
                                      selectizeInput('egoselect_palette', "Quelle palette de couleur", choices = c("Set3","Paired","Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG"), multiple=FALSE, selected="Set3"),
                                      uiOutput("egoselect_shape"),
                                      selectizeInput("egotypesize", label="Définition taille des noeuds", choices=list("Taille fixe", "Indicateurs analyse de réseaux"), selected="Taille fixe"),
                                      conditionalPanel(condition="input.egotypesize == 'Indicateurs analyse de réseaux'",
                                                       selectizeInput("egosize3", label="Définition indicateur pour la taille des noeuds", choices=list("Degré"="centrality_degree", "Betweenness"="centrality_betweenness", "Closeness"="centrality_closeness"), selected=""),
                                                       sliderInput("egoslidernode2", "Intervalle taille des noeuds",min = 1 , max = 50, value = c(3, 8))),
                                      tags$hr(),
                                      h5("Paramètres Labels"),
                                      selectizeInput("egotypefont", label="Police des labels", choices=list("Times", "Helvetica", "Courier", "AvantGarde", "Palatino", "Bookman"), selected="Times"),
                                      selectizeInput("egofontface", label="caractéristique police", choices=list("plain", "bold", "italic", "bold.italic"), selected="plain"),
                                      selectizeInput("egotypefontsize", label="Filtrer les labels", choices=list("Pas de filtre", "Indicateurs analyse de réseaux"), selected="Pas de filtre"),
                                      conditionalPanel(condition="input.egotypefontsize == 'Indicateurs analyse de réseaux'",
                                                       selectizeInput("egofontsize3", label="Définition indicateur pour filtrer les labels", choices=list("Degré"="centrality_degree", "Betweenness"="centrality_betweenness", "Closeness"="centrality_closeness"), selected=""),
                                                       sliderInput("egosliderlabel2", "Valeur filtre",min = 0 , max = 100, value = 0, step=1)),
                                      tags$hr(),
                                      h5("Paramètres Liens"),
                                      selectizeInput("egosensedge", label="Indiquer le sens du lien", choices=list("Ne pas indiquer", "Indiquer"), selected="Ne pas indiquer"),
                                      uiOutput("egoselect_sizeedge"),
                                      sliderInput("egoslideredge", "Intervalle épaisseur liens",min = 0.1 , max = 10, value = c(0.2, 3), step=0.1)
                                    ),
                                    
                                    ############################################################################
                                    # 4. Le panneau principal, pour l'affichage des résultats Arbre de décision#
                                    ############################################################################
                                    mainPanel(
                                      tabPanel("Ego-Réseau",
                                               plotOutput("egoreseau", height="900px"),
                                               radioButtons(
                                                 inputId = "filetype_egoreseau",
                                                 label = "Quel format d'image :",
                                                 inline = TRUE,
                                                 choices = list("PDF", "PNG","SVG")),
                                               downloadButton(outputId = "downloadegoreseau", label = "Télécharger Ego-Reseau")
                                      )
                                    )
                                  )
                         ),
                         tabPanel("Sous-Réseau",
                                  sidebarLayout(
                                    #############################################################
                                    # 3. Le menu de gauche, présentant les options de l'analyse #
                                    #############################################################
                                    sidebarPanel(# 1.2. Choix de la  variable à prédire :
                                      # 1.3. Paramètres de l'analyse :
                                      h5("Paramètres réseau"),
                                      uiOutput("select_varsc"),
                                      uiOutput("select_modsc"),
                                      tags$hr(),
                                      selectizeInput('sousselect_layout', "Algorithm de spacialisation du réseau", choices = c("nicely","stress", "fr", "kk", "drl", "lgl","tree", "sugiyama", "star", "circle", "dh", "gem", "graphopt", "grid", "mds", "sphere", "randomly"), multiple=FALSE, selected="nicely"),
                                      selectizeInput('sousposleg', "Position de la légende", choices = c("En-bas"="bottom", "A droite"="right", "A gauche"="left", "Au-dessus"="top", "Pas de légende"="none"), multiple=FALSE, selected="bottom"),
                                      h5("Paramètres noeuds"),
                                      selectizeInput("soustypecoul", label="Définition couleur des noeuds", choices=list("Attributs noeuds", "Communautés réseaux"), selected=""),
                                      conditionalPanel(condition="input.soustypecoul == 'Attributs noeuds'",
                                                       uiOutput("sousselect_coul")),
                                      conditionalPanel(condition="input.soustypecoul == 'Communautés réseaux'",
                                                       selectizeInput("souscomm", label="Définition de l'algorithm de création de communauté", choices=list("Louvain"="cluster_louvain", "Walktrap"="cluster_walktrap", "Spinglass"="cluster_spinglass", "Leading eigen"="cluster_leading_eigen", "Edge betweenness"="cluster_edge_betweenness", "Fast greedy"="cluster_fast_greedy", "Label prop"="cluster_label_prop"), selected="")),
                                      selectizeInput('sousselect_palette', "Quelle palette de couleur", choices = c("Set3","Paired","Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG"), multiple=FALSE, selected="Set3"),
                                      uiOutput("sousselect_shape"),
                                      selectizeInput("soustypesize", label="Définition taille des noeuds", choices=list("Taille fixe", "Indicateurs analyse de réseaux"), selected="Taille fixe"),
                                      conditionalPanel(condition="input.soustypesize == 'Indicateurs analyse de réseaux'",
                                                       selectizeInput("soussize3", label="Définition indicateur pour la taille des noeuds", choices=list("Degré"="centrality_degree", "Betweenness"="centrality_betweenness", "Closeness"="centrality_closeness"), selected=""),
                                                       sliderInput("sousslidernode2", "Intervalle taille des noeuds",min = 1 , max = 50, value = c(3, 8))),
                                      tags$hr(),
                                      h5("Paramètres Labels"),
                                      selectizeInput("soustypefont", label="Police des labels", choices=list("Times", "Helvetica", "Courier", "AvantGarde", "Palatino", "Bookman"), selected="Times"),
                                      selectizeInput("sousfontface", label="caractéristique police", choices=list("plain", "bold", "italic", "bold.italic"), selected="plain"),
                                      selectizeInput("soustypefontsize", label="Filtrer les labels", choices=list("Pas de filtre", "Indicateurs analyse de réseaux"), selected="Pas de filtre"),
                                      conditionalPanel(condition="input.soustypefontsize == 'Indicateurs analyse de réseaux'",
                                                       selectizeInput("sousfontsize3", label="Définition indicateur pour filtrer les labels", choices=list("Degré"="centrality_degree", "Betweenness"="centrality_betweenness", "Closeness"="centrality_closeness"), selected=""),
                                                       sliderInput("soussliderlabel2", "Valeur filtre",min = 0 , max = 100, value = 0, step=1)),
                                      tags$hr(),
                                      h5("Paramètres Liens"),
                                      selectizeInput("soussensedge", label="Indiquer le sens du lien", choices=list("Ne pas indiquer", "Indiquer"), selected="Ne pas indiquer"),
                                      uiOutput("sousselect_sizeedge"),
                                      sliderInput("sousslideredge", "Intervalle épaisseur liens",min = 0.1 , max = 10, value = c(0.2, 3), step=0.1)
                                      ),
                                    
                                    ############################################################################
                                    # 4. Le panneau principal, pour l'affichage des résultats Arbre de décision#
                                    ############################################################################
                                    mainPanel(
                                      tabPanel("Sous réseau",
                                               plotOutput("sousreseau", height="900px"),
                                               radioButtons(
                                                 inputId = "filetype_sousreseau",
                                                 label = "Quel format d'image :",
                                                 inline = TRUE,
                                                 choices = list("PDF", "PNG","SVG")),
                                               downloadButton(outputId = "downloadsousreseau", label = "Télécharger Sous-Reseau")
                                      )
                                    )
                                  )
                         )#,
                         #tabPanel("Mumuse",
                        #          mainPanel(visNetworkOutput("Mumuse")))
              )
)



server <- function(input, output, session) {
  
  
  library(shiny)
  library(shinythemes)
  library(igraph)
  library(ggraph)
  library(tidygraph)
  library(influenceR)
  library(RColorBrewer)
  library(visNetwork)
  library(dplyr)
  library(readr)
  library(DT)
  library(tibble)
  
  
  Greys_anatomy_Attributes <- read_csv("~/zPublish/shiny/Reseau/data/Greys_anatomy_Attributes.csv")
  Greys_anatomy_Sexual_relation <- read_csv("~/zPublish/shiny/Reseau/data/Greys_anatomy_Sexual_relation.csv")
  node_got <- read_csv("~/zPublish/shiny/Reseau/data/node_got.csv")
  edge_got <- read_csv("~/zPublish/shiny/Reseau/data/edge_got.csv")
  
  
  
  output$downloadexliens <- downloadHandler(
    filename = function() {
      paste("Greys_anatomy_Sexual_relation", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(Greys_anatomy_Sexual_relation, file, row.names = FALSE)
    }
  )
  
  output$downloadexnoeuds <- downloadHandler(
    filename = function() {
      paste("Greys_anatomy_Attributes", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(Greys_anatomy_Attributes, file, row.names = FALSE)
    }
  )
  
  output$downloadex2liens <- downloadHandler(
    filename = function() {
      paste("edge_got", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(edge_got, file, row.names = FALSE)
    }
  )
  
  output$downloadex2noeuds <- downloadHandler(
    filename = function() {
      paste("node_got", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(node_got, file, row.names = FALSE)
    }
  )
  
  
  #####################################################
  # 5. Charger les données importées et les visualiser#
  #####################################################
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)
    
  })  
  output$table <- renderTable({
    req(input$file1)
    
    # Si le séparateur de champs est un point-virgule,
    # Avoir une virgule comme séparateur causera une erreur dans la fonction read.csv
    df <- reactive({ read.csv(input$file1$datapath,
                              header = input$header,
                              sep = input$sep,
                              quote = input$quote)
    })
    
    if(input$disp == "head") {
      return(head(df()))
    }
    else {
      return(df())
    }
  })
  
  output$tb1 <- renderUI({
    tableOutput("table")
  })
  
  #####################################################
  # 6. Charger les données importées et les visualiser#
  #####################################################
  data2 <- reactive({
    file2 <- input$file2
    if(is.null(file2)){return()} 
    read.csv(input$file2$datapath,
             header = input$header2,
             sep = input$sep2,
             quote = input$quote2)
    
  })  
  output$table2 <- renderTable({
    req(input$file2)
    
    # Si le séparateur de champs est un point-virgule,
    # Avoir une virgule comme séparateur causera une erreur dans la fonction read.csv
    df2 <- reactive({ read.csv(input$file2$datapath,
                               header = input$header2,
                               sep = input$sep2,
                               quote = input$quote2)
    })
    
    if(input$disp2 == "head") {
      return(head(df2()))
    }
    else {
      return(df2())
    }
  })
  
  output$tb2 <- renderUI({
    tableOutput("table2")
  })
  
  
  
  ################################################################
  # 6. Charger les données et maj des selectizeInput #
  ################################################################ 
  dat <- reactive({
    file1 <- input$file1
    req(file1)
    dataSet <- read.csv(file=file1$datapath, header = input$header, sep = input$sep, quote = input$quote)
    vars <- colnames(dataSet)
    #row <- nrow(dataSet)
    
    dataSet
  })
  
  dat2 <- reactive({
    file2 <- input$file2
    req(file2)
    dataSet2 <- read.csv(file=file2$datapath, header = input$header2, sep = input$sep2, quote = input$quote2)
    vars2 <- colnames(dataSet2)
    #row <- nrow(dataSet)
    
    dataSet2
  }) 
  
  
  output$select_coul <- renderUI({
    selectizeInput('selectcoul', 'Variable pour la couleur des noeuds', choices = c("Pas de distinction de couleur" ,  vertex_attr_names(Mon_reseau())), selected="Pas de distinction de couleur")
  })
  
  output$select_shape <- renderUI({
    selectizeInput('selectshape', 'Variable pour la forme des noeuds', choices = c("Pas de distinction forme" , vertex_attr_names(Mon_reseau())), selected="Pas de distinction forme")
  }) 
  
  output$select_size2 <- renderUI({
    selectizeInput('size2', 'Définition variable pour la taille des noeuds', choices = c("select" = "",  vertex_attr_names(Mon_reseau())))
  }) 
  
  output$select_fontsize2 <- renderUI({
    selectizeInput('fontsize2', 'Définition variable pour filtrer les labels', choices = c("select" = "",  vertex_attr_names(Mon_reseau())))
  }) 
  
  output$select_sizeedge <- renderUI({
    selectizeInput('sizeedge', 'Définition variable pour epaisseur des liens', choices = c("Pas de différence d'épaisseur" ,   edge_attr_names(Mon_reseau())))
  }) 
  
  Mon_reseau <- reactive({
    graph_from_data_frame(dat(), 
                          vertices = dat2(), 
                          directed = input$directed)
  })
  
  
  
  colrs <-  reactive({
    #col <- brewer.pal(12, input$select_palette)
    if(input$selectcoul=="Pas de distinction de couleur")
      colrs <- "Pas de couleurs"
    else
      colrs <- as.factor(vertex_attr(Mon_reseau())[[input$selectcoul]])
  })
  
  colrs2 <- reactive({
    colrs2 <- as.character(membership(get(input$comm)(Mon_reseau())))
  })
  
  colrs3 <- reactive({
    colrs3 <- get(input$typenode)()
  })
  
  colrs4 <- reactive({
    if(input$typecoul == "Attributs noeuds")
      colrs4<- colrs()
    else if(input$typecoul == "Type de noeuds")
      colrs4 <- colrs3()
    else
      colrs4 <- colrs2()
  })
  
  shp <- reactive({
    if(input$selectshape=="Pas de distinction forme")
      shp <- "Pas de forme"
    else
      shp <- as.factor(vertex_attr(Mon_reseau())[[input$selectshape]])
  })
  
  
  
  size2 <- reactive({
    size2 <- V(Mon_reseau())$size
    if(input$typesize == "Taille fixe")
      size2 <- 5
    else if(input$typesize == "Attributs noeuds")
      size2 <- vertex_attr(Mon_reseau())[[input$size2]]
    else
      size2 <- get(input$size3)()
  })
  
  size3 <- reactive({
    size3 <- V(Mon_reseau())$size
    if(input$typefontsize == "Pas de filtre")
      size3 <- 5
    else if(input$typefontsize == "Attributs noeuds")
      size3 <- vertex_attr(Mon_reseau())[[input$fontsize2]]
    else
      size3 <- get(input$fontsize3)()
  })
  
  ecart <- reactive({
    if(input$typesize == "Taille fixe")
      ecart <- 3
    else if(input$typesize == "Attributs noeuds")
      ecart <- input$slidernode1
    else
      ecart <- input$slidernode2
  })
  
  
  ecart2 <- reactive({
    if(input$typefontsize == "Pas de filtre")
      ecart2 <- 3
    else if(input$typefontsize == "Attributs noeuds")
      ecart2 <- input$sliderlabel1
    else
      ecart2 <- input$sliderlabel2
  })
  
  
  
  sizeedge <- reactive({
    if(input$sizeedge=="Pas de différence d'épaisseur")
      sizeedge <- NULL
    else
      sizeedge <- edge_attr(Mon_reseau())[[input$sizeedge]]
  })
  
  arrowedge <- reactive({
    if (input$sensedge == "Ne pas indiquer")
      arrowedge <- NULL
    else
      arrowedge <- arrow(type = "closed", length = unit(2, 'mm'))
  })
  
  output$Reseau <- renderPlot({ 
    #plot(Mon_reseau(), vertex.color= colrs3(), vertex.shape =shp())
    ggraph(Mon_reseau(), layout = input$select_layout) + 
      geom_edge_link0(aes(edge_width = sizeedge()), arrow = arrowedge() ) +
      geom_node_point(aes(shape = shp(),  colour = colrs4(), size=size2() )) +  
      geom_node_text(aes(filter = size3() >= ecart2(), label = name), family= input$typefont, fontface=input$fontface, repel=TRUE)+
      scale_color_brewer(palette = input$select_palette)+
      scale_size_continuous(range = ecart())+
      scale_edge_width_continuous(range = input$slideredge)+
      theme_graph()+
      theme(legend.position = input$posleg)
  })
  
  ##########################'
  #Pour télécharger graphe 1
  ##########################'
  
  reseau1 <- reactive({
    ggraph(Mon_reseau(), layout = input$select_layout) + 
      geom_edge_link0(aes(edge_width = sizeedge()) ) +
      geom_node_point(aes(shape = shp(),  colour = colrs4(), size=size2() )) +  
      geom_node_text(aes(filter = size3() >= ecart2(), label = name), family= input$typefont, fontface=input$fontface, repel=TRUE)+
      scale_color_brewer(palette = input$select_palette)+
      scale_size_continuous(range = ecart())+
      scale_edge_width_continuous(range = input$slideredge)+
      theme_graph()+
      theme(legend.position = input$posleg)
  })
  
  
  output$downloadreseau <- downloadHandler(
    filename = function(){
      paste("Mon_reseau", tolower(input$filetype_reseau), sep =".")
    }, 
    content = function(file){
      width  <- 900
      height <- 900
      pixelratio <- 2
      if(input$filetype_reseau == "PNG")
        png(file, width=width*pixelratio, height=height*pixelratio,
            res=72*pixelratio, units = "px")
      else if(input$filetype_reseau == "SVG")
        svg(file, width=12, height=12)
      else
        pdf(file, width = 12, height = 12)
      print(reseau1())
      dev.off()
    })
  
  ################################'
  ################################'
  #Graph visnetwork
  ################################'
  ################################'
  
  
  visdata <- reactive({
    toVisNetworkData(Mon_reseau())
  })
  
  output$visnet <- renderVisNetwork({
    visNetwork(nodes = visdata()$nodes, edges = visdata()$edges) %>% 
      visIgraphLayout(layout = "layout_nicely") %>%
      visOptions(highlightNearest = list(enabled = T, hover = T), nodesIdSelection = T)
  })
  
  ######################'
  ## export noeuds avec indicateurs
  ######################'
  nodes <- reactive({
    
    Monreseau <- Mon_reseau()
    # Les valeurs des centralités de degré sont insérées dans la variable 'degre'
    degree  <-  degree(Monreseau)
    
    # Centralité d'intermediarité dans la variable 'intermed'
    betweenness  <-  betweenness(Monreseau)
    
    # Centralité de proximité dans la variable 'proximit'
    closeness  <-  closeness(Monreseau)
    
    #Sommet_de_mon_reseau <- as_data_frame(Mon_reseau(), what="vertices")
    
    
    cbind(degree, betweenness, closeness)
  })
  
  
  nodes2 <- reactive({
    nodes2 <- as.data.frame(nodes())
  })
  
  
  output$table_noeuds = DT::renderDataTable({
    nodes2()
  })
  
  
  ######################'
  ##### Ego-réseau
  ######################'
  
  EgoNet <- reactive({
    make_ego_graph(Mon_reseau(),
                   nodes = V(Mon_reseau()),
                   order = 2,
                   mode = c("all"),
                   mindist = 0)
  })
  
  
  EgoNet_id <- reactive({
    rowid_to_column(dat2())
  })
  
  idrows <- reactive({
    EgoNet_id()$rowid
  })
  
  output$select_name <- renderUI({
    selectizeInput('name', "Sélectionner l'identifiant du noeud pour créer l'ego-reseau" , choices = idrows(), selected="")
  })
  
  
  
  monego <- reactive({
    a <- as.numeric(input$name)
    EgoNet()[[a]]
  })
  
  #######
  output$egoselect_coul <- renderUI({
    selectizeInput('egoselectcoul', 'Variable pour la couleur des noeuds', choices = c("Pas de distinction de couleur" ,  vertex_attr_names(monego())), selected="Pas de distinction de couleur")
  })
  
  output$egoselect_shape <- renderUI({
    selectizeInput('egoselectshape', 'Variable pour la forme des noeuds', choices = c("Pas de distinction forme" , vertex_attr_names(monego())), selected="Pas de distinction forme")
  })
  
  output$egoselect_sizeedge <- renderUI({
    selectizeInput('egosizeedge', 'Définition variable pour epaisseur des liens', choices = c("Pas de différence d'épaisseur" ,   edge_attr_names(monego())))
  }) 
  ##########
  
  l <- reactive({
    l <- get(input$egoselect_layout)(monego())
  })
  
  ##
  egocolrs <-  reactive({
    #col <- brewer.pal(12, input$select_palette)
    if(input$egoselectcoul=="Pas de distinction de couleur")
      egocolrs <- "Pas de couleurs"
    else
      egocolrs <- as.factor(vertex_attr(monego())[[input$egoselectcoul]])
  })
  
  egocolrs2 <- reactive({
    egocolrs2 <- as.character(membership(get(input$egocomm)(monego())))
  })
  
  
  egocolrs4 <- reactive({
    if(input$egotypecoul == "Attributs noeuds")
      egocolrs4<- egocolrs()
    else
      egocolrs4 <- egocolrs2()
  })
  
  
  ###
  
  egoshp <- reactive({
    if(input$egoselectshape=="Pas de distinction forme")
      shp <- "Pas de forme"
    else
      shp <- as.factor(vertex_attr(monego())[[input$egoselectshape]])
  })
  
  ##
  
  egosize2 <- reactive({
    egosize2 <- V(monego())$size
    if(input$egotypesize == "Taille fixe")
      egosize2 <- 5
    else
      egosize2 <- get(input$egosize3)(normalized = TRUE)
  })
  
  ##
  
  egoecart <- reactive({
    if(input$egotypesize == "Taille fixe")
      egoecart <- 3
    else
      egoecart <- input$egoslidernode2
  })
  
  ##
  egosize3 <- reactive({
    egosize3 <- V(monego())$size
    if(input$egotypefontsize == "Pas de filtre")
      egosize3 <- 5
    else
      egosize3 <- get(input$egofontsize3)()
  })
  
  ##
  egoecart2 <- reactive({
    if(input$egotypefontsize == "Pas de filtre")
      egoecart2 <- 3
    else
      egoecart2 <- input$egosliderlabel2
  })
  
  ##
  
  egosizeedge <- reactive({
    if(input$egosizeedge=="Pas de différence d'épaisseur")
      egosizeedge <- NULL
    else
      egosizeedge <- edge_attr(monego())[[input$egosizeedge]]
  })
  ##
  egoarrowedge <- reactive({
    if (input$egosensedge == "Ne pas indiquer")
      egoarrowedge <- NULL
    else
      egoarrowedge <- arrow(type = "closed", length = unit(2, 'mm'))
  })
  
  ##
  
  output$egoreseau <- renderPlot({ 
    #plot(monego(), layout = l(), margin=c(0,0,0.2,0))
    ggraph(monego(), layout = input$egoselect_layout) + 
      geom_edge_link0(aes(edge_width = egosizeedge()), arrow = egoarrowedge()) +
      geom_node_point(aes(shape = egoshp(), colour = egocolrs4(), size=egosize2())) +
      geom_node_text(aes(filter = egosize3() >= egoecart2(),label = V(monego())$name), family= input$egotypefont, fontface=input$egofontface, repel=TRUE)+
      scale_color_brewer(palette = input$egoselect_palette)+
      scale_size_continuous(range = egoecart())+
      scale_edge_width_continuous(range = input$egoslideredge)+
      theme_graph()+
      theme(legend.position = input$egoposleg)
  })
  
  
  
  ego <- reactive({
    ggraph(monego(), layout = input$egoselect_layout) + 
      geom_edge_link0(aes(edge_width = egosizeedge()), arrow = egoarrowedge()) +
      geom_node_point(aes(shape = egoshp(), colour = egocolrs4(), size=egosize2())) +
      geom_node_text(aes(filter = egosize3() >= egoecart2(),label = V(monego())$name), family= input$egotypefont, fontface=input$egofontface, repel=TRUE)+
      scale_color_brewer(palette = input$egoselect_palette)+
      scale_size_continuous(range = egoecart())+
      scale_edge_width_continuous(range = input$egoslideredge)+
      theme_graph()+
      theme(legend.position = input$egoposleg)
  })
  
  
  output$downloadegoreseau <- downloadHandler(
    filename = function(){
      paste("Mon_ego-reseau", tolower(input$filetype_egoreseau), sep =".")
    }, 
    content = function(file){
      width  <- 900
      height <- 900
      pixelratio <- 2
      if(input$filetype_egoreseau == "PNG")
        png(file, width=width*pixelratio, height=height*pixelratio,
            res=72*pixelratio, units = "px")
      else if(input$filetype_egoreseau == "SVG")
        svg(file, width=12, height=12)
      else
        pdf(file, width = 12, height = 12)
      print(ego())
      dev.off()
    })
  
  
  ###########################'
  #### Sous corpus
  ###########################'
  
  output$select_varsc <- renderUI({
    selectizeInput('varsc', 'Variable pour la création du sous réseau', choices = vertex_attr_names(Mon_reseau()), selected="")
  })
  
  mod <- reactive({
    vertex_attr(Mon_reseau())[[input$varsc]]
    
  })
  
  output$select_modsc <- renderUI({
    selectizeInput('modsc', 'Variable pour la création du sous réseau', choices = mod(), selected="")
  }) 
  
  nn <- reactive({
    n <- V(Mon_reseau())
    varsc <- input$varsc
    nn <- n$modsc
  })
  
  #vertex_attr(Mon_reseau())[[input$varsc]]
  

  
  sousnet <- reactive({induced_subgraph(Mon_reseau(), 
                              v = which(vertex_attr(Mon_reseau())[[input$varsc]] == c(input$modsc)))
  })
  
  
  #######
  output$sousselect_coul <- renderUI({
    selectizeInput('sousselectcoul', 'Variable pour la couleur des noeuds', choices = c("Pas de distinction de couleur" ,  vertex_attr_names(sousnet())), selected="Pas de distinction de couleur")
  })
  
  output$sousselect_shape <- renderUI({
    selectizeInput('sousselectshape', 'Variable pour la forme des noeuds', choices = c("Pas de distinction forme" , vertex_attr_names(sousnet())), selected="Pas de distinction forme")
  })
  
  output$sousselect_sizeedge <- renderUI({
    selectizeInput('soussizeedge', 'Définition variable pour epaisseur des liens', choices = c("Pas de différence d'épaisseur" ,   edge_attr_names(sousnet())))
  }) 
  ##########
  
  l2 <- reactive({
    l2 <- get(input$sousselect_layout)(sousnet())
  })
  
  ##
  souscolrs <-  reactive({
    #col <- brewer.pal(12, input$select_palette)
    if(input$sousselectcoul=="Pas de distinction de couleur")
      souscolrs <- "Pas de couleurs"
    else
      souscolrs <- as.factor(vertex_attr(sousnet())[[input$sousselectcoul]])
  })
  
  souscolrs2 <- reactive({
    souscolrs2 <- as.character(membership(get(input$souscomm)(sousnet())))
  })
  
  
  souscolrs4 <- reactive({
    if(input$soustypecoul == "Attributs noeuds")
      souscolrs4<- souscolrs()
    else
      souscolrs4 <- souscolrs2()
  })
  
  
  ###
  
  sousshp <- reactive({
    if(input$sousselectshape=="Pas de distinction forme")
      sousshp <- "Pas de forme"
    else
      sousshp <- as.factor(vertex_attr(sousnet())[[input$sousselectshape]])
  })
  
  ##
  
  soussize2 <- reactive({
    soussize2 <- V(sousnet())$size
    if(input$soustypesize == "Taille fixe")
      soussize2 <- 5
    else
      soussize2 <- get(input$soussize3)(normalized = TRUE)
  })
  
  ##
  
  sousecart <- reactive({
    if(input$soustypesize == "Taille fixe")
      sousecart <- 3
    else
      sousecart <- input$sousslidernode2
  })
  
  ##
  soussize3 <- reactive({
    soussize3 <- V(sousnet())$size
    if(input$soustypefontsize == "Pas de filtre")
      soussize3 <- 5
    else
      soussize3 <- get(input$sousfontsize3)()
  })
  
  ##
  sousecart2 <- reactive({
    if(input$soustypefontsize == "Pas de filtre")
      sousecart2 <- 3
    else
      sousecart2 <- input$soussliderlabel2
  })
  
  ##
  
  soussizeedge <- reactive({
    if(input$soussizeedge=="Pas de différence d'épaisseur")
      soussizeedge <- NULL
    else
      soussizeedge <- edge_attr(sousnet())[[input$soussizeedge]]
  })
  ##
  sousarrowedge <- reactive({
    if (input$soussensedge == "Ne pas indiquer")
      sousarrowedge <- NULL
    else
      sousarrowedge <- arrow(type = "closed", length = unit(2, 'mm'))
  })
  
  ##
  
  
  output$sousreseau <- renderPlot({ 
    #plot(sousnet())
    ggraph(sousnet(), layout = input$sousselect_layout) + 
      geom_edge_link0(aes(edge_width = soussizeedge()), arrow = sousarrowedge()) +
      geom_node_point(aes(shape = sousshp(), colour = souscolrs4(), size=soussize2())) +
      geom_node_text(aes(filter = soussize3() >= sousecart2(),label = V(sousnet())$name), family= input$soustypefont, fontface=input$sousfontface, repel=TRUE)+
      scale_color_brewer(palette = input$sousselect_palette)+
      scale_size_continuous(range = sousecart())+
      scale_edge_width_continuous(range = input$sousslideredge)+
      theme_graph()+
      theme(legend.position = input$sousposleg)
  })
  
  
  sousnet2 <- reactive({
    ggraph(sousnet(), layout = input$sousselect_layout) + 
      geom_edge_link0(aes(edge_width = soussizeedge()), arrow = sousarrowedge()) +
      geom_node_point(aes(shape = sousshp(), colour = souscolrs4(), size=soussize2())) +
      geom_node_text(aes(filter = soussize3() >= sousecart2(),label = V(sousnet())$name), family= input$soustypefont, fontface=input$sousfontface, repel=TRUE)+
      scale_color_brewer(palette = input$sousselect_palette)+
      scale_size_continuous(range = sousecart())+
      scale_edge_width_continuous(range = input$sousslideredge)+
      theme_graph()+
      theme(legend.position = input$sousposleg)
  })
  
  output$downloadsousreseau <- downloadHandler(
    filename = function(){
      paste("Mon_sous-reseau", tolower(input$filetype_sousreseau), sep =".")
    }, 
    content = function(file){
      width  <- 900
      height <- 900
      pixelratio <- 2
      if(input$filetype_sousreseau == "PNG")
        png(file, width=width*pixelratio, height=height*pixelratio,
            res=72*pixelratio, units = "px")
      else if(input$filetype_sousreseau == "SVG")
        svg(file, width=12, height=12)
      else
        pdf(file, width = 12, height = 12)
      print(sousnet2())
      dev.off()
    })
  
  
  #################'
  ####### Mumuse
  #################'
  
  
#  output$Mumuse <- renderVisNetwork({
#    visNetwork(nodes = visdata()$nodes, edges = visdata()$edges)
#  })
  
  
  
}



shinyApp(ui=ui,server=server)


