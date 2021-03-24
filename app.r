library(shinythemes)
library(shiny)
library(knitr)
library(visNetwork)
library(sever)
require(particlesjs)
library(shinyWidgets)

options(shiny.maxRequestSize = 30*1024^2)


ui<-fluidPage(theme = shinytheme("yeti"),
              use_sever(),
              particles("pat/to/particlesjs-config02.json"),
              setBackgroundImage( src = "kraken_07_04.png"),
              setBackgroundColor("rgba(232, 236, 241, 1)"), #pour voir l'image en dessous penser à la transparence
              
              navbarPage(tags$a("kRaken", href = 'https://github.com/LeCampionG/kRaken/tree/master',
                                icon("github"), target="_blank"),
                         tabPanel("A propos",
                                  #code Html
                                  div(class='simpleDiv',
                                      tags$style(HTML(
                                        '.simpleDiv{
            background-color:#e8ecf173;
            color: #000000ff 
            }')),#style="background-color:black", 
                                      h3(strong("Bienvenue sur l'application kRaken !!!")),
                                      p(strong("kRaken")," est une application Rshiny qui permet la réalisation de réseaux et de sous-réseaux, ainsi que leur export en svg. Toute les analyses que vous réaliserez, reposent sur les packages", a("igraph", href="https://cran.r-project.org/web/packages/igraph/index.html", target="_blank"), "et", a("ggraph", href="https://cran.r-project.org/web/packages/ggraph/index.html", target="_blank"),"."),
                                      p("Cette application n'est qu'une première approche, pour exploiter ces packages au maximum de leur possibilités, la meilleure solution est de les utiliser directement dans R."),
                                      p(""),
                                      h4(strong("Pré-requis pour utiliser kRaken :")),
                                      p(strong("1- "), "Il est nécessaire, d'avoir 2 fichiers distinct, un pour les liens et un second pour les noeuds."),
                                      p(strong("2- "), "Concernant la table des liens: les deux premières colonnes doivent correspondre à l'origine et la destination du liens du réseau. Les variables suceptible de caractériser les liens (poids...) doivent venir aprés. Vous pouvez nommer les variables origine et destination, comme vous le souhaitez (from/to, source/target, i/j, origine/destination...), mais il est nécessaire de conserver cette ordre. N'hésitez pas à télécarger les fichiers exemple présents sur la page",em("Import Table des Liens")),
                                      p(strong("3- "), "Pour la table noeuds : il est absoluement nécessaire d'avoir une variable", strong("name")," qui sert d'identifiant des noeuds et de labels dans la réalisation des réseaux. Tous les noeuds identifiés dans la base liens doivent être présent dans la table noeuds, et inversement. Par ailleurs, il ne faut aucun doublons dans la table noeuds. N'hésitez pas à télécarger les fichiers exemple présents sur la page",em("Import Table des Noeuds")),
                                      h4(strong("Spatialisation des réseaux :")),
                                      p("Les algorithmes de spatialisation qui sont proposés sont ceux du package", a("igraph", href="https://cran.r-project.org/web/packages/igraph/index.html", target="_blank"), ". Par défaut, la première spatialisation proposée est nicely qui permet de représenter le réseau de la manière la plus lisible possible . Vous pourrez bien sur modifier les algorithmes et choisir le plus adapté à vos analyses. Pour plus d'informations, je vous renvois vers la documentation du package ", a("igraph", href="https://cran.r-project.org/web/packages/igraph/index.html", target="_blank"),"."),
                                      h4(strong("Détection des communautés dans le réseau :")),
                                      p("Les algorithmes de détection de communautés proposés sont ceux implémentés dans le package ", a("igraph", href="https://cran.r-project.org/web/packages/igraph/index.html", target="_blank"), ". Vous pourrez modifier l'algorithme de détection de communautés en fonction de vos besoins et des spécificités de votre réseau. Pour un descriptif des algorithmes proposés et  une réflexion plus poussée sur l'analyse des commuanutés dans l'étude de réseaux, je vous renvois vers ce billet de blog de ", strong(a("Laurent Beauguitte", href="https://arshs.hypotheses.org/1314", target="_blank")), " , vous pouvez aussi lire la documentation correspondante du package ", a("igraph", href="https://cran.r-project.org/web/packages/igraph/index.html", target="_blank"),"."),
                                      h4(strong("Bref descriptif des onglets de l'application :")),
                                      p("1- A propos : Il s'agit d'un bref descriptif de l'application kRaken. Onglet que vous consultez en ce moment !"),
                                      p("2- Import table des Liens : Permet de charger et visualiser vos données concernant les liens du réseau."),
                                      p("3- Import table des Noeuds : Permet de charger et visualiser vos données concernant les noeuds du réseau."),
                                      p("4- Explo : Permet une exploration rapide et de manière intéractive du réseau à l'ai du package ", a("visnetwork", href="https://cran.r-project.org/web/packages/visNetwork/index.html", target="_blank"), " Vous pourrez sélectionner les noeuds, les déplacer, zoomer et dézoomer."),
                                      p("5- Réseau : Permet de visualiser votre réseau dans son intégralité. Vous pourrez modifier les différents paramètres du réseau en fonction de différents indicateurs ou de variables associées à vos liens ou noeuds."),
                                      p("6- Ego-réseau : Permet de réaliser le sous réseau d'un noeud en particulier. Vous devez choisir la taille de cet égo-réseau et vous pourrez modifier votre réseau avec les mêmes options que dans l'onglet Réseau. Cet égo-réseau est constitué à l'aide de la fonction ", em("make_ego_graph()")," du package igraph."),
                                      p("7- Sous-réseau : Permets de réaliser un sous-réseau en fonction d'une variable de la table noeuds. Vous pourrez modifier le réseau obtenu de la même manière que dans l'onglet Réseau."),
                                      p("8- Sous-réseau des communautés : Permet d'isoler la communauté de votre choix, obtenu à l'aide de l'algorithme de détection de communautés sélectionné. Vous pourrez modifier le sous-réseau obtenu de la même manière que dans l'onglet Réseau."),
                                      p("9- Export noeuds : Permet un export de la table noeuds enrichie de différents indicateurs : le degré, le betweeness centrality et du closeness centrality, ainsi que de la communauté d'appartenance du noeud. L'algorithme de détection de communautés est celui défini dans l'onglet Sous-réseau de communautés, par défaut il s'agit de Edge beetweenes. Davantage d'informations concernant ces différents indicateurs est fournie dans l'onglet. "),
                                      p("10- Indicateur réseau : Permet d'obtenir les principaux indicateurs concernant nos différents réseaux."),
                                      p("Lorsque vous allez dans les onglets de réprésentation de réseau, après avoir correctement chargé vos fichiers liens et noeuds il est possible qu'un message d'erreur s'affiche pendant quelques secondes. Ce message d'erreur n'apparait que le temps que l'application procède à la création du réseau. Plus votre réseau sera important, plus le temps d'affichage de ce message sera long. Si le message persite alors c'est qu'il y a bel et bien une erreure quelque part!"),
                                      h4(strong("Citation")), 
                                      p("Dans l'éventualité où vous utiliseriez cette application dans le cadre d'une publication, vous pouvez citer cet outil comme ceci :"),
                                      p(strong("Le Campion G. ", a("kRaken: un outil Rshiny pour produire, visualiser, explorer et analyser des réseaux.", href="https://analytics.huma-num.fr/Gregoire.LeCampion/Reseau/")," Pôle ARD UMR 5319 UMR Passages. 2019.")),
                                      h4(strong("Packages et ressources")),
                                      p("Plusieurs package interviennent dans la construction de kRaken. Pour la réalisation des réseaux il s'agit des package ", a("igraph", href="https://cran.r-project.org/web/packages/igraph/index.html", target="_blank"), "/", a("ggpraph", href="https://cran.r-project.org/web/packages/ggraph/index.html", target="_blank"),"/", a("tidygraph", href="https://cran.r-project.org/web/packages/tidygraph/index.html", target="_blank"),"/", a("graphlayouts", href="https://cran.r-project.org/web/packages/graphlayouts/index.html", target="_blank"),"/", a("visnetwork", href="https://cran.r-project.org/web/packages/visNetwork/index.html", target="_blank"), ". Pour ce qui est de l'application en elle même, les packages utilisés sont : ", a("shiny", href="https://cran.r-project.org/web/packages/shiny/index.html", target="_blank"),"/", a("shinytheme", href="https://cran.r-project.org/web/packages/shinythemes/index.html", target="_blank"), "/",a("shinyWidget", href="https://cran.r-project.org/web/packages/shinyWidgets/index.html", target="_blank"),"/", a("sever", href="https://sever.john-coene.com/", target="_blank"),"/", a("particleJS", href="https://github.com/dreamRs/particlesjs", target="_blank"),"/", a("DT", href="https://cran.r-project.org/web/packages/DT/index.html", target="_blank"),"/", a("dplyr", href="https://cran.r-project.org/web/packages/dplyr/index.html", target="_blank"),"/", a("readr", href="https://cran.r-project.org/web/packages/readr/index.html", target="_blank"),"."),
                                      p("Pour ce qui est des ressources sur l'analyse de réseaux, il existe une très importante littérature scientifique. Je vous conseillerai également les écrits et travaux de Laurent Beauguitte et François Briatte."),
                                      p("Si vous souhaitez, réaliser vous même vos réseau sur R, vous pourrez vous aider entres autres des documents suivants :"),
                                      p("- ", a("Network Analysis and Visualization with R and igraph de Katherine Ognyanova,", href="https://kateto.net/networks-r-igraph", target="_blank")),
                                      p("- ", a("Introduction to Network Analysis with R de Jesse Sadle,", href="https://www.jessesadler.com/post/network-analysis-with-r/", target="_blank")),
                                      p("- ", a("Network Analysis in R Cookbook de Sacha Epskamp.", href="http://sachaepskamp.com/files/Cookbook.html", target="_blank")),
                                      br(),
                                      p(em("Pour plus d'applications rendez-vous sur le site du pôle ARD :", strong(a("OUVRIR", href="http://ouvrir.passages.cnrs.fr/")),"."))
                                      
                                  )),
                         
                         tabPanel("Import Table des Liens",
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
                                      selectizeInput("encoding1", label="Encodage du fichier", choices=c("UTF-8", "ISO-8859-15", "WINDOWS-1252"), selected= "UTF-8", multiple=FALSE),
                                      # Pour déterminer si la première ligne correspond aux en-tête de colonne
                                      checkboxInput("header", "1ere ligne comme en-tête", TRUE),
                                      #déterminer le séparateur de champ
                                      radioButtons("sep", "Séparateur de champ",
                                                   choices = c("Virgule" = ",",
                                                               "Point-virgule" = ";",
                                                               "Tabulation" = "\t"),
                                                   selected = ","),
                                      #déterminer séparateur de texte
                                      radioButtons("quote", "Séparateur de texte",
                                                   choices = c("Aucun" = "",
                                                               "Guillemet double" = '"',
                                                               "Guillemet simple" = "'"),
                                                   selected = '"'),
                                      #Choix du mode de visualistaion
                                      #radioButtons("disp", "Visualiser",
                                      #choices = c("Uniquement les 1eres lignes" = "head",
                                      #"Ensemble des données"= "all"),
                                      #selected = "head")
                                    ),
                                    #########################################################
                                    # 2. Panneau principal pour visualiser données chargées #
                                    #########################################################
                                    mainPanel(tabsetPanel(
                                      tabPanel("Liens",
                                               DT::dataTableOutput("tb1")
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
                                      selectizeInput("encoding2", label="Encodage du fichier", choices=c("UTF-8", "ISO-8859-15", "WINDOWS-1252"), selected= "UTF-8", multiple=FALSE),
                                      # Pour déterminer si la première ligne correspond aux en-tête de colonne
                                      checkboxInput("header2", "1ere ligne comme en-tête", TRUE),
                                      #déterminer le séparateur de champ
                                      radioButtons("sep2", "Séparateur de champ",
                                                   choices = c("Virgule" = ",",
                                                               "Point-virgule" = ";",
                                                               "Tabulation" = "\t"),
                                                   selected = ","),
                                      #déterminer séparateur de texte
                                      radioButtons("quote2", "Séparateur de texte",
                                                   choices = c("Aucun" = "",
                                                               "Guillemet double" = '"',
                                                               "Guillemet simple" = "'"),
                                                   selected = '"'),
                                      #Choix du mode de visualistaion
                                      #radioButtons("disp2", "Visualiser",
                                      # choices = c("Uniquement les 1eres lignes" = "head",
                                      # "Ensemble des données"= "all"),
                                      #selected = "head")
                                    ),
                                    #########################################################
                                    # 2. Panneau principal pour visualiser données chargées #
                                    #########################################################
                                    mainPanel(tabsetPanel(
                                      tabPanel("Noeuds",
                                               DT::dataTableOutput("tb2")
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
                                      div(style ="color:lightslategray", h5(strong("Paramètres réseau"))),
                                      radioButtons("directed", label="Votre réseau est-il dirigé ?", choices=list("Oui"="TRUE","Non"="FALSE"), selected="FALSE"),
                                      helpText("Les algorithmes de spatialisation présentés ici sont ceux fournit par le package igraph, davantage d'information est disponible dans la", strong(a("documentation du package", href ="https://igraph.org/r/doc/igraph.pdf", target="_blank")), ", sur le ", strong(a("site du package", href="https://igraph.org/r/doc/layout_nicely.html", target="_blank")), "ou encore sur ", strong(a("rdrr", href="https://rdrr.io/cran/ggraph/man/layout_tbl_graph_igraph.html", target="_blank")), ". L'algorithme stress vient du package graphlayouts"),
                                      selectizeInput('select_layout', "Algorithme de spacialisation du réseau", choices = c("nicely","stress", "Fruchterman-Reingold (fr)"="fr", "Kamada-Kawai (kk)"="kk", "force directed (drl)"="drl", "Large Graph Layout (lgl)"="lgl","tree", "sugiyama", "star", "circle", "Davidson-Harels (dh)"="dh", " GEM force-directed (gem)"="gem", "graphopt", "grid", "Multidimensional scaling (mds)"="mds", "sphere", "randomly"), multiple=FALSE, selected="nicely"),
                                      #radioButtons("advance", " Paramètres avancés", choices = c("Non", "Oui"), selected = "Oui"),
                                      #conditionalPanel(condition="input.advance == 'Oui'",
                                      selectizeInput('posleg', "Position de la légende", choices = c("En-bas"="bottom", "A droite"="right", "A gauche"="left", "Au-dessus"="top", "Pas de légende"="none"), multiple=FALSE, selected="bottom"),
                                      tags$hr(),
                                      div(style ="color:lightslategray", h5(strong("Paramètres noeuds"))),
                                      selectizeInput("typecoul", label="Définition de la couleur des noeuds", choices=list("Attributs noeuds","Type de noeuds", "Communautés réseaux"), selected=""),
                                      conditionalPanel(condition="input.typecoul == 'Attributs noeuds'",
                                                       uiOutput("select_coul")),
                                      conditionalPanel(condition="input.typecoul == 'Communautés réseaux'",
                                                       helpText("Attention : certains algorithmes de détection des communautés ne fonctionnent que pour des réseaux dirigés et inversement d'autres ne fonctionnent que dans le cas de réseaux non-dirigés. Un choix non adapté entrainera un message d'erreur."),
                                                       selectizeInput("comm", label="Définition de l'algorithme de création de communauté", choices=list("Louvain"="cluster_louvain", "Walktrap"="cluster_walktrap", "Spinglass"="cluster_spinglass", "Leading eigen"="cluster_leading_eigen", "Edge betweenness"="cluster_edge_betweenness", "Fast greedy"="cluster_fast_greedy", "Label prop"="cluster_label_prop"), selected="")),
                                      conditionalPanel(condition="input.typecoul == 'Type de noeuds'",
                                                       helpText("Les choix proposés ici viennent du package tidygraph, plus précisément de la fonction node_types. Pour davantage d'information se référer à ", strong(a("la documentation", href="https://cran.r-project.org/web/packages/tidygraph/tidygraph.pdf", target="_blank")), " du package"),
                                                       selectizeInput("typenode", label="Type de noeuds", choices=list("node_is_center", "node_is_cut", "node_is_sink", "node_is_source", "node_is_isolated", "node_is_universal", "node_is_simplical", "node_is_adjacent"), selected="")),
                                      selectizeInput('select_palette', "Palette de couleur à utiliser", choices = c("Set3","Paired","Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG"), multiple=FALSE, selected="Set3"),
                                      helpText("Les palettes proposées, viennent de ", strong(a("RcolorBrewer", href="https://cran.r-project.org/web/packages/RColorBrewer/index.html", target="_blank"))),
                                      uiOutput("select_shape"),
                                      selectizeInput("typesize", label="Définition de la taille des noeuds", choices=list("Taille fixe", "Attributs noeuds", "Indicateurs analyse de réseaux"), selected=""),
                                      #conditionalPanel(condition="input.typesize == 'Taille fixe'",
                                      #sliderInput("size1", label="Taille des noeuds", value=1, min=1, max=50, step=1)),
                                      conditionalPanel(condition="input.typesize == 'Attributs noeuds'",
                                                       uiOutput("select_size2"),
                                                       sliderInput("slidernode1", "Intervalle pour la taille des noeuds",min = 1 , max = 50, value = c(3, 8))),
                                      conditionalPanel(condition="input.typesize == 'Indicateurs analyse de réseaux'",
                                                       selectizeInput("size3", label="Définition indicateur pour la taille des noeuds", choices=list("Degré"="centrality_degree", "Betweenness"="centrality_betweenness", "Closeness"="centrality_closeness"), selected=""),
                                                       sliderInput("slidernode2", "Intervalle pour taille des noeuds",min = 1 , max = 50, value = c(3, 8))),
                                      tags$hr(),
                                      div(style ="color:lightslategray", h5(strong("Paramètres Labels"))),
                                      selectizeInput("typefont", label="Police à utiliser pour les labels", choices=list("Times", "Helvetica", "Courier", "AvantGarde", "Palatino", "Bookman"), selected="Times"),
                                      selectizeInput("fontface", label="Caractéristique police", choices=list("Normal"="plain","Gras"="bold","Italique"="italic","Gras et italique"="bold.italic"), selected="plain"),
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
                                      div(style ="color:lightslategray", h5(strong("Paramètres Liens"))),
                                      selectizeInput("sensedge", label="Indiquer le sens du lien", choices=list("Ne pas indiquer", "Indiquer"), selected="Ne pas indiquer"),
                                      selectizeInput("couleuredges", label="Définition de la couleur des liens", choices=list("Couleur par défaut","Attributs liens","Attributs noeuds"), selected=""),
                                      conditionalPanel(condition = "input.couleuredges=='Attributs liens'",
                                                       uiOutput("selectedge_coledge")),
                                      conditionalPanel(condition = "input.couleuredges=='Attributs noeuds'",
                                                       uiOutput("select_coledge")),
                                      uiOutput("select_sizeedge"),
                                      sliderInput("slideredge", "Intervalle pour définir l'épaisseur des liens",min = 0.1 , max = 10, value = c(0.2, 3), step=0.1)#)
                                    ),
                                    
                                    ############################################################################
                                    # 4. Le panneau principal, pour l'affichage des résultats Arbre de décision#
                                    ############################################################################
                                    mainPanel(
                                      tabPanel("Réseau",
                                               plotOutput("Reseau", height="1000px"),
                                               radioButtons(
                                                 inputId = "filetype_reseau",
                                                 label = "Quel format d'image :",
                                                 inline = TRUE,
                                                 choices = list("PNG","SVG")),
                                               downloadButton(outputId = "downloadreseau", label = "Télécharger Reseau")
                                      )
                                    )
                                  )
                         ),
                         
                         
                         tabPanel("Ego-Réseau",
                                  sidebarLayout(
                                    #############################################################
                                    # 3. Le menu de gauche, présentant les options de l'analyse #
                                    #############################################################
                                    sidebarPanel(# 1.2. Choix de la  variable à prédire :
                                      # 1.3. Paramètres de l'analyse :
                                      div(style ="color:lightslategray", h5(strong("Paramètres réseau"))),
                                      sliderInput("voisin", "Etendu du voisinnage de l'égo-Réseau",min = 1 , max = 10, value = 2, step=1),
                                      uiOutput("select_name"),
                                      helpText("Les algorithmes de spatialisation présentés ici sont ceux fournit par le package igraph, davantage d'information est disponible dans la", strong(a("documentation du package", href ="https://igraph.org/r/doc/igraph.pdf", target="_blank")), ", sur le ", strong(a("site du package", href="https://igraph.org/r/doc/layout_nicely.html", target="_blank")), "ou encore sur ", strong(a("rdrr", href="https://rdrr.io/cran/ggraph/man/layout_tbl_graph_igraph.html", target="_blank")), ". L'algorithme stress vient du package graphlayouts"),
                                      selectizeInput('egoselect_layout', "Algorithme de spacialisation du réseau", choices = c("nicely","stress", "Fruchterman-Reingold (fr)"="fr", "Kamada-Kawai (kk)"="kk", "force directed (drl)"="drl", "Large Graph Layout (lgl)"="lgl","tree", "sugiyama", "star", "circle", "Davidson-Harels (dh)"="dh", " GEM force-directed (gem)"="gem", "graphopt", "grid", "Multidimensional scaling (mds)"="mds", "sphere", "randomly"), multiple=FALSE, selected="nicely"),
                                      selectizeInput('egoposleg', "Position de la légende", choices = c("En-bas"="bottom", "A droite"="right", "A gauche"="left", "Au-dessus"="top", "Pas de légende"="none"), multiple=FALSE, selected="bottom"),
                                      tags$hr(),
                                      div(style ="color:lightslategray", h5(strong("Paramètres noeuds"))),
                                      selectizeInput("egotypecoul", label="Définition de la couleur des noeuds", choices=list("Attributs noeuds", "Communautés réseaux"), selected=""),
                                      conditionalPanel(condition="input.egotypecoul == 'Attributs noeuds'",
                                                       uiOutput("egoselect_coul")),
                                      conditionalPanel(condition="input.egotypecoul == 'Communautés réseaux'",
                                                       helpText("Attention : certains algorithmes de détection des communautés ne fonctionnent que pour des réseaux dirigés et inversement d'autres ne fonctionnent que dans le cas de réseaux non-dirigés. Un choix non adapté entrainera un message d'erreur."),
                                                       selectizeInput("egocomm", label="Définition de l'algorithme de création de communauté", choices=list("Louvain"="cluster_louvain", "Walktrap"="cluster_walktrap", "Spinglass"="cluster_spinglass", "Leading eigen"="cluster_leading_eigen", "Edge betweenness"="cluster_edge_betweenness", "Fast greedy"="cluster_fast_greedy", "Label prop"="cluster_label_prop"), selected="")),
                                      selectizeInput('egoselect_palette', "Quelle palette de couleur", choices = c("Set3","Paired","Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG"), multiple=FALSE, selected="Set3"),
                                      helpText("Les palettes proposées, viennent de ", strong(a("RcolorBrewer", href="https://cran.r-project.org/web/packages/RColorBrewer/index.html", target="_blank"))),
                                      uiOutput("egoselect_shape"),
                                      selectizeInput("egotypesize", label="Définition de la taille des noeuds", choices=list("Taille fixe", "Indicateurs analyse de réseaux"), selected="Taille fixe"),
                                      conditionalPanel(condition="input.egotypesize == 'Indicateurs analyse de réseaux'",
                                                       selectizeInput("egosize3", label="Définition indicateur pour la taille des noeuds", choices=list("Degré"="centrality_degree", "Betweenness"="centrality_betweenness", "Closeness"="centrality_closeness"), selected=""),
                                                       sliderInput("egoslidernode2", "Intervalle pour la taille des noeuds",min = 1 , max = 50, value = c(3, 8))),
                                      tags$hr(),
                                      div(style ="color:lightslategray", h5(strong("Paramètres Labels"))),
                                      selectizeInput("egotypefont", label="Police à utiliser pour les labels", choices=list("Times", "Helvetica", "Courier", "AvantGarde", "Palatino", "Bookman"), selected="Times"),
                                      selectizeInput("egofontface", label="Caractéristique de la police", choices=list("Normal"="plain","Gras"="bold","Italique"="italic","Gras et italique"="bold.italic"), selected="plain"),
                                      selectizeInput("egotypefontsize", label="Filtrer les labels", choices=list("Pas de filtre", "Indicateurs analyse de réseaux"), selected="Pas de filtre"),
                                      conditionalPanel(condition="input.egotypefontsize == 'Indicateurs analyse de réseaux'",
                                                       selectizeInput("egofontsize3", label="Définition indicateur pour filtrer les labels", choices=list("Degré"="centrality_degree", "Betweenness"="centrality_betweenness", "Closeness"="centrality_closeness"), selected=""),
                                                       sliderInput("egosliderlabel2", "Valeur filtre",min = 0 , max = 100, value = 0, step=1)),
                                      tags$hr(),
                                      div(style ="color:lightslategray", h5(strong("Paramètres Liens"))),
                                      selectizeInput("egosensedge", label="Indiquer le sens du lien", choices=list("Ne pas indiquer", "Indiquer"), selected="Ne pas indiquer"),
                                      selectizeInput("egocouleuredges", label="Définition de la couleur des liens", choices=list("Couleur par défaut","Attributs liens","Attributs noeuds"), selected=""),
                                      conditionalPanel(condition = "input.egocouleuredges=='Attributs liens'",
                                                       uiOutput("egoselectedge_coledge")),
                                      conditionalPanel(condition = "input.egocouleuredges=='Attributs noeuds'",
                                                       uiOutput("egoselect_coledge")),
                                      uiOutput("egoselect_sizeedge"),
                                      sliderInput("egoslideredge", "Intervalle épaisseur liens",min = 0.1 , max = 10, value = c(0.2, 3), step=0.1)
                                    ),
                                    
                                    ############################################################################
                                    # 4. Le panneau principal, pour l'affichage des résultats Arbre de décision#
                                    ############################################################################
                                    mainPanel(
                                      tabPanel("Ego-Réseau",
                                               plotOutput("egoreseau", height="1000px"),
                                               radioButtons(
                                                 inputId = "filetype_egoreseau",
                                                 label = "Quel format d'image :",
                                                 inline = TRUE,
                                                 choices = list("PNG","SVG")),
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
                                      div(style ="color:lightslategray", h5(strong("Paramètres réseau"))),
                                      uiOutput("select_varsc"),
                                      uiOutput("select_modsc"),
                                      tags$hr(),
                                      helpText("Les algorithmes de spatialisation présentés ici sont ceux fournit par le package igraph, davantage d'information est disponible dans la", strong(a("documentation du package", href ="https://igraph.org/r/doc/igraph.pdf", target="_blank")), ", sur le ", strong(a("site du package", href="https://igraph.org/r/doc/layout_nicely.html", target="_blank")), "ou encore sur ", strong(a("rdrr", href="https://rdrr.io/cran/ggraph/man/layout_tbl_graph_igraph.html", target="_blank")), ". L'algorithme stress vient du package graphlayouts"),
                                      selectizeInput('sousselect_layout', "Algorithme de spacialisation du réseau", choices = c("nicely","stress", "Fruchterman-Reingold (fr)"="fr", "Kamada-Kawai (kk)"="kk", "force directed (drl)"="drl", "Large Graph Layout (lgl)"="lgl","tree", "sugiyama", "star", "circle", "Davidson-Harels (dh)"="dh", " GEM force-directed (gem)"="gem", "graphopt", "grid", "Multidimensional scaling (mds)"="mds", "sphere", "randomly"), multiple=FALSE, selected="nicely"),
                                      selectizeInput('sousposleg', "Position de la légende", choices = c("En-bas"="bottom", "A droite"="right", "A gauche"="left", "Au-dessus"="top", "Pas de légende"="none"), multiple=FALSE, selected="bottom"),
                                      div(style ="color:lightslategray", h5(strong("Paramètres noeuds"))),
                                      selectizeInput("soustypecoul", label="Définition de la couleur des noeuds", choices=list("Attributs noeuds", "Communautés réseaux"), selected=""),
                                      conditionalPanel(condition="input.soustypecoul == 'Attributs noeuds'",
                                                       uiOutput("sousselect_coul")),
                                      conditionalPanel(condition="input.soustypecoul == 'Communautés réseaux'",
                                                       helpText("Attention : certains algorithmes de détection des communautés ne fonctionnent que pour des réseaux dirigés et inversement d'autres ne fonctionnent que dans le cas de réseaux non-dirigés. Un choix non adapté entrainera un message d'erreur."),
                                                       selectizeInput("souscomm", label="Définition de l'algorithme de création de communauté", choices=list("Louvain"="cluster_louvain", "Walktrap"="cluster_walktrap", "Spinglass"="cluster_spinglass", "Leading eigen"="cluster_leading_eigen", "Edge betweenness"="cluster_edge_betweenness", "Fast greedy"="cluster_fast_greedy", "Label prop"="cluster_label_prop"), selected="")),
                                      selectizeInput('sousselect_palette', "Quelle palette de couleur", choices = c("Set3","Paired","Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG"), multiple=FALSE, selected="Set3"),
                                      helpText("Les palettes proposées, viennent de ", strong(a("RcolorBrewer", href="https://cran.r-project.org/web/packages/RColorBrewer/index.html", target="_blank"))),
                                      uiOutput("sousselect_shape"),
                                      selectizeInput("soustypesize", label="Définition de la taille des noeuds", choices=list("Taille fixe", "Indicateurs analyse de réseaux"), selected="Taille fixe"),
                                      conditionalPanel(condition="input.soustypesize == 'Indicateurs analyse de réseaux'",
                                                       selectizeInput("soussize3", label="Définition indicateur pour la taille des noeuds", choices=list("Degré"="centrality_degree", "Betweenness"="centrality_betweenness", "Closeness"="centrality_closeness"), selected=""),
                                                       sliderInput("sousslidernode2", "Intervalle taille des noeuds",min = 1 , max = 50, value = c(3, 8))),
                                      tags$hr(),
                                      div(style ="color:lightslategray", h5(strong("Paramètres Labels"))),
                                      selectizeInput("soustypefont", label="Police à utiliser pour les labels", choices=list("Times", "Helvetica", "Courier", "AvantGarde", "Palatino", "Bookman"), selected="Times"),
                                      selectizeInput("sousfontface", label="Caractéristique de la police", choices=list("Normal"="plain","Gras"="bold","Italique"="italic","Gras et italique"="bold.italic"), selected="plain"),
                                      selectizeInput("soustypefontsize", label="Filtrer les labels", choices=list("Pas de filtre", "Indicateurs analyse de réseaux"), selected="Pas de filtre"),
                                      conditionalPanel(condition="input.soustypefontsize == 'Indicateurs analyse de réseaux'",
                                                       selectizeInput("sousfontsize3", label="Définition indicateur pour filtrer les labels", choices=list("Degré"="centrality_degree", "Betweenness"="centrality_betweenness", "Closeness"="centrality_closeness"), selected=""),
                                                       sliderInput("soussliderlabel2", "Valeur filtre",min = 0 , max = 100, value = 0, step=1)),
                                      tags$hr(),
                                      div(style ="color:lightslategray", h5(strong("Paramètres des Liens"))),
                                      selectizeInput("soussensedge", label="Indiquer le sens du lien", choices=list("Ne pas indiquer", "Indiquer"), selected="Ne pas indiquer"),
                                      selectizeInput("souscouleuredges", label="Définition de la couleur des liens", choices=list("Couleur par défaut","Attributs liens","Attributs noeuds"), selected=""),
                                      conditionalPanel(condition = "input.souscouleuredges=='Attributs liens'",
                                                       uiOutput("sousselectedge_coledge")),
                                      conditionalPanel(condition = "input.souscouleuredges=='Attributs noeuds'",
                                                       uiOutput("sousselect_coledge")),
                                      uiOutput("sousselect_sizeedge"),
                                      sliderInput("sousslideredge", "Intervalle épaisseur liens",min = 0.1 , max = 10, value = c(0.2, 3), step=0.1)
                                    ),
                                    
                                    ############################################################################
                                    # 4. Le panneau principal, pour l'affichage des résultats Arbre de décision#
                                    ############################################################################
                                    mainPanel(
                                      tabPanel("Sous réseau",
                                               plotOutput("sousreseau", height="1000px"),
                                               radioButtons(
                                                 inputId = "filetype_sousreseau",
                                                 label = "Quel format d'image :",
                                                 inline = TRUE,
                                                 choices = list("PNG","SVG")),
                                               downloadButton(outputId = "downloadsousreseau", label = "Télécharger Sous-Reseau")
                                      )
                                    )
                                  )
                         ),
                         tabPanel("Sous réseau des communautés",
                                  sidebarLayout(
                                    #############################################################
                                    # 3. Le menu de gauche, présentant les options de l'analyse #
                                    #############################################################
                                    sidebarPanel(# 1.2. Choix de la  variable à prédire :
                                      # 1.3. Paramètres de l'analyse :
                                      div(style ="color:lightslategray", h5(strong("Paramètres réseau"))),
                                      helpText("Attention : certains algorithmes de détection des communautés ne fonctionnent que pour des réseau dirigés et inversement d'autres ne fonctionnent que dans le cas de réseau non-dirigés.Un choix non adapté entrainera un message d'erreur."),
                                      selectizeInput("sousreseau_comm", label="Définition de l'algorithme de création de communauté", choices=list("Louvain"="cluster_louvain", "Walktrap"="cluster_walktrap", "Spinglass"="cluster_spinglass", "Leading eigen"="cluster_leading_eigen", "Edge betweenness"="cluster_edge_betweenness", "Fast greedy"="cluster_fast_greedy", "Label prop"="cluster_label_prop"), selected="cluster_edge_betweenness"),
                                      uiOutput("select_modsc_com"),
                                      tags$hr(),
                                      helpText("Les algorithmes de spatialisation présentés ici sont ceux fournit par le package igraph, davantage d'information est disponible dans la", strong(a("documentation du package", href ="https://igraph.org/r/doc/igraph.pdf", target="_blank")), ", sur le ", strong(a("site du package", href="https://igraph.org/r/doc/layout_nicely.html", target="_blank")), "ou encore sur ", strong(a("rdrr", href="https://rdrr.io/cran/ggraph/man/layout_tbl_graph_igraph.html", target="_blank")), ". L'algorithme stress vient du package graphlayouts"),
                                      selectizeInput('comselect_layout', "Algorithm de spacialisation du réseau", choices = c("nicely","stress", "Fruchterman-Reingold (fr)"="fr", "Kamada-Kawai (kk)"="kk", "force directed (drl)"="drl", "Large Graph Layout (lgl)"="lgl","tree", "sugiyama", "star", "circle", "Davidson-Harels (dh)"="dh", " GEM force-directed (gem)"="gem", "graphopt", "grid", "Multidimensional scaling (mds)"="mds", "sphere", "randomly"), multiple=FALSE, selected="nicely"),
                                      radioButtons("comdirected", label="Votre réseau est-il dirigé ?", choices=list("Oui"="TRUE","Non"="FALSE"), selected="FALSE"),
                                      selectizeInput('composleg', "Position de la légende", choices = c("En-bas"="bottom", "A droite"="right", "A gauche"="left", "Au-dessus"="top", "Pas de légende"="none"), multiple=FALSE, selected="bottom"),
                                      div(style ="color:lightslategray", h5(strong("Paramètres des noeuds"))),
                                      selectizeInput("comtypecoul", label="Définition de la couleur des noeuds", choices=list("Attributs noeuds", "Communautés réseaux"), selected=""),
                                      conditionalPanel(condition="input.comtypecoul == 'Attributs noeuds'",
                                                       uiOutput("comselect_coul")),
                                      conditionalPanel(condition="input.comtypecoul == 'Communautés réseaux'",
                                                       helpText("Attention : certains algorithmes de détection des communautés ne fonctionnent que pour des réseaux dirigés et inversement d'autres ne fonctionnent que dans le cas de réseaux non-dirigés. Un choix non adapté entrainera un message d'erreur."),
                                                       selectizeInput("comcomm", label="Définition de l'algorithme de création de communauté", choices=list("Louvain"="cluster_louvain", "Walktrap"="cluster_walktrap", "Spinglass"="cluster_spinglass", "Leading eigen"="cluster_leading_eigen", "Edge betweenness"="cluster_edge_betweenness", "Fast greedy"="cluster_fast_greedy", "Label prop"="cluster_label_prop"), selected="")),
                                      selectizeInput('comselect_palette', "Quelle palette de couleur", choices = c("Set3","Paired","Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG"), multiple=FALSE, selected="Set3"),
                                      helpText("Les palettes proposées, viennent de ", strong(a("RcolorBrewer", href="https://cran.r-project.org/web/packages/RColorBrewer/index.html", target="_blank"))),
                                      uiOutput("comselect_shape"),
                                      selectizeInput("comtypesize", label="Définition de la taille des noeuds", choices=list("Taille fixe", "Indicateurs analyse de réseaux"), selected="Taille fixe"),
                                      conditionalPanel(condition="input.comtypesize == 'Indicateurs analyse de réseaux'",
                                                       selectizeInput("comsize3", label="Définition indicateur pour la taille des noeuds", choices=list("Degré"="centrality_degree", "Betweenness"="centrality_betweenness", "Closeness"="centrality_closeness"), selected=""),
                                                       sliderInput("comslidernode2", "Intervalle taille des noeuds",min = 1 , max = 50, value = c(3, 8))),
                                      tags$hr(),
                                      div(style ="color:lightslategray", h5(strong("Paramètres des Labels"))),
                                      selectizeInput("comtypefont", label="Police à utiliser pour les labels", choices=list("Times", "Helvetica", "Courier", "AvantGarde", "Palatino", "Bookman"), selected="Times"),
                                      selectizeInput("comfontface", label="Caractéristique de lapolice", choices=list("Normal"="plain","Gras"="bold","Italique"="italic","Gras et italique"="bold.italic"), selected="plain"),
                                      selectizeInput("comtypefontsize", label="Filtrer les labels", choices=list("Pas de filtre", "Indicateurs analyse de réseaux"), selected="Pas de filtre"),
                                      conditionalPanel(condition="input.comtypefontsize == 'Indicateurs analyse de réseaux'",
                                                       selectizeInput("comfontsize3", label="Définition indicateur pour filtrer les labels", choices=list("Degré"="centrality_degree", "Betweenness"="centrality_betweenness", "Closeness"="centrality_closeness"), selected=""),
                                                       sliderInput("comsliderlabel2", "Valeur filtre",min = 0 , max = 100, value = 0, step=1)),
                                      tags$hr(),
                                      div(style ="color:lightslategray", h5(strong("Paramètres des Liens"))),
                                      selectizeInput("comsensedge", label="Indiquer le sens du lien", choices=list("Ne pas indiquer", "Indiquer"), selected="Ne pas indiquer"),
                                      selectizeInput("comcouleuredges", label="Définition de la couleur des liens", choices=list("Couleur par défaut","Attributs liens","Attributs noeuds"), selected=""),
                                      conditionalPanel(condition = "input.comcouleuredges=='Attributs liens'",
                                                       uiOutput("comselectedge_coledge")),
                                      conditionalPanel(condition = "input.comcouleuredges=='Attributs noeuds'",
                                                       uiOutput("comselect_coledge")),
                                      uiOutput("comselect_sizeedge"),
                                      sliderInput("comslideredge", "Intervalle épaisseur liens",min = 0.1 , max = 10, value = c(0.2, 3), step=0.1)
                                    ),
                                    
                                    ############################################################################
                                    # 4. Le panneau principal, pour l'affichage des résultats Arbre de décision#
                                    ############################################################################
                                    mainPanel(
                                      tabPanel("Sous réseau des communautés",
                                               plotOutput("comreseau", height="1000px"),
                                               radioButtons(
                                                 inputId = "filetype_comreseau",
                                                 label = "Quel format d'image :",
                                                 inline = TRUE,
                                                 choices = list("PNG","SVG")),
                                               downloadButton(outputId = "downloadcomreseau", label = "Télécharger Sous-Reseau communautés")
                                      )
                                    )
                                  )
                         ),
                         tabPanel("Export noeuds",
                                  div(class='simpleDiv',
                                      tags$style(HTML(
                                        '.simpleDiv{
            background-color:#e8ecf173;
            color: #000000ff 
            }')),
                                      p("Vous pouvez télécharger votre table de noeuds enrichie de", strong("7 indices")," associés au noeuds du réseau."),
                                      p(strong("degré")," : il s'agit du nombre de fois où le noeud est à l'extrémité d'un lien"),
                                      p(strong("indegree")," : nombre de lien arrivant au noeud. Si le réseau est non-dirigé, cet indicateur est le même que le degré."),
                                      p(strong("outdegree "),": nombre de lien quittant le noeud. Si le réseau est non-dirigé, cet indicateur est le même que le degré. "),
                                      p(strong("betweenness")," : correspond au nombre de plus courts chemins du graphe passant par chaque sommet. Les noeuds avec une forte centralité d’intermédiarité sont des « points de passages importants » pour relier rapidement deux sommets du graphes."),
                                      p(strong("closeness")," : correspond à la normalisation entre 0 et 1 de l’indice, où 1 correspond à une centralité importante. Utile pour mesurer l’accessibilité dans un graphe"),
                                      p(strong("eccentricity")," : correspond au nombre de liens nécessaires pour relier le sommet le plus distant. Plus l’excentricité est importante, moins le noeud est central."),
                                      p(strong("communaute01")," : Indique la communauté d'appartenance du noeud en fonction de l'algorithme de détection choisit dans l'onglet sous réseau des communautés. Par défaut, il s'agit de l'algorithme Edge betweeness. Un choix non adapté au type de réseau (dirigé ou non-dirigé) entraînera un message d'erreur. ")),
                                  downloadButton("downloadexnode", "Télécharger indicateurs noeuds"),
                                  mainPanel(DT::dataTableOutput("table_noeuds"))),
                         
                         tabPanel("Indicateur Réseau",
                                  sidebarLayout(
                                    #############################################################
                                    # 3. Le menu de gauche, présentant les options de l'analyse #
                                    #############################################################
                                    sidebarPanel(# 1.2. Choix de la  variable à prédire :
                                      # 1.3. Paramètres de l'analyse :
                                      h4("Sélectionner Réseau"),
                                      uiOutput("select_selectr")
                                    ),
                                    
                                    ############################################################################
                                    # 4. Le panneau principal, pour l'affichage des résultats Arbre de décision#
                                    ############################################################################
                                    mainPanel(
                                      tabPanel("Indicateurs Réseau",
                                               div( style =" background-color:#e8ecf173", h4(strong("Nombre de sommets"))),
                                               verbatimTextOutput("nbsom"),
                                               div( style =" background-color:#e8ecf173", h4(strong("Nombre d'arrêtes"))),
                                               verbatimTextOutput("nbedge"),
                                               div( style =" background-color:#e8ecf173", h4(strong("Densité du réseau")),
                                                    p("La densité d’un réseau désigne le rapport entre le nombre de liens présents et le nombre de liens possibles. La densité du réseau peut varier entre 0 (graphe vide sans aucun lien) et 1 (graphe complet, l’ensemble des liens possibles entre noeuds sont présents).")),
                                               verbatimTextOutput("densi"),
                                               div( style =" background-color:#e8ecf173", h4(strong("Nombre de composantes connexes")),
                                                    p("S'il existe au moins un chemin entre toutes les paires de noeuds du réseau, alors on dit que le réseau est connexe. Si ce n'est pas le cas alors le réseau est constitué de composantes connexes"),
                                                    p("Ici vous trouverez 3 informations. 1- $membership : qui fournit l'identifiant de la composante connexe d’appartenance du noeud / 2-$csize : qui indique le nombre de sommets de chaque composante connexe / 3- $no : qui donne le nombre de composantes connexes du réseau.")),
                                               verbatimTextOutput("connexe"),
                                               div( style =" background-color:#e8ecf173", h4(strong("Diamètre du réseau")),
                                                    p("Le diamètre du réseau est la plus grande distance possible qui puisse exister entre deux noeuds du réseau. La distance entre deux noeuds étant définie par la longueur d'un plus court chemin entre ces deux neouds, le diamètre est donc le plus long des plus courts chemin.")),
                                               verbatimTextOutput("diam"),
                                               div( style =" background-color:#e8ecf173", h4(strong(" Transitivité globale du réseau")),
                                                    p("La transitivité du réseau mesure la proportion de triades fermées. Une triade est constituée par un ensemble de trois noeuds et les liens éventuels entre ces sommets. Lorsqu’un lien est présent entre chacun des 3 noeuds, on parle de triade fermée."),
                                                    p("Cet indice mesure à quel point le voisinage d’un noeud est connecté, et donc plus largement la connectivité du réseau.")),
                                               verbatimTextOutput("transi")
                                      )
                                      
                                      
                                    )
                                  )
                                  
                         )
              )
)




server <- function(input, output, session) {
  
  
  library(shiny)
  library(shinythemes)
  library(igraph)
  library(ggraph)
  library(ggiraph)
  library(tidygraph)
  library(influenceR)
  library(RColorBrewer)
  library(visNetwork)
  library(dplyr)
  library(readr)
  library(DT)
  library(tibble)
  
  sever()
  
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
  data1 <- reactive({
    file1 <- input$file1
    if(is.null(file1)){return()} 
    read.csv(input$file1$datapath,
             header = input$header,
             fileEncoding = input$encoding1,
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
                              fileEncoding = input$encoding1,
                              quote = input$quote)
    })
    
    if(input$disp == "head") {
      return(head(df()))
    }
    else {
      return(df())
    }
  })
  
  #  output$tb1 <- renderUI({
  #    tableOutput("table")
  #  })
  
  
  output$tb1 = DT::renderDataTable({
    data1()
  },extensions = c("FixedHeader", "Scroller"),  rownames = TRUE, filter = "top", class = "cell-border stripe",
  options = list(dom = "Blfrtip", searching = T, searchHighlight = TRUE,  scrollY = 400,  scroller = TRUE, scrollX = TRUE))
  
  #####################################################
  # 6. Charger les données importées et les visualiser#
  #####################################################
  data2 <- reactive({
    file2 <- input$file2
    if(is.null(file2)){return()} 
    read.csv(input$file2$datapath,
             header = input$header2,
             fileEncoding = input$encoding2,
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
                               fileEncoding = input$encoding2,
                               quote = input$quote2)
    })
    
    if(input$disp2 == "head") {
      return(head(df2()))
    }
    else {
      return(df2())
    }
  })
  
  #  output$tb2 <- renderUI({
  #    tableOutput("table2")
  #  })
  
  output$tb2 = DT::renderDataTable({
    data2()
  },extensions = c("FixedHeader", "Scroller"),  rownames = TRUE, filter = "top", class = "cell-border stripe",
  options = list(dom = "Blfrtip", searching = T, searchHighlight = TRUE,  scrollY = 400,  scroller = TRUE, scrollX = TRUE))
  
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
  
  
  output$selectedge_coledge <- renderUI({
    selectizeInput('edgecoledge', 'Définition couleur des liens', choices = c("Pas de distinction de couleur" , edge_attr_names(Mon_reseau())), selected="Pas de distinction de couleur")
  })
  
  output$select_coledge <- renderUI({
    selectizeInput('coledge', 'Définition couleur des liens', choices = c("Pas de distinction de couleur" , paste0("node.",vertex_attr_names(Mon_reseau()))), selected="Pas de distinction de couleur")
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
  
  
  No_distinction <- "No_distinction"
  
  edgecol_edge<- reactive({
    if(input$couleuredges == "Couleur par défaut")
      edgecol_edge<- "No_distinction"
    else if(input$edgecoledge == "Pas de distinction de couleur")
      edgecol_edge <- "No_distinction"
    else
      edgecol_edge <- input$edgecoledge
  })
  
  col_edge <- reactive({
    if(input$couleuredges != "Attributs noeuds")
      col_edge<- edgecol_edge()
    else if(input$coledge == "Pas de distinction de couleur")
      col_edge <- "No_distinction"
    else
      col_edge <- input$coledge
  })
  
  #  col_edge2 <- reactive({
  #    if(input$couleuredges == "Couleur par défaut")
  #      col_edge2<- "grey66"
  #    else
  #     col_edge2 <- col_edge()
  #  })
  
  
  # coledge2 <- reactive({
  #   if(input$coledge=="Couleur par défaut")
  #     coledge2 <- "grey66"
  #   else
  #     coledge2<- eval(parse(text=coledge()))
  # })
  
  No_distinction_size <- 0
  
  sizeedge <- reactive({
    if(input$sizeedge=="Pas de différence d'épaisseur")
      sizeedge <- "No_distinction_size"
    else
      sizeedge <- input$sizeedge
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
      geom_edge_link2(aes(edge_colour = get(col_edge()), edge_width = get(sizeedge())), arrow = arrowedge() ) +
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
      geom_edge_link2(aes(edge_colour = get(col_edge()), edge_width = get(sizeedge())), arrow = arrowedge() ) +
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
        dsvg(file, width=12, height=12)
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
    visNetwork(nodes = visdata()$nodes, edges = visdata()$edges, width = "100%", background = "rgba(255, 255, 255, 0.5)") %>% 
      visIgraphLayout(layout = "layout_nicely") %>%
      visOptions(highlightNearest = list(enabled = T, hover = T), nodesIdSelection = T)
  })
  
  
  ##########################################################################################'
  ##### Ego-réseau
  ##########################################################################################'
  
  EgoNet <- reactive({
    make_ego_graph(Mon_reseau(),
                   nodes = V(Mon_reseau()),
                   order = input$voisin,
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
  
  output$egoselectedge_coledge <- renderUI({
    selectizeInput('egoedgecoledge', 'Définition couleur des liens', choices = c("Pas de distinction de couleur" , edge_attr_names(Mon_reseau())), selected="Pas de distinction de couleur")
  })
  
  output$egoselect_coledge <- renderUI({
    selectizeInput('egocoledge', 'Définition couleur des liens', choices = c("Pas de distinction de couleur" , paste0("node.",vertex_attr_names(Mon_reseau()))), selected="Pas de distinction de couleur")
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
  
  egoedgecol_edge<- reactive({
    if(input$egocouleuredges == "Couleur par défaut")
      egoedgecol_edge<- "No_distinction"
    else if(input$egoedgecoledge == "Pas de distinction de couleur")
      egoedgecol_edge <- "No_distinction"
    else
      egoedgecol_edge <- input$egoedgecoledge
  })
  
  egocol_edge <- reactive({
    if(input$egocouleuredges != "Attributs noeuds")
      egocol_edge<- egoedgecol_edge()
    else if(input$egocoledge == "Pas de distinction de couleur")
      egocol_edge <- "No_distinction"
    else
      egocol_edge <- input$egocoledge
  })
  
  
  egosizeedge <- reactive({
    if(input$egosizeedge=="Pas de différence d'épaisseur")
      egosizeedge <- "No_distinction_size"
    else
      egosizeedge <- input$egosizeedge
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
      geom_edge_link2(aes(edge_colour = get(egocol_edge()), edge_width = get(egosizeedge())), arrow = egoarrowedge()) +
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
      geom_edge_link2(aes(edge_colour = get(egocol_edge()),edge_width = get(egosizeedge())), arrow = egoarrowedge()) +
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
      print(ego())
      dev.off()
    })
  
  
  ################################################################################################'
  #### Sous corpus
  ################################################################################################'
  
  
  
  
  output$select_varsc <- renderUI({
    selectizeInput('varsc', 'Variable pour la création du sous réseau', choices = c(vertex_attr_names(Mon_reseau())), selected="")
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
  
  output$sousselectedge_coledge <- renderUI({
    selectizeInput('sousedgecoledge', 'Définition couleur des liens', choices = c("Pas de distinction de couleur" , edge_attr_names(Mon_reseau())), selected="Pas de distinction de couleur")
  })
  
  output$sousselect_coledge <- renderUI({
    selectizeInput('souscoledge', 'Définition couleur des liens', choices = c("Pas de distinction de couleur" , paste0("node.",vertex_attr_names(Mon_reseau()))), selected="Pas de distinction de couleur")
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
  
  sousedgecol_edge<- reactive({
    if(input$souscouleuredges == "Couleur par défaut")
      sousedgecol_edge<- "No_distinction"
    else if(input$sousedgecoledge == "Pas de distinction de couleur")
      sousedgecol_edge <- "No_distinction"
    else
      sousedgecol_edge <- input$sousedgecoledge
  })
  
  souscol_edge <- reactive({
    if(input$souscouleuredges != "Attributs noeuds")
      souscol_edge<- sousedgecol_edge()
    else if(input$souscoledge == "Pas de distinction de couleur")
      souscol_edge <- "No_distinction"
    else
      souscol_edge <- input$souscoledge
  })
  
  
  soussizeedge <- reactive({
    if(input$soussizeedge=="Pas de différence d'épaisseur")
      soussizeedge <- "No_distinction_size"
    else
      soussizeedge <- input$soussizeedge
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
      geom_edge_link2(aes(edge_colour = get(souscol_edge()),edge_width = get(soussizeedge())), arrow = sousarrowedge()) +
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
      geom_edge_link2(aes(edge_colour = get(souscol_edge()),edge_width = get(soussizeedge())), arrow = sousarrowedge()) +
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
      print(sousnet2())
      dev.off()
    })
  
  
  ################################################################################################'
  #### Sous communauté
  ################################################################################################'
  
  communaute <- reactive({
    communaute <- get(input$sousreseau_comm)(Mon_reseau(), weights=NULL)
  })
  
  lien_com <- reactive({
    lien_com <- dat()
  })
  
  node_com <- reactive({
    nodecom <- dat2()
    
    membership <- as.data.frame(communaute()$membership, optional=TRUE)
    
    node_com <- cbind(nodecom, membership)
    #node_com$membership <- communaute()$membership
  })
  
  member <- reactive({
    graph_from_data_frame(lien_com(), 
                          vertices = node_com(),
                          directed = input$comdirected) 
  })
  
  
  
  output$select_modsc_com <- renderUI({
    selectizeInput('modsc_com', 'Variable pour la création du sous réseau', choices = communaute()$membership, selected="")
  }) 
  
  
  
  comnet <- reactive({induced_subgraph(member(), 
                                       v = which(vertex_attr(member())$membership == c(input$modsc_com)))
  })
  
  
  #######
  output$comselect_coul <- renderUI({
    selectizeInput('comselectcoul', 'Variable pour la couleur des noeuds', choices = c("Pas de distinction de couleur" ,  vertex_attr_names(comnet())), selected="Pas de distinction de couleur")
  })
  
  output$comselect_shape <- renderUI({
    selectizeInput('comselectshape', 'Variable pour la forme des noeuds', choices = c("Pas de distinction forme" , vertex_attr_names(comnet())), selected="Pas de distinction forme")
  })
  
  output$comselect_sizeedge <- renderUI({
    selectizeInput('comsizeedge', 'Définition variable pour epaisseur des liens', choices = c("Pas de différence d'épaisseur" ,   edge_attr_names(comnet())))
  }) 
  
  output$comselectedge_coledge <- renderUI({
    selectizeInput('comedgecoledge', 'Définition couleur des liens', choices = c("Pas de distinction de couleur" , edge_attr_names(Mon_reseau())), selected="Pas de distinction de couleur")
  })
  
  output$comselect_coledge <- renderUI({
    selectizeInput('comcoledge', 'Définition couleur des liens', choices = c("Pas de distinction de couleur" , paste0("node.",vertex_attr_names(Mon_reseau()))), selected="Pas de distinction de couleur")
  })
  
  ##########
  
  coml2 <- reactive({
    coml2 <- get(input$comselect_layout)(comnet())
  })
  
  ##
  comcolrs <-  reactive({
    #col <- brewer.pal(12, input$select_palette)
    if(input$comselectcoul=="Pas de distinction de couleur")
      comcolrs <- "Pas de couleurs"
    else
      comcolrs <- as.factor(vertex_attr(comnet())[[input$comselectcoul]])
  })
  
  comcolrs2 <- reactive({
    comcolrs2 <- as.character(membership(get(input$comcomm)(comnet())))
  })
  
  
  comcolrs4 <- reactive({
    if(input$comtypecoul == "Attributs noeuds")
      comcolrs4<- comcolrs()
    else
      comcolrs4 <- comcolrs2()
  })
  
  
  ###
  
  comshp <- reactive({
    if(input$comselectshape=="Pas de distinction forme")
      sousshp <- "Pas de forme"
    else
      comshp <- as.factor(vertex_attr(comnet())[[input$comselectshape]])
  })
  
  ##
  
  comsize2 <- reactive({
    comsize2 <- V(comnet())$size
    if(input$comtypesize == "Taille fixe")
      comsize2 <- 5
    else
      comsize2 <- get(input$comsize3)(normalized = TRUE)
  })
  
  ##
  
  comecart <- reactive({
    if(input$comtypesize == "Taille fixe")
      comecart <- 3
    else
      comecart <- input$comslidernode2
  })
  
  ##
  comsize3 <- reactive({
    comsize3 <- V(comnet())$size
    if(input$comtypefontsize == "Pas de filtre")
      comsize3 <- 5
    else
      comsize3 <- get(input$comfontsize3)()
  })
  
  ##
  comecart2 <- reactive({
    if(input$comtypefontsize == "Pas de filtre")
      comecart2 <- 3
    else
      comecart2 <- input$comsliderlabel2
  })
  
  ##
  
  comedgecol_edge<- reactive({
    if(input$comcouleuredges == "Couleur par défaut")
      comedgecol_edge<- "No_distinction"
    else if(input$comedgecoledge == "Pas de distinction de couleur")
      comedgecol_edge <- "No_distinction"
    else
      comedgecol_edge <- input$comedgecoledge
  })
  
  comcol_edge <- reactive({
    if(input$comcouleuredges != "Attributs noeuds")
      comcol_edge<- comedgecol_edge()
    else if(input$comcoledge == "Pas de distinction de couleur")
      comcol_edge <- "No_distinction"
    else
      comcol_edge <- input$comcoledge
  })
  
  
  comsizeedge <- reactive({
    if(input$comsizeedge=="Pas de différence d'épaisseur")
      comsizeedge <- "No_distinction_size"
    else
      comsizeedge <- input$comsizeedge
  })
  ##
  comarrowedge <- reactive({
    if (input$comsensedge == "Ne pas indiquer")
      comarrowedge <- NULL
    else
      comarrowedge <- arrow(type = "closed", length = unit(2, 'mm'))
  })
  
  ##
  
  
  output$comreseau <- renderPlot({ 
    # plot(comnet())
    ggraph(comnet(), layout = input$comselect_layout) + 
      geom_edge_link2(aes(edge_colour = get(comcol_edge()),edge_width = get(comsizeedge())), arrow = comarrowedge()) +
      geom_node_point(aes(shape = comshp(), colour = comcolrs4(), size=comsize2())) +
      geom_node_text(aes(filter = comsize3() >= comecart2(),label = V(comnet())$name), family= input$comtypefont, fontface=input$comfontface, repel=TRUE)+
      scale_color_brewer(palette = input$comselect_palette)+
      scale_size_continuous(range = comecart())+
      scale_edge_width_continuous(range = input$comslideredge)+
      theme_graph()+
      theme(legend.position = input$composleg)
  })
  
  
  com_network <- reactive({
    ggraph(comnet(), layout = input$comselect_layout) + 
      geom_edge_link2(aes(edge_colour = get(comcol_edge()),edge_width = get(comsizeedge())), arrow = comarrowedge()) +
      geom_node_point(aes(shape = comshp(), colour = comcolrs4(), size=comsize2())) +
      geom_node_text(aes(filter = comsize3() >= comecart2(),label = V(comnet())$name), family= input$comtypefont, fontface=input$comfontface, repel=TRUE)+
      scale_color_brewer(palette = input$comselect_palette)+
      scale_size_continuous(range = comecart())+
      scale_edge_width_continuous(range = input$comslideredge)+
      theme_graph()+
      theme(legend.position = input$composleg)
  })
  
  output$downloadcomreseau <- downloadHandler(
    filename = function(){
      paste("Mon_sous-reseau_communaute", tolower(input$filetype_comreseau), sep =".")
    }, 
    content = function(file){
      width  <- 900
      height <- 900
      pixelratio <- 2
      if(input$filetype_comreseau == "PNG")
        png(file, width=width*pixelratio, height=height*pixelratio,
            res=72*pixelratio, units = "px")
      else if(input$filetype_comreseau == "SVG")
        svg(file, width=12, height=12)
      print(com_network())
      dev.off()
    })
  
  
  #######################################################################################'
  ## export noeuds avec indicateurs
  #######################################################################################'
  
  
  nodes <- reactive({
    
    Monreseau <- Mon_reseau()
    
    
    # Les valeurs des centralités de degré sont insérées dans la variable 'degre'
    degree  <-  degree(Monreseau)
    indegree <- degree(Monreseau, mode="in")
    outdegree <- degree(Monreseau, mode="out")
    # Centralité d'intermediarité dans la variable 'intermed'
    betweenness  <-  betweenness(Monreseau)
    
    # Centralité de proximité dans la variable 'proximit'
    closeness  <-  closeness(Monreseau)
    
    eccentricity <- eccentricity(Monreseau)
    #Sommet_de_mon_reseau <- as_data_frame(Mon_reseau(), what="vertices")
    communaute01 <- communaute()$membership
    #modularite <- communaute()$modularity
    nodes <- dat2()
    
    cbind(nodes, degree, indegree, outdegree, betweenness, closeness, eccentricity, communaute01)
  })
  
  
  nodes2 <- reactive({
    nodes2 <- as.data.frame(nodes())
  })
  
  
  output$table_noeuds = DT::renderDataTable({
    nodes2()
  },extensions = c("FixedHeader", "Scroller"),  rownames = TRUE, filter = "top", class = "cell-border stripe",
  options = list(dom = "Blfrtip", searching = T, searchHighlight = TRUE,  scrollY = 400,  scroller = TRUE, scrollX = TRUE))
  
  
  output$downloadexnode <- downloadHandler(
    filename = function() {
      paste("export_nodes", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(nodes2(), file, row.names = TRUE)
    }
  )
  
  
  ##############'
  ###### Indicateur réseau
  ##############'
  
  output$select_selectr <- renderUI({
    
    selectizeInput("selectr", label="Réseau à selectionner", choices=c("Réseau global"= "Mon_reseau", "Ego-réseau"="monego", "Sous-réseau"="sousnet", "Sous-reseau communauté"= "comnet"), selected="")
    
  })
  
  rr <- reactive({
    get(input$selectr)()
  })
  
  nombre_noeuds<-reactive({
    gorder(rr())
  })
  
  nombre_liens <-reactive({
    gsize(rr())
  })
  
  densite_r<-reactive({
    graph.density(rr())
  })
  
  composante_r <-reactive({
    components(rr())
  })
  
  diametre <-reactive({
    diameter(rr(), directed = input$directed, weights = NULL)
  })
  
  transitiviti<-reactive({
    transitivity(rr(), type = "global")
  })
  
  output$nbsom <-renderPrint({
    nombre_noeuds()
  })
  
  output$nbedge <-renderPrint({
    nombre_liens()
  })
  
  output$densi <-renderPrint({
    densite_r()
  })
  
  output$connexe <-renderPrint({
    composante_r()
  })
  
  output$diam <-renderPrint({
    diametre()
  })
  
  output$transi <-renderPrint({
    transitiviti()
  })
  #################'
  ####### Mumuse
  #################'
  
  
  #  output$Mumuse <- renderVisNetwork({
  #    visNetwork(nodes = visdata()$nodes, edges = visdata()$edges)
  #  })
  
  
  
}



shinyApp(ui=ui,server=server)


