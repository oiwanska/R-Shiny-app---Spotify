library(readr)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(shiny)
library(ggthemes)
library(DT)

data <- read_csv("data.csv")

data <- data %>% 
  mutate(duration_ms = round(duration_ms/60000, 3)) 

#data$name <- gsub(pattern = "'", replacement = "", x = data$name) 
  

axis_vars <- c("Data produkcji" = "year", "Czas trwania piosenki [min]" = "duration_ms",
               "Poziom energii [0-1]" = "energy", "Poziom głośności w decybelach (-60,0) [dB]" = "loudness",
               "Popularność piosenki w ostatnim czasie [0-100]" = "popularity", 
               "Tempo (uderzenie na minutę) [BPM]" = "tempo", "Poziom akustyczności [0-1]" = "acousticness",
               "Nastrój piosenki [0 - smutna - 1 - radosna]" = "valence", 
               "Relatywna zawartość głosu ludzkiego [0-1]" = "speechiness", 
               "Czy piosenka jest łatwa do tańca? [0-1]" = "danceability")

#Zmiana jezyka tabeli na polski
pol <- list(
  sProcessing = "Przetwarzanie...",
  sSearch = "Szukaj:",
  sLengthMenu = "Pokaż _MENU_ pozycji",
  sInfo = "Pozycje od _START_ do _END_ z _TOTAL_ łącznie",
  sInfoEmpty = "Pozycji 0 z 0 dostępnych",
  sInfoFiltered = "(filtrowanie spośród _MAX_ dostępnych pozycji)",
  sInfoPostFix =  "",
  sLoadingRecords = "Wczytywanie...",
  sZeroRecords = "Nie znaleziono pasujących pozycji",
  sEmptyTable = "Brak danych",
  oPaginate = list(
    sFirst =  "Pierwsza",
    sPrevious = "Poprzednia",
    sNext = "Następna", 
    sLast = "Ostatnia"
  ),
  oAria = list(
    sortAscending = ": aktywuj, by posortować kolumnę rosnąco",
    sortDescending = ": aktywuj, by posortować kolumnę malejąco"
  )
)


ui <- fluidPage(
  
  navbarPage( img(weidght = 40, height = 40,src="https://storage.googleapis.com/pr-newsroom-wp/1/2018/11/Spotify_Logo_CMYK_Black.png"),
########################## PIERWSZA STRONA ###################################### 
    tabPanel(h4(strong("Informacje")),
             sidebarLayout(
               sidebarPanel(
                 h2("Co to jest?"),
                 p("Aplikacja, która pozwala na ", em("interaktywny "), 
                   "podgląd statystyk ulubionych piosenek, porównania, przyjrzenia się trendom oraz korelacjom. ",
                   strong("Obserwuj, analizuj"), " na podstawie znanych utworów. Dostępnych jest prawie ", 
                   strong("170 tysięcy"), " piosenek wydanych od 1921 roku!"),
                 br(),
                 p("Aplikacja powstała za pomocą pakietu", span(strong("Shiny"), style = "color:blue"), 
                   "który jest produktem", span(strong("RStudio."), style = "color:blue")),
                 p("Więcej informacji o Shiny możesz znaleźć na ", a("stronie domowej Shiny."),
                     href = ("http://shiny.rstudio.com"))
               ),
               mainPanel(
                 h2("Jak to zrobić?"),
                 p(h4('1. Przejdź do zakładki "Wykres"')),
                 p(h4('2. Wybierz interesujące cię statystyki i sprawdź co pokaże się na wykresie. ')),
                 br(),
                 p(h4(em(strong("Przykłady:")))),
                 br(),
                 p(h4(em("1. Możesz ograniczyć piosenki do określonego wykonawcy lub tytułu oraz wybrać zmienne, 
                      które pojawią się  na wykresie."))),
                 img(height = 450,src="spotify 1.png"),
                 p(hr(h4(em("2. Możesz zaznaczyć piosenki i sprawdzić ich statystyki.")))),
                 img(height = 600, src="spotify 2.png"),
                 p(hr(h4(em("3. Możesz dodać linię regresji do wykresu oraz sprawdzić współczynnik korelacji.")))),
                 img(height = 200, src="spotify 3.png"),
                 p(hr(h4(em("4. W dodatkowej zakładce możesz sprawdzić rozkład zmiennych na osiach X i Y.")))),
                 img(height = 600, src="spotify 4.png")
                )
              )
             ),              
########################## DRUGA STRONA ######################################              
    tabPanel( h4(strong("Wykres")),
      fluidRow(
        column(3, 
          wellPanel(
            h4(strong("Wybierz:")),
            helpText("Pokaż tylko piosenki konkretnego wykonwacy ",
                     "lub o podanym tytule."),
            textInput(inputId = "nazwa",
                    label = "Tytuł piosenki:"), 
            textInput(inputId = "wykonawca",
                      label = "Wykonawca:"),
            h4(strong(hr("Osie XY"))),
            selectInput(inputId = "x",
                        label = "Oś X:", choices = axis_vars, selected = "danceability"),
            helpText(textOutput("x_expl")),
            br(),
            selectInput(inputId = "y",
                        label = "Oś Y:", choices = axis_vars, selected = "energy"),
            helpText(textOutput("y_expl"))
            ),
          wellPanel(radioButtons(inputId = "linia", label = "Linia regresji",
                       choices = c("Brak linii regresji" = "brak",
                                   "Dodaj linię regresji" = "linia")),
                    p(hr(h5(strong("Korelacja:", br(), p(verbatimTextOutput("kor")))))))
        ),
        column(6,
          actionButton("reset", label = "Odśwież"),
          ggiraph::girafeOutput(outputId = "plot"),
          h4(hr(strong("Wybrane piosenki:"))),
          br(),
          DT::dataTableOutput("datatab")
               ),
        column(3,
          wellPanel( fluidPage( tabsetPanel(
            tabPanel(strong("Ogranicz dane"),
            h4(strong("Ogranicz dane:")),
            sliderInput(inputId = "rok",
                        label = "Rok produkcji:",
                        value = c(1921,2020), min = 1921, max = 2020, step = 1),
            sliderInput(inputId = "czas", 
                      label = "Czas trwania piosenki [min]:", 
                      value = c(1,80), min = 0, max = 91, step = 0.001),
            sliderInput(inputId = "energia", 
                      label = "Poziom energii [0-1]:", 
                      value = c(0.1,0.99), min = 0, max = 1, step = 0.001),
            sliderInput(inputId = "glosnosc", 
                      label = "Poziom głośności w decybelach (-60,0) [dB]:", 
                      value = c(-60,-12), min = -60, max = 0, step = 0.001),
            sliderInput(inputId = "popularnosc", 
                      label = "Popularność piosenki w ostatnim czasie [0-100]:", 
                      value = c(5,100), min = 0, max = 100, step = 1),
            sliderInput(inputId = "tempo1", 
                      label = "Tempo (uderzenie na minute) [BPM]:", 
                      value = c(50,100), min = 0, max = 250, step = 1),
            sliderInput(inputId = "akustyk",  
                        label = "Poziom akustyczności [0-1]:", 
                        value = c(0.15,0.9), min = 0, max = 1, step = 0.001),
            sliderInput(inputId = "nastroj",  
                        label = "Nastrój piosenki [0 - smutna - 1 - radosna]:", 
                        value = c(0,1), min = 0, max = 1, step = 0.001),
            sliderInput(inputId = "glos",  
                        label = "Relatywna zawartość głosu ludzkiego [0-1]", 
                        value = c(0,0.75), min = 0, max = 1, step = 0.001),
            sliderInput(inputId = "taniec", 
                        label = "Czy piosenka jest łatwa do tańca? [0-1]:", 
                        value = c(0.15,0.9), min = 0, max = 1, step = 0.001)
            ),
            tabPanel( strong("Statystyki"),
              helpText("Rozkłady zmiennych OX i OY"),
              plotOutput("histOX"),
              plotOutput("histOY")
            )))
          )
        )
      )
    )
  )
)

server <- function(input, output, session){
  
  data_selected <- reactive({     ###### zapisanie wybranych obserwacji przez usera
      
      minrok <- input$rok[1]
      maxrok <- input$rok[2]
      minczas <- input$czas[1] 
      maxczas <- input$czas[2]
      minenergia <- input$energia[1] 
      maxenergia <- input$energia[2]
      minglosnosc <- input$glosnosc[1]
      maxglosnosc <- input$glosnosc[2]
      minpopularnosc <- input$popularnosc[1] 
      maxpopularnosc <- input$popularnosc[2]
      mintempo <- input$tempo1[1] 
      maxtempo <- input$tempo1[2]
      minakustyk <- input$akustyk[1] 
      maxakustyk <- input$akustyk[2]
      minnastroj <- input$nastroj[1] 
      maxnastroj <- input$nastroj[2]
      minglos <- input$glos[1] 
      maxglos <- input$glos[2]
      mintaniec <- input$taniec[1] 
      maxtaniec <- input$taniec[2]

  
  d <- data %>% 
    filter(
      duration_ms >= minczas,
      duration_ms <= maxczas,
      energy >= minenergia,
      energy <= maxenergia,
      loudness >= minglosnosc,
      loudness <= maxglosnosc,
      popularity >= minpopularnosc,
      popularity <= maxpopularnosc,
      tempo >= mintempo,
      tempo <= maxtempo,
      acousticness >= minakustyk,
      acousticness <= maxakustyk,
      valence >= minnastroj,
      valence <= maxnastroj,
      speechiness >= minglos,
      speechiness <= maxglos,
      danceability >= mintaniec,
      danceability <= maxtaniec,
      year >= minrok,
      year <= maxrok
    ) 
  
  # wyswietlenie wybranych piosenek po tytule / wykonawcy
  d1 <- if(isTruthy(input$nazwa)) 
    d %>% filter(Reduce(`&`, lapply(strsplit(input$nazwa,' ')[[1]], grepl, name,ignore.case=T))) else d
  
  d2 <- if(isTruthy(input$wykonawca)) 
    d1 %>% filter(Reduce(`&`, lapply(strsplit(input$wykonawca,' ')[[1]], grepl, artists,ignore.case=T))) else d1

  })


  # zbudowanie tabeli
  tabela_selected <- reactive({
    tabela <- data %>% 
      select(name, artists, year, input$x, input$y, id) %>% 
      filter(id %in% selected_song()) %>% 
      select(name, artists, year, input$x, input$y) %>% 
      setNames(.,c("Tytuł", "Wykonawca", "Rok produkcji", names(axis_vars)[axis_vars == input$x], 
                   names(axis_vars)[axis_vars == input$y]))
      
  })
  
  #zbudowanie wykresu
  gg_songs <- reactive({
     ggplot(data = data_selected(), aes_string(x = input$x, y = input$y)) +
      labs(x = names(axis_vars)[axis_vars == input$x],
           y = names(axis_vars)[axis_vars == input$y]) +
      geom_point_interactive(aes(data_id = id, tooltip = name), size = 2, alpha = 1/3, color = "steelblue4" ) +
      theme(axis.title = element_text(face = "bold", size = 11),
            axis.text.y = element_text(angle = 90,hjust = 1, vjust = 0),
            axis.line = element_line(colour = "black", linetype = "solid", size = 0.5)
      )
  })
  
  # dodanie linii regresji i animacja wykresu
  output$plot <- renderGirafe({
    gg_wykresselected <- switch(input$linia,
                   brak = gg_songs(),
                   linia = gg_songs() + geom_smooth(color = "dodgerblue4"),
                   gg_songs())
    
    wykres <- girafe(code = print(gg_wykresselected),
                options = list(
                  opts_selection(type = "multiple", css = "fill:#FF3333;stroke:yellow;"),
                  opts_hover(css = "fill:#FF3333;stroke:yellow;cursor:pointer;")
                ))
    wykres
    
  })
  
  
  # zapisanie piosenek oznaczonych na wykresie 
  selected_song <- reactive({
    input$plot_selected
  })
  
  # reset wyboru na wykresie
  observeEvent(input$reset, {
    session$sendCustomMessage(type = 'plot_set', message = character(0))
  })
  
  # wstawienie tablicy
  output$datatab <- DT::renderDataTable({
    out <- DT::datatable(tabela_selected(), options = list(orderClasses = TRUE, language = pol))
    out
  })
  
  # wstawienie wykresow pobocznych dodatkowych
  output$histOX <- renderPlot({
    ggplot(data=data_selected(), aes_string(input$x)) + 
      geom_histogram(alpha = 1/7, color = "maroon4") +
      labs(x = names(axis_vars)[axis_vars == input$x],
           y = "Liczba") +
      theme(axis.title = element_text(face = "bold", size = 8),
            axis.text.y = element_text(angle = 90,hjust = 1, vjust = 0),
            axis.line = element_line(colour = "black", linetype = "solid", size = 0.25))
  })
  
  output$histOY <- renderPlot({
    ggplot(data=data_selected(), aes_string(input$y)) + 
      geom_histogram(alpha = 1/7, color = "maroon4") +
      labs(x = names(axis_vars)[axis_vars == input$y],
                                y = "Liczba") +
      theme(axis.title = element_text(face = "bold", size = 8),
            axis.text.y = element_text(angle = 90,hjust = 1, vjust = 0),
            axis.line = element_line(colour = "black", linetype = "solid", size = 0.25))
  })
  
  #### Dodanie korelacji
  
  kor_selected <- reactive({
    x <- data_selected() %>% 
      select(input$x)
    y <- data_selected() %>% 
      select(input$y)
    cor(x, y)
  })
    
  output$kor <- renderText({
    kor_selected()
  })
  
  ### Dodanie opisow osi
  
  x_explenation <- reactive({
    if (input$x == "year") {
      print("Rok, w którym utwór został wyprodukowany.")
    } else if (input$x == "duration_ms") {
      print("Czas trwania utworu w minutach.")
    } else if (input$x == "energy") {
      print("Miara energii utworu od 0 do 1. Reprezentuje intensywność 
            i aktywność piosenki. Energetyczne utwory są określone jako
            szybkie, głośne i hałaśliwe, np. death metal jest bliski 1, 
            a preludium Bacha bliższy 0. Inne cechy brane pod uwagę przy 
            wyliczeniu poziomu energii to dynamika, głośność, barwa, 
            częstotliwość początkową i ogólna entropia.")
    } else if (input$x == "loudness") {
      print("Poziom głośności w decybelach w skali od -60 do 0.")
    } else if (input$x == "popularity") {
      print("Popularność piosenki w ostatnim czasie oceniona przez Spotify w skali od 0 do 100.")
    } else if (input$x == "tempo") {
      print("Liczba uderzeń na minutę. W języku angielskim 
            funkcjonuje skrót BPM (beats per minute). 
            Tempo wynoszące 60 BPM oznacza jedno uderzenie na sekundę, czyli 1 Hz.")
    } else if (input$x == "acousticness") {
      print("Wartość, która określa to w jakim stopniu utwór jest akustyczny. 
            Im bliżej 1 tym piosenka jest w większym stopniu wykonana przy użyciu
            instrumentów akustycznych, tj. takich które wytwarzają dźwięk przy użyciu 
            wibratora, a nie poprzez syntezę elektroniczną.")
    } else if (input$x == "valence") {
      print("Miara od 0 do 1, która opisuje czy utwór ma pozytywny charakter. 
            Im bliżej 1 tym piosenki brzmią bardziej radośnie, pogodnie, 
            motywująco, im bliżej 0 tym brzmienie jest smutniejsze, depresyjne, złowrogie. ")
    } else if (input$x == "speechiness") {
      print("Zmienna, która wykrywa obecność mowy w utworze. Im wskaźnik jest bliższy 1, tym 
            piosenka zawiera więcej tekstu. ")
    } else if (input$x == "danceability") {
      print("Miara, która określa jak dany utwór pasuje do tańca bazując na tempie, 
            rytmie i ogólnych cecha muzycznych. Im bliżej 1 tym piosenka jest bardziej tanczena.")
    }
    
  })
  
  y_explenation <- reactive({
    if (input$y == "year") {
      print("Rok, w którym utwór został wyprodukowany.")
    } else if (input$y == "duration_ms") {
      print("Czas trwania utworu w minutach.")
    } else if (input$y == "energy") {
      print("Miara energii utworu od 0 do 1. Reprezentuje intensywność 
            i aktywność piosenki. Energetyczne utwory są określone jako
            szybkie, głośne i hałaśliwe, np. death metal jest bliski 1, 
            a preludium Bacha bliższy 0. Inne cechy brane pod uwagę przy 
            wyliczeniu poziomu energii to dynamika, głośność, barwa, 
            częstotliwość początkową i ogólna entropia.")
    } else if (input$y == "loudness") {
      print("Poziom głośności w decybelach w skali od -60 do 0.")
    } else if (input$y == "popularity") {
      print("Popularność piosenki w ostatnim czasie oceniona przez Spotify w skali od 0 do 100.")
    } else if (input$y == "tempo") {
      print("Liczba uderzeń na minutę. W języku angielskim 
            funkcjonuje skrót BPM (beats per minute). 
            Tempo wynoszące 60 BPM oznacza jedno uderzenie na sekundę, czyli 1 Hz.")
    } else if (input$y == "acousticness") {
      print("Wartość, która określa to w jakim stopniu utwór jest akustyczny. 
            Im bliżej 1 tym piosenka jest w większym stopniu wykonana przy użyciu
            instrumentów akustycznych, tj. takich które wytwarzają dźwięk przy użyciu 
            wibratora, a nie poprzez syntezę elektroniczną.")
    } else if (input$y == "valence") {
      print("Miara od 0 do 1, która opisuje czy utwór ma pozytywny charakter. 
            Im bliżej 1 tym piosenki brzmią bardziej radośnie, pogodnie, 
            motywująco, im bliżej 0 tym brzmienie jest smutniejsze, depresyjne, złowrogie. ")
    } else if (input$y == "speechiness") {
      print("Zmienna, która wykrywa obecność mowy w utworze. Im wskaźnik jest bliższy 1, tym 
            piosenka zawiera więcej tekstu. ")
    } else if (input$y == "danceability") {
      print("Miara, która określa jak dany utwór pasuje do tańca bazując na tempie, 
            rytmie i ogólnych cecha muzycznych. Im bliżej 1 tym piosenka jest bardziej taneczna.")
    }
  })
  
  output$x_expl <- renderText({
    x_explenation()
  })
  
  output$y_expl <- renderText({
    y_explenation()
  })
  
}

shinyApp(ui = ui, server = server)


