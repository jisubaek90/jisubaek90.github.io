
server <- function(input, output, session) {

  # reactive values----
  v <- reactiveValues(
    id = NULL, # selected songs
    i = 1, # playlist index of song
    n = 10, # number of songs in playlist
    ok = FALSE # if correct number of songs selected from genres
  )
  
  # generate playlist----
  observeEvent(
    eventExpr = input$run,
    {
      if (as.numeric(input$genre1) + 
          as.numeric(input$genre2) +
          as.numeric(input$genre3) +
          as.numeric(input$genre4) == v$n) {
        v$id <- dbGetQuery(
          conn = con,
          statement = paste0(
            '(SELECT * FROM songs WHERE genre = \'Dance\' ORDER BY RANDOM() LIMIT \'', input$genre1, '\')',
            'UNION',
            '(SELECT * FROM songs WHERE genre = \'K-Pop\' ORDER BY RANDOM() LIMIT \'', input$genre2, '\')',
            'UNION',
            '(SELECT * FROM songs WHERE genre = \'Pop\' ORDER BY RANDOM() LIMIT \'', input$genre3, '\')',
            'UNION',
            '(SELECT * FROM songs WHERE genre = \'Rock\' ORDER BY RANDOM() LIMIT \'', input$genre4, '\')',
            'ORDER BY yt_views DESC;' # <<Need to modify this query
          )
        )
        v$ok = TRUE
      } else {
        v$ok = FALSE
        sendSweetAlert(
          session = session,
          title = 'Warning !!!',
          text = 'You must select exactly 10 songs before generating the playlist.',
          type = "warning"
        )
      }
    }
  )
  
  # go backward in playlist----
  observeEvent(
    eventExpr = input$back,
    if (v$i > 1) {
      v$i <- v$i - 1
    } else {
      v$i <- v$n
    }
  )
      
      # <<Need to fill in logic here

  
  # go forward in playlist----
  observeEvent(
    eventExpr = input$forw,
    if (v$i < v$n) {
      v$i <- v$i + 1
    } else {
      v$i <- 1
    }
  )
      # <<Need to fill in logic here

  
  
  # show song info----
  output$info <- renderUI(
    if (v$ok) {
      div(
        align = 'center',
        h3(
          paste0( v$id$song_name[v$i], ' by ',
                  v$id$artist[v$i],
                  '(', format(round(v$id$yt_views[v$i]/1000000,-1), trim = T, big.mark = ","),
                  'M',' ','Views',')'
            # <<Need to fill in logic here # song name & view
          )
        ),
        fluidRow(
          column(
            width = 4,
            align = 'left',
            radioGroupButtons(
              inputId = 'genre',
              choices = v$id$genre[v$i], # <<Need to modify genre ???
              status = 'info'
            )
          ),
          column(
            width = 4,
            align = 'center',
            actionBttn(
              inputId = 'back',
              label = '<',
              color = 'royal'
            ),
            HTML('&nbsp;&nbsp;&nbsp;&nbsp;'),
            actionBttn(
              inputId = 'forw',
              label = '>',
              color = 'royal'
            )
          ),
          column(
            width = 4,
            align = 'right',
            h4(
              paste0('song', ' ',v$i, ' ','out of', ' ', v$n 
                # <<Need to fill in logic here song 3/10
              )
            )
          )
        )
      )
    }
  )
  
  # show embedded video----
  output$vid <- renderUI(
    if (v$ok) {
      
      tags$iframe(
        height = '500',
        width = '100%',
        src = paste0(
          'https://www.youtube.com/embed/',
          v$id$ytv_id[v$i]
        ),
        frameborder = '1',
        autoplay = 1,
        allow = 'accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope;'
      )
    }
  )
  
}

