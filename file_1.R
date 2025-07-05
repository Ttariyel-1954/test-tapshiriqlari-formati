# Test Tapşırıqlarının Tərtibi Formatları - Tam Shiny Demo Tətbiqi

# Lazım olan paketlər
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(shinyWidgets)
library(shinycssloaders)
library(ggplot2)
library(dplyr)

# Test sualları məlumat bazası
test_questions <- list(
  
  # 1. Multiple Choice (Çoxlu Seçim)
  multiple_choice = list(
    question_id = 1,
    title = "🔘 Çoxlu Seçim Sualı",
    question_text = "2x + 8 = 20 tənliyinin həlli nədir?",
    question_type = "multiple_choice",
    options = c(
      "x = 4" = "A",
      "x = 6" = "B", 
      "x = 8" = "C",
      "x = 12" = "D"
    ),
    correct_answer = "B",
    difficulty_level = 2,
    topic = "Tənliklər",
    time_limit = 60,
    explanation = "2x + 8 = 20 → 2x = 12 → x = 6",
    usage_percent = 45
  ),
  
  # 2. Multiple Response (Çoxlu Cavab)
  multiple_response = list(
    question_id = 2,
    title = "☑️ Çoxlu Cavab Sualı",
    question_text = "Aşağıdakı hansı ədədlər irrational ədədlərdir? (Birdən çox seçim mümkündür)",
    question_type = "multiple_response",
    options = c(
      "√2" = "A",
      "π (pi)" = "B",
      "3.14" = "C",
      "√9" = "D",
      "e (Euler ədədi)" = "E"
    ),
    correct_answers = c("A", "B", "E"),
    difficulty_level = 3,
    topic = "Ədədlər",
    time_limit = 90,
    explanation = "√2, π və e irrational ədədlərdir. 3.14 və √9=3 rational ədədlərdir.",
    usage_percent = 20
  ),
  
  # 3. True/False (Doğru/Yanlış)
  true_false = list(
    question_id = 3,
    title = "✓/✗ Doğru/Yanlış Sualı",
    question_text = "Kvadrat köklər həmişə müsbət ədədlər verir",
    question_type = "true_false",
    correct_answer = TRUE,
    difficulty_level = 1,
    topic = "Kök əməliyyatları",
    time_limit = 30,
    explanation = "Bəli, riyaziyyatda kvadrat kök simvolu (√) həmişə müsbət nəticə verir",
    usage_percent = 15
  ),
  
  # 4. Open-ended (Açıq cavablı)
  open_ended = list(
    question_id = 4,
    title = "📝 Açıq Cavablı Sual",
    question_text = "Kvadrat tənliyin diskriminant düsturunu yazın və onun mənasını izah edin.",
    question_type = "open_ended",
    expected_keywords = c("diskriminant", "D", "b²-4ac", "kök", "həll", "tənlik"),
    max_score = 10,
    difficulty_level = 3,
    topic = "Kvadrat tənliklər",
    time_limit = 300,
    sample_answer = "Diskriminant D = b²-4ac düsturu ilə hesablanır. Əgər D>0 iki fərqli həll, D=0 bir həll, D<0 həll yoxdur.",
    usage_percent = 10
  ),
  
  # 5. Fill in the blanks (Boşluqları doldurun)
  fill_blanks = list(
    question_id = 5,
    title = "📄 Boşluqları Doldurun",
    question_text = "Sin²x + Cos²x = ___ və tg(45°) = ___",
    question_type = "fill_blanks",
    blanks = list(
      blank1 = list(correct = "1", alternatives = c("1", "bir")),
      blank2 = list(correct = "1", alternatives = c("1", "bir"))
    ),
    difficulty_level = 2,
    topic = "Trigonometriya",
    time_limit = 45,
    explanation = "Sin²x + Cos²x = 1 (əsas trigonometrik düstur), tg(45°) = 1",
    usage_percent = 5
  ),
  
  # 6. Matching (Uyğunlaşdırma)
  matching = list(
    question_id = 6,
    title = "🔗 Uyğunlaşdırma Sualı",
    question_text = "Sol və sağ sütunları uyğunlaşdırın:",
    question_type = "matching",
    left_items = c(
      "1" = "Sin(30°)",
      "2" = "Cos(60°)", 
      "3" = "Tg(45°)",
      "4" = "Sin(90°)"
    ),
    right_items = c(
      "A" = "1",
      "B" = "1/2",
      "C" = "√3/2",
      "D" = "0"
    ),
    correct_matches = list("1" = "B", "2" = "B", "3" = "A", "4" = "A"),
    difficulty_level = 3,
    topic = "Trigonometriya",
    time_limit = 120,
    usage_percent = 3
  ),
  
  # 7. Ordering (Sıralama)
  ordering = list(
    question_id = 7,
    title = "🔢 Sıralama Sualı",
    question_text = "Aşağıdakı ədədləri kiçikdən böyüyə doğru düzün:",
    question_type = "ordering",
    items = c(
      "A" = "√16",
      "B" = "π",
      "C" = "2.5",
      "D" = "3²/2",
      "E" = "√5"
    ),
    correct_order = c("E", "C", "B", "A", "D"), # √5≈2.24, 2.5, π≈3.14, 4, 4.5
    difficulty_level = 4,
    topic = "Ədədlərin müqayisəsi",
    time_limit = 90,
    explanation = "√5≈2.24 < 2.5 < π≈3.14 < √16=4 < 3²/2=4.5",
    usage_percent = 2
  )
)

# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "🎓 Test Tapşırıqları Format Nümayişi"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("📚 Test Formatları", tabName = "formats", icon = icon("book")),
      menuItem("🎮 İnteraktiv Test", tabName = "interactive", icon = icon("gamepad")),
      menuItem("📊 Nəticələr", tabName = "results", icon = icon("chart-bar")),
      menuItem("📈 Statistika", tabName = "statistics", icon = icon("chart-line")),
      menuItem("ℹ️ Məlumat", tabName = "info", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .question-box {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          border-radius: 15px;
          padding: 20px;
          margin: 15px 0;
          box-shadow: 0 8px 32px rgba(0,0,0,0.1);
        }
        .demo-box {
          background: white;
          border: 2px solid #3c8dbc;
          border-radius: 10px;
          padding: 20px;
          margin: 10px 0;
          box-shadow: 0 4px 16px rgba(0,0,0,0.1);
        }
        .correct-answer { 
          color: #28a745; 
          font-weight: bold; 
          background: #d4edda;
          padding: 5px 10px;
          border-radius: 5px;
        }
        .timer { 
          color: #fd7e14; 
          font-weight: bold;
          background: #fff3cd;
          padding: 10px;
          border-radius: 10px;
          text-align: center;
        }
        .explanation { 
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 15px; 
          border-radius: 10px; 
          margin-top: 15px;
        }
        .format-card {
          transition: transform 0.3s ease;
        }
        .format-card:hover {
          transform: translateY(-5px);
        }
        .btn-custom {
          border-radius: 25px;
          padding: 10px 25px;
          font-weight: bold;
          text-transform: uppercase;
          letter-spacing: 1px;
        }
      "))
    ),
    
    tabItems(
      # Test Formatları Tab
      tabItem(tabName = "formats",
              fluidRow(
                box(title = "🎯 Test Formatları Haqqında", status = "primary", 
                    solidHeader = TRUE, width = 12,
                    background = "light-blue",
                    div(style = "color: white;",
                        h3("Test Tapşırıqlarının 7 Əsas Formatı:"),
                        tags$div(class = "row",
                                 tags$div(class = "col-md-6",
                                          tags$ul(style = "font-size: 16px;",
                                                  tags$li("🔘 Çoxlu Seçim - Ən populyar format (45%)"),
                                                  tags$li("☑️ Çoxlu Cavab - Mürəkkəb bilik (20%)"),
                                                  tags$li("✓/✗ Doğru/Yanlış - Sürətli qiymətləndirmə (15%)"),
                                                  tags$li("📝 Açıq Cavablı - Dərin düşüncə (10%)")
                                          )
                                 ),
                                 tags$div(class = "col-md-6",
                                          tags$ul(style = "font-size: 16px;",
                                                  tags$li("📄 Boşluqları Doldurun - Spesifik bilik (5%)"),
                                                  tags$li("🔗 Uyğunlaşdırma - Əlaqələr (3%)"),
                                                  tags$li("🔢 Sıralama - Məntiq və analiz (2%)")
                                          )
                                 )
                        )
                    )
                )
              ),
              
              fluidRow(
                lapply(names(test_questions), function(format_name) {
                  question <- test_questions[[format_name]]
                  
                  box(
                    title = question$title,
                    status = "info",
                    solidHeader = TRUE,
                    width = 6,
                    height = "550px",
                    class = "format-card",
                    
                    div(class = "demo-box",
                        h4(question$question_text),
                        
                        # Progress bar
                        div(style = "margin: 10px 0;",
                            div(class = "progress",
                                div(class = "progress-bar progress-bar-striped", 
                                    style = paste0("width: ", question$usage_percent, "%; background-color: #3c8dbc;"),
                                    paste0("İstifadə: ", question$usage_percent, "%"))
                            )
                        ),
                        
                        hr(),
                        
                        # Format tipinə görə UI göstər
                        if(question$question_type == "multiple_choice") {
                          tagList(
                            radioButtons(paste0("demo_", format_name), "Seçiminiz:",
                                         choices = question$options, selected = character(0)),
                            div(class = "correct-answer",
                                paste("✓ Düzgün cavab:", names(question$options)[question$options == question$correct_answer]))
                          )
                        } else if(question$question_type == "multiple_response") {
                          tagList(
                            checkboxGroupInput(paste0("demo_", format_name), "Seçimləriniz:",
                                               choices = question$options),
                            div(class = "correct-answer",
                                paste("✓ Düzgün cavablar:", paste(names(question$options)[question$options %in% question$correct_answers], collapse = ", ")))
                          )
                        } else if(question$question_type == "true_false") {
                          tagList(
                            radioButtons(paste0("demo_", format_name), "Cavabınız:",
                                         choices = list("Doğru" = TRUE, "Yanlış" = FALSE),
                                         selected = character(0)),
                            div(class = "correct-answer",
                                paste("✓ Düzgün cavab:", ifelse(question$correct_answer, "Doğru", "Yanlış")))
                          )
                        } else if(question$question_type == "open_ended") {
                          tagList(
                            textAreaInput(paste0("demo_", format_name), "Cavabınız:", 
                                          placeholder = "Cavabınızı buraya yazın...", rows = 3),
                            div(class = "correct-answer",
                                paste("✓ Nümunə:", substr(question$sample_answer, 1, 50), "..."))
                          )
                        } else if(question$question_type == "fill_blanks") {
                          tagList(
                            fluidRow(
                              column(6, textInput(paste0("demo_", format_name, "_1"), "Birinci boşluq:", placeholder = "1-ci boşluq")),
                              column(6, textInput(paste0("demo_", format_name, "_2"), "İkinci boşluq:", placeholder = "2-ci boşluq"))
                            ),
                            div(class = "correct-answer",
                                "✓ Düzgün cavablar: 1, 1")
                          )
                        } else if(question$question_type == "matching") {
                          tagList(
                            p("Sol tərəfdən seçin, sağ tərəflə uyğunlaşdırın:"),
                            fluidRow(
                              column(6,
                                     h5("Sol sütun:"),
                                     lapply(names(question$left_items), function(key) {
                                       div(style = "margin: 5px 0; padding: 8px; background: #e9ecef; border-radius: 5px;",
                                           paste(key, ":", question$left_items[[key]]))
                                     })
                              ),
                              column(6,
                                     h5("Sağ sütun:"),
                                     lapply(names(question$right_items), function(key) {
                                       div(style = "margin: 5px 0; padding: 8px; background: #f8f9fa; border-radius: 5px;",
                                           paste(key, ":", question$right_items[[key]]))
                                     })
                              )
                            ),
                            div(class = "correct-answer",
                                "✓ Düzgün uyğunluq: 1-B, 2-B, 3-A, 4-A")
                          )
                        } else if(question$question_type == "ordering") {
                          tagList(
                            p("Verilən ədədləri kiçikdən böyüyə sıralayın:"),
                            div(style = "background: #f8f9fa; padding: 10px; border-radius: 5px;",
                                lapply(names(question$items), function(key) {
                                  span(style = "margin: 5px; padding: 5px 10px; background: white; border-radius: 3px; display: inline-block;",
                                       paste(key, ":", question$items[[key]]))
                                })
                            ),
                            div(class = "correct-answer",
                                paste("✓ Düzgün sıra:", paste(question$correct_order, collapse = " < ")))
                          )
                        },
                        
                        hr(),
                        div(style = "font-size: 14px;",
                            p(strong("📊 Çətinlik: "), 
                              paste(rep("⭐", question$difficulty_level), collapse = ""),
                              paste0(" (", question$difficulty_level, "/5)")),
                            p(strong("📚 Mövzu: "), question$topic),
                            p(strong("⏱️ Vaxt: "), question$time_limit, " saniyə"),
                            if(!is.null(question$explanation)) {
                              p(strong("💡 İzah: "), question$explanation)
                            }
                        )
                    )
                  )
                })
              )
      ),
      
      # İnteraktiv Test Tab
      tabItem(tabName = "interactive",
              fluidRow(
                box(title = "🎮 İnteraktiv Test Sessiyası", status = "success", 
                    solidHeader = TRUE, width = 12,
                    
                    fluidRow(
                      column(4,
                             h4("🎯 Test Konfiqurasiyası"),
                             selectInput("selected_format", "Test Formatını Seçin:",
                                         choices = list(
                                           "🔘 Çoxlu Seçim" = "multiple_choice",
                                           "☑️ Çoxlu Cavab" = "multiple_response", 
                                           "✓/✗ Doğru/Yanlış" = "true_false",
                                           "📝 Açıq Cavablı" = "open_ended",
                                           "📄 Boşluqları Doldurun" = "fill_blanks",
                                           "🔗 Uyğunlaşdırma" = "matching",
                                           "🔢 Sıralama" = "ordering"
                                         )),
                             br(),
                             actionButton("start_test", "🚀 Testi Başlat", 
                                          class = "btn-success btn-custom btn-lg btn-block")
                      ),
                      column(4,
                             h4("📋 Test Məlumatları"),
                             div(class = "demo-box",
                                 withSpinner(verbatimTextOutput("test_info"))
                             )
                      ),
                      column(4,
                             h4("⏰ Vaxt Sayğacı"),
                             div(class = "timer",
                                 h2(textOutput("timer_text")))
                      )
                    ),
                    
                    hr(),
                    
                    # Test sualı sahəsi
                    conditionalPanel(
                      condition = "input.start_test > 0",
                      div(class = "question-box",
                          withSpinner(uiOutput("current_question")),
                          br(),
                          fluidRow(
                            column(6,
                                   actionButton("submit_answer", "✅ Cavabı Təsdiqlə", 
                                                class = "btn-primary btn-custom btn-block")
                            ),
                            column(6,
                                   actionButton("next_question", "➡️ Növbəti Sual", 
                                                class = "btn-info btn-custom btn-block")
                            )
                          ),
                          br(),
                          uiOutput("answer_feedback")
                      )
                    )
                )
              )
      ),
      
      # Nəticələr Tab
      tabItem(tabName = "results",
              fluidRow(
                box(title = "📊 Test Nəticələri", status = "warning", 
                    solidHeader = TRUE, width = 12,
                    
                    conditionalPanel(
                      condition = "output.has_results",
                      div(class = "demo-box",
                          h4("🏆 Ümumi Nəticələr"),
                          fluidRow(
                            valueBoxOutput("total_questions"),
                            valueBoxOutput("success_rate"),
                            valueBoxOutput("average_score")
                          ),
                          br(),
                          withSpinner(DT::dataTableOutput("results_table")),
                          br(),
                          downloadButton("download_results", "📥 Nəticələri Yüklə", 
                                         class = "btn-success btn-custom")
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "!output.has_results",
                      div(class = "text-center", style = "padding: 50px;",
                          icon("clipboard-list", "fa-5x", style = "color: #bbb;"),
                          h3("Hələ test nəticəsi yoxdur", style = "color: #777;"),
                          p("İnteraktiv test bölməsindən test keçərək nəticələri görə bilərsiniz"),
                          actionButton("go_to_test", "🎮 Testə Get", class = "btn-primary btn-custom")
                      )
                    )
                )
              )
      ),
      
      # Statistika Tab  
      tabItem(tabName = "statistics",
              fluidRow(
                box(title = "📈 Çətinlik Səviyyəsi Analizi", status = "info", 
                    solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("difficulty_chart", height = "400px"))
                ),
                
                box(title = "🥧 Format Populyarlığı", status = "info", 
                    solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("format_pie_chart", height = "400px"))
                )
              ),
              
              fluidRow(
                box(title = "📊 Format İstifadə Statistikaları", status = "primary", 
                    solidHeader = TRUE, width = 8,
                    withSpinner(plotlyOutput("usage_bar_chart", height = "350px"))
                ),
                
                box(title = "🎯 Tövsiyələr", status = "success", 
                    solidHeader = TRUE, width = 4,
                    div(class = "demo-box",
                        h5("💡 Format Seçimi Tövsiyələri:"),
                        tags$ul(
                          tags$li("🔘 Çoxlu seçim - Əsas bilik yoxlaması"),
                          tags$li("📝 Açıq cavablı - Dərin düşüncə"),
                          tags$li("☑️ Çoxlu cavab - Mürəkkəb anlayış"),
                          tags$li("✓/✗ Doğru/Yanlış - Sürətli yoxlama")
                        ),
                        hr(),
                        h5("⚖️ Optimal Kompozisiya:"),
                        div(class = "progress", style = "margin: 5px 0;",
                            div(class = "progress-bar", style = "width: 60%; background: #3c8dbc;", "60% MC")),
                        div(class = "progress", style = "margin: 5px 0;",
                            div(class = "progress-bar", style = "width: 20%; background: #00a65a;", "20% MR")),
                        div(class = "progress", style = "margin: 5px 0;",
                            div(class = "progress-bar", style = "width: 15%; background: #f39c12;", "15% T/F")),
                        div(class = "progress", style = "margin: 5px 0;",
                            div(class = "progress-bar", style = "width: 5%; background: #dd4b39;", "5% Digər"))
                    )
                )
              ),
              
              fluidRow(
                box(title = "📝 Ətraflı Analiz Hesabatı", status = "primary", 
                    solidHeader = TRUE, width = 12,
                    div(class = "demo-box",
                        withSpinner(verbatimTextOutput("detailed_analysis"))
                    )
                )
              )
      ),
      
      # Məlumat Tab
      tabItem(tabName = "info",
              fluidRow(
                box(title = "ℹ️ Sistem Haqqında", status = "primary", 
                    solidHeader = TRUE, width = 12,
                    div(class = "demo-box",
                        h3("🎓 Test Tapşırıqları Format Nümayişi"),
                        p("Bu tətbiq müxtəlif test formatlarını nümayiş etdirir və onların xüsusiyyətlərini göstərir."),
                        
                        h4("🔧 Texniki Xüsusiyyətlər:"),
                        tags$ul(
                          tags$li("R Shiny framework"),
                          tags$li("İnteraktiv vizuallaşdırma"),
                          tags$li("Real-time timer"),
                          tags$li("Dinamik UI elementləri"),
                          tags$li("CSV export funksiyası")
                        ),
                        
                        h4("📚 Test Formatları:"),
                        div(class = "row",
                            lapply(test_questions, function(q) {
                              div(class = "col-md-4",
                                  div(style = "margin: 10px; padding: 15px; background: #f8f9fa; border-radius: 8px;",
                                      h5(q$title),
                                      p(strong("Çətinlik: "), paste(rep("⭐", q$difficulty_level), collapse = "")),
                                      p(strong("İstifadə: "), paste0(q$usage_percent, "%")),
                                      p(strong("Vaxt: "), q$time_limit, "s")
                                  )
                              )
                            })
                        )
                    )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive dəyişənlər
  values <- reactiveValues(
    current_question = NULL,
    test_results = data.frame(),
    timer_active = FALSE,
    remaining_time = 0,
    test_started = FALSE
  )
  
  # Test başlatma
  observeEvent(input$start_test, {
    req(input$selected_format)
    values$current_question <- test_questions[[input$selected_format]]
    values$timer_active <- TRUE
    values$remaining_time <- values$current_question$time_limit
    values$test_started <- TRUE
    
    # Debug üçün
    cat("Timer başladı: ", values$current_question$time_limit, " saniyə\n")
    
    showNotification("🚀 Test başladı! Uğurlar!", type = "default", duration = 3)
  })
  
  # Timer - Təkmilləşdirilmiş versiya
  timer_observer <- observe({
    invalidateLater(1000, session)
    
    # Timer aktiv olub-olmadığını yoxla
    if(isTRUE(values$timer_active)) {
      # Remaining time yoxla
      if(is.numeric(values$remaining_time) && values$remaining_time > 0) {
        values$remaining_time <- values$remaining_time - 1
        # Debug
        if(values$remaining_time %% 10 == 0) {
          cat("Qalan vaxt: ", values$remaining_time, " saniyə\n")
        }
      } else if(is.numeric(values$remaining_time) && values$remaining_time <= 0) {
        values$timer_active <- FALSE
        cat("Vaxt bitdi!\n")
        showNotification("⏰ Vaxt bitdi!", type = "warning", duration = 5)
      }
    }
  })
  
  # Timer göstəricisi
  output$timer_text <- renderText({
    # Reactive values-ları düzgün oxumaq üçün
    timer_active <- values$timer_active
    remaining_time <- values$remaining_time
    test_started <- values$test_started
    
    if(!is.null(timer_active) && timer_active && !is.null(remaining_time)) {
      minutes <- floor(remaining_time / 60)
      seconds <- remaining_time %% 60
      sprintf("%02d:%02d", minutes, seconds)
    } else if(!is.null(test_started) && test_started) {
      "00:00"
    } else {
      "--:--"
    }
  })
  
  # Test məlumatları
  output$test_info <- renderText({
    if(!is.null(values$current_question)) {
      paste(
        "📋 Format:", values$current_question$title,
        "\n📚 Mövzu:", values$current_question$topic,
        "\n⭐ Çətinlik:", values$current_question$difficulty_level, "/5",
        "\n⏱️ Vaxt limiti:", values$current_question$time_limit, "saniyə",
        "\n🎯 ID:", values$current_question$question_id
      )
    } else {
      "Test seçilməyib.\nYuxarıdan format seçib 'Testi Başlat' düyməsini basın."
    }
  })
  
  # Hazırki sual UI
  output$current_question <- renderUI({
    req(values$current_question)
    question <- values$current_question
    
    div(
      h3(style = "color: white; margin-bottom: 20px;", 
         paste("📝 Sual", question$question_id, ":", question$question_text)),
      
      if(question$question_type == "multiple_choice") {
        radioButtons("current_answer", "Cavabınızı seçin:",
                     choices = question$options, selected = character(0))
      } else if(question$question_type == "multiple_response") {
        checkboxGroupInput("current_answer", "Cavablarınızı seçin:",
                           choices = question$options)
      } else if(question$question_type == "true_false") {
        radioButtons("current_answer", "Cavabınızı seçin:",
                     choices = list("✓ Doğru" = TRUE, "✗ Yanlış" = FALSE),
                     selected = character(0))
      } else if(question$question_type == "open_ended") {
        textAreaInput("current_answer", "Cavabınızı yazın:", 
                      placeholder = "Ətraflı cavabınızı buraya yazın...", 
                      rows = 5, width = "100%")
      } else if(question$question_type == "fill_blanks") {
        tagList(
          h5("Boşluqları doldurun:", style = "color: white;"),
          p("Sin²x + Cos²x = ___ və tg(45°) = ___", style = "color: white; font-size: 16px;"),
          fluidRow(
            column(6, 
                   textInput("blank1", "Birinci boşluq (Sin²x + Cos²x = ?):", 
                             placeholder = "Cavabı buraya yazın", value = "")
            ),
            column(6,
                   textInput("blank2", "İkinci boşluq (tg(45°) = ?):", 
                             placeholder = "Cavabı buraya yazın", value = "")
            )
          )
        )
      } else if(question$question_type == "matching") {
        tagList(
          h5("Sol və sağ sütunları uyğunlaşdırın:", style = "color: white;"),
          br(),
          fluidRow(
            column(6,
                   h6("Sol sütun:", style = "color: white;"),
                   div(style = "background: rgba(255,255,255,0.1); padding: 15px; border-radius: 8px;",
                       lapply(names(question$left_items), function(key) {
                         div(style = "margin: 8px 0; padding: 10px; background: rgba(255,255,255,0.2); border-radius: 5px; color: white;",
                             paste(key, ":", question$left_items[[key]]))
                       })
                   )
            ),
            column(6,
                   h6("Sağ sütun:", style = "color: white;"),
                   div(style = "background: rgba(255,255,255,0.1); padding: 15px; border-radius: 8px;",
                       lapply(names(question$right_items), function(key) {
                         div(style = "margin: 8px 0; padding: 10px; background: rgba(255,255,255,0.2); border-radius: 5px; color: white;",
                             paste(key, ":", question$right_items[[key]]))
                       })
                   )
            )
          ),
          br(),
          textAreaInput("current_answer", "Uyğunluqları yazın (məsələn: 1-B, 2-A, 3-C, 4-D):",
                        placeholder = "Cavabınızı '1-B, 2-A, 3-C, 4-D' formatında yazın", 
                        rows = 3)
        )
      } else if(question$question_type == "ordering") {
        tagList(
          h5("Aşağıdakı ədədləri kiçikdən böyüyə sıralayın:", style = "color: white;"),
          br(),
          div(style = "background: rgba(255,255,255,0.1); padding: 15px; border-radius: 8px; margin: 10px 0;",
              lapply(names(question$items), function(key) {
                span(style = "margin: 8px; padding: 8px 15px; background: rgba(255,255,255,0.2); border-radius: 5px; display: inline-block; color: white; font-weight: bold;",
                     paste(key, ":", question$items[[key]]))
              })
          ),
          br(),
          textAreaInput("current_answer", "Sıralamanı yazın (məsələn: E, C, B, A, D):",
                        placeholder = "Hərf sırasını kiçikdən böyüyə yazın: E, C, B, A, D", 
                        rows = 2)
        )
      }
    )
  })
  
  # Cavab qiymətləndirmə funksiyası
  evaluate_answer <- function(question, user_answer) {
    
    # Əgər cavab boşdursa
    if(is.null(user_answer) || length(user_answer) == 0 || 
       (is.character(user_answer) && all(trimws(user_answer) == ""))) {
      return(list(
        is_correct = FALSE,
        score = 0,
        correct_info = "Cavab verilməyib"
      ))
    }
    
    if(question$question_type == "multiple_choice") {
      is_correct <- user_answer == question$correct_answer
      correct_text <- tryCatch({
        names(question$options)[question$options == question$correct_answer]
      }, error = function(e) {
        question$correct_answer
      })
      
      return(list(
        is_correct = is_correct,
        score = if(is_correct) 1 else 0,
        correct_info = correct_text
      ))
      
    } else if(question$question_type == "multiple_response") {
      is_correct <- setequal(user_answer, question$correct_answers)
      partial_score <- length(intersect(user_answer, question$correct_answers)) / length(question$correct_answers)
      
      correct_text <- tryCatch({
        paste(names(question$options)[question$options %in% question$correct_answers], collapse = ", ")
      }, error = function(e) {
        paste(question$correct_answers, collapse = ", ")
      })
      
      return(list(
        is_correct = is_correct,
        score = if(is_correct) 1 else round(partial_score, 2),
        correct_info = correct_text
      ))
      
    } else if(question$question_type == "true_false") {
      is_correct <- as.logical(user_answer) == question$correct_answer
      return(list(
        is_correct = is_correct,
        score = if(is_correct) 1 else 0,
        correct_info = ifelse(question$correct_answer, "Doğru", "Yanlış")
      ))
      
    } else if(question$question_type == "open_ended") {
      # Açar sözlər əsasında qiymətləndirmə
      keywords_found <- sum(sapply(question$expected_keywords, function(keyword) {
        grepl(tolower(keyword), tolower(paste(user_answer, collapse = " ")), fixed = TRUE)
      }))
      
      score <- (keywords_found / length(question$expected_keywords)) * question$max_score
      
      return(list(
        is_correct = score >= question$max_score * 0.6,
        score = round(score, 1),
        correct_info = question$sample_answer
      ))
      
    } else if(question$question_type == "fill_blanks") {
      # Boşluq doldurma qiymətləndirməsi
      user_text <- tolower(paste(user_answer, collapse = " "))
      
      # "1" rəqəmlərini axtarırıq
      ones_count <- length(gregexpr("1", user_text)[[1]])
      if(ones_count == -1) ones_count <- 0
      
      # Həm Sin²x + Cos²x = 1, həm də tg(45°) = 1 olmalıdır
      is_correct <- ones_count >= 2
      score <- if(is_correct) 1 else ones_count * 0.5
      
      return(list(
        is_correct = is_correct,
        score = round(score, 2),
        correct_info = "Sin²x + Cos²x = 1 və tg(45°) = 1"
      ))
      
    } else if(question$question_type == "matching") {
      # Uyğunlaşdırma qiymətləndirməsi
      user_text <- paste(user_answer, collapse = " ")
      user_matches <- trimws(strsplit(user_text, "[,;]")[[1]])
      correct_count <- 0
      total_matches <- length(question$correct_matches)
      
      for(match in user_matches) {
        if(grepl("-", match)) {
          parts <- trimws(strsplit(match, "-")[[1]])
          if(length(parts) == 2) {
            left_item <- parts[1]
            right_item <- parts[2]
            if(left_item %in% names(question$correct_matches) && 
               question$correct_matches[[left_item]] == right_item) {
              correct_count <- correct_count + 1
            }
          }
        }
      }
      
      score <- if(total_matches > 0) correct_count / total_matches else 0
      is_correct <- score >= 0.8
      
      correct_info <- paste(sapply(names(question$correct_matches), function(x) {
        paste0(x, "-", question$correct_matches[[x]])
      }), collapse = ", ")
      
      return(list(
        is_correct = is_correct,
        score = round(score, 2),
        correct_info = correct_info
      ))
      
    } else if(question$question_type == "ordering") {
      # Sıralama qiymətləndirməsi
      user_text <- paste(user_answer, collapse = " ")
      user_order <- trimws(strsplit(gsub("[,;]", ",", user_text), ",")[[1]])
      user_order <- user_order[user_order != ""]
      
      is_correct <- identical(user_order, question$correct_order)
      
      # Qismən bal - neçə element düzgün yerdədir
      partial_score <- 0
      if(length(user_order) == length(question$correct_order)) {
        for(i in 1:length(user_order)) {
          if(i <= length(user_order) && i <= length(question$correct_order) &&
             user_order[i] == question$correct_order[i]) {
            partial_score <- partial_score + 1
          }
        }
        partial_score <- partial_score / length(question$correct_order)
      }
      
      return(list(
        is_correct = is_correct,
        score = if(is_correct) 1 else round(partial_score, 2),
        correct_info = paste(question$correct_order, collapse = " < ")
      ))
      
    } else {
      # Naməlum format üçün
      return(list(
        is_correct = FALSE,
        score = 0,
        correct_info = "Format dəstəklənmir"
      ))
    }
  }
  
  # Cavab təsdiqlənməsi
  observeEvent(input$submit_answer, {
    req(values$current_question)
    
    question <- values$current_question
    
    # Müxtəlif input növləri üçün cavab toplama
    if(question$question_type == "fill_blanks") {
      if(!is.null(input$blank1) && !is.null(input$blank2)) {
        user_answer <- paste("Boşluq 1:", input$blank1, "| Boşluq 2:", input$blank2)
        # Xüsusi qiymətləndirmə üçün
        blank1 <<- input$blank1
        blank2 <<- input$blank2
      } else {
        user_answer <- input$current_answer
      }
    } else {
      user_answer <- input$current_answer
    }
    
    if(is.null(user_answer) || (is.character(user_answer) && trimws(user_answer) == "")) {
      showNotification("⚠️ Zəhmət olmasa cavab verin!", type = "warning", duration = 3)
      return()
    }
    
    # Qiymətləndirmə
    result <- evaluate_answer(question, user_answer)
    
    # Nəticəni saxla
    new_result <- data.frame(
      question_id = question$question_id,
      question_text = substr(question$question_text, 1, 50),
      question_type = question$question_type,
      topic = question$topic,
      difficulty = question$difficulty_level,
      user_answer = if(is.null(user_answer)) "Cavab verilməyib" else {
        if(is.character(user_answer) && length(user_answer) > 0) {
          paste(user_answer, collapse = ", ")
        } else {
          as.character(user_answer)[1]
        }
      },
      correct_answer = if(question$question_type == "multiple_response") {
        paste(names(question$options)[question$options %in% question$correct_answers], collapse = ", ")
      } else if(question$question_type == "open_ended") {
        substr(question$sample_answer, 1, 50)
      } else if(question$question_type == "fill_blanks") {
        "Sin²x + Cos²x = 1, tg(45°) = 1"
      } else if(question$question_type == "matching") {
        paste(sapply(names(question$correct_matches), function(x) {
          paste0(x, "-", question$correct_matches[[x]])
        }), collapse = ", ")
      } else if(question$question_type == "ordering") {
        paste(question$correct_order, collapse = " < ")
      } else {
        if(question$question_type == "multiple_choice" && !is.null(question$options) && question$correct_answer %in% question$options) {
          names(question$options)[question$options == question$correct_answer]
        } else {
          as.character(question$correct_answer)
        }
      },
      is_correct = result$is_correct,
      score = result$score,
      max_score = if(question$question_type == "open_ended") question$max_score else 1,
      response_time = question$time_limit - values$remaining_time,
      timestamp = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )
    
    values$test_results <- rbind(values$test_results, new_result)
    values$timer_active <- FALSE
    
    # Feedback göstər
    output$answer_feedback <- renderUI({
      div(class = "explanation",
          h4(if(result$is_correct) "🎉 Əla! Doğru cavab!" else "😔 Təəssüf, yanlış cavab"),
          h5("📋 Ətraflı məlumat:"),
          p(strong("Sizin cavabınız: "), user_answer),
          p(strong("Düzgün cavab: "), result$correct_info),
          p(strong("Aldığınız bal: "), result$score, "/", 
            if(question$question_type == "open_ended") question$max_score else 1),
          if(!is.null(question$explanation)) {
            div(
              h5("💡 İzah:"),
              p(question$explanation)
            )
          },
          p(strong("⏱️ Cavab müddəti: "), question$time_limit - values$remaining_time, " saniyə")
      )
    })
    
    # Bildiriş göstər
    showNotification(
      if(result$is_correct) "🎉 Təbriklər! Doğru cavab!" else "😐 Yanlış cavab, ancaq təcrübə qazandınız!",
      type = if(result$is_correct) "default" else "warning",
      duration = 4
    )
  })
  
  # Növbəti sual
  observeEvent(input$next_question, {
    # Başqa format seç
    current_formats <- names(test_questions)
    current_index <- which(current_formats == input$selected_format)
    next_index <- if(current_index < length(current_formats)) current_index + 1 else 1
    
    updateSelectInput(session, "selected_format", selected = current_formats[next_index])
    
    # Timer sıfırla
    values$timer_active <- FALSE
    values$remaining_time <- 0
    values$current_question <- NULL
    
    # Feedback təmizlə
    output$answer_feedback <- renderUI(NULL)
    
    showNotification("➡️ Növbəti format üçün hazır! 'Testi Başlat' basın.", 
                     type = "default", duration = 3)
  })
  
  # Nəticələrə keçid
  observeEvent(input$go_to_test, {
    updateTabItems(session, "sidebar", "interactive")
  })
  
  # Nəticələr cədvəli
  output$results_table <- DT::renderDataTable({
    req(nrow(values$test_results) > 0)
    
    display_data <- values$test_results %>%
      mutate(
        Sual = paste("Q", question_id),
        Format = case_when(
          question_type == "multiple_choice" ~ "🔘 Çoxlu Seçim",
          question_type == "multiple_response" ~ "☑️ Çoxlu Cavab",
          question_type == "true_false" ~ "✓/✗ Doğru/Yanlış",
          question_type == "open_ended" ~ "📝 Açıq Cavablı",
          TRUE ~ question_type
        ),
        Nəticə = ifelse(is_correct, "✅ Doğru", "❌ Yanlış"),
        Bal = paste0(score, "/", max_score),
        Vaxt = paste0(response_time, "s")
      ) %>%
      select(
        "Sual" = Sual,
        "Format" = Format,
        "Mövzu" = topic,
        "Çətinlik" = difficulty,
        "Nəticə" = Nəticə,
        "Bal" = Bal,
        "Vaxt" = Vaxt
      )
    
    DT::datatable(display_data, 
                  options = list(
                    pageLength = 10, 
                    scrollX = TRUE,
                    dom = 'Bfrtip'
                  ),
                  rownames = FALSE,
                  escape = FALSE) %>%
      DT::formatStyle("Nəticə", 
                      color = DT::styleEqual(c("✅ Doğru", "❌ Yanlış"), 
                                             c("green", "red")))
  }, server = FALSE)
  
  # Value boxes
  output$total_questions <- renderValueBox({
    valueBox(
      value = nrow(values$test_results),
      subtitle = "Ümumi Sual Sayı",
      icon = icon("question-circle"),
      color = "blue"
    )
  })
  
  output$success_rate <- renderValueBox({
    if(nrow(values$test_results) > 0) {
      rate <- round(sum(values$test_results$is_correct) / nrow(values$test_results) * 100, 1)
    } else {
      rate <- 0
    }
    
    valueBox(
      value = paste0(rate, "%"),
      subtitle = "Uğur Nisbəti",
      icon = icon("trophy"),
      color = if(rate >= 80) "green" else if(rate >= 60) "yellow" else "red"
    )
  })
  
  output$average_score <- renderValueBox({
    if(nrow(values$test_results) > 0) {
      avg <- round(mean(values$test_results$score), 1)
    } else {
      avg <- 0
    }
    
    valueBox(
      value = avg,
      subtitle = "Orta Bal",
      icon = icon("star"),
      color = "purple"
    )
  })
  
  # Nəticələr mövcudluğu
  output$has_results <- reactive({
    nrow(values$test_results) > 0
  })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)
  
  # Çətinlik chartı
  output$difficulty_chart <- renderPlotly({
    # Real data varsa onu istifadə et, yoxsa nümunə data
    if(nrow(values$test_results) > 0) {
      difficulty_data <- values$test_results %>%
        count(difficulty, name = "Sayı") %>%
        mutate(Çətinlik = paste0("Səviyyə ", difficulty))
    } else {
      difficulty_data <- data.frame(
        Çətinlik = paste0("Səviyyə ", 1:5),
        Sayı = c(2, 3, 1, 1, 0)
      )
    }
    
    p <- ggplot(difficulty_data, aes(x = Çətinlik, y = Sayı, fill = Çətinlik)) +
      geom_col(alpha = 0.8) +
      scale_fill_viridis_d(option = "plasma") +
      labs(title = "⭐ Çətinlik Səviyyəsinə Görə Suallar",
           x = "Çətinlik Səviyyəsi", y = "Sual Sayı") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Format pie chart
  output$format_pie_chart <- renderPlotly({
    format_data <- data.frame(
      Format = c("Çoxlu Seçim", "Çoxlu Cavab", "Doğru/Yanlış", "Açıq Cavablı", "Boşluq", "Uyğunlaşdırma", "Sıralama"),
      Faiz = c(45, 20, 15, 10, 5, 3, 2),
      Emoji = c("🔘", "☑️", "✓/✗", "📝", "📄", "🔗", "🔢")
    ) %>%
      mutate(Label = paste(Emoji, Format, paste0(Faiz, "%")))
    
    plot_ly(format_data, 
            labels = ~Label, 
            values = ~Faiz,
            type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            hovertemplate = paste('<b>%{label}</b><br>',
                                  'İstifadə: %{value}%<br>',
                                  '<extra></extra>'),
            marker = list(colors = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FECA57", "#FF9FF3", "#54A0FF"))) %>%
      layout(title = list(text = "🥧 Test Formatlarının Paylanması", 
                          font = list(size = 16, family = "Arial")),
             showlegend = FALSE)
  })
  
  # Usage bar chart
  output$usage_bar_chart <- renderPlotly({
    usage_data <- sapply(test_questions, function(q) q$usage_percent) %>%
      data.frame(Faiz = .) %>%
      mutate(
        Format = rownames(.),
        Emoji = c("🔘", "☑️", "✓/✗", "📝", "📄", "🔗", "🔢"),
        Label = paste(Emoji, case_when(
          Format == "multiple_choice" ~ "Çoxlu Seçim",
          Format == "multiple_response" ~ "Çoxlu Cavab", 
          Format == "true_false" ~ "Doğru/Yanlış",
          Format == "open_ended" ~ "Açıq Cavablı",
          Format == "fill_blanks" ~ "Boşluq Doldur",
          Format == "matching" ~ "Uyğunlaşdırma",
          Format == "ordering" ~ "Sıralama"
        ))
      ) %>%
      arrange(desc(Faiz))
    
    p <- ggplot(usage_data, aes(x = reorder(Label, Faiz), y = Faiz, fill = Label)) +
      geom_col(alpha = 0.8, width = 0.7) +
      coord_flip() +
      scale_fill_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FECA57", "#FF9FF3", "#54A0FF")) +
      labs(title = "📊 Test Formatlarının İstifadə Tezliyi",
           x = "Test Formatı", y = "İstifadə Faizi (%)") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "none",
        axis.text.y = element_text(size = 12)
      ) +
      geom_text(aes(label = paste0(Faiz, "%")), 
                hjust = -0.1, size = 4, fontface = "bold")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Ətraflı analiz
  output$detailed_analysis <- renderText({
    if(nrow(values$test_results) > 0) {
      total_questions <- nrow(values$test_results)
      correct_answers <- sum(values$test_results$is_correct)
      success_rate <- round((correct_answers / total_questions) * 100, 1)
      avg_score <- round(mean(values$test_results$score), 2)
      avg_time <- round(mean(values$test_results$response_time), 1)
      
      # Format analizi
      format_performance <- values$test_results %>%
        group_by(question_type) %>%
        summarise(
          count = n(),
          success = round(mean(is_correct) * 100, 1),
          avg_score = round(mean(score), 2),
          .groups = 'drop'
        )
      
      paste(
        "🎯 === ŞƏXSİ TEST NƏTİCƏLƏRİ ===",
        paste0("\n📊 Ümumi statistika:"),
        paste0("\n   • Ümumi sual sayı: ", total_questions),
        paste0("\n   • Doğru cavablar: ", correct_answers, " (", success_rate, "%)"),
        paste0("\n   • Orta bal: ", avg_score),
        paste0("\n   • Orta cavab müddəti: ", avg_time, " saniyə"),
        
        "\n\n🏆 === PERFORMANs QİYMƏTLƏNDİRMƏSİ ===",
        if(success_rate >= 90) "\n🥇 ƏLAHƏZRƏT! Siz test formatlarında ustadsınız!" 
        else if(success_rate >= 80) "\n🥈 ƏLA! Çox yaxşı nəticə göstərdiniz." 
        else if(success_rate >= 70) "\n🥉 YAXŞI! Daha çox təcrübə ilə daha da yaxşılaşacaq."
        else if(success_rate >= 60) "\n📈 ORTA! Əlavə təcrübə tövsiyə olunur."
        else "\n📚 BAŞLANĞIC! Narahat olmayın, hər kəs öyrənməklə başlayır.",
        
        "\n\n📈 === FORMAT ANALİZİ ===",
        if(nrow(format_performance) > 0) {
          paste0("\n", paste(apply(format_performance, 1, function(row) {
            format_name <- switch(row[["question_type"]],
                                  "multiple_choice" = "🔘 Çoxlu Seçim",
                                  "multiple_response" = "☑️ Çoxlu Cavab",
                                  "true_false" = "✓/✗ Doğru/Yanlış", 
                                  "open_ended" = "📝 Açıq Cavablı",
                                  row[["question_type"]]
            )
            paste0("   • ", format_name, ": ", row[["success"]], "% uğur (", row[["count"]], " sual)")
          }), collapse = "\n"))
        } else "",
        
        "\n\n💡 === TOVSİYƏLƏR ===",
        "\n🎯 Güclü tərəfləriniz:",
        if(success_rate >= 80) "\n   • Test formatlarını yaxşı anlayırsınız"
        else "\n   • Öyrənməyə həvəslisiniz",
        "\n   • Müxtəlif sual tiplərini sınaqdan keçirdiniz",
        
        "\n\n🔄 Təkmilləşdirmə sahələri:",
        if(success_rate < 80) "\n   • Daha çox müxtəlif format növlərini sınayın",
        if(avg_time > 60) "\n   • Cavab vermə sürətinizi artırın",
        "\n   • Açıq cavablı suallar üçün açar sözlərə diqqət edin",
        
        "\n\n📚 === ÜMUMİ TÖVSIYƏLƏR ===",
        "\n• Çoxlu seçim: Diqqətlə oxuyun, təsadüfi seçim etməyin",
        "\n• Çoxlu cavab: Bütün seçimləri nəzərdən keçirin", 
        "\n• Doğru/Yanlış: Mütləq ifadələrə diqqət edin",
        "\n• Açıq cavablı: Açar sözləri istifadə edin və strukturlaşdırın"
      )
    } else {
      paste(
        "📋 === TEST FORMATLARININ ƏTRAYLI ANALİZİ ===",
        "\n\n🎯 FORMATLARIN XÜSUSİYYƏTLƏRİ:",
        
        "\n\n1️⃣ ÇOXLU SEÇİM (Multiple Choice) - 45% istifadə",
        "\n   🔸 Üstünlükləri:",
        "\n     • Avtomatik qiymətləndirmə",
        "\n     • Obyektiv nəticələr", 
        "\n     • Böyük qruplar üçün əlverişli",
        "\n     • Sürətli test keçirmə",
        "\n   🔸 Çatışmazlıqları:",
        "\n     • Təsadüfi cavab riski (25% 4 variantda)",
        "\n     • Yaradıcı düşüncəni məhdudlaşdırır",
        "\n     • Yalnız tanıma səviyyəsində yoxlayır",
        
        "\n\n2️⃣ ÇOXLU CAVAB (Multiple Response) - 20% istifadə", 
        "\n   🔸 Üstünlükləri:",
        "\n     • Daha dəqiq bilik ölçümü",
        "\n     • Qismən bal verilə bilər",
        "\n     • Təsadüfi cavab şansı azdır",
        "\n   🔸 Çatışmazlıqları:",
        "\n     • Mürəkkəb qiymətləndirmə",
        "\n     • Şagirdlər üçün çaşdırıcı ola bilər",
        
        "\n\n3️⃣ DOĞRU/YANLIŞ (True/False) - 15% istifadə",
        "\n   🔸 Üstünlükləri:",
        "\n     • Çox sürətli",
        "\n     • Sadə qiymətləndirmə",
        "\n     • Çox sual sormaq mümkün",
        "\n   🔸 Çatışmazlıqları:",
        "\n     • 50% təsadüfi doğru cavab şansı",
        "\n     • Yüzeysel bilik yoxlaması",
        
        "\n\n4️⃣ AÇIQ CAVABLI (Open-ended) - 10% istifadə",
        "\n   🔸 Üstünlükləri:",
        "\n     • Dərin düşüncə tələb edir",
        "\n     • Yaradıcılığı təşviq edir",
        "\n     • Məntiq bacarığını ölçür",
        "\n   🔸 Çatışmazlıqları:",
        "\n     • Manual qiymətləndirmə lazım",
        "\n     • Vaxt aparır",
        "\n     • Subyektiv ola bilər",
        
        "\n\n📊 OPTIMAL TEST KOMPOZİSİYASI:",
        "\n• 60% Çoxlu seçim (əsas bilik)",
        "\n• 20% Çoxlu cavab (əlaqələr)", 
        "\n• 15% Doğru/Yanlış (faktlar)",
        "\n• 5% Açıq cavablı (dərin anlayış)",
        
        "\n\n🎯 FORMAT SEÇİMİ MEYARLARı:",
        "\n• Nəyi ölçmək istəyirsiniz? (faktlar/anlayış/tətbiq)",
        "\n• Neçə vaxtınız var? (qısa/orta/uzun)",
        "\n• Neçə iştirakçı var? (az/çox)",
        "\n• Qiymətləndirmə resursu? (avtomatik/manual)",
        "\n• Çətinlik səviyyəsi? (asan/orta/çətin)"
      )
    }
  })
  
  # Nəticələri yükləmə
  output$download_results <- downloadHandler(
    filename = function() {
      paste("test_results_", Sys.Date(), "_", format(Sys.time(), "%H%M"), ".csv", sep = "")
    },
    content = function(file) {
      export_data <- values$test_results %>%
        mutate(
          test_date = Sys.Date(),
          export_time = Sys.time()
        )
      write.csv(export_data, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

# Tətbiqi başlat
shinyApp(ui = ui, server = server)