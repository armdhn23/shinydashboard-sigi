library(rsconnect)
library(e1071)
library(shiny)
library(shinydashboard)
library(highcharter)
library(leaflet)
library(C50)
library(plyr)
library(dplyr)
library(DT)
library(dashboardthemes)
library(caret)

theme_blue_gradient <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(248,248,248)"
  
  ### header
  ,logoBackColor = "rgb(23,103,124)"
  
  ,headerButtonBackColor = "rgb(23,103,124)"
  ,headerButtonIconColor = "rgb(75,75,75)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor ="rgb(23,103,124)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(20,97,117)"
    ,colorMiddle = "rgb(56,161,187)"
    ,colorEnd = "rgb(3,22,56)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)

ui <- dashboardPage(
  dashboardHeader(title="Klasifikasi Desa",
                  tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/ashari-ramadhan-7607a3141/", icon("linkedin"), "Profil", target="_blank")),
                  tags$li(class="dropdown",tags$a(href="https://twitter.com/A_rmdhn23" ,icon("twitter"), "Twitter", target="_blank")),
                  tags$li(class="dropdown",tags$a(href="https://github.com/armdhn23/shinydashboard-sigi", icon("github"), "Source Code", target="_blank"))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Grafik",tabName="dashboard",icon=icon("area-chart")),
      menuItem("Prediksi",tabName="prediksi",icon=icon("dashboard")),
      menuItem("Download Data",tabName="download",icon=icon("download")),
      menuItem("Lihat Data",tabName="data",icon=icon("check-circle")))
  ),
  dashboardBody(theme_blue_gradient,
    tabItems(
      tabItem(tabName = "prediksi",
              h3("Input Nilai",align="center"),
            fluidRow(
              box(textInput("nama","Nama Desa"),width = "1000px")
                    ),
            fluidRow(
              box(width = "300px", height = "400px",
                numericInput("jp",h3("Jumlah Penduduk"),value = 0,min = 100,max = 11000),
                numericInput("tinggi",h3("Ketinggian (mdpl)"),value=0,min = 0,max = 1500),
                numericInput("ju",h3("Jumlah Usaha"),value = 0,min = 0,max = 500)
                 ),
              box(width = "300px",height = "400px",
                sliderInput("bts",h3("Jumlah BTS"),value = 0,min = 0,max = 5),
                sliderInput("jarak",h3("Jarak Ibu Kota Kecamatan-Kabupaten (km)"),
                            value = 0,min = 0,max = 130)
                 ),
              box(width = "300px",height = "400px",
                selectInput("pus", h3("Keberadaan Puskesmas"),
                             choices = list("Tidak ada" = "tidak ada", "Ada" = "ada"),
                                            selected = "ada"),
                selectInput("psr", h3("Keberadaan Pasar"),
                             choices = list("Tidak ada" = "tidak ada", "Ada" = "ada"),
                             selected = "ada"),
                selectInput("topologi", h3("Topologi"),
                             choices = list("Dataran" = "dataran",
                                            "Pegunungan" = "pegunungan","Perbukitan"= "perbukitan" ),
                             selected = 1),actionButton("goButton", "Predict")
                 ),
              box(h4("Hasil Klasifikasi Desa:",
                     inline=T ),br(),textOutput("clima"),
                  br(),width = "1000px")
                    )
              ),
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("jumlah_penduduk"),
                valueBoxOutput("jumlah_usaha"),
                valueBoxOutput("tinggi"),
                box(title = "Map Tree Klasifikasi Desa", status = "primary", solidHeader = TRUE,
                    collapsible = T,width = "1000px",
                    highchartOutput("map_tree_desa", width = "1000px"))
                ),
              fluidRow(
                box(title = "Pie Chart Pasar", status = "primary", solidHeader = TRUE,
                    collapsible = T,width = "300px",
                    highchartOutput("pie_pasar",width = "300px")),
                box(title = "Pie Chart Ketinggian",status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, width = "400px",
                    highchartOutput("pie_tinggi",width="400px")),
                box(title = "Pie Chart Puskesmas",status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,width = "300px",
                    highchartOutput("pie_puskesmas",width = "300px"))
                      ),
              fluidRow(
                box(title = "Bar Chart Jumlah Penduduk", status = "primary", solidHeader = TRUE,
                    collapsible = T,width = "330px",
                    highchartOutput("bar_jp",width = "330px")),
                box(title = "Bar Chart BTS", status = "primary", solidHeader = TRUE,
                    collapsible = T,width = "330px",
                    highchartOutput("bar_bts",width = "330px")),
                box(title = "Bar Chart Usaha", status = "primary", solidHeader = TRUE,
                    collapsible = T,width = "330px",
                    highchartOutput("bar_usaha",width = "330px"))
                      ),
              fluidRow(
                box(title = "Peta Ibu Kota Kecamatan-Kabupaten Sigi", status = "primary", solidHeader = TRUE,
                    collapsible = T, width = "1000px",height = "500px",
                    leafletOutput("peta", width = "1000px",height = "500px")
                      ))
                ),
      tabItem(tabName = "download",
              fluidRow(
                box(h4("Download Data Desa"),
                  downloadButton("data_desa", "Download")
              )),
              verbatimTextOutput("str_data_desa"),
              h3("Berikut data yang digunakan membuat Grafik: "),
              h3(""),
              fluidRow(
                box(h4("Download Data BTS"),
                    downloadButton("data_bts", "Download")
                )),
              verbatimTextOutput("str_data_bts"),
              fluidRow(
                box(h4("Download Data Jumlah Penduduk"),
                    downloadButton("data_jp", "Download")
                )),
              verbatimTextOutput("str_data_jp"),
              fluidRow(
                box(h4("Download Data Ketinggian"),
                    downloadButton("data_tinggi", "Download")
                )),
              verbatimTextOutput("str_data_tinggi"),
              fluidRow(
                box(h4("Download Data Usaha"),
                    downloadButton("data_usaha", "Download")
                )),
              verbatimTextOutput("str_data_usaha"),
              fluidRow(
                box(h4("Download Data untuk Peta"),
                    downloadButton("data_peta", "Download")
                )),
              verbatimTextOutput("str_data_peta")
              ),
      
      tabItem(tabName = "data",
              h2("Data Desa Kabupaten Sigi",align="center"),
              DT::dataTableOutput("tabel_data"))
    )
  )
)
server <- function(input, output) {
  desa<-read.csv("data/coba7.csv")
  
  output$jumlah_penduduk <- renderValueBox({
    desa_jp <- desa %>% 
      select(Desa, Jumlah.Penduduk) %>%
      filter(Jumlah.Penduduk == max(desa$Jumlah.Penduduk))
    desa_jp<- as.matrix(desa_jp)
    desa_jp<- desa_jp[1,1]
    desa_jp<- paste0("Jumlah Penduduk Terbanyak: ", desa_jp)
    valueBox(
      value = max(desa$Jumlah.Penduduk),
      subtitle = desa_jp,
      icon = icon("users")
    )
  })
  
  output$jumlah_usaha <- renderValueBox({
    desa_ju <- desa %>% 
      select(Desa, Jumlah.Usaha) %>%
      filter(Jumlah.Usaha == max(desa$Jumlah.Usaha))
    desa_ju<- as.matrix(desa_ju)
    desa_ju<- desa_ju[1,1]
    desa_ju<- paste0("Jumlah Usaha Terbanyak: ", desa_ju)
    valueBox(
      value = max(desa$Jumlah.Usaha),
      subtitle = desa_ju,
      icon = icon("money-bill-wave")
    )
  })
  
  output$tinggi <-renderValueBox({
    desa_tinggi <- desa %>% 
      select(Desa, Ketinggian) %>%
      filter(Ketinggian == max(desa$Ketinggian))
    desa_tinggi<- as.matrix(desa_tinggi)
    desa_tinggi<- desa_tinggi[1,1]
    desa_tinggi<- paste0("Desa Tertinggi (mdpl): ", desa_tinggi)
    valueBox(
      value = max(desa$Ketinggian),
      subtitle = desa_tinggi,
      icon = icon("tree")
    )
  })
  
output$map_tree_desa<-renderHighchart({
  klasifikasi_desa <- plyr::count(desa, "Klasifikasi.Desa")
  unique<- c(1,2,3)
  klasifikasi_desa<- data.frame(klasifikasi_desa, unique)
  
  hchart(klasifikasi_desa, "treemap", 
         hcaes(x = Klasifikasi.Desa, value = freq, color = unique))%>%
    hc_title(text = "Klasifikasi Desa Kabupaten Sigi",
             align = "center",
             style = list(fontWeight = "bold", fontSize = "20px"))
})

output$pie_pasar<-renderHighchart({
  pasar <- plyr::count(desa, "Pasar")
  tidak_ada<- 151/176*100
  tidak_ada<- format(round(tidak_ada, 2), nsmall = 2)
  tidak_ada<- paste0("Tidak ada ", tidak_ada,"%")
  ada<- 100-85.8
  ada<- paste0("Ada ", ada,"%")
  pasar$Pasar<- c(ada,tidak_ada)
  pasar %>%  hchart(type = 'pie', hcaes(Pasar, freq)) %>%  
    hc_title(text = "Keberadaaan Pasar Desa Kabupaten Sigi",
             align = "center",
             style = list(fontWeight = "bold", fontSize = "10px")) %>% 
    hc_tooltip(enabled = T) %>%  
    hc_subtitle(text = "Tahun 2019 
              (dalam Persen)",
                align = "center",
                style = list(fontWeight = "bold")) %>% 
    hc_add_theme(hc_theme_google()) %>% 
    hc_credits(enabled = T,text = "Jumlah Desa: 176 Desa")
})

output$pie_puskesmas<-renderHighchart({
  puskesmas <- plyr::count(desa, "Puskesmas")
  tidak_ada<- 158/176*100
  tidak_ada<- format(round(tidak_ada, 2), nsmall = 2)
  tidak_ada<- paste0("Tidak ada ", tidak_ada,"%")
  ada<- 100-89.77
  ada<- paste0("Ada ", ada,"%")
  puskesmas$Puskesmas<- c(ada,tidak_ada)
  puskesmas %>%  hchart(type = 'pie', hcaes(Puskesmas, freq)) %>%  
    hc_title(text = "Keberadaaan Puskesmas Desa Kabupaten Sigi",
             align = "center",
             style = list(fontWeight = "bold", fontSize = "10px")) %>% 
    hc_tooltip(enabled = T) %>%  
    hc_subtitle(text = "Tahun 2019 
              (dalam Persen)",
                align = "center",
                style = list(fontWeight = "bold")) %>% 
    hc_add_theme(hc_theme_google()) %>% 
    hc_credits(enabled = T,text = "Jumlah Desa: 176 Desa")
})

output$pie_tinggi<-renderHighchart({
  data6<-read.csv("data/tinggi.csv")
  data6 %>%  hchart(type = 'pie', hcaes(Dataran, Jumlah)) %>%  
    hc_title(text = "Persentase Ketinggian Desa Kabupaten Sigi",
             align = "center",
             style = list(fontWeight = "bold", fontSize = "10px")) %>% 
    hc_tooltip(enabled = T) %>%  
    hc_subtitle(text = "Tahun 2019 
              (dalam Persen)",
                align = "center",
                style = list(fontWeight = "bold")) %>% 
    hc_add_theme(hc_theme_google()) %>% 
    hc_credits(enabled = T,text = "Jumlah Desa: 176 Desa")
})

output$bar_jp<-renderHighchart({
  data2<-read.csv("data/JP.csv")
  highchart() %>% 
    hc_add_series(data2, colorByPoint = TRUE,"column",
                  hcaes(x = Penduduk, y = Jumlah), name = "Jumlah Desa") %>%
    hc_xAxis(categories = data2$Penduduk) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE)))%>%
    hc_title(text = "Banyak Desa Berdasarkan Jumlah Penduduk Kabupaten Sigi 2019",
             align = "center",
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_tooltip(enabled = T)
})

output$bar_bts<-renderHighchart({
  bts<-read.csv("data/BTS.csv")
  bts$BTS<-as.factor(bts$BTS)
  highchart() %>% 
    hc_add_series(bts, colorByPoint = TRUE,"column",
                  hcaes(x = BTS, y = Jumlah), name = "Jumlah Desa") %>%
    hc_xAxis(categories = bts$BTS) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE)))%>%
    hc_title(text = "Keberadaan BTS Desa Kabupaten Sigi 2019",
             align = "center",
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_tooltip(enabled = T)
  
})

output$bar_usaha<-renderHighchart({
  usaha<-read.csv("data/usaha.csv")
  highchart() %>% 
    hc_add_series(usaha, colorByPoint = TRUE,"column",
                  hcaes(x = Kategori, y = Jumlah), name = "Jumlah Desa") %>%
    hc_xAxis(categories = usaha$Kategori) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE)))%>%
    hc_title(text = "Jumlah Usaha Desa Kabupaten Sigi",
             align = "center",
             style = list(fontWeight = "bold", fontSize = "15px")) %>% 
    hc_tooltip(enabled = T)
  
})

output$peta<-renderLeaflet({
  peta<-read.csv("data/titik peta.csv")
  new <- c("red", "green","blue","orange")[peta$warna]
  
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = new)
  
  leaflet(peta) %>%
    addTiles() %>%
    addAwesomeMarkers (lng = ~lat,
                       lat = ~long,
                       popup = ~Wilayah,label=~as.character(Wilayah),icon = icons)

})

output$clima<-renderText({
  set.seed(11) #86
  
  Data1<-read.csv("data/coba7.csv")
  Data1<-Data1[-1]
  
  sampel<-sample(1:nrow(Data1),0.80*nrow(Data1),replace = FALSE)
  training<-data.frame(Data1)[sampel,]
  testing<-data.frame(Data1)[-sampel,] 

  training <- training[sample(nrow(training)),] 
  swasembada <- training[training$Klasifikasi.Desa == 'Swasembada', ] 
  swakarya <- training[training$Klasifikasi.Desa == 'Swakarya', ] 
  swadaya <- training[training$Klasifikasi.Desa == 'Swadaya', ] 
  
  dataswasembada <- swasembada[sample(nrow(swasembada)),] 
  foldsswasembada <- cut(seq(1,nrow(dataswasembada)), breaks = 10, labels = FALSE) 
  
  dataswakarya <- swakarya[sample(nrow(swakarya)),] 
  foldsswakarya <- cut(seq(1,nrow(dataswakarya)), breaks = 10, labels = FALSE) 
  
  dataswadaya <- swadaya[sample(nrow(swadaya)),] 
  foldsswadaya <- cut(seq(1,nrow(dataswadaya)), breaks = 10, labels = FALSE)
  
  akurasi_folds <- vector(length = 10) 
  akurasi_testing <- vector(length = 10) 
  akurasi_data <- vector(length = 10) 
  
  for(i in 1:10){
    indexswasembada <- which(foldsswasembada==i, arr.ind = TRUE)   
    indexswakarya <- which(foldsswakarya==i, arr.ind = TRUE)   
    indexswadaya <- which(foldsswadaya==i, arr.ind = TRUE)   
    testDataswasembada <- dataswasembada[indexswasembada, ]   
    trainDataswasembada <- dataswasembada[-indexswasembada, ]   
    testDataswakarya <- dataswakarya[indexswakarya, ]   
    trainDataswakarya <- dataswakarya[-indexswakarya, ]   
    testDataswadaya <- dataswadaya[indexswadaya, ]   
    trainDataswadaya <- dataswadaya[-indexswadaya, ] 
    testData <- join_all(list(testDataswadaya,testDataswakarya,testDataswasembada), type = 'full') 
    trainData <- join_all(list(trainDataswadaya,trainDataswakarya,trainDataswasembada), type = 'full')
    assign(paste0("dataTes",i), testData)
    assign(paste0("dataTrain",i), trainData)
    #model
    modelnya <- C5.0(Klasifikasi.Desa~., data=trainData)
    assign(paste0("Model",i), modelnya)
    #### data folds
    prediksinya <- (predict(modelnya, testData))
    temp <- table(prediksinya, testData$Klasifikasi.Desa)
    akurasi <- ((temp[1,1]+temp[2,2]+temp[3,3])/sum(temp))*100
    akurasi_folds[i] <- akurasi
    ##### data testing
    prediksinya1 <- predict(modelnya,testing)
    temp1 <- table(prediksinya1, testing$Klasifikasi.Desa)
    akurasitesting <- ((temp1[1,1]+temp1[2,2]+temp1[3,3])/sum(temp1))*100
    akurasi_testing[i] <- akurasitesting
    ##### data semua
    prediksinya2 <- predict(modelnya,Data1)
    temp2 <- table(prediksinya2, Data1$Klasifikasi.Desa)
    akurasidata <- ((temp2[1,1]+temp2[2,2]+temp2[3,3])/sum(temp2))*100
    akurasi_data[i] <- akurasidata
    #### confusion matrix
    assign(paste0("Hasil_folds",i), confusionMatrix(prediksinya, testData$Klasifikasi.Desa)) 
    assign(paste0("Hasil_testing",i), confusionMatrix(prediksinya1, testing$Klasifikasi.Desa)) 
    assign(paste0("Hasil_data",i), confusionMatrix(prediksinya2, Data1$Klasifikasi.Desa))
  } 
  input$goButton
  Jumlah.Penduduk<-isolate(c(input$jp))
  Ketinggian<-isolate(c(input$tinggi))
  Puskesmas<-isolate(c(input$pus))
  Puskesmas<-isolate(as.factor(Puskesmas))
  Pasar<-isolate(c(input$psr))
  Pasar<-isolate(as.factor(Pasar))
  Jumlah.Usaha<-isolate(c(input$ju))
  Topologi<-isolate(c(input$topologi))
  Topologi<-isolate(as.factor(Topologi))
  Jarak<-isolate(c(input$jarak))
  BTS<-isolate(c(input$bts))
  
  databaru<-data.frame(Jumlah.Penduduk,Ketinggian,Puskesmas,Pasar,Jumlah.Usaha,Topologi,Jarak,BTS)
  databaru$Puskesmas<-as.factor(databaru$Puskesmas)
  databaru$Pasar<-as.factor(databaru$Pasar)
  databaru$Topologi<-as.factor(databaru$Topologi)
  hasil<- predict(Model5,databaru)
  hasil<- as.integer(hasil)
  ###nama desa
  input$goButton
  nama_desa<-isolate(input$nama)
  
  if (hasil == 3) { 
    hasil1="Swasembada"
  } else if (hasil == 2) {
    hasil1="Swakarya"
  } else {
    hasil1="Swadaya"
  }
  print(paste0(nama_desa," termasuk desa ",hasil1))
})

#output$nama_desa<-renderText({
 # input$goButton
  #nama_desa<-isolate(input$nama)
  #nama_desa
#})

output$tabel_data<- DT::renderDataTable({
  tabel_data<-read.csv("data/coba7.csv")
  tabel_data
})

output$str_data_desa<- renderPrint({
  desa_data<-read.csv("data/coba7.csv")
  str(desa_data)
})

output$str_data_bts<- renderPrint({
  bts_data<-read.csv("data/BTS.csv")
  bts_data$BTS<-as.factor(bts_data$BTS)
  str(bts_data)
})

output$str_data_jp<- renderPrint({
  jp_data<-read.csv("data/JP.csv")
  str(jp_data)
})

output$str_data_tinggi<- renderPrint({
  tinggi_data<-read.csv("data/tinggi.csv")
  str(tinggi_data)
})

output$str_data_usaha<- renderPrint({
  usaha_data<-read.csv("data/usaha.csv")
  str(usaha_data)
})

output$str_data_peta<- renderPrint({
  peta_data<-read.csv("data/titik peta.csv")
  str(peta_data)
})

tabel_data_desa<-read.csv("data/coba7.csv")
tabel_data_bts<- read.csv("data/BTS.csv")
tabel_data_jp<- read.csv("data/JP.csv")
tabel_data_tinggi<- read.csv("data/tinggi.csv")
tabel_data_usaha<- read.csv("data/usaha.csv")
tabel_data_peta<- read.csv("data/titik peta.csv")

output$data_desa <- downloadHandler(
  filename = function() {
    paste("data-desa-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(tabel_data_desa, file)
  }
)

output$data_bts <- downloadHandler(
  filename = function() {
    paste("data-bts-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(tabel_data_bts, file)
  }
)

output$data_jp <- downloadHandler(
  filename = function() {
    paste("data-jumlah_penduduk-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(tabel_data_jp, file)
  }
)

output$data_tinggi <- downloadHandler(
  filename = function() {
    paste("data-tinggi-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(tabel_data_tinggi, file)
  }
)

output$data_usaha <- downloadHandler(
  filename = function() {
    paste("data-usaha-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(tabel_data_usaha, file)
  }
)


output$data_peta <- downloadHandler(
  filename = function() {
    paste("data-peta-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(tabel_data_peta, file)
  }
)
}


shinyApp(ui, server)
