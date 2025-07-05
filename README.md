# 🎓 Test Tapşırıqları Format Nümayişi

## 📖 Haqqında
Bu interaktiv Shiny tətbiqi müxtəlif test formatlarını nümayiş etdirir və onların təlim prosesindəki rolunu göstərir.

## ✨ Xüsusiyyətlər
- 🔘 7 müxtəlif test formatı (Multiple Choice, Multiple Response, və s.)
- ⏱️ Real-time timer sistemi
- 📊 İnteraktiv qiymətləndirmə və statistik analiz
- 📈 Qrafik və vizuallaşdırma
- 📥 CSV eksport funksiyası

## 🚀 Demo
Canlı demo: [Tətbiqi Burada İşlədin](https://t01061954.shinyapps.io/test-formatlari/)
## 📂 GitHub Repository  
[Kodlara baxın](https://github.com/Ttariyel-1954/test-tapshiriqlari-formati)

## 💻 Lokal İstifadə
```r
# Lazımi paketləri yükləyin
install.packages(c("shiny", "shinydashboard", "DT", "plotly", 
                   "shinyWidgets", "shinycssloaders", "ggplot2", "dplyr"))

# Tətbiqi işə salın
shiny::runApp("index.R")