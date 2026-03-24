DecodeDigits <- function(word, alphabet = c(0:9, LETTERS)) {
  digits <- unlist(strsplit(toupper(word), NULL))
  return(match(digits, alphabet)-1)
}

#' Décode une chaîne de caractère word représentant un nombre n écrit dans une base comprise entre 2 et 36.
#'
#' @param word la chaîne de caractère représentant le nombre ne contenant que des caractères alphanumériques.
#' @param base la base dans laquelle est écrite le nombre
#' @return le nombre n ou NA si le format de la chaîne est invalide.

DecodeNumber <- function(word, base) {
  if (base<2 || base>36) return (NA)
  
  digits<-DecodeDigits(word) #donne un vecteur compose de 
  if (any(is.na(digits)) || any(digits >=base)) {
    return(NA)
  }
  n <- 0
  for (d in digits) {
    n <- n * base + d
  }
 return(n) 
}

#' Écrit le nombre n dans une chaîne de caractères en utilisant une base comprise entre 2 et 36
#' @param n le nombre à encoder
#' @param base la base dans laquelle écrire le nombre
#' @return la chaîne de caractère contenant le nombre n écrit dans la base
EncodeNumber <- function(n, base,alphabet=c(0:9,LETTERS)){
  if (base<2 || base >length((alphabet))) return(NA)
  
  if (n==0) return(alphabet[1])  #cas special
  
  digits<-character(0)  #vecteur vide pour stocker les res
  while (n>0){
    last<-n%%base
    digits<-c(alphabet[last+1],digits) #append works mais plus lente
    n<-n%/%base
  }
  
  return(paste(digits,collapse = ""))
}

#' Écrit un nombre n dans une chaîne de caractère avec le système bibi-binaire
#' @param n le nombre à encoder
#' @return la chaîne de caractère contenant le nombre n écrit en bibi-binaire
EncodeBibi <- function(n) {

  
  bibi<- c("HO","HA","HE","HI", 
           "BO","BA","BE","BI",
           "KO","KA","KE","KI",
           "DO","DA","DE","DI")
  
  return(EncodeNumber(n, 16, bibi))
}

library(shiny)
#' La fonction server d'une application shiny réalise le traitement des données et la génération des graphiques/tableaux.
server <- function(input, output) {
  CheckBase <- function(base) base >= 2 && base <= 36
  fromBase <- reactive( {
    validate(
      need(!is.na(input$fromBase), "Base d'origine manquante"),
      need(CheckBase(input$fromBase), "Base d'origine doit être entre 2 et 36.")
    )
    input$fromBase
  })
  number <- reactive( {
    number <- trimws(input$number)
    validate(
      need(nchar(number) > 0, "Pas de nombre en entrée."),
      need(!grepl("[^a-zA-Z0-9]", number), "Format de nombre incorrect")
    )
    number <- DecodeNumber(number, fromBase())
    validate(
      need(!is.na(number), "Chiffres invalides dans le nombre.")
      )
    number
  })

  ConvertNumber <- function(n, base) {
    validate(
      need(!is.na(base), "Base de destination manquante")
    )
    if( CheckBase(base) ) {
      return(EncodeNumber(n, base))
    } else {
      return(EncodeBibi(n))
    }
  }
  output$toBase1 <- renderText({ ConvertNumber(number(), input$toBase1)})
  output$toBase2 <- renderText({ ConvertNumber(number(), input$toBase2)})
  output$toBase3 <- renderText({ ConvertNumber(number(), input$toBase3)})
  output$toBibi <- renderText({ EncodeBibi(number())})
}

#' La fonction ui d'une application shiny construit l'interface graphique à partir de ses entrées/sorties.
ui <- fluidPage(
  titlePanel("Convertisseur à Bibi"),
  sidebarLayout(
    ## Barre latérale contenant les entrées de l'application
    sidebarPanel(
      textInput("number", "Nombre", "26"),
      numericInput("fromBase", "Depuis la base:", "10")
    ),
    ## Panneau principal contenant les sorties de l'application
    mainPanel(
      column(3,
             numericInput("toBase1", "Vers la base:", "10"),
             verbatimTextOutput("toBase1")
             ),
      column(3,
             numericInput("toBase2", "Vers la base:", "2"),
             verbatimTextOutput("toBase2")
             ),
    column(3,
           numericInput("toBase3", "Vers la base:", "16"),
           verbatimTextOutput("toBase3")
           ),
    column(3,
           h4("Bibi-binaire"),
           verbatimTextOutput("toBibi"))
    )
  )
)

## Construit un objet représentant l'application
shinyApp(ui = ui, server = server)
