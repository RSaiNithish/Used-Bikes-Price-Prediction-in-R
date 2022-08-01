library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage( 
    # Application title
    titlePanel("Used Bike Price Prediction"),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel( class = "sidepanel", width = 4,
            textInput("m_name", h4("Model Name"), 
                    placeholder = "Enter the bike's name..."),
            textInput("city", h4("Location"),
                      placeholder = "Enter the Location..."),
            sliderInput("kms", h4("Kilometers driven"), 
                     value = 5000, min = 0, max = 1000000),
            selectInput("owner", h4("Owner"),
                    choices = list("First" = 1, "Second" = 2, "Third" = 3, "Fourth or more" = 4)),
            sliderInput("age", h4("Age of the vehicle"), 
                         value = 5, min = 1, max = 16),
            sliderInput("power", h4("Power"), 
                     value = 20, min = 7, max = 198),
            sliderInput("engine", h4("Engine (cc)"), 
                         value = 100, min = 100, max = 1800),
            sliderInput("mileage", h4("Mileage"), 
                         value = 15, min = 5, max = 104),
            textInput("brand", h4("Brand"), 
                      placeholder = "Enter the Brand name..."),
            actionButton("predict","Predict"),
    
            tags$style(HTML("
                @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
                
                body {
                    background-color: black;
                    font-family: 'Yusei Magic', sans-serif;
                }
                
                .sidepanel{
                    overflow-y: auto;
                    position: relative;
                }
                
                .container-fluid{
                    font-style: bold;
                    text-align: center;
                    background-color: black;
                    color: pink;
                }
                
                .col-sm-8{
                    color: black;
                }
                
                .row{
                    font-style:;
                    font-family: 'Yusei Magic', sans-serif;
                    text-align: left ;
                    background-color: #FFEFF2;
                }
                
                .control-label{
                    font-size: 35px;
                }
                
                .well{
                    color: #191919;
                    background-color: pink;
                    overflow-y:auto;
                    position: relative;
                }
                
                .form-control shiny-bound-input{
                    color: linen;
                }
                
                .btn{
                    color: white;
                    background-color: #0076FF;
                }
              "))

        ),

        # Show a plot of the generated distribution
        mainPanel(
          h3("Price of the bike you are looking:") ,
          textOutput("pred_price"),
          
          tags$style(HTML("
              h3{
                  text-align: center;
                  position: relative;
                  top: 400px;
              }
              .shiny-text-output{
                  text-align: center;
                  font-size: 60px;
                  position: relative;
                  top: 415px;
              }
          "))
        )
    )
)

server <- function(input, output) {
  
  observeEvent(input$predict,{
      output$pred_price <- renderText({
          city_tier <- 3
          city <- input$city
        
          tier_1 <- c("Ahmedabad", "Bangalore", "Chennai", "Delhi", "Hyderabad", "Kolkata", "Mumbai", "Pune")
          tier_2 <- c("Agra", "Ajmer", "Aligarh", "Amravati", "Amritsar", "Asansol", "Aurangabad", "Bareilly", "Belgaum", "Bhavnagar", "Bhiwandi", "Bhopal", "Bhubaneswar","Bikaner", "Bilaspur", "Bokaro Steel City","Chandigarh", "Coimbatore", "Cuttack"," Dehradun","Dhanbad", "Bhilai","Durgapur", "Erode", "Faridabad", "Firozabad", "Ghaziabad","Gorakhpur", "Gulbarga", "Guntur", "Gwalior", "Gurugram", "Guwahati", "Hamirpur", "Hubli–Dharwad", "Indore", "Jabalpur", "Jaipur", "Jalandhar", "Jalgaon", "Jammu", "Jamnagar", "Jamshedpur", "Jhansi", "Jodhpur","Navi Mumbai" ,"Kakinada", "Kannur"," Kanpur", "Karnal", "Kochi"," Kolhapur", "Kollam", "Kozhikode", "Kurnool", "Ludhiana","Lucknow","Madurai", "Malappuram"," Mathura", "Mangalore", "Meerut", "Moradabad", "Mysore", "Nagpur"," Nanded"," Nashik", "Nellore","Navi Mumbai","Noid", "Patna", "Puducherry", "Purulia", "Prayagraj", "Raipur","Rajkot", "Rajamahendravaram"," Ranchi", "Rourkela", "Ratlam", "Salem", "Sangli", "Shimla", "Siliguri"," Solapur", "Srinagar", "Surat", "Thanjavur", "Thiruvananthapuram", "Thrissur", "Tiruchirappalli", "Tirunelveli", "Tiruvannamalai", "Ujjain", "Vijayapura", "Vadodara", "Varanasi", "Vasai-Virar City", "Vijayawada", "Visakhapatnam", "Vellore", "Warangal")
        
          if (city %in% tier_1){
            city_tier <- 1
          }
          else if (city %in% tier_2){
            city_tier <- 2
          }
          else{
            city_tier <- 3
          }
      
          pred_model <- readRDS("LM.rds")
        
          paste("₹",round(predict(pred_model, data.frame(city_tier = city_tier,engine = input$engine, kms_driven = input$kms, owner = as.numeric(input$owner), age = input$age, mileage = input$mileage, power = input$power)),digits=3))
      })})
}

# Run the application 
shinyApp(ui = ui, server = server)
