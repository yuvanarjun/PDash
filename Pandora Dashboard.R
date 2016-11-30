#Author: Arjun Mahalingam
#Pandora Dashboard

.libPaths("C:/Users/mahalingama/AppData/Local/Temp/RtmpuCxl47/downloaded_packages")

#Load required R packages
library(shiny)
library(jsonlite)
library(RCurl)
library(varhandle)
library(ggplot2)
library(maps)
library(leaflet)
library(reshape)
library(plyr)
library(DT)
library(plotly)

#Define a function to round digits
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

#Read data from Pandora REST API
json_file <- getURL("https://pan-api.cambridgeriskframework.com/api/exceedance_probability/?format=json&runmodel=96&phasenumber=1&sectionnumber=1",userpwd="crfadmin:CRFCentury2188")
json_file_1 <- fromJSON(json_file)
json_file <- getURL("https://pan-api.cambridgeriskframework.com/api/portfolio_nodes_results/?format=json&runmodel=96&phasenumber=1&sectionnumber=1",userpwd="crfadmin:CRFCentury2188")
json_file_2 <- fromJSON(json_file)

#Convert NULL values to NA and then unlist the json list file created above
json_file_1 <- lapply(json_file_1, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})
json_file_2 <- lapply(json_file_2, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})


#Convert list to a data frame and convert factors to appropriate datatypes
data_EP <- data.frame(do.call("cbind", json_file_1)) #EP Curve dataset
data_EP <- unfactor(data_EP)
data_PNR <- data.frame(do.call("cbind", json_file_2)) #Portolio Nodes Result dataset
data_PNR <- unfactor(data_PNR)

##Get the location coordinates from the Portolio Nodes Result dataset, split coordinates data into lat and lon, rename columns
df <- data.frame(city = matrix(unlist(regmatches(data_PNR$locationwkt, gregexpr("(?<=\\().*?(?=\\))", data_PNR$locationwkt, perl=T))),byrow=T),stringsAsFactors=FALSE)
df <- ldply(strsplit(df$city, " "))
colnames(df) <- c("lon", "lat")
df$lon <- as.numeric(df$lon)
df$lat <- as.numeric(df$lat)
data_PNR <- cbind(data_PNR,df)
data_PNR$popup_info <- paste0("City: ",data_PNR$name, "<br> Percent GDP@Risk Rank: ",data_PNR$ranking_of_percent_gdp_at_risk_from_all_threat, "<br> Overall GDP@Risk Rank: ",data_PNR$ranking_of_usd_bn_gdp_at_risk_from_all_threat)
names(data_PNR) <- gsub("\\_"," ",names(data_PNR))
names(data_PNR) <- toupper(names(data_PNR))
data_PNR$NAME <- iconv(data_PNR$NAME,from="UTF-8",to="WINDOWS-1252") #Change text encoding of city names to WINDOWS 1252 format

#Convert appropriate fields to numeric format
to_numeric <- names(data_PNR)[grep("RANKING OF PERCENT|GDP 2|POP |GDP AT RISK USD|PERCENT OF|RANKING OF USD", names(data_PNR))]
data_PNR[,to_numeric] <- sapply(data_PNR[,to_numeric],as.numeric)
data_PNR <- round_df(data_PNR,3)

#Order dataframe alphabetically by column name
data_PNR <- data_PNR[,order(names(data_PNR))]

#Load choices for the set of all cities and threats in the database
city_choices <- setNames(1:nrow(data_PNR),data_PNR$NAME)
threat_choices <- setNames(1: sum(grepl(" GDP AT RISK USD BN", names(data_PNR))), as.character(gsub(" GDP AT RISK USD BN","",names(data_PNR)[grep(" GDP AT RISK USD BN", names(data_PNR))])))

#Intialize the LIS information for each threat as a dataframe
LIS_data <- data.frame(matrix(c(
  "CYBER CATASTROPHE","CY1","A sporadic set of technology failures. (E.g. GDP outages, accidental technical faults, cyber attacks on individual organisations, reduces outputs of companies with high dependence on technology, and consumer confidence is affected)",
  "CYBER CATASTROPHE","CY2","Systemic cyber attack. (E.g. Sybil logic bomb, causes heavy losses to many commercial companies operating in that city and undermines confidence of general public in IT systems in general)",
  "CYBER CATASTROPHE","CY3","Cyber attacks on critical infrastructure destroys the power distribution grid and causes power loss in the city for many months",
  "DROUGHT","DR1","Severe Drought - Localised drought causes water consumption restrictions for that city for six months, resulting in water rationing for businesses and residential. Water is prioritized for industry, agriculture and emergency provision",
  "DROUGHT","DR2","Extreme Drought - Three successive seasons of record levels of below average rainfall results in major water shortages for several years",
  "DROUGHT","DR3","Exceptional Drought - Sustained for several years. Major change in precipitation patterns causes extended drought, which results in severe water consumption restrictions for several years",
  "EARTHQUAKE","EQ1","A 'Large Magnitude Earthquake' (Ms6.5) within the city boundaries. Centroid of city experiences VII (PGA 250 - 400)",
  "EARTHQUAKE","EQ2","A 'Great Earthquake' (Ms7.0) with its epicentre close to the edge of the city, just outside its boundaries. Centroid of city experiences VIII (PGA 400 - 600)",
  "EARTHQUAKE","EQ3","A 'Great Earthquake' (Ms7.5) occurring at shall depth with its epicentre close to the centre of the city. Centroid of city experiences IX (PGA 600 - 1,000)",
  "FLOOD","FL1","10% of city affected by flooding, reaching 1m depth in parts, with low velocity water and a recovery period that is several months long",
  "FLOOD","FL2","25% of city area affected by flood waters that reach over 3m depth (more than one storey) in parts, with medium velocity water moderately contaminated",
  "FLOOD","FL3","Over 50% of city land area affected by flooding, reaching more than two storeys in parts, high velocity destructive water flows and highly polluted waters",
  "FREEZE","FR1","Freeze of up to five degrees below 0°C for three weeks (-20 to 100 Degree-days) with some snow and ice, moderate winds",
  "FREEZE","FR2","Freeze of up to 10 degrees below 0°C for eight weeks, combined with deep snow and high winds",
  "FREEZE","FR3","Freeze of up to 20 degrees below 0°C for 12 weeks, combined with heavy snow and severe ice loads periodically",
  "HEATWAVE","HW1","Heatwave of 1 - 5°C above 32°C for four weeks (20 to 100 Degree-days)",
  "HEATWAVE","HW2","Heatwave of 5 - 8°C above 32°C for eight weeks (50 to 500 Degree-days)",
  "HEATWAVE","HW3","Heatwave of 8 - 12°C above 32°C for 16 weeks (112 to 1,300 Degree-days)",
  "HUMAN PANDEMIC","HE1","Localised epidemic of new emergent disease with case fatality rate (CFR) of 10% causes public health emergency and fear in population, leads to a loss of tourism trade",
  "HUMAN PANDEMIC","HE2","Pandemic influenza virus infects 43% of the population, with CFR of 0.3%",
  "HUMAN PANDEMIC","HE3","Pandemic of high fatality disease, with CFR of 3%",
  "INTERSTATE WAR","IW1","City mobilized for war, but not attacked; mobilization switches civilian commerce to military production; population gripped by fear, consumer demand drops, parts of population flees. Investor confidence is affected; Conflict lasts a year",
  "INTERSTATE WAR","IW2","City suffers sporadic attack from occasional missiles or aerial bombardment, possible damage to city infrastructure from military cyber attack; City is mobilized for war; significant emigration of population from city. Investors withdraw",
  "INTERSTATE WAR","IW3","City is the target of strategic bombing by energy forces, destroying industrial and commercial output and military facilities in the city; Major emigration by population. Possible rebuilding afterwards by major injection of capita. Conflict lasts three years",
  "MARKET CRASH","MC1","Stock market index drops 10% peak-to-trough within a single year (E.g. Asian Crisis, 1997)",
  "MARKET CRASH","MC2","Stock market index drops 50% peak-to-trough within a single year (E.g. Great Financial Crisis, 2008)",
  "MARKET CRASH","MC3","Stock market index drops 85% peak-to-trough within a single year (E.g. Wall Street Crash, 1929)",
  "NUCLEAR MELTDOWN","NP1","City receives radioactive fallout of >0.01 Bq/km2 or 0.3 Curies of C137 (I.e. Similar to within 200km of Chernobyl 1986 or 120km of Fukushima 2011)",
  "NUCLEAR MELTDOWN","NP2","City receives radioactive fallout of >0.1 Bq/km2 or 3 Curies of C137 (I.e. Similar to within 70km of Chernobyl 1986 or 50km of Fukushima 2011)",
  "NUCLEAR MELTDOWN","NP3","City receives radioactive fallout of >1 Bq/km2 or 30 Curies of C137 (I.e. Similar to within 30km of Chernobyl 1986)",
  "OIL PRICE SHOCK","OP1","Sudden increase in oil price by 10%",
  "OIL PRICE SHOCK","OP2","Sudden increase in oil price by 25% (Similar to Oil Price Crisis of 1974)",
  "OIL PRICE SHOCK","OP3","Sudden increase in oil price by 50%",
  "PLANT EPIDEMIC","PE1","Localised plant epidemic affects prices of staple foods in city markets",
  "PLANT EPIDEMIC","PE2","National plant epidemic affects price of staple foods in city markets",
  "PLANT EPIDEMIC","PE3","International plant epidemic affects price of staple foods in city markets",
  "POWER OUTAGE","PO1","One City-Day of Power Loss (100% of city loses power for 11 day or 50% of city loses power for 2 days, etc.)",
  "POWER OUTAGE","PO2","A 5-City-Day event (100% of city loses power for 5 days, 50% of city loses power for 10 days, etc.)",
  "POWER OUTAGE","PO3","A 10-City-Day event (100% of city loses power for 10 days)",
  "SEPARATISM","SP1","Civil Unrest causes riots and protests in the streets for months; violent confrontations with police",
  "SEPARATISM","SP2","Incidents of sectarian fighting between armed gangs and private militias in the streets of the city for multiple years",
  "SEPARATISM","SP3","Civil war involves months of street fighting between well-organized and well-equipped armies, using heavy weaponry in sectarian divide in country",
  "SOCIAL UNREST","SU1","Civil Unrest causes riots and protests in the streets for months; violent confrontations with police",
  "SOLAR STORM","SS1","NOAA Space Weather Scale for radiation storms level S4 and equivalent to a solar flare of X20. Radiation hazard to passengers and crew in commercial jets at high latitudes (approximately 10 chest x-rays). Satellite systems experience memory device problems and noise on imaging systems, GPS navigation systems prone to error, blackout of HF radio communications. Some low level electrical interference and voltage control problems. 3-5 days of disruption caused",
  "SOLAR STORM","SS2","NOAA Space Weather Scale for radiation storms level S5 and equivalent to a solar flare of X40 (Similar to 'Carrington Event'; Radiation hazard to passengers and crew in commercial jets at high latitudes (approximately 10 chest x-rays). Satellite systems experience memory device problems and noise on imaging systems, GPS navigation systems prone to error, blackout of HF radio communications. Some low level electrical interference and voltage control problems. 3-5 days of disruption caused",
  "SOLAR STORM","SS3","NOAA Space Weather Scale for radiation storms level S6+ (Beyond 5-point NOAA Scale). Estimated effects of a solar flare of X60 - also known as a class Z event. High radiation exposure to passengers and crew in commercial jets at high latitudes (approximately 100 chest x-rays).Satellites rendered useless, GPS navigation systems fail, serious noise on imaging systems. Telecommunication systems fail. Widespread voltage control problems and protective system problems can occur, some grid systems may experience complete collapse or blackouts. Transformers may experience damage. Several weeks of disruption caused before systems back online",
  "SOVEREIGN DEFAULT","SD1","Country defaults and reschedules its debt, devalues its currency substantially; Investors flee. National economy loses substantial foreign direct investment",
  "TEMPERATE WINDSTORM","HU1","Category 1 Hurricane: Windspeed 118 - 153km/hr",
  "TEMPERATE WINDSTORM","HU2","Category 3 Hurricane: Windspeed 178 - 209km/hr",
  "TEMPERATE WINDSTORM","HU3","Category 5 Hurricane: Windspeed >250km/hr",
  "TERRORISM","TR1","Terror campaign with small arms and limited resources. (E.g. Shootings, poisonings, food chain sabotage, etc., with repeated attacks over a period of many months that causes fear and distrust in urban population.)",
  "TERRORISM","TR2","Well resourced and organised terrorist attacks on high profile targets. (E.g. Major truck bombings, airplanes into buildings or other surprise destructive events, causes horrific loss of life and major destruction to property in and around city centre.)",
  "TERRORISM","TR3","WMD Terrorist Attack - City is attacked by sophisticated terrorist operation using weapons of mass destruction (WMD). (E.g. Anthrax, air-dispersed bio-weapons, chemical or radioactive contaminant, or small yield nuclear detonation kills large numbers of people and contaminates many buildings in the Central Business District.)",
  "TROPICAL WINDSTORM","HU1","Category 1 Hurricane: Windspeed 118 - 153km/hr",
  "TROPICAL WINDSTORM","HU2","Category 3 Hurricane: Windspeed 178 - 209km/hr",
  "TROPICAL WINDSTORM","HU3","Category 5 Hurricane: Windspeed >250km/hr",
  "TSUNAMI","TS1","Tsunami with 3m run-up",
  "TSUNAMI","TS2","Tsunami with 6m run-up",
  "TSUNAMI","TS3","Tsunami with 12m run-up",
  "VOLCANIC ERUPTION","VE1","Ashcloud shuts city for extended period, and covers it with several centimeters of ash, preventing air travel, road traffic, port functions and normal business activity",
  "VOLCANIC ERUPTION","VE2","Ashcloud covers city to 1m depth, entailing lengthy recovery process",
  "VOLCANIC ERUPTION","VE3","Parts of city impacted by direct effects of volcanic eruption (pyroclastic gases, lahar flows, etc.). City evacuated and population not allowed to return for some time"
), ncol=3, byrow=TRUE))
names(LIS_data) <- c("Threat","LIS","Description")



#Shiny User Interface (SHINY UI)
ui <- fluidPage(
  tabsetPanel(
    tabPanel("City Level Data",
             fluidRow(
               column(2, offset=0, selectInput("select_city", label = h3("City selection"), 
                                               choices = city_choices,
                                               selected = 161)), #Choosing London as default
               
               column(2, offset=0, selectInput("select_threat", label = h3("Threat selection"),
                                               choices = threat_choices,
                                               selected = 1)),
               
               column(2, offset=0, selectInput("select_category", label = h3("Category selection"), 
                                               choices = list("All Categories" = 1, "Assessment Grade" = 2, "Economic" = 3, "GDP@Risk: GDP" = 4, "GDP@Risk: Percentage" = 5, "Risk Ranking: GDP" = 6, "Risk Ranking: Percentage" = 7), 
                                               selected = 1)),
               
               column(3, offset=3, br(), img(src="http://people.ds.cam.ac.uk/ar808/assets/images/jbs-logo.gif"))
               
             ),
             
             fluidRow(tags$br()),
             
             fluidRow(
               column(4, offset = 0, DT::dataTableOutput("city_table")),
               column(8, leafletOutput("map", width = "100%", height=800), actionButton("reset_button", "World view"), actionButton("currentthreat_layer", "Current threat"))
               
             ),
             
             fluidRow(renderText(""), tags$br(),tags$br()),
             
             fluidRow(
               # splitLayout(cellWidths = c("33%", "33%","33%"), plotlyOutput("cityrankchart"), plotlyOutput("gdpchart"), plotlyOutput("riskrankchart"))
               column(4, offset = 0, plotlyOutput("cityrankchart",width = "60%", height = 400)),
               column(4, offset = 0, plotlyOutput("gdpchart",width = "60%", height = 400)),
               column(4, offset = 0, plotlyOutput("riskrankchart",width = "60%", height = 400))
             ),
             
             fluidRow(tags$br(),tags$br())
    ),
    
    
    tabPanel("EP Curve",
             fluidRow(
               column(5, offset= 2," ",tags$p(),tags$br(),
                      actionButton(inputId = "generate", label = "Generate"),tags$hr())
             ),
             fluidRow(
               column(8, offset=2, plotlyOutput("graph"))
             )
    ),
    
    tabPanel("Portfolio Impacts", tags$br(), tags$br(),
             fluidRow(
               column (3, offset =1, fileInput("portfolio_file","Choose portfolio file to upload (in csv)", accept= c('text/csv', 
                                                                                                                      'text/comma-separated-values',
                                                                                                                      'text/tab-separated-values',
                                                                                                                      'text/plain',
                                                                                                                      '.csv',
                                                                                                                      '.tsv')))
             )
    ),
    
    tabPanel("Definitions",fluidRow())
    
  )
)


#Shiny server code (SHINY SERVER)
server <- function(input, output,session) {
  
  #Display City Table
  
  temp <- reactive ({
    if(input$select_threat == 1)
    {
      if (input$select_category == 1) temp <- grepl("THREAT ASSESSMENT GRADE|GDP 2|POP |GDP AT RISK USD|PERCENT OF|RANKING OF USD", names(data_PNR))
      else if (input$select_category == 2) temp <- grepl("THREAT ASSESSMENT GRADE", names(data_PNR))
      else if (input$select_category == 3) temp <- grepl("GDP 2|POP ", names(data_PNR))
      else if (input$select_category == 4) temp <- grepl("GDP AT RISK USD", names(data_PNR))
      else if (input$select_category == 5) temp <- grepl("PERCENT OF", names(data_PNR))
      else if (input$select_category == 6) temp <- grepl("RANKING OF USD", names(data_PNR))
    }
    
    else
    {
      if (input$select_category == 1) temp <- grepl(names(threat_choices)[as.numeric(input$select_threat)], names(data_PNR))
      else if (input$select_category == 2) temp <- grepl("THREAT ASSESSMENT GRADE", names(data_PNR)) & grepl(names(threat_choices)[as.numeric(input$select_threat)], names(data_PNR))
      else if (input$select_category == 3) temp <- grepl("GDP 2|POP ", names(data_PNR))
      else if (input$select_category == 4) temp <- grepl("GDP AT RISK USD", names(data_PNR)) & grepl(names(threat_choices)[as.numeric(input$select_threat)], names(data_PNR))
      else if (input$select_category == 5) temp <- grepl("PERCENT OF", names(data_PNR)) & grepl(names(threat_choices)[as.numeric(input$select_threat)], names(data_PNR))
      else if (input$select_category == 6) temp <- grepl("RANKING OF USD", names(data_PNR)) & grepl(names(threat_choices)[as.numeric(input$select_threat)], names(data_PNR))
      else temp <- grepl("RANKING OF PERCENT", names(data_PNR)) & grepl(names(threat_choices)[as.numeric(input$select_threat)], names(data_PNR))
    }
  })
  output$city_table <- DT::renderDataTable(DT::datatable(t(data_PNR[input$select_city,temp()]), options = list(lengthMenu = c(10, 15, 25, 50), pageLength = 15)))
  
  #data_PNR$popup_new <- reactive(paste0("City: ",data_PNR$NAME, "<br> Percent GDP@Risk Rank: ",data_PNR[,paste0("RANKING OF PERCENTAGE GDP AT RISK FROM ",names(threat_choices)[as.numeric(input$select_threat)]," THREAT")], "<br> Overall GDP@Risk Rank for chosen threat:",data_PNR[,paste0("RANKING OF USD BN GDP AT RISK FROM ",names(threat_choices)[as.numeric(input$select_threat)]," THREAT")]))
  
  #Display Threat Map
  threat_layers <- c("CYBER CATASTROPHE","DROUGHT","EARTHQUAKE","FLOOD","HEATWAVE AND FREEZE","HUMAN PANDEMIC","INTERSTATE WAR","MARKET CRASH","NUCLEAR MELTDOWN","OIL PRICE SHOCK","PLANT EPIDEMIC","POWER OUTAGE","SOCIAL UNREST","SOLAR STORM","SOVEREIGN DEFAULT","WINDSTORM","TERRORISM","TSUNAMI","VOLCANIC ERUPTION")
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate="https://a.tiles.mapbox.com/v3/mapbox.world-bright/{z}/{x}/{y}.png") %>%
      
      ##Add individual layers for threat to the map
      #Cyber Catastrophe
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:cyber_threat-copy",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="Cyber Style", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="CYBER CATASTROPHE")%>%
      
      #Drought
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:drought_threat",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="Drought Style", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="DROUGHT")%>%
      
      #Earthquake
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:eq_threat",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="EQ style", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="EARTHQUAKE")%>%
      
      #Flood
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:flood_threat",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="FLOOD")%>%
      
      #Heatwave and Freeze
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:freeze_threat",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="Heat Freeze style", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="HEATWAVE AND FREEZE")%>%
      
      #Human Pandemic
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:humanpandemic_threat",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="Human Pandemic Style", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="HUMAN PANDEMIC")%>%
      
      #War
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:war_threat",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="War threat", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="INTERSTATE WAR")%>%
      
      #War Conflict Lines
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:war_conflictlines",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="War conflict lines", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="INTERSTATE WAR")%>%
      
      #Market Crash
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:fincat_threat",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="FinCat Market Crash Style", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="MARKET CRASH")%>%
      
      #Nuclear Power Plant operational
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:nuclear_plant_op",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="Nuclear Plant Style", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="NUCLEAR MELTDOWN")%>%
      
      #Nuclear Plant Buffer
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:nuclear_buffer",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="Nuclear Buffer Style", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="NUCLEAR MELTDOWN")%>%
      
      #Oil Price Shock
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:oil_threat",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="Oil Shock Style", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="OIL PRICE SHOCK")%>%
      
      #Plant Epidemic
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:plantepidemic_threat",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="Plant Epidemic Style", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="PLANT EPIDEMIC")%>%
      
      #Power Outage
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:outage_threat",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="Outage style", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="POWER OUTAGE")%>%
      
      #Social Unrest
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:socialunrest_threat",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="Social Unrest Style", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="SOCIAL UNREST")%>%
      
      #Solar Storm Threat
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:solarstorm_contourbands",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="Solar Storm Band", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="SOLAR STORM")%>%
      
      #Solar Storm Lines
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:solarstorm_contourlines",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="Solar Storm Lines", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="SOLAR STORM")%>%
      
      #Sovereign Default
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:sovereigndefault_threat",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="Sovereign Default Style", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="SOVEREIGN DEFAULT")%>%
      
      #Windstorm
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:windstorm_threat",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="Windstorm style", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="WINDSTORM")%>%
      
      #Terrorism
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:terrorism_threat",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="Terrorism style", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="TERRORISM")%>%
      
      #Tsunami
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:tsunami_threat",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="Tsunami style", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="TSUNAMI")%>%
      
      #Volcano Threat
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:volcanic_threat1",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="Volcano locations", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="VOLCANIC ERUPTION")%>%
      
      #Volcano Buffer Zone
      addWMSTiles(
        "https://pan-api.cambridgeriskframework.com/geoserver/geo-crf/wms",
        layers = "geo-crf:volcano_buffer",
        options = WMSTileOptions(format = "image/png", transparent = TRUE, styles="Volcano buffer zone", opacity=50),
        attribution = "This map was researched by CAR Ltd 2014", group ="VOLCANIC ERUPTION")%>%
      
      
      setView(lng = data_PNR[input$select_city,"LON"], lat = data_PNR[input$select_city,"LAT"], zoom = 10) %>%
      addCircleMarkers(data=data_PNR, lat = ~LAT, lng = ~LON, popup = ~data_PNR$`POPUP INFO`, color = "red", fillOpacity = 1,  weight=1, radius=3+data_PNR[,paste0(names(threat_choices)[as.numeric(input$select_threat)], " GDP AT RISK USD BN")]) %>%
      
      #Add layers control
      addLayersControl(overlayGroups=threat_layers, options = layersControlOptions(collapsed=TRUE,autoZIndex = TRUE), position = c("topright", "bottomright", "bottomleft", "topleft"))  %>%
      hideGroup(threat_layers)
  })
  
  #Reset view to a world view
  observe({
    input$reset_button
    leafletProxy("map") %>% setView(lat = 20, lng = 0, zoom = 2)
  })
  
  
  #Display only current threat layer on the map
  changed_threat <- reactive({names(threat_choices)[as.numeric(input$select_threat)]})
  observe({ 
    
    if(changed_threat() %in% c("FREEZE","HEATWAVE"))
    {
      observe({input$currentthreat_layer
        leafletProxy("map") %>% hideGroup(threat_layers) %>% showGroup("HEATWAVE AND FREEZE")
      })
    }
    else if(changed_threat() %in% c("TEMPERATE WINDSTORM","TROPICAL WINDSTORM"))
    {
      observe({input$currentthreat_layer
        leafletProxy("map") %>% hideGroup(threat_layers) %>% showGroup("WINDSTORM")
      })
    }
    else
    {
      observe({input$currentthreat_layer
        leafletProxy("map") %>% hideGroup(threat_layers) %>% showGroup(changed_threat())
      })
    }
  })
  
  
  #Display risk ranked bar chart
  output$riskrankchart <- renderPlotly({
    
    riskrank <-  data.frame( threat = names(threat_choices), value = as.numeric(data_PNR[input$select_city, grepl("GDP AT RISK USD", names(data_PNR))]))
    riskrank <- riskrank[order(riskrank$value, decreasing=TRUE),]
    
    plot_ly(data=riskrank, y = ~ reorder(threat,value), x= ~value, type = "bar", text= ~paste0("Rank: ",0:(nrow(riskrank)-1))) %>%
      layout(title = paste0("TOP THREATS FOR ",toupper(names(city_choices)[as.numeric(input$select_city)])),
             xaxis = list(title = "GDP Loss in Billion USD", tickangle = 0),
             yaxis = list(title = "", size=5),
             autosize = F, width=600, margin=list(l = 180, r = 40, b = 50, t = 50, pad = 3))
    
  })
  
  #Display GDP timechart
  output$gdpchart <- renderPlotly({
    
    gdp <- data.frame( year = gsub("GDP | BN","",names(data_PNR)[grep("GDP 20", names(data_PNR))]), value = as.numeric(data_PNR[input$select_city,grepl("GDP 20", names(data_PNR))]))
    
    plot_ly(data=gdp, y = ~value, x= ~year, type = "bar", color="red") %>%
      layout(title = "GDP TIMESERIES",
             xaxis = list(title = "", tickangle = 0),
             yaxis = list(title = "GDP in Billion USD", size=5),
             autosize = F, width=600, margin=list(l = 70, r = 90, b = 50, t = 50, pad = 2))
    
  })
  
  #Display top cities for each threat
  
  output$cityrankchart <- renderPlotly({
    
    threatcity <- data_PNR[,c("NAME",paste0(names(threat_choices)[as.numeric(input$select_threat)]," GDP AT RISK USD BN"))]
    threatcity <- cbind(threatcity,as.character("N"))
    names(threatcity) <- c("city","loss","current")
    threatcity <- threatcity[order(threatcity$loss, decreasing=TRUE),]
    threatcity <- threatcity[1:10,]
    
    #If chosen city is NOT in the top 10 cities for that threat
    if(!names(city_choices)[as.numeric(input$select_city)] %in% threatcity[,"city"])
    {
      currentcity <- data.frame(matrix(c(names(city_choices)[as.numeric(input$select_city)], data_PNR[input$select_city, paste0(names(threat_choices)[as.numeric(input$select_threat)]," GDP AT RISK USD BN")], as.character("Y")), byrow=TRUE, ncol=3, nrow=1))
      names(currentcity) <- c("city","loss","current")
      
      threatcity <- rbind(threatcity,currentcity)
      threatcity$loss <- as.numeric(threatcity$loss)
      hoverinfo <- c(1:(nrow(threatcity)-1),"x")
    }
    
    #If chosen city is already in the top 10
    else{
      threatcity$current <- as.character(threatcity$current)
      currentcity <- names(city_choices)[as.numeric(input$select_city)]
      location <- which(threatcity$city == currentcity)
      threatcity[location,] <- c(names(city_choices)[as.numeric(input$select_city)],threatcity$loss[location],as.character("Y"))
      threatcity$loss <- as.numeric(threatcity$loss)
      hoverinfo <- 1:nrow(threatcity)
    }
    
    
    plot_ly(data=threatcity, y = ~reorder(city,loss), x= ~loss, type = "bar", color =~as.factor(current), hoverinfo="text",  text= ~paste0(loss, "</br>Rank: ",hoverinfo))  %>%
      layout(title = paste0("TOP 10 CITIES AT RISK FOR ",names(threat_choices)[as.numeric(input$select_threat)]," THREATS"),
             xaxis = list(title = "GDP loss in Billion USD", tickangle = 0),
             yaxis = list(title = "", size=5),
             autosize = F, width=600, margin=list(l = 120, r = 40, b = 50, t = 50, pad = 0),
             showlegend = FALSE)
    
  })
  
  #Display E-P curve 
  output$graph <- renderPlotly({
    
    plot_ly(data=data_EP, y = ~1/returnperiodmid, x= ~loss, type= "scatter", mode= "markers", color=~threat_guid) %>%
      layout(title = "EXCEEDANCE PROBABILITY CURVE",
             xaxis = list(title = "Loss"),
             yaxis = list(title = "Probability", size=4),
             autosize = F, margin=list(l = 160, r = 40, b = 50, t = 50, pad = 0))
  })
  
  #Dynamically adjust the UI for relevant city that is clicked on the map by the user
  observeEvent(input$map_marker_click,{
    
    clicked <- input$map_marker_click
    city_clicked <- which(clicked$lng ==data_PNR$LON & clicked$lat == data_PNR$LAT)
    if(input$select_city!=city_clicked) updateSelectInput(session, "select_city", selected=as.numeric(city_clicked))
  })
  
  #Import portfolio file uploaded by user
  data_portfolio <- reactive(input$portfolio_file)
  
}

#Launch App
shinyApp(ui = ui, server = server)










# ##-----------------------------------------------------------------------------------------------
# ##------------------------------------------------------------------------------------------------  
# #UNUSED CODE - APPENDIX
# 
# # Download city coordinates from Google API
# cities_pandora <- read.csv("C:/Users/mahalingama/OneDrive/CRS Files/TEMP/Cities List.csv")
# cities_pandora <- cbind(cities_pandora,geocode(as.character(cities_pandora$City)))
# cities_pandora[cities_pandora$City=="Kawasaki",c("lat","lon")] <- c(35.529979, 139.713339)
# cities_pandora$City = as.character(cities_pandora$City)
# cities_pandora <- cities_pandora[order(cities_pandora$City),]
# 
