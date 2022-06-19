#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(dplyr)
library(plotly)
library(rgdal)
library(leaflet)
library(DT)
library(mapview)
library(randomcoloR)
library(htmltools)


all.ae = fread("data/VidhaSabhaNumber.csv",na="") %>% subset(DelimID==3)
setnames(all.ae, old=c("state","year","sa_no"),new=c("State_Name","Year","Assembly_No"))

make_map_party <- function(shape,data,var, pal,inp_title){
    shape_as <- merge(shape,data,by.x=c("AC_NO"),by.y=c("Constituency_No"),all.x=T)
    shape_as  <- merge(shape_as,pal,by.x=var,by.y=var,all.x=T)
    idx = which(!is.na(shape_as$Color))
    shape_as <- shape_as[idx,]
    shape_as$popup <- paste(shape_as$AC_NO,shape_as$AC_NAME,shape_as$Constituency_Name,shape_as[[var]],shape_as[["Color"]],sep = ", ")
    shape_as$x <- shape_as[[var]]

    cols <- shape_as$Color
    Vars <- shape_as$x
    Vars[which(is.na(Vars))] <- "|No Contest|"
    tmp <- data.table(Vars,cols)
    tmp = tmp[,.(freq=.N),by=c("Vars","cols")]

    tmp <- tmp[order(tmp$Vars),]
    map_as <- leaflet(shape_as) %>% addPolygons(weight=1,popup= ~popup, fillOpacity = 1,smoothFactor = 1,fillColor = ~Color) %>% addLegend(position = "topright",colors = tmp$cols,labels = paste0(tmp$Vars," (",tmp$freq,")"),title = var) %>% addControl(tags$div(HTML(inp_title)), position = "bottomleft", className="map-title")
    return(map_as)
}

party.pal = read.csv("data/colours.csv",na="") %>% subset(select = c("Party","Color"))
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Pre2008 Maps"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "stateName",label = "Select_State",choices = sort(unique(all.ae$State_Name))),
            uiOutput("assemblySelect")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("mapPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    assemblies = reactive({subset(all.ae,State_Name==input$stateName)})
    map.data = reactive({fread(paste0("data/",input$stateName,"_ae_ld_maps.csv"),na="") %>% subset(Assembly_No %in% assemblies()$Assembly_No)})
    shape = reactive({readOGR(paste0("Maps/",input$stateName,"_pre2008ac.geojson"))})
    new.party.pal = reactive({
        data = map.data()
        idx = which(!data$Party %in% party.pal$Party)
        if(length(idx)==0){
            return(party.pal)
        }
        else{
            new.parties = unique(data$Party[idx])
            new.colors = distinctColorPalette(length(new.parties))
            return(rbind(party.pal,data.frame(Party=new.parties,Color=new.colors)))
        }
        
    })


    output$assemblySelect <-  renderUI({selectInput(inputId = "Assembly_No",label = "select assembly",choices = assemblies()$Assembly_No)})

    assemblyYear = reactive({subset(assemblies(),Assembly_No == input$Assembly_No)$Year})
    plotData = reactive({subset(map.data(), Assembly_No == input$Assembly_No)})
    plotTitle = reactive({paste0("#Map for assembly", input$Assembly_No," year: ",assemblyYear())})
    output$mapPlot <- renderLeaflet({
        # generate bins based on input$bins from ui.R
        make_map_party(shape = shape(),data = plotData(),var="Party",inp_title= plotTitle(), pal= new.party.pal())
    })
}

# Run the application
shinyApp(ui = ui, server = server)
