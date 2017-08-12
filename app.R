# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   01:16 PM Wednesday, 17 June 2015
# * Copyright: AS IS

library(shiny)
library(dplyr)
library(magrittr)
library(ggplot2)
library(readxl)

source('barcode.R')
source('labels.R')



designs <- read_excel('design.xlsx', 'Design') 
names(designs) <- tolower(names(designs))
traits <- read_excel('design.xlsx', 'Traits')
names(traits) <- tolower(names(traits))


trials_name <- unique(designs$trial)
genotypes <- unique(designs$genotype)

measurements <- traits$measurement %>% unique
researchers <- read_excel('design.xlsx', 'Researchers') %>% use_series('name')

traits_sel <- traits %>%
    filter(measurement %in% measurements[1]) %>%
    use_series(name)

# Create the basic plot
pd_grid <- expand.grid(
    row = seq(min(designs$row), max(designs$row)),
    column = seq(min(designs$column), max(designs$column))) %>%
    mutate(row = as.factor(row),
           column = as.factor(column))

p_grid <- ggplot(pd_grid) +
    geom_tile(aes(row, column),
              colour = 'gray',
              alpha = 0) +
#     geom_tile(aes(row, column),
#               colour = 'gray',
#               alpha = 0.3, data = pd_fill) +
    guides(fill = FALSE,
           colour = FALSE) +
    theme_bw() +
    xlab('Row') + ylab('Column') +
    theme(panel.grid = element_blank(),
          legend.position = 'bottom')
# Product id of avery
avery_product <- c('L7163', 'L7651')


ui <- pageWithSidebar(
    # Application title
    headerPanel('Labels with barcode'),

    # Sidebar with a slider input for number of observations
    sidebarPanel(
        # wellPanel(
        # submitButton('Update View')),
        wellPanel(
            selectInput('measurement', 'Select a measurement',
                    measurements),
            selectInput('trial', 'Select trials:', trials_name,
                        selected = trials_name[1],
                        multiple = TRUE),
            radioButtons('double_plot', 'Filter double plot',
                         c('min', 'max', 'all'), 'min', inline = TRUE),
            selectInput('genotype', 'Select genotypes:', genotypes,
                        selected = genotypes,
                        multiple = TRUE),
            selectInput('trait', 'Select a traits:', traits_sel,
                        selected = traits_sel,
                        multiple = TRUE),

            # measurement date
            dateInput('measure_date', 'Measurement date',
                      format = 'dd/mm/yyyy'),
            # Names of researchers
            textInput('researcher', 'Type in researcher', researchers),
            # Product of avery
            selectInput('product', 'Select a product of Avery', avery_product),
            # File name without extensions
            textInput('filename', 'Type in filename without extension',
                      'measurement'),
            # Download label
            downloadButton('download_excel', 'Download excel file'),
            # Download label
            downloadButton('download_label', 'Download labels')
        ),

        wellPanel(
            helpText(p('Bangyou Zheng'))
        )
    ),
    mainPanel(

            plotOutput('labels_plot', height = '1000px')
    )
)



server <- function(input, output, session)
{
    # Render the table for design
    # Change genotype by measurement and trials
    observe({
#         mearsure <- input$measurement
#         trial_sel <- input$trial
#         genotypes <- designs %>%
#             filter(trial %in% trial_sel) %>%
#             select(genotype) %>%
#             distinct() %>%
#             use_series(genotype)
#
#         if (mearsure == 'qh_key')
#         {
#             g <- c('7770', '7770tin', 'Hartog', 'HartogNoVigour')
#             g <- g[g %in% genotypes]
#         } else {
#             g <- genotypes
#         }
#
#         updateSelectInput(session, 'genotype',
#                           selected = g)
    })

    # Change traits by measurement
    observe({
#         traits <- traits_qh
#         if (input$measurement == 'head_dev') {
#             traits <- traits_head_dev
#         }
#         updateSelectInput(session, 'trait',
#                           choices = traits,
#                           selected = traits)
    })

    # Filter by trial
    r_trials <- reactive({
        i_trial <- input$trial
        
        designs %>%
            filter(trial %in% i_trial)
    })

    # Filter by double plot
    r_doubleplot <- reactive({
       
        doubleplot <- r_trials()
        
        # save(list = ls(), file = 'tmp.Rdata')
        # stop('AAAAAAAAAAAAAAA')
        # load('tmp.Rdata')
        # 
        
        if (input$double_plot == 'min') {
            doubleplot <- doubleplot %>%
                group_by(year, site, trial,
                         genotype, treatment, replicate) %>%
                filter(row == min(row),
                       column == min(column)) %>%
                ungroup()
        } else if (input$double_plot == 'max') {
            doubleplot <- doubleplot %>%
                group_by(year, site, trial,
                         genotype, treatment, replicate) %>%
                filter(row == max(row),
                       column == max(column)) %>%
                ungroup()
        }
        doubleplot
    })

    # Filter by genotypes
    r_genotypes <- reactive({
        # r_trials() %>%
        r_doubleplot() %>%
            filter(genotype %in% input$genotype)
    })



    # Filter by measurement
    r_measurement <- reactive({
        measurement <- r_genotypes()
        if (grepl('^qh.*', input$measurement)) {
            measurement <- measurement %>%
                group_by(year, site, trial,
                         replicate, management,
                         density, genotype) %>%
                filter(row == min(row),
                       column == min(column))
        } else if ('head_dev' == input$measurement) {
            measurement <- measurement %>%
                group_by(year, site, trial,
                         replicate, management,
                         density, genotype) %>%
                filter(row == max(row),
                       column == max(column))
        }
        measurement %>%
            arrange(column, row)


    })

    # Get the sie index
    r_siteidx <- reactive({
        return(2)
    })
    # Generate Barcode
    r_barcode <- reactive({
        designs <- r_measurement()
        measure_date = input$measure_date
        site_idx = r_siteidx()
        measure_traits = input$trait
        sample_num = 1
#         measure_traits_map = traits
#         save(list = ls(), file = 'tmp.Rdata')
#         stop()
#         load('Shiny/tmp.Rdata')
        bc <- createBarcode(
            designs = r_measurement(),
            measure_date = input$measure_date,
            site_idx = r_siteidx(),
            measure_traits = input$trait,
            sample_num = 1,
            measure_traits_map = traits
        )
        bc
    })

    # Show a table of plots for measurements
    output$labels_design = renderDataTable({

        labels_df <- r_measurement()
        labels_df %>%
            select(year, site, trial, row, column,
                   replicate,
                   management, density, genotype)
    })

    # plot the selected plots
    output$labels_plot <- renderPlot({
        library(ggplot2)
        
        labels_df <- r_measurement()


        labels_df <- labels_df %>%
            ungroup() %>%
            mutate(
                row = as.factor(row),
                column = as.factor(column),
                treatment = as.factor(treatment))
        p_grid +
            geom_tile(aes(row, column,
                          fill = treatment),
                      data = labels_df) +
            geom_text(aes(row, column, label = treatment),
                      data = labels_df)


    })
    # Download excel file
    output$download_excel <- downloadHandler(
        filename = function() {
            paste0(input$filename,'.xlsx')
        },
        content = function(file) {
            bc <- r_barcode() %>%
                mutate(Value = '') %>%
                select(
                    Year = year,
                    Site = site,
                    TrialCode = trial,
                    Row = row,
                    Column = column,
                    Replicate = replicate,
                    Density = density,
                    Genotype = genotype,
                    MeasureTrait,
                    MeasureDate,
                    Barcode,
                    Value
                    )

            library(XLConnect)

            newWB <- loadWorkbook(filename = file, create = TRUE)
            createSheet(newWB, name = 'Measurement')
            writeWorksheet(
                newWB,
                data = as.data.frame(bc),
                sheet = 'Measurement',
                header = TRUE,
                rownames = NULL)
            saveWorkbook(newWB)
        }
    )
    # Download labels
    output$download_label <- downloadHandler(
        filename = function() {
            paste0(input$filename,'.pdf')
            },
        content = function(file) {
            bc <- r_barcode()
            measure_traits <- input$trait
            researcher <- input$researcher
            product <- input$product
            bc$researcher <- researcher

            designs <- r_measurement()
#                     save(list = ls(), file = 'tmp.RData')
#                     stop('A')
#                     load('Shiny/tmp.RData')

            averyLabel(
                bc, file,
                product = product)
        }
    )

}

shinyApp(ui = ui, server = server)





