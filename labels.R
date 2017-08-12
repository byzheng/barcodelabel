# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   04:21 PM Monday, 28 April 2014
# * Copyright: AS IS
# *


#' Create labels with barcode
#'
#' @param designs A data.frame of design file, which has to include several columns
#' @param file A output pdf file
#' @param measure_date Date of measurement
#' @param site_idx Index of site
#' @param sample_num Number of sample in each plot
#' @param product Product ID of lables
#' @export
createBarcode <- function(
    designs,
    measure_date,
    site_idx,
    measure_traits,
    sample_num = 1,
    measure_traits_map = NULL) {
    # Convert into small case to avoid problems
    names(designs) <- tolower(names(designs))
    library(readr)
    library(dplyr)
    library(magrittr)
    names(measure_traits_map) <- tolower(names(measure_traits_map))
    measure_trait_idx <- measure_traits_map$measurementindex[
        match(measure_traits, measure_traits_map$name)]

    bc_plot <- designs %>%
        mutate(Barcode = paste0(
            format(measure_date, '%y%m%d'),
            site_idx,
            formatC(row, width = 2, flag = '0'),
            formatC(column, width = 2, flag = '0')
        ))

    bc <- expand.grid(
        Barcode = bc_plot$Barcode,
        measure_trait = measure_trait_idx,
        sample = seq(length = sample_num),
        stringsAsFactors = FALSE
    ) %>%
        left_join(bc_plot, by = 'Barcode') %>%
        left_join(measure_traits_map,
                  by = c('measure_trait' = 'measurementindex')) %>%
        mutate(
            Barcode = paste0(
                Barcode,
                formatC(measure_trait, width = 2, flag = '0'),
                sample)) %>%
        rename(MeasureTrait = name) %>%
        mutate(MeasureDate = measure_date,
               Researcher = researcher) %>%
        arrange(column, row)
    bc
}



#' Generate labels for avery products
#'
#' @param labels A character vector will be used to genearte labels
#' @param file the file name to export
#' @param product The product ID of Avery
#' @param researcher Name of researcher
#' @param researcher Name of researcher
#' @export
averyLabel <- function(
    labels, file,
    product = 'L7163')
{
    names(labels) <- tolower(names(labels))
    # Total number of labels
    num_label <- nrow(labels)
    # Check the labels
    if (num_label == 0)
    {
        stop('NO label specified')
    }
    # Read teh AveryDB
    averydb <- read.csv('data/dbavery.csv')

    # Check the product ID
    if (!(product %in% averydb$ID))
    {
        stop(paste0('Product ID ', product, ' don\'t support.'))
    }
    # Get the design
    a_design <- averydb[averydb$ID == product,]

    # Define the page size (A4)
    page_height <- 297
    page_width <- 210

    # Calculate label width and height
    a_design$LabelHeight <- (page_height - a_design$TopBorder -
        a_design$BottomBorder -
        a_design$MarginRow * (a_design$LabelRows - 1)) /
        a_design$LabelRows
    a_design$LabelWidth <- (page_width - a_design$LeftBorder -
        a_design$RightBorder -
        a_design$MarginCol * (a_design$LabelCols - 1)) /
        a_design$LabelCols

    # load the library
    library(grid)
    # page_border <- 0.25 * 25.4
    # Printed labels
    num_printed <- 1

    # Create all labels
    # Calculate total pages
    pages <- ceiling(num_label / (a_design$LabelRows * a_design$LabelCols))
    pdf(file, width = page_width / 25.4, height = page_height / 25.4)
    op <- par(mar = rep(0, 4))
    for (k in seq(length = pages))
    {
        grid.newpage()
        grid.rect()
        for (i in seq(length = a_design$LabelRows))
        {
            for (j in seq(length =  a_design$LabelCols))
            {
                # Check if all labels is printed
                if (num_printed > num_label)
                {
                    break
                    return(NULL)
                }

                # Calculate the viewport
                vp_x <- unit(
                    # Left border
                    a_design$LeftBorder +
                    # Page border
                    # page_border +
                    # Size for labels in the left
                    a_design$LabelWidth * (j - 1) +
                    # Middle of this label
                    a_design$LabelWidth / 2 +
                    # Margins between labels in the left
                    a_design$MarginCol * (j - 1),
                    'mm')
                vp_y <- unit(
                    # Print from the top
                    page_height - (
                    # Top border
                    a_design$TopBorder +
                    # Page border

                    # Size for labels in the top
                    a_design$LabelHeight * (i - 1) +
                    # Middle of this label
                    a_design$LabelHeight / 2 +
                    # Margins between labels in the top
                    a_design$MarginRow * (i - 1)),
                    'mm')

                # Create viewport
                vp <- viewport(vp_x, vp_y,
                    unit(a_design$LabelWidth, 'mm'),
                    unit(a_design$LabelHeight, 'mm'))

                pushViewport(vp)
                grid.rect()


                # keep a 3 mm border
                # canvas
                canvas_width <- a_design$LabelWidth - 6
                canvas_height <- a_design$LabelHeight - 6
                vp <- viewport(
                    0.5, 0.5,
                    unit(canvas_width, 'mm'),
                    unit(canvas_height, 'mm'))

                pushViewport(vp)
                
                #reate label
                do.call(eval(parse(text = paste0('label_', product))),
                        args = list(labels = labels, num_printed = num_printed,
						canvas_width = canvas_width, canvas_height = canvas_height))
                
                popViewport()
                popViewport()
                num_printed <- num_printed + 1
            }
        }
    }
    par(op)
    dev.off()
}

label_L7651 <- function(labels, num_printed, canvas_width, canvas_height) {
    bc_img <- zint_barcode(
        labels$barcode[num_printed],
        height = 100,
        border = 0,
        is_show_text = TRUE)
    vp_img <- viewport(
        x = unit(0.5, "npc"),
        y = unit(0.6, "npc"),
        width = unit(1, "npc"),
        height = unit(0.8, "npc")
    )
    pushViewport(vp_img)
    #grid.rect()
    grid.raster(bc_img[[1]], 
                width = unit(1, 'npc'),
                height = unit(1, 'npc'))
    popViewport()
    vp_txt <- viewport(
        x = unit(0.5, "npc"),
        y = unit(0.1, "npc"),
        width = unit(1, "npc"),
        height = unit(0.2, "npc")
    )
    pushViewport(vp_txt)
    grid.text(label = labels$label[num_printed],
              gp = gpar(cex = 0.3))
    popViewport()
}

label_L7163 <- function(labels, num_printed, canvas_width, canvas_height) {
    
    #grid.rect()
    # Barcode
    bc_img <- zint_barcode(
        labels$barcode[num_printed],
        height = 30,
        border = 0)
    vp_img <- viewport(
        unit(0.25 * canvas_width, 'mm'),
        unit(3 * 1 / 4 * canvas_height, 'mm'),
        unit(1 / 2 * canvas_width, 'mm'),
        unit(1 / 2 * canvas_height, 'mm'))
    pushViewport(vp_img)
    #grid.rect()
    grid.raster(bc_img[[1]])
    popViewport()
    
    # column
    vp_row <- viewport(
        unit(0.5 / 8 * canvas_width, 'mm'),
        unit((1 / 4) * canvas_height, 'mm'),
        unit(0.5 / 4 * canvas_width, 'mm'),
        unit(0.5 / 4 * canvas_width, 'mm'))
    pushViewport(vp_row)
    grid.rect()
    grid.text(paste0('Col\n', labels$column[num_printed]))
    popViewport()
    
    
    
    # row
    vp_col <- viewport(
        unit(3 * 0.5 / 8 * canvas_width, 'mm'),
        unit((1 / 4) * canvas_height, 'mm'),
        unit(0.5 / 4 * canvas_width, 'mm'),
        unit(0.5 / 4 * canvas_width, 'mm'))
    pushViewport(vp_col)
    grid.rect()
    grid.text(paste0('Row\n', labels$row[num_printed]))
    popViewport()
    
    
    
    # replicate
    vp_rep <- viewport(
        unit(5 * 0.5 / 8 * canvas_width, 'mm'),
        unit((1 / 4) * canvas_height, 'mm'),
        unit(0.5 / 4 * canvas_width, 'mm'),
        unit(0.5 / 4 * canvas_width, 'mm'))
    pushViewport(vp_rep)
    grid.rect()
    grid.text(paste0('Rep\n', labels$replicate[num_printed]))
    popViewport()
    
    # Sample
    vp_sam <- viewport(
        unit(7 * 0.5 / 8 * canvas_width, 'mm'),
        unit((1 / 4) * canvas_height, 'mm'),
        unit(0.5 / 4 * canvas_width, 'mm'),
        unit(0.5 / 4 * canvas_width, 'mm'))
    pushViewport(vp_sam)
    grid.rect()
    grid.text(paste0('Sam\n', labels$sample[num_printed]))
    popViewport()
    
    # Traits
    vp_traits <- viewport(
        unit(0.75 * canvas_width, 'mm'),
        10 / 12,
        0.5,
        2 / 6)
    pushViewport(vp_traits)
    #grid.rect()
    grid.text(labels$measuretrait[num_printed],
              gp = gpar(cex = 1.1))
    popViewport()
    
    # TrialCode
    vp_trialcode <- viewport(
        unit(0.75 * canvas_width, 'mm'),
        7 / 12,
        0.5,
        1 / 6)
    pushViewport(vp_trialcode)
    #grid.rect()
    grid.text(labels$trial[num_printed])
    popViewport()
    
    
    # Genotype
    vp_genotype <- viewport(
        unit(0.75 * canvas_width, 'mm'),
        5 / 12,
        0.5,
        1 / 6)
    pushViewport(vp_genotype)
    #grid.rect()
    grid.text(labels$genotype[num_printed])
    popViewport()
    
    # Site
    vp_site <- viewport(
        unit(0.75 * canvas_width, 'mm'),
        3 / 12,
        0.5,
        1 / 6)
    pushViewport(vp_site)
    #grid.rect()
    grid.text(labels$site[num_printed])
    popViewport()
    
    # Researcher and date
    vp_res_date <- viewport(
        unit(0.75 * canvas_width, 'mm'),
        1 / 12,
        0.5,
        1 / 6)
    pushViewport(vp_res_date)
    #grid.rect()
    grid.text(paste0(
        'by ',
        labels$researcher[num_printed],
        ' on ',
        labels$measuredate[num_printed]),
        x = 0.9,
        gp = gpar(cex = 0.5),
        hjust = 1)
    popViewport()
}
