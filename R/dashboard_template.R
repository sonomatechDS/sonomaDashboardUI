#' Creates template for STi dashboard with tab panel
#'
#' Code template will be written according to specifications below into
#' a file and opened in RStudio.
#'
#' @param filename string Name of file (should follow format app_*.R). Must not
#'                        already exist.
#' @param tab_num integer Number of tabs
#' @param tab_names character vector Names of tabs
#' @param tab_groups integer vector (optional) Desired tab groupings. Each group will have
#'                                  a unique color. Must sum to tab_num. e.g.
#'                                  If tab_num = 5, valid tab_groups
#'                                  include c(2,3), c(1,4), c(3,2), etc. If no
#'                                   tab groups are specified (default), all
#'                                    tabs will be the same color.
#' @param app_title string (optional) Title of App to be written in header. If not specified, title is blank.
#' @param header_img string (optional) Filename of image to include in title header. Must
#'                          be in directory named www.If not specified, no image is added.
#' @param readme_file string (optional) Filename of README file. Must be in the www folder.
#' @param PAL string (optional) The name of the non-colorblind-palette function
#'                   to use (default = sonoma_color_palette)
#' @param CB_PAL string (optional) The name of the colorblind-palette function
#'                      to use (default = sonoma_color_palette)
#' @param PAL_a double (optional) An alpha value between 0 and 1 to set opacity (default = 1)
#'
#' @return
#' @importFrom assertthat assert_that
#' @importFrom styler style_file
#' @importFrom rstudioapi navigateToFile
#' @export
#'
#' @examples
dashboard_template <- function(filename,
                            tab_num,
                            tab_names,
                            tab_groups = NULL,
                            app_title = NULL,
                            header_img = NULL,
                            readme_file = '',
                            PAL = 'sonoma_color_palette',
                            CB_PAL = 'viridis::magma',
                            PAL_a = 1
) {
  #### Check parameters ####
  # CHECK FILENAME
  assertthat::assert_that(!any(grepl(filename, list.files())),
                          msg = paste(filename, "already exists in directory.",
                                      "Choose new filename, or delete existing file."))
  assertthat::assert_that(substr(filename, nchar(filename)-1,nchar(filename)) == '.R',
                          msg = 'filename must end in ".R"')
  if(substr(filename, 1,3) != 'app') warning(paste('Filename should begin with',
                                                   '"app" (e.g. "app_test.R") for',
                                                   'best compliance with RShiny',
                                                   'configurations in RStudio.'))
  # CHECK TAB CONFIGURATIONS
  assertthat::assert_that(length(tab_names) == tab_num,
                          msg = paste0('Length of tab_names (',length(tab_names),
                                       ') must equal tab_num (', tab_num, ').'))
  if (!is.null(tab_groups)) {
    assertthat::assert_that(sum(tab_groups) == tab_num,
                            msg = paste0('Sum of tab groups (',sum(tab_groups),
                                         ') must equal tab_num (', tab_num, ').'))
    assertthat::assert_that(class(tab_groups) == 'numeric',
                            msg = paste('tab_groups should be a numeric vector',
                                        'with desired number of tabs per',
                                        'group. (e.g. for tab_num = 5, valid',
                                        'values for tab group include: c(1, 4),',
                                        'c(3,2), c(2,3), etc.'))
  }
  # CHECK HEADER_IMG
  if (!is.null(header_img)) {
    valid_image <- any(grepl(header_img, list.files('www')))
    assertthat::assert_that(valid_image, msg = "header_img must be stored in directory named \`www\` per shiny configuration. header_img cannot be located.")
    image_file <- paste0('src = "', header_img,'",')
  } else {
    image_file <- '# src = ADD IMG TO www DIRECTORY and INSERT FILENAME HERE,'
  }

  #### Configure Header ####
  # Load packages
  packages <- 'library(shiny)\nlibrary(shinycssloaders)'
  # Begin "UI" object
  start_ui <- 'ui <- fluidPage('
  # Add title header, including README link and color palette toggle
  add_header <- paste0('\nfluidRow(\ncolumn(6, align = "center", offset = 3,',
                       '\ntitlePanel(title = div(img(',
                       image_file,
                       ',\n height = 100, width = 100),',
                       '\nHTML("',
                       app_title,
                       '")), windowTitle = paste("',
                       app_title,
                       '"))),',
                       # Add readme link and color toggle
                       '\ncolumn(3, align = "right",',
           '\ndiv(style="padding-top:50px; font-size:16px; padding-right:25px",',
               '\nfluidRow(tags$a("View README",href="',readme_file,'",target="_blank")),',
               '\nfluidRow(checkboxGroupInput(inputId = "color_pal",',
                                           '\nchoices = c("Accessible Color Palette" = "CB"),',
                                           '\nlabel = NULL))))',
                       '\n),\n')

  #### Add panel of tabs (tab menu) ####
  add_tabset_panel <- '#### Tabs ####\ntabsetPanel(id = "tabs",'

  #### Define colors ####
  # Extract tab groups and create list of colors to apply to tabs based on
  # defined group sizes
  if (!is.null(tab_groups)) {
    # tab colors from standard sonoma color palette
    tab_colors <- c()
    for (i in 1:length(tab_groups)) {
      # For each group, repeat a color "size of group" times
      tab_colors <- c(tab_colors, rep(paste0('colors$palette', '(',i,')[',i,']'), tab_groups[i]))
    }
  } else {
    # If there are no groups, all tabs will be purple
    tab_colors <- rep(paste0('colors$palette', '(1)[1]'), tab_num)
  }

  # Add style render statement
  add_tab_style_ui <- "shiny::uiOutput('style_tag'),"

  # Add each tab, including html style components
  add_tabs <- ''
  add_tab_styles <- paste0('\noutput$style_tag <- renderUI({',
                           '\ntags$style(HTML("\n\t\t.tabbable > .nav > li > a  {color: white}",\npaste0(') # Font color
  # For each desired tab, add the named tab to a list of tab panels to be created,
  # and add each tab to the html style statement with title and specified
  # color.
  for (i in 1:tab_num) {
    # Extract tab name
    tab_name <- tab_names[i]
    # Add tab to list of tab panels
    tab <- paste0('\n#### ',tab_name,' ####\n','tabPanel("',tab_name,'"),')
    # Add tab to html style statement



    tab_style <- paste0('\n"',
                        # '\n"',
                        '\n\t\t.tabbable > .nav > li > a[data-value=\'',
                        tab_name,
                        '\'] {background-color: ",\n\t',
                        tab_colors[i],
                        ',\n"; font-weight: bold}",')



    # If last tab in list, remove comma from tab/add closing to add_tab_styles
    if (i == tab_num){
      print(tab)
      tab <- paste0(substr(tab, 1, nchar(tab) - 1), ')')
      print(tab)
      tab_style <- paste0(substr(tab_style, 1, nchar(tab_style) - 1), ')))\n})')
    }
    add_tabs <- paste0(add_tabs, tab)
    add_tab_styles <- paste0(add_tab_styles, tab_style)
  }

  #### End UI function ####
  end_ui <- '\n)'

  #### Add server function ####
  start_server_function <- 'server <- function(input, output, session) {'
  # Add color toggle observer
  color_toggle_observer <- paste0(
  '\n# Observe toggle of color-blind-friendly palette',
  '\ncolors <- reactiveValues(palette = ',PAL,')',
  '\n\nobserve({',
  '\nif (!is.null(input$color_pal)) {',
  '\ncolors$palette = ',CB_PAL,
  '\n} else {',
  '\ncolors$palette = ',PAL,
  '\n}',
  '\n})',
  '\n\n# Set tab colors based on palette choice'
  )

  end_server_function <- '\n}'

  #### Add "run app" statement ####
  run_app <- 'shinyApp(ui = ui, server = server)'

  #### Concatenate script ####
  text <- paste0(packages, '\n\n',
                 start_ui,
                 add_tab_style_ui,
                 add_header, add_tabset_panel, add_tabs, end_ui, '\n\n',
                 start_server_function,
                 color_toggle_observer,
                 add_tab_styles,
                 end_server_function,
                 '\n\n',
                 run_app)

  cat(text)

  ### Write script to file ####
  open_and_write(filename, text)
  # Format script
  styler::style_file(filename)

  #### Open file ####
  rstudioapi::navigateToFile(filename)
}

