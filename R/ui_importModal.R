#' @include internal.R
NULL

#' Import modal
#'
#' Constructs a modal for importing data.
#'
#' @param id `character` identifier.
#'
#' @return A `shiny.tag` object.
#'
#' @examples
#' \dontrun{
#' # run Shiny app to demo the sidebar pane
#' if (interactive()) {
#'   runExample("importModal")
#' }
#' }
#'
#' @export
importModal <- function(id) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(id),
    assertthat::noNA(id)
  )

  # create modal
  shiny::modalDialog(
    title = htmltools::tags$p(
      "Welcome to CAP-BC",
      style = "text-align:center"
    ),
    easyClose = FALSE,
    fade = TRUE,
    footer = htmltools::tags$div(
      # styling
      style = "text-align: center",
      # builtin button
      shiny::conditionalPanel(
        condition = paste0("input.", id, "_method == 'builtin'"),
        shinyFeedback::loadingButton(
          inputId = paste0(id, "_builtin_button"),
          label = "Import",
          loadingLabel = "Loading..."
        )
      ),
      # manual button
      shiny::conditionalPanel(
        condition = paste0("input.", id, "_method == 'manual'"),
        shinyFeedback::loadingButton(
          inputId = paste0(id, "_manual_button"),
          label = "Import",
          loadingLabel = "Loading..."
        ),

      ),
      # spatial button
      shiny::conditionalPanel(
        condition = paste0("input.", id, "_method == 'spatial'"),
        shinyFeedback::loadingButton(
          inputId = paste0(id, "_spatial_button"),
          label = "Import",
          loadingLabel = "Loading..."
        )
      )
    ),

    ## import method
    shiny::selectInput(
      inputId = paste0(id, "_method"),
      label = "Select import method",
      choices = c(
        "built-in project" = "builtin",
        "upload project data" = "manual",
        "upload shapefile" = "spatial"
      ),
      selected = "built-in project",
      multiple = FALSE
    ),

    ## add project descriptions text
    shiny::HTML(
      "<div id=project-descriptions><b>Project availables</b><br>
      <ul>
          <li><b>CAP-BC-1km:</b> Fine resolution (1km) better and closer to the actual data and features, however it takes more time to load and process</li>
          <li><b>CAP-BC-5km:</b> Coarse resolution (5km) works faster than fine resolution and provides to users with a quick exploration</li>
      </ul>
      </div>"),

    ## builtin method
    shiny::conditionalPanel(
      ### condition
      condition = paste0("input.", id, "_method == 'builtin'"),
      ### main
      shiny::selectInput(
        inputId = paste0(id, "_name"),
        label = "Select project",
        choices = c("No built-in projects available" = "NA"),
        multiple = FALSE
      ),
      shiny::checkboxInput(
        paste0(id, "_builtin_hide_layers"),
        shiny::HTML("Hide all input layers (to load the project faster)"),
        value = FALSE
      )
    ),

    ## manual method
    shiny::conditionalPanel(
      ### condition
      condition = paste0("input.", id, "_method == 'manual'"),
      ### main
      shiny::fileInput(
        paste0(id, "_manual_configuration_file"),
        "Select configuration file",
        multiple = FALSE,
        accept = ".yaml"
      ),
      shiny::fileInput(
        paste0(id, "_manual_spatial_file"),
        "Select spatial data",
        multiple = TRUE,
        accept = c(".shp", ".shx", ".prj", ".dbf", ".cpg", ".tif")
      ),
      shiny::fileInput(
        paste0(id, "_manual_attribute_file"),
        "Select attribute data",
        multiple = FALSE,
        accept = c(".csv", ".csv.gz", ".gz")
      ),
      shiny::fileInput(
        paste0(id, "_manual_boundary_file"),
        "Select boundary data",
        multiple = FALSE,
        accept = c(".csv", ".csv.gz", ".dat", ".dat.gz", ".gz")
      ),
      shiny::hr(),
      shiny::checkboxInput(
        paste0(id, "_manual_hide_layers"),
        shiny::HTML("<b> Hide theme, weight and include layers </b> <br> 
                    recommended for large projects"),
        value = FALSE
      )
    ),

    ## spatial method
    shiny::conditionalPanel(
      ### condition
      condition = paste0("input.", id, "_method == 'spatial'"),
      ### main
      shiny::fileInput(
        paste0(id, "_spatial_spatial_file"),
        "Select shapefile",
        multiple = TRUE,
        accept = c(".shp", ".shx", ".prj", ".dbf", ".cpg"),
      ),
      htmltools::tags$label(
        id = paste0(id, "_spatial_text"),
        class = "control-label",
        "Select fields"
      ),
      importSettingsOutput(outputId = paste0(id, "_spatial_settings")),
      shiny::hr(),
      shiny::checkboxInput(
        paste0(id, "_spatial_hide_layers"),
        shiny::HTML("<b> Hide theme, weight and include layers </b> <br> 
                    recommended for large projects"),
        value = FALSE
      ),
      shiny::hr(),
      htmltools::tags$label(
        id = paste0(id, "_spatial_note"),
        class = "control-label",
        "Note that non-numeric data are excluded."
      )
    )
  )
}
