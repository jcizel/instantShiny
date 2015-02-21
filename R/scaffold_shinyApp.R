scaffold_global <- function(
    libraries = NULL,
    sources = NULL,
    load = NULL
){
    if (is.null(libraries)){
        libraries %>>%
        list.map(
            sprintf("library(%s)",.)
        ) %>>% unlist %>>%
        paste(collapse = "\n") ->
            libraries
    } else {
        libraries = "\n"
    }

    
    if (!is.null(sources)){
        sources  %>>%
        list.map(
            sprintf("source(%s)",.)
        ) %>>% unlist %>>%
        paste(collapse = "\n") ->
            sources
    } else {
        sources = '\n'
    }

    if (!is.null(load)){
        load  %>>%
        list.map(
            sprintf("load(file = '%s')",.)
        ) %>>% unlist %>>%
        paste(collapse = "\n") ->
            load
    } else {
        load = '\n'
    }


    tmp <-
        c("
## -------------------------------------------------------------------------- ##
## Load packages                                                              ##
## -------------------------------------------------------------------------- ##
library(devtools)
library(shiny)
library(shinydashboard)
library(data.table)",
         libraries,
          "
## -------------------------------------------------------------------------- ##
## Sources                                                                    ##
## -------------------------------------------------------------------------- ##
",
          sources,
          "
## -------------------------------------------------------------------------- ##
## Load .RData files                                                          ##
## -------------------------------------------------------------------------- ##
",
          load) %>>% paste(collapse = "\n")
    
    return(tmp)
}


scaffold_ui <- function(
    title = NULL
){
    
    tmp <-
        "
## -------------------------------------------------------------------------- ##
## UI.R                                                                       ##
## -------------------------------------------------------------------------- ##        
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem('Home', icon = icon('home',lib='glyphicon'), tabName = 'welcome',
                 badgeLabel = 'new', badgeColor = 'green'),
        menuItem(
            '<Item>',
            tabName = 'tabname',
            menuSubItem(
                '<SubItem>',
                'subitem'
            )
        )
    )
)
    
## -------------------------------------------------------------------------- ##
## Source UI modules                                                          ##
## -------------------------------------------------------------------------- ##

## source(<Add Module Path>, local = TRUE)

body <-
    dashboardBody(
        tabItems(
        ##    <Add Items>
        )
    )


dashboardPage(
    title = '$title',
    dashboardHeader(
        title = '$title'
    ),
    sidebar,
    body,
    skin = 'black'
)
"
    o <- rprintv(
        tmp,
        title = title
    )
        
    return(o)
}


scaffold_server <- function(
){    
    tmp <- "
## ## GLOBAL VARS FOR REGRESSION TABLES
## reg.results <- list()
## reg.depvars <- c()
## reg.model <- c('Model')
## reg.effects <- c('Main Effects')
## reg.absorb <- c('Fixed Effects')
## reg.standardize <- c('Standardized?')
## reg.setype <- c('SE Type')

shinyServer(function(input,output,session){
    ## source(<module>, local = TRUE)    
})
"
    return(tmp)
}

## root = '~/Downloads'
## dir.name = 'testApp'

##' Scaffold Shiny Dashboards
##'
##' This functions generates boilerplate code for a fully functioning
##' shiny-dashboard  applications.
##' 
##' @param root path of the directory in which the folder with app files is to
##' be created.
##' @param app.name App name.  
##' @param delete.existing If the folder with same name as `app.name` already
##' exists, should it be overwritten? Default is FALSE.
##' @param run Run the resulting app? Default is TRUE.
##' @return NULL
##' @author Janko Cizel
##'
##' @export
##' @import pipeR rlist rprintf shiny
##'
##' @examples
##' \dontrun{
##' scaffold_dashboard(root = '~/Downloads', app.name = 'testApp',
##' delete.existing = TRUE)
##'
##' }
scaffold_dashboard <- function(
    root = NULL,
    app.name = NULL,
    delete.existing = FALSE,
    run = TRUE
){
    oldwd <- getwd()

    newwd <- sprintf('%s/%s',root,app.name)

    if (file.exists(newwd) & delete.existing == TRUE)
        unlink(newwd, recursive = TRUE, force = TRUE)
    else
        stop(newwd, "already exists. Set `delete.existing = TRUE` if you want to overwrite the folder.") 

    scaffold_global() %>>%
    save2file(root = root,
              dir.name = dir.name,
              file.name = 'global.R')

    scaffold_ui(title = 'test') %>>%
    save2file(root = root,
              dir.name = dir.name,
              file.name = 'ui.R')

    scaffold_server() %>>%
    save2file(root = root,
              dir.name = dir.name,
              file.name = 'server.R')

    if (run == TRUE){
        shiny::runApp(sprintf('%s/.',newwd))
    }

    cat(sprintf("The app is saved in the following path: %s\n\n", newwd))
    return(newwd)
}


