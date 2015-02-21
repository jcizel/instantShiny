available.modules <- c(
    'plmRegression'
)

scaffold_global <- function(
    libraries = NULL,
    sources = NULL,
    load = NULL,
    modules = NULL
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


    if (!is.null(modules)){
        ## Scaffold files
        modules  %>>%
        list.map(
            sprintf("instantShiny::scaffold_module_%s()\n",
                    modules) 
        ) %>>% unlist %>>%
        paste(collapse = "\n") ->
            modules.init
        
        ## Source the files
        modules  %>>%
        list.map(
            sprintf("source('./module/%s/global.R', local = TRUE)",.)
        ) %>>% unlist %>>%
        paste(collapse = "\n") ->
            modules.source
        
        c(
            modules.init,
            modules.source
        ) %>>%
        paste(collapse = "\n\n") ->
            modules
        
    } else {
        modules = '\n'
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
library(instantShiny)
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
## Modules                                                                    ##
## -------------------------------------------------------------------------- ##
",
          modules,          
          "
## -------------------------------------------------------------------------- ##
## Load .RData files                                                          ##
## -------------------------------------------------------------------------- ##
",
          load) %>>% paste(collapse = "\n")
    
    return(tmp)
}


scaffold_ui <- function(
    title = NULL,
    modules = NULL
){

    if (!is.null(modules)){
        modules  %>>%
        list.map(
            sprintf("source('./module/%s/ui.R', local = TRUE)",.)
        ) %>>% unlist %>>%
        paste(collapse = "\n") ->
            modules
    } else {
        modules = '\n'
    }  
    
    tmp <-
        c("
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
",
          modules,
          "
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
") %>>% paste(collapse = "\n")

    
    o <- rprintv(
        tmp,
        title = title
    )
        
    return(o)
}


scaffold_server <- function(
    modules = NULL
){

    if (!is.null(modules)){
        modules  %>>%
        list.map(
            sprintf("source('./module/%s/global.R', local = TRUE)",.)
        ) %>>% unlist %>>%
        paste(collapse = "\n") ->
            modules
    } else {
        modules = '\n'
    }
    
    tmp <- c("
shinyServer(function(input,output,session){

",
    modules,
    "
    ## source(<module>, local = TRUE)    
})
") %>>% paste(collapse = "\n")
    
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
##' @param tilte 
##' @param libraries 
##' @param sources 
##' @param load 
##' @param modules 
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
    run = TRUE,
    title = 'Test App',
    libraries = NULL,
    sources = NULL,
    load = NULL,
    modules = NULL    
){
    ## Create directory with the app
    oldwd <- getwd()
    newwd <- sprintf('%s/%s',root,app.name)

    if (file.exists(newwd)){
        if (delete.existing == TRUE)
            unlink(newwd, recursive = TRUE, force = TRUE)
        else
            stop(newwd, " already exists. Set `delete.existing = TRUE` if you want to overwrite the folder.") 
    }

    ## Add modules
    if (!is.null(modules)){
        if (any(!modules %in% available.modules))
            stop('One of the modules is not available')        
    }       
    
    if (is.null(root) | is.null(app.name))
        stop("`root` and `app.name` are required arguments.")
    

    scaffold_global(
        libraries = libraries,
        sources = sources,
        load = load,
        modules = modules
    ) %>>%
    save2file(root = root,
              dir.name = app.name,
              file.name = 'global.R')

    scaffold_ui(
        title = title,
        modules = modules        
    ) %>>%
    save2file(root = root,
              dir.name = app.name,
              file.name = 'ui.R')

    scaffold_server(
        modules = modules
    ) %>>%
    save2file(root = root,
              dir.name = app.name,
              file.name = 'server.R')

    setwd(oldwd)
    
    if (run == TRUE){
        shiny::runApp(sprintf('%s/.',newwd))
    }

    cat(sprintf("The app is saved in the following path: %s\n\n", newwd))
    return(newwd)
}


