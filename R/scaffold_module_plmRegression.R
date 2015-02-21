tmp.ui <-  "
reg.$id <- fluidRow(
    box(
        title = 'Inputs',
        selectizeInput(
            'depvar-$id',
            'Choose a dependent variable',
            choices = NULL,
            multiple = FALSE
        ),
        selectInput(
            'modsDepvar-$id',
            NULL,
            choices = NULL,
            selected = NULL,
            multiple = TRUE
        ),                
        selectizeInput(
            'indepSet1-$id',
            'Choose a set of independent variables',
            choices = NULL,
            selected = NULL,
            multiple = TRUE
        ),
        selectInput(
            'modsIndepSet1-$id',
            NULL,
            choices = NULL,
            selected = NULL,
            multiple = TRUE
        ),                                
        selectInput(
            'indepSet2-$id',
            'Choose the additional set of controls:',
            choices = NULL,
            selected = NULL,
            multiple = TRUE
        ),
        selectInput(
            'modsIndepSet2-$id',
            NULL,
            choices = NULL,
            selected = NULL,
            multiple = TRUE
        ),                                                
        selectInput(
            'fe-$id',
            'Fixed Effects:',
            choices = c(
                'none'
            ),
            selected = 'none',
            multiple = TRUE
        ),
        checkboxInput(
            'standardize-$id',
            'Standardize all variables?',
            TRUE
        ),
        width = 4
    ),
    box(
        title = 'Additional parameters',
        selectInput(
            'model-$id',
            'Model?',
            choices = eval(formals(plm)[['model']]),
            selected = 'pooling'
        ),                
        selectInput(
            'effects-$id',
            'Effects?',
            choices = eval(formals(plm)[['effect']]),
            selected = 'individual'
        ),
        h4('Standard errors:'),
        selectInput(
            'se-$id',
            'Which SE to use?',
            choices =
                c('Default',
                  'Back & Katz'),
            selected = 'default'
        ),
        hr(),
        actionButton(
            'addModel-$id',
            'Add model!'
        ),
        actionButton(
            'rmModel-$id',
            'Remove model!'
        ),
        width = 4
    )
)
"

tmp.server <- "
updateSelectizeInput(
    session,
    'depvar-$id',
    choices = NULL,
    selected = NULL
)

updateSelectizeInput(
    session,
    'indepSet1-$id',
    choices = NULL,    
    selected = NULL
)

updateSelectizeInput(
    session,
    'indepSet2-$id',
    choices = NULL,
    selected = NULL
)

reg_output_$id <- reactive({
    input[['addModel-$id']]

    isolate({
        depvar = input[['depvar-$id']] 
        indepSet1 = input[['indepSet1-$id']]
        indepSet2 = input[['indepSet2-$id']]
        absorb = input[['absorb-$id']]
        effect = input[['effects-$id']]
        model = input[['model-$id']]        
        mods.depvar = input[['modsDepvar-$id']] 
        mods.indepSet1 = input[['modsIndepSet1-$id']]
        mods.indepSet2 = input[['modsIndepSet2-$id']]
        standardize = ifelse(input[['standardize-$id']],'Yes','No')       

        if (absorb == 'none')
            static = NULL
        else
            static = NULL

        $data %>>%
        (.[,.SD,.SDcols =
               c(
                   static,
                   depvar,
                   indepSet1,
                   indepSet2
               )])  %>>%
        ({
            na.omit(.)
        }) %>>%
        copy %>>%
        ({
            ## Convert integers to numeric
            colInt <- Filter(is.integer,.) %>>%names
            .[, (colInt) :=  lapply(.SD, as.numeric),
              .SDcols = colInt]
        }) %>>% 
        (
            if (absorb %>>% paste(collapse = ',') != 'none')
                .[,c(depvar,
                     indepSet1,
                     indepSet2) := lapply(.SD, function(x){
                         x - mean(x,na.rm = TRUE)
                     }),
                  by = absorb,
                  .SDcols  =
                      c(depvar,
                        indepSet1,
                        indepSet2)]
            else
                .
        ) %>>%
        (
            if (input[['standardize-$id']] == TRUE)
                .[,c(
                    ## depvar,
                    indepSet1,
                    indepSet2) := lapply(.SD, function(x){
                        (x - mean(x,na.rm = TRUE))/sd(x,na.rm = TRUE)
                    }),
                  .SDcols  =
                      c(
                          ## depvar,
                          indepSet1,
                          indepSet2)]
            else
                .
        ) %>>%            
        (filter(
            .[complete.cases(.)]
        )) %>>%
        ## ({
        ##     setnames(.,
        ##              names(.),
        ##              names(.) %>>% recycleNames_$id)
        ## }) %>>%
        (? cat('Prior to procExpand')) %>>%
        (? cat(mods.depvar %>>% paste(collapse = ';'))) %>>%
        (? cat(mods.indepSet1 %>>% paste(collapse = ';'))) %>>%
        (? cat(mods.indepSet2 %>>% paste(collapse = ';'))) %>>%        
        procExpand(
            by = $idVar,
            keep = $dateVar,
            convert =
                list(
                    sprintf('~%s~%s',
                            depvar %>>% recycleNames_$id,
                            mods.depvar %>>% paste(collapse = ';')),
                    sprintf('~%s~%s',
                            indepSet1 %>>% recycleNames_$id %>>% paste(collapse = ','),
                            mods.indepSet1 %>>% paste(collapse = ';')),
                    sprintf('~%s~%s',
                            indepSet2 %>>% recycleNames_$id %>>% paste(collapse = ','),
                            mods.indepSet2 %>>% paste(collapse = ';'))            
                )
        ) %>>%
        ({
            f <-
                as.formula(
                    sprintf('%s ~ %s + %s',
                            depvar %>>%
                            recycleNames_$id,
                            indepSet1 %>>%
                            recycleNames_$id %>>%
                            sprintf(fmt = '`%s`') %>>%
                            paste(collapse = '+'),
                            indepSet2 %>>%
                            recycleNames_$id %>>%
                            sprintf(fmt = '`%s`') %>>%
                            paste(collapse = '+'))
                )
            
            plm(
                data = .,
                formula = f,
                index = c(idVar,dateVar),
                effect = effect,
                model = model
            )
        }) ->> reg_results_$id[[length(reg_results_$id)+1]]

        depvar ->> reg_depvars_$id[length(reg_depvars_$id)+1]
        effect %>>% paste(collapse = ', ') ->> reg_effects_$id[length(reg_effects_$id)+1]
        model  %>>% paste(collapse = ', ') ->> reg_model_$id[length(reg_model_$id)+1]
        absorb  %>>% paste(collapse = ', ') ->> reg_absorb_$id[length(reg_absorb_$id)+1]
        standardize ->> reg_standardize_$id[length(reg_standardize_$id)+1]    
    })
})

reg_rmModel <- reactive({        
    if (input[['rmModel-$id']]){
        
        reg_results_$id <<- reg_results_$id %>>% (.[-length(.)])
        reg_depvars_$id <<- reg_depvars_$id %>>% (.[-length(.)])
        reg_model_$id <<- reg_model_$id %>>% (.[-length(.)])
        reg_effects_$id <<- reg_effects_$id %>>% (.[-length(.)])
        reg_absorb_$id <<- reg_absorb_$id %>>% (.[-length(.)])
        reg_standardize_$id <<- reg_standardize_$id %>>% (.[-length(.)])
        
        return()
    } else {
        return()
    }       
})

reg_star_$id <- reactive({
    reg_rmModel()
    
    withProgress(
        message = 'Estimation in progress...',
        expr = {
            (reg_output_$id())
        }
    )

    ## Label tests...
    if (input[['console-label-$id']] == TRUE){
        reg_results_$id %>>%
        list.map(coefficients %>>% names) %>>%
        unlist %>>%
        unique %>>%
        (? .) ->
            covariate.labels

        ## Dependent variable labels
        reg_results_$id %>>%
        list.map(model %>>% names %>>% (.[1])) %>>%       
        unlist %>>%
        unique ->
            dep.var.labels        
        
    } else {
        covariate.labels = NULL
        dep.var.labels = NULL
    }
    
    list(
        reg_results,
        add.lines =
            list(reg_model,
                 reg_effects,
                 reg_absorb,
                 reg_standardize),
        covariate.labels = covariate.labels[-1],
        dep.var.labels = dep.var.labels,
        digits = 2
    ) ->
        o   
    
    return(o)
})

output[['console-$id']] <- renderUI({
    do.call(
        stargazer,
        c(
            reg_star_$id(),
            type = 'html'
        )
    ) %>>% HTML
})


output[['download-$id']] <- downloadHandler(
    filename = function(){
        input[['download-fileName-$id']]
    },
    content = function(file){
        sink(file)
        do.call(
            stargazer,
            c(
                reg_star_$id(),
                type = 'html'
            )
        )        
        sink()
    }
)

output[['download-$id']] <- downloadHandler(
    filename = function(){
        sprintf('%s.tar', input[['download-fileName-$id']])
    },
    content = function(file){
        oldwd <- getwd()

        tmp <- sprintf('%s/%s',tempdir(),'temp')
        dir.create(tmp)
        setwd(tmp)
        ## HTML Table

        sink('table.html')
        do.call(
            stargazer,
            c(
                reg_star_$id(),
                type = 'html'
            )
        )        
        sink()

        ## LATEX Table
        sink('table.tex')
        do.call(
            stargazer,
            c(
                reg_star_$id(),
                type = 'latex'
            )
        )        
        sink()        

        tar(tarfile = file, '.')
        setwd(oldwd)

        unlink(tmp, recursive = TRUE)        
    },
    contentType = 'application/tar'
)
"

tmp.global <- "
## DEFINE GLOBAL VARIABLES
library(plm)

reg_results_$id <- list()
reg_depvars_$id <- c()
reg_model_$id <- c('Model')
reg_effects_$id <- c('Main Effects')
reg_absorb_$id <- c('Fixed Effects')
reg_standardize_$id <- c('Standardized?')
reg_setype_$id <- c('SE Type')
"

## id = 'test'
## data = 'data'
## idVar = 'idVar'
## dateVar = 'dateVar'


##' instantShiny module: PLM Regression
##'
##' Function that scaffolds the instantShiny module
##' 
##' @param id ID char string to be appended to input and output shiny objects 
##' @param data name of data file
##' @param idVar name of variable that defines cross-sectional dimension of the data
##' @param dateVar name of variable that defines time-dimension of the data
##' @param app.dir path of the directory in which to insert the module
##' @return NULL 
##' @author Janko Cizel
##'
##' @export
scaffold_module_plmRegression <- function(
    id = '<id>',
    data = '<data>',
    idVar = '<idVar>',
    dateVar = '<dateVar>',
    app.dir = '.'
){
    newwd <- sprintf('%s/module',app.dir)
    
    if (!file.exists(newwd))
        dir.create(newwd)
    
    rprintv(
        tmp.server,
        id = id,
        data = data,
        idVar = idVar,
        dateVar = dateVar
    ) %>>%
    save2file(root = newwd,
              dir.name = 'plmRegression',
              file.name = 'server.R')
    
    
    rprintv(
        tmp.ui,
        id = id
    ) %>>%        
    save2file(root = newwd,
              dir.name = 'plmRegression',
              file.name = 'ui.R')
    
    rprintv(
        tmp.global,
        id = 'test'
    ) %>>%
    save2file(root = newwd,
              dir.name = 'plmRegression',
              file.name = 'global.R')

    return(NULL)
}



