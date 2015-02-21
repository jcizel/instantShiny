scaffold_module_regression <- function(id){
    tmp.reg_ui <-  "
fluidRow(
    box(
        title = 'Inputs',
        selectizeInput(
            '$id-depvar',
            'Choose a dependent variable',
            choices = NULL,
            multiple = FALSE
        ),
        selectInput(
            '$id-modsDepvar',
            NULL,
            choices = modOptions,
            selected = '`*`(1)',
            multiple = TRUE
        ),                
        selectizeInput(
            '$id-indepSet1',
            'Choose a set of independent variables',
            choices = NULL,
            selected = 'ratingnum',
            multiple = TRUE
        ),
        selectInput(
            '$id-modsIndepSet1',
            NULL,
            choices = modOptions,
            selected = '`*`(1)',
            multiple = TRUE
        ),                                
        selectInput(
            '$id-indepSet2',
            'Choose the additional set of controls:',
            choices = NULL,
            selected = 'spread',
            multiple = TRUE
        ),
        selectInput(
            '$id-modsIndepSet2',
            NULL,
            choices = modOptions,
            selected = '`*`(1)',
            multiple = TRUE
        ),                                                
        selectInput(
            '$id-absorb',
            'Fixed Effects:',
            choices = c(
                'none',
                'Country',
                'Year'
            ),
            selected = 'none',
            multiple = TRUE
        ),
        checkboxInput(
            '$id-standardize',
            'Standardize all variables?',
            TRUE
        ),
        width = 4
    ),
    box(
        title = 'Additional parameters',
        selectInput(
            '$id-model',
            'Model?',
            choices = eval(formals(plm)[['model']]),
            selected = 'pooling'
        ),                
        selectInput(
            '$id-effects',
            'Effects?',
            choices = eval(formals(plm)[['effect']]),
            selected = 'individual'
        ),
        h4('Standard errors:'),
        selectInput(
            '$id-se',
            'Which SE to use?',
            choices =
                c('Default',
                  'Back & Katz'),
            selected = 'default'
        ),
        hr(),
        actionButton(
            '$id-addModel',
            'Add model!'
        ),
        actionButton(
            '$id-rmModel',
            'Remove model!'
        ),
        width = 4
    )
)
"

        tmp.reg_server <- "
updateSelectizeInput(
    session,
    '$id-depvar',
    choices = NULL,
    selected = NULL
)

updateSelectizeInput(
    session,
    '$id-indepSet1',
    choices = NULL,    
    selected = NULL
)

updateSelectizeInput(
    session,
    '$id-indepSet2',
    choices = NULL,
    selected = NULL
)

reg.output <- reactive({
    input[['$id-addModel']]

    isolate({
        depvar = input[['$id-depvar']] 
        indepSet1 = input[['$id-indepSet1']]
        indepSet2 = input[['$id-indepSet2']]
        absorb = input[['$id-absorb']]
        effect = input[['$id-effects']]
        model = input[['$id-model']]        
        mods.depvar = input[['$id-modsDepvar']] 
        mods.indepSet1 = input[['$id-modsIndepSet1']]
        mods.indepSet2 = input[['$id-modsIndepSet2']]
        standardize = ifelse(input[['$id-standardize']],'Yes','No')       

        if (absorb == 'none')
            static = NULL
        else
            static = NULL

        macrodatalarge %>>%
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
            if (input[['$id-standardize']] == TRUE)
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
        ({
            setnames(.,
                     names(.),
                     names(.) %>>% .recycleNames)
        }) %>>%
        (? cat('Prior to procExpand')) %>>%
        (? cat(mods.depvar %>>% paste(collapse = ';'))) %>>%
        (? cat(mods.indepSet1 %>>% paste(collapse = ';'))) %>>%
        (? cat(mods.indepSet2 %>>% paste(collapse = ';'))) %>>%        
        procExpand(
            by = 'iso3',
            keep = 'date',
            convert =
                list(
                    sprintf('~%s~%s',
                            depvar %>>% .recycleNames,
                            mods.depvar %>>% paste(collapse = ';')),
                    sprintf('~%s~%s',
                            indepSet1 %>>% .recycleNames %>>% paste(collapse = ','),
                            mods.indepSet1 %>>% paste(collapse = ';')),
                    sprintf('~%s~%s',
                            indepSet2 %>>% .recycleNames %>>% paste(collapse = ','),
                            mods.indepSet2 %>>% paste(collapse = ';'))            
                )
        ) %>>%
        ({
            f <-
                as.formula(
                    sprintf('%s ~ %s + %s',
                            depvar %>>% .recycleNames,
                            indepSet1 %>>%
                            .recycleNames %>>%
                            sprintf(fmt = '`%s`') %>>%
                            paste(collapse = '+'),
                            indepSet2 %>>%
                            .recycleNames %>>%
                            sprintf(fmt = '`%s`') %>>%
                            paste(collapse = '+'))
                )
            
            plm(
                data = .,
                formula = f,
                index = c('iso3','date'),
                effect = effect,
                model = model
            )
        }) ->> reg.results[[length(reg.results)+1]]

        depvar ->> reg.depvars[length(reg.depvars)+1]
        effect %>>% paste(collapse = ', ') ->> reg.effects[length(reg.effects)+1]
        model  %>>% paste(collapse = ', ') ->> reg.model[length(reg.model)+1]
        absorb  %>>% paste(collapse = ', ') ->> reg.absorb[length(reg.absorb)+1]
        standardize ->> reg.standardize[length(reg.standardize)+1]    
    })
})

reg.rmModel <- reactive({        
    if (input[['$id-rmModel']]){
        
        reg.results <<- reg.results %>>% (.[-length(.)])
        reg.depvars <<- reg.depvars %>>% (.[-length(.)])
        reg.model <<- reg.model %>>% (.[-length(.)])
        reg.effects <<- reg.effects %>>% (.[-length(.)])
        reg.absorb <<- reg.absorb %>>% (.[-length(.)])
        reg.standardize <<- reg.standardize %>>% (.[-length(.)])
        
        return()
    } else {
        return()
    }       
})

reg.star <- reactive({
    reg.rmModel()
    
    withProgress(
        message = 'Estimation in progress...',
        expr = {
            (reg.output())
        }
    )

    ## Label tests...
    if (input[['$id-console-label']] == TRUE){
        reg.results %>>%
        list.map(coefficients %>>% names) %>>%
        unlist %>>%
        unique %>>%
        (? .) ->
            covariate.labels

        ## Dependent variable labels
        reg.results %>>%
        list.map(model %>>% names %>>% (.[1])) %>>%       
        unlist %>>%
        unique ->
            dep.var.labels        
        
    } else {
        covariate.labels = NULL
        dep.var.labels = NULL
    }
    
    list(
        reg.results,
        add.lines =
            list(reg.model,
                 reg.effects,
                 reg.absorb,
                 reg.standardize),
        covariate.labels = covariate.labels[-1],
        dep.var.labels = dep.var.labels,
        digits = 2
    ) ->
        o   
    
    return(o)
})

output[['$id-console']] <- renderUI({
    do.call(
        stargazer,
        c(
            reg.star(),
            type = 'html'
        )
    ) %>>% HTML
})


output[['$id-download']] <- downloadHandler(
    filename = function(){
        input[['$id-download-fileName']]
    },
    content = function(file){
        sink(file)
        do.call(
            stargazer,
            c(
                reg.star(),
                type = 'html'
            )
        )        
        sink()
    }
)

output[['$id-download']] <- downloadHandler(
    filename = function(){
        sprintf('%s.tar', input[['$id-download-fileName']])
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
                reg.star(),
                type = 'html'
            )
        )        
        sink()

        ## LATEX Table
        sink('table.tex')
        do.call(
            stargazer,
            c(
                reg.star(),
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

    o <- list(
        server = rprintv(
            tmp.reg_server,
            id = id
        ),
        ui = rprintv(
            tmp.ui,
            id = id
        )        
    )

    return(o)
}
