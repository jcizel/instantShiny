
updateSelectizeInput(
    session,
    'depvar-test',
    choices = NULL,
    selected = NULL
)

updateSelectizeInput(
    session,
    'indepSet1-test',
    choices = NULL,    
    selected = NULL
)

updateSelectizeInput(
    session,
    'indepSet2-test',
    choices = NULL,
    selected = NULL
)

reg_output_test <- reactive({
    input[['addModel-test']]

    isolate({
        depvar = input[['depvar-test']] 
        indepSet1 = input[['indepSet1-test']]
        indepSet2 = input[['indepSet2-test']]
        absorb = input[['absorb-test']]
        effect = input[['effects-test']]
        model = input[['model-test']]        
        mods.depvar = input[['modsDepvar-test']] 
        mods.indepSet1 = input[['modsIndepSet1-test']]
        mods.indepSet2 = input[['modsIndepSet2-test']]
        standardize = ifelse(input[['standardize-test']],'Yes','No')       

        if (absorb == 'none')
            static = NULL
        else
            static = NULL

        mtcars %>>%
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
            if (input[['standardize-test']] == TRUE)
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
        ##              names(.) %>>% recycleNames_test)
        ## }) %>>%
        (? cat('Prior to procExpand')) %>>%
        (? cat(mods.depvar %>>% paste(collapse = ';'))) %>>%
        (? cat(mods.indepSet1 %>>% paste(collapse = ';'))) %>>%
        (? cat(mods.indepSet2 %>>% paste(collapse = ';'))) %>>%        
        procExpand(
            by = t,
            keep = d,
            convert =
                list(
                    sprintf('~%s~%s',
                            depvar %>>% recycleNames_test,
                            mods.depvar %>>% paste(collapse = ';')),
                    sprintf('~%s~%s',
                            indepSet1 %>>% recycleNames_test %>>% paste(collapse = ','),
                            mods.indepSet1 %>>% paste(collapse = ';')),
                    sprintf('~%s~%s',
                            indepSet2 %>>% recycleNames_test %>>% paste(collapse = ','),
                            mods.indepSet2 %>>% paste(collapse = ';'))            
                )
        ) %>>%
        ({
            f <-
                as.formula(
                    sprintf('%s ~ %s + %s',
                            depvar %>>%
                            recycleNames_test,
                            indepSet1 %>>%
                            recycleNames_test %>>%
                            sprintf(fmt = '`%s`') %>>%
                            paste(collapse = '+'),
                            indepSet2 %>>%
                            recycleNames_test %>>%
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
        }) ->> reg_results_test[[length(reg_results_test)+1]]

        depvar ->> reg_depvars_test[length(reg_depvars_test)+1]
        effect %>>% paste(collapse = ', ') ->> reg_effects_test[length(reg_effects_test)+1]
        model  %>>% paste(collapse = ', ') ->> reg_model_test[length(reg_model_test)+1]
        absorb  %>>% paste(collapse = ', ') ->> reg_absorb_test[length(reg_absorb_test)+1]
        standardize ->> reg_standardize_test[length(reg_standardize_test)+1]    
    })
})

reg_rmModel <- reactive({        
    if (input[['rmModel-test']]){
        
        reg_results_test <<- reg_results_test %>>% (.[-length(.)])
        reg_depvars_test <<- reg_depvars_test %>>% (.[-length(.)])
        reg_model_test <<- reg_model_test %>>% (.[-length(.)])
        reg_effects_test <<- reg_effects_test %>>% (.[-length(.)])
        reg_absorb_test <<- reg_absorb_test %>>% (.[-length(.)])
        reg_standardize_test <<- reg_standardize_test %>>% (.[-length(.)])
        
        return()
    } else {
        return()
    }       
})

reg_star_test <- reactive({
    reg_rmModel()
    
    withProgress(
        message = 'Estimation in progress...',
        expr = {
            (reg_output_test())
        }
    )

    ## Label tests...
    if (input[['console-label-test']] == TRUE){
        reg_results_test %>>%
        list.map(coefficients %>>% names) %>>%
        unlist %>>%
        unique %>>%
        (? .) ->
            covariate.labels

        ## Dependent variable labels
        reg_results_test %>>%
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

output[['console-test']] <- renderUI({
    do.call(
        stargazer,
        c(
            reg_star_test(),
            type = 'html'
        )
    ) %>>% HTML
})


output[['download-test']] <- downloadHandler(
    filename = function(){
        input[['download-fileName-test']]
    },
    content = function(file){
        sink(file)
        do.call(
            stargazer,
            c(
                reg_star_test(),
                type = 'html'
            )
        )        
        sink()
    }
)

output[['download-test']] <- downloadHandler(
    filename = function(){
        sprintf('%s.tar', input[['download-fileName-test']])
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
                reg_star_test(),
                type = 'html'
            )
        )        
        sink()

        ## LATEX Table
        sink('table.tex')
        do.call(
            stargazer,
            c(
                reg_star_test(),
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
