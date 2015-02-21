
reg.test <- fluidRow(
    box(
        title = 'Inputs',
        selectizeInput(
            'depvar-test',
            'Choose a dependent variable',
            choices = NULL,
            multiple = FALSE
        ),
        selectInput(
            'modsDepvar-test',
            NULL,
            choices = NULL,
            selected = NULL,
            multiple = TRUE
        ),                
        selectizeInput(
            'indepSet1-test',
            'Choose a set of independent variables',
            choices = NULL,
            selected = NULL,
            multiple = TRUE
        ),
        selectInput(
            'modsIndepSet1-test',
            NULL,
            choices = NULL,
            selected = NULL,
            multiple = TRUE
        ),                                
        selectInput(
            'indepSet2-test',
            'Choose the additional set of controls:',
            choices = NULL,
            selected = NULL,
            multiple = TRUE
        ),
        selectInput(
            'modsIndepSet2-test',
            NULL,
            choices = NULL,
            selected = NULL,
            multiple = TRUE
        ),                                                
        selectInput(
            'fe-test',
            'Fixed Effects:',
            choices = c(
                'none'
            ),
            selected = 'none',
            multiple = TRUE
        ),
        checkboxInput(
            'standardize-test',
            'Standardize all variables?',
            TRUE
        ),
        width = 4
    ),
    box(
        title = 'Additional parameters',
        selectInput(
            'model-test',
            'Model?',
            choices = eval(formals(plm)[['model']]),
            selected = 'pooling'
        ),                
        selectInput(
            'effects-test',
            'Effects?',
            choices = eval(formals(plm)[['effect']]),
            selected = 'individual'
        ),
        h4('Standard errors:'),
        selectInput(
            'se-test',
            'Which SE to use?',
            choices =
                c('Default',
                  'Back & Katz'),
            selected = 'default'
        ),
        hr(),
        actionButton(
            'addModel-test',
            'Add model!'
        ),
        actionButton(
            'rmModel-test',
            'Remove model!'
        ),
        width = 4
    )
)
