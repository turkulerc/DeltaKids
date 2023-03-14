


# function to return number and proportion
sympProp <- function(symp, df, var = "unge_kjonn_t0", value = NULL){
  if (!is.null(value)){
    df <- df %>% filter(!!as.symbol(var) == value)
  }
  n <- df %>% select(!!as.symbol(symp)) %>% filter(!!as.symbol(symp) == 1) %>% nrow() %>% unlist()
  prop <- df %>% select(!!as.symbol(symp)) %>% summarise(propS = mean(!!as.symbol(symp), na.rm = T) * 100) %>% select(propS) %>% unlist() %>% round(0)
  return(paste0(n, " (", prop, "%)"))
}

# function to return mean and sd
meanSd <-  function(variable, df, var = "unge_kjonn_t0", value = NULL){
  if (!is.null(value)){
    df <- df %>% filter(!!as.symbol(var) == value)
  }
  res <- c(paste0(df %>% 
                    summarise(mean = mean(!!as.symbol(variable), na.rm = T)) %>% 
                    select(mean) %>% 
                    unlist() %>% 
                    round(1),
                  " (",
                  df %>% 
                    summarise(sd = sd(!!as.symbol(variable), na.rm = T)) %>% 
                    select(sd) %>% 
                    unlist() %>% 
                    round(1), ")"))
  return(res)
}

# function to return number
returnn <- function(variable, df){
  res <- df %>% 
    filter(!is.na(!!as.symbol(variable))) %>% 
    nrow(.) %>% 
    unlist()
  return(res)
}

# functions to return regression results

regressionLogistic <- function(outcome, covariates, df, var){
  formula <- as.formula(paste0(outcome, "~", paste0(covariates, collapse = "+")))
  model <- glm(formula, family = binomial(), data = df) %>% 
    broom::tidy(exp=T, conf.int=T)
  output <- model %>% 
    filter(term == var) %>% 
    mutate(newVar = paste0(sprintf("%3.2f", estimate), " (", sprintf("%3.2f", conf.low), "-", sprintf("%3.2f", conf.high), ") ", sprintf("%4.3f", p.value))) %>% 
    select(newVar) %>% 
    unlist()
  return(output)
}

regressionPoisson <- function(outcome, covariates, df, var){
  formula <- as.formula(paste0(outcome, "~", paste0(covariates, collapse = "+")))
  model <- glm(formula, family = poisson(link = "log"), data = df) %>% 
    broom::tidy(exp=T, conf.int=T)
  output <- model %>% 
    filter(term == var) %>% 
    mutate(newVar = paste0(round(estimate,2), " (", round(conf.low,2), "-", round(conf.high,2), ") ", round(p.value, 3))) %>% 
    select(newVar) %>% 
    unlist()
  return(output)
}
