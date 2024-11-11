## Functions----

gg_boxplot <- function(dat, xaxis, yaxis, rows, columns) {
  ggplot(data = dat, aes(x = {{xaxis}}, y = {{yaxis}}, group = {{xaxis}})) + 
    geom_jitter() + 
    geom_boxplot(alpha = 0.02) + 
    facet_grid(rows = vars({{rows}}), cols = vars({{columns}}), scales = "free") + 
    theme_bw()
}


backwards_selection <- function(model) {
  original_formula <- formula(model)
  original_formula_str <- paste(deparse(original_formula), collapse = " ")
  if ("lmerModLmerTest" %in% class(model)) {
    data <- model.frame(model)
  } else {
    data <- model$frame
  }
  terms <- attr(terms(original_formula), "term.labels")
  terms <- terms[!grepl("\\|", terms)] # Exclude random effects
  # Function to check if all components of a term appear in other terms
  is_part_of_other_term <- function(term, terms) {
    if (length(terms) == 0) {
      return(FALSE)
    } else {
      if (length(terms) == 1) {
        return(FALSE)
      } else {
        components <- unlist(strsplit(term, split = ":"))
        matches <- sapply(components, function(comp) grepl(paste0("\\b", comp, "\\b"), terms))
        return(apply(matches, 1, all) %>% any)
      }
    }
  }
  terms_update <- terms[!sapply(seq_along(terms), function(i) is_part_of_other_term(terms[i], terms[-i]))]
  anova_results <- list()
  for (term in terms_update) {
    formula_string <- paste0(original_formula_str, " - ", term)
    new_formula <- as.formula(formula_string, env = environment(original_formula))
    new_model <- update(model, formula = new_formula)
    anova_result <- anova(model, new_model)
    anova_results[[term]] <- list(anova = anova_result)
  }
  list(anova = anova_results, summary = summary(model))
}


forwards_selection <- function(model, factors) {
  original_formula <- formula(model)
  original_formula_str <- paste(deparse(original_formula), collapse = " ")
  original_formula_str <- paste(deparse(original_formula), collapse = " ")
  if ("lmerModLmerTest" %in% class(model)) {
    data <- model.frame(model)
  } else {
    data <- model$frame
  }
  terms <- attr(terms(original_formula), "term.labels")
  terms <- terms[!grepl("\\|", terms)] # Exclude random effects
  # Generate all possible combinations of factors not already in the model
  all_combinations <- unlist(lapply(seq_along(factors), function(n) combn(factors, n, paste, collapse = ":")), recursive = FALSE)
  all_combinations <- all_combinations[!all_combinations %in% terms]
  # Function to add new terms
  adding_new_terms <- function(all_combinations, terms) {
    # Remove terms from all_combinations
    new_combinations <- sapply(all_combinations, function(x) {
      for (term in terms) {
        x <- gsub(term, "", x)
      }
      return(x)
    })
    # Keep only strings with no colons, one colon and nothing else, or exactly one colon with non-colon characters on both sides
    valid_combinations <- new_combinations[grepl("^$|^[^:]*$|^:$", new_combinations)]
    # Create a data frame that links the original terms to the remaining terms
    terms_df <- data.frame(original = all_combinations, remaining = new_combinations, stringsAsFactors = FALSE)
    # Find the original terms that correspond to the remaining valid combinations
    original_terms <- terms_df$original[terms_df$remaining %in% valid_combinations]
    return(original_terms)
  }
  terms_update <- adding_new_terms(all_combinations, terms)
  anova_results <- list()
  for (term in terms_update) {
    formula_string <- paste0(original_formula_str, " + ", term)
    new_formula <- as.formula(formula_string, env = environment(original_formula))
    new_model <- update(model, formula = new_formula)
    anova_result <- anova(model, new_model)
    anova_results[[term]] <- list(formula = formula_string, anova = anova_result, summary = summary(new_model))
  }
  anova_results
}