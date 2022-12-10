library(rlang)
library(tidyverse)
library(performance)

data(diamonds)
head(diamonds)
diam <- diamonds %>% slice(sample(500, size = 200))
crabs <- read.table("https://users.stat.ufl.edu/~aa/cat/data/Crabs.dat", header = T)
crabs

rv_scatterplot <- function(data = data, 
                           x, 
                           y,
                           model = NULL,
                           modelFamily = gaussian,
                           modelLink = identity,
                           bestTransformation = F,
                           modelWeights = NULL,
                           test = NULL,
                           alternative = NULL,
                           g = NULL,
                           method = NULL,
                           labelx = .05,
                           labely = .95,
                           just = "left",
                           colour = "black",
                           fill = NULL,
                           blocks = NULL,
                           wrap_by=NULL,
                           title=NULL,
                           xlab=NULL,
                           ylab=NULL,
                           sub=NULL,
                           classic=T,
                           method1 = NULL,
                           ...) {
  

  
  # data ----------------------------------
  
    # Goals 
    # 1. switch between tests (works with more parameters than necessary)
    # 2. output the tests
    # 3. send messages
  
  # Preparing data frames
  if (is.null(data)) {
    data <- data.frame(x, y)
  }
  
  data <- data %>% filter(!is.na({{x}}), !is.na({{y}}))
  
  # Statistical Analysis -----------------------------------
  
  
  # 2-Sample tests # ----------------------------------
  if (!is.null(test)) {
    
    # Get test expression
    test.expr <- enexpr(test)
    
    # Set test expression
    test.string <- as.character(substitute(test))
 
    if (is_true(test.string == "t.test" | test.string == "wilcox.test" | test.string == "cor.test")) {
      test.result.expr <- expr((!!test.expr)(x, y,
                                             alternative = alternative, 
                                             method = method, ...))
    } else if (is_true(test.string == "chisq.test")   ) { 
      test.result.expr <- expr((!!test.expr)(x, y, ...))
    } else {
      print("Test not found")
    }
    
    # Evaluate test expression 
    result <- eval_tidy(test.result.expr, data = data)
    label.p.value <- signif(result$p.value, 3)
    print(result)
  }
  
  # End Two Sample Tests -------------------------------------
  
  # Modeling # ------------------------------------------------
  
  # First checks if user specified model to be nonempty, skips modeling if it is empty
  if (!is.null(model)) {
    
    
    
    # At this point, the user has input something in the model, intended to be glm or lm or really
    # anything of that particular type, if the family
    # is specified, then it uses that family, otherwise, it will just do a simple linear model
    
    # Prepare model expression
    model.expr <- enexpr(model)
    family <- enexpr(modelFamily)
    x.var <- ensym(x)
    y.var <- ensym(y)
    link = enexpr(modelLink)
    
    
    # Create model expression
    model.result.expr <- expr((!!model.expr)((!!y.var) ~ (!!x.var), family = (!!family)((!!link)), data = data))
    

    # Evaluate model expression
    result <- eval_tidy(model.result.expr)
    
    
    # Model output
    model.summary <- summary(result)
    model.summary$call <- expr(glm(y ~ x, data = data))
    print(model.summary)
    label.p.value <- signif(as.numeric(model.summary$coefficients[2, 4]), 3)
    
    # Model Checking
    if (as.character(substitute(modelFamily)) == "gaussian") {
      print(performance::check_heteroscedasticity(result))
      print(performance::check_normality(result))
      print(performance::check_outliers(result))
    } else if (as.character(substitute(modelFamily)) == "binomial") {
      print(performance::performance_hosmer(result))
      print(performance::check_outliers(result))
    }
    
    
  }
  
  

#  End Modeling ----------------------------------------------- 
  
  # Labeling Work # ------------------
  
  # Formatting p-value label
  if (label.p.value < .001) {
    label.p.value = "P < 0.001"
  } else if (label.p.value > .001) {
    label.p.value = paste("P =", label.p.value)
  } 
  
  # Creating label to be used for plotting
  label <- grid::textGrob(label = label.p.value,
                          x = unit(labelx, "npc"),
                          y = unit(labely, "npc"), just = {{just}},
                          gp = grid::gpar(size = 14, fontface = "bold"))

  # End Labeling Work 
  
# Plotting Settings ---------------------------------

   
   gg=ggplot(data,aes(x={{x}},y={{y}})) +
    geom_point(colour = colour) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) 
 

  # If there was a test
  if (!is.null(test)) {
    gg = gg + annotation_custom(label)
  }
  
  # If there was a model
  if (!is.null(model)) {
    gg = gg + annotation_custom(label)
    if (as.character(substitute(modelFamily)) == "gaussian") {
      gg = gg + stat_smooth(method = "glm", se = F, 
                            method.args = list(family = gaussian))
    } else if (as.character(substitute(modelFamily)) == "binomial") {
      gg = gg + stat_smooth(method = "glm", se = F, 
                            method.args = list(family = binomial))
    }
    
  }
  
  # Plot 
  gg
# --------------------------------------------------------  

}



# Caveats, if you do logistic regression, you'll need to specify the modelLink, I could do like an
# if statement to change it, but kinda don't want to right now.

rv_scatterplot(crabs, weight, y, model = glm, modelFamily = binomial, modelLink = logit,
               colour = "blue") +
  theme_classic()

rv_scatterplot(diam, depth, y, model = glm, modelFamily = gaussian,
               colour = "blue") +
  theme_classic()

rv_scatterplot(diam, x, y, model = glm, modelFamily = gaussian,
               colour = "blue") +
  theme_classic()

rv_scatterplot(diam, depth, x, test = t.test, paired = T, colour = 'purple') + theme_classic()

rv_scatterplot(diam, depth, x, test = t.test, paired = T,
               mu = 3, colour = 'purple') + theme_classic()

rv_scatterplot(diam, x, y, test = wilcox.test, colour = "orange") + theme_classic()

rv_scatterplot(diam, carat, price, test = cor.test, method = "kendall", colour = "blue")

rv_scatterplot(diam, carat, x, test = cor.test, method = "kendall", colour = "blue")

rv_scatterplot(diam, carat, x, test = cor.test, method = "pearson")

rv_scatterplot(diam, carat, x, test = cor.test, method = "pearson", colour = "yellow")

























































































































































































