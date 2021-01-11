library(psych) # for describe	
library(car) # for residualPlots, vif, pairs.panels, ncvTest	
library(lmtest) # bptest	
library(sandwich) # for coeftest vcovHC estimator	
library(boot) # for bootstrapping	
library(lmboot) # for wild bootsrapping	
library(tidyverse)
library(gridExtra)
library(lm.beta)

require(mediation)
library(car)
# ## Custom functions	

# We will use these custom functions to get bootstrapped confidence intervals.	


# function to obtain regression coefficients	
# source: https://www.statmethods.net/advstats/bootstrapping.html	
bs_to_boot <- function(model, data, indices) {	
  d <- data[indices,] # allows boot to select sample 	
  fit <- lm(formula(model), data=d)	
  return(coef(fit)) 	
}	

# function to obtain adjusted R^2	
# source: https://www.statmethods.net/advstats/bootstrapping.html (partially modified)	
adjR2_to_boot <- function(model, data, indices) {	
  d <- data[indices,] # allows boot to select sample 	
  fit <- lm(formula(model), data=d)	
  return(summary(fit)$adj.r.squared)	
}	


# Computing the booststrap BCa (bias-corrected and accelerated) bootstrap confidence intervals by Elfron (1987)	
# This is useful if there is bias or skew in the residuals.	

confint.boot <- function(model, data = NULL, R = 1000){	
  if(is.null(data)){	
    data = eval(parse(text = as.character(model$call[3])))	
  }	
  boot.ci_output_table = as.data.frame(matrix(NA, nrow = length(coef(model)), ncol = 2))	
  row.names(boot.ci_output_table) = names(coef(model))	
  names(boot.ci_output_table) = c("boot 2.5 %", "boot 97.5 %")	
  results.boot = results <- boot(data=data, statistic=bs_to_boot, 	
                                 R=1000, model = model)	
  
  for(i in 1:length(coef(model))){	
    boot.ci_output_table[i,] = unlist(unlist(boot.ci(results.boot, type="bca", index=i))[c("bca4", "bca5")])	
  }	
  
  return(boot.ci_output_table)	
}	

# Computing the booststrapped confidence interval for a linear model using wild bottstrapping as descibed by Wu (1986) <doi:10.1214/aos/1176350142>	
# requires the lmboot pakcage	

wild.boot.confint <- function(model, data = NULL, B = 1000){	
  if(is.null(data)){	
    data = eval(parse(text = as.character(model$call[3])))	
  }	
  
  wild_boot_estimates = wild.boot(formula(model), data = data, B = B)	
  
  result = t(apply(wild_boot_estimates[[1]], 2, function(x) quantile(x,probs=c(.025,.975))))	
  
  return(result)	
  
}	

coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	

Data_utan_bortfall_250_deltagare$Kön = factor(Data_utan_bortfall_250_deltagare$Kön)
Data_utan_bortfall_250_deltagare$Group = factor(Data_utan_bortfall_250_deltagare$Group,
                                                 levels = c("3", "2", "1"))
ggplot(Data_utan_bortfall_250_deltagare) +
  aes(y = Communal, x = Kön) +
  geom_violin()



Mod_1 = lm(Communal ~ Kön + gillameditation, data = Data_utan_bortfall_250_deltagare)
summary(Mod_1) 
AIC(Mod_1)

mod_grp = lm(Communal ~ Group, data =Data_utan_bortfall_250_deltagare)
summary(mod_grp)
AIC(mod_grp)


mod_grp_full = lm(Communal ~ Group + Kön, data =Data_utan_bortfall_250_deltagare)
summary(mod_grp_full)
AIC(mod_grp_full)

data_filtered = Data_utan_bortfall_250_deltagare[, c("Communal", "Group", "Kön", "gillameditation")]


ggplot(data_filtered) +
  aes(y = gillameditation, x = Group) +
  geom_violin()



data_filtered %>% 
  drop_na() %>% 
  filter(Group == "3", Kön == 1) %>% 
  summarize(mean = mean(Communal), sd = sd(Communal))



lm(Communal ~ Group + Kön, data =data_filtered)


data_simulation_function = function(effec_size_Group1, effec_size_Group2, total_N){
  
  effect_size_Kön2 = -0.4373
  
  communal_sim = rnorm(mean = 5.29, sd = 0.710, n = total_N)
  Kön_sim = rbinom(n = total_N, size = 1, prob = 0.5)
  Group = rep(c("Group1", "Group2", "Group3"), each = total_N/3)
  Group_1 = Group
  Group_2 = Group
  
  Group_1[which(Group == "Group1")] = 1
  Group_1[which(Group != "Group1")] = 0
  Group_2[which(Group == "Group2")] = 1
  Group_2[which(Group != "Group2")] = 0
  
  Group_1 = as.numeric(Group_1 )
  Group_2 = as.numeric(Group_2 )
  
  
  Communal = communal_sim + Kön_sim*effect_size_Kön2 + Group_1 * effec_size_Group1 + Group_2 * effec_size_Group2
  
  data_sim = data.frame(Communal = Communal, Kön_sim = Kön_sim, Group = Group)
  
  
  data_sim$Group = factor(data_sim$Group, levels = c("Group3", "Group1", "Group2"))
  
  
  
  mod_grp_full_sim = lm(Communal ~ Group + Kön_sim, data =data_sim)
  p_value = summary(mod_grp_full_sim)$coefficients[2,4]
  
  decision = if(p_value < 0.05){"H1"} else {"Inconclusive"}
  
  return(decision)
}


number_iterations = 10000


all_decisions = replicate(n = number_iterations, data_simulation_function(effec_size_Group1 = 0.3075, effec_size_Group2 = 0.1848, total_N = 120*3))

sum(all_decisions == "H1")/length(all_decisions)






