setwd( "C:/Users/Ameer Asif Khan/Documents/Academic/Northwestern/401 Stat/Project/" );

# Exploratory Analysis

corrplot( cor( cat_lm ), method = "number" );

#### Fit Logistic Models ####

# Model with sqrt order predictors ####

fitlogit_null = glm( targdol_bin ~ 1, family = "binomial", data = cat_logit );

fitlogit_step1 = step( fitlogit_null, scope = ~ sqrt( falord ) + sqrt( falord ):sqrt( sprord ) + sqrt( slshist ) + sqrt( ordtyr ) * sqrt( ordlyr ) + sqrt( ord2ago ) + sqrt( ord3ago ) + sqrt( loyalty_months ) + sqrt( recency_months ) + sqrt( slsPerOrd ), direction = "both" );

summary( fitlogit_step1 );

print_table( fitlogit_step1 );
print_table( fitlogit_step1, level = 0.227 );
print_table( fitlogit_step1, cat_logit_test, 0.227 );

# Best model with binary order predictors and log sales predictors ####

fitlogit_null = glm( targdol_bin ~ 1, family = "binomial", data = cat_logit );

fitlogit_step2 = step( fitlogit_null, scope = ~ falord_bin + sprord_bin:falord_bin + log( slshist + 1 ) + ordtyr_bin * ordlyr_bin + ord2ago_bin + ord3ago_bin + sqrt( loyalty_months ) + sqrt( recency_months ) + sqrt( slsPerOrd ), direction = "both" );

summary( fitlogit_step2 );

print_table( fitlogit_step2 );
print_table( fitlogit_step2, level = 0.1 );
print_table( fitlogit_step2, cat_logit_test, 0.1 );

#### Final Logistic Models ####

fitlogit1 = glm( targdol_bin ~ sqrt( recency_months ) + sqrt( loyalty_months ) + sqrt( falord ):sqrt( sprord ) + sqrt( falord ) + sqrt( ordtyr ) * sqrt( ordlyr ) + sqrt( ord2ago ) #+ sqrt( slshist )
                   , family = "binomial", data = cat_logit );

summary( fitlogit1 );
vif( fitlogit1 );

print_table( fitlogit1 );
print_table( fitlogit1, level = 0.227 );
print_table( fitlogit1, cat_logit_test, 0.226 );

1 - pchisq( fitlogit1$null.deviance - fitlogit1$deviance, df = fitlogit1$df.null - fitlogit1$df.residual );

fitlogit2 = glm( targdol_bin ~ sqrt( recency_months ) + sqrt( loyalty_months ) + ordtyr_bin + falord_bin + ord3ago_bin + ord2ago_bin + log( slshist + 1 ) + ordlyr_bin + ordtyr_bin:ordlyr_bin, family = "binomial", data = cat_logit );

summary( fitlogit2 );
vif( fitlogit2 );

print_table( fitlogit2 );
print_table( fitlogit2, level = 0.1 );
print_table( fitlogit2, cat_logit_test, 0.1 );

1 - pchisq( fitlogit2$null.deviance - fitlogit1$deviance, df = fitlogit2$df.null - fitlogit2$df.residual );

plot.roc( cat_logit[ , "targdol_bin" ], fitted( fitlogit_null ) );
plot.roc( cat_logit[ , "targdol_bin" ], fitted( fitlogit1 ) , add = T, col = 2 );
plot.roc( cat_logit[ , "targdol_bin" ], fitted( fitlogit2 ) , add = T, col = 3 );

# Cutoff Level Analysis ####

sen = 0;
spec = 0;

for( i in 1:96 ) {
  level = i / 100;
  sen[ i ] = sensitivity_func( fitlogit1, level = level );
}

for( i in 1:96 ) {
  level = i / 100;
  spec[ i ] = specificity_func( fitlogit1, level = level );
}

plot_data = data.frame( cbind( level = 1:96 / 100, sen, spec ) );

ggplot( plot_data, aes( y = sen, x = level ) ) + geom_point( ) + geom_line( aes( y = sen, x = level ) ) +
  geom_point( aes( y = spec, x = level ), color = "red" ) + geom_line( aes( y = spec, x = level ), color = "red" ) + geom_point( aes( y = spec[ 23 ], x = 0.23 ), color = "blue" ) + ylab( "Sensitivity/Specificity" ) + xlab( "Cutoff Level");

sen = 0;
spec = 0;

for( i in 1:98 ) {
  level = i / 100;
  sen[ i ] = sensitivity_func( fitlogit1, level = level );
}

for( i in 1:98 ) {
  level = i / 100;
  spec[ i ] = specificity_func( fitlogit1, level = level );
}

plot_data = data.frame( cbind( level = 1:98 / 100, sen, spec ) );

ggplot( plot_data, aes( y = sen, x = level ) ) + geom_point( ) + geom_line( aes( y = sen, x = level ) ) +
  geom_point( aes( y = spec, x = level ), color = "red" ) + geom_line( aes( y = spec, x = level ), color = "red" ) + geom_point( aes( y = spec[ 23 ], x = 0.23 ), color = "blue" );
