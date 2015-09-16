setwd( "C:/Users/Ameer Asif Khan/Documents/Academic/Northwestern/401 Stat/Project/" );

# Fit Linear Models

fit_null = lm( log( targdol + 1 ) ~ 1, data = cat_lm );

#### Model with sqrt predictors ####

fit_step1 = step( fit_null, scope = ~ sqrt( ordhist ) + sqrt( slshist ) + sqrt( ordtyr * ordlyr ) + sqrt(   ordtyr ) + sqrt( ordlyr ) + sqrt( slstyr * slslyr ) + sqrt( slstyr ) + sqrt( slslyr ) + recency_months + sqrt( slsPerOrd ), direction = "both" );

summary( fit_step1 );
vif( fit_step1 );

MSEP( fitlogit1, fit_step1, cat_logit_test, 0.227 );
fin_criterion( fitlogit1, fit_step1, cat_logit_test, level = 0.227 );

fit_reg1 = regsubsets( log( targdol + 1 ) ~ sqrt( ordhist ) + sqrt( slshist ) + sqrt( ordtyr * ordlyr ) + sqrt( ordtyr ) + sqrt( ordlyr ) + sqrt( slstyr * slslyr ) + sqrt( slstyr ) + sqrt( slslyr ) + recency_months + sqrt( slsPerOrd ), data = cat_lm, method = "exhaustive" );

plot( fit_reg1 );

fit1 = lm( log( targdol + 1 ) ~ sqrt( slshist ) + sqrt( ordhist ) + sqrt( slstyr * slslyr ), data = cat_lm );

summary( fit1 );
vif( fit1 );

MSEP( fitlogit1, fit1, cat_logit_test, 0.2278 );
fin_criterion( fitlogit1, fit1, cat_logit_test, 0.2278 );

avPlots( fit1 );
crPlots( fit1 );

n = nrow( cat_lm );
p = fit1$rank - 1;

# Infulential + Outlier Removal using Cook's Distance
cutoff = 4 / ( n - p - 1 );
outliers = which( cooks.distance( fit1 ) > cutoff );

influencePlot( fit1 );
plot( fit1, which = 4, cook.levels = cutoff );

cat_lm = cat_lm[ -outliers, ];

fit1 = lm( log( targdol + 1 ) ~ sqrt( slshist ) + sqrt( ordhist ) + sqrt( slstyr * slslyr ), data = cat_lm );

summary( fit1 );
vif( fit1 );

MSEP( fitlogit1, fit1, cat_logit_test, 0.2278 );
fin_criterion( fitlogit1, fit1, cat_logit_test, 0.2278 );

#### Model with inverse transforms on orders and logarithmic transforms on sales predictors ####

fit_null = lm( log( targdol + 1 ) ~ 1, data = cat_lm );

fit_step2 = step( fit_null, scope = ~ I( 1 / ( ordhist + 1 ) ) + log( slshist + 1 ) + I( 1 / ( ord2ago + 1 ) ) + loyalty_months  + log( slstyr * slslyr + 1 ) + log( slstyr + 1 ) + log( slslyr + 1 ) + log( slsPerOrd + 1 ), direction = "both" );

summary( fit_step2 );
vif( fit_step2 );

MSEP( fitlogit2, fit_step2, cat_logit_test, 0.227 );
fin_criterion( fitlogit2, fit_step2, cat_logit_test, 0.227 );

crPlots( fit_step2 );

fit_reg2 = regsubsets( log(targdol + 1) ~ I( 1/( ordhist + 1 ) ) + log( slshist + 1 ) + I( 1/( ord2ago + 1 ) ) + loyalty_months  + log( slstyr * slslyr + 1 ) + log( slstyr + 1 ) + log( slslyr + 1 ) + log( slsPerOrd + 1 ), data = cat_lm, method = "exhaustive" );

plot( fit_reg2 );

fit2 = lm( log( targdol + 1 ) ~ I( 1 / ( ordhist + 1 ) ) + log( slshist + 1 ) + log( slstyr * slslyr + 1 ), cat_lm );

summary( fit2 );
vif( fit2 );

MSEP( fitlogit2, fit2, cat_logit_test, 0.2297 );
fin_criterion( fitlogit2, fit2, cat_logit_test, 0.2297 );

avPlots( fit2 );
crPlots( fit2 );

n = nrow( cat_lm );
p = fit2$rank - 1;

# Infulential + Outlier Removal using Cook's Distance
cutoff = 4 / ( n - p - 1 );
outliers = which( cooks.distance( fit2 ) > cutoff );

influencePlot( fit2 );
plot( fit2, which = 4, cook.levels = cutoff );

cat_lm = cat_lm[ -outliers, ];

fit2 = lm( log( targdol + 1 ) ~ I( 1 / ( ordhist + 1 ) ) + log( slshist + 1 ) + log( slstyr * slslyr + 1 ), cat_lm );

summary( fit2 );
vif( fit2 );

MSEP( fitlogit2, fit2, cat_logit_test, 0.2297 );
fin_criterion( fitlogit2, fit2, cat_logit_test, 0.2297 );
