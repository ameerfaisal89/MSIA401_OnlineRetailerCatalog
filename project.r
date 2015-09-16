library( car );
library( ggplot2 );
library( leaps );
library( corrplot );
library( pROC );

setwd( "C:/Users/Ameer Asif Khan/Documents/Academic/Northwestern/401 Stat/Project/" );

catalog_data = data.frame( read.csv( "catalog sales data for 2014 project.csv", header = T, stringsAsFactors = F ) );

# Convert to Date type

catalog_data = transform( catalog_data, datead6 = as.Date( datead6, "%m/%d/%Y" ),
                          datelp6 = as.Date( datelp6, "%m/%d/%Y" ) );

#### Data Cleaning ####

# Check for NAs

sapply( catalog_data, function( x ) all( !is.na( x ) ) );

# Remove Blank rows

blank_index = apply( catalog_data[ , c( 1, 5:16 ) ], MARGIN = 1,
                     function( x ) all( x == 0 ) );

catalog_data = catalog_data[ !blank_index, ];

# Get the 4 digit year from date last purchase and date created

datelp6_year = as.numeric( format( catalog_data[ , "datelp6" ], "%Y" ) );
datead6_year = as.numeric( format( catalog_data[ , "datead6" ], "%Y" ) );

# Replace NAs in lpuryear by last digit of datelp6

missing_index = is.na( catalog_data[ , "lpuryear" ] );
catalog_data[ missing_index, "lpuryear" ] = datelp6_year[ missing_index ] %% 10;

# Index of all rows with inconsistent last purchase years
# 
# year_inconsistency = ( datelp6_year %% 10 != catalog_data[ , "lpuryear" ] );
# 
# length( catalog_data[ datelp6_year == 2012 & year_inconsistency, "lpuryear" ] );
# nrow( catalog_data[ datelp6_year == 2012 & catalog_data$lpuryear == 3, ] );
# catalog_data[ datelp6_year == 2012 & catalog_data$lpuryear < 2, "ordhist" ];

# New last purchase year column

catalog_data[ , "yearad" ] = datead6_year;
catalog_data[ , "yearlp" ] = datelp6_year;

## Note: no inconsistencies from 2002 to 2008
## We only make changes to the ones that do not match with the datelp6

# catalog_data[ datelp6_year == 2009 & catalog_data[ , "lpuryear" ] == 0 , "last_pur_year" ] = 2010;
# catalog_data[ datelp6_year == 2010 & catalog_data[ , "lpuryear" ] == 1 , "last_pur_year" ] = 2011;
# catalog_data[ datelp6_year == 2011 & catalog_data[ , "lpuryear" ] == 2 , "last_pur_year" ] = 2012;
# catalog_data[ datelp6_year == 2012 & catalog_data[ , "lpuryear" ] == 3 , "last_pur_year" ] = 2013;

# Fall and Spring orders consistency check
# Assumption: All orders must be either Fall or Spring!

catalog_data[ , "ordhist_new" ] = catalog_data[ , "ordhist" ];

season_inconsistency = ( catalog_data[ , "ordhist" ] <
                        catalog_data[ , "falord" ] + catalog_data[ , "sprord" ] );

catalog_data[ season_inconsistency & datead6_year < 2009, "ordhist_new" ] = rowSums( catalog_data[ season_inconsistency & datead6_year < 2009, c( "falord", "sprord" ) ] );

# Check for ordtyr and update yearlp
# Reference year is from Jul 2011 to Jun 2012

# year_inconsistency = ( catalog_data[ , "ordtyr" ] & catalog_data[ , "datelp6" ] < "2011-07-01" );
# catalog_data[ year_inconsistency, "ordhist_new" ] = catalog_data[ year_inconsistency, "ordhist_new" ] - catalog_data[ year_inconsistency, "ordtyr" ];
# catalog_data[ year_inconsistency, "ordtyr" ] = 0;
# 
# year_inconsistency = ( catalog_data[ , "ordlyr" ] & catalog_data[ , "datelp6" ] < "2010-07-01" );
# catalog_data[ year_inconsistency, "ordhist_new" ] = catalog_data[ year_inconsistency, "ordhist_new" ] - catalog_data[ year_inconsistency, "ordlyr" ];
# catalog_data[ year_inconsistency, "ordlyr" ] = 0;
# 
# year_inconsistency = ( catalog_data[ , "ord2ago" ] & catalog_data[ , "datelp6" ] < "2009-07-01" );
# catalog_data[ year_inconsistency, "ordhist_new" ] = catalog_data[ year_inconsistency, "ordhist_new" ] - catalog_data[ year_inconsistency, "ord2ago" ];
# catalog_data[ year_inconsistency, "ord2ago" ] = 0;
# 
# year_inconsistency = ( catalog_data[ , "ord3ago" ] & catalog_data[ , "datelp6" ] < "2008-07-01" );
# catalog_data[ year_inconsistency, "ordhist_new" ] = catalog_data[ year_inconsistency, "ordhist_new" ] - catalog_data[ year_inconsistency, "ord3ago" ];
# catalog_data[ year_inconsistency, "ord3ago" ] = 0;

# Make orders for a specific year zero if no sales exist for that year
# 
# order_inconsistency = ( catalog_data[ , "ordtyr" ] & !catalog_data[ , "slstyr" ] );
# catalog_data[ order_inconsistency, "ordtyr" ] = 0;
# 
# order_inconsistency = ( catalog_data[ , "ordlyr" ] & !catalog_data[ , "slslyr" ] );
# catalog_data[ order_inconsistency, "ordlyr" ] = 0;
# 
# order_inconsistency = ( catalog_data[ , "ord2ago" ] & !catalog_data[ , "sls2ago" ] );
# catalog_data[ order_inconsistency, "ord2ago" ] = 0;
# 
# order_inconsistency = ( catalog_data[ , "ord3ago" ] & !catalog_data[ , "sls3ago" ] );
# catalog_data[ order_inconsistency, "ord3ago" ] = 0;

# Check for order and sales history inconsistency from 2009 onwards

check_orders = catalog_data[  catalog_data[ , "datead6" ] > "2008-07-01", c( "ordhist", "ordtyr", "ordlyr", "ord2ago", "ord3ago" ) ];

check_sales = catalog_data[  catalog_data[ , "datead6" ] > "2008-07-01", c( "slshist", "slstyr", "slslyr", "sls2ago", "sls3ago" ) ];

sum( check_orders[ , 1 ] > rowSums( check_orders[ , 2:5 ] ) );
sum( check_sales[ , 1 ] > rowSums( check_sales[ , 2:5 ] ) );

#### Variable Creation ####

# Functions to Compute Month ####

months = function( date ) {
  date = as.POSIXlt( as.Date( date ) );
  return( 12 * date$year + date$mon + 1 );
}

months_diff = function( date, months_ref ) {
  months_ref - months( date );
}

months_ref = months( "2012-09-01" );

# Dataset for Logistic Models

cat_logit = catalog_data[ , -c( 2:4, 17 ) ];

cat_logit[ , "recency_months" ] = months_diff( catalog_data[ , "datelp6" ], months_ref );
cat_logit[ , "targdol_bin" ] = as.numeric( cat_logit[ , "targdol" ] > 0 );

cat_logit[ , "slsPerOrd" ] = 0;
cat_logit[ cat_logit[ , "ordhist" ], "slsPerOrd" ] = cat_logit[ cat_logit[ , "ordhist" ], "slshist" ] / cat_logit[ cat_logit[ , "ordhist" ], "ordhist" ];

cat_logit_test = cat_logit[ catalog_data[ , "train" ] == 0, ];
cat_logit = cat_logit[ catalog_data[ , "train" ] > 0, ];

# random_data = cat_logit[ cat_logit$targdol_bin == 0, ];
# random_data = random_data[ sample( nrow( random_data ), sum( cat_logit$targdol_bin ) ), ];
# cat_logit2 = data.frame( rbind( cat_logit[ cat_logit$targdol_bin > 0, ], random_data ) );

# Dataset for Linear Models

cat_lm = catalog_data[ , -c( 2:4, 17 ) ];
cat_lm[ , "recency_months" ] = months_diff( catalog_data[ , "datelp6" ], months_ref );

cat_lm[ , "slsPerOrd" ] = 0;
cat_lm[ cat_lm[ , "ordhist_new" ], "slsPerOrd" ] = cat_lm[ cat_lm[ , "ordhist_new" ], "slshist" ] / cat_lm[ cat_lm[ , "ordhist_new" ], "ordhist_new" ];

cat_lm[ , "sqrt_slshist" ] = sqrt( cat_lm[ , "slshist" ] );
cat_lm[ , "sqrt_slstyr" ] = sqrt( cat_lm[ , "slstyr" ] );
cat_lm[ , "sqrt_slslyr" ] = sqrt( cat_lm[ , "slslyr" ] );

cat_lm_test = cat_lm[ cat_lm[ , "targdol" ] & !catalog_data[ , "train" ], ];
cat_lm = cat_lm[ cat_lm[ , "targdol" ] & catalog_data[ , "train" ], ];

n = nrow( cat_lm );

cat_lm2 = data.frame( scale( cat_lm ) );

# Exploratory Analysis

corrplot( cor( cat_lm ), method = "number" );

# Fit Logistic Models

fitnull_logit = glm( targdol_bin ~ 1, family = "binomial", data = cat_logit );
fitstep_logit = step( fitnull_logit, scope = ~ falord * sprord + log( slshist + 1 ) + ordtyr * ordlyr + ord2ago + I( yearad - yearlp ) + recency_months + slsPerOrd, direction = "both" );
summary( fitstep_logit );

fitstep_logit = step( fitnull_logit, scope = ~ falord * sprord + sqrt( slshist ) + ordtyr * ordlyr + ord2ago + I( yearad - yearlp ) + recency_months + sqrt( slsPerOrd ), direction = "both" );
summary( fitstep_logit );

tab = table( cat_logit[ , "targdol_bin" ], as.numeric( fitted( fitstep_logit ) > 0.5 ) );
tab;
prop.table( tab, 1 );
prop.table( tab, 2 );

plot.roc( cat_logit[ , "targdol_bin" ], fitted( fitstep_logit ) , add = T, col = 5 );

predicted = predict( )

# Fit Linear Models

fitnull = lm( log( targdol + 1 ) ~ 1, data = cat_lm );
fitstep = step( fitnull, scope = ~ sqrt( ordhist ) + sqrt( slshist ) + sqrt( ordtyr ) * sqrt( ordlyr ) + I( yearad - yearlp ) + sqrt( slstyr * slslyr ) + sqrt( slstyr ) + sqrt( slslyr ) + recency_months + sqrt( slsPerOrd ), direction = "both" );
summary( fitstep );

fitnull = lm( log( targdol + 1 ) ~ 1, data = cat_lm );
fitstep = step( fitnull, scope = ~ ordhist + sqrt_slshist + ordtyr:ordlyr + I( yearad - yearlp ) + sqrt_slstyr * sqrt_slslyr + slsPerOrd + recency_months, direction = "both" );
summary( fitstep );

p = 4;

leverage = hat( model.matrix( fitstep ) );
influential = ( leverage > ( 2 * ( p + 1 ) / n ) );
influencePlot( fitstep );

stud_residuals = rstudent( fitstep );
outliers = abs( stud_residuals ) > 2;

cat_lm = cat_lm[ !( influential | outliers ), ];
cat_lm2 = data.frame( scale( cat_lm ) );
n = nrow( cat_lm2 );

fitreg = regsubsets( log( targdol + 1 ) ~ sqrt( ordhist ) + sqrt( slshist ) + sqrt( ordtyr ) * sqrt( ordlyr ) + I( yearad - yearlp ) + sqrt( slstyr * slslyr ) + sqrt( slstyr ) + sqrt( slslyr ) + recency_months + sqrt( slsPerOrd ), data = cat_lm, method = "exhaustive" );
summary( fitreg );
plot( fitreg );
