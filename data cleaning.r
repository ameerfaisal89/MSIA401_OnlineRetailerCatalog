library( car );
library( ggplot2 );
library( leaps );
library( outliers );
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

# Columns for years of date created and last purchase
catalog_data[ , "yearad" ] = as.numeric( format( catalog_data[ , "datead6" ], "%Y" ) );
catalog_data[ , "yearlp" ] = as.numeric( format( catalog_data[ , "datelp6" ], "%Y" ) );

# Replace NAs in lpuryear by last digit of datelp6
missing_index = is.na( catalog_data[ , "lpuryear" ] );
catalog_data[ missing_index, "lpuryear" ] = catalog_data[ missing_index, "yearlp" ] %% 10;

# Index of all rows with inconsistent last purchase years
# 
# year_inconsistency = ( datelp6_year %% 10 != catalog_data[ , "lpuryear" ] );
#
## Note: no inconsistencies from 2002 to 2008
## We only make changes to the ones that do not match with the datelp6

# catalog_data[ datelp6_year == 2009 & catalog_data[ , "lpuryear" ] == 0 , "last_pur_year" ] = 2010;
# catalog_data[ datelp6_year == 2010 & catalog_data[ , "lpuryear" ] == 1 , "last_pur_year" ] = 2011;
# catalog_data[ datelp6_year == 2011 & catalog_data[ , "lpuryear" ] == 2 , "last_pur_year" ] = 2012;
# catalog_data[ datelp6_year == 2012 & catalog_data[ , "lpuryear" ] == 3 , "last_pur_year" ] = 2013;


# Check for ordtyr and update yearlp
# Reference year is from Jul 2011 to Jun 2012 but datelp6 is accurate within 6 months.
year_inconsistency = ( catalog_data[ , "ordtyr" ] & catalog_data[ , "datelp6" ] < "2011-07-01" );
catalog_data[ year_inconsistency, "datelp6" ] = "2011-07-01";
catalog_data[ year_inconsistency, "yearlp" ] = 2011;

year_inconsistency = ( catalog_data[ , "ordlyr" ] & catalog_data[ , "datelp6" ] < "2010-07-01" );
catalog_data[ year_inconsistency, "datelp6" ] = "2010-07-01";
catalog_data[ year_inconsistency, "yearlp" ] = 2010;

year_inconsistency = ( catalog_data[ , "ord2ago" ] & catalog_data[ , "datelp6" ] < "2009-07-01" );
catalog_data[ year_inconsistency, "datelp6" ] = "2009-07-01";
catalog_data[ year_inconsistency, "yearlp" ] = 2009;

year_inconsistency = ( catalog_data[ , "ord3ago" ] & catalog_data[ , "datelp6" ] < "2008-07-01" );
catalog_data[ year_inconsistency, "datelp6" ] = "2008-07-01";
catalog_data[ year_inconsistency, "yearlp" ] = 2008;

# Make orders for a specific year zero if no sales exist for that year
order_inconsistency = ( catalog_data[ , "ordtyr" ] & !catalog_data[ , "slstyr" ] );
catalog_data[ order_inconsistency, "ordhist" ] = catalog_data[ order_inconsistency, "ordhist" ] - catalog_data[ order_inconsistency, "ordtyr" ];
catalog_data[ order_inconsistency, "ordtyr" ] = 0;

order_inconsistency = ( catalog_data[ , "ordlyr" ] & !catalog_data[ , "slslyr" ] );
catalog_data[ order_inconsistency, "ordhist" ] = catalog_data[ order_inconsistency, "ordhist" ] - catalog_data[ order_inconsistency, "ordlyr" ];
catalog_data[ order_inconsistency, "ordlyr" ] = 0;

order_inconsistency = ( catalog_data[ , "ord2ago" ] & !catalog_data[ , "sls2ago" ] );
catalog_data[ order_inconsistency, "ordhist" ] = catalog_data[ order_inconsistency, "ordhist" ] - catalog_data[ order_inconsistency, "ord2ago" ];
catalog_data[ order_inconsistency, "ord2ago" ] = 0;

order_inconsistency = ( catalog_data[ , "ord3ago" ] & !catalog_data[ , "sls3ago" ] );
catalog_data[ order_inconsistency, "ordhist" ] = catalog_data[ order_inconsistency, "ordhist" ] - catalog_data[ order_inconsistency, "ord3ago" ];
catalog_data[ order_inconsistency, "ord3ago" ] = 0;

# Remove customers without order or sales history
catalog_data = catalog_data[ !( catalog_data[ , "ordhist" ] == 0 & catalog_data[ , "slshist" ] == 0 ), ];

# Fall and Spring orders consistency check
# Assumption: All orders must be either Fall or Spring!
catalog_data[ , "ordhist_new" ] = catalog_data[ , "ordhist" ];

season_inconsistency = ( catalog_data[ , "ordhist" ] <
                           catalog_data[ , "falord" ] + catalog_data[ , "sprord" ] );

catalog_data[ season_inconsistency & catalog_data[ , "yearlp" ] < 2009, "ordhist_new" ] = rowSums( catalog_data[ season_inconsistency & catalog_data[ , "yearlp" ] < 2009, c( "falord", "sprord" ) ] );

# Check for order and sales history inconsistency from 2009 onwards
sums_inconsistency = catalog_data[ , "datead6" ] > "2009-01-01" & ( catalog_data[ , "ordhist" ] > rowSums( catalog_data[ , 10:13 ] ) );

catalog_data[ sums_inconsistency , "ordhist" ] = rowSums( catalog_data[ sums_inconsistency , 10:13 ] );
catalog_data[ sums_inconsistency , "slshist" ] = rowSums( catalog_data[ sums_inconsistency , 5:8 ] );

#### Univariate Outlier Removal ####

uni_outliers = outlier( catalog_data[ , c( 2:3, 5:16 ) ], logical = T );
uni_outliers = ( rowSums( uni_outliers ) > 0 );

catalog_data = catalog_data[ !uni_outliers, ];

