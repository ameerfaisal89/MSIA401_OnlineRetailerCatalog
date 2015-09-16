setwd( "C:/Users/Ameer Asif Khan/Documents/Academic/Northwestern/401 Stat/Project/" );

#### Variable Creation ####

months_ref = "2012-09-01";

# Dataset for Logistic Models

cat_logit = catalog_data[ , -c( 2:4, 17 ) ];

# Months since last purchase, and number of months between last purchase and date created
cat_logit[ , "recency_months" ] = months_diff( catalog_data[ , "datelp6" ], months_ref );
cat_logit[ , "loyalty_months" ] = months_diff( catalog_data[ , "datead6" ], catalog_data[ , "datelp6" ] );

# Binary variables
cat_logit[ , "ordhist_bin" ] = as.numeric( cat_logit[ , "ordhist" ] > 0 );
cat_logit[ , "ordtyr_bin" ] = as.numeric( cat_logit[ , "ordtyr" ] > 0 );
cat_logit[ , "ordlyr_bin" ] = as.numeric( cat_logit[ , "ordlyr" ] > 0 );
cat_logit[ , "ord2ago_bin" ] = as.numeric( cat_logit[ , "ord2ago" ] > 0 );
cat_logit[ , "ord3ago_bin" ] = as.numeric( cat_logit[ , "ord3ago" ] > 0 );
cat_logit[ , "falord_bin" ] = as.numeric( cat_logit[ , "falord" ] > 0 );
cat_logit[ , "sprord_bin" ] = as.numeric( cat_logit[ , "sprord" ] > 0 );
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
cat_lm[ , "loyalty_months" ] = months_diff( catalog_data[ , "datead6" ], catalog_data[ , "datelp6" ] );

cat_lm[ , "slsPerOrd" ] = 0;
cat_lm[ cat_lm[ , "ordhist" ], "slsPerOrd" ] = cat_lm[ cat_lm[ , "ordhist" ], "slshist" ] / cat_lm[ cat_lm[ , "ordhist" ], "ordhist" ];

cat_lm_test = cat_lm[ cat_lm[ , "targdol" ] & !catalog_data[ , "train" ], ];
cat_lm = cat_lm[ cat_lm[ , "targdol" ] & catalog_data[ , "train" ], ];
