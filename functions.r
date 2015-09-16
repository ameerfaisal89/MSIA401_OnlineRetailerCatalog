# Functions to Compute Month Difference

months = function( date ) {
  date = as.POSIXlt( as.Date( date ) );
  return( 12 * date$year + date$mon + 1 );
}

months_diff = function( date1, date_ref ) {
  abs( months( date_ref ) - months( date1 ) );
}

# Function to Display Confusion Matrix for Logistic Regression

print_table = function( model, data = NULL, level = 0.5 ) {
  if ( is.null( data ) ) {
    tab = table( cat_logit[ , "targdol_bin" ], as.numeric( fitted( model ) > level ) );
  }
  
  else {
    predicted = predict( model, newdata = data, type = "resp" );
    tab = table( data[ , "targdol_bin" ], as.numeric( predicted > level ) );
  }
  
  print( addmargins( tab ) );
  print( prop.table( tab, 1 ) );
  print( prop.table( tab, 2 ) );
}

# Function to compute MSEP

MSEP = function( fit_logit, fit_lm, data, level = 0.1 ) {
  predicted_logit = predict( fit_logit, newdata = data, type = "resp" );
  data_logit = data[ predicted_logit > level, ];
  
  predicted_lm = predict( fit_lm, newdata = data_logit );
  predicted_lm = exp( predicted_lm ) - 1;

  msep = sum( ( data_logit[ , "targdol" ] - predicted_lm ) ^ 2 ) / ( nrow( data_logit ) - fit_lm$rank );
  print( msep );
}

# Function for Financial Criterion

fin_criterion = function( fit_logit, fit_lm, data, level = 0.1 ) {
  predicted_logit = predict( fit_logit, newdata = data, type = "resp" );
  targeted_buyers = data[ predicted_logit > level, ];
  
  predicted_lm = predict( fit_lm, newdata = targeted_buyers );
  predicted_lm = exp( predicted_lm ) - 1;
  
  actual_buyers = targeted_buyers[ order( -predicted_lm ), ];
  actual_buyers = actual_buyers[ 1:5000, ];
  actual_response = sum( actual_buyers[ , "targdol" ], na.rm = T );
  print( actual_response );
}

# Steven's Financial Criterion

fin_criterion2 = function( fitlogit, fitlm, data ) {
  predicted_logit = predict( fitlogit, newdata = data, type = "resp" );
  
  predicted_lm = predict( fitlm, newdata = data );
  predicted_lm = exp( predicted_lm ) - 1;
  
  actual_buyers = data[ order( -( predicted_logit * predicted_lm ) ), ];
  actual_buyers = actual_buyers[ 1:5000, ];
  actual_response = sum( actual_buyers[ , "targdol" ], na.rm = T );
  print( actual_response );
}

# Function to Compute Sensitivity

sensitivity_func = function( model, data = NULL, level ) {
  if ( is.null( data ) ) {
    tab = table( cat_logit[ , "targdol_bin" ], as.numeric( fitted( model ) > level ) );
  }
  
  else {
    predicted = predict( model, newdata = data, type = "resp" );
    tab = table( data[ , "targdol_bin" ], as.numeric( predicted > level ) );
  }
  
  return ( tab[ 2, 2 ] / sum( tab[ 2, ] ) );
}

# Function to Compute Specificity

specificity_func = function( model, data = NULL, level ) {
  if ( is.null( data ) ) {
    tab = table( cat_logit[ , "targdol_bin" ], as.numeric( fitted( model ) > level ) );
  }
  
  else {
    predicted = predict( model, newdata = data, type = "resp" );
    tab = table( data[ , "targdol_bin" ], as.numeric( predicted > level ) );
  }
  
  return ( tab[ 1, 1 ] / sum( tab[ 1, ] ) );
}
