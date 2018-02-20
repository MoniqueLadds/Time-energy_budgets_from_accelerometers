#include <Rcpp.h>
// #include <stl.h>
using namespace Rcpp;



// [[Rcpp::export]]


CharacterVector add_place(CharacterVector Place, 
                          NumericVector start_date,
                          NumericVector time_stamp){
  
  // diclare variables
  CharacterVector raw_data_place = time_stamp.length();
  int i = 0;
  int j = 0;
  int nr_place = Place.size();
  int nr_time_stamp = time_stamp.size();
  
  for(i = 0; i < nr_place; i++){
    
    while(j < nr_time_stamp &&
          time_stamp(j) >= start_date(i) &&
          time_stamp(j) < start_date(i + 1) ){
      
      raw_data_place(j) = Place(i);
      j++;
      
    } // end while j
    
    
  } // end for i
  
  return(raw_data_place);
}