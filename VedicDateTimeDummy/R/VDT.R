#'VedicDateTime
#'Prajwal Patil

#' A list containing all the Samvatsars in chronological order
samvatsar_list <- list('Prabhava','Vibhava','Shukla','Pramoda','Prajapati','Angirasa',
                'Shri Mukha','Bhava','Yuva','Dhatri','Ishvara','Bahudhanya',
                'Pramathi','Vikrama','Vrisha','Chitrabhanu','Svabhanu',
                'Tarana','Parthiva','Vyaya','Sarvajeeth','Sarvadhari',
                'Virodhi','Vikriti','Khara','Nandana','Vijaya','Jaya',
                'Manmatha','Durmukhi','Hevilambi','Vilambi','Vikari',
                'Sharvari','Plava','Shubhakruta','Shobhakruta',
                'Krodhi','Vishvavasu','Parabhava','Plavanga','Kilaka',
                'Saumya','Sadharana','Virodhikruthi','Paridhavi',
                'Pramadicha','Ananda','Rakshasa','Anala','Pingala',
                'Kalayukti','Siddharthi','Raudra','Durmathi','Dundubhi',
                'Rudhirodgari','Raktakshi','Krodhana','Akshaya')


#'Used to calculate the Vikram samvat year from a given year and month
#'@export
#'@param year Year as an integer
#'@param month Month as an integer
vikram_samvat<-function(year,month = 4){

  #Standard Calculation
  vikram_samvat_year <- year + 56

  #TODO: Calibrate the Samvatsar according
  #      to the date of Gudi Padwa(Ugadi)
  #Assuming Gudi Padwa lies in April
  if(month >= 4){
    vikram_samvat_year = vikram_samvat_year + 1
  }
  return(vikram_samvat_year)
}

#'Used to calculate the Vikram samvat year from system date
#'@export
get_vikram_samvat<-function(){
  system_year <- as.integer(format(Sys.Date(),format = "%Y"));
  system_month <- as.integer(format(Sys.Date(),format = "%m"));
  #Calls function vikram_samvat() by passing system
  #year and month as its arguments.
  return(vikram_samvat(system_year,system_month))
}

#'Used to calculate the Shaka samvat year from a given year and month
#'@export
#'@param year Year as an integer
#'@param month Month as an integer
shaka_samvat<-function(year,month = 4){

  #Standard Calculation
  shaka_samvat_year <- year - 78

  #TODO: Calibrate the Samvatsar according
  #      to the date of Gudi Padwa(Ugadi)
  #Assuming Gudi Padwa lies in April
  if(month >= 4){
    shaka_samvat_year = shaka_samvat_year + 1
  }
  if(year < 78){
    shaka_samvat_year = 0;
  }
  return(shaka_samvat_year)
}

#'Used to calculate the Shaka samvat year from system date
#'@export
get_shaka_samvat<-function(){
  system_year <- as.integer(format(Sys.Date(),format = "%Y"));
  system_month <- as.integer(format(Sys.Date(),format = "%m"));
  #Calls function shaka_samvat() by passing system
  #year and month as its arguments.
  return(shaka_samvat(system_year,system_month))
}

#'Used to retrieve the samvatsar
#'@export
#'@param year Year as an integer
#'@param month Month as an integer
samvatsar<-function(year,month = 4){
  vikram_samvat_year <- vikram_samvat(year,month)
  year_index <- ((vikram_samvat_year - 3) %% 60)
  return(samvatsar_list[[year_index]])
}

#'Used to retrieve the samvatsar from system date
#'@export
get_samvatsar<-function(){
  system_year <- as.integer(format(Sys.Date(),format = "%Y"));
  system_month <- as.integer(format(Sys.Date(),format = "%m"));
  #Calls function samvatsar() by passing system
  #year and month as its arguments.
  return(samvatsar(system_year,system_month))
}
