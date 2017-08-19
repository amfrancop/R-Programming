setwd("~/Coursera - DS/specdata")

##FUNCTION 1: pollutantmean

pollutantmean<-function(directory, pollutant, id=1:332)
{
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        ## Function Body
        
        
        # Initialize the mean vector
        m_means<-c()
        
        # Start a loop to read every monitor
        for(i in id)
        {
                # Set the correct name for every file
                if (i<10)
                {
                        n<-paste('00',i, sep = "")
                }
                else if (i >=10 && i<100)
                {
                        n<-paste('0',i,sep = "")
                } else 
                {
                        n<-i
                }
                
                name<-paste(n,'.csv',sep = "")
                
                # Reading the file 
                m_file<-read.csv(name, header = TRUE)
                
                # Subsetting the pollutant column into a vector
                c_file<-m_file[pollutant]
                
                # Removing NA from the c_file
                c_file<-c_file[!is.na(c_file)]
                
                # Concatenate the clean vector to the m_means
                m_means<-c(m_means,c_file)
        }
        
        # Get the mean of the m_means vector
        mean(m_means, na.rm = TRUE)
        
}

## TESTING FUNCTION: Pollutantmean

pollutantmean("aaa", "sulfate", 1:2)
