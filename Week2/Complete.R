setwd("~/Coursera - DS/specdata")

##FUNCTION 2: complete

complete<-function(directory, id=1:332)
{
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id'is the monitor ID number and nobs
        ## is the number of complete cases
        
        ## Function Body
        
        #Create empty dataframe
        id_nobs <- data.frame(id=integer(), nobs=integer())
        
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
                
                # Removing NA from dataset
                l_file<-complete.cases(m_file)
                c_file<-m_file[l_file,]
                
                # Filling the data frame with the id-nobs
                id_nobs<-rbind(id_nobs,c(id=i,nobs=nrow(c_file)))
        }
        
        # Adding column names to dataframe id_nobs
        names(id_nobs)<-c("id","nobs")
        
        # Returning dataframe id_nobs
        id_nobs
        
}

## TESTING FUNCTION: Complete

complete("",1:20)