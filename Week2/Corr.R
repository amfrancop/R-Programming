setwd("~/Coursera - DS/specdata")

##FUNCTION 3: corr

corr<-function(directory, threshold = 0)
{
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of lenght 1 indicating the
        ## number of completely observed observations (on all 
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is zero
        
        ## Return a numeric vector of correlations
        
        ## Function Body
        
        # Creating empty vector for correlations
        c_vector<- c()
        
        # Get a dataframe with id / num. of obs
        id_nobs <- complete("", 1:332)
        
        # Applying threshold to subset
        nobs <- id_nobs$nobs
        s_data <- id_nobs$id[nobs > threshold]
        
        # Start a loop to read every monitor
        for(i in s_data)
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
                
                # Computing Correlation of a file
                corr_result <- cor(c_file$sulfate, c_file$nitrate, use = "complete.obs")
                #cat("j =",j, "resultado: ", corr_result[j])
                
                
                # Filling the vector with correlations
                
                c_vector <-c(c_vector,corr_result)
        }
        
        # Returning vector of correlation
        c_vector
        
        
}

## TESTING FUNCTION: corr

cr <-corr("",150)
head(cr)
