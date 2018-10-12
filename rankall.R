###############################################
#Function to return the ranking in each state corresponding to a specific rank
# withing a specific state for a spicific outcome ( all three passed as argument)
# args are state, outcome and rank;best, worst or a number. 
# if the number is > number of hospitalk then the function return NA




rankall <- function(outcome, num = "best") {
        
        
        ## Read outcome data
        
        
        my_df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        
        list_valid_outcomes<-c("heart attack",
                               "heart failure",
                               "pneumonia")
        
        list_valid_states <-unique(my_df$State)
        
        
        if(! outcome %in% list_valid_outcomes){
                stop(paste(outcome,
                           " is not a valid outcome. valid outcomes are: heart attack, heart failure, pneumonia",
                           sep=""))
                
        }
        
        
        
        df_result <- data.frame(hospital=rep("",length(list_valid_states)),
                                state=list_valid_states)
       df_result<- df_result%>%arrange(state)
       df_result$hospital <- sapply(df_result[[2]],rankhospitalmodified,my_df,outcome,num)
       row.names(df_result)<-df_result[[2]]
       
       return(df_result)
}



###modifier rank hospital to just read the df once

rankhospitalmodified <- function(state,the_df, outcome, num = "best") {
        
        
        ## Read outcome data
        
        
        my_df <-the_df
        
        
        ## Check that state and outcome are valid
        
        list_valid_outcomes<-c("heart attack",
                               "heart failure",
                               "pneumonia")
        
        list_valid_states <-unique(my_df$State)
        
        
        if(! outcome %in% list_valid_outcomes){
                stop(paste(outcome,
                           " is not a valid outcome. valid outcomes are: heart attack, heart failure, pneumonia",
                           sep=""))
                
        }
        
        if(! state %in% list_valid_states){
                stop(paste(state,
                           " is an invalid state",
                           sep=""))
                
        }
        
        
        ## determine right column to sue depending on the outcome chosen by the user
        
        index_outcomes_column<-c(11,17,23)
        names(index_outcomes_column)<-c("heart attack","heart failure","pneumonia")
        
        #selection hopital in the right state and the 30 day mortality rate corresponding tothe outcom
        
        my_df_filtered <- my_df%>%
                filter(State==state)%>%
                select(hospital=2,
                       outcome=index_outcomes_column[outcome])
        
        ##converting rate in numeric and filtering out NA and arranging by outcome and hospital name to manage tie
        
        my_df_filtered<-my_df_filtered%>%filter(outcome != "Not Available")
        my_df_filtered[,2] <-as.numeric(my_df_filtered[,2])
        my_df_filtered<- my_df_filtered%>%
                filter(!is.na(outcome))%>%
                arrange(outcome,hospital)
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        if (is.numeric(num) & num>nrow(my_df_filtered)){
                
                return(NA)
        }
        
        
        return(my_df_filtered$hospital[ifelse(num=="best",
                                              1,
                                              ifelse(num=="worst",
                                                     nrow(my_df_filtered),
                                                     num))])
}
