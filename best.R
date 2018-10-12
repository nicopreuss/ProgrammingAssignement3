###############################################
#Function to return the hospital within a state( passed as arugment state) with 
#the best measure in the metric chosen ( outcome argument).
#outcome can be either "heart attack", "heart failure","pneumonia"
#handling ties with alphabetic orders
#handling errors for wrong input


best <- function(state, outcome) {
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
                                                                                                
        return(my_df_filtered$hospital[1])
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
}