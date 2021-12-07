dataframe_to_latex_table = function(df, title = TRUE, axis = 0){
    m = dim(df)[1]
    n = dim(df)[2]
    
    
    if (axis == 0){
        total_string = paste(names(df), collapse = ' & ')
        for (i in c(1:m)){
                string = paste(df[i,],collapse = ' & ')
                total_string = paste(c(total_string,string),collapse = '\\\\ \n')
        }
        
    } else{
        total_string = ''
        for (j in c(1:n)){
            headstring = names(df)[j]
            rowstring =  paste(df[,j], collapse = ' & ')
            string = paste(c(headstring, rowstring), collapse = ' & ')
            if (total_string == ''){
                total_string = string
            }else{
                
                total_string = paste(c(total_string, string), collapse = '\\\\ \n ')
            }
            
            
        }
    }
    
    
    output = total_string
    print(total_string) 
}