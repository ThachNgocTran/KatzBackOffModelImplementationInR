##########################################################################################################
# Important assumption: This is for 3-gram table. Do similarly for 2-gram or 4-gram table to have "discount" column and leftover_probability.


# Supposed that we have data.table "threeGramTable" like this:
# 
#          firstTerms lastTerm frequency
#      1:         a_a        a        14
#      2:         a_a advisory         1
#      3:         a_a  airline         1
#      4:         a_a  alcohol         2
#      5:         a_a      all         1
#     ---                               
#8513726: zydeco_love     them         1
#8513727: zydeco_star       go         1
#8513728:   zydeco_we      got         1
#8513729:  zygote_but      how         1
#8513730:   zygote_id     only         1


##########################################################################################################
# Now we make extended 3-gram table with Discount column and Remaining Probabilities.
# Apply the formula:
# d = r* / r = ((r+1)/r)(n_(r+1)/n_r)
# For example, for frequency = 5, d = (6/5) * (N6/N5)
# N5: number of 3-grams that have frequency of 5.
# Supposed: in 3-gram, only these 3-grams appear 5 times:
#           A-B-C     5 times
#           A-B-E     5 times
#           X-Y-Z     5 times
#           L-M-N     5 times
# ==> we have N5 = 4
createThreeGramTableExtended = function(){
        # Supposed table "threeGramTable" as above, we want to add a "discount" column.
        threeGramTable$discount = rep(1, nrow(threeGramTable))
        
        # Calculate the discount coefficient.
        # We only consider n-grams that have 0 < frequency <= k (5). Larger than 5: "Reliable enough".
        for(i in 5:1){
            currRTimes = i
            nextRTimes = currRTimes + 1
            
            currN = nrow(threeGramTable[frequency == currRTimes])
            nextN = nrow(threeGramTable[frequency == nextRTimes])
            
            currd = (nextRTimes / currRTimes) * (nextN / currN) # assumption: 0 < d < 1
            
            # the beauty of "data.table"!
            threeGramTable[frequency == currRTimes, discount := currd]
        }
        
        # Calculate the remaining probability (thanks to discounting...).
        threeGramTable_leftOverProb = threeGramTable[, .(leftoverprob=calcLeftOverProb(lastTerm, frequency, discount)), by=firstTerms]
        
        # We now have two important objects: threeGramTable, threeGramTable_leftOverProb
        # ...
}

##########################################################################################################
# Calculate the remaining probability (thanks to discounting...).
# Given that the input is grouped based on firstTerms already.
calcLeftOverProb = function(lastTerm, frequency, discount){
    all_freq = sum(frequency)
    
    return(1-sum((discount*frequency)/all_freq))
}

