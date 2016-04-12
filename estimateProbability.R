library(stringr)
library(data.table)


##########################################################################################################
# Important assumption: Here, we 'primarily' use 3-gram as the main modeling with support from 2-gram and 1-gram.
# Other possibilities: Use primarily 2-gram with support from 1-gram; Use primarily 4-gram with support from 3-gram, 2-gram and 1-gram.
# Reference: https://en.wikipedia.org/wiki/Katz%27s_back-off_model
# Notice the notations: Alpha, Beta, d, k.


##########################################################################################################
# These following are the 3-gram, 2-gram and 1-gram tables. The "discount" column is of Good-Turing Discounting.
# Katz suggested that in practice, frequencies being larger than 5 are "reliable enough", so their discount factors remain "1"; otherwise, they should be within (0,1).
# The "frequency" column is of "true frequency" in the corpus. For example, "a_a_a" appears 14 times in the corpus.
# The "discount" column is also known as "Discounting factor" or "Discounting coefficient".
#
# Supposed that we have data.table "threeGramTable" like this:
#
#          firstTerms lastTerm frequency  discount
#      1:         a_a        a        14 1.0000000
#      2:         a_a advisory         1 0.1805165
#      3:         a_a  airline         1 0.1805165
#      4:         a_a  alcohol         2 0.5212007
#      5:         a_a      all         1 0.1805165
#     ---                                         
#8513726: zydeco_love     them         1 0.1805165
#8513727: zydeco_star       go         1 0.1805165
#8513728:   zydeco_we      got         1 0.1805165
#8513729:  zygote_but      how         1 0.1805165
#8513730:   zygote_id     only         1 0.1805165
#
# Supposed that we have data.table "twoGramTable" like this:
#
#         firstTerms lastTerm frequency  discount
#      1:          a        a       463 1.0000000
#      2:          a       ab         1 0.3696323
#      3:          a    abbey         2 0.6420323
#      4:          a  ability         2 0.6420323
#      5:          a     able         2 0.6420323
#     ---                                        
#2609866:     zydeco     love         2 0.6420323
#2609867:     zydeco     star         1 0.3696323
#2609868:     zydeco       we         1 0.3696323
#2609869:     zygote      but         1 0.3696323
#2609870:     zygote       id         1 0.3696323
#
# Supposed that we have data.table "oneGramTable" like this:
#
#        lastTerm frequency  discount
#    1:         a    342631 1.0000000
#    2:  aardvark         7 1.0000000
#    3: aardvarks         1 1.0475666
#    4:        ab       193 1.0000000
#    5:     aback         3 1.0278129
#   ---                              
#48237:    zounds         2 0.9976876
#48238:  zucchini        41 1.0000000
#48239: zucchinis         1 1.0475666
#48240:    zydeco         5 0.9905074
#48241:    zygote         2 0.9976876
#
# Supposed that we have data.table "threeGramTable_leftOverProb" like this:
# Thanks to Discounting, we have some left-over probabilities reserved for "un-seen" words.
#
#          firstTerms leftoverprob
#      1:         a_a    0.5773009
#      2:        a_ab    0.8194835
#      3:     a_abbey    0.8194835
#      4:   a_ability    0.8194835
#      5:      a_able    0.8194835
#     ---                         
#2609866: zydeco_love    0.8194835
#2609867: zydeco_star    0.8194835
#2609868:   zydeco_we    0.8194835
#2609869:  zygote_but    0.8194835
#2609870:   zygote_id    0.8194835


##########################################################################################################
# This function is used to get the probability of a given text, using Katz Backoff (with Good-Turing Discounting).
# Only consider the last 3 words.
# Input1: I want to sleep
# Output1: 0.04536
# Input2: I want to go
# Output2: 0.4323
# Thus, input2 has more chance to appear than input1. Statistically, it is more relevant to people.
getProbabilityFrom3Gram = function(inputString){
    # Preprocessing
    mylist = separateTerms(getLastTerms(inputString, num = 3))
    inFirstTerms3gram = mylist$firstTerms
    inLastTerm3gram = mylist$lastTerm
    
    finalProb = -1
    
    oneGroupIn3Gram = threeGramTable[firstTerms == inFirstTerms3gram]
    if (nrow(oneGroupIn3Gram) > 0){
        # Algorithm here
        oneRecordIn3Gram = threeGramTable[firstTerms == inFirstTerms3gram & lastTerm == inLastTerm3gram]
        if (nrow(oneRecordIn3Gram) > 0){
            # We found one in 3-gram
            all_freq = sum(oneGroupIn3Gram$frequency)
            finalProb = ((oneRecordIn3Gram$discount * oneRecordIn3Gram$frequency) / all_freq)
            ### We're done!
        } else {
            # NOT found in 3-gram => check 2-gram & 1-gram
            mylist = separateTerms(getLastTerms(inputString, num = 2))
            inFirstTerms2gram = mylist$firstTerms
            inLastTerm2gram = mylist$lastTerm
            
            # Get the left-over probability so that we can distribute it for lower-order grams.
            beta_leftoverprob = threeGramTable_leftOverProb[firstTerms == inFirstTerms3gram]$leftoverprob
            
            oneGroupIn2Gram = twoGramTable[firstTerms == inFirstTerms2gram]
            oneRecordIn2Gram = twoGramTable[firstTerms == inFirstTerms2gram & lastTerm == inLastTerm2gram]
            if (nrow(oneRecordIn2Gram) > 0){
                # We found one in 2-gram!
                # We only consider ones that do not appear in 3-grams...
                oneGroupIn2Gram_Remain = oneGroupIn2Gram[!(oneGroupIn2Gram$lastTerm %in% oneGroupIn3Gram$lastTerm)]
                all_freq = sum(oneGroupIn2Gram$frequency)
                
                alpha = beta_leftoverprob / sum((oneGroupIn2Gram_Remain$frequency * oneGroupIn2Gram_Remain$discount) / all_freq)
                
                finalProb = alpha * ((oneRecordIn2Gram$frequency * oneRecordIn2Gram$discount ) / all_freq)
                ### We're done!
            } else {
                # We only have hope in 1-gram!
                oneGroupIn1Gram = oneGramTable # we don't have "firstTerms" here!
                oneRecordIn1Gram = oneGramTable[lastTerm == inLastTerm2gram] # what if this returns "zero" row?
                
                oneGroupIn1Gram_Remain = oneGroupIn1Gram[!(oneGroupIn1Gram$lastTerm %in% oneGroupIn3Gram$lastTerm)]
                all_freq = sum(oneGroupIn1Gram$frequency)
                
                alpha = beta_leftoverprob / sum((oneGroupIn1Gram_Remain$frequency * oneGroupIn1Gram_Remain$discount) / all_freq)
                
                finalProb = alpha * ((oneRecordIn1Gram$frequency * oneRecordIn1Gram$discount) / all_freq)
                ### We're done!
            }
        }
    } else {
        stop(sprintf("[%s] not found in the 3-gram model.", inFirstTerms3gram))
        # The workaround could be:
        # + Write another function in which we primarily use 2-gram with support from 1-gram.
        # + Increase the corpus size so that the 3-gram can capture more diversity of words...
    }
    
    finalProb
}


##########################################################################################################
# This function is used to extract terms.
# Input: "A_B_C"
#        "X_Y_Z"
# Output: firstTerms  lastTerm
#         "A_B"       "C"
#         "X_Y"       "Z"
separateTerms = function(x){
    # Pre-allocate
    firstTerms = character(length(x))
    lastTerm = character(length(x))
    
    for(i in 1:length(x)){
        posOfSpaces = gregexpr("_", x[i])[[1]]
        posOfLastSpace = posOfSpaces[length(posOfSpaces)]
        firstTerms[i] = substr(x[i], 1, posOfLastSpace-1)
        lastTerm[i] = substr(x[i], posOfLastSpace+1, nchar(x[i]))
    }
    
    list(firstTerms=firstTerms, lastTerm=lastTerm)
}


##########################################################################################################
# This function is used to get the last "num" terms of a given text.
# Input: We are students of the university
# Output: of_the_university (if num = 3)
getLastTerms = function(inputString, num = 3){
    # Preprocessing
    inputString = gsub("[[:space:]]+", " ", str_trim(tolower(inputString)))
    
    # Now, ready!
    words = unlist(strsplit(inputString, " "))
    
    if (length(words) < num){
        stop("Number of Last Terms: Insufficient!")
    }
    
    from = length(words)-num+1
    to = length(words)
    tempWords = words[from:to]
    
    paste(tempWords, collapse="_")
}

