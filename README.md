# Katz BackOff Model Implementation In R
Katz's Backoff Model implementation in R

Katz's BackOff Model is useful in N-gram language modeling to estimate the conditional probability of a word, given its history (actually, its preceding words, normally 2-3-4 words). The problem is that the corpus for training must be large to cover as much the diversity of language as possible. Nevertheless, there are cases where "large" is not "large enough". Katz's approach is to fall back to lower-order N-gram in this case.

However, one cannot just fall back like this because this naive approach is unfair. Let's say: A-B-C appears 15 times, while A-B-? totally appear 100 times. As a result, Probability(A-B-C|A-B) = 15%. But A-B-N does not appear, so we fall back to B-N and similarly, find that Probability(B-N|B) = 40%. It is unfair because "A-B" gives more context than just "B", but it is NOT chosen!

Katz fixes this issue by redistributing some probability of high-order N-gram to lower-order N-gram, so that all of the probabilities accumulate to 1. But first we have to reap some probability of the high-order N-gram, making it available to lower-order N-gram. It is done by using Good-Turing Discounting.

After having some left-over probability for lower-order N-gram, we distribute it fairly. That is, in the lower-order N-gram, the N-grams who have more probability to appear will have more share in this left-over probability.

The Katz's approach makes sense.

## Test Environment:

R version 3.2.4 (2016-03-10)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 8.1 x64 (build 9600)


## Reference:
1. [Katz's back-off model](https://en.wikipedia.org/wiki/Katz%27s_back-off_model)

2. [Estimation of Probabilities from Sparse Data for the Language Model Component of a Speech Recognizer](http://l2r.cs.uiuc.edu/~danr/Teaching/CS546-09/Papers/Katz87.pdf)
