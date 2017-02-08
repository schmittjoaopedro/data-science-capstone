Next Word Prediction
========================================================
author: João Pedro Schmitt
date: 07, Feb 2017
autosize: true

Project presented to the data science capstone project for the [Data Science Specialization](https://www.coursera.org/specializations/jhu-data-science) from Coursera.

Introduction
========================================================

Around the world, people are spending an increasing amount of time on their mobile devices for email, social networking, banking and the whole range of other activities. But typing on mobile devices can be a serious pain. This project builds a smart algorithm that can suggest for people the next word of a phrase, for example if someone type: *I went to the* the keyboard presents three options for what the next word might be. For example, the three words might be *gym*, *store*, *restaurant*.

**Goal:** Using the dataset provided by SwiftKey, build an app that predict the next word of a phrase. 


Algorithm
========================================================

The algorithm is composed in 6 parts:

1. Download and sample 1% of the dataset from [Swiftkey](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip).
2. Remove non english phrases, punctuation and numbers, strip the white spaces and pass content to lower case.
3. Generate a [tetra-gram](https://en.wikipedia.org/wiki/N-gram) of the phrases and their frequencies.
4. Generate a model combining the words of the tetra-grams.
5. Calculate the Simple Good Turing (SGT) probabilities to the frequency of frequency of the words combinations. 
6. Execute a validation test to get the accuracy the algorithm using 0.01% of the words for each dataset.

Usage
========================================================

The webapp for word prediction is available at [WordPrediction App](https://schmittjoaopedro.shinyapps.io/WordPredicting/), this app receives any phrase (1) and calculates the probability for the next possible words (2). To the sentence entered the last three words are used to predict the next word, for example, if the sentence is "i like to buy a" the words "to buy a" will be used to predict next words.
***
![ShinyApp](TextMining.png)


Conclusions
========================================================

The predictions from the app are very concise for very common cases, but in the specific cases the words found would not be the right choices. A very common example: if I writte "my name" the next word predicted will be "is" that is correct in the most case, but if I write "today i will buy a" the word "book" could not be my next choice.

The algorithm available at [WordPrediction Code](https://github.com/schmittjoaopedro/data-science-capstone/) shows very organised and with the use of the standard libraries, was did a huge research in a various articles, books and examples to found a good mixed of strategies that could be used to build a good algorithm, the accuracy achieved was a mean of 20% in twitter, news and blogs.
