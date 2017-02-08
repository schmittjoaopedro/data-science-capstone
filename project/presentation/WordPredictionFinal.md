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

1. Downloaded and sampled 1% of the dataset from [Swiftkey](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip).
2. To the training dataset, remove non english phrases, punctuation and numbers, strip the white spaces and pass content to lower case.
3. Generate a [tetra-gram](https://en.wikipedia.org/wiki/N-gram) of the phrases and their frequencies.
4. Generate a model combining the words of the tetra-grams.
5. Calculate the [Simple Good Turing (SGT)](http://www.grsampson.net/RGoodTur.html) probabilitie to the frequencie of frequencie of the words combinations, 
6. Executed a validation test to get the accuracy the algorithm using 0.01% of the words for each dataset.

Usage
========================================================

The webapp for word prediction is available at [WordPrediction App](https://schmittjoaopedro.shinyapps.io/WordPredicting/), this app receives any phrase (1) and calculates the probability for the next possible words (2).

![ShinyApp](TextMining.png)
