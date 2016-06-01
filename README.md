# swirl-tbp: template-based practice problems in *swirl*

The *swirl-tbp* extension introduces *template-based practice* problems to the *swirl* framework. Specifically, *swirl-tbp* extends *swirl* by allowing instructors to include template-based problems in *swirl* lessons. Template-based problems are problems that include numbers, variable names, or other features that are randomly generated at run-time. As a result, a user can be provided with an endless supply of practice problems that differ, e.g., with respect to the numbers used. This allows users to repeatedly practice problems in order to reinforce concepts and practice their problem-solving skills. 

## Instructions
1. Install the *swirl* package from http://swirlstats.com
2. Download the *swirl-tbp* files, either by cloning the repository from github, or by downloading and extracting the zip file https://github.com/gdancik/swirl-tbp/archive/master.zip
3.  Source the *R* files, for example by typing the following from within *R*, where '/path' is the path to the *swirl-tbp* files:

	`setwd("/path")`
	`source("swirl-tbp.R")`
 
4. The  *swirl* package has now been extended to allow for template-based practice problems. To begin *swirl*, type the following:

	`library(swirl)`
5. For students, lessons are run in the same fashion as standard *swirl* lessons. However, at any time the student can type *rpt()* to repeat the previous question (with different dynamic values), if desired. 
 
## Creating lessons  
Lessons with template-based practice problems are created in the same way as regular *swirl* lessons, but *tokens* are used to store dynamic *R* objects and values, and questions can be repeated multiple times. This is best illustrated by looking at an example block from the YAML file: 

```
- Class: cmd_question
  NumTimes: 2
  Token: |
    num1 = sample(1:10)
    num2 = sample(11:20)
  Output: Create a vector named 'x' that holds the values <num1> and <num2>.
  CorrectAnswer: x <- c(<num1>,<num2>)
  AnswerTests: omnitest(correctExpr='x <- c(<num1>,<num2>)')
```

The main addition is that a 'Token' line is specified which uses *R* code to dynamically generate tokens (values). Elsewhere in the YAML file, these tokens are surrounded by angle brackets (e.g., `<num1>`) and will be replaced by their values when the lesson is run. 

For example, the above segment generates a question of the form 'Create a vector named *x* that holds the values *num1* and *num2*', where *num1* will be a random value between 1 and 10 and num2 a random value between 11 and 20. 

Note that in the 'Token' line above, the vertical line (|) is necessary to indicate that the *R* code spans multiple lines. Each line must begin with a number of blank spaces, as tabs are not allowed in YAML files. Alternatively, the *R* code can be specified on a single line with statements separated by semi-colons. 

A developer can also provide a 'NumTimes' value. When a lesson is run, the question will be repeated (using dynamically generated values) the specified number of times (if not specified, 'NumTimes' defaults to 1).

An example lesson.yaml file is provided in the  'Template_Based_Practice_Problems' directory.

## Contributors:
- Main contributor: Garrett Dancik
- Additional contributors: Kyle Marrotte and Ryan Shooner

## Acknowledgements
KM and RS contributed as part of an independent study in Computer Science at Eastern Connecticut State University, Willimantic, CT,  USA.

Note from Kyle on 12/6/2015:  This repo is now as finished as it is going to be.  It's going out of my hands and into the hands of Dr. Dancik, and his new independent study students next semester.  Good luck, guys!

