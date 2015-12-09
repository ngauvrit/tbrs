# TBRS version 0.1
# Gauvrit, N. and Mathy, F.

December 4th, 2015


Table of content:
# What is the tbrs R-function?
# How to import the tbrs function into R?
# Parameters
# Example
# References



# What is the tbrs R-function?
The tbrs function defined below is an R-script implementing the TBRS2 model of working memory. It allows you to predict the probability of recall of items in a complex span task. This function is a companion script to the submitted corresponding paper “TBRS2. A mathematical transcription of the TBRS theory of working memory.”

The tbrs function computes activation (odd of correct recall) by tenth of second.

# How to import the tbrs function into R?
The easiest way to use the function is to download or copy-paste the code from the 'tbrsRscript.R' file. Once this is done, just run the script (feel free to tweak the function as you see fit). You can now use the function tbrs().

# Parameters
Parameters of the tbrs functions are as follow:

-- taskString (required) is a character string describing the memory task you are interested in. In this string, each symbol correspond to a one-second interval. “L” means that a letter (to-be-remembered item) is presented. “0” means free time. “1” means that a dual task is being performed. For instance, say a letter like “K” is presented one seconde, then there is a free second, then letter “M” is presented, then 5 seconds are devoted to dual tasks, and 1 second free before recall. You would code that as “L0L11110”.

-- startType is either “first”, “next” or “lowest”. See our paper for more information.

-- type is either “steady” or “threshold”. See our paper for more information.

-- baseline is the log of activation during presentation. For instance, if you type “baseline=2”, the activation is set to exp(2) when an item is presented.

-- duration is either the duration of refreshment of a particular item in the “steady” variant, or the log of activation that has to be reached before switching in the “threshold” variant. NOTE: duration is expressed in tenth of seconds and should be an integer.

-- d and r are the decay and refreshment rate. They are expressed in points of log-activation per second. For instance, if you set d = 1, the log of activation loses one point in a second, therefore the activation is divided by exp(1) (2.71), meaning that the odd of correct recall is divided by 2.71 every second.

# Example
To use the function, just type “tbrs(…)”

tbrs("L01L10L00000",type="steady",startType="first",d=1,r=3,duration=3,baseline=0)

>> [1] 0.9308616 0.5986877 0.7310586

The function traces the corresponding plot, and returns the list of estimated probability of recall at the end of the task.

# References
The TBRS model of working memory was developped (but not formalized) in:

Pierre Barrouillet, Sophie Bernardin and Valéerie Camos. Time constraints and resource sharing in adults’ working memory spans. Journal of Experi- mental Psychology: General, 133(1):83, 2004.

The present implentation is described in:

Nicolas Gauvrit and Fabien Mathy.TBRS2: a mathematical transcription of the Time-Based Resource Sharing theory of working memory. Submitted Manuscript.
