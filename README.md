# Armenian Survey Sample
The Armenian survey data from 2011 is available on Armstat webiste but it is in CSPro format. 
This is a code that transforms it into an R compatible dataset.

## Data and Code Structure 

Data for both datasets is available at : 

The code is structured in two files - one for each dataset. Household Data Conversion convert household data into R format. 
It is much smaller in size (around 76000 households) and has fewer variables. Individual Data Conversion converts individual 
dataset into R format. It is much bigger (10% of the Population - approximately 300k people). However, the automatic code generates 
a lot of empty variables as CSPro automaticall assumes household size that is much larger than any household in this dataset. 
For this reason Data Cleaning for Individual Dataset cleans this. In addition, it also creates a couple of tables on some demographic 
characteristics. 

Marz_kod is a dataset which shows how marz (region) numbers were replaced with regional names. 


