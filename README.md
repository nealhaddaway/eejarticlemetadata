# eejarticlemetadata

This repository contains scraped meta-data from all eej articles based on a suite of PDF and HTML scraping functions that extract information from the publisher URL and doi.

The list of extracted meta-data/data are as follows:  
- authors  
- corresponding author email address(es)  
- year  
- title  
- journal  
- volume  
- pages  
- doi  
- abstract  
- parsed full text taken from the HTML available at the doi target as a .txt file  
- PDF file  
- parsed full text taken from the PDF as a .txt file  

Additional meta-data/data on the to-do list include:  
- keywords  
- background section text  
- methods section text  
- results section text  
- discussion section text  
- declarations  
- parsed references as an .ris file  

Functions have been built for parsing each of the above items and can be adapted to other sources as needed. No guarantee is provided that they will work outside of the journal Environmental Evidence, published by BioMed Central.

Articles were extracted most recently in June 2020.
