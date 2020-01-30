*Boxiang Tang*

### Overall Grade: 100/110

### Quality of report: 10/10

-   Is the homework submitted (git tag time) before deadline? 

    Yes. `Jan 24, 2020, 7:49 PM PST`.

-   Is the final report in a human readable format html? 

    Yes. `html` file. 

-   Is the report prepared as a dynamic document (R markdown) for better reproducibility?

    Yes. `Rmd`.

-   Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how are results produced by just reading the report? 

    Yes. 
	  


### Correctness and efficiency of solution: 56/60

-   Q1 (10/10)

-   Q2 (19/20)

	\#4. (-1 pt) `uniq` compares adjacent lines, hence you need to `sort` before `uniq`. Try:
	  
	  ```
	  grep HISPANIC $datafile | awk -F',' '{ print $2 }'| sort | uniq | awk 'END {print NR}'
	  ```
	
	
-   Q3 (14/15)
    
    \#3. (-1 pt) Why do we need the first line of the shell script (`#!/bin/sh`)?

-  Q4 (13/15) 

    \#3. (-2 pts) Table looks crude. Use `kable` to print the table in the given format.
	
	    
### Usage of Git: 8/10

-   Are branches (`master` and `develop`) correctly set up? Is the hw submission put into the `master` branch?

    Yes.

-   Are there enough commits? Are commit messages clear? (-2 pts)

    9 commits. **Make sure** to start version control from the very beginning of a project. Make as many commits as possible during the process.

          
-   Is the hw1 submission tagged? 

    Yes. `hw1`. 

-   Are the folders (`hw1`, `hw2`, ...) created correctly? 

    Yes.
  
-   Do not put a lot auxiliary files into version control. 

	  Yes.  

### Reproducibility: 10/10

-   Are the materials (files and instructions) submitted to the `master` branch sufficient for reproducing all the results? Just click the `knit` button will produce the final `html` on teaching server? 

	  Yes.  
	
-   If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results?

    Yes.

### R code style: 16/20

-   [Rule 3.](https://google.github.io/styleguide/Rguide.xml#linelength) The maximum line length is 80 characters. (-2 pts)

     Some violations:
	
	- `ReadNTable.R`: lines 21, 26, 27
	- `autoSim.R`: line 12

-   [Rule 4.](https://google.github.io/styleguide/Rguide.xml#indentation) When indenting your code, use two spaces.

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Place spaces around all binary operators (=, +, -, &lt;-, etc.). 	

	
-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Do not place a space before a comma, but always place one after a comma. (-2 pts)

     Some violations:
	
	- `ReadNTable.R`: lines 21, 26, 27
	- `autoSim.R`: line 11

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Place a space before left parenthesis, except in a function call.

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Do not place spaces around code in parentheses or square brackets.
