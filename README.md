This is a minimal example of a book based on R Markdown and the `R` package **`bookdown`**, geared specifically for students in ESCI 222 (Sewanee).  

Ezell will explain what bookdown is and how it works in class. The goal now is to setup the software on your machine and make sure it works.  

&nbsp;  

### Test that `bookdown` works on your own computer

**1.** Copy the repo link with the green button above.   


**2.**  In Terminal, navigate to your `repos` folder, then use `git clone <paste url>` to clone this repo to your local machine. 

**3.**  Look inside the `bookdown_minimal` repo using your Explorer/Finder. Double-click on `bookdown-minimal.Rproj` to open this "project" in `RStudio`.  

**4.** If you need to, install the `bookdown` package.

```
install.packages("bookdown")
```

**5.** In the top right pane (the one that has "*Environment*" as one of the tabs), look to see if there is now a tab named "*Build*".  

**6.** On that "*Build*"" tab, click on the "*Build Book*" button, and wait to see if a new window pops up with your book.  (It should look like [this](https://ericmkeen.github.io/bookdown_minimal/).)  

*If so...*  

**7.** Now look in the bottom-right pane (the one that typically shows plots), under the "Files" tab. Click on the `index.Rmd` file. 

**8.** In the `YAML` at the top of `index.Rmd`, customize the title, author and description of your book.  Re-build to see if the changes were applied.  


*If that all worked, then...* 


&nbsp;  

### Setup your own `bookdown` project on your own computer

**9.** Go back to Github and create a new public repository, just as you have before.  Call it "data_story_3".

**10.** Clone it to to your local `repos` folder. (Make sure to navigate out of the `bookdown_minimal` folder in Terminal before using the `git clone` command!)  

**12.** Now copy and paste all the files from this `bookdown-minimal` repo into that new empty repo.  

**13.** Close the `RStudio` window of the `bookdown-minimal` project.
 
**14.** In your new repo, change the `.Rproj` filename from `bookdown-minimal` to your own preferred filename.  

**15.** Open up that `.Rproj` file and start developing your book! 


&nbsp;  


### How to develop a `bookdown` book:

*In `bookdown`, the basic rules are:*

*(a)* Each chapter should occur in its own `Rmd` file; 

*(b)* The top line in each `Rmd` file should be the chapter title (using a single "#" at the start of the line).  

*(c)* The chapters will be appear in the alphabetical order of their filenames, so include a number at the start of each `Rmd` filename to control their sequence. Rename files as needed to get the order you want. 

*(d)* As you work on each `Rmd` individually, you can render them using the "*Knit*" button, as you would any `Rmd` project, to polish how each individual chapter looks.  

*(e)* But when you are ready to build the entire book into a single product (i.e., a single website), you should click the "*Build Book*" button in the top right pane.  


&nbsp;  

### How to publish your `bookdown` book online:

Do this exactly as you would a regular `Rmd`-based repo that you have already done several times: 

- In *Terminal*, `add`-`commit`-`push` your changes to Github.  
- On *Github*, set up the Github Pages website for your project. (The rendered book is saved inside the `docs` folder of your repo, just as we have been doing for previous data stories.) 
- Put a link to your Github Pages site in your repo's README file.
- If you update that README on Github, make sure to `git pull` on your local machine so that the online and local versions of your repo are synced.  


&nbsp;  


For help using `bookdown`, you can find the bookdown book [here](https://yihui.org/bookdown/).

For help formatting `citations`, look [here](https://bookdown.org/fmcron/Rhodes-template/bibliographies.html).  

