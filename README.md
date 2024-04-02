# rshiny-stepped-wedge

Visualize a stepped wedge implementation trial

**Authors:**

-   [Samuel Jaros](https://profiles.stanford.edu/sam-jaros)
-   [Mia Navarro, MS](https://profiles.stanford.edu/mia-navarro)
-   [Wouter Vermeer, PhD](https://www.feinberg.northwestern.edu/faculty-profiles/az/profile.html?xid=39402)
-   [C Hendricks Brown, PhD](https://www.feinberg.northwestern.edu/faculty-profiles/az/profile.html?xid=27859)

**Aims:**

Produce an interactive web application which:

-   Encourages the appropriate use of a stepped-wedge trial design in research

-   Is simple to use while remaining customizable

-   Produces useful calculations and visualizations to use in the proposal and implementation of a trial

**Dependencies:**

This project requires `R` \>= 4.3, `shiny` \>= 1.3.3, and `renv` \>= 1.05. There are other dependencies tracked by `renv` that can be loaded using `renv::restore()`.

**Setup:**

Get the files from this repository by:

-   Run these lines of code in R:

    ```{r}
    install.packages("usethis")
    usethis::create_from_github("https://github.com/samjaros-stanford/rshiny-stepped-wedge.git")
    renv::restore()
    ```

-   **OR** Create a new project in RStudio from git version control. Use this link as the repository: "<https://github.com/samjaros-stanford/rshiny-stepped-wedge.git>".

-   **OR** Use the green "Code" button above and click "Download ZIP", unpack the files, and move them into your working directory.

Use `renv::restore()` to get the required packages.
