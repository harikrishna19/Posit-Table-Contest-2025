# Posit-Table-Contest-2025

# Premier League Transfers Dashboard (2025/26)

[Click to view the project](https://rpubs.com/hari_k/PL_Transfers)

A dynamic **Reactable** table in RMarkdown showcasing all Premier League transfers for the 2025/26 season. This project provides interactive tables with detailed insights into player movements, transfer fees, and club spending.

------------------------------------------------------------------------

## Project Overview

This project leverages **R**, **RMarkdown**, and **reactable** to create an interactive table displaying Premier League transfer data. Users can explore:

-   Player names, positions, and clubs.
-   Transfer fees (incoming and outgoing).
-   Detailed financial metrics including overpaid percentage and ROI.

The dashboard is fully reproducible and styled with custom themes, logos, and CSS enhancements for a polished presentation.

------------------------------------------------------------------------

## Steps to Reproduce the Table

1.  **Clone the Repository**

    ```         
    git clone https://github.com/harikrishna19/Posit-Table-Contest-2025.git
    ```

2.  **Open `main.Rmd` in RStudio**\

    This file contains the code to read the transfer CSVs, clean the data, and render the Reactable table.

    **Ensure Data Files Are Present**\

    Place `pl_incomings.csv` and `pl_outgoings.csv` in the `data/` folder

3.  **Install Required Packages**\

    Make sure all R packages listed in the prerequisites are installed.

4.  **Render the table**

    -   Click **Knit** in RStudio to generate the HTML output

        Alternatively, run in R console:

        ```         
        rmarkdown::render("main.Rmd", output_format = "prettydoc::html_pretty")
        ```

## Contact

For questions, suggestions, or feedback, reach out to:

**Hari Krishna**\
GitHub: <https://github.com/harikrishna19>
