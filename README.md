# JSC370 Final Project
## Important note: I found my GitHub cannot open the pdf and this maybe because the file is large, if you are facing the same issue, please download the file (this may take few minutes) to view the pdf, thanks for understanding!

## Title
**How do socioeconomic and demographic factors influence the prevalence of different types of disabilities across U.S. states?**  
*by Jingwen Zhong*

## Description
This project investigates how various socioeconomic indicators and demographic factors impact disability prevalence across U.S. states. Using data from the CDC, U.S. Census Bureau, and the Kaiser Family Foundation, the analysis explores relationships between disability rates and variables such as poverty, insurance coverage, hospital infrastructure, and Medicaid/Medicare spending.

Key methods include:
- Choropleth mapping
- Correlation analysis
- ANOVA and linear regression modeling
- Clustering analysis using HDBSCAN
- Interactive dashboards (via `flexdashboard` and `plotly`)

## Deliverables
- 📄 **Final Report**: `JSC370_Project.pdf` (contains Introduction, Methods, Results, Conclusion)
- 📊 **Interactive Visualization Dashboard**: `vis.html`
- 📂 **Appendix**: `appendix.html` (summary tables of all U.S. states)

## Repository Contents
- `JSC370_Project.Rmd`: Source code for the main report
- `vis.Rmd` & `vis_code.R`: Code for the interactive dashboard
- `appendix.Rmd`: Source for the full table appendix
- `data/`: Cleaned datasets used in analysis
- `_site.yml`: Site configuration
- `README.md`: Project overview

## How to Run Locally
1. Clone or download this repository
2. Ensure you have all required R packages installed (see setup chunk in `.Rmd` files)
3. Open the R project (`.Rproj`) file in RStudio
4. Run `rmarkdown::render_site()` to generate the site locally

## Contact
For questions or feedback, feel free to contact: **lisazjw.zhong@mail.utoronto.ca**
