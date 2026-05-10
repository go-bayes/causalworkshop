# Research-report template (PSYC 434, Option A)

This folder is the scaffold for the 2026 Option A research report.

## Start here

1. Open this folder in RStudio or another editor.
2. Open `setup.R` and choose one exposure:

   ```r
   name_exposure <- "religious_service"   # or "volunteer_work"
   ```

3. Leave `outcome_short_names` unchanged. The report must analyse all four wellbeing outcomes: purpose, belonging, self-esteem, and life satisfaction.
4. Render the report:

   ```sh
   quarto render manuscript.qmd
   ```

5. Open the PDF in `_output/` and check that it rendered.
6. Replace the placeholder text in `manuscript.qmd` with your own writing.
7. Render again before submitting.

## Files

| File | Role |
|---|---|
| `setup.R` | Study decisions, packages, helper functions, and the four-outcome analysis pipeline. |
| `manuscript.qmd` | The report text, tables, figures, appendix, and citations. |
| `_quarto.yml` | Quarto render settings. |
| `references.bib` | References used by the report. Add any extra sources here. |
| `apa.csl` | APA citation style. |
| `_output/` | Rendered PDF and HTML, created after rendering. |

## Submit

Submit the rendered PDF with the R code appendix through Nuku by the due date listed on the course Assessments page.

Do not submit the `.qmd`, `.R`, or `.bib` files unless asked.
