# Codespaces student sandbox

This repository includes a GitHub Codespaces configuration for students who
need a browser-based R environment. It does not require Posit Cloud or a paid
RStudio account.

## Start a Codespace

1. Open the GitHub Classroom assignment repository.
2. Select **Code** > **Codespaces** > **Create codespace on main**. If your
   teacher gives you a different branch, choose that branch instead.
3. Wait for the setup step to finish. The first build installs R packages and
   can take several minutes.
4. Open a terminal in VS Code and start R:

   ```sh
   R
   ```

5. Check the workshop installation:

   ```r
   library(causalworkshop)
   check_workshop_prerequisites()
   ```

## Start the report template

Create a fresh report folder:

```r
causalworkshop::get_report_template("research-report-template")
```

Open `research-report-template/setup.R`, choose the exposure, then render:

```sh
cd research-report-template
quarto render manuscript.qmd
```

The rendered files will appear in `research-report-template/_output/`. Use the
default Codespaces machine unless your teacher tells you otherwise.

## Editor choices

The default Codespaces editor is VS Code in the browser. That is the supported
student path for this sandbox.

The same container can also be reached with SSH for local editors such as Zed:

```sh
gh codespace ssh
```

Zed users should treat this as an optional advanced path, because it requires a
local editor, GitHub CLI authentication, and SSH setup.
