# Tree Canopy Report

**By Paul Spasojevic for YIMBY Melbourne Inc.**

# Setup

**Note: working file by @jonobri**

1. Clone this repo (e.g. via `gh repo clone yimbymelbourne/tree_data`)

> _TODO: clone finished repo into yimbymelbourne org_

> **1.a. Temporary step:** Add various `.Rdata` files to `r_objects/` from @Synfax
>
> _TODO: these should be stored on Github LFS, or in a publicly accessible equivalent_
>
> Files include `mesh_blocks.Rdata`, `agg_df.Rdata`, etc.

2. Open the repo in your IDE (e.g. RStudio, VSCode).

3. Ensure you have [quarto](https://quarto.org/) installed on your machine (e.g. within zsh/bash terminal via `brew install quarto`).

4. You will also need to install `tinytex` to render PDFs: `quarto install tinytex`.

5. Within an R terminal, install the required R dependencies by running `source('markdown/install_dependencies.R')`.

6. Within zsh/bash, render the report with the following command: `quarto render markdown`

This should successfully render the page and associated files for full viewing of the report in your browser, as well as an associated PDF of the report.

Now that the project is set up, you will be able to generate your own report in the YIMBY Melbourne theme!
