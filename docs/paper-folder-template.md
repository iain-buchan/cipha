# Adding the code for a paper

Create `papers/<year>-<short-topic>/` and give it a `README.md` on this pattern. Keep scripts as they were run; if something has to change (for example removing local file paths), say so in the README and in `ATTRIBUTION.md`.

```markdown
# <Title of the paper or output>

**Paper.** <Authors>. <Title>. *<Journal>* <year>;<volume>:<pages>. https://doi.org/<DOI> (preprint: <link>)

**What it does.** Two or three sentences: the question, the data, the design and the main methods.

**Source.** Where the code came from (repository and commit, or "written for this repository"), who wrote it, its licence, and any changes made here.

## Scripts

| Script | Purpose |
|---|---|
| `01_clean.R` | ... |
| `02_models.R` | ... |

Run order, and which scripts call which.

## Data

What each script expects as input, where it can be obtained (CIPHA / Data into Action request, NHS Digital DARS agreement, public sources), and what cannot be shared.

## R packages

Alphabetical list of packages the scripts load (`grep -h -o -E "library\([A-Za-z0-9.]+" *.R | sort -u`), with the R version used if known.
```

Checklist before committing:

- no record-level data, credentials, API keys or personal file paths in the scripts (search for `Users/`, `OneDrive`, `/Volumes/`, `@`, `key`, `token`)
- original licence file kept alongside imported code; licence confirmed with the authors if the source had none
- row added to `ATTRIBUTION.md`
- status updated in `docs/publications.md`
