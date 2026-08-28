# PREDICTS Integration

Sequential scripts for downloading and preparing PREDICTS source inputs.

Run from the `code` folder:

```sh
cd predicts_download_integration/code
Rscript 02_get_predicts.R
```

Script sequence:

- `01_config.R`: shared paths and source identifiers.
- `02_get_predicts.R`: downloads/caches PREDICTS 2016 + 2022 database,
  column descriptions, site summaries, final combined reference table, and
  filtered readable previews.
