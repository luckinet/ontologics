# ontologics 0.5.2

- fix documentation to make it fit for CRAN.

# ontologics 0.5.1

- describe an ontology either by version, or by date
- revision of vignettes

# ontologics 0.5.0

- include functions `export_as_rdf()` for being able to explort any ontology in other, semantic-web based, formats.

# ontologics 0.4.2

- various fixes of convenience and to make ontologics work smoothly together with `arealDB`

# ontologics 0.4.1

- allow it that also classes are handled with the `new_mapping()` function, which also means that `new_class()` can now only set harmonised classes.

# ontologics 0.4.0

- adapt the onto-class structure according to issue https://github.com/luckinet/ontologics/issues/6; this entails that concepts and classes are summarised into their respective slot, with sub-tables "harmonised" and "external", featuring specific columns.
- change all functions to support this new structure
- this changes the behaviour of the function `new_concept()` insofar that this only creates harmonised concepts (so no source needs to be provided here anymore) and of the function `new_mapping()` insofar that this now creates external concepts and their mapping to harmonised concepts (and thus writes into the sub-table "external" and into the "mapping-columns" of the sub-table "harmonised")

# ontologics 0.3.2

- adapt column names according to issue https://github.com/luckinet/ontologics/issues/5
- revise how to extract concepts via `get_concept()`. Now you have to provide a table that contains the column name on which to subset and the values in that column that should be filtered from the ontology.

# ontologics 0.3.1

- adapt `new_concept()` so that also concepts can be defined that have no class (for various reasons). This now gives a warning, and inserts the concepts with an "undefined" class.

# ontologics 0.3.0

* revise all functions that access an ontology to work either in the `.GlobalEnv`, or at the given path. The argument `path = ` was therefore changed to `ontology = `.
* Instead of taking merely the terms, functions `new_concept()` and `new_mapping()` now take the output of `get_concept()` of already existing concepts, or at least a table with columns 'code', 'label_en' and 'class'.

# ontologics 0.2.0

* revise `get_concept()` so that it's faster.

# ontologics 0.1.0

* Initial commit
