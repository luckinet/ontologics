# ontologics 0.6.4

- implement better filtering on parent concepts to simplify editing matches.

# ontologics 0.6.3

- include matches with 0 difference already as close match, for convenience.

# ontologics 0.6.2

- fix a little bug located in `new_mapping()`.

# ontologics 0.6.1

- when matching a new dataseries with old concepts, give a message and ignore non-close matches.

# ontologics 0.6.0 - fuzzy matching

- fuzzy matching implemented. When concepts that are new for mapping do not match exactly, fuzzy matching is carried out and three new columns are offered for assigning the new concepts to harmonised concepts

# ontologics 0.5.6

- small fix that would not allow to match with an empty set of values.

# ontologics 0.5.5

- introduce function where a table with columns per classes in the ontology can be used to retrieve concepts. This table is matched with a flattened ontology. Currently, no mappings are attached to this table.

# ontologics 0.5.4

- reconstruct regex functionality and enable extracting of external concepts in `get_concept()`

# ontologics 0.5.3

- reconstruct extracting mappings in `get_concept()`

# ontologics 0.5.2

- write the getters `get_class()` and `get_source()`
- reconstruct sorting in `get_concept()` so that it corresponds to the order of the input table

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
