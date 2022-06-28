# ontologics 0.3.1

- adapt `new_concept()` so that also concepts can be defined that have no class (for various reasons). This now gives a warning, and inserts the concepts with an "undefined" class.

# ontologics 0.3.0

* revise all functions that access an ontology to work either in the `.GlobalEnv`, or at the given path. The argument `path = ` was therefore changed to `ontology = `.
* Instead of taking merely the terms, functions `new_concept()` and `new_mapping()` now take the output of `get_concept()` of already existing concepts, or at least a table with columns 'code', 'label_en' and 'class'.

# ontologics 0.2.0

* revise `get_concept()` so that it's faster.

# ontologics 0.1.0

* Initial commit
