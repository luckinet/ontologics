# Database

The Database of the package is stored in an list that holds the five tables `sources`, `classes$internal`, `classes$external`, `concepts$internal` and `concepts$external`. The name of the list is defined when loading or starting a new ontology (see: [create_an_ontology](create_an_ontology.html): `crops <- load_ontology()` or `lulc <- start_ontology()`). The tables are accessed with `@`, e.g., `crops@sources` or `crops@concepts$harmonised`.

## Sources

Each row in the sources table represents a source of one or more concepts and/or classes. A source is typically some form of a knowledge organization system (KOS); e.g., a vocabulary. A source record features the six properties:

__id__ (*mandatory*): Unique identifier of this source record. Built by counting upwards, starting with `1`.

__label__ (*mandatory*): A speaking label for the source.

__description__ (*optional*): A description of the source.

__homepage__ (*mandatory*): Homepage of the source. Ideally, this is some Website that hosts the terms that are described in the source-KOS. It can also be a link to a PDF or similar. It is also possible to enter the ISBN (or another identifier) of a book or an other real world document. If everything is correct, the values that are inserted in the homepage should also be unique, i.e. be primary keys.

__license__ (*mandatory*): Licensing information of the source-KOS.

__notes__ (*optional*): Further notes about the source.

The first row of the sources table refers the ontology itself as a source. Every harmonised concept and class is considered to belong to this source.
