# Aphrograph

## TODO List

* Don't use 'Past' in Graphite API, prefer `TimeSpan` type
* Make Graphite URL configurable
* improve graphite tests
* explore ways to combine the two "Graph.bounds" tests
* Enable units in days
* Enable intermediary units for two discrete labels (discrete -> continuous)
* Don't kill program when we can't reach graphite
* Moved scaled to it's own package
* Cleanup LabelSpec (long list of labels, generative testing?)
* log error for datapoint parsing
* move Scaled and normalisation to projection "Internal"
* Improve test coverage
* Add logging to widget construction (debug label & `unsafePerformIO`?)
* Handle NoData version of `generateLabels`
* Dynamically add version information from stack build
* Include links to Graphite API in the argument documentation
* Make 'to' argument optional at command-line
* Handle Graphite errors (5**) more gracefully