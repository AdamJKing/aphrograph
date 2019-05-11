# Aphrograph

## TODO List

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
* Handle potential empty list in graphite response
* improve graphite tests
* investigate replacing 'Hourglass' with 'time'
* remove unsafe exceptions from bounds calls in graph
* Replace 'time' labels with localised time
* Remove temporary Testable Maybe typeclass
* Expand scale to Seconds and Milliseconds
* Remove magic numbers (ie, 86400, 3600, etc)

** user config dependant **
choose between numerical and word representation for days