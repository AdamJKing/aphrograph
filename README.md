# Aphrograph

## TODO List

* Add value markers to the edges of the graph
* Add logging capabilities to Graphite interactions
* Used named arguments
* Don't use 'Past' in Graphite API, prefer `TimeSpan` type
* Make Graphite URL configurable
* improve graphite tests
* explore ways to combine the two "Graph.bounds" tests
* Enable units in days
* Enable intermediary units for two discrete labels (discrete -> continuous)
* Don't kill program when we can't reach graphite
* Moved scaled to it's own package
* Cleanup LabelSpec (long list of labels, generative testing?)

-- it is hardcoding 'headspace' for y

-- gets the min max, creates a delta?
-- widens if there is no delta??
    -- defines that min CANNOT equal max
    -- we 'scale' the axis values to make room (in Y)

-- uses SQRT to determine how many ticks you can get in there
-- divides change by number of ticks
-- dec = floor . log10 gives the number of decimal places to keep? or number of useful decimal places?
-- norm = delta / magn where magn is (10 ^ dec)
-- needed to set the decimal places somehow?
