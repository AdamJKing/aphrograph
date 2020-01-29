# Aphrograph

## TODO List

* Add error reason to "failure state"
* Investigate weird label cropping when using day labels (-12d in from section)
* Make application logger part of the app state
* Create version of logger that takes buildable fmt data rather than strings (make it lazy)
* Come up with better equality instance for app state than two failed states not being equal
* Graphite being unavailable shows the error message "thread blocked indefinitely in an STM transaction" on exit
* Add view for app error state 
* Make Args part of the app state
* Remove ExceptT from app monad
* Make JSON parsing test (GraphiteSpec) property based
* Use relude map/functions in graph
* Move graph resizing into Graph module (Graph Ops module?)
* Http config in graphite config?
* Combine `failed` and `FailedState` ?
* Better name for `AppGraphiteError` 
* Split CommonProperties along TestM/utils
* Create proper liftGen for TestM
* Remove tests with multiple assertions, not supported by quickcheck monadic

