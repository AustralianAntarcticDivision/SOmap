# dev

* New data set ACC_FRONTS.

# 0.5.1

* lots of bug fixes

* changes to internal SOmap object structures

# 0.4.0

* Allow `SOplot()` to add RGB raster objects. 

## BREAKING

* parameter names to many functions have been changed, to make them case- and style-consistent

* `SOauto_map` renamed to `SOmap_auto` for consistency

## CHANGES

* refactored ggplot code

* improved `SOmap_auto` functionality


# SOmap 0.2.1.9005

## BREAKING

* `SOauto_map()` function and class is now defunct, replaced by `SOmap_auto()`.

## CHANGES

* Arguments `mask` and `trim_background`  now removed from `SOmap_auto()`.

* Argument `buffer` removed from `SOmap_auto()`, changed `expand` to numeric fraction (`expand = 0` equivalent to old `expand = FALSE`).

* Argument `sample_type` to `SOmap_auto()` moved to internal function `automap_nothing()`.

* New internal functions to become the engine behind `SOmap_auto()`,  `automap_maker()` to create a background extent from disparate inputs, and `automap_nothing()` to create a background by random data.

* New auto extent logic for `SOmap_auto()` to address #30.

* New `reproj::reproj` methods for `SOmap_auto` and `SOmap` classes.

# SOmap 0.1.3.9000

* Added control to `SOauto_map` to draw sp lines as lines, points as points correctly. 

* Added a `NEWS.md` file to track changes to the package.
