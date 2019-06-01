# SOmap 0.2.1.9002

* Argument `mask` is now deprecated, it doesn't do anything (was causing crashes, so needs review). 

* Argument `buffer` removed from `SOauto_map()`, changed `expand` to numeric fraction (`expand = 0` equivalent to old `expand = FALSE`). 

* Argument `sample_type` to `SOauto_map()` moved to internal function `automap_nothing()`. 
 
* New internal functions to become the engine behind `SOauto_map()`,  `automap_maker()` to create a background extent from disparate inputs, and `automap_nothing()` to create a background by random data. 
 
* New auto extent logic for `SOauto_map()` to address #30. 

* New `reproj::reproj` methods for `SOauto_map` and `SOmap` classes. 

# SOmap 0.1.3.9000

* Added control to `SOauto_map` to draw sp lines as lines, points as points correctly. 

* Added a `NEWS.md` file to track changes to the package.
