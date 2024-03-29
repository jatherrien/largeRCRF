# README

This R package is used to train random competing risks forests, ideally for large data.
It's based heavily off of [randomForestSRC](https://github.com/kogalur/randomForestSRC/), although there are some differences.

This package is not yet on CRAN, so in the meantime to install it use the `devtools` package and run the following command:
```
R> devtools::install_git("https://github.com/jatherrien/largeRCRF.git")
```

If you care about vignettes and have the packages available to build them you can include `build_vignettes = TRUE` as a parameter in the command above.

## System Requirements

You need:

* R version 3.4.0 or greater
* The `rJava` package version 0.9-9 or greater
* A Java runtime version 1.8 or greater

## Troubleshooting

### I get an `OutOfMemoryException` error but I have plenty of RAM

`largeRCRF` makes use of the Java virtual machine, which unfortunately restricts itself by default to a quarter of your system memory. 
You can override the default by including **before** loading `largeRCRF` or any other `rJava` based package the following line:

```
R> options(java.parameters <- c("-Xmx13G", "-Xms13G"))
```

with `13G` replaced with a little less than your available system memory.

### I get an `OutOfMemoryException` error and I'm short on RAM

Obviously if you're short on RAM there is a limit on how large of a dataset you can train,
but there are some techniques you can use to limit how much `largeRCRF` needs.

* If your training dataset is large you might not want both R and `largeRCRF` to have their own separate copies 
(limitations due to Java require `largeRCRF` have its own copy). When specifying the `data` parameter into `train`, 
instead provide an environment containing one object called `data` which is the dataset. `largeRCRF` will delete that variable
after importing it into the Java environment.

Example:
```
R> data.env <- new.env()
R> data.env$data <- trainingData
R> rm(trainingData)
R> model <- train(..., data=data.env, ...)
```

* Each core that is training trees requires its own memory; you can try limiting `largeRCRF` to train only one tree at a time by specifiying `cores=1`.

* By default `largeRCRF` keeps the entire forest loaded in memory during training, 
when in practice only the trees being trained on need to be loaded. 
You can specify `savePath` to give a directory for `largeRCRF` to save trees in during training,
which will allow to `largeRCRF` to conserve memory for only those trees being currently trained.

### Training stalls immediately at 0 trees and the CPU is idle

This issue has been observed before on one particular system (and only on that system) but it's not clear what causes it. 
It would be appreciated if you could report this bug to [joelt@sfu.ca](mailto:joelt@sfu.ca) and give your operating system
and the version of Java installed (the entire output of `java --version`). 

As a workaround, this issue seems to occur randomly; so try restarting your code to see if it runs.


