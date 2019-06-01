# README

This R package is used to train random competing risks forests, ideally for large data.
It's based heavily off of [randomForestSRC](https://github.com/kogalur/randomForestSRC/), although there are some differences which are described in (TODO - link to paper here).

This package is still in a pre-release state and so it not yet available on CRAN.
To install it now, in R install the `devtools` package and run the following command:
```
R> devtools::install_git("https://git.joeltherrien.ca/joel/largeRCRF.git")
```

## System Requirements

You need:

* R version 3.4.2 or greater
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
