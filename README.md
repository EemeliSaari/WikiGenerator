# WikiGeneratoR

Research project aimed to generate [Wikipedia articles](https://en.wikipedia.org/wiki/Wikipedia:What_is_an_article%3F) using deep learning using R. Project also takes a look at the available tools for deep learning in the R programming language.

Project is currently under development.

> Note that the project aims to generate articles in Finnish language.

### Project goals

1. Get the raw article data.
    * Using the [MediaWiki Action API](https://www.mediawiki.org/wiki/API:Main_page)
2. *(optional)* Parse the raw data.
    * No available tooling for R.
3. Research available deep learning tools.
    * [MXNet](https://mxnet.incubator.apache.org/api/r/index.html)
    * [TensorFlow API](https://tensorflow.rstudio.com/tensorflow/)
    * [Keras](https://tensorflow.rstudio.com/keras/)
4. Simple framework for experimentation.
    1. Creating datasets.
    2. Batch generators *(since large dataset)*.
    3. Training process.
5. Prototype with the models.
    * Different RNN approaches
    * Different features *(words, chars etc.)*
6. Generate articles.
7. *(optional)* Interface for generation.

> Note: Project doesn't aim to provide fully generalized solution

***

## Requirements

The R version used in the project has been **3.4.3**.

#### Libraries

- Keras
- Tensorflow

> Libraries might also have specific sub requirements.

***

## Usage

The project aims to provides tools for:

- Downloading articles
- Parsing articles
- Training models
- Generating text

using a simple commandline interface.

***

## TODO

- Examples of to the usage section
- Finish the interface tooling.
- Test MXNet framework.
- Finish the generation method.
- Document tested network solutions.
- Provide example results to README.
