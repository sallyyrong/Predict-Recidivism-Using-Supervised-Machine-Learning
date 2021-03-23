<h1> Training Supervised Machine Learning Models to Predict Recidivism
  </h1>
<h2>
  Introduction
 </h2>
 <body>
  <p>
  In this project, we use supervised learning methods to train and evaluate three different predictive models (logistic regression, Ridge regression, and boosted trees) to predict whether or not a criminal defendant will recidivate, utilizing data in the recidivism_data_sample.csv dataset. From the three models we train and evaluate, we proceed to select a final model to best deploy in the real world.
  </p>
<h2>
  Description of Files
  </h2>
<h3>
  <strong>
  write-up.pdf
  </strong>
  </h3> 
  <p>
    This is the write-up that communicates the results of our project. It including visual outputs of our written code in R, and data visualizations of the three models employed, in addition to tables and graphs.

<h3>
  <strong>
  code.rmd
  </strong>
  </h3> 
  <p>
    This R Markdown Notebook file includes code written for the three different supervised machine learning models that we deployed.
  </p>
  
  
<h3>
  <strong>
  recidivism_data_sample.csv
  </strong>
  </h3>
  <p>
    A historical data set of criminal defendants from Broward
County, Florida. Contains a sample of 6000 criminal defendants. This is our training data.
  </p>
 <h3>
  <strong>
  recidivism data pseudo new.csv
  </strong>
  </h3> 
    <p>
     A sample of 3000 pseudo (fake) held-out observations. We use our final model to make predictions for these pseudo observations. This pseudo set is meant to verify that our final model can be used to predict onto the test data (note that the test data was witheld by instructor, and is not uploaded in this repository). The pseudo dataset is not used at all in model training, nor to assess the models' predictive performance.
      </p>
  </body>
