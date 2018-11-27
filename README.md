# Data-Mining
Linear Regression, Regularization and Model Comparison; Linear Classification Methods and Discriminant Analysis; CART, random forest, SVM; Cluster Analysis

##############################################################
For Linear Regression, Regularization and Model Comparison, I use the diabetes data in Efron et al. (2003). The data consists ten baseline
variables which are age, sex, body mass index, average blood pressure, and six blood serum
measurements as well as the response of interest, a quantitative measure of disease
progression one year after baseline. The ten baseline variables were obtained for each of n=442
diabetes patients. The data is available in the “lars” R package.

The aim of this project is to compare linear regression, ridge regression and lasso regression
models through mean squared errors (MSE).

To avoid overfitting problem, I first randomly split the data into training data and test data. In
our case, the test dataset contains approximately one quarter of all the observations. Then, I
apply each of the models on the training data to fit the model and estimate the prediction error
using the test data.


##############################################################
In Linear Classification Methods and Discriminant Analysis, I used the dataset “Gender Recognition by Voice” from Kaggle, which can be
downloaded from https://www.kaggle.com/primaryobjects/voicegender. 

This dataset contains voice samples collected from 3168 speakers, half male and half female. It consists of 20
acoustic properties (predictors) that can be used to identify a voice as male or female. The
properties include the mean frequency (in kHz) (“meanfreq”), standard deviation of frequency
(“sd”), median frequency (“median”), first quantile frequency (“Q25”), third quantile frequency
(“Q75”), interquartile range (“IQR”), skewness (“skew”), kurtosis (“kurt”), spectral entropy
(“sp.ent”), spectral flatness (“sfm”), mode frequency (“mode”), frequency centroid (“centroid”),
peak frequency (“peakf”), average of fundamental frequency measured across acoustic signal
(“meanfun”), minimum fundamental frequency measured across acoustic signal (“minfun”),
maximum fundamental frequency measured across acoustic signal (“maxfun”), average of
dominant frequency measured across acoustic signal (“meandom”), minimum of dominant
frequency measured across acoustic signal (“mindom”), maximum of dominant frequency
measured across acoustic signal (“maxdom”), range of dominant frequency measured across
acoustic signal (“dfrange”), and modulation index (“modindx”).

The aim of this project is to use multiple classifiers including linear regression to indicator matrix,
logistic regression, linear discriminant analysis (LDA), quadratic discriminant analysis (QDA)
and regularized discriminant analysis (RDA) to identify the voice as male (coded as “1”) or
female (coded as “0”). We also compare the multiple methods through misclassification error. In
addition, I examine whether dimension reduction techniques such as principal component
analysis and variable selection improve misclassification error.

I first excluded three variables, frequency centroid, interquartile range and range of dominant
frequency measured across acoustic signal (since they are perfect linear combinations of other
predictors), to avoid multicollinearity. Then, I randomly splitted the data into training dataset
and testing dataset. In this case, the testing dataset contains approximately one quarter of all
the observations. Then, I apply each of the methods on the training data to fit the model and
estimate the misclassification error using the both training data and testing data.


##############################################################
In advanced classification project, the data were extracted from the TIMIT database, which is a widely used resource for research
in speech recognition. The dataset we use was formed by selecting five phonemes for
classification based on digitized speech from this database. The phonemes are transcribed as
follows: "sh" as in "she", "dcl" as in "dark", "iy" as the vowel in "she", "aa" as the vowel in "dark",
and "ao" as the first vowel in "water". From continuous speech of 50 male speakers, 4509
speech frames of 32 msec duration were selected, approximately 2 examples of each phoneme
from each speaker. Each speech frame represents one of the above five phonemes.

From each speech frame, a log-periodogram is computed to cast speech data in a form suitable
for speech recognition. Thus, the data used in what follows consists of 4509 log-periodograms
of length 256, with known class (phoneme) memberships. The data contains 256 columns
labelled "x.1" - "x.256", a response column labelled "g", and a column labelled "speaker"
identifying the different speakers. My goal is to identify one of the five phonemes for a given
speech frame. Figure 1.1.1 shows the mean frequency for each class.

The curves for “dcl” and “sh” are very different from the other 3, so it should be easy to identify.
The curves for “aa” and “ao” behave very similarly (this makes sense since they sounds similar)
and it would hard to distinguish them. These observations is consistent with the classification
results in section 2. For simplicity, I code five the phonemes “aa”, “ao”, “dcl”, “iy” and “sh” as
1, 2, 3, 4 and 5, respectively. In order to test the performances of different classifiers, I
randomly split the data into training dataset and test dataset. The test dataset contains
approximately one quarter of all the observations. Then, I apply each of the methods on the
training data to fit the model and then test the performance on the training data. 


##############################################################

In cluster analysis project, the goal is to cluster similar log-periodograms into groups using K-Means Clustering
and Gaussian mixture model. Thus, I will not use the response variable in the analyses. I will try
several different values of clusters and we will evaluate the performance of the clustering methods
using adjusted rand index.
