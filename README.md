# Multinomial-logistic-regression-on-multi-feature-digit-dataset
We have 2000 images described by 6 sets of features (Fourier coefficients, correlation profile, Karhunen-Love coefficients, averaged pixel values, Zernike moments, morphology) and the goal is to use multinomial logistic regression to predict  handwritten digits on unseen data in R.

Each of these images represents a handwritten digit between 0 and 9. The data and its description can be accessed here: http://archive.ics.uci.edu/ml/datasets/multiple+features

The main steps followed are as follow:

- Data preprocessing undertaken, including the exploratory data analysis.
- Principal Component Analysis (PCA) to describe the data and K-means algorithm applied on the results of PCA.
- Multinomial logistic regression (10 classes) on the target variable.
