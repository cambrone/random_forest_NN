# random_forest_NN
Group project on predicting traffic accident severity using Random Forests and NN


This repository contains the code and data to reproduce the report “Overcoming Imbalance to Predict Accident Severity Using Neural Networks and Random Forests.” This report was completed as part GROUP project for STAT503 Statistical Learning-Multivariate Analysis. My group members were Rui Ma and Joe Lipa, both masters students in statistics at UM-Ann Arbor. 

As mentioned in the paper, researchers have devoted substantial resources to develop algorithms that can effectively predict the severity of traffic accidents. While the results in publications often indicate positive results, few articles provide extensive details on the approaches used to overcome an intrinsic characteristic of accident datasets: class imbalance. 

Our project examined how the performance of Random Forest and Neural Net varies when these algorithms were trained on datasets that oversampled the minority class or deployed with adaptive learning. The results suggest that training the Random Forest and Neural Net on oversampled data achieved higher recall in the test set for minority class observations. Adaptive learning did not achieve substantially better results for either method. 

In this project, I was responsible for data cleaning, data exploration and writing a substantial portion of the paper. Each members responsibilities are listed in the front page of the paper. 


The project used data on traffic accidents in the UK over a span of a decade. The details of the dataset are included in the paper. This report was completed entirely in R.

The project covers the theoretical concepts and applications of the following topics:

-	Random Forests
-	Neural Net
-	Effects of Imbalance data on classification 
-	Random Oversampling
-	Adaptive Learning
