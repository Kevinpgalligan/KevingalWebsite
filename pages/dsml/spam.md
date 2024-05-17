name: Spam detection with Naive Bayes
notebook: spam.html

<figure>
<img src="{{ url_for('static', filename='img/dsml/spam-confusion-matrix.png') }}"
     alt="Confusion matrix consisting of 4 panels: 1199 true negatives (not spam, classified as not spam), 176 true positives (spam, classified as spam), 13 false negatives and 5 false positives. This differs from the "
     class="centered">
<figcaption>Confusion matrix showing the number of true positives (spam classified as spam), false positives (spam classified as ham), true negatives and false negatives.</figcaption>
</figure>

Implementing the Naive Bayes classifier to achieve almost 99% test set accuracy on Kaggle's [SMS Spam Collection Dataset](https://www.kaggle.com/datasets/uciml/sms-spam-collection-dataset/). The classifier also achieves high precision (98.8%) and recall (92.7%). The features are based on words used, as well as the presence of money symbols and numbers of various lengths.

