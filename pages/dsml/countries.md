name: Country clustering with k-means
notebook: countries.html

<figure>
<img src="{{ url_for('static', filename='img/dsml/country-clusters.png') }}"
     alt="2-dimensional plot of country data, clustered into 4 groups."
     class="centered">
<figcaption>PCA visualisation of country clusters with k=4.</figcaption>
</figure>

Implementing the k-means algorithm to cluster countries based on [economic data](https://www.kaggle.com/datasets/rohan0301/unsupervised-learning-on-country-data/). The elbow and silhouette methods are used to pick an appropriate number of clusters.
