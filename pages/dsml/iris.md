name: Linear models and gaussian discriminative analysis of iris dataset
notebook: iris.html

<figure>
<img src="{{ url_for('static', filename='img/dsml/iris-prediction.png') }}"
     alt="Plot of sepal length vs. the predicted value, showing that the prediction is pretty good for all flower types."
     class="centered">
<figcaption>Sepal length vs. predicted value, roughly following the line x=y.</figcaption>
</figure>

Applying linear models to regression and classification tasks on the iris flower dataset. Also includes my own implementation of gaussian discriminative analysis, which happens to perform worse than logistic regression on this dataset.
