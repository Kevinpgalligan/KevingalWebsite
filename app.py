import sys
from flask import Flask, render_template
from flask_flatpages import FlatPages
from flask_frozen import Freezer

FLATPAGES_EXTENSION = '.md'

app = Flask(__name__)
app.config.from_object(__name__)
pages = FlatPages(app)
freezer = Freezer(app)

date_sorted_blog_posts = sorted(
    [pg for pg in pages if "blog/" in pg.path and "draft" not in pg.meta],
    key=lambda pg: pg.meta['date'],
    reverse=True)

@app.route('/')
@app.route('/index.html')
def index():
    return render_template('index.html')

@app.route('/blog.html')
def blog():
    return render_template('blog.html', date_sorted_blog_posts=date_sorted_blog_posts)

@app.route('/apps.html')
def apps():
    return render_template('apps.html')

@app.route('/404.html')
def not_found():
    return render_template('404.html')

@app.route('/<path:path>.html')
def blog_post(path):
    page = pages.get_or_404(path)
    return render_template('blog-post.html', page=page)

@freezer.register_generator
def error_handlers():
    yield "/404.html"

if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] == "build":
        freezer.freeze()
    else:
        app.run(port=8000)