import sys
from flask import Flask, render_template
from flask_flatpages import FlatPages
from flask_frozen import Freezer

FLATPAGES_EXTENSION = '.md'

app = Flask(__name__)
app.config.from_object(__name__)
pages = FlatPages(app)
freezer = Freezer(app)

date_sorted_pages = sorted(pages, key=lambda pg: pg.meta['date'], reverse=True)

@app.route('/')
@app.route('/index.html')
def index():
    return render_template('index.html')

@app.route('/blog.html')
def blog():
    return render_template('blog.html', pages=pages, date_sorted_pages=date_sorted_pages)

@app.route('/apps.html')
def apps():
    return render_template('apps.html')

@app.route('/<path:path>.html')
def page(path):
    page = pages.get_or_404(path)
    return render_template('page.html', page=page)

if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] == "build":
        freezer.freeze()
    else:
        app.run(port=8000)