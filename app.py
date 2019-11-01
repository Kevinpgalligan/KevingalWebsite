import sys
import collections
import functools
from flask import Flask, render_template, render_template_string, Markup
from flask_flatpages import FlatPages, pygmented_markdown, pygments_style_defs
from flask_frozen import Freezer

FLATPAGES_EXTENSION = '.md'

# Used to help render images.
def prerender_jinja(text, flatpages):
    prerendered_body = render_template_string(Markup(text))
    return pygmented_markdown(prerendered_body, flatpages)

app = Flask(__name__)
app.config.from_object(__name__)
app.config['FLATPAGES_HTML_RENDERER'] = prerender_jinja
app.config['FLATPAGES_EXTENSION_CONFIGS'] = {
    'codehilite': {
        'guess_lang': 'False'
    }
}
pages = FlatPages(app)
freezer = Freezer(app)

# Only calculate it once, otherwise it'll get crazy for large numbers of blog posts.
@functools.lru_cache(maxsize=None)
def get_blog_posts():
    date_sorted_blog_posts = sorted(
            # Display drafts when debugging.
            [pg for pg in pages if "blog/" in pg.path and ("draft" not in pg.meta or app.config["DEBUG"])],
            key=lambda pg: pg.meta['date'])
    previous_page = collections.defaultdict(lambda: None)
    next_page = collections.defaultdict(lambda: None)
    for idx, pg in enumerate(date_sorted_blog_posts):
        if idx > 0:
            previous_page[pg] = date_sorted_blog_posts[idx - 1]
        if idx < len(date_sorted_blog_posts) - 1:
            next_page[pg] = date_sorted_blog_posts[idx + 1]
    return list(reversed(date_sorted_blog_posts)), previous_page, next_page

@app.route('/')
@app.route('/index.html')
def index():
    return render_template('index.html')

@app.route('/blog.html')
def blog():
    date_sorted_blog_posts, _, _ = get_blog_posts()
    return render_template(
        'blog.html',
        date_sorted_blog_posts=date_sorted_blog_posts,
        num_posts=len(date_sorted_blog_posts))

@app.route('/projects.html')
def projects():
    return render_template('projects.html')

@app.route('/apps/<name>.html')
def specific_app(name):
    return render_template('apps/' + name + '.html')

@app.route('/404.html')
def not_found():
    return render_template('404.html')

@app.route('/<path:path>.html')
def blog_post(path):
    _, previous_page, next_page = get_blog_posts()
    page = pages.get_or_404(path)
    return render_template(
        'blog-post.html',
        page=page,
        previous_page=previous_page[page],
        next_page=next_page[page])

@freezer.register_generator
def error_handlers():
    yield "/404.html"

@app.route('/pygments.css')
def pygments_css():
    return pygments_style_defs('tango'), 200, {'Content-Type': 'text/css'}

if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] == "build":
        freezer.freeze()
    else:
        app.run(port=8000, debug=True)
