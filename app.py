import sys
import collections
import datetime
from flask import (Flask, render_template, render_template_string,
    Markup, Response, send_from_directory)
from flask_flatpages import FlatPages, pygmented_markdown, pygments_style_defs
from flask_frozen import Freezer
import os.path
import re

FLATPAGES_EXTENSION = '.md'
FOOTNOTE_REGEX = re.compile(r"\[\^[0-9a-zA-Z-]+\]")

def render_html(text, flatpages, page):
    if page.path.startswith("blog/"):
        # Add a horizontal bar at the end of a blog post if
        # there are no footnotes. If there are, then the
        # footnotes extension will add a bar above the
        # footnotes automatically.
        match = FOOTNOTE_REGEX.search(text)
        if match is None:
            text += "\n<hr>"
    prerendered_body = render_template_string(Markup(text))
    return pygmented_markdown(prerendered_body, flatpages)

app = Flask(__name__)
app.config.from_object(__name__)
app.config['FLATPAGES_HTML_RENDERER'] = render_html
app.config['FLATPAGES_MARKDOWN_EXTENSIONS'] = [
    "codehilite",
    "footnotes",
    "mdx_math"
]
app.config['FLATPAGES_EXTENSION_CONFIGS'] = {
    'codehilite': {
        'guess_lang': 'False'
    }
}
pages = FlatPages(app)
freezer = Freezer(app)

MAX_NUM_POSTS_IN_FEED = 10

def get_blog_posts():
    for page in pages:
        date = page.meta["date"]
        page.meta["date_rssified"] = date.strftime('%a, %d %b %Y %T')
    date_sorted_blog_posts = sorted(
            [pg for pg in pages if "blog/" in pg.path and "draft" not in pg.meta],
            key=lambda pg: pg.meta['date'])
    return list(reversed(date_sorted_blog_posts))

@app.route('/')
@app.route('/index.html')
def index():
    return render_template('index.html')

@app.route('/blog.html')
def blog():
    date_sorted_blog_posts = get_blog_posts()
    posts_by_year = collections.defaultdict(list)
    for post in date_sorted_blog_posts:
        posts_by_year[post.meta["date"].year].append(post)
    posts_by_year = list(
        sorted(
            posts_by_year.items(),
            # Don't use lexicographic sorting, it'll fail
            # after the year 9999.
            key=lambda pair: int(pair[0]),
            reverse=True))
    return render_template(
        'blog.html',
        posts_by_year=posts_by_year,
        num_posts=len(date_sorted_blog_posts))

@app.route('/software.html')
def apps():
    return render_template('software.html')

@app.route('/apps/<name>.html')
def specific_app(name):
    return render_template('apps/' + name + '.html')

@app.route('/404.html')
def not_found():
    return render_template('404.html')

@app.route('/blog/drafts.html')
def draft_posts():
    return render_template(
        'blog/drafts.html',
        draft_posts=[pg for pg in pages if "blog" in pg.path and "draft" in pg.meta])

@app.route('/<path:path>.html')
def blog_post(path):
    page = pages.get_or_404(path)
    return render_template(
        'blog-post.html',
        page=page,
        requires_math="requires" in page.meta and "math" in page.meta["requires"],
        requires_code="requires" in page.meta and "code" in page.meta["requires"])

@freezer.register_generator
def drafts():
    yield "/blog/drafts.html"

@freezer.register_generator
def error_handlers():
    yield "/404.html"

@app.route('/pygments.css')
def pygments_css():
    return pygments_style_defs('emacs'), 200, {'Content-Type': 'text/css'}

@app.route("/feed.xml")
def rss_feed():
    date_sorted_blog_posts = get_blog_posts()
    date_sorted_blog_posts = date_sorted_blog_posts[:MAX_NUM_POSTS_IN_FEED]
    return Response(
        render_template(
            "rss.xml",
            blog_posts=date_sorted_blog_posts,
            pub_date=date_sorted_blog_posts[0].meta["date_rssified"]),
        mimetype="application/rss+xml")

@app.route("/favicon.ico")
def favicon():
    return send_from_directory(
        os.path.join(app.root_path, "static"),
        "favicon.ico",
        mimetype="image")

if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] == "build":
        freezer.freeze()
    else:
        app.run(port=8000, debug=True)
