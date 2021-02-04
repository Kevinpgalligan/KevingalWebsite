import sys
import collections
import datetime
from flask import (Flask, render_template, render_template_string,
    Markup, Response, send_from_directory)
from flask_flatpages import FlatPages, pygmented_markdown, pygments_style_defs
from flask_frozen import Freezer
import os.path
import re
import functools

FLATPAGES_EXTENSION = '.md'
# I hope I never have to change this.
WEBSITE_URL = "https://kevingal.com"

def render_html(text, flatpages, page):
    prerendered_body = render_template_string(Markup(text))
    return pygmented_markdown(prerendered_body, flatpages)

app = Flask(__name__)

def full_url(path):
    # Takes an absolute path, like /static/img/blah.jpg, and joins it with
    # the website URL (https://blah.com or 127.0.0.1).
    return WEBSITE_URL + path

app.config.from_object(__name__)
app.config['FLATPAGES_HTML_RENDERER'] = render_html
app.config['FLATPAGES_MARKDOWN_EXTENSIONS'] = [
    "codehilite",
    "footnotes",
    "markdown_katex",
    "toc",
    "tables"
]
app.config['FLATPAGES_EXTENSION_CONFIGS'] = {
    'codehilite': {
        'guess_lang': 'False'
    },
    'toc': {
        'toc_depth': '3-5'
    },
    'markdown_katex': {
        'insert_fonts_css': False
    }
}
app.jinja_env.globals.update(full_url=full_url)
pages = FlatPages(app)
freezer = Freezer(app)

MAX_NUM_POSTS_IN_FEED = 10

class Tag:
    def __init__(self, name, count):
        self.name = name
        self.count = count

# Cache so it doesn't have to be recomputed
# every time.
@functools.lru_cache(maxsize=1)
def get_blog_posts():
    posts = [pg for pg in pages if "blog/" in pg.path and "publish" in pg.meta]
    posts.sort(key=lambda post: post.meta["date"], reverse=True)
    for post in posts:
        tweak_post_meta(post)
    return posts

def tweak_post_meta(pg):
    if "date" in pg.meta:
        pg.meta["date_rssified"] = pg.meta["date"].strftime('%a, %d %b %Y %T')
    if "tags" in pg.meta and not isinstance(pg.meta["tags"], list):
        pg.meta["tags"] = pg.meta["tags"].split(" ")

def get_posts_for_tag(tag):
    return [post for post in get_blog_posts()
            if tag in post.meta["tags"]]

def get_tags():
    posts = get_blog_posts()
    counts = collections.defaultdict(int)
    for post in posts:
        for tag in post.meta["tags"]:
            counts[tag] += 1
    return sorted([Tag(name, count) for name, count in counts.items()],
                   key=lambda t: t.count,
                   reverse=True)

@app.route('/')
@app.route('/index.html')
def index():
    return render_template('index.html', tags=get_tags())

@app.route('/blog.html')
def blog():
    posts = get_blog_posts()
    return render_template(
        'blog.html',
        posts=posts,
        num_posts=len(posts),
        tags=get_tags())

@app.route('/apps/<name>.html')
def specific_app(name):
    return render_template('apps/' + name + '.html')

@app.route('/blog/tag/<name>.html')
def tag(name):
    posts = get_posts_for_tag(name)
    return render_template(
        'tag.html',
        num_posts=len(posts),
        tag_name=name,
        posts=posts)

@app.route('/404.html')
def not_found():
    return render_template('404.html')

@app.route('/blog/drafts.html')
def draft_posts():
    return render_template(
        'blog/drafts.html',
        draft_posts=[pg for pg in pages if "blog" in pg.path and "publish" not in pg.meta])

@app.route('/<path:path>.html')
def page(path):
    page = pages.get_or_404(path)
    tweak_post_meta(page)
    if path.startswith("blog/"):
        template = 'blog-post.html'
    else:
        template = "page.html"
    return render_template(
        template,
        page=page,
        requires_code="requires" in page.meta and "code" in page.meta["requires"],
        requires_math="requires" in page.meta and "math" in page.meta["requires"])

@freezer.register_generator
def missing_links():
    # These aren't linked using app.route() or the
    # url_for() command, we've gotta let Frozen Flask
    # know where they are.
    links = [
        "/blog/drafts.html",
        "/404.html",
        "/software.html",
        "/apps/pixelate.html"
    ]
    for link in links:
        yield link

# Leaving this in case I need to generate pygments.css again.
#@app.route('/pygments.css')
#def pygments_css():
#    return pygments_style_defs('friendly'), 200, {'Content-Type': 'text/css'}

@app.route("/feed.xml")
def rss_feed():
    posts = get_blog_posts()
    posts = posts[:MAX_NUM_POSTS_IN_FEED]
    return Response(
        render_template(
            "rss.xml",
            blog_posts=posts,
            pub_date=posts[0].meta["date_rssified"]),
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
