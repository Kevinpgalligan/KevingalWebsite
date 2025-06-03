import sys
import collections
import datetime
import os.path
import re
import functools
import argparse

from flask import (Flask, render_template, render_template_string,
    Markup, Response, send_from_directory)
from flask_flatpages import (FlatPages, pygmented_markdown,
    pygments_style_defs)
from flask_frozen import Freezer
from git import Repo

FLATPAGES_EXTENSION = '.md'
# I hope I never have to change this.
DOMAIN = "kevingal.com"
WEBSITE_URL = f"https://{DOMAIN}"

def render_html(text, flatpages, page):
    prerendered_body = render_template_string(Markup(text))
    return pygmented_markdown(prerendered_body, flatpages)

app = Flask(__name__)

def full_url(path):
    # Takes an absolute path, like /static/img/blah.jpg, and joins it with
    # the website URL (https://blah.com or 127.0.0.1).
    return WEBSITE_URL + path

def proj_type_to_emoji(t):
    return {
        "web": "web",
        "desktop": "desktop"
    }.get(t, "BLAH")

def url_to_linkname(url):
    m = re.search(r"https?://([^/]+)/", url)
    if not m:
        return DOMAIN
    return m.group(1)

BLOG_POST_REGEX = re.compile(r"^/blog/([^/]+)\.html$")

app.config.from_object(__name__)
app.config['FLATPAGES_HTML_RENDERER'] = render_html
app.config['FLATPAGES_MARKDOWN_EXTENSIONS'] = [
    "codehilite",
    "footnotes",
    "toc",
    "markdown_katex",
    "tables"
]
app.config['FLATPAGES_EXTENSION_CONFIGS'] = {
    'codehilite': {
        'guess_lang': 'False'
    },
    'toc': {
        'toc_depth': '2-5'
    },
    'markdown_katex': {
        'insert_fonts_css': False
    }
}
app.jinja_env.globals.update(full_url=full_url)
app.jinja_env.globals.update(proj_type_to_emoji=proj_type_to_emoji)
app.jinja_env.globals.update(url_to_linkname=url_to_linkname)
pages = FlatPages(app)

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

def make_should_skip(rebuild_all, target=None):
    dir_path = os.path.dirname(os.path.realpath(__file__))
    repo = Repo(dir_path)
    posts = get_blog_posts()
    def is_blog_post(url):
        return BLOG_POST_REGEX.match(url) is not None
    def url_to_md_path(url):
        # /blog/blah.html -> pages/blog/blah.md
        m = BLOG_POST_REGEX.match(url)
        return f"pages/blog/{m.group(1)}.md"
    def may_need_next_link_updated(url):
        md_path = url_to_md_path(url)
        # Assuming that new posts only come at the head of the blog...
        return (any(md_path.endswith(post.path) for post in posts)
            and 1 >= next(i for i, post in enumerate(posts)
                            if md_path.endswith(post.path)))
    def build_file_exists(filepath):
        # Not sure if filepath can be None.
        return filepath and os.path.exists(filepath)
    def blog_post_changed(url, filepath):
        # Find last commit before this file was generated. That is
        # presumably the commit it was generated from. Then check
        # if the MarkDown file has changed since that commit.
        file_mod_time = os.path.getmtime(filepath)
        for commit in repo.iter_commits("master"):
            if commit.committed_date < file_mod_time:
                break
        # Regenerate if the post itself has changed or any of
        # the templates it depends on.
        return any([diff.a_path in ["templates/blog-post.html",
                             "templates/page.html",
                             "templates/base.html",
                             url_to_md_path(url)]
                    for diff in repo.head.commit.diff(commit)])
    def should_skip(url, filepath):
        # Don't regenerate blog posts if we can avoid it, they
        # take up the bulk of the time.
        if target is not None:
            return url!=target
        return (not rebuild_all
                and is_blog_post(url)
                and not may_need_next_link_updated(url)
                and build_file_exists(filepath)
                and not blog_post_changed(url, filepath))
    return should_skip
freezer = Freezer(app)

MAX_NUM_POSTS_IN_FEED = 10

class Tag:
    def __init__(self, name, count):
        self.name = name
        self.count = count

def get_posts_for_tag(tag):
    return [post for post in get_blog_posts()
            if tag in post.meta["tags"]]

def get_tags():
    posts = get_blog_posts()
    counts = collections.defaultdict(int)
    for post in posts:
        if "tagcount-exclude" not in post.meta:
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

@app.route('/projects.html')
def projects():
    return render_template(
        "projects.html",
        projects=sorted([proj_with_metadata(pg) for pg in pages if "projects/" in pg.path],
                        key=lambda pg: pg.meta["date"]))

@app.route('/dsml-portfolio.html')
def dsml_portfolio():
    return render_template(
        "dsml-portfolio.html",
        projects=sorted([proj_with_metadata(pg) for pg in pages if "dsml/" in pg.path],
                        key=lambda pg: int(pg.meta["z"]) if "z" in pg.meta else 0,
                        reverse=True))


def proj_with_metadata(page):
    page.meta["id"] = "-".join(page.meta["name"].lower().split(" "))
    return page

@app.route('/notebooks/<name>.html')
def notebook(name):
    return send_from_directory(
        os.path.join(app.root_path, f"templates/notebooks/"),
        f"{name}.html",
        mimetype="html")

@app.route('/<path:path>.html')
def page(path):
    page = pages.get_or_404(path)
    tweak_post_meta(page)
    is_blog = False
    if path.startswith("blog/"):
        template = 'blog-post.html'
        is_blog = True
    else:
        template = "page.html"
    return render_template(
        template,
        page=page,
        requires_code="requires" in page.meta and "code" in page.meta["requires"],
        requires_math="requires" in page.meta and "math" in page.meta["requires"],
        **({} if not is_blog else get_surrounding_posts_for_blog_post(page)))

def get_surrounding_posts_for_blog_post(post):
    def htmlize_path(p):
        return "/" + p + ".html"
    posts = get_blog_posts()
    i = posts.index(post) if post in posts else None
    return dict(**({} if (i is None) or i==len(posts)-1 else dict(prev_post=htmlize_path(posts[i+1].path))),
                **({} if (i is None) or i==0 else dict(next_post=htmlize_path(posts[i-1].path))))

@freezer.register_generator
def missing_links():
    # These aren't linked using app.route() or the
    # url_for() command, we've gotta let Frozen Flask
    # know where they are.
    # Me from the future: gods, this is an awful hack.
    links = [
        "/blog/drafts.html",
        "/404.html",
        "/apps/metronome.html",
        "/apps/emojipasta.html",
        "/apps/pixelate.html",
        "/apps/collision.html",
        "/apps/slingshotchess.html",
        "/apps/tuner.html",
    ]
    for link in links:
        yield link

# Leaving this in case I need to generate pygments.css again.
#@app.route('/pygments.css')
#def pygments_css():
#    return pygments_style_defs('friendly'), 200, {'Content-Type': 'text/css'}

@app.route("/feed.xml")
def rss_feed():
    posts = [post for post in get_blog_posts()
             if "rss-exclude" not in post.meta]
    posts = posts[:MAX_NUM_POSTS_IN_FEED]
    return Response(
        render_template(
            "rss.xml",
            blog_posts=posts,
            pub_date=posts[0].meta["date_rssified"]),
        mimetype="application/rss+xml")

# Really shouldn't need to have a separate function for each file, not
# bothered to figure it out right now.
@app.route("/camino-plan.txt")
def camino_plan():
    return send_from_directory(
        os.path.join(app.root_path, "files"),
        "camino-plan.txt",
        mimetype="text")

@app.route("/favicon.ico")
def favicon():
    return send_from_directory(
        os.path.join(app.root_path, "static"),
        "favicon.ico",
        mimetype="image")

def make_should_skip_from_args(args):
    if args.selective:
        return make_should_skip(False)
    elif args.all:
        return make_should_skip(True)
    elif args.file:
        return make_should_skip(False, target=args.file)
    else:
        sys.exit(1)

if __name__ == '__main__':
    if len(sys.argv) == 1:
        app.run(port=8000, debug=True)
    else:
        parser = argparse.ArgumentParser()
        parser.add_argument("--all", default=False, action="store_true")
        parser.add_argument("--selective", default=False, action="store_true",
                            help="skips certain files that don't need to be regenerated.")
        parser.add_argument("--file", required=False,
                            help="Path to single file to regenerate, like /index.html")
        args = parser.parse_args()
        app.config["FREEZER_SKIP_EXISTING"] = make_should_skip_from_args(args) 
        if args.file:
            def gen():
                yield args.file
            freezer.register_generator(gen)
        freezer.freeze()
