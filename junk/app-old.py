from flask import Flask
from flask import render_template
import jinja2_highlight

# Meta data about blog posts.
# Post ID, date (YYYY-MM-DD), title, subtitle.
# Newest comes first.
BLOG_POSTS = [
    ("partitioning", "2018-11-13", "How to Partition Data in a Database")
]

POST_ID_TO_DETAILS = {post_id: (date, title)
                      for post_id, date, title in BLOG_POSTS}

class TamingTheMachine(Flask):
    jinja_options = dict(Flask.jinja_options)
    jinja_options.setdefault('extensions', []).append('jinja2_highlight.HighlightExtension')

app = TamingTheMachine(__name__)

@app.route('/')
@app.route('/index.html')
@app.route('/home')
def main_page():
    return render_template('home.html', posts=BLOG_POSTS)

@app.route('/blog')
def blog():
    return render_template('blog.html', posts=BLOG_POSTS)

@app.route('/blog/<post_id>')
def blog_post(post_id):
    if post_id not in POST_ID_TO_DETAILS:
        return render_template('blog-post-does-not-exist.html')
    date, title  = POST_ID_TO_DETAILS[post_id]
    return render_template(
        'blog-posts/{}.html'.format(post_id),
        date=date,
        title=title)

@app.route('/apps')
def projects():
    return render_template('apps.html')

@app.route('/about')
def about():
    return render_template('about.html')

if __name__ == "__main__":
    app.run()
