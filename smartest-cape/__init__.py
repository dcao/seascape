from flask import Flask, render_template

def create_app(test_config=None):
    # create and configure the app
    app = Flask(__name__, instance_relative_config=True)

    @app.route('/')
    def root():
        return render_template('index.html')

    @app.route('/listing')
    def listing():
        return render_template('listing.html')

    return app
