from flask import Flask, render_template, request
import pandas as pd

def to_int(s):
    try: 
        x = int(s)
        return x
    except ValueError:
        return None

def create_app(test_config=None):
    # create and configure the app
    app = Flask(__name__, instance_relative_config=True)

    # We have to load our dataset first, so we load our premade hdf5 file which
    # contains all the data.
    df = pd.read_hdf("./data/sparse.h5")
    
    @app.route('/')
    def root():
        return render_template('index.html')

    @app.route('/listing')
    def listing():
        class_id = request.args.get('class')
        prof = request.args.get('professor')

        if class_id is None:
            class_id = ""

        if prof is None:
            prof = ""

        fdf = df[(df['course'].str.contains(class_id, regex=False)) & (df['instr'].str.contains(prof, regex=False))]
        fdf = fdf.groupby(["instr", "course"], as_index=False).mean()
        
        return render_template('listing.html', class_id=class_id, prof=prof, data=fdf)

    @app.route('/course/<cid>')
    def course(cid):
        return render_template("course.html")

    return app
