from flask import Flask, render_template, request
import pandas as pd
import numpy as np

def to_int(s):
    try: 
        x = int(s)
        return x
    except ValueError:
        return None

def group_df(df):
    fdf = df[["instr", "course", "evals", "rcmnd_class", "rcmnd_instr", "rcmnd_diff", "time", "class_weighted_evals", "instr_weighted_evals", "gpa_expected", "gpa_actual"]].groupby(["instr", "course"]).agg(
        evals=("evals", sum),
        rcmnd_class=("rcmnd_class", np.mean),
        rcmnd_instr=("rcmnd_instr", np.mean),
        rcmnd_diff=("rcmnd_diff", np.mean),
        time=("time", sum),
        class_weighted_evals=("class_weighted_evals", sum),
        instr_weighted_evals=("instr_weighted_evals", sum),
        gpa_expected=("gpa_expected", np.mean),
        gpa_actual=("gpa_actual", np.mean)).reset_index()

    fdf['cid'] = np.arange(len(fdf))
    return fdf

def pctle_df(gdf):
    gdf["evals"] = gdf["evals"].rank(pct=True)
    gdf["rcmnd_class"] = gdf["rcmnd_class"].rank(pct=True)
    gdf["rcmnd_instr"] = gdf["rcmnd_instr"].rank(pct=True)
    gdf["rcmnd_diff"] = gdf["rcmnd_diff"].rank(pct=True)
    gdf["time"] = gdf["time"].rank(pct=True)
    gdf["class_weighted_evals"] = gdf["class_weighted_evals"].rank(pct=True)
    gdf["instr_weighted_evals"] = gdf["instr_weighted_evals"].rank(pct=True)
    gdf["gpa_expected"] = gdf["gpa_expected"].rank(pct=True)
    gdf["gpa_actual"] = gdf["gpa_actual"].rank(pct=True)
    return gdf

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

        fdf = group_df(df)
        fdf = fdf[(fdf['course'].str.contains(class_id, regex=False)) & (fdf['instr'].str.contains(prof, regex=False))]

        with pd.option_context('display.max_colwidth', -1):
            fdf["course"] = fdf.apply(lambda x: '<a href="/course/{0}">{1}</a>'.format(x["cid"], x["course"]), axis=1)
        
        return render_template('listing.html', class_id=class_id, prof=prof, data=fdf)

    @app.route('/course/<cid>')
    def course(cid):
        cid = int(cid)
        gdf = group_df(df)
        sz = gdf['instr'].size - 1
        row = gdf.iloc[cid]
        rowp = pctle_df(gdf).iloc[cid]
        return render_template("course.html", row=row, rowp=rowp)

    return app
