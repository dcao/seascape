from flask import Flask, render_template, request
import pandas as pd
import numpy as np
import json
from . import ml

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
        time=("time", np.mean),
        class_weighted_evals=("class_weighted_evals", sum),
        instr_weighted_evals=("instr_weighted_evals", sum),
        gpa_expected=("gpa_expected", np.mean),
        gpa_actual=("gpa_actual", np.mean)).reset_index()

    fdf['cid'] = np.arange(len(fdf))
    return fdf

def pctle_df(gdf):
    pdf = gdf.copy()
    pdf["evals"] = gdf["evals"].rank(pct=True)
    pdf["rcmnd_class"] = gdf["rcmnd_class"].rank(pct=True)
    pdf["rcmnd_instr"] = gdf["rcmnd_instr"].rank(pct=True)
    pdf["rcmnd_diff"] = gdf["rcmnd_diff"].rank(pct=True)
    pdf["time"] = gdf["time"].rank(pct=True)
    pdf["class_weighted_evals"] = gdf["class_weighted_evals"].rank(pct=True)
    pdf["instr_weighted_evals"] = gdf["instr_weighted_evals"].rank(pct=True)
    pdf["gpa_expected"] = gdf["gpa_expected"].rank(pct=True)
    pdf["gpa_actual"] = gdf["gpa_actual"].rank(pct=True)
    return pdf

def create_app(test_config=None):
    # create and configure the app
    app = Flask(__name__, instance_relative_config=True)

    # We have to load our dataset first, so we load our premade hdf5 file which
    # contains all the data.
    df = ml.dataset
    
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

        class_id = class_id.upper()

        fdf = group_df(df)
        fdf = fdf[(fdf['course'].str.contains(class_id, regex=False)) & (fdf['instr'].str.contains(prof, regex=False))]

        with pd.option_context('display.max_colwidth', -1):
            with pd.option_context('display.float_format', '{:,.3f}'.format):
                if not fdf.empty:
                    fdf["course"] = fdf.apply(lambda x: '<a href="/course/{0}">{1}</a>'.format(x["cid"], x["course"]), axis=1)
                return render_template(
                    'listing.html', class_id=class_id, prof=prof, data=fdf.to_html(
                        classes="table table-hover", border=0, index=False, escape=False,
                        columns=["instr", "course", "evals", "rcmnd_class", "rcmnd_instr", "rcmnd_diff", "time", "class_weighted_evals", "instr_weighted_evals", "gpa_expected", "gpa_actual"]
                    ))
        

    @app.route('/course/<cid>')
    def course(cid):
        cid = int(cid)
        gdf = group_df(df)
        sz = gdf['instr'].size - 1
        row = gdf.iloc[cid]
        rowp = pctle_df(gdf).iloc[cid]

        other_profs = gdf.loc[gdf["course"] == row["course"]]
        other_profs = json.dumps(other_profs.to_dict(orient='records'), indent=4)
        other_dept = pctle_df(gdf.loc[gdf["course"].str.contains(row["course"].split()[0], regex=False)])
        rowpo = other_dept.loc[other_dept["cid"] == cid]

        regress = ml.regress(row["instr"], row["course"], ["rcmnd_class", "rcmnd_instr", "rcmnd_diff", "time", "gpa_actual"])
        
        return render_template("course.html", row=row, rowp=rowp, other_profs=other_profs, rowpo=rowpo, regress=regress)

    return app
