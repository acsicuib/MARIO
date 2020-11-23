import pandas as pd

def showActionsbyGroup(df0,dfmov0):
    df0 = df0.groupby(["time", "action"]).agg({"action": "count"})
    df0.columns = ["freq"]
    df0.reset_index(level=["time", "action"], inplace=True)
    df0 = df0.pivot_table('freq', ['time'], 'action')
    df0 = df0.fillna(0)

    df0 = df0.drop(["nop"], axis=1)
    if "none" in df0.columns:
        df0 = df0.drop(["none"], axis=1)

    sumaActions = df0.sum()
    totalmovements0 = dfmov0.taxi.sum() - dfmov0.taxi[0]

    actions = df0.columns.values
    avActMov = {}
    for a in actions:
        avActMov[a] = sumaActions[a] / totalmovements0
    return avActMov


experiments = [
    ("P1_s3", "Rome", "scenarios/TaxiRome/", "policy/", [[41.878037, 12.4462643], [41.919234, 12.5149603]],
     "policy1.pl"),
    ("P2_s3", "Rome", "scenarios/TaxiRome/", "policy/", [[41.878037, 12.4462643], [41.919234, 12.5149603]],
     "policy2.pl"),
    ("P3_s3", "Rome", "scenarios/TaxiRome/", "policy/", [[41.878037, 12.4462643], [41.919234, 12.5149603]],
     "policy3.pl"),
    (
    "P4_s3", "Rome", "scenarios/TaxiRome/", "policy/", [[41.878037, 12.4462643], [41.919234, 12.5149603]], "policy4.pl")
]


for ncase, name, experiment_path, policy_folder, projection, policy_file in experiments:
    # pathcommon = experiment_path+"results_%s_20201122/"%ncase
    pathcommon = experiment_path + "results_%s_20201122w5/" % ncase
    actions = pathcommon+"action_stats.txt"
    mov = pathcommon+"movements.csv"
    res = pathcommon+"Results_Rome_0.csv"
    detailedMoves = pathcommon+"moves_stats.txt"

    df = pd.read_csv(mov)

    # Getting users
    users_raw = df.groupby("taxi")["DES"].apply(list)  # DES=ID, len(DES)=Number of changes
    usersCode = {}  # key: DES, value: (taxiCode,user_changes)
    usersMovs = {}
    for p in range(len(users_raw)):
        usersCode[users_raw[p][0]] = users_raw.index[p]
        usersMovs[users_raw[p][0]] = len(users_raw[p])

    dfc = pd.read_csv(res)
    # Getting the app of each user
    users_app = {}
    for sample in dfc.iterrows():
        users_app[sample[1]["DES.src"]] = sample[1]["app"]
        if len(users_app) == len(usersCode):
            break

    dfapp = pd.DataFrame([users_app, usersCode, usersMovs]).T
    dfapp.columns = ["app", "code", "movs"]

    ## apps 1,2,3 => Workload senstivie
    ## apps 4,5,6 => Latency sensitive
    mapapps = {1: 0, 2: 0, 3: 0, 4: 1, 5: 1, 6: 1}
    dfapp["type"] = dfapp.app.map(mapapps)

    # print("Total Movements of users for each app")
    # print("\t type=0 are 1,2,3 apps ")
    # print(dfapp.groupby("type").movs.agg([sum]))

    ### Geting the movements for each group of app
    dfmov = pd.read_csv(mov)  # all movements
    dfmovG = dfmov.groupby("time").count()

    # get mov from users Type0
    codes = dfapp[dfapp.type == 0].code.values
    dfmov0 = dfmov[dfmov.taxi.isin(codes)]
    dfmov0 = dfmov0.groupby("time").count()

    # get mov from users Type1
    codes = dfapp[dfapp.type == 1].code.values
    dfmov1 = dfmov[dfmov.taxi.isin(codes)]
    dfmov1 = dfmov1.groupby("time").count()

    ### Geting the operations for each group of app
    dem = pd.read_csv(detailedMoves)
    df0 = dem.loc[dem.app < 4]
    df1 = dem.loc[dem.app > 3]

    aG = showActionsbyGroup(dem, dfmovG)
    a0 = showActionsbyGroup(df0, dfmov0)
    a1 = showActionsbyGroup(df1, dfmov1)


    print("CASE ",ncase)
    print("Apps ALL: ",aG)
    print("Apps WS: ",a0)
    print("Apps LS: ",a1)
    print("**"*5)

print("Done")