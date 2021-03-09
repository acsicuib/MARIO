import pandas as pd
import json
import collections 

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

def run():

    with open("experiment.json") as f:
        experiments = json.load(f)


    for item in experiments:
    # item = experiments[0]

        code = item["code"]
        name = item["scenario"]
        policy = item["policy"]
        nm_policy = item["n_policy"]
        radius = item["radius"]
        reversepath = item["reversepath"]

        experiment_path = "scenarios/%s/"%name

        # datestamp = time.strftime('%Y%m%d_%H%M')
        datestamp = "X"
        pathcommon = "results/results_%s_%s" % (code, datestamp) + "/"

        pathfile_agg_actions = pathcommon+"agg_operations_NM.csv"
        pathfile_mov = pathcommon+"movements.csv"
        res = pathcommon+"Results_%s_0.csv"%name
        spc_actions = pathcommon+"specific_actions.csv"

        fileStats = open(pathcommon+"stats_%s.txt"%code,"w")

        dfag = pd.read_csv(pathfile_agg_actions)

        if "none" in dfag.columns:
           del dfag["none"]

        fileStats.write("Total number of NM activations:\n")
        fileStats.write(str(len(dfag)))


        print("Total number of actions by TYPE:\n")
        print(dfag[dfag.columns[1:9]].sum(axis=0))

        fileStats.write("\n\nTotal number of actions by TYPE:\n")
        fileStats.write(str(dfag[dfag.columns[1:9]].sum(axis=0)))

        print("Total number of actions: (wihtout nops):")
        if "nop" in dfag.columns: del dfag["nop"]
        print(dfag[dfag.columns[1:8]].sum(axis=0).sum())

        fileStats.write("\n\nTotal number of actions: (wihtout nops):\n")
        fileStats.write(str(dfag[dfag.columns[1:8]].sum(axis=0).sum()))

        fileStats.write("\n\nTotal number of NM invocations:\n")

        freqNM = collections.Counter(dfag["nodeManager"])
        for n in freqNM:
            fileStats.write("Node: %i -- %i\n"%(n,freqNM[n]))



        dfsa = pd.read_csv(spc_actions)

        fileStats.write("\n\nTotal number of ACTIONS:\n")
        fileStats.write("%i\n"%(len(dfsa)))


        fileStats.write("\n\nTotal number of ACTIONS requests by type APPs:\n")
        typeAPP = collections.Counter(dfsa["App"])
        for n in typeAPP:
            fileStats.write("APP: %i -- %i\n"%(n,typeAPP[n]))


        dfsaA = dfsa[ (dfsa["Status"]=="Accept") | (dfsa["Status"]=="Accept-1") | (dfsa["Status"]=="Accept-2")]

        fileStats.write("\n\nTotal number of ACTIONS ACCEPTED (with Shrink->replicate+migrate and evit->rep+migrate):\n")
        fileStats.write(str(dfsaA.groupby("Action")["NM"].agg(["count"])))


        fileStats.write("\n\nTotal number of ACTIONS ACCEPTED withOUT Shrink->replicate+migrate and evit->rep+migrate):\n")
        dfsaA = dfsa[ (dfsa["Status"]=="Accept")]
        fileStats.write(str(dfsaA.groupby("Action")["NM"].agg(["count"])))




        dfsaR = dfsa[ (dfsa["Status"]=="Reject")]
        fileStats.write("\n\nTotal number of ACTIONS REJECTED:\n")
        fileStats.write(str(dfsaR.groupby("Action")["NM"].agg(["count"])))




        fileStats.flush()
        fileStats.close()

   
    # df = pd.read_csv(mov)

    # # Getting users
    # users_raw = df.groupby("taxi")["DES"].apply(list)  # DES=ID, len(DES)=Number of changes
    # usersCode = {}  # key: DES, value: (taxiCode,user_changes)
    # usersMovs = {}
    # for p in range(len(users_raw)):
    #     usersCode[users_raw[p][0]] = users_raw.index[p]
    #     usersMovs[users_raw[p][0]] = len(users_raw[p])

    # dfc = pd.read_csv(res)
    # # Getting the app of each user
    # users_app = {}
    # for sample in dfc.iterrows():
    #     users_app[sample[1]["DES.src"]] = sample[1]["app"]
    #     if len(users_app) == len(usersCode):
    #         break

    # dfapp = pd.DataFrame([users_app, usersCode, usersMovs]).T
    # dfapp.columns = ["app", "code", "movs"]

    # ## apps 1,2,3 => Workload senstivie
    # ## apps 4,5,6 => Latency sensitive
    # mapapps = {1: 0, 2: 0, 3: 0, 4: 1, 5: 1, 6: 1}
    # dfapp["type"] = dfapp.app.map(mapapps)

    # # print("Total Movements of users for each app")
    # # print("\t type=0 are 1,2,3 apps ")
    # # print(dfapp.groupby("type").movs.agg([sum]))

    # ### Geting the movements for each group of app
    # dfmov = pd.read_csv(mov)  # all movements
    # dfmovG = dfmov.groupby("time").count()

    # # get mov from users Type0
    # codes = dfapp[dfapp.type == 0].code.values
    # dfmov0 = dfmov[dfmov.taxi.isin(codes)]
    # dfmov0 = dfmov0.groupby("time").count()

    # # get mov from users Type1
    # codes = dfapp[dfapp.type == 1].code.values
    # dfmov1 = dfmov[dfmov.taxi.isin(codes)]
    # dfmov1 = dfmov1.groupby("time").count()

    # ### Geting the operations for each group of app
    # dem = pd.read_csv(detailedMoves)
    # df0 = dem.loc[dem.app < 4]
    # df1 = dem.loc[dem.app > 3]

    # aG = showActionsbyGroup(dem, dfmovG)
    # a0 = showActionsbyGroup(df0, dfmov0)
    # a1 = showActionsbyGroup(df1, dfmov1)


    # print("CASE ",ncase)
    # print("Apps ALL: ",aG)
    # print("Apps WS: ",a0)
    # print("Apps LS: ",a1)
    # print("**"*5)

if __name__ == '__main__':
    run()
    print("Done")