import pandas as pd
import json
import collections 

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator

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


    for item in   experiments:
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
        
        
        ### Compute N.instancias totales vs recursos 
        
        
        
        
         
        appLevels={1:{"small":1,"medium":2,"large":5},
             2:{"small":1,"medium":2,"large":3},
             3:{"medium":4}}
        
        acc ={1:5,2:2,3:4} #initial deployment
        countInstances = {1:1,2:1,3:1}
        
        
        hw = collections.defaultdict(int)
        ins = collections.defaultdict(int)
        
        
        for ix,row in dfsa.iterrows():
            app = row["App"]
            if "Accept" in row["Status"]:
                if row["Action"] == "migrate":
                    #counter ==
                    #level == 
                    pass
                if row["Action"] == "adapt":
                    # counter == 
                    acc[app] += appLevels[app][row["NewLevel"]]-appLevels[app][row["OldLevel"]]
                    
                if row["Action"] == "replicate": 
                    acc[app] += appLevels[app][row["NewLevel"]]
                    countInstances[app]+=1
            
                if row["Action"] == "undeploy":
                    acc[app] -= appLevels[app][row["NewLevel"]]       
                    countInstances[app]-=1
        
                if row["Action"] == "shrink":
                    acc[app] += appLevels[app][row["NewLevel"]]-appLevels[app][row["OldLevel"]]
        
            tH =0
            tI = 0 
            for x  in acc:
                tH += acc[x]
                tI += countInstances[x]
            
            hw[row["Time"]]= tH
            ins[row["Time"]]= tI
        
                
                
        fileStats.write("\n\nLast image of number of HW by APP:\n")
        for ap in acc:
           fileStats.write("\tApp %i use HW: %i\n"%(ap,acc[ap]))
           fileStats.write("\tInstances of App %i: %i\n"%(ap,countInstances[ap]))
        
        
        fileStats.write("\nAVG number of HW used: %i\n"%np.array(list(hw.values())).mean())
        fileStats.write("MAX number of HW used: %i\n"%np.array(list(hw.values())).max())
        fileStats.write("STD number of HW used: %i\n"%np.array(list(hw.values())).std())
        fileStats.write("\nAVG number of INSTANCES created: %i\n"%np.array(list(ins.values())).mean())
        fileStats.write("MAX number of INSTANCES created: %i\n"%np.array(list(ins.values())).max())
        fileStats.write("STD number of INSTANCES created: %i\n"%np.array(list(ins.values())).std())
        
        fileStats.flush()
        fileStats.close()
        
        x = range(len(hw))
        yHW = list(hw.values())
        yIns = list(ins.values())
        fig, ax = plt.subplots(figsize=(20, 12))
        
        l1 = ax.plot(x,yHW,color="green",linewidth=2,label="Hw Units")
        ax.xaxis.set_major_locator(MaxNLocator(integer=True))
        plt.title('Used Hardware vs Number of instances', fontsize=38)
        plt.xlabel(r"activations", fontsize=30)
        plt.ylabel(r"Used Hardware", fontsize=30,color="green")
        ax.tick_params(axis='y', labelcolor="green")
        ax.set_ylim(0,60)
        for tick in ax.get_yticklabels():
                 tick.set_fontsize(24)
        ax.grid()
        ax2 = ax.twinx() 
        ax2.set_ylabel('Number of instances', color="blue", fontsize=30)  # we already handled the x-label with ax1
        l2  = ax2.plot(x,yIns,color="blue",linewidth=2.4,linestyle= '--',marker="o",label="Instances")
        # added these three lines
        lns = l1+l2
        labs = [l.get_label() for l in lns]
        ax.legend(lns, labs, loc=4, prop={'size': 30})
        ax.xaxis.set_major_locator(MaxNLocator(integer=True))
        ax2.tick_params(axis='y', labelcolor="blue")
        for tick in ax2.get_yticklabels():
                 tick.set_fontsize(24)
                         
        fig.savefig(pathcommon+"hw_instances_%s.pdf"%code, dpi=400)
if __name__ == '__main__':
    run()
    print("Done")