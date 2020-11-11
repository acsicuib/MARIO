# Description

MARIOII in progress.

How to install Prolog: 
https://www.swi-prolog.org/build/PPA.html

Run on server
```
git clone --branch MarioII https://github.com/acsicuib/MARIO
git pull origin MarioII

(mariovenv - configure a virtual env.)
python -m pip install -r requirements.txt 

(on MARIO/multi-agent-policies/ folder)
export PYTHONPATH="/home/isaac/projects/MARIO:/home/isaac/projects/MARIO/multi-agent-policies/environment:$PYTHONPATH"
python3 main.py

(Video generation)
ffmpeg -r 1 -i multi-agent-policies/scenarios/TaxiRome/results_20201028/images/network_%05d.png -c:v libx264 -vf fps=1 -pix_fmt yuv420p video.mp4

(me copypaste)
scp isaac@deepblue:/home/isaac/projects/MARIO/video.mp4 .

```
scp isaac@deepblue:/home/isaac/projects/MARIO/multi-agent-policies/scenarios/TaxiRome/results_20201028/models/rules_swi_UID96_nn0lt0ln0_s0_X_9700.pl .