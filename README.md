# Description

MARIOII in progress.


Run on server
```
git clone --branch MarioII https://github.com/acsicuib/MARIO
git pull origin MarioII

(mariovenv)
python -m pip install -r requirements.txt 

export PYTHONPATH="/home/isaac/projects/MARIO:/home/isaac/projects/MARIO/multi-agent-policies/environment:$PYTHONPATH"
python3 multi-agent-policies/main.py
```