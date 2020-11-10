
import trackanimation
import numpy as np
import json
from datetime import datetime, tzinfo, timedelta

class simple_utc(tzinfo):
    def tzname(self,**kwargs):
        return "UTC"
    def utcoffset(self, dt):
        return timedelta(0)




def create_czml_path(dft):
    results = []

    for idx,item in dft.iterrows():
        results.append(item.duration)
        results.append(item.longitude)
        results.append(item.latitude)
        results.append(30)  # for use with point = {"heightReference" : "RELATIVE_TO_GROUND"}

    return results


def point_with_trailing_path(df_input, time_multiplier=10):
    # Store output in array
    czml_output = []

    # Define global variables
    global_id = "document"
    global_name = "Visualizing GPX Data"
    global_version = "1.0"
    global_author = "Will Geary Original author, " \
                    "Modification by Isaac Lera"

    # ctime = datetime.utcnow()
    # etime = ctime + timedelta(0,60)
    # ctime = ctime.replace(tzinfo=simple_utc()).isoformat().replace('+00:00', 'Z')
    # etime=  etime.replace(tzinfo=simple_utc()).isoformat().replace('+00:00', 'Z')

    ctime = "2012-08-04T10:00:00Z"
    etime = "2012-08-04T11:00:00Z"

    global_starttime = ctime
    global_stoptime = etime


    global_availability = global_starttime + "/" + global_stoptime

    # Create packet with global variables
    global_element = {
        "id": global_id,
        "name": global_name,
        "version": global_version,
        "author": global_author,
        "clock": {
            "interval": global_availability,
            "currentTime": global_starttime,
            "multiplier": time_multiplier
        }
    }

    # Append global packet to output
    czml_output.append(global_element)

    # Define path variables
    path_id = "path"
    path_starttime = ctime
    path_stoptime = etime
    path_availability = path_starttime + "/" + path_stoptime

    # Create path object
    path_object = {
        "id": path_id,
        "name": "my path ",
        "availability": path_availability,

        "path": {
            "material": {
                "polylineOutline": {
                    "color": {
                        "rgba": [255, 255, 255, 200]
                    }
                    # ,
                    # "outlineColor": {
                    #     "rgba": [0, 173, 253, 200]
                    # },
                    # "outlineWidth": 5
                }
            },
            "width": 2,
            "leadTime": 5,
            "trailTime": 60,
            "resolution": 15,
            "heightReference": "RELATIVE_TO_GROUND"
        },

        "position": {
            "epoch": path_starttime,
            "cartographicDegrees": create_czml_path(df_input)
        }


    }

    # Append path element to output
    czml_output.append(path_object)

    # Define point variable
    # point_id = "Point"
    # point_starttime = ctime.replace(tzinfo=simple_utc()).isoformat().replace('+00:00', 'Z')
    # point_stoptime = etime.replace(tzinfo=simple_utc()).isoformat().replace('+00:00', 'Z')
    # point_availability = point_starttime + "/" + point_stoptime
    #
    # point_object = {
    #     "id": point_id,
    #
    #     "availability": point_availability,
    #
    #     "position": {
    #         "epoch": point_starttime,
    #         "cartographicDegrees": create_czml_path(df_input)
    #     },
    #
    #     "point": {
    #         "color": {
    #             "rgba": [255, 255, 255, 255]
    #         },
    #         "outlineColor": {
    #             "rgba": [0, 173, 253, 255]
    #         },
    #         "outlineWidth": 6,
    #         "pixelSize": 8,
    #         "heightReference": "RELATIVE_TO_GROUND"
    #     }
    # }

    # czml_output.append(point_object)

    return czml_output


input_directory = "/Users/isaaclera/PycharmProjects/DistributedPolicies/" \
                  "multi-agent-policies/scenarios/TaxiRome/results_20201028/normalized_trajectories.csv"
tracks = trackanimation.read_track(input_directory)

codes = set(np.array(tracks.df.CodeRoute.values))

alltracks = []
for code in codes:
    dft = tracks.df[tracks.df.CodeRoute == code]
    dft = dft[["Latitude","Longitude","VideoFrame"]]
    dft.columns = ["latitude","longitude","duration"]

    alltracks.append(point_with_trailing_path(dft))
    # with open("route_%s.czml"%code,"w") as f:
    #     json.dump(czml_output, f)
    # break

with open("all_tracks.txt","w") as f:
    f.write("var alltracks = %s;"%alltracks)
