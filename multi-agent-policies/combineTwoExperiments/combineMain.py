import sys
from PIL import Image
from PIL import ImageFont
from PIL import ImageDraw
import os, os.path

list_im = ["prototype3",'prototype4']
description = ["Exp 3 \n Latency Sensitive","Exp 4 \n Balanced Load"]

base_path = "results/images/"
max_images = 40000
# simple version for working with CWD
last_img0 = None
last_img1 = None

for i in range(max_images):
    print("Image: %i"%i)
    no0,no1 = False,False
    file0_path = "../scenarios/%s/%s/network_%05d.png" % (list_im[0],base_path,i)
    file1_path = "../scenarios/%s/%s/network_%05d.png" % (list_im[1],base_path,i)

    if os.path.isfile(file0_path):
        last_img0 = file0_path
    else:
        no0 = True

    if os.path.isfile(file1_path):
        last_img1 = file1_path
    else:
        no1 = True

    if no0 and no1: break

    images = [Image.open(x) for x in [last_img0,last_img1]]
    widths, heights = zip(*(i.size for i in images))
    total_width = sum(widths)
    max_height = max(heights)

    new_im = Image.new('RGB', (total_width, max_height))
    x_offset = 0
    for im in images:
      new_im.paste(im, (x_offset,0))
      x_offset += im.size[0]

    draw = ImageDraw.Draw(new_im)
    # font = ImageFont.truetype(<font-file>, <font-size>)
    font = ImageFont.truetype("Langdon.otf", 40)
    # draw.text((x, y),"Sample Text",(r,g,b))
    # print(total_width)
    # print(max_height)
    draw.text((200, 400),description[0],(0,0,0),font=font)
    draw.text((1800, 400),description[1],(0,0,0),font=font)

    new_im.save('images/network_%05d.png'%i)

