import os
import subprocess as sp
import sys

videoPath = '/Users/shariliu/Documents/HarvardLDS/Studies/DOE-lookit/stim/mp4/'

videoFiles = os.listdir(videoPath)

for video in videoFiles:
    (shortname, ext) = os.path.splitext(video)
    if shortname == "eff" or shortname == "ineff" or shortname == "hab": 
        print(shortname)

        if ext in ['.mp4']:

            sp.call(['ffmpeg', '-i', os.path.join(videoPath, video), \
                '-vcodec', 'libvpx', '-acodec', 'libvorbis', os.path.join(videoPath, shortname + '.webm')])

        elif ext in ['.mp3']:

            sp.call(['ffmpeg', '-i', os.path.join(videoPath,video), \
                os.path.join(videoPath, shortname + '.ogg')])