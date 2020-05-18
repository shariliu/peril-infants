import os
import subprocess as sp
import sys

videoPath = '/Users/shariliu/Documents/HarvardLDS/Studies/DOE-lookit/stim/video/origs/'

videoFiles = os.listdir(videoPath)

for video in videoFiles:
    (shortname, ext) = os.path.splitext(video)
    print(shortname)
    if ext in ['.mp4']:
    	# trim all videos
    	trimmed_vidname = shortname + '_20s'

    	sp.call(['ffmpeg', '-ss', '00:00:00', '-i', os.path.join(videoPath, video), \
    		'-c', 'copy', '-t', '00:00:24', trimmed_vidname +'.mp4'])