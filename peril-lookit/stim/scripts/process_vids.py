import os
import subprocess as sp
import sys

videoPath = '/Users/shariliu/Documents/HarvardLDS/Studies/RISK/github/peril-lookit/stim/origs/'

videoFiles = os.listdir(videoPath)

for video in videoFiles:
    (shortname, ext) = os.path.splitext(video)
    print(shortname)
    # if ext in ['.mp4'] and 'orig' in shortname:
    if ext in ['.mp4'] and 'cliff' in shortname:
      
    	# # crop all videos 
    	# cropped_vidname = os.path.join(videoPath, shortname + '_crop')

    	# sp.call(['ffmpeg', '-i', os.path.join(videoPath, video), \
    	# 	'-vf', 'crop=1000:720:300:400', '-c:a', 'copy', cropped_vidname +'.mp4'])

    	# trim all videos
    	trimmed_vidname = os.path.join(videoPath, shortname + '_trim')

    	sp.call(['ffmpeg', '-ss', '00:00:03', '-i', os.path.join(videoPath, shortname + '.mp4'), \
    		'-c', 'copy', '-t', '00:00:20', trimmed_vidname +'.mp4'])
    	
    	# flipped_vidname = cropped_vidname + '_flip'

    	# # flip all videos
    	# sp.call(['ffmpeg', '-i', cropped_vidname + '.mp4', \
    	# 	'-vf', 'hflip', '-c:a', 'copy', flipped_vidname +'.mp4'])