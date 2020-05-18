import os
import subprocess as sp
import sys

videoPath = '/Users/shariliu/Documents/HarvardLDS/Studies/DOE-lookit/stim/video/origs/new_hab/'

videoFiles = os.listdir(videoPath)

# hab1 video
sp.call(["ffmpeg", "-i", os.path.join(videoPath, "hab1_crop.mp4"), "-i", os.path.join(videoPath, "hab2_crop_flip.mp4"), "-filter_complex",\
 "[0]pad=iw+500:color=white[left];[left][1]hstack=inputs=2", os.path.join(videoPath, "hab1_hab2.mp4")])

# hab2 video
sp.call(["ffmpeg", "-i", os.path.join(videoPath, "hab2_crop.mp4"), "-i", os.path.join(videoPath, "hab1_crop_flip.mp4"), "-filter_complex",\
 "[0]pad=iw+500:color=white[left];[left][1]hstack=inputs=2", os.path.join(videoPath, "hab2_hab1.mp4")])

# test video 1
#sp.call(["ffmpeg", "-i", "high_orig_crop_trim.mp4", "-i", "low_orig_crop_trim_flip.mp4", "-filter_complex",\
# "[0]pad=iw+500:color=white[left];[left][1]hstack=inputs=2", "ineff_eff.mp4"])

# test video 2
#sp.call(["ffmpeg", "-i", "low_orig_crop_trim.mp4", "-i", "high_orig_crop_trim_flip.mp4", "-filter_complex",\
# "[0]pad=iw+500:color=white[left];[left][1]hstack=inputs=2", "eff_ineff.mp4"])