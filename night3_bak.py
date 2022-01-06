from picamera import mmalobj as mo, mmal
from signal import pause

camera = mo.MMALCamera()
splitter = mo.MMALSplitter()
render_l = mo.MMALRenderer()
render_r = mo.MMALRenderer()

camera.outputs[0].framesize = (720,720)
camera.outputs[0].framerate = 30
camera.outputs[0].commit()

p = render_l.inputs[0].params[mmal.MMAL_PARAMETER_DISPLAYREGION]
p.set = mmal.MMAL_DISPLAY_SET_FULLSCREEN | mmal.MMAL_DISPLAY_SET_DEST_RECT
p.fullscreen = False
p.dest_rect = mmal.MMAL_RECT_T(0, 0, 720, 720)
render_l.inputs[0].params[mmal.MMAL_PARAMETER_DISPLAYREGION] = p
p.dest_rect = mmal.MMAL_RECT_T(0, 720, 720, 720)

splitter.connect(camera.outputs[0])
render_l.connect(splitter.outputs[0]) 
render_r.connect(splitter.outputs[1])

splitter.enable()
render_l.enable()
render_r.enable()

pause()
