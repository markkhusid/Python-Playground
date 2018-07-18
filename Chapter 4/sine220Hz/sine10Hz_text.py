import numpy as np
import wave, math

sRate = 44100
#nSamples = sRate * 5
nSamples = sRate

x = np.arange(nSamples)/float(sRate)
vals = np.sin(2.0*math.pi*10.0*x)
rawData = np.array(vals*32767, 'int16')
data = np.array(vals*32767, 'int16').tostring()

#for item in rawData:
#    print item

print 'Opening wave file...'
file = wave.open('sine10Hz.wav', 'wb')
file.setparams((1, 2, sRate, nSamples, 'NONE', 'uncompressed'))

print 'Opening data file...'
datFile = open('sine10Hz.dat', 'wt')

print 'Writing wave file...'
file.writeframes(data)

print 'Writing data file...'
for i in range(nSamples):
    dataElement = str(vals[i]) + '\n'
    datFile.write(dataElement)

print 'Closing wave file...'
file.close()

print 'Closing data file...'
datFile.close()

