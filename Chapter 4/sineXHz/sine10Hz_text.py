import numpy as np
import wave, math
import matplotlib
matplotlib.use('TkAgg')

from matplotlib import pyplot as plt

sRate = 44100
freq = 10.0
num_periods = 10
nSamples = sRate * num_periods


x = np.arange(nSamples)/float(sRate)
vals = np.sin(2.0*math.pi*freq*x)
rawData = np.array(vals*32767, 'int32')
data = np.array(vals*32767, 'int32').tostring()

vals_in_Hz = np.sin((x*360*(math.pi/180.0)*freq))
rawdata_in_Hz = np.array(vals_in_Hz*32767, 'int32')
#valsFFT = np.fft.fft(rawData)

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
    dataElement = str(vals_in_Hz[i]) + '\n'
    datFile.write(dataElement)

print 'Closing wave file...'
file.close()

print 'Closing data file...'
datFile.close()

print 'Plotting data...'
plt.plot(rawdata_in_Hz)
plt.show()
#plt.plot(rawdataFFT)
#plt.show()
