# This script creates a sine wave.  It then saves the sine wave
# as a wav file, and as a raw data file.
# Finally, it plots the raw data.

import numpy as np                          # Used to create arrays
import wave, math                           # Used to create wav files and to get the constant pi
import matplotlib                           # Used for plotting
matplotlib.use('TkAgg')
# TkAgg is a tKinter renderer to make the graphics for the plots

from matplotlib import pyplot as plt        # Used for plotting

sRate = 44100                               # sample rate in Hz
freq = 50.0                                 # Sine wave frequency in Hz
num_periods = 10                            # number of periods of the sine wave
nSamples = sRate * num_periods              # Total number of samples
nChannels = 1                               # For mono mode
sampleWidth = 2                             # Number of bytes per sample

# Create the x-axis.  The goal is to create nSamples number of values from 0 to 1 as floats
x = np.arange(nSamples)/float(sRate*num_periods)

# Convert frequency to radians and apply to x-axis, and then take the sine
vals = np.sin(2.0*math.pi*freq*x)

# Since the amplitude swings from -1 to 1, we multiply by (2**16)-1, and convert to 16 bit integer
rawData = np.array(vals*32767, 'int16')

# Wave files need the numbers to be represented as strings, which are then stored as binary numbers
data = np.array(vals*32767, 'int16').tostring()

# Open wave file with listed filename in write mode as binary file
print 'Opening wave file...'
file = wave.open('sine50Hz.wav', 'wb')
# Set file parameters
file.setparams((nChannels, sampleWidth, sRate, nSamples, 'NONE', 'uncompressed'))

# Open a text file in write mode.  This is for simple observation of the generated results
print 'Opening data file...'
datFile = open('sine50Hz.dat', 'wt')

# Write array into wave file
print 'Writing wave file...'
file.writeframes(rawData)

# Traverese array of sine values, write out data element with added new line.
print 'Writing data file...'
for i in range(nSamples):
    dataElement = str(vals[i]) + '\n'
    datFile.write(dataElement)

# Close wave file
print 'Closing wave file...'
file.close()

# Close text data file
print 'Closing data file...'
datFile.close()

# Plot sine values to observe operation
print 'Plotting data...'
plt.plot(vals)
plt.title('Sine Wave 50Hz Sampled at 44100Hz for 10 Periods')
plt.ylabel('Amplitude [Arbitrary Units]')
plt.xlabel('Samples [Arbitrary Units]')
plt.show()
