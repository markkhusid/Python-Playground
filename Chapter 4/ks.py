# generate note of given frequency
def generageNote(freq):
    nSamples = 44100
    sampleRate = 44100
    N = int(sampleRate / freq)
    
    # initialize ring buffer
    buf = deque([random.random() - 0.5 for i in range(N)])
    # initialize sample buffer
    samples = np.array([0]*nSamples, 'float32')
    for i in range(nSamples):
        samples[i] = buf[0]
        avg = 0.996 * 0.5 * (buf[0] + buf[1])
        buf.append(avg)
        buf.popleft()
        
    # convert samples to 16-bit values and then to a string
    # the maximum value is 32767 for 16-bit
    samples = np.array(samples*32767, 'int16')
    return samples.tostring()
