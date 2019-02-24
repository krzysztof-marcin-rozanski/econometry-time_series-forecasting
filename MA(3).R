# Generate noise
noise=rnorm(10000)

# Introduce a variable
ma_3=NULL

# Loop for generating MA(3) process

for(i in 4:10000){
  ma_3[i]=noise[i] + 0.7*noise[i-1] + 0.2*noise[i-2] + 0.1*noise[i-3]
}

# Shift data to left by 2 units
moving_average_process=ma_3[4:10000]

# Put time series structure on a vanilla data
moving_average_process=ts(moving_average_process)

# Partition output graphics as a multi frame of 2 rows and 1 column
par(mfrow=c(2,1))

# plot the process and plot its ACF
plot(moving_average_process, main='A moving average process of order 3', ylab=' ', col='blue')
acf(moving_average_process, main='Correlogram of a moving average process of order 3')

