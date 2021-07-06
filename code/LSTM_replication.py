from math import sqrt
import pandas as pd
from numpy import concatenate
from matplotlib import pyplot
from pandas import read_csv
from pandas import DataFrame
from pandas import concat
from sklearn.preprocessing import MinMaxScaler, StandardScaler
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import mean_squared_error
from keras.models import Sequential
from keras.layers import Dense, TimeDistributed
from keras.layers import LSTM
import numpy as np
from keras.callbacks import EarlyStopping
from keras.models import load_model
from plotnine import qplot
import datetime
import matplotlib.dates as mdates
from keras.utils.vis_utils import plot_model
from  matplotlib import ticker
from matplotlib.ticker import ScalarFormatter

# convert series to supervised learning
def series_to_supervised(data, n_in=1, n_out=1, dropnan=True):
    n_vars = 1 if type(data) is list else data.shape[1]
    df = DataFrame(data)
    cols, names = list(), list()
    # input sequence (t-n, ... t-1)
    for i in range(n_in, 0, -1):
        cols.append(df.shift(i))
        names += [('var%d(t-%d)' % (j + 1, i)) for j in range(n_vars)]
    # forecast sequence (t, t+1, ... t+n)
    for i in range(0, n_out):
        cols.append(df.shift(-i))
        if i == 0:
            names += [('var%d(t)' % (j + 1)) for j in range(n_vars)]
        else:
            names += [('var%d(t+%d)' % (j + 1, i)) for j in range(n_vars)]
    # put it all together
    agg = concat(cols, axis=1)
    agg.columns = names
    # drop rows with NaN values
    if dropnan:
        agg.dropna(inplace=True)
    return agg


# load dataset
dataset = read_csv('data_final.csv', header=0, sep=",")
values = dataset.values[:, :]
neurons = 50
n_days = 30
n_features = 9
n_output = 1
# ensure all data is float
values = values.astype('float32')
# normalize features

scaler = MinMaxScaler(feature_range=(0, 1))
scaled = scaler.fit_transform(values)
# frame as supervised learning
reframed = series_to_supervised(scaled, n_days, n_output)
# drop columns we don't want to predict
reframed.drop(reframed.columns[[-1,-2]], axis=1, inplace=True)
print(reframed.head())

# split into train and test sets
values = reframed.values
n_train_days = 365
#n_train_days = round(len(values) * 0.85)
train = values[:n_train_days, :]
test = values[n_train_days:, :]
# split into input and outputs
n_obs = n_days * n_features
train_X, train_y = train[:, :n_obs], train[:, -(n_features-2) * n_output:]
test_X, test_y = test[:, :n_obs], test[:, -(n_features-2) * n_output:]
# reshape input to be 3D [samples, timesteps, features]
train_X = train_X.reshape((train_X.shape[0], n_days, n_features))
test_X = test_X.reshape((test_X.shape[0], n_days, n_features))
print(train_X.shape, train_y.shape, test_X.shape, test_y.shape)

# design network
stop = EarlyStopping(monitor='val_loss', min_delta=0.000000000001, patience=300, restore_best_weights=True) #this big patience is important
model = Sequential()
model.add(LSTM(neurons, input_shape=(train_X.shape[1], train_X.shape[2])))
model.add(Dense((train_X.shape[2]-2) * n_output, activation="linear"))
model.compile(loss='mae', optimizer='adam')
# fit network
history = model.fit(train_X, train_y, epochs=1000, batch_size=32, callbacks=[stop], validation_data=(test_X, test_y), verbose=2,shuffle=False)
# plot history
pyplot.figure(0)
pyplot.plot(history.history['loss'], label='train')
pyplot.plot(history.history['val_loss'], label='test')
pyplot.legend()
pyplot.show()
pyplot.savefig('results/loss.png')


yhat = model.predict(test_X)
# invert scaling for forecast
# inv_yhat = concatenate((yhat, test_X[:, -8:]), axis=1)
# test_X = test_X.reshape((test_X.shape[0], n_obs + ((rolling - 1) * n_features)))
# inv_yhat = concatenate((yhat, test_X[:,-9:-7]),axis=1)
# yhat=yhat.reshape(yhat.shape[0]*n_output,n_features)
inv_yhat_pred = scaler.inverse_transform(concatenate((yhat,yhat[:,-2:]),axis=1))
# real_forecast = scaler.inverse_transform(yhat.reshape(n_output,9))
# invert scaling for actual
# test_y = test_y.reshape((len(test_y), 9))
# inv_y = concatenate((test_y, test_X[:, -8:]), axis=1)
inv_y_pred = scaler.inverse_transform(concatenate((test_y,test_y[:,-2:]),axis=1))

for i in range(0, 7, 1):
    pyplot.rc('xtick', labelsize=20)
    pyplot.rc('ytick', labelsize=20)
    pyplot.figure(i + 1)
    pyplot.title(dataset.columns[i], fontsize=30, fontweight="bold")
    pyplot.plot(inv_yhat_pred[:, i], label="predicted")
    pyplot.plot(inv_y_pred[:, i], label="real")
    pyplot.legend()
    pyplot.savefig('results/test_' + dataset.columns[i] + '.png')


# calculate RMSE
# rmse = sqrt(mean_squared_error(inv_y, inv_yhat))
# print('Test RMSE: %.3f' % rmse)
# real_forecast=real_forecast.reshape(rolling-1,n_features)



#the model for predictions
newModel = Sequential()
newModel.add(LSTM(neurons, batch_input_shape=(1,train_X.shape[1], train_X.shape[2]), return_sequences=False, stateful=True))
newModel.add(Dense((train_X.shape[2]-2) * n_output, activation="linear"))
newModel.set_weights(model.get_weights())
newModel.compile(loss='mae', optimizer='adam')
#predicting from test:
newModel.reset_states()
for i in range(len(train_X)) :
        newModel.predict(train_X[i].reshape(1,n_days,n_features))
for i in range(len(test_X)-1) :
        newModel.predict(test_X[i].reshape(1,n_days,n_features))

rolling = 91

real_forecast=[]
last_test = test_X[-1].reshape(1,n_days,n_features)
#linear interpolation of exogenous variables
covid = pd.DataFrame(dataset[["c19_cases","stringency"]].iloc[dataset.index[-1],:])
covid = covid.T
covid = covid.reset_index(drop=True)
covid.loc[90]=(100,10) #positif
covid = covid.reindex(index=range(0,91))
covid = covid.interpolate(method="linear")
#positif
#x1 = np.asarray([100,10])
#neutre
#x1 = np.asarray([1300,52])
#négatif
#x1 = np.asarray([5500,80])

for i in range(1,rolling,1):
        x1 = np.asarray(covid.loc[i])
        x2 = x1.reshape(1, 2)
        yhat = newModel.predict(last_test)
        inv_yhat = scaler.inverse_transform(concatenate((yhat,yhat[:,-2:]),axis=1))
        inv_yhat = concatenate((inv_yhat[:,:-2],x2), axis=1)
        real_forecast.append(inv_yhat)
        last_test = concatenate((last_test.reshape(n_days,n_features),scaler.transform(inv_yhat)))[1:,:]
        last_test = last_test.reshape((1, n_days , n_features))





"""

# make multistep prediction
model.reset_states()
model.predict(train_X)
model.predict(test_X[:-1])
rolling = 91
real_forecast=[]
last_test = test_X[-1].reshape(1,n_days,n_features)
x1 = np.asarray([400,20])
x2 = x1.reshape(1,2)
for i in range(1,rolling,1):
        yhat = model.predict(last_test)
        inv_yhat = scaler.inverse_transform(concatenate((yhat,yhat[:,-2:]),axis=1))
        inv_yhat = concatenate((inv_yhat[:,:-2],x2), axis=1)
        real_forecast.append(inv_yhat)
        last_test = concatenate((last_test.reshape(n_days,n_features),scaler.transform(inv_yhat)))[1:,:]
        last_test = last_test.reshape((1, n_days , n_features))


"""

real_forecast=np.array(real_forecast)
real_forecast=real_forecast.reshape(rolling-1,n_features)

longueur=len(dataset)-1
realplusforecast=concatenate((dataset.values[:-1,:], real_forecast))
numdays=range(0,longueur+(real_forecast.shape[0]),1)
base = datetime.datetime(2020,1,1)
x = [base + datetime.timedelta(days=z) for z in numdays]

for i in range(0,9,1) :
    fig = pyplot.figure(i + 10)
    ax = fig.add_subplot(111)
    pyplot.title(dataset.columns[i])
    #pyplot.title("Neutral", fontsize=30, fontweight="bold")
    ax.plot(x[:longueur], realplusforecast[:longueur, i], color="black")
    ax.plot(x[longueur - 1:], realplusforecast[longueur - 1:, i], color="blue")
    ax.xaxis.set_major_locator(mdates.MonthLocator(bymonth=(1, 7)))
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%b-%Y'))
    # rotate and align the tick labels so they look better
    fig.autofmt_xdate()
    pyplot.xticks(rotation=0, ha="center")
    pyplot.rc('xtick', labelsize=20)
    pyplot.rc('ytick', labelsize=20)
    # pyplot.legend()
    pyplot.show()
    pyplot.savefig('results/' +dataset.columns[i] +'.png')

""""
model.save('train_model.h5')
model.save('predict_model.h5')
DataFrame(realplusforecast, columns=dataset.columns, index=x).to_csv("results/results.csv")
"""
"""
results=[]

for i in range(300,600,25):
   results.append(errorrmse(i))

results1 = results.reshape(2,len(range(25,300,25)))
"""