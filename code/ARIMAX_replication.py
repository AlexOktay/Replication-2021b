import os

import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
from pmdarima import auto_arima
from sklearn.metrics import mean_squared_error
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
from statsmodels.tsa.stattools import adfuller, kpss

from cleaning import cleaning

S = pd.to_datetime('2021-04-01')

RESULTS = 'results_positive'
TITLE = 'Negative'
PATH = 'data/' + RESULTS + '.csv'

dirname = os.getcwd()
plot_path = '../REME/images/plots/'

# Data import
df = cleaning(PATH, date_init='01-01-2020', set_index=True, sep=',')
y = cleaning('data/seco.csv', date_init='01-01-2020', set_index=False, sep=';')
# resampling of y and interpolation
idx = pd.date_range(start=y.index[0], end=y.index[-1])
y = y.reindex(idx)
y = y.interpolate(method='akima')
plt.plot(y)
plt.savefig('../REME/images/plots/' + 'y_interpolated.jpg')
plt.show()
y.to_csv('seco_journalized.csv')

# making sure the seco weekly index is stationary with the AD-fuller test

print(" > Is the data stationary ?")

dftest = adfuller(y, autolag='AIC')
print('ADF:')
print("Test statistic = {:.3f}".format(dftest[0]))
print("P-value = {:.3f}".format(dftest[1]))
print("Critical values :")
for k, v in dftest[4].items():
    print("\t{}: {} - The data is {} stationary with {}% confidence".format(k, v, "not" if v < dftest[0] else "",
                                                                            100 - int(k[:-1])))
print('KPSS:')
dftestkpss = kpss(y, nlags='auto')
print("Test statistic = {:.3f}".format(dftestkpss[0]))
print("P-value = {:.3f}".format(dftestkpss[1]))
print("Critical values :")
for k, v in dftestkpss[3].items():
    print("\t{}: {} - The data is {} stationary with {}% confidence".format(k, v, "" if v < dftestkpss[0] else "not",
                                                                            100 - float(k[:-1])))
d = 0
y_diff = y.diff().dropna()

while dftest[1] > 0.05:
    y_diff = y.diff().dropna()
    dftest = adfuller(y_diff, autolag='AIC')
    print('ADF:')
    print("Test statistic = {:.3f}".format(dftest[0]))
    print("P-value = {:.3f}".format(dftest[1]))
    print("Critical values :")
    for k, v in dftest[4].items():
        print("\t{}: {} - The data is {} stationary with {}% confidence".format(k, v, "not" if v < dftest[0] else "",
                                                                                100 - int(k[:-1])))
    d += 1
    if d == 5:
        break
print(d)

# Then making sure with kpss

print(" > Is the data stationary ?")
print('KPSS:')
dftest = kpss(y_diff, nlags='auto')
print("Test statistic = {:.3f}".format(dftest[0]))
print("P-value = {:.3f}".format(dftest[1]))
print("Critical values :")
for k, v in dftest[3].items():
    print("\t{}: {} - The data is {} stationary with {}% confidence".format(k, v, "" if v < dftest[0] else "not",
                                                                            100 - float(k[:-1])))

# plotting acf and pacf to select parameters of the model

fig, ax = plt.subplots(2, figsize=(24, 12))
ax[0] = plot_acf(y_diff, ax=ax[0], lags=20)
ax[1] = plot_pacf(y_diff, ax=ax[1], lags=10)
plt.savefig('../REME/images/plots/ARIMA/' + 'autocorrelation.jpg')
plt.show()

train = y.loc[:'2020-12-31']
test = y.loc['2021-01-01':]
timesteps = 100
enddate = test.index[-1]
offset_ = enddate + pd.DateOffset(1)
endpreds = offset_ + pd.DateOffset(timesteps)

bigdf = pd.concat([y, df.loc[:enddate]], axis=1)
sns.heatmap(bigdf.corr(), annot=True)
plt.tight_layout()
plt.savefig('../REME/images/plots/ARIMA/' + 'corr.jpg')
plt.show()

# model with exogenous variable

exo = df
exo_short = exo.loc[:enddate - pd.DateOffset(len(test))]
y_short = y.loc[:enddate - pd.DateOffset(len(test))]
# making the arima model


mod = auto_arima(y=y_short, X=exo_short.values, start_p=5, start_q=0, d=1, max_p=10, max_q=10, max_order=None,
                 maxiter=1000)

results = mod
print(results.summary())
results.plot_diagnostics(figsize=(15, 12))
plt.savefig('../REME/images/plots/ARIMA/' + 'plot_diagnostic_exo.jpg')
plt.show()

exog = exo.loc[enddate - pd.DateOffset(len(test) - 1):enddate].values


def forecast_to_df(model, steps=12):
    forecast = model.predict(X=exog, n_periods=len(test), return_conf_int=True,
                             alpha=0.1)
    pred_df = pd.DataFrame(forecast[1], columns=['lower', 'upper'], index=test.index)
    pred_df['pred'] = forecast[0]
    return pred_df


pred_df = forecast_to_df(results, steps=len(train))
pred = pred_df['pred']
mse = mean_squared_error(pred, test)
print('Mean squared error:')
print(mse)


# Plotting Predictions in sample

def plot_train_test_pred(train, test, pred_df):
    fig, ax = plt.subplots(figsize=(8, 6), dpi=500)

    ax.plot(train, label='Train')
    ax.plot(test, label='Test')
    ax.plot(pred_df['pred'], label='Prediction', ls='--', linewidth=3)
    fmt_half_year = mdates.MonthLocator(bymonth=(1, 7))
    ax.xaxis.set_major_locator(fmt_half_year)
    fmt_month = mdates.MonthLocator()
    ax.xaxis.set_minor_locator(fmt_month)
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m'))
    fig.autofmt_xdate()
    plt.xticks(rotation=0, ha="center")
    plt.tight_layout()
    ax.fill_between(x=pred_df.index, y1=pred_df['lower'], y2=pred_df['upper'], alpha=0.3)
    ax.set_title('SECO in sample with exogenous variable', fontsize=22)
    fig.tight_layout()
    return fig, ax


plot_train_test_pred(train, test, pred_df)
plt.legend(loc='lower right')
plt.savefig('../REME/images/plots/ARIMA/' + 'in_sample_prediction_exo.jpg')
plt.show()
order = mod.get_params()['order']
exotest = exo.loc[:enddate]
exo_long = exo.loc[:endpreds]
results = auto_arima(X=exotest, y=y, start_p=5, start_q=0, d=1, max_p=10, max_q=10, max_order=None,
                     maxiter=1000)
print(results.summary())
# Get forecast ahead in future
preds_periods = len(exo[offset_:])
pred_uc = results.predict(X=exo[offset_:].values, n_periods=preds_periods, return_conf_int=True,
                          alpha=0.1)

# Get confidence intervals of forecasts
pred_ci = pd.DataFrame(pred_uc[1], index=pd.date_range(start=offset_, periods=preds_periods))


# plotting predictions out of sample
fig, ax = plt.subplots(figsize=(4, 3), dpi=500)
ax.plot(y)
ax.plot(pd.Series(pred_uc[0], index=pd.date_range(start=offset_, periods=preds_periods)))
ax.fill_between(pred_ci.index,
                pred_ci.iloc[:, 0],
                pred_ci.iloc[:, 1], color='k', alpha=.25)
ax.set_xlabel('Date')
ax.set_ylabel('WEA')
plt.ylim(top=7.5)
# Major ticks every 6 months.
fmt_half_year = mdates.MonthLocator(bymonth=(1, 7))
ax.xaxis.set_major_locator(fmt_half_year)

# Minor ticks every month.
fmt_month = mdates.MonthLocator()
ax.xaxis.set_minor_locator(fmt_month)

# Text in the x axis will be displayed in 'YYYY-mm' format.
ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m'))
fig.autofmt_xdate()
plt.xticks(rotation=0, ha="center")
ax.legend(['observed', 'Forecast'], loc='lower right')
ax.set_title(TITLE)
plt.tight_layout()
plt.savefig('../REME/images/plots/ARIMA/' + 'forecast_exo_' + RESULTS + '.png')
plt.show()

Q1 = y[(pd.Index(y.index.quarter).isin([1])) & (pd.Index(y.index.year).isin([2021]))]
avgQ1 = Q1.mean().values[0]

Q2 = y.loc[S:]
predsQ2 = 91 - len(Q2)
Q2preds = pred_uc[0][:predsQ2]
indexQ2 = pd.date_range(start=S + pd.DateOffset(len(Q2)), periods=len(Q2preds))
Q2preds = pd.DataFrame(Q2preds, index=indexQ2, columns=['seco'])
Q2 = Q2.append(Q2preds)

avgQ2 = Q2.mean().values[0]

print('Q1:' + str(avgQ1))
print('Q2:' + str(avgQ2))
