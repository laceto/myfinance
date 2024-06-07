import numpy as np

# Gain expectancies and Kelly criterion 
def expectancy(win_rate,avg_win,avg_loss):  
    # win% * avg_win% - loss% * abs(avg_loss%) 
    return win_rate * avg_win + (1-win_rate) * avg_loss 
     
def george(win_rate,avg_win,avg_loss):  
    # (1+ avg_win%)** win% * (1- abs(avg_loss%)) ** loss%  -1 
    return (1+avg_win) ** win_rate * (1 + avg_loss) ** (1 - win_rate) - 1  
     
def kelly(win_rate,avg_win,avg_loss):  
    # Kelly = win% / abs(avg_loss%) - loss% / avg_win% 
    return win_rate / np.abs(avg_loss) - (1-win_rate) / avg_win 

def rolling_profits(returns,window):
    profit_roll = returns.copy()
    profit_roll[profit_roll < 0] = 0
    profit_roll_sum = profit_roll.rolling(window).sum().ffill()
    return profit_roll_sum

def rolling_losses(returns,window):
    loss_roll = returns.copy()
    loss_roll[loss_roll > 0] = 0
    loss_roll_sum = loss_roll.rolling(window).sum().ffill()
    return loss_roll_sum

def expanding_profits(returns): 
    profit_roll = returns.copy() 
    profit_roll[profit_roll < 0] = 0 
    profit_roll_sum = profit_roll.expanding().sum().ffill()
    return profit_roll_sum 
 
def expanding_losses(returns): 
    loss_roll = returns.copy() 
    loss_roll[loss_roll > 0] = 0 
    loss_roll_sum =    loss_roll.expanding().sum().ffill()
    return loss_roll_sum 

def profit_ratio(profits, losses):    
    pr = profits.ffill() / abs(losses.ffill())
    return pr


def rolling_tail_ratio(cumul_returns, window, percentile,limit):
    left_tail = np.abs(cumul_returns.rolling(window).quantile(percentile))
    right_tail = cumul_returns.rolling(window).quantile(1-percentile)
    np.seterr(all='ignore')
    tail = np.maximum(np.minimum(right_tail / left_tail,limit),-limit)
    return tail

def expanding_tail_ratio(cumul_returns, percentile,limit):
    left_tail = np.abs(cumul_returns.expanding().quantile(percentile))
    right_tail = cumul_returns.expanding().quantile(1 - percentile)
    np.seterr(all='ignore')
    tail = np.maximum(np.minimum(right_tail / left_tail,limit),-limit)
    return tail
