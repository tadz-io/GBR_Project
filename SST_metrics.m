% select summer months  
% now runs from september untill februari
summer = (YearMonth_unique(:,2) >= 9) | (YearMonth_unique(:,2) <= 2); 

% calculate median SST over summer months per pixel
SST_median = median(FilledSST_month(:,:,summer),3);
% calculate standard deviation over the entire time series for every pixel
SST_std = std(FilledSST_month,0,3)