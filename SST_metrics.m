%% load data
load('/Users/tadzio/Documents/UQProject/data/SST_DHW/Matlab/GBR_Window_FilledSST_Monthly.mat')

%% select summer months  in the last 10 years
% December, January, February (=austral summer)
summer = (YearMonth_unique(:,2) == 1) | (YearMonth_unique(:,2) == 2) | (YearMonth_unique(:,2) == 12); 
% select last 10 years
ysel = YearMonth_unique(:,1)>=2000;
% find index for which summer and ysel is TRUE
ind = find(summer & ysel);
%% calculate mean SST over summer months (because summer is more stable) per pixel
SST_mean = nanmean(FilledSST_month(:,:,ind),3);
% fill land pixels with nans
SST_mean(find(land)) = nan;
% calculate standard deviation over the entire time series for every pixel
SST_std = std(FilledSST_month,0,3);
% fill land pixels with nans
SST_std(find(land)) = nan;
%% write geotifs
% load bbox
load('/Users/tadzio/Documents/UQProject/data/SST_DHW/Matlab/CoRTAD_GeoTiff_Stuff.mat')
% write out
addpath('/Users/tadzio/Documents/UQProject/data/SST_DHW/Matlab/geotiffwrite_20100817')
geotiffwrite('SST_V4_std.tif', bbox, SST_std, 32)