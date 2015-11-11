%% for CoRTAD V5 data
% load refs for export of geotiff
load('/Users/tadzio/Documents/UQProject/data/SST_DHW/Matlab/CoRTAD_GeoTiff_Stuff.mat')
% load geotiffs and stack
cd('/Users/tadzio/Documents/UQProject/data/SST_DHW/CoRTAD_V5/DHW');
ls = dir('*.tif');
%allocate matrix
DHW_Max = zeros([size(imread(ls(1).name)), size(ls,1)]);
% load tifs into 3D matrix
for i = 1:size(ls,1)
    DHW_Max(:,:,i) = imread(ls(i).name);
    % get year
    yr_c = regexp(ls(i).name, 'DHW_Max_(\d+)_v5_m1.tif', 'tokens');
    yr(i) = cellfun(@(x) str2num(x{1}), yr_c);
end
yr = transpose(yr);
%% calculate median DHW
DHW_med = median(DHW_Max, 3);

%% calculate probability of occurence
% first have to set a threshold
% bleaching @ 4 <= DHW < 8
% wide-spread bleaching + mortality @ 8 <= DHW
% see Liu et al 2003
t1 = 4;
t2 = 8;
% allocate matrix
DHW_logi = zeros(size(DHW_Max));

for i = 1:size(DHW_Max,3)
    % check if the number of DHW exceed the threshold for every year in the
    % dataset
    DHW_logi(:,:,i) = DHW_Max(:,:,i)>=t1 & DHW_Max(:,:,i)<t2;
end

%% Calculate time since last bleaching event
%calculate year in which bleaching event happened (set by DHW_logi)
for i = 1:size(DHW_logi,3)
    DHW_year(:,:,i) = DHW_logi(:,:,i).*yr(i);
end
% get most recent date of event
% check: exclude year 2012??? -> if exlucding 2012 just remove 2012 matrix
DHW_date = max(DHW_year,[],3);

% for datapoints with no date set to NaNs
DHW_date(DHW_date==0) = nan;
% calculate time since last event
DHW_time = 2012 - DHW_date;

%% Calculate probability of event
%calculate average occurence
DHW_ave = sum(DHW_logi,3)/size(DHW_Max,3);
% calculate probability of an event
% following Puotinen et al. (draft): prob = 1-exp(-lambda)
% assuming poisson distribution
for j = 1:size(DHW_ave,1)
    for k = 1:size(DHW_ave,2)
        % calculate probability
        DHW_prob(j,k) = 1-exp(-DHW_ave(j,k));
    end
end
% set land values to NaNs
DHW_prob(find(land)) = nan;
%% calculate average intensity of DHW over last x years
% set threshold
th = (yr>=2002);
% calculate average intensity; excluding NaNs and 0's
DHW_mean = nansum(DHW_Max(:,:,th),3)./sum(DHW_Max(:,:,th)~=0 & ~isnan(DHW_Max(:,:,th)),3);
DHW_mean(find(land)) = nan;
%% write geotiffs
addpath('/Users/tadzio/Documents/UQProject/data/SST_DHW/Matlab/geotiffwrite_20100817')
geotiffwrite('DHW_prob_4-8.tif', bbox, DHW_prob, 32)
