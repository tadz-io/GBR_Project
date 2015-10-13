% calculate median DHW
DHW_med = median(DHW_Max, 3);


% calculate probability of occurence
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
    DHW_logi(:,:,i) = DHW_Max(:,:,i)>t;
    % calculate weighted DHW = 1/(time+1)*DHW
    DHW_wMax(:,:,i) = DHW_Max(:,:,i).* (1/(yr(end)-yr(i)+1));
end

%now calculate weighted mean DHW
DHW_wMean = mean(DHW_wMax,3);

% calculate average occurence
DHW_ave = sum(DHW_logi,3)/size(DHW_Max,3);


% calculate return time of an event
% following Puotinen et al. (draft): return time = 1/(1-exp(-lambda))
for j = 1:size(DHW_ave,1)
    for k = 1:size(DHW_ave,2)
        % calculate expected return time
        DHW_rt(j,k) = 1/(1-exp(-DHW_ave(j,k)));
    end
end
