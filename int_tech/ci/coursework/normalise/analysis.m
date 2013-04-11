num_files = 6;
num_cols = 17;
tst_sizes = [];
trn_sizes = [];
for i = 1:num_files;
    tst_sizes(i, :) = size(load(strcat(int2str(i), 'tst.norm.ssv')));
    trn_sizes(i, :) = size(load(strcat(int2str(i), 'trn.norm.ssv')));
end

tst = [];
trn = [];
for i = 1:num_files;
    tst(i,:,:) = load(strcat(int2str(i), 'tst.norm.ssv'));
    trn(i,:,:) = load(strcat(int2str(i), 'trn.norm.ssv'));
end;

data_mins = [];
data_maxs = [];
data_means = [];
for i = 1:num_files;
    for j = 1:num_cols;
        data_means(i, j)             = mean(tst(i, :, j));
        data_means(num_files + i, j) = mean(trn(i, :, j));
        data_mins(i, j)              = min(tst(i, :, j));
        data_mins(num_files + i, j)  = min(trn(i, :, j));
        data_maxs(i, j)              = max(tst(i, :, j));
        data_maxs(num_files + i, j)  = max(trn(i, :, j));
    end
end
