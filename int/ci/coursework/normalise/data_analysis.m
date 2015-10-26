% Get some information about the values in the data provided:
% size of the files, range of values in the columns and means.

num_files = 6;
num_cols  = 17;
tst_sizes = [];
trn_sizes = [];
path      = '../third_party/';
for i = 1:num_files;
    trn_sizes(i, :) = size(load(strcat(path, int2str(i), 'trn.ssv')));
    tst_sizes(i, :) = size(load(strcat(path, int2str(i), 'tst.ssv')));
end;
fprintf('trn_sizes:\n');
disp(trn_sizes);
fprintf('tst_sizes:\n');
disp(tst_sizes);
fprintf('\n');
tst = [];
trn = [];
for i = 1:num_files;
    tst(i,:,:) = load(strcat(path, int2str(i), 'tst.ssv'));
    trn(i,:,:) = load(strcat(path, int2str(i), 'trn.ssv'));
end;
data_mins  = [];
data_maxs  = [];
data_means = [];
for i = 1:num_files;
    for j = 1:num_cols;
        data_means(i, j)             = mean(tst(i, :, j));
        data_means(num_files + i, j) = mean(trn(i, :, j));
        data_mins(i, j)              =  min(tst(i, :, j));
        data_mins(num_files + i, j)  =  min(trn(i, :, j));
        data_maxs(i, j)              =  max(tst(i, :, j));
        data_maxs(num_files + i, j)  =  max(trn(i, :, j));
    end;
end;
format compact;
fprintf('means:\n');
disp(data_means);
fprintf('mins:\n');
disp(data_mins);
fprintf('maximums:\n');
disp(data_maxs);

