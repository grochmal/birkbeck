Ecoli = load('ecoli_mod');
[rsize csise] = size(Ecoli);
Recoli = Ecoli(randperm(rsize), :);
TrnRecoli = Recoli([1:(rsize*3/4)],:);
TstRecoli = Recoli([(rsize*3/4 + 1):rsize],:);

%kNN
k = 7;
[rtrn ctrn] = size(TrnRecoli);
[rtst ctst] = size(TstRecoli);
dists = dist(TstRecoli, TrnRecoli');
tmp = zeros(ctrn, rtrn);
idx = zeros(ctrn, rtrn);
for i = 1:rtst
    [tmp(i,:) idx(i,:)] = sort(dists(i,:));
end
Lambda = Recoli';
nnlabels = idx(:,1:k);
for r = 1:rtst
    for c = 1:7
        label(r, c) = Lambda(8, nnlabels(r, c));
    end
end
