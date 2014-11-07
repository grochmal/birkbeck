X = [-1 -1; -1 1; 1 -1; 1 1];
Y = [-1; -1; 1; -1];
W = X' * Y;
disp(W);
hardlims(X*W - 1)
