A = 0.7:0.5:3;
B = 1:15;
C = 8:2:15;
B(1) = []; B(length(B)) = [];
idx = 1;
if 0 == rem(B(1), 2)
    idx = 2;
end
B(idx:2:length(B)) = -1;
B
