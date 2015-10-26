nind = 50;
nvar = 10;
lind = 10;
xov  = 0.7;
mutr = 0.01;
ggap = 0.9;
gen  = 0;
maxg = 100;
FieldD = [ lind lind lind lind lind lind lind lind lind lind
         ; 0    0    0    0    0    0    0    0    0    0
         ; 100  100  100  100  100  100  100  100  100  100
         ; 1    1    1    1    1    1    1    1    1    1
         ; 0    0    0    0    0    0    0    0    0    0
         ; 1    1    1    1    1    1    1    1    1    1
         ; 1    1    1    1    1    1    1    1    1    1
         ];
% everything below needs the GA toolbox
Chrom = crtbp(nind, lind*nvar);
Phen1 = bs2rv(chrom, FieldD);
