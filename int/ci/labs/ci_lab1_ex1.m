P = [0 0 1 1; 0 1 0 1];
T = [0 1 1 1];
netu = newp([0 1; -2 2], 1);
netu.trainParam.epochs = 20;
resun = netu(P);
nett = train(netu, P, T);
restr = nett(P);
