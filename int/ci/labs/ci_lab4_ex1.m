% it is all contained in the GA pakcage

clear all;
load inputs;
load targets;
load phen;
load fam_data_twin;
load gen_input;
load gen_output;
P = inputs;
T = targets;
P = P';
T = T';
gen_input = gen_input';
gen_output = gen_output';
for members=1:10
    pt_in = cell2mat(fam_data_twin(members, 1));
    pt_out = cell2mat(fam_data_twin(members, 2));
    pt_in = pt_in';
    pt_out = pt_out';
    ffnet = newff2(pt_in, pt_out, (roundoff(phen(members, 1), 0)));
    ffnet.trainParam.lr = phen(members, 2);
    ffnet.trainParam.mc = phen(members, 3);
    ffnet.trainParam.goal = 1e-5;
    ffnet.trainParam.epochs = 10;
    ffnet.PerformFcn = 'msereg';
    ffnet.PerformParam.ratio = phen(members, 5);
    [ffnett, tr] = train(ffnet, pt_in, pt_out);
    act_out = sim(ffnett, P);
    error = act_out - T;
    act_gen_out = sim(ffnett, gen_input);
    error_gen = act_gen_out - gen_output;
    PTnets{members} = ffnett;
    PTnetworks{members, 1} = tr;
    PTnetworks{members, 2} = act_out;
    PTnetworks{members, 3} = error;
    PTnetworks{members, 4} = act_gen_out;
    PTnetworks{members, 5} = error_gen;
    save PTnets;
    save PTnetworks;
end
open PTnetworks;
