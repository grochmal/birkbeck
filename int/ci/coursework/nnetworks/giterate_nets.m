% iterate_nets version for good_net.m

function hdlst = giterate_nets(nfiles, trns, dgtrns, tst, dgtst)
    hdlst = 5:5:55;
    for h = hdlst
	fprintf('Run nets for frame %i with %i hidden nodes\n', nfiles, h);
	res = run_experiments(nfiles, h, trns, dgtrns, tst, dgtst);
    end
end

