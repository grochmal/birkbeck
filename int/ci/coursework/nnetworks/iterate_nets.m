% Run a set of 100 networks (run_experiments) over all needed iterations

function hdlst = iterate_nets(nfiles, trns, dgtrns, tst, dgtst)
    hdlst = 5:5:55;
    for i = nfiles
	for h = hdlst
	    fprintf('Run nets for frame %i with %i hidden nodes\n', i, h);
	    res = run_experiments(i, h, trns, dgtrns, tst, dgtst);
	end
    end
end

