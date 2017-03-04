<<moves.m
<<firstgen.m
<<crossover.m
<<tools.m
<<fitness.m


PM = 0.001;
PC = 0.7;
Nind = 300;
gen = FirstGeneration[];

gennumber = 40;
gens = {gen};

prova2[]:= For[incr=2,incr<gennumber,incr++,
				Print["GEN: " , incr];
				gens = 	Append[	gens,
								StepGen[ gens [[incr-1]] 
								] 
						]
];


run2[filename_String]:=Module[{},
	prova2[];
	PR[filename];
	PR[]
];

