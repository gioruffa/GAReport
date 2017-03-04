<<perfetto.m

(*resituisci una lista di coppie {fitness , individui con quella fiteess}*)
(*si puo' applicare a qualsiasi lista*) 
F2D[fitlist_] := Module[{sortfit, asd},
	sortfit = Sort[fitlist];
	asd = Table[ {Split[sortfit][[k]][[1]], 
	   Length[Split[sortfit][[k]]]}, {k, Length[Split[sortfit]]}];
	Return[asd];
];

(*analizza la singola generazione*)
(*richiede che sia abilitato il profiling su fith,leafh e deph*) 
GenGraphs[n_]:= Module[{fitnessg, fitdistg , leafg , depg},
	(*build the graph*) 
	fitnessg = ListPlot[fith[[n]] , 
						AxesLabel->{ "individui","fitness" }	,
						PlotLabel->"fitness generazione " + n
	];
	
	fitdistg = ListPlot[F2D[fith[[n]]] , 
						AxesLabel->{"individui","fitness"  }	,
						PlotLabel->"distribuzioe fitness generazione " + n
	];

	leafg = ListPlot[leafh[[n]] , 
						AxesLabel->{"individui","LeafCount" }	,
						PlotLabel->"leaves " +n
	];

	depg = ListPlot[leafh[[n]] , 
						AxesLabel->{"individui","Depth"}	,
						PlotLabel->"depth "+n
	];
	
	
	grid = {{fitnessg , fitdistg},{leafg,depg}};
	
	Show[GraphicsGrid[grid]]

];


(* brutto ma necessario*) 
Lol[media_,sigma_,a_]:= Module[{Mymax , Myfunc},
	Myfunc = PDF[NormalDistribution[media,sigma]];
	Mymax = Apply[Myfunc,{media}];
 	Return[Apply[Myfunc , {a}] / Mymax];
];


PlotTrends[]:= Module[{fit , leaf ,dep,dev},
	fit = ListPlot[meanh , PlotLabel->"mean fitness"];
	dev = ListPlot[devh , PlotLabel->"fitness stdvev"];
	leaf = ListPlot[meanleafh , PlotLabel->"mean leaves"];
	dep = ListPlot[meandeph , PlotLabel->"men depth"];

	grid = {{fit,dev},{leaf,dep}};

	Show[GraphicsGrid[grid]]
];

(*cerca nelle stack di tutte le generazioni se ci sono *) 
(*stack corrette *) 
good[]:= Module[{},
	Return[Position[stackdisth,TargetWord]];

];

(*data una generazione stampa valutazione elementi giusti*)
(*e salva gli individui giusti in "giusti" *) 
ProfileGeneration[n_]:=Module[{},
	If[Length[good[]]=!= 0 ,
		Print["primo vincitore a gen: " , Extract[good[],{1,1}]];
		(*lavora solo su ultima generazione*)
		posgiusti = Position[stackh[[n]],TargetWord];
		giusti = Extract[gens[[n]] , posgiusti];
		valgiusti = Map[EvalInd,giusti];
		Print[valgiusti];
	,
	Print["NESSUN VINCITORE"]
	];
];

(*restituisce {generazione,numerogiusti per generazione} *) 
popgiusti[]:= Module[{stackdist},
	posgood = good[];
	numpos = Map[Append[#1[[1 ;; -2]], 2] &, posgood];
	nums = Map[Extract[stackdisth,#1 ]&,numpos];
	giusgen = Map[Extract[#1,{1}]&,posgood];
	Return[Table[{giusgen[[k]] , nums[[k]]},{k,Length[giusgen]}]];

];

(*fai il profilo di una run*)
(*e salvalo su un file di nome filename*) 
ProfileRun[filename_String]:=Module[{prefix,finaldest,stream},
	prefix = "results/";
	finaldest = prefix<>filename;
	stream = OpenWrite[finaldest];
	

	WriteString[stream,"popolazione totale:\n"];
	Write[stream,Nind];

	WriteString[stream,"parametri:\n"];
	WriteString[stream,"{rlw,dsw,riw,bdw,stw,msl,dsl,mdl,ddl}\n"];
	Write[stream,{rlw,dsw,riw,bdw,stw,msl,dsl,mdl,ddl}];

	WriteString[stream,"popolazione di elementi perfetti: \n"];
	Write[stream,perfetti[]];

	WriteString[stream,"popolazione di elementi giusti: \n"];
	Write[stream,popgiusti[]];

	WriteString[stream,"numero runs:\n"]; 
	Write[stream,gennumber];
	
	WriteString[stream,"MAXDU:\n"]; 
	Write[stream,MAXDU];

	WriteString[stream,"MAXSTEP:\n"]; 
	Write[stream,MAXSTEP];

	WriteString[stream,"\n\n"]; 
	Write[stream,PlotTrends[]];

	Close[stream];
];

PR[filename_String]:=ProfileRun[filename];

(* fai il profilo della run e stampalo a video *) 
PR[]:= Module[{},
	If[Length[Position[gens,unperfetto]]=!=0,
		Print["ATTENZIONE INDIVIUDO PERFETTO TROVATO!!!"];
		Print["posizione:",Position[gens,unperfetto]] ,
		Null
	];
	Print["popolazione di elementi giusti"];
	Print[popgiusti[]];
	Print["parametri:"];
	Print["{rlw,dsw,riw,bdw,stw,msl,dsl,mdl,ddl}"];
	Print[{rlw,dsw,riw,bdw,stw,msl,dsl,mdl,ddl}];
	Print["numero runs:"]; 
	Print[gennumber];


	PlotTrends[]


];

(* salva individuo su un file*) 
salvaindividuo[ind_ , filename_String]:=Module[{prefix,finaldest,stream},
	prefix = "results/";
	finaldest = prefix<>filename;
	stream = OpenWrite[finaldest];
	Write[stream,ind];
	Close[stream];
];


(*restituisci coppie {generazione, numero perfetti}*) 
perfetti[]:=Module[{posperf,subs,toret,posgood,veryperf,splitted},

	posperf = Sort[Position[gens,unperfetto]];
	posgood = Position[posperf,x_/;Length[x] ===2];
	veryperf = Extract[posperf,posgood];
	splitted = Split[veryperf,#1[[1]] === #2[[1]] &];
	toret = {#[[1]][[1]], Length[#]} & /@ splitted;	Return[toret2]; 
	Return[toret]; 
];
