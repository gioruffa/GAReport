(************************************************************)
(*				VALUTAZIONE 								*)
(************************************************************) 

finalsost = {	fEQ 	:> 	EQ	,
				fNOT	:>	NOT	,
				fDU		:>	DU	,
				fMTT	:>	MTT	,
				fMTS	:>	MTS	,
				fNN		:>	NN	,
				fCS		:>	CS	,
				fTB		:>	TB	
};

DefaultStack = {u,n,i,v} ;
DefaultTable = {e,r,s,a,l};

EvalInd[ind_] := Block[{ 	TargetWord = {l,a,s,r,e,v,i,n,u},
							MyStack = DefaultStack,
							MyTable = DefaultTable,
							BrokenDU = 0 ,
							StepCount = 0,
							MAXDU = 5,
							newind,
							rich = 0
					} ,
	
	newind = ind /. finalsost;
	rich = Richness[ind];

	Return[{MyStack , MyTable , BrokenDU, StepCount , rich}];
];

(************************************************************)
(*********  		P U N T E G G I          ****************)
(************************************************************)

easysubs = { l :> 1 , a :> 2 , s:> 3 , e:>4 ,r:> 5 , v:>6 , i:> 7 , n:>8 , u:>9	};

exprslist= {fEQ , fNOT , fDU , fMTT , fMTS , fCS ,fNN , fTB };

(*conta il numero di lettere che si trovano nella posizione corretta*)

RigthLetters[stack_] := Module[{newstack , score},
	newstack = stack /. easysubs;
	score =Count[ 	Table[newstack[[i]] === i , {i,Length[newstack]}   ],
					True];
	Return[score];
];

(* conta quanto lo stack sia cambiato sia per differenza tra singole
	lettere che per differenza di lunghezza *)

DidSome[stack_] := Module[{score},
	(* guarda se ha cambiato la DefaultStack *)
	score =Count[ 	Table[	stack[[i]] =!= DefaultStack[[i]] , 
							{i,Length[newstack]}   ],
					True];
	score += Abs[Length[stack]-Length[DefaultStack]];
	Return[score];
];

(* conta la ricchezza di espressioni nell'individuo *)

Richness[ind_] := Module[{score, pos},
	score = Count [ Table[ 	FreeQ[ind,exprslist[[i]]] , 
							{i,Length[exprslist] } ] 	,
					False
	];
	Return[score];
];

(************************************************************)

FitnessParameters[evalres_] := Module[{rl,ds,ri,bd,st},
	(*valuta l'individuo e valuta risultato*)
(*	evalres = EvalInd[ind];*)
	
	rl = RigthLetters[evalres[[1]]];
	ds = DidSome[evalres[[1]]];
	ri = evalres[[5]];(* la richness viene calcolata in EvalInd*) 
	bd = evalres[[3]];(* broken du*)
	st = evalres[[4]];(* numero di step*)

	Return[{rl,ds,ri,bd,st}];
];


ProvaFit[params_]:=Module[{toret},

	(*pesi dei singoli punteggi : *)
	(*i pesi dei punteggi rappresentano l'"ambiente"*)
	rlw	=	1000;	(*right letter weight*)
	dsw	=	1;		(*do something weight*)
	riw	=	5;		(*richness weight*) 
	bdw	=	10;		(*broken DU weight*) 
	stw	=	10;		(*step weight*) 

	msl = 0;	(*mean step lol*) 
	dsl = 2;	(*delta step lol*) 
	mdl = 13;	(*mean broken du lol*) 
	ddl = 5;	(*delta broken du lol*) 

	MAXSTEP = 30;
	(*penalizza fortemente i programmi troppo lunghi*) 
	(*
	If[	params[[4]] > MAXDU || params[[5]] >= MAXSTEP,
		toret=	0	,
		toret=	params[[1]] rlw + 
				params[[2]] dsw + 
				params[[3]] riw +
				Lol[msl,dsl,params[[4]]] * 1.0  bdw + 
				Lol[mdl,ddl,params[[5]]]  * 1.0 stw ;  
	];
	*)
	toret=	params[[1]] rlw + 
				params[[2]] dsw + 
				params[[3]] riw +
				params[[4]] * 1.0  bdw + 
				params[[5]]  * 1.0 stw ; 
	Return[toret];
	

];

GenCoppie[gen_List,normfitlist_List]:= Module[{coppie},	
	(*supponendo che sia normalizzata*)
	coppie =Table[RandomChoice[(normfitlist * 1.0) -> gen , 2] ,{Nind/2}];
	Return[coppie];
];


Muta[ind_]:=Module[{},
	(* get all the subexpressions (our bits)*)
	subs = Level[ind,Infinity];
	positions = Table[Position[ind , subs[[i]]], {i, Length[subs]}];
	positions = Flatten[positions,1];
	(*togli dupplicati*)
	positions = Union[positions];
	(*muta*)
	(*Print[positions];*)
	Map[ChangeHead[ind ,#1]&  ,positions];
];


ChangeHead[ind_,posexpr_] := Module[{posfath , fathead , compheadlist , comphead , replpos },
	If[	Random[] < PM , 
			(* get fath head*)
			expr = Extract[ind,posexpr];
			posfath  = Append[posexpr[[1;;-2]],0];
			fathed = Extract[ind,posfath];

			(* get compatible expr*)
			compheadlist = Map[Head,ChooseList[fathed , posexpr]];
			(* nella lista potrebbe esserci la stessa espressione*)
			compheadlist = Delete[	compheadlist , 
									Flatten[Position[compheadlist,
													 Head[expr]] ,
											1]
			];
			comphead = RandomChoice[compheadlist];
			(* need the position of the "son head"*)
			If[Last[posexpr] =!= 0 ,
				replpos = Append[ posexpr  ,0],
				replpos = posexpr
			];
			(* e' buggato, bisogna controllare anche se il figlio e' compatibile *)
			ind = ReplacePart[ind,replpos->comphead];	
		Null
	];
		
];

(* histories*)
parh={};
fith={};
normh={};
stackh = {};
stackdisth={};
devh={};
meanh = {};
meanleafh = {};
meandeph = {};
leafh = {};
deph = {};

Profiler[ingen_,fitlist_,params_,valutazioni_]:=Module[{stack},
	Print["\n PROFILING BEGIN\n"];

	(*parh = Append[parh,params];*)
	
	stacks = Table[valutazioni[[j]] [[1]] , {j,Nind}] ;
	stackdisth = Append[stackdisth , F2D[stacks]];
	stackh = Append[stackh , stacks];

	fith = Append[fith,fitlist];
	
	(* calculate std dev e media*)
	devh = Append[devh, StandardDeviation[ fitlist ]];
	meanh = Append[meanh, Mean[ fitlist ]];

	meanleafh = Append[meanleafh, Mean[Map[LeafCount,ingen]] ];
	(*leafh = Append[leafh, Map[LeafCount,ingen]] ;*)

	meandeph = Append[meandeph, Mean[Map[Depth,ingen]] ];
	(*deph = Append[deph, Map[Depth,ingen] ];*)
		
	Print["\n PROFILING END \n"];


];


StepGen[ingen_List] := Module[{ params , fitlist , normfitlist , coppie , outgen },
	Print["\n STEP BEGIN\n"];
		
	(* qui avviene la vautazione degli individui*)
	Print["valuto"];
	valutazioni = Map[EvalInd,ingen];

	Print["parametrizzo"];
	params = Map[FitnessParameters,valutazioni];

	Print["fitness"];
	fitlist = Map[ProvaFit , params];
	If[Min[fitlist] < 0 , fitlist -=  Min[fitlist] , Null];

	Print["profilo"];
	Profiler[ingen,fitlist,params,valutazioni];

	normfitlist = fitlist / Apply[Plus,fitlist];

	Print["crossover"];
	coppie = GenCoppie[ingen,normfitlist];
	outgen = Map[CrossOver,coppie];
	outgen = Flatten[outgen,1];
	
	Print["\n STEP END\n"];

	Return[outgen];
];

