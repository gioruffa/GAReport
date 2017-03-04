TargetWord = {l,a,s,r,e,v,i,n,u};
MyStack = {};
MyTable = {};

LispNil = "lisp nil";
maxdustep = 2 * Length[TargetWord] ;
BrokenDU = 0;
StepCount = 0;
MAXDU = 5;

RandomInit[] := Module[{cutpos , permutations , tocut},
	cutpos = Random[Integer, {1, Length[TargetWord]}];
	(*get random permutation*)
	permutations = Permutations[TargetWord];
	tocut = permutations[[ 
				Random[Integer, {0,Length[permutations]} ] 
			]];
	MyStack = Take[tocut,cutpos];
	MyTable = Take[tocut,-(Length[tocut]-cutpos)];

	Print["starting stack and table:" , {MyStack , MyTable}];
	
];

CS[] := If[Length[MyStack] === 0,Return[LispNil] , Return[Last[MyStack]]];

TB[] := Module[{i=0, c , tab , first , splitted} , 
		
		tab = 	Table[ MyStack[[i]] === TargetWord[[i]] , 
						{i,Length[MyStack] }
				];		
		
		splitted = Split[tab];
		If[	Length[splitted ] === 0,
			Return[LispNil] 	,
			first = splitted[[1]];
		]; 
		
		c = If[MemberQ[first,True] , 
				Length[first],
				0
			];

		If[ c === 0,
			Return[LispNil]	,
			Return[MyStack[[c]]]
		];
];

NN[] := Module[{i=0 , toret , c , tab ,first , splitted},

		tab = 	Table[ MyStack[[i]] === TargetWord[[i]] , 
						{i,Length[MyStack] }
				];		
		
		splitted = Split[tab];
		If[	Length[splitted ] === 0,
			Return[TargetWord[[1]]] 	,
			first = splitted[[1]];
		];

		c = If[MemberQ[first,True] , 
				Length[first],
				0
			];
		
		If[	c===0,
			Return[TargetWord[[1]]],
			Null
		];
		
		If[ c===Length[TargetWord]	,
			Return[LispNil],
			Return[TargetWord[[c+1]]]
		];
];


(************************************************************)
(************************************************************)
(************************************************************)


MTS[x_] := Module[{},
	StepCount ++;
	If[ MemberQ[MyTable,x]	,
		(* potrebbe non funzionare con le lettere doppie*)
		(*Print["stack table" , {MyStack , MyTable}];*)
		MyTable = DeleteCases[MyTable , x];
			MyStack = Append[MyStack , x] ;
			Return[x]	,
		(* se non c'e' sul tavolo non fare niente*)
		Return[x]
	];
];


MTT[x_] := Module[{},
	StepCount ++;
	If[ Length[MyStack] === 0 , Return[x] , Null];

	If[ Last[MyStack] === x	,
		(* togli l'ultimo e solo l'ultimo *)
		(*Print["stack table" , {MyStack , MyTable}];*)
		MyStack = Delete[MyStack , Length[MyStack]];
			MyTable = Append[MyTable , x] ;
			Return[x] 	,

		(* se non e' l'ultimo della stack non fare niente*)
		Return[x] ;
	];
];


(************************************************************)
(************************************************************)
(************************************************************)


NOT[expr_] := If[expr === LispNil , True , False];

EQ[expr1_ , expr2_] := Module [ {} ,Return[expr1===expr2] ];



DU[work_, test_] :=	Module[{tmp,counter = 0},
	If[BrokenDU >= MAXDU,
		Return[Null],
		Null
	];
	While[	test ===False  , 
		(*forza valutazione con assegnazione*)
		tmp = work ;
		counter ++ ;
		If[counter -1 === maxdustep ,
			BrokenDU ++;
			Return[LispNil] ,
			Null
		];
	];
];

SetAttributes[DU,HoldAll];


