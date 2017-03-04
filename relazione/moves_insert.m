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

MTT[x_] := Module[{},
	StepCount ++;
	If[ Length[MyStack] === 0 , Return[x] , Null];

	If[ Last[MyStack] === x	,
		(* togli l'ultimo e solo l'ultimo *)
		MyStack = Delete[MyStack , Length[MyStack]];
			MyTable = Append[MyTable , x] ;
			Return[x] 	,
		(* se non e' l'ultimo della stack non fare niente*)
		Return[x] ;
	];
];

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
