TotalCrossFail = 0;
PartialCrossFail = 0;

CrossOver[{ind1_,ind2_}]:= Module[{	rami1,posrami1,ramo1,posramo1 ,
									pospadre1, headpadre1 ,listescelta1	,
									firstgood , secondgood,
									livelli2 , head2 , candidati1	,
									poscandidati , i,
									ramo2 , ramo2pos , ramo2padre ,
									ramo2padrepos , listascelta2 ,
									figlio1 , figlio2
									},
If[Random[]<PC,
 
	rami1 = Level[ind1,Infinity];

	(*voglio una lista ordinata a caso delle posizioni dei varirami*) 
	posrami1 = Table[Position[ind1 , rami1 [[l]] ] , {l,Length[rami1]}  ];
	posrami1 = Flatten[posrami1,1];
	posrami1 = Union[posrami1];
	posrami1 = RandomSample[posrami1];

	(*caratteri di controllo per vedere se c'e' un punto di taglio*)
	(*con dei candidati compatibili per lo scambio*)
	firstgood = False;
	secondgood = False;

	(* prova in sequenza le varie posizioni, finche' non ne trovi una *)
	(* compatiblie per tagliare *) 
	For[i=1 , i<=Length[posrami1] , i++ , 

		posramo1 = posrami1[[i]];
		ramo1 = Extract[ind1,posramo1];
		pospadre1 = Append[posramo1[[1;;-2]],0];
		headpadre1 = Extract[ind1,pospadre1];
		
		listascelta1 = ChooseList[headpadre1 , posramo1];

		livelli2 = Level[ind2,Infinity];
		head2 = Map[Head,livelli2];
		
		(* di tutte le sottoespressioni cerca quelle con una head compatibile *)
		(* con la head del ramo1*) 
		candidati1 = Position[	head2 , 
								x_ /; MemberQ[ Map[Head,listascelta1 ], x ]
		];
		
		If[	Length[candidati1] === 0	,
			firstgood = False;
				PartialCrossFail ++;
				(*se non ci sono candidati passa alla prossiama sottoespressione*) 
				Continue[] ,
			firstgood = True
		];
		
		(*a questo punto abbiamo almeno un candidati compatibile*) 
		(*analogamente a quanto fatto prima posiziono a random le*)
		(*posizioni dei candidati*) 
		levelcandidati = Extract[livelli2,candidati1];
		poscandidati = Table[Position[ind2,levelcandidati[[l]] ] ,{l,Length[levelcandidati]} ] ;
		poscandidati = Flatten[poscandidati , 1];
		poscandidati = Union[poscandidati];	
		poscandidati = RandomSample[poscandidati];

		(*trova il primo candidato compatible per lo scambio contrario*)
		For[j=1 , j<=Length[poscandidati] , j++ ,

			ramo2pos = poscandidati[[j]];
			ramo2 = Extract[ind2,ramo2pos];

			(*la pos del padre ha come ultimo valore uno zero*)
			ramo2padrepos = Append[ramo2pos[[1;;-2]],0];
			ramo2padre = Extract[ind2,ramo2padrepos];

			(*ora devi controlla se si piu' fare lo scambio al contrario*)
			(*controlla se ramo1 puo' essere come argomento di ramo2fath*)

			listascelta2 = ChooseList[ ramo2padre , ramo2pos  ];

			If[MemberQ[Map[Head , listascelta2]  ,  Head[ramo1] ] , 
				secondgood = True ;
					Break[]; ,
					PartialCrossFail ++;
					secondgood = False
			];
		
		]; 

		(*se hai trovato sia il punto di taglio che il candidato giusto*)
		(*esci dal ciclo*) 
		If[firstgood && secondgood , Break[] , Null];

	];(*FINE PRIMO FOR!*) 
	
	(************************************************************)
	(* 		ora crossa effettivamente							*)
	(************************************************************) 

	If[firstgood === False,Print["NESSUN PUNTO DI TAGLIO TROVATO"],Null];
	If[secondgood === False,Print["NESSUN CANDIDATO TROVATO"],Null];
	
	(*ReplacePart e' un comando molto comodo in quanto accetta l'output *)
	(*di position come argomento*) 
	If[ firstgood && secondgood ,
			figlio1 = ReplacePart[ind1 , posramo1 -> ramo2]; 
			figlio2 = ReplacePart[ind2 , ramo2pos -> ramo1]; ,
		Print["IL CROSS NON PUO' AVVENIRE"];
			figlio1 = ind1;
			figlio2 = ind2;
			TotalCrossFail ++;
	];

	
	Return[{figlio1,figlio2}];

,(*fine if random*) 

Return[{ind1,ind2}]
];

];


(************************************************************)
(************************************************************)
(************************************************************)



(*resituisci la lista di compatibilita' per una determinata head*)
(*la posizione del ramo figlio serve per capire se il ramo figlio*)
(*e' il primo o il secondo argomento di DU*) 
ChooseList[head_ , posramofiglio_ ]:= Module[{testhead , listascelta},

testhead = head;
	
	(*problema dut e duw*)
	(*devo vedere se e' il promo argomento o il secondo del DU*)
	If[ testhead === fDU ,
		If [ Last[posramofiglio] === 2 ,
				testhead = fDUT , 
				testhead = fDUW
		]; ,
		Null
	];
	Which[ 	testhead === fEQ,
			listascelta = EQList,
			
			testhead === fNOT,
			listascelta = NOTList,

			testhead === fDUT,
			listascelta = DUTList,

			testhead === fDUW,
			listascelta = DUWList,

			testhead === fMTT,
			listascelta = MTTList,

			testhead === fMTS,
			listascelta = MTSList,

			testhead === fCS,
			listascelta = CSList,

			testhead === fNN,
			listascelta = NNList,
			
			testhead === fTB,
			listascelta = TBList,

			True,
			Print["\n"];
			Print["Hey stai attento! ChooseArg non ha assegnato niete!"];
			Print["\n"];
	];
	Return[listascelta];
];


