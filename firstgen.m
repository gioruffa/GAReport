Nind = 300;
MaxHacCall = 20;
CurrHacCall = 0;
MinLeaf = 3;
MaxLeaf = 20;

Dummies = {e , n , duw , dut , mt , ms};

sostituzioni := {
	e1 	-> xHAC[fEQ1]	,
	e2	-> xHAC[fEQ2]	,
	n	-> xHAC[fNOT]	,
	duw	-> xHAC[fDUW]	,
	dut	-> xHAC[fDUT]	,
	mt	-> xHAC[fMTT]	,
	ms	-> xHAC[fMTS]	
};

finalsost = {	fEQ 	:> 	EQ	,
				fNOT	:>	NOT	,
				fDU		:>	DU	,
				fMTT	:>	MTT	,
				fMTS	:>	MTS	,
				fNN		:>	NN	,
				fCS		:>	CS	,
				fTB		:>	TB	
};

EQList = {	fEQ[e1,e2] , fNOT[n] , fDU[duw,dut] , fMTT[mt] , fMTS[ms] , 	
			fCS[] , fTB[] , fNN[] };

NOTList = { fCS[] , fTB[] , fNN[] }; 

DUWList = {	fEQ[e1,e2] , fNOT[n] , fDU[duw,dut] , fMTT[mt] , fMTS[ms] , 	
			fCS[] , fTB[] , fNN[] };

DUTList = { fEQ[e1,e2] , fNOT[n] };

MTTList = {  fCS[] , fTB[] , fNN[] };

MTSList = {  fCS[] , fTB[] , fNN[] };

CSList = NNList = TBList = {};


ChooseArg[head_] := Module[{toret},

	Which[ 	head === fEQ1,
			toret = RandomChoice[EQList],
			
			head === fEQ2,
			toret = RandomChoice[EQList],
			
			head === fNOT,
			toret = RandomChoice[NOTList],

			head === fDUT,
			toret = RandomChoice[DUTList],

			head === fDUW,
			toret = RandomChoice[DUWList],

			head === fMTT,
			toret = RandomChoice[MTTList],

			head === fMTS,
			toret = RandomChoice[MTSList],

			head === fCS,
			toret = RandomChoice[CSList],

			head === fNN,
			toret = RandomChoice[NNList],
			
			head === fTB,
			toret = RandomChoice[TBList],

			True,
			Print["\n"];
			Print["Hey stai attento! ChooseArg non ha assegnato niete!"];
			Print["\n"];
	];
	Return[toret];
];


HAC[]:=Module[{randhead , sostitutiprova},
	randhead = RandomChoice[EQList];
	
	sostituitiprova = randhead /. sostituzioni ;
	(*
	Print ["sost prova" , sostituitiprova];
	*)
	Return[ sostituitiprova /. { xHAC -> HAC} ];

];	


HAC[fathhead_] := Module [{args , sostituiti , sostitutiprova} ,

	If[	CurrHacCall >= MaxHacCall,
		Print["max hac calls raggiunto!"];
		Return[Null],
		Null
	];

	CurrHacCall ++;

	args = ChooseArg[fathhead];
	sostituitiprova = args /. sostituzioni ;
	Return[ sostituitiprova /. { xHAC -> HAC} ];
];

RigthInd[] := Module[{ind},
	CurrHacCall = 0 ;
	ind = HAC[];
	(*finche c'e' Null rifai l'individuo*)
	While[	!FreeQ[ind,Null] 			|| 
			LeafCount[ind] < MinLeaf 	||  
			LeafCount[ind] > MaxLeaf 	, 	

			CurrHacCall =0;
			ind= HAC[];
	];
	
	Return[ind];
];


FirstGeneration[] := Table[RigthInd[] , {Nind}];

