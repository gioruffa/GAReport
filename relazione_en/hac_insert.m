sostituzioni := {
	e1 	-> xHAC[fEQ1]	,
	e2	-> xHAC[fEQ2]	,
	n	-> xHAC[fNOT]	,
	duw	-> xHAC[fDUW]	,
	dut	-> xHAC[fDUT]	,
	mt	-> xHAC[fMTT]	,
	ms	-> xHAC[fMTS]	
};

HAC[]:=Module[{randhead , sostitutiprova},
	randhead = RandomChoice[EQList];
	(*per esempio randhead = fEQ[e1,e2]*) 
	
	sostituitiprova = randhead /. sostituzioni ;
	(*sostituti prova diventa fEQ[xHAC[fEQ1],xHAC[fEQ2]]*) 

	(*a questo punto xHAC viene sostituita con HAC e *)
	(*la funzione viene effettivamente chiamata*) 
	(*l'espressione ritornata sara' del tipo *)
	(*fEQ[HAC[EQ1],HAC[EQ2]]*) 
	
	Return[ sostituitiprova /. { xHAC -> HAC} ];
];	

HAC[fathhead_] := Module [{args , sostituiti , sostitutiprova} ,

	(*controlla il numero di chiamate totale*) 
	If[	CurrHacCall >= MaxHacCall,
		Print["max hac calls raggiunto!"];
		Return[Null],
		Null
	];

	CurrHacCall ++;
	
	(*scegli un argomento compatibile con la head del padre*) 
	args = ChooseArg[fathhead];
	sostituitiprova = args /. sostituzioni ;
	Return[ sostituitiprova /. { xHAC -> HAC} ];
];
