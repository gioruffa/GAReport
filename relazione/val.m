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
