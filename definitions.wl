(* ::Package:: *)

(* ------ Setup ------ *)
ClearAll[
	\[ScriptCapitalN], dim, \[Eta], \[DoubleStruckCapitalI],
	\[Omega], \[CapitalLambda], \[Omega]tx, \[Omega]ty, \[Omega]xy,
	e, \[CapitalNu], \[Beta], \[Nu]x, \[Nu]y,
	exx, eyy, exy,
	uf, u, g, hf, h,
	A, nc$, 
	mainVielInit,meanVielInit,vielbeinInit
];

(* ------ Basic objects ------ *)
\[ScriptCapitalN] = 3;   (* Number of metrics/vielbeins *)
dim = 3; (* Spacetime dimensions *)

\[Eta] = ({
	{-1, 0, 0},
	{0, 1, 0},
	{0, 0, 1}
});

\[DoubleStruckCapitalI] = IdentityMatrix[dim];

(* ------ Assumptions ------ *)
$Assumptions = And @@ Join[
	Flatten @ Table[{
		\[Omega]tx[J] \[Element] Reals, \[Omega]ty[J] \[Element] Reals, \[Omega]xy[J] \[Element] Reals,
		exx[J] \[Element] Reals, exy[J] \[Element] Reals, eyy[J] \[Element] Reals,
		\[Nu]x[J] \[Element] Reals, \[Nu]y[J] \[Element] Reals, \[CapitalNu][J] \[Element] Reals
	},{J, 1, \[ScriptCapitalN]}],
	Table[\[Beta][J] > 0, {J, 1, \[ScriptCapitalN]}]
];

(* ------ Vielbein creation ------ *)
(* Lorentz transformation parametrization *)
\[Omega][\[Omega]tx_,\[Omega]ty_,\[Omega]xy_] := ({
	{0, \[Omega]tx, \[Omega]ty},
	{-\[Omega]tx, 0, \[Omega]xy},
	{-\[Omega]ty, -\[Omega]xy, 0}
});
\[CapitalLambda][\[Omega]tx_,\[Omega]ty_,\[Omega]xy_] := Inverse @ (\[Eta]+\[Omega][\[Omega]tx, \[Omega]ty, \[Omega]xy]) . (\[Eta]-\[Omega][\[Omega]tx, \[Omega]ty, \[Omega]xy]);

mainVielInit[] := Block[{},
	(* NOTE: negative spatial vielbein to give positive shift control *)
	Quiet @ ClearAll[e];
	Do[
		e[J] = \[Beta][J] \[CapitalLambda][\[Omega]tx[J], \[Omega]ty[J], \[Omega]xy[J]] . ({
			{\[CapitalNu][J], 0, 0},
			{\[Nu]x[J], 1, 0},
			{\[Nu]y[J], 0, 1}
		}) . ({
			{1, 0, 0},
			{0, -exx[J], 0},
			{0, -exy[J], -eyy[J]}
		}) // Simplify,
	{J, \[ScriptCapitalN]}]
];

(* Arithmetic mean and reduced form *)
meanVielInit[] := Block[{},
	Quiet @ Clear[u];
	Do[
		u[J] = uf - e[J] // Simplify,
	{J, \[ScriptCapitalN]}]
];

vielbeinInit[] := Block[{},
	Quiet @ ClearAll[e, uf, u];
	mainVielInit[]; 
	uf := Sum[e[I], {I, 1, \[ScriptCapitalN]}];
	(*u[I_] := uf - e[I] // Simplify;*)
	(*meanVielInit[];*)
	u[I_] := Total[e /@ DeleteCases[Range[\[ScriptCapitalN]], I]];
];
vielbeinInit[]

(* ------ Bilinear form creator ------ *)
\[LeftAngleBracket]eI_?MatrixQ\[RightAngleBracket] := eI\[Transpose] . \[Eta] . eI;
\[LeftAngleBracket]I_?NumberQ\[RightAngleBracket] := e[I]\[Transpose] . \[Eta] . e[I];
\[LeftAngleBracket]eI_?MatrixQ, eJ_?MatrixQ\[RightAngleBracket] := eI\[Transpose] . \[Eta] . eJ;
\[LeftAngleBracket]I_?NumberQ, eJ_?MatrixQ\[RightAngleBracket] := e[I]\[Transpose] . \[Eta] . eJ;
\[LeftAngleBracket]eI_?MatrixQ, J_?NumberQ\[RightAngleBracket] := eI\[Transpose] . \[Eta] . e[J];
\[LeftAngleBracket]I_?NumberQ, J_?NumberQ\[RightAngleBracket] := e[I]\[Transpose] . \[Eta] . e[J];

(* ------ Geometric mean ------ *)
\[NumberSign][gI_?MatrixQ, gJ_?MatrixQ] := gI . MatrixPower[Inverse@gI . gJ, 1/2];
\[NumberSign][I_?NumberQ, gJ_?MatrixQ] := g[I] . MatrixPower[Inverse@g[I] . gJ, 1/2];
\[NumberSign][gI_?MatrixQ, J_?NumberQ] := gI . MatrixPower[Inverse@gI . g[J], 1/2];
\[NumberSign][I_?NumberQ, J_?NumberQ] := g[I] . MatrixPower[Inverse@g[I] . g[J], 1/2];

(* ------ Metrics ------ *)
g[eI_?MatrixQ] := \[LeftAngleBracket]eI\[RightAngleBracket];
g[I_?NumberQ] := \[LeftAngleBracket]I\[RightAngleBracket];
hf := \[LeftAngleBracket]uf\[RightAngleBracket];
h[eI_?MatrixQ] := \[LeftAngleBracket]u[eI]\[RightAngleBracket];
h[I_?NumberQ] := \[LeftAngleBracket]u[I]\[RightAngleBracket];

(* ------ Alt. notation ------ *)
Subscript[e, n_]:=e[n];
Subscript[u, n_]:=u[n];
Subscript[g, n_]:=g[n];
Subscript[h, n_]:=h[n];

(* ------ Symmetrization ------ *)
A[X_, Y_] := (\[LeftAngleBracket]X,Y\[RightAngleBracket]-\[LeftAngleBracket]Y,X\[RightAngleBracket]);

(* ------ Null cone equation ------ *)
nc$[met_]:=({{\[DoubleStruckCapitalT]t,\[DoubleStruckCapitalT]x,\[DoubleStruckCapitalT]y}} . met . {{\[DoubleStruckCapitalT]t,\[DoubleStruckCapitalT]x,\[DoubleStruckCapitalT]y}}\[Transpose])[[1,1]];
