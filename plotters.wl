(* ::Package:: *)

ClearAll[
	colors, contourColors,coneColors,
	plotCones2D, plotOverlap2D,
	plotCones3D, imgSize
];

colors = {
	Blue,
	Red,
	Darker@Green,
	Purple,
	Orange,
	Magenta,
	Darker@Yellow,
	Cyan,
	Green
};

contourColors = Join[
	Directive[#, Dashing[{}], Opacity[1]] & /@ colors[[1 ;; 3]],
	Directive[#, Dashed, Opacity[1]] & /@ colors[[4 ;; 6]],
	Directive[#, DotDashed, Opacity[1]] & /@ colors[[7 ;; All]]
];

overlapColors = Directive[#, Opacity[0.3]] & /@ colors;
coneColors = Directive[#, Opacity[0.4]] & /@ colors;

imgSize = 715;

plotCones2D[eqs_, zoom_, plotText_, labels_:{}, crossing_:False, EoMcheck_:True] := 
	Module[{styles, legend, funcs, meshF, bgc},
	styles = Take[contourColors, UpTo[Length[eqs]]];
	legend = If[labels === {}, None,
		Placed[LineLegend[styles, labels], {Right, Top}]];
	funcs = eqs[[All,1]];
	meshF = If[TrueQ[crossing],(Function[{\[DoubleStruckCapitalT]x,\[DoubleStruckCapitalT]y},#]&)/@funcs,{}];
	bgc = If[EoMcheck===True, Directive[White, Opacity[0]], Directive[Red, Opacity[0.2]]];

	ContourPlot[
		Evaluate@eqs,
		{\[DoubleStruckCapitalT]x, -zoom, zoom}, {\[DoubleStruckCapitalT]y, -zoom, zoom}, 
		PlotPoints -> ControlActive[15, 50],
		MaxRecursion -> 1,
		PerformanceGoal -> "Speed",
		Method -> {"SymbolicProcessing" -> 0},
		ContourStyle -> styles,
		Background -> bgc,
		PlotLegends -> legend,
		MeshFunctions -> meshF,
		Mesh -> {{0}},
		MeshStyle -> Directive[PointSize[Large],Magenta,Opacity[0.8]],
		ImageSize -> imgSize,
		ImageMargins -> 0,
		ImagePadding -> None,
		Frame -> False,
		Axes -> True,
		Prolog -> Inset[plotText, {-zoom, zoom}, {Left, Top}]
	]
];

plotOverlap2D[selected_List, slotConds_List, zoom_, showArea_:False] := Module[
  {validSel, regionCond, area, plotExpr},

  validSel = Select[
    selected,
    IntegerQ[#] &&
    1 <= # <= Length[slotConds] &&
    slotConds[[#]] =!= None &
  ];

  If[validSel === {}, Return[{}]];

  regionCond = And @@ slotConds[[validSel]];

  area = If[
    TrueQ[showArea],
    Quiet @ NIntegrate[
      Boole[regionCond],
      {\[DoubleStruckCapitalT]x, -zoom, zoom},
      {\[DoubleStruckCapitalT]y, -zoom, zoom},
      Method -> "QuasiMonteCarlo",
      PrecisionGoal -> 2,
      AccuracyGoal -> 2
    ], None
  ];

  plotExpr = If[
    area === None,
    regionCond,
    Callout[
      regionCond,
      "Area: " <> ToString[Round[area, 0.001]],
      Above,
      Appearance -> "Leader",
      LabelStyle -> {FontSize -> 10, Bold},
      CalloutStyle -> Directive[Black, Dashing[{}]],
      Background -> Directive[White, Opacity[1]]
    ]
  ];

  First @ RegionPlot[
    Evaluate @ plotExpr,
    {\[DoubleStruckCapitalT]x, -zoom, zoom},
    {\[DoubleStruckCapitalT]y, -zoom, zoom},
    PlotPoints -> ControlActive[15, 50],
    PlotStyle -> Directive[Magenta, Opacity[0.25]],
    BoundaryStyle -> None,
    Frame -> False,
    ImageSize -> imgSize
  ]
];

plotCones3D[eqs_, zoom_:1, plotText_:"", labels_:{}] := Module[
	{styles, legend},
	styles = Take[coneColors, UpTo[Length[eqs]]];
	legend = If[labels === {}, None, Placed[labels, {Right, Bottom}]];
	
	ContourPlot3D[
		Evaluate[eqs],
		{\[DoubleStruckCapitalT]t, -zoom, zoom},{\[DoubleStruckCapitalT]x, -zoom, zoom},{\[DoubleStruckCapitalT]y, -zoom, zoom},
		ContourStyle -> styles,
		Boxed -> False,
		Axes -> True,
		AxesOrigin->{0,0,0},
		AxesLabel -> {"t", "x", "y"},
		AxesStyle -> Italic,
		Mesh -> None,
		ViewPoint -> {0.5, 0, -1},
		ViewProjection -> "Orthographic",
		RegionBoundaryStyle->None,
		RegionFunction->Function[{t,x,y},x^2+y^2+t^2<=0.98zoom],
		MaxRecursion->2,
		PerformanceGoal -> "Speed",
		Method -> {"SymbolicProcessing" -> 0},
		ImageSize -> imgSize,
		PlotPoints -> ControlActive[15, 50],
		PlotRange -> {{-zoom, zoom}, {-zoom, zoom}, {-zoom, zoom}}
	]
];
