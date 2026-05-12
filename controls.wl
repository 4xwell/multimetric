(* ::Package:: *)

ClearAll[
	nForms, nFormRows, nFormCols,
	defaultPairs, pairDefault, defaultShifts,
	vielLabel, formRows, formGrid, target,
	shiftOverlay, shiftOverlays, triadControls, 
	gaugeControls, overlapControl,
	zoomControl, tSliceControl
];

(* ------ Bilinear form selector ------ *)

nForms = 9; nFormRows = 3; nFormCols = Ceiling[nForms/nFormRows];

vielLabel := Association @ Join[
	{"None" -> Style["-", 10]},
	Table["e" <> ToString @ i -> Style[Subscript["e", i], 10], {i, \[ScriptCapitalN]}],
	{"u" -> Style["u", 10]},
	Table["u" <> ToString @ i -> Style[Subscript["u", i], 10], {i, \[ScriptCapitalN]}]
];

defaultPairs := Association @ Append[
	Table[{i -> {"e" <> ToString @ i, "e" <> ToString @ i}}, {i, \[ScriptCapitalN]}],
	{\[ScriptCapitalN]+1 -> {"u", "u"}}
];

pairDefault[i_] := Lookup[defaultPairs, i, {"None", "None"}];

formRows[i_] := Module[{defaults},
	defaults = pairDefault[i];
	If[!MatchQ[pair[i], {_String, _String}], pair[i] = defaults];
	Row[{
		Style[ToString @ i <> " ", Bold, 8],
		PopupMenu[
			Dynamic[pair[i][[1]],
				(pair[i] = ReplacePart[pair[i], 1 -> #]) & ],
			Normal @ vielLabel,
			Alignment -> {Center, Bottom},
			Appearance -> "Button",
			ContentPadding -> False,
			ImageSize -> {20, 18}],
			
		Style["\[Transpose] \[Eta] ", "SR", 8],
		
		PopupMenu[
			Dynamic[pair[i][[2]],
				(pair[i] = ReplacePart[pair[i], 2 -> #]) & ],
			Normal @ vielLabel,
			Alignment -> {Center, Bottom},
			Appearance -> "Button",
			ContentPadding -> False,
			ImageSize -> {20, 18}]
	}]
];

formGrid[] := Grid[
	Partition[
		Table[formRows[i], {i, nForms}],
		nFormCols
	], Alignment -> Left, Spacings -> {0.5, 1}
];

(* ------ Shift controls ------ *)

defaultShifts := Table[{Cos[(i-1) 2\[Pi]/\[ScriptCapitalN]], Sin[(i-1) 2\[Pi]/\[ScriptCapitalN]]}, {i, \[ScriptCapitalN]}];

target[i_] := Graphics[{
	Directive[colors[[i]], Opacity[0.6]], 
	Disk[{0,0}, Scaled[.06]],
	Directive[White, Opacity[1]], 
	Text[Style[Subsuperscript["\[Nu]", "xy", ToString @ i], Bold, 8]]
}];

shiftOverlay[i_] := Locator[
	Dynamic[
		{\[Nu]x[i], \[Nu]y[i]},
		({\[Nu]x[i], \[Nu]y[i]} = #) &
	], target[i]
];

shiftOverlays[] := Graphics @ Table[shiftOverlay[i], {i, \[ScriptCapitalN]}]

(*shiftOverlay[] := Graphics @ Table[
	With[{j = i},
		Locator[
			Dynamic[
				{\[Nu]x[j], \[Nu]y[j]},
				({\[Nu]x[j], \[Nu]y[j]} = #) &
			], target[j]]
	], {i,\[ScriptCapitalN]}
];*)

(* ------ Overlap controls ------ *)

overlapControl[] := Labeled[Control[{
	{modes, {}, ""},
	Thread[Range[nForms] -> Range[nForms]],
	ControlType -> TogglerBar,
	Appearance -> "Horizontal",
	Enabled -> Dynamic[plotDim==="2D"],
	Background -> colors
	}], "Overlap", Top, {}
];

(* ------ Gauge slider controls ------ *)

gaugeControls[sym_, i_, lo_, hi_] := Labeled[
	HorizontalGauge[
		Dynamic[sym[i]],
		{lo, hi},
		PlotTheme -> "Monochrome",
		Frame -> False,
		GaugeMarkers -> Placed["BarMarker", "Center"],
		ImageSize -> 175,
		ImageMargins -> 0,
		ScalePadding -> 0
	],
	Subscript[ToString @ sym, i], Left
];

zoomControl[] := Labeled[HorizontalGauge[ (*Zoom slider*)
	Dynamic @ zoom, {0.5, 3},
	PlotTheme -> "Monochrome",
	GaugeMarkers -> Placed["BarMarker", "Center"],
	Frame -> False, ScalePadding -> 0,
	ImageSize -> 165, ImageMargins -> 0
], "Zoom", Left];

tSliceControl[] := Labeled[HorizontalGauge[ (*t-slice slider*)
	Dynamic @ tSlice, {-3, 3},
	PlotTheme -> "Monochrome",
	GaugeMarkers -> Placed["BarMarker", "Center"],
	Frame -> False, ScalePadding -> 0,
	ImageSize -> 160, ImageMargins -> 0
], "t-slice", Left];

(* ------ Spatial vielbein controls ------ *)

triadControls[i_] := Labeled[
	(* NOTE: reversed min/max for more intuitive controls *)
	Column[{
		Slider2D[
			Dynamic[
				{exx[i], eyy[i]},
				({exx[i], eyy[i]} = #) &
			],
			{{1.5, 1.5}, {0.5, 0.5}},
			ImageSize -> Small
		],
	
		Slider[
			Dynamic[exy[i]],
			{1.5, -1.5},
			ImageSize -> 60
		]	
	}, Spacings -> 0.1],
	Subsuperscript["E", "ij", i], Top
];
