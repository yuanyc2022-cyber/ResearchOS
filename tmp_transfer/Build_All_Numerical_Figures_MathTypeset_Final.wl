(* ::Title:: *)
(* Standalone publication build for AIGC numerical figures. *)

ClearAll["Global`*"];

(* Exact model definitions *)
pH[alpha_, q_, theta_, t_: 1] := (((1 - theta) + alpha (theta + 3)) q + (5 - 3 alpha) t)/(5 (1 + alpha));
pA[alpha_, q_, theta_, t_: 1] := ((2 (theta - 1) + alpha (3 theta - 1)) q + (5 + alpha) t)/(5 (1 + alpha));
DH[alpha_, q_, theta_, t_: 1] := (alpha + 2) pH[alpha, q, theta, t]/(6 t);
DA[alpha_, q_, theta_, t_: 1] := (1 - alpha) pA[alpha, q, theta, t]/(3 t);
piH[alpha_, q_, theta_, t_: 1] := pH[alpha, q, theta, t] DH[alpha, q, theta, t];
piA[alpha_, q_, theta_, t_: 1] := pA[alpha, q, theta, t] DA[alpha, q, theta, t];
humanRevenue[alpha_, q_, theta_, t_: 1] := 2 piH[alpha, q, theta, t];
aiRevenue[alpha_, q_, theta_, t_: 1] := piA[alpha, q, theta, t];
transactionRevenue[alpha_, q_, theta_, t_: 1] := humanRevenue[alpha, q, theta, t] + aiRevenue[alpha, q, theta, t];
qH[alpha_, theta_, t_: 1] := (3 alpha^2 + 14 alpha + 27) t/(theta (alpha^2 + 4 alpha + 7) + 3 alpha^2 + 8 alpha + 9);
platformProfit[alpha_, q_, theta_, rho_, g_, kappa_, t_: 1] := rho transactionRevenue[alpha, q, theta, t] + g alpha - kappa alpha^2/2;
revenuePrime[alpha_, q_, theta_, t_: 1] := Evaluate[D[transactionRevenue[z, q, theta, t], z] /. z -> alpha];
alphaStar[g_?NumericQ, q_?NumericQ, theta_?NumericQ, rho_?NumericQ, kappa_?NumericQ, alphaBar_?NumericQ, t_: 1] := Module[{lo, hi, root},
  lo = -rho revenuePrime[0, q, theta, t];
  hi = kappa alphaBar - rho revenuePrime[alphaBar, q, theta, t];
  Which[g <= lo, 0., g >= hi, N[alphaBar], True,
    root = Quiet[alpha /. FindRoot[rho revenuePrime[alpha, q, theta, t] + g - kappa alpha == 0, {alpha, N[alphaBar/2, 30]}, WorkingPrecision -> 30, AccuracyGoal -> 16, PrecisionGoal -> 16]];
    Clip[N[root], {0., N[alphaBar]}]
  ]
];

(* Publication style *)
fontFamily = "Times New Roman";
navy = RGBColor[0.13, 0.32, 0.52];
amber = RGBColor[0.82, 0.45, 0.10];
teal = RGBColor[0.13, 0.50, 0.43];
midGray = GrayLevel[0.35];
gridGray = GrayLevel[0.90];
baseStyle = Directive[FontFamily -> fontFamily, FontSize -> 9, Black];
labelStyle = Directive[FontFamily -> fontFamily, FontSize -> 10, Black];
panelStyle = Directive[FontFamily -> fontFamily, FontSize -> 10, Bold, Black];
frameStyle = Directive[Black, AbsoluteThickness[0.8]];
gridStyle = Directive[gridGray, AbsoluteThickness[0.45]];
humanStyle = Directive[navy, AbsoluteThickness[2.2]];
aiStyle = Directive[amber, AbsoluteThickness[2.0], AbsoluteDashing[{5.0, 2.2}]];
lowStyle = Directive[midGray, AbsoluteThickness[1.9], Dotted];
midStyle = Directive[amber, AbsoluteThickness[2.0], AbsoluteDashing[{5.0, 2.2}]];
highStyle = Directive[navy, AbsoluteThickness[2.2]];
commonPlotOptions = {Frame -> True, Axes -> False, FrameStyle -> frameStyle, FrameTicks -> {{Automatic, None}, {Automatic, None}}, FrameTicksStyle -> baseStyle, LabelStyle -> labelStyle, BaseStyle -> baseStyle, PlotRangePadding -> None, PlotRangeClipping -> True, ImagePadding -> {{50, 8}, {36, 8}}, AspectRatio -> 0.82, PlotPoints -> 240, MaxRecursion -> 5, Exclusions -> None, PerformanceGoal -> "Quality", GridLinesStyle -> gridStyle, ImageSize -> 335};

(* Native Wolfram box objects provide true subscripts/superscripts; no fake underscores or asterisks. *)
mathLabel[box_, size_: 9, color_: Black] := Style[RawBoxes[FormBox[box, TraditionalForm]], size, color, FontFamily -> fontFamily];
mathFunction[sym_, arg_] := RowBox[{sym, "(", arg, ")"}];
mathSubStar[sym_, sub_] := SubsuperscriptBox[sym, sub, "*"];
alphaBox = "\[Alpha]";
thetaBox = "\[Theta]";
rhoBox = "\[Rho]";
alphaStarBox = SuperscriptBox[alphaBox, "*"];
alphaBarBox = OverscriptBox[alphaBox, "\[OverBar]"];
alphaHBox = SubscriptBox[alphaBox, "H"];
piHBox = SubsuperscriptBox["\[Pi]", "H", "*"];
piABox = SubsuperscriptBox["\[Pi]", "A", "*"];
PiPBox = SubscriptBox["\[CapitalPi]", "P"];
qOverTLabel[value_, size_: 8.8, color_: Black] := mathLabel[RowBox[{"q", "/", "t", "=", ToString[NumberForm[N[value], {4, 3}, NumberPadding -> {"", "0"}]]}], size, color];
thetaValueLabel[value_, size_: 8.8, color_: Black] := mathLabel[RowBox[{thetaBox, "=", ToString[NumberForm[N[value], {3, 2}, NumberPadding -> {"", "0"}]]}], size, color];
alphaValueLabel[symbolBox_, value_, size_: 8.6, color_: midGray] := mathLabel[RowBox[{symbolBox, "\[TildeTilde]", ToString[NumberForm[N[value], {4, 3}, NumberPadding -> {"", "0"}]]}], size, color];
legendStyle = Directive[FontFamily -> fontFamily, FontSize -> 8.8, Black];
legendFunction = (Framed[#, FrameStyle -> GrayLevel[0.80], Background -> White, RoundingRadius -> 0, FrameMargins -> {{4, 4}, {1, 1}}] &);

outputDir = FileNameJoin[{$TemporaryDirectory, "AIGC_MathTypeset_Final_Output"}];
If[DirectoryQ[outputDir], DeleteDirectory[outputDir, DeleteContents -> True]];
CreateDirectory[outputDir];
exportPDF[name_, graphic_] := Export[FileNameJoin[{outputDir, name}], graphic, "PDF"];

(* Figure 3 *)
Module[{t = 1, theta = 4/5, q = 9/5, alphaBar = 1/2, priceLegend, demandLegend, p1, p2, fig},
  priceLegend = Placed[LineLegend[{humanStyle, aiStyle}, {mathLabel[FractionBox[mathFunction[mathSubStar["p", "H"], alphaBox], "t"]], mathLabel[FractionBox[mathFunction[mathSubStar["p", "A"], alphaBox], "t"]]}, LegendMarkerSize -> 28, LegendLayout -> "Row", LabelStyle -> legendStyle, LegendFunction -> legendFunction], Above];
  demandLegend = Placed[LineLegend[{humanStyle, aiStyle}, {mathLabel[mathFunction[mathSubStar["D", "H"], alphaBox]], mathLabel[mathFunction[mathSubStar["D", "A"], alphaBox]]}, LegendMarkerSize -> 28, LegendLayout -> "Row", LabelStyle -> legendStyle, LegendFunction -> legendFunction], Above];
  p1 = Plot[Evaluate[{pH[alpha, q, theta, t]/t, pA[alpha, q, theta, t]/t}], {alpha, 0, alphaBar}, Evaluate[Sequence @@ commonPlotOptions], PlotStyle -> {humanStyle, aiStyle}, PlotLegends -> priceLegend, PlotRange -> {{0, alphaBar}, {0.78, 1.10}}, GridLines -> {Range[0, .5, .1], Range[.8, 1.1, .1]}, FrameLabel -> {mathLabel[alphaBox, 10], mathLabel[FractionBox[mathFunction[mathSubStar["p", "j"], alphaBox], "t"], 10]}];
  p2 = Plot[Evaluate[{DH[alpha, q, theta, t], DA[alpha, q, theta, t]}], {alpha, 0, alphaBar}, Evaluate[Sequence @@ commonPlotOptions], PlotStyle -> {humanStyle, aiStyle}, PlotLegends -> demandLegend, PlotRange -> {{0, alphaBar}, {0.10, 0.43}}, GridLines -> {Range[0, .5, .1], Range[.1, .4, .1]}, FrameLabel -> {mathLabel[alphaBox, 10], mathLabel[mathFunction[mathSubStar["D", "j"], alphaBox], 10]}];
  fig = GraphicsGrid[{{p1, p2}, {Style["(a) Equilibrium prices", panelStyle], Style["(b) Equilibrium demands", panelStyle]}}, Spacings -> {.10, .03}, ImageSize -> 710];
  exportPDF["Fig03_Market_Outcomes_Mathematica.pdf", fig]
];

(* Figure 4 *)
Module[{t = 1, alphaBar = 1/2, theta = 4/5, qValues = {327/200, 43/25, 48/25}, q2 = 19/10, thetaValues = {11/20, 7/10, 19/20}, alphaMinQ, alphaMinTheta, legendQ, legendTheta, yBox, p1, p2, fig},
  yBox = FractionBox[mathFunction[piHBox, alphaBox], mathFunction[piHBox, "0"]];
  alphaMinQ = N[alpha /. FindRoot[qH[alpha, theta, t] == qValues[[2]], {alpha, .25}], 20];
  legendQ = Placed[LineLegend[{lowStyle, midStyle, highStyle}, {qOverTLabel[qValues[[1]]], qOverTLabel[qValues[[2]]], qOverTLabel[qValues[[3]]]}, LegendMarkerSize -> 25, LegendLayout -> "Row", LabelStyle -> legendStyle, LegendFunction -> legendFunction], Above];
  p1 = Plot[Evaluate[piH[alpha, #, theta, t]/piH[0, #, theta, t] & /@ qValues], {alpha, 0, alphaBar}, Evaluate[Sequence @@ commonPlotOptions], PlotStyle -> {lowStyle, midStyle, highStyle}, PlotLegends -> legendQ, PlotRange -> {{0, alphaBar}, {.935, 1.11}}, GridLines -> {Range[0, .5, .1], Range[.95, 1.10, .05]}, FrameLabel -> {mathLabel[alphaBox, 10], mathLabel[yBox, 10]}, Epilog -> {Directive[midGray, Dashed, AbsoluteThickness[1.0]], Line[{{alphaMinQ, .935}, {alphaMinQ, piH[alphaMinQ, qValues[[2]], theta, t]/piH[0, qValues[[2]], theta, t]}}], PointSize[.014], amber, Point[{alphaMinQ, piH[alphaMinQ, qValues[[2]], theta, t]/piH[0, qValues[[2]], theta, t]}], Text[alphaValueLabel[alphaHBox, alphaMinQ], {alphaMinQ + .074, piH[alphaMinQ, qValues[[2]], theta, t]/piH[0, qValues[[2]], theta, t] - .010}]}];
  alphaMinTheta = N[alpha /. FindRoot[qH[alpha, thetaValues[[2]], t] == q2, {alpha, .18}], 20];
  legendTheta = Placed[LineLegend[{lowStyle, Directive[teal, AbsoluteThickness[2.0], AbsoluteDashing[{5.0, 2.2}]], highStyle}, {thetaValueLabel[thetaValues[[1]]], thetaValueLabel[thetaValues[[2]]], thetaValueLabel[thetaValues[[3]]]}, LegendMarkerSize -> 25, LegendLayout -> "Row", LabelStyle -> legendStyle, LegendFunction -> legendFunction], Above];
  p2 = Plot[Evaluate[piH[alpha, q2, #, t]/piH[0, q2, #, t] & /@ thetaValues], {alpha, 0, alphaBar}, Evaluate[Sequence @@ commonPlotOptions], PlotStyle -> {lowStyle, Directive[teal, AbsoluteThickness[2.0], AbsoluteDashing[{5.0, 2.2}]], highStyle}, PlotLegends -> legendTheta, PlotRange -> {{0, alphaBar}, {.95, 1.17}}, GridLines -> {Range[0, .5, .1], Range[.95, 1.15, .05]}, FrameLabel -> {mathLabel[alphaBox, 10], mathLabel[yBox, 10]}, Epilog -> {Directive[midGray, Dashed, AbsoluteThickness[1.0]], Line[{{alphaMinTheta, .95}, {alphaMinTheta, piH[alphaMinTheta, q2, thetaValues[[2]], t]/piH[0, q2, thetaValues[[2]], t]}}], PointSize[.014], teal, Point[{alphaMinTheta, piH[alphaMinTheta, q2, thetaValues[[2]], t]/piH[0, q2, thetaValues[[2]], t]}], Text[alphaValueLabel[alphaHBox, alphaMinTheta], {alphaMinTheta + .074, piH[alphaMinTheta, q2, thetaValues[[2]], t]/piH[0, q2, thetaValues[[2]], t] - .012}]}];
  fig = GraphicsGrid[{{p1, p2}, {Style["(a) Sensitivity to market value", panelStyle], Style["(b) Sensitivity to AI value", panelStyle]}}, Spacings -> {.10, .03}, ImageSize -> 710];
  exportPDF["Fig04_Human_Profit_Sensitivity_Mathematica.pdf", fig]
];

(* Figure 5 *)
Module[{t = 1, theta = 1/2, alphaBar = 4/5, qLow = 19/10, qHigh = 203/100, alphaTurn, totalStyle, humanComponentStyle, aiComponentStyle, legend, makePanel, p1, p2, fig},
  totalStyle = Directive[Black, AbsoluteThickness[2.2]];
  humanComponentStyle = Directive[navy, AbsoluteThickness[2.0], AbsoluteDashing[{5.0, 2.2}]];
  aiComponentStyle = Directive[amber, AbsoluteThickness[2.0], Dotted];
  legend = Placed[LineLegend[{totalStyle, humanComponentStyle, aiComponentStyle}, {mathLabel[FractionBox[mathFunction["R", alphaBox], mathFunction["R", "0"]]], mathLabel[FractionBox[RowBox[{"2", mathFunction[piHBox, alphaBox]}], mathFunction["R", "0"]]], mathLabel[FractionBox[mathFunction[piABox, alphaBox], mathFunction["R", "0"]]]}, LegendMarkerSize -> 25, LegendLayout -> "Row", LabelStyle -> legendStyle, LegendFunction -> legendFunction], Above];
  alphaTurn = N[alpha /. FindRoot[D[transactionRevenue[alpha, qHigh, theta, t], alpha] == 0, {alpha, .60}], 20];
  makePanel[qValue_, turn_: None] := Module[{r0, epilog},
    r0 = transactionRevenue[0, qValue, theta, t];
    epilog = {Directive[GrayLevel[0.58], AbsoluteThickness[0.9]], Line[{{0, 1}, {alphaBar, 1}}]};
    If[turn =!= None, epilog = Join[epilog, {Directive[midGray, Dashed, AbsoluteThickness[1.0]], Line[{{turn, 0}, {turn, transactionRevenue[turn, qValue, theta, t]/r0}}], PointSize[.014], midGray, Point[{turn, transactionRevenue[turn, qValue, theta, t]/r0}], Text[mathLabel[RowBox[{SuperscriptBox["R", "\[Prime]"], "(", alphaBox, ")", "=", "0", ",", alphaBox, "\[TildeTilde]", ToString[NumberForm[N[turn], {4, 3}, NumberPadding -> {"", "0"}]]}], 8.5, midGray], {turn + .12, transactionRevenue[turn, qValue, theta, t]/r0 - .035}]}]];
    Plot[Evaluate[{transactionRevenue[alpha, qValue, theta, t]/r0, humanRevenue[alpha, qValue, theta, t]/r0, aiRevenue[alpha, qValue, theta, t]/r0}], {alpha, 0, alphaBar}, Evaluate[Sequence @@ commonPlotOptions], PlotStyle -> {totalStyle, humanComponentStyle, aiComponentStyle}, PlotLegends -> legend, PlotRange -> {{0, alphaBar}, {0, 1.04}}, GridLines -> {Range[0, .8, .2], Range[0, 1.0, .2]}, FrameLabel -> {mathLabel[alphaBox, 10], Row[{Style["Revenue relative to ", 10, FontFamily -> fontFamily], mathLabel[mathFunction["R", "0"], 10]}]}, Epilog -> epilog]
  ];
  p1 = makePanel[qLow]; p2 = makePanel[qHigh, alphaTurn];
  fig = GraphicsGrid[{{p1, p2}, {Style["(a) Transaction loss dominates", panelStyle], Style["(b) Human-market expansion reverses the effect", panelStyle]}}, Spacings -> {.10, .03}, ImageSize -> 710];
  exportPDF["Fig05_Revenue_Decomposition_Mathematica.pdf", fig]
];

(* Figure 6 *)
Module[{t = 1, theta = 4/5, alphaBar = 1/2, kappa = 3/4, qLow = 7/4, qHigh = 203/100, rho0 = 1/5, g0 = 3/10, lowPolicyStyle, highPolicyStyle, legend, p1, p2, fig},
  lowPolicyStyle = Directive[amber, AbsoluteThickness[2.0], AbsoluteDashing[{5.0, 2.2}]];
  highPolicyStyle = Directive[navy, AbsoluteThickness[2.2]];
  legend = Placed[LineLegend[{lowPolicyStyle, highPolicyStyle}, {qOverTLabel[qLow], qOverTLabel[qHigh]}, LegendMarkerSize -> 27, LegendLayout -> "Row", LabelStyle -> legendStyle, LegendFunction -> legendFunction], Above];
  p1 = Plot[Evaluate[{alphaStar[g, qLow, theta, rho0, kappa, alphaBar, t], alphaStar[g, qHigh, theta, rho0, kappa, alphaBar, t]}], {g, 0, 23/50}, Evaluate[Sequence @@ commonPlotOptions], PlotStyle -> {lowPolicyStyle, highPolicyStyle}, PlotLegends -> legend, PlotRange -> {{0, 23/50}, {0, alphaBar}}, GridLines -> {Range[0, .45, .1], Range[0, .5, .1]}, FrameLabel -> {mathLabel["g", 10], mathLabel[alphaStarBox, 10]}, Epilog -> {Directive[midGray, Dotted, AbsoluteThickness[0.9]], Line[{{0, alphaBar}, {23/50, alphaBar}}], Text[mathLabel[RowBox[{alphaStarBox, "=", alphaBarBox}], 8.6, midGray], {.385, .485}]}];
  p2 = Plot[Evaluate[{alphaStar[g0, qLow, theta, rho, kappa, alphaBar, t], alphaStar[g0, qHigh, theta, rho, kappa, alphaBar, t]}], {rho, 0, 1/2}, Evaluate[Sequence @@ commonPlotOptions], PlotStyle -> {lowPolicyStyle, highPolicyStyle}, PlotLegends -> legend, PlotRange -> {{0, 1/2}, {.18, .43}}, GridLines -> {Range[0, .5, .1], Range[.2, .4, .05]}, FrameLabel -> {mathLabel[rhoBox, 10], mathLabel[alphaStarBox, 10]}];
  fig = GraphicsGrid[{{p1, p2}, {Style["(a) Sensitivity to the user-value benefit", panelStyle], Style["(b) Sensitivity to the commission rate", panelStyle]}}, Spacings -> {.10, .03}, ImageSize -> 710];
  exportPDF["Fig06_Platform_Control_Sensitivity_Mathematica.pdf", fig]
];

(* Figure 7 *)
Module[{t = 1, theta = 4/5, alphaBar = 1/2, rho = 1/5, kappa = 3/4, q1 = 42/25, g1 = 7/20, q2 = 39/20, g2 = 0, alpha1, platformStyle, creatorStyle, legend, p1, p2, fig, platformRatioBox, creatorRatioBox},
  alpha1 = alphaStar[g1, q1, theta, rho, kappa, alphaBar, t];
  platformStyle = Directive[navy, AbsoluteThickness[2.2]]; creatorStyle = Directive[amber, AbsoluteThickness[2.0], AbsoluteDashing[{5.0, 2.2}]];
  platformRatioBox = FractionBox[mathFunction[PiPBox, alphaBox], mathFunction[PiPBox, "0"]]; creatorRatioBox = FractionBox[mathFunction[piHBox, alphaBox], mathFunction[piHBox, "0"]];
  legend = Placed[LineLegend[{platformStyle, creatorStyle}, {mathLabel[platformRatioBox], mathLabel[creatorRatioBox]}, LegendMarkerSize -> 27, LegendLayout -> "Row", LabelStyle -> legendStyle, LegendFunction -> legendFunction], Above];
  p1 = Plot[Evaluate[{platformProfit[alpha, q1, theta, rho, g1, kappa, t]/platformProfit[0, q1, theta, rho, g1, kappa, t], piH[alpha, q1, theta, t]/piH[0, q1, theta, t]}], {alpha, 0, alphaBar}, Evaluate[Sequence @@ commonPlotOptions], PlotStyle -> {platformStyle, creatorStyle}, PlotLegends -> legend, PlotRange -> {{0, alphaBar}, {.94, 1.27}}, GridLines -> {Range[0, .5, .1], Range[.95, 1.25, .05]}, FrameLabel -> {mathLabel[alphaBox, 10], Style["Normalized payoff", 10, FontFamily -> fontFamily]}, Epilog -> {Directive[midGray, Dashed, AbsoluteThickness[1.0]], Line[{{alpha1, .94}, {alpha1, platformProfit[alpha1, q1, theta, rho, g1, kappa, t]/platformProfit[0, q1, theta, rho, g1, kappa, t]}}], PointSize[.014], navy, Point[{alpha1, platformProfit[alpha1, q1, theta, rho, g1, kappa, t]/platformProfit[0, q1, theta, rho, g1, kappa, t]}], Text[alphaValueLabel[alphaStarBox, alpha1], {alpha1 - .10, 1.245}]}];
  p2 = Plot[Evaluate[{platformProfit[alpha, q2, theta, rho, g2, kappa, t]/platformProfit[0, q2, theta, rho, g2, kappa, t], piH[alpha, q2, theta, t]/piH[0, q2, theta, t]}], {alpha, 0, alphaBar}, Evaluate[Sequence @@ commonPlotOptions], PlotStyle -> {platformStyle, creatorStyle}, PlotLegends -> legend, PlotRange -> {{0, alphaBar}, {.45, 1.12}}, GridLines -> {Range[0, .5, .1], Range[.5, 1.1, .1]}, FrameLabel -> {mathLabel[alphaBox, 10], Style["Normalized payoff", 10, FontFamily -> fontFamily]}, Epilog -> {PointSize[.014], navy, Point[{0, 1}], Text[mathLabel[RowBox[{alphaStarBox, "=", "0"}], 8.6, midGray], {.085, 1.075}]}];
  fig = GraphicsGrid[{{p1, p2}, {Style["(a) Platform expansion can hurt human creators", panelStyle], Style["(b) Platform underprovision can leave human gains unrealized", panelStyle]}}, Spacings -> {.10, .03}, ImageSize -> 710];
  exportPDF["Fig07_Stakeholder_Misalignment_Mathematica.pdf", fig]
];

Print[outputDir];
