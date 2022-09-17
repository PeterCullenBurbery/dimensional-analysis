(* ::Package:: *)

BeginPackage["PeterBurbery`DimensionalAnalysis`"];

(* Declare your packages public symbols here. *)


CanonicalDimensions;
Begin["`Private`"];

(* Define your public and private symbols here. *)

ClearAll[CanonicalDimensions]
CanonicalDimensions[unit_]:=Block[{assoc},
assoc=Association[Rule@@@UnitDimensions[unit]];Append[Association["TimeUnit"->0,"LengthUnit"->0,
"MassUnit"->0,"ElectricCurrentUnit"->0,"TemperatureUnit"->0,"AmountUnit"->0,"LuminousIntensityUnit"->0],assoc]]
End[]; (* End `Private` *)

EndPackage[];
