(* ::Package:: *)

BeginPackage["PeterBurbery`DimensionalAnalysis`"];

(* Declare your packages public symbols here. *)


(*CanonicalDimensions;
CanonicalDimensionsVector;
CanonicalDimensionalForm;
CanonicalDimensionalScientificNotationForm;
IdealizedSIConstantsDefinition;
PhysicalObservationInformation;
PlanckUnitConversion;
StoneyUnitConversion;*)
UnitSystemTransform;
CanonicalDimensionalProduct;
Begin["`Private`"];

(* Define your public and private symbols here. *)

ClearAll[UnitSystemTransform]
CanonicalDimensionalProduct//ClearAll

CanonicalDimensionalProduct[quantity_?QuantityQ]:=Block[{permutation,quantityDimensions,dimensions,exponents,permutedDimensions,permutedExponents,SIDimensions},quantityDimensions=UnitDimensions[quantity];SIDimensions={"TimeUnit","LengthUnit","MassUnit","ElectricCurrentUnit","TemperatureUnit","AmountUnit","LuminousIntensityUnit","AngleUnit","InformationUnit","MoneyUnit","PersonUnit","SolidAngleUnit","TemperatureDifferenceUnit"};dimensions=First[Transpose[quantityDimensions]];exponents=Last[Transpose[quantityDimensions]];permutation=FindPermutation[dimensions,DeleteElements[SIDimensions,Complement[SIDimensions,dimensions]]];permutedDimensions=Permute[dimensions,permutation];permutedExponents=Permute[exponents,permutation];Row[Superscript@@@(Transpose[{permutedDimensions,permutedExponents}]/. Thread[{"TimeUnit"->"T","AmountUnit"->"N","AngleUnit"->"\[CapitalAlpha]","ElectricCurrentUnit"->"I","LengthUnit"->"L","MassUnit"->"M","TemperatureUnit"->"\[CapitalTheta]","LuminousIntensityUnit"->"J","InformationUnit"->"F","MoneyUnit"->"C","PersonUnit"->"P","SolidAngleUnit"->"\[CapitalPhi]","TemperatureDifferenceUnit"->"D","PhotonUnit"->"H"}])]]/;ContainsAll[{"TimeUnit","LengthUnit","MassUnit","ElectricCurrentUnit","TemperatureUnit","AmountUnit","LuminousIntensityUnit","AngleUnit","InformationUnit","MoneyUnit","PersonUnit","SolidAngleUnit","TemperatureDifferenceUnit"},First[Transpose[UnitDimensions[quantity]]]]


CanonicalDimensionalProduct[entity_Entity]:=If[EntityTypeName[entity]=="PhysicalQuantity",If[QuantityQ[entity["SIUnit"]],CanonicalDimensionalProduct[entity["SIUnit"]]],If[EntityTypeName[entity]=="PhysicalConstant",If[QuantityQ[entity["Value"]],CanonicalDimensionalProduct[entity["Value"]]]]]
(*CanonicalDimensions[unit_]:=Block[{assoc},
assoc=Association[Rule@@@UnitDimensions[unit]];Append[Association["TimeUnit"->0,"LengthUnit"->0,
"MassUnit"->0,"ElectricCurrentUnit"->0,"TemperatureUnit"->0,"AmountUnit"->0,"LuminousIntensityUnit"->0],assoc]]
CanonicalDimensionsVector[unit_]:=Block[{assoc,canonicalassoc},
assoc=Association[Rule@@@UnitDimensions[unit]];
canonicalassoc=Append[Association["TimeUnit"->0,"LengthUnit"->0,
"MassUnit"->0,"ElectricCurrentUnit"->0,"TemperatureUnit"->0,"AmountUnit"->0,"LuminousIntensityUnit"->0],assoc];
{canonicalassoc["TimeUnit"],canonicalassoc["LengthUnit"],canonicalassoc["MassUnit"],canonicalassoc["ElectricCurrentUnit"],
canonicalassoc["TemperatureUnit"],canonicalassoc["AmountUnit"],canonicalassoc["LuminousIntensityUnit"]}];

CanonicalDimensionalForm[unit_]:=Block[{assoc,canonicalassoc,canonicalvector,canonicaldimensionalsymbols},
assoc=Association[Rule@@@UnitDimensions[unit]];
canonicalassoc=Append[Association["TimeUnit"->0,"LengthUnit"->0,
"MassUnit"->0,"ElectricCurrentUnit"->0,"TemperatureUnit"->0,"AmountUnit"->0,"LuminousIntensityUnit"->0],assoc];
canonicalvector={canonicalassoc["TimeUnit"],canonicalassoc["LengthUnit"],canonicalassoc["MassUnit"],canonicalassoc["ElectricCurrentUnit"],
canonicalassoc["TemperatureUnit"],canonicalassoc["AmountUnit"],canonicalassoc["LuminousIntensityUnit"]};
canonicaldimensionalsymbols={"T","L","M","I","\[CapitalTheta]","N","J"};
Times@@Power@@@Transpose[{canonicaldimensionalsymbols,CanonicalDimensionsVector[Quantity["Webers"]]}]]

CanonicalDimensionalScientificNotationForm[unit_]:=Block[{assoc,canonicalassoc,canonicalvector,canonicaldimensionalsymbols},
assoc=Association[Rule@@@UnitDimensions[unit]];
canonicalassoc=Append[Association["TimeUnit"->0,"LengthUnit"->0,
"MassUnit"->0,"ElectricCurrentUnit"->0,"TemperatureUnit"->0,"AmountUnit"->0,"LuminousIntensityUnit"->0],assoc];
canonicalvector={canonicalassoc["TimeUnit"],canonicalassoc["LengthUnit"],canonicalassoc["MassUnit"],canonicalassoc["ElectricCurrentUnit"],
canonicalassoc["TemperatureUnit"],canonicalassoc["AmountUnit"],canonicalassoc["LuminousIntensityUnit"]};
canonicaldimensionalsymbols={"T","L","M","I","\[CapitalTheta]","N","J"};
CenterDot@@Superscript@@@Transpose[{canonicaldimensionalsymbols,CanonicalDimensionsVector[Quantity["Webers"]]}]]*)


(*IdealizedSIConstantsDefinition[unit_]:=Block[{assoc,canonicalassoc,canonicalvector,HYPERFINETRANSITIONFREQUENCYOFCAESIUM,
SPEEDOFLIGHTINVACUUM,PLANCKCONSTANT,ELEMENTARYCHARGE,BOLTZMANNCONSTANT,AVOGADROCONSTANT,LUMINOUSEFFICACYMONOCHROMATICRADIATION,
DEFININGCONSTANTS,SIEXACTCONSTANTS,multipliers,angleextra},assoc=Association[Rule@@@UnitDimensions[unit]];canonicalassoc=Append[
Association["TimeUnit"->0,"LengthUnit"->0,"MassUnit"->0,"ElectricCurrentUnit"->0,"TemperatureUnit"->0,"AmountUnit"->0,"LuminousIntensityUnit"->0],
assoc];canonicalvector={canonicalassoc["TimeUnit"],canonicalassoc["LengthUnit"],canonicalassoc["MassUnit"],canonicalassoc["ElectricCurrentUnit"],
canonicalassoc["TemperatureUnit"],canonicalassoc["AmountUnit"],canonicalassoc["LuminousIntensityUnit"]};
HYPERFINETRANSITIONFREQUENCYOFCAESIUM={-1,0,0,0,0,0,0};SPEEDOFLIGHTINVACUUM={-1,1,0,0,0,0,0};PLANCKCONSTANT={-1,2,1,0,0,0,0};
ELEMENTARYCHARGE={1,0,0,1,0,0,0};BOLTZMANNCONSTANT={-2,2,1,0,-1,0,0};AVOGADROCONSTANT={0,0,0,0,0,-1,0};
LUMINOUSEFFICACYMONOCHROMATICRADIATION={3,-2,-1,0,0,0,1};DEFININGCONSTANTS={HYPERFINETRANSITIONFREQUENCYOFCAESIUM,
SPEEDOFLIGHTINVACUUM,PLANCKCONSTANT,ELEMENTARYCHARGE,BOLTZMANNCONSTANT,AVOGADROCONSTANT,LUMINOUSEFFICACYMONOCHROMATICRADIATION};
SIEXACTCONSTANTS={Quantity["Cesium133HyperfineSplittingFrequency"],Quantity["SpeedOfLight"],Quantity["PlanckConstant"],
Quantity["ElementaryCharge"],Quantity["BoltzmannConstant"],Quantity["AvogadroConstant"],Quantity["MonochromaticRadiation540THzLuminousEfficacy"]};
multipliers=Power@@@Transpose[{SIEXACTCONSTANTS,LinearSolve[Transpose[DEFININGCONSTANTS],canonicalvector]}];
angleextra=(UnitDimensions[UnitSimplify[unit/Quantity["MonochromaticRadiation540THzLuminousEfficacy"],UnityDimensions->{
"TimeUnit","LengthUnit","MassUnit","ElectricCurrentUnit","TemperatureUnit","AmountUnit","LuminousIntensityUnit"}]])/.{{"AngleUnit",x_
}:>Quantity[("Radians")^x]};
multipliers;
angleextra;
If[canonicalassoc["LuminousIntensityUnit"]!=0,
UnitConvert[unit,Times@@Join[multipliers,angleextra]],UnitConvert[unit,Times@@multipliers]]]

PhysicalObservationInformation[unit_]:=Block[{assoc,canonicalassoc,canonicalvector,HYPERFINETRANSITIONFREQUENCYOFCAESIUM,
SPEEDOFLIGHTINVACUUM,PLANCKCONSTANT,ELEMENTARYCHARGE,BOLTZMANNCONSTANT,AVOGADROCONSTANT,LUMINOUSEFFICACYMONOCHROMATICRADIATION,
DEFININGCONSTANTS,SIEXACTCONSTANTS,multipliers,angleextra,IDEALIZEDSICONSTANTSDEFINITION,canonicaldimensionalsymbols,dimensionalproduct},assoc=
Association[Rule@@@UnitDimensions[unit]];canonicalassoc=Append[Association["TimeUnit"->0,"LengthUnit"->0,"MassUnit"->0,
"ElectricCurrentUnit"->0,"TemperatureUnit"->0,"AmountUnit"->0,"LuminousIntensityUnit"->0],assoc];canonicalvector={
canonicalassoc["TimeUnit"],canonicalassoc["LengthUnit"],canonicalassoc["MassUnit"],canonicalassoc["ElectricCurrentUnit"],
canonicalassoc["TemperatureUnit"],canonicalassoc["AmountUnit"],canonicalassoc["LuminousIntensityUnit"]};
HYPERFINETRANSITIONFREQUENCYOFCAESIUM={-1,0,0,0,0,0,0};SPEEDOFLIGHTINVACUUM={-1,1,0,0,0,0,0};PLANCKCONSTANT={-1,2,1,0,0,0,0};
ELEMENTARYCHARGE={1,0,0,1,0,0,0};BOLTZMANNCONSTANT={-2,2,1,0,-1,0,0};AVOGADROCONSTANT={0,0,0,0,0,-1,0};
LUMINOUSEFFICACYMONOCHROMATICRADIATION={3,-2,-1,0,0,0,1};DEFININGCONSTANTS={HYPERFINETRANSITIONFREQUENCYOFCAESIUM,SPEEDOFLIGHTINVACUUM,
PLANCKCONSTANT,ELEMENTARYCHARGE,BOLTZMANNCONSTANT,AVOGADROCONSTANT,LUMINOUSEFFICACYMONOCHROMATICRADIATION};SIEXACTCONSTANTS={Quantity[
"Cesium133HyperfineSplittingFrequency"],Quantity["SpeedOfLight"],Quantity["PlanckConstant"],Quantity["ElementaryCharge"],Quantity[
"BoltzmannConstant"],Quantity["AvogadroConstant"],Quantity["MonochromaticRadiation540THzLuminousEfficacy"]};
multipliers=Power@@@Transpose[{SIEXACTCONSTANTS,LinearSolve[Transpose[DEFININGCONSTANTS],canonicalvector]}];
angleextra=(UnitDimensions[UnitSimplify[unit/Quantity["MonochromaticRadiation540THzLuminousEfficacy"],
UnityDimensions->{"TimeUnit","LengthUnit","MassUnit","ElectricCurrentUnit","TemperatureUnit","AmountUnit","LuminousIntensityUnit"}]])/.{{
"AngleUnit",x_}:>Quantity[("Radians")^x]};
IDEALIZEDSICONSTANTSDEFINITION=If[canonicalassoc["LuminousIntensityUnit"]!=0,
UnitConvert[unit,Times@@Join[multipliers,angleextra]],UnitConvert[unit,Times@@multipliers]];
canonicaldimensionalsymbols={"T","L","M","I","\[CapitalTheta]","N","J"};
dimensionalproduct=Times@@Power@@@Transpose[{canonicaldimensionalsymbols,CanonicalDimensionsVector[Quantity["Webers"]]}];
<|
"dimensions-notation"->CanonicalDimensionalScientificNotationForm[unit],"dimensional-product"->dimensionalproduct,
"make-unit-planck"->PlanckUnitConversion[unit],"make-unit-stoney"->StoneyUnitConversion[unit],"idealized-SI-constants-definition"->IdealizedSIConstantsDefinition[unit],"continuous-idealized-SI-constants-definition"->
N[IdealizedSIConstantsDefinition[unit]],"make-unit-simple"->UnitSimplify[unit]
|>]*)


(*ClearAll[PlanckUnitConversion];
$unitRules := $unitRules = Dispatch[{"LengthUnit" -> "PlanckLength", "MassUnit" -> "PlanckMass", "TemperatureUnit" -> 
"PlanckTemperature", "TimeUnit" -> "PlanckTime", "ElectricCurrentUnit" -> "Amperes", "AmountUnit" -> "Moles",
 "LuminousIntensityUnit" -> "Candelas", "AngleUnit" -> "Radians", "InformationUnit" -> "Bits", "MoneyUnit" -> "USDollars",
  "SolidAngleUnit" -> "Steradians", "TemperatureDifferenceUnit" -> UnitConvert[Quantity["PlanckTemperature"] Quantity["KelvinsDifference"]/Quantity["Kelvins"]]}];
SetAttributes[PlanckUnitConversion, Listable];
PlanckUnitConversion[q_?QuantityQ] := With[{res = UnitConvert[q]}, UnitConvert[res, Times @@ (Power @@@ (UnitDimensions[res] /. $unitRules))]]
ClearAll[StoneyUnitConversion];
$unitRules := $unitRules = Dispatch[{"LengthUnit" -> "StoneyLength", "MassUnit" -> "StoneyMass", "TemperatureUnit" -> "StoneyTemperature", "TimeUnit" -> "StoneyTime", "ElectricCurrentUnit" -> "StoneyElectricCurrent", "AmountUnit" -> "Moles", "LuminousIntensityUnit" -> "Candelas", "AngleUnit" -> "Radians", "InformationUnit" -> "Bits", "MoneyUnit" -> "USDollars", "SolidAngleUnit" -> "Steradians", "TemperatureDifferenceUnit" -> Quantity["Kelvins"]}];
SetAttributes[StoneyUnitConversion, Listable];
StoneyUnitConversion[q_?QuantityQ] := With[{res = UnitConvert[q]}, UnitConvert[res, Times @@ (Power @@@ (UnitDimensions[res] /. $unitRules))]]

*)
ClearAll[UnitSystemTransform]
UnitSystemTransform[targetquantity_,"PlanckUnits"]:=Block[{dimensionalCombinations,quantityVariables,mapping},quantityVariables=(QuantityVariable[UnitDimensions[#1]]&)/@{Quantity[1,"PlanckTime"],Quantity[1,"PlanckLength"],Quantity[1,"PlanckMass"],Quantity[1,"PlanckTemperature"]};
dimensionalCombinations=DimensionalCombinations[quantityVariables,QuantityVariable[UnitDimensions[targetquantity]],GeneratedParameters->None];
mapping=MapThread[#1->#2&,{quantityVariables,{Quantity[1,"PlanckTime"],Quantity[1,"PlanckLength"],Quantity[1,"PlanckMass"],Quantity[1,"PlanckTemperature"]}}];(UnitConvert[targetquantity,#1/.mapping]&)/@dimensionalCombinations];
{
 {UnitSystemTransform[targetquantity_,"SIDefiningConstants"]:=Block[{dimensionalCombinations,quantityVariables,mapping},quantityVariables=(QuantityVariable[UnitDimensions[#1]]&)/@{Quantity[1,"Cesium133HyperfineSplittingFrequency"],Quantity[1,"SpeedOfLight"],Quantity[1,"PlanckConstant"],Quantity[1,"ElementaryCharge"],Quantity["BoltzmannConstant"],Quantity["AvogadroConstant"],Quantity["MonochromaticRadiation540THzLuminousEfficacy"]};dimensionalCombinations=dimensionalCombinations=DimensionalCombinations[quantityVariables,QuantityVariable[UnitDimensions[targetquantity]],GeneratedParameters->None];mapping=MapThread[#1->#2&,{quantityVariables,{Quantity[1,"Cesium133HyperfineSplittingFrequency"],Quantity[1,"SpeedOfLight"],Quantity[1,"PlanckConstant"],Quantity[1,"ElementaryCharge"],Quantity["BoltzmannConstant"],Quantity["AvogadroConstant"],Quantity["MonochromaticRadiation540THzLuminousEfficacy"]}}];(UnitConvert[targetquantity,#1/. mapping]&)/@dimensionalCombinations];},
 { },
 {UnitSystemTransform[targetquantity_,"NaturalUnits"]:=Block[{dimensionalCombinations,quantityVariables,mapping},quantityVariables=(QuantityVariable[UnitDimensions[#1]]&)/@{Quantity[1,"PlanckTime"],Quantity[1,"PlanckLength"],Quantity[1,"PlanckMass"],Quantity[1,"VonKlitzingConstant"],Quantity[1,"PlanckTemperature"]};dimensionalCombinations=DimensionalCombinations[quantityVariables,QuantityVariable[UnitDimensions[targetquantity]],GeneratedParameters->None];mapping=MapThread[#1->#2&,{quantityVariables,{Quantity[1,"PlanckTime"],Quantity[1,"PlanckLength"],Quantity[1,"PlanckMass"],Quantity[1,"VonKlitzingConstant"],Quantity[1,"PlanckTemperature"]}}];(UnitConvert[targetquantity,#1/. mapping]&)/@dimensionalCombinations];
  
  UnitSystemTransform[targetquantity_,"StoneyUnits"]:=Block[{dimensionalCombinations,quantityVariables,mapping},quantityVariables=(QuantityVariable[UnitDimensions[#1]]&)/@{Quantity[1,"StoneyTime"],Quantity[1,"StoneyLength"],Quantity[1,"StoneyMass"],Quantity[1,"StoneyElectricCurrent"]};dimensionalCombinations=DimensionalCombinations[quantityVariables,QuantityVariable[UnitDimensions[targetquantity]],GeneratedParameters->None];mapping=MapThread[#1->#2&,{quantityVariables,{Quantity[1,"StoneyTime"],Quantity[1,"StoneyLength"],Quantity[1,"StoneyMass"],Quantity[1,"StoneyElectricCurrent"]}}];(UnitConvert[targetquantity,#1/. mapping]&)/@dimensionalCombinations];},
 { },
 {UnitSystemTransform[targetquantity_,basisquantities_]:=Block[{dimensionalCombinations,quantityVariables,mapping},quantityVariables=(QuantityVariable[UnitDimensions[#1]]&)/@basisquantities;dimensionalCombinations=DimensionalCombinations[quantityVariables,QuantityVariable[UnitDimensions[targetquantity]],GeneratedParameters->None];mapping=MapThread[#1->#2&,{quantityVariables,basisquantities}];(UnitConvert[targetquantity,#1/. mapping]&)/@dimensionalCombinations];}
}
End[]; (* End `Private` *)

EndPackage[];
