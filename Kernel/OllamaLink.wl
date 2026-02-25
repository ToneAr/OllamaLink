(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::DifferentLine:: *)
PreemptProtect[BeginPackage["ToneAr`OllamaLink`"]; EndPackage[]];
With[{
	path =
		(
			FileNames[
				"*.wl",
				FileNameJoin[
					{
						PacletObject["ToneAr/OllamaLink"]["Location"],
						"Kernel",
						"Source"
					}
				],
				Infinity
			] //
			Map[FileNameTake] //
			StringReplace["/" -> "`"] //
			StringReplace[(name__ ~~ ".wl") :> ("Source`" <> name <> "`")]
		)
},
	Get["ToneAr`OllamaLink`" <> #]& /@ {"Public`", "Private`", Splice[path]}
];
(* :!CodeAnalysis::EndBlock:: *)
