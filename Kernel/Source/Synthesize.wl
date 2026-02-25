BeginPackage[
	"ToneAr`FileScope`Synthesize`",
	{"ToneAr`OllamaLink`", "ToneAr`OllamaLink`Private`"}
];
Begin["`Private`"];

(* -------------------------------------------------------------------------- *)
(* ::Section:: *)
(* OllamaSynthesize *)
(* Description:  Send a prompt to Ollama service and get back response
 * Return:       { respObj__Association } | _Failure
 *)
OllamaSynthesize // Options =
	{
		"BaseURL" -> Automatic,
		"Context" -> None,
		"WriteToStdout" -> True,
		"Images" -> {},
		"Role" -> "user",
		"Stream" -> True,
		(* Ollama model options *)
		"NumKeep" -> Automatic,
		"Seed" -> Automatic,
		"NumPredict" -> Automatic,
		"TopK" -> Automatic,
		"TopP" -> Automatic,
		"MinP" -> Automatic,
		"TfsZ" -> Automatic,
		"TypicalP" -> Automatic,
		"RepeatLastN" -> Automatic,
		"Temperature" -> Automatic,
		"RepeatPenalty" -> Automatic,
		"PresencePenalty" -> Automatic,
		"FrequencyPenalty" -> Automatic,
		"Mirostat" -> Automatic,
		"MirostatTau" -> Automatic,
		"MirostatEta" -> Automatic,
		"PenalizeNewline" -> Automatic,
		"Stop" -> Automatic,
		"Numa" -> Automatic,
		"NumCtx" -> Automatic,
		"NumBatch" -> Automatic,
		"NumGpu" -> Automatic,
		"MainGpu" -> Automatic,
		"LowVram" -> Automatic,
		"VocabOnly" -> Automatic,
		"UseMmap" -> Automatic,
		"UseMlock" -> Automatic,
		"NumThread" -> Automatic
	};
OllamaSynthesize[model_String][prompt_String, opts : OptionsPattern[]] :=
	OllamaSynthesize[prompt, model, opts];
OllamaSynthesize[prompt_String, model_String, OptionsPattern[]] :=
	Module[{
		task,
		options =
			Association @
			Replace[
				{
					"num_keep" -> OptionValue["NumKeep"],
					"seed" -> OptionValue["Seed"],
					"num_predict" -> OptionValue["NumPredict"],
					"top_k" -> OptionValue["TopK"],
					"top_p" -> OptionValue["TopP"],
					"min_p" -> OptionValue["MinP"],
					"tfs_z" -> OptionValue["TfsZ"],
					"typical_p" -> OptionValue["TypicalP"],
					"repeat_last_n" -> OptionValue["RepeatLastN"],
					"temperature" -> OptionValue["Temperature"],
					"repeat_penalty" -> OptionValue["RepeatPenalty"],
					"presence_penalty" -> OptionValue["PresencePenalty"],
					"frequency_penalty" -> OptionValue["FrequencyPenalty"],
					"mirostat" -> OptionValue["Mirostat"],
					"mirostat_tau" -> OptionValue["MirostatTau"],
					"mirostat_eta" -> OptionValue["MirostatEta"],
					"penalize_newline" -> OptionValue["PenalizeNewline"],
					"stop" -> OptionValue["Stop"],
					"numa" -> OptionValue["Numa"],
					"num_ctx" -> OptionValue["NumCtx"],
					"num_batch" -> OptionValue["NumBatch"],
					"num_gpu" -> OptionValue["NumGpu"],
					"main_gpu" -> OptionValue["MainGpu"],
					"low_vram" -> OptionValue["LowVram"],
					"vocab_only" -> OptionValue["VocabOnly"],
					"use_mmap" -> OptionValue["UseMmap"],
					"use_mlock" -> OptionValue["UseMlock"],
					"num_thread" -> OptionValue["NumThread"]
				},
				HoldPattern[Rule][_, Automatic] :> Nothing,
				1
			],
		ds = CreateDataStructure["ExtensibleVector"],
		context = Replace[OptionValue["Context"], None -> {}],
		imageData =
			Replace[
				OptionValue["Images"],
				{
					None -> Nothing,
					file_File :> ExportString[Import[file], "Base64"],
					img_Image :> ExportString[
						ExportString[img, "PNG"],
						"Base64"
					],
					str_String :> str
				},
				{1}
			]
	},
		Enclose[
			ConfirmMatch[imageData, {___String}, "Invalid image data"];
			task =
				URLSubmit[
					HTTPRequest[
						URLBuild[
							{
								Replace[
									OptionValue["BaseURL"],
									Automatic :> $OllamaBase
								],
								"api",
								"generate"
							}
						],
						<|
							"Method" -> "POST",
							"Body" -> ExportString[
								<|
									"model" -> model,
									"prompt" -> prompt,
									"context" -> context,
									"stream" -> OptionValue["Stream"],
									"images" -> imageData,
									"options" -> options
								|>,
								"RawJSON"
							]
						|>
					],
					HandlerFunctionsKeys -> All,
					HandlerFunctions -> <|
						"BodyChunkReceived" -> Function[
							If[OptionValue["WriteToStdout"], With[{
								res =
									Quiet @
									ImportString[#"BodyChunk", "RawJSON"]
							},
								WriteString[
									$StandardOutputStream,
									res["response"] //
									Replace[
										{
											Except[_String] /;
												AssociationQ[res] -> res[
												"thinking"
											],
											Except[_String] -> ""
										}
									]
								]
							]]; ds["Append", #"BodyChunk"]
						]
					|>
				];
			(* Block while task is active *)
			While[task["TaskStatus"] =!= "Removed", Pause[0.1]];
			(* Build the return *)
			Replace[
				{s : Except[""] :> ImportString[s, "RawJSON"], "" :> Nothing}
			] /@ ds["Elements"]
		]
	];

End[];
EndPackage[];
