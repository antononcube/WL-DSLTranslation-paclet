
BeginPackage["AntonAntonov`DSLTranslation`"];

DSLTranslation::usage = "Code generation based on Domain Specific Language (DSL) specifications.";

Begin["`Private`"];

Clear[GetShellSession];
GetShellSession[] :=
    Block[{lsSS = Select[ExternalSessions[], #["System"] == "Shell" &]},
      If[Length[lsSS] == 0, "Shell", First[lsSS]]];

Clear[DSLWebServiceInterpretationURL];

Options[DSLWebServiceInterpretationURL] = {
  "URL" -> "http://accendodata.net:5040/translate",
  "Sub" -> None,
  "ToLanguage" -> Automatic, "FromLanguage" -> Automatic};

DSLWebServiceInterpretationURL[command_String, opts : OptionsPattern[]] :=
    Block[{toLang, fromLang},

      toLang = OptionValue[DSLWebServiceInterpretationURL, "ToLanguage"];
      If[! StringQ[toLang], toLang = "WL"];

      fromLang = OptionValue[DSLWebServiceInterpretationURL, "FromLanguage"];
      If[! StringQ[fromLang], fromLang = "Automatic"];

      If[StringQ[OptionValue[DSLWebServiceInterpretationURL, "Sub"]],
        OptionValue[DSLWebServiceInterpretationURL, "URL"] <>
            OptionValue[DSLWebServiceInterpretationURL, "Sub"] <> "?command='" <>
            URLEncode[command] <> "'" <> "&lang=" <> URLEncode[toLang] <>
            "&from-lang=" <> URLEncode[fromLang],
        (*ELSE*)
        OptionValue[DSLWebServiceInterpretationURL, "URL"] <> "?command='" <>
            URLEncode[command] <> "'" <> "&lang=" <> URLEncode[toLang] <>
            "&from-lang=" <> URLEncode[fromLang]
      ]
    ];

DSLWebServiceInterpretation::wlcop =
    "The value of the option \"WLCode\" is expected to be one of Automatic, False, or True";

DSLWebServiceInterpretation::nowl = "The value of the key \"CODE\" of the result is not WL code.";

Clear[DSLWebServiceInterpretation];

Options[DSLWebServiceInterpretation] =
    Join[Options[DSLWebServiceInterpretationURL], {"WLCode" -> Automatic, "DeferringFunction" -> "Hold"}];

DSLWebServiceInterpretation[command_String, opts : OptionsPattern[]] :=
    Block[{codeForm, deferFunc, url, aRes},
      codeForm = OptionValue[DSLWebServiceInterpretation, "WLCode"];

      deferFunc = OptionValue[DSLWebServiceInterpretation, "DeferringFunction"];

      url = DSLWebServiceInterpretationURL[command,
        FilterRules[{opts}, Options[DSLWebServiceInterpretationURL]]];

      aRes = Import[url, "JSON"];

      If[! MatchQ[aRes, {_Rule ..}], Return[aRes]];

      aRes = Association[aRes];

      If[! MemberQ[{Automatic, False, True}, codeForm],
        Message[DSLWebServiceInterpretation::wlcop]];

      If[StringQ[aRes["DSLTARGET"]] &&
          StringMatchQ[aRes["DSLTARGET"], ___ ~~ "WL" ~~ ___] &&
          TrueQ[codeForm === Automatic] || TrueQ[codeForm],
        aRes["CODE"] =
            ToExpression[
              "Defer[" <>
                  StringReplace[aRes["CODE"], {"==>" -> "\[DoubleLongRightArrow]"}] <>
                  "]"]
      ];
      aRes
    ];

Options[DSLCLIInterpretation] = {"ToLanguage" -> Automatic,
  "FromLanguage" -> Automatic, "WLCode" -> Automatic,
  "DeferringFunction" -> "Hold", "ShellSession" -> Automatic,
  "CLIPath" -> Automatic};

DSLCLIInterpretation[command_String, opts : OptionsPattern[]] :=
    Block[{codeForm, deferFunc, fromLang, toLang, shellSession, cliPpath,
      shellCommand, res, aRes},

      (* Code from *)
      codeForm = OptionValue[DSLCLIInterpretation, "WLCode"];

      (* Defer function *)
      deferFunc = OptionValue[DSLCLIInterpretation, "DeferringFunction"];

      (* From language *)
      fromLang = OptionValue[DSLCLIInterpretation, "FromLanguage"];
      If[TrueQ[fromLang == Automatic], fromLang = "Automatic"];

      (* To language *)
      toLang = OptionValue[DSLCLIInterpretation, "ToLanguage"];
      If[TrueQ[toLang == Automatic], toLang = "WL"];

      (* Shell session *)
      shellSession = OptionValue[DSLCLIInterpretation, "ShellSession"];
      If[! (shellSession != "Shell" ||
          TrueQ[Head[shellSession] === ExternalSessionObject] ||
          TrueQ[shellSession === Automatic]),
        ResourceFunction["ResourceFunctionMessage"][DSLTranslation::nss];
        shellSession = "Shell"
      ];
      If[TrueQ[shellSession === Automatic], shellSession = GetShellSession[]];

      (* Path *)
      cliPpath = OptionValue[DSLCLIInterpretation, "CLIPath"];
      If[TrueQ[cliPpath == Automatic], cliPpath = "dsl-translation"];

      (*Evaluation*)
      shellCommand =
          cliPpath <> " --from-language=" <> ToString[fromLang] <>
              " --format=json " <> ToString[toLang] <> " '" <> command <> "'";

      Block[{Print = Null},
        res = ExternalEvaluate[shellSession, shellCommand]
      ];
      aRes = ImportString[res["StandardOutput"], "JSON"];

      If[! MatchQ[aRes, {_Rule ..}], Return[aRes]];

      aRes = Association[aRes];

      If[! MemberQ[{Automatic, False, True}, codeForm],
        Message[DSLCLIInterpretation::wlcop]];

      If[StringQ[aRes["DSLTARGET"]] &&
          StringMatchQ[aRes["DSLTARGET"], ___ ~~ "WL" ~~ ___] &&
          TrueQ[codeForm === Automatic] || TrueQ[codeForm],
        aRes["CODE"] =
            ToExpression[
              "Defer[" <>
                  StringReplace[aRes["CODE"], {"==>" -> "\[DoubleLongRightArrow]"}] <>
                  "]"]
      ];
      aRes
    ];

Clear[DSLTranslation];

DSLTranslation::nsrc =
    "The value of the option \"Source\" is expected to be one of \"Web\", \"CLI\", Automatic.";

DSLTranslation::nfmt =
    "The value of the option \"Format\" is expected to be one of \"CODE\", \"JSON\", Association, Automatic.";

DSLTranslation::lngs = "Assuming from language `1` and to language `2`";

DSLTranslation::nss =
    "The value of the option ShellSession is expected to be a \"Shell\", Automatic, or ExternalSessionObject.";

Options[DSLTranslation] =
    Join[Options[DSLWebServiceInterpretation], {"Source" -> "Web",
      "Format" -> Automatic, "ShellSession" -> Automatic,
      "CLIPath" -> Automatic, "CopyToClipboard" -> True}];

DSLTranslation[command_String, toLang : (Automatic | _String), opts___] :=
    DSLTranslation[command, "ToLanguage" -> toLang, opts];

DSLTranslation[command_String, optsArg___] :=
    Block[{opts = {optsArg}, langRuleSpec, source, fmt, cpcbQ, shellSession,
      res},

      opts = Select[opts, Head[#] === Rule &];

      langRuleSpec = Complement[opts, FilterRules[opts, Options[DSLTranslation]]];
      If[Length[langRuleSpec] > 0,
        If[Length[langRuleSpec] > 1,
          ResourceFunction["ResourceFunctionMessage"][DSLTranslation::alng,
            langRuleSpec[[1, 1]], langRuleSpec[[1, 2]]]
        ];
        opts =
            Join[{"FromLanguage" -> langRuleSpec[[1, 1]],
              "ToLanguage" -> langRuleSpec[[1, 2]]}, opts]
      ];

      (*Source*)
      source = "Source" /. opts /. Options[DSLTranslation];

      If[TrueQ[source === Automatic], source = "Web"];
      If[! (StringQ[source] &&
          MemberQ[{"web", "cli", "shell"}, ToLowerCase[source]]),
        ResourceFunction["ResourceFunctionMessage"][DSLTranslation::nfmt];
        Return[$Failed]
      ];

      If[StringQ[source],
        source = ToLowerCase[source]
      ];

      (*Format*)
      fmt = "Format" /. opts /. Options[DSLTranslation];

      If[TrueQ[fmt === Automatic], fmt = "CODE"];
      If[TrueQ[fmt === Association], fmt = "JSON"];
      If[! (StringQ[fmt] &&
          MemberQ[{"code", "json", "ast", "association"}, ToLowerCase[fmt]]),
        ResourceFunction["ResourceFunctionMessage"][DSLTranslation::nsrc];
        Return[$Failed]
      ];
      If[ToLowerCase[fmt] == "association", fmt = "JSON"];
      fmt = ToUpperCase[fmt];

      (*Shell session*)
      shellSession = "ShellSession" /. opts /. Options[DSLTranslation];
      If[! (shellSession != "Shell" ||
          TrueQ[Head[shellSession] === ExternalSessionObject] ||
          TrueQ[shellSession === Automatic]),
        ResourceFunction["ResourceFunctionMessage"][DSLTranslation::nss];
        shellSession = "Shell"
      ];
      If[TrueQ[shellSession === Automatic], shellSession = GetShellSession[]];

      (*CopyToClipboard*)
      cpcbQ = TrueQ["CopyToClipboard" /. opts /. Options[DSLTranslation]];
      If[cpcbQ && fmt == "CODE",
        opts = Prepend[opts, "DeferringFunction" -> "Defer"]
      ];

      (*Get the code*)
      Which[
        MemberQ[{"web"}, source],
        res =
            DSLWebServiceInterpretation[command,
              FilterRules[opts, Options[DSLWebServiceInterpretation]]],

        True,
        res =
            DSLCLIInterpretation[command,
              FilterRules[opts, Options[DSLCLIInterpretation]]]
      ];

      If[TrueQ[res === $Failed], Return[$Failed]];

      res =
          Which[
            fmt == "JSON", res,
            fmt == "AST", res,
            True, res[fmt]
          ];

      If[cpcbQ, CopyToClipboard[res]];

      res
    ];

DSLTranslation[___] := (ResourceFunction["ResourceFunctionMessage"][
  "The first argument is expected to be string; the second optional \
argument is expected to be a to-language specification or a rule of from- and \
to-language specification."]; $Failed);

End[];
EndPackage[];
