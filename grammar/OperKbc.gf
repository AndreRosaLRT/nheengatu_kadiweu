 {-Extension GrammKbc (Kadiwéu) ofGrammYEP - a multilingual computational grammar for Nheengatu, English, and Portuguese
 (c) 2020 Leonel Figueiredo de Alencar
 Licensed under the terms of the GNU Lesser General Public License, version 2.1
 See LICENSE or visit the URL
 https://www.gnu.org/licenses/old-licenses/lgpl-2.1.en.html

-}

resource OperKbc = open Oper, Prelude in { 
flags coding=utf8 ;
param
    
	--PARAMS KBC

	-- PARAMS FOR THE NOUN
	--Person = P1 | P2 | P3; from oper
	--Number = Sg|Pl ; from de oper
	Alienability = Alnbl | Inlnbl | NoAlnbl ; --(NOT APPLICABLE) ; --Alienable or not (prefix ) (CLASS I or II in the thesis) -- could be a Bool?
	SufClassifier = AnimPlant| Cultivated | Instrum | ActorNmnlzr | UndefClassfr ; --(Animals or plants | cultivated plants | Instruments | actor in nominalizations - e.g. speaker )
	PsorNum = PsorSg|PsorPl|NoPsorNum ;
	PsorPers = PsorP1|PsorP2|PsorP3|Undef|NoPoss; --(NOT APPLICABLE) ;

	-- PARAMS FOR THE DEMONSTRATIVES
	Position = Standing|Sitting|Lying|Coming|Going;
	Presence = Absent|Present;

	--PARAMS FOR THE (PREDICATE): 
	--POLARITY
	Polarity = Pos|Neg;
	
	--params verb
	AspectPre = Completive | Incompletive | Durative | AspPreNone ; -- -7 --Completed action|incompleted action|
    NegationMain = NegMain | PositMain ; -- -6
    NegationSubord = NegSub | NegCondImp | PositSubord ; -- -6

    Mood = Ind | Imp | Cond | Desid ; -- -5
    Person = P1 | P2 | P3 | P3Unacc | Impers | PersonNone ; -- -3
    Number = Sg | Pl ; -- -4 (sujeito)
    Reflexive = Refl | NonRefl ; -- -2
    Directional = Hither | NoDir ; -- -1 (n- "hither")
    Valence = Trans | Intrans ; -- 1
    AspectPost = Telic | Atelic |ExclusiveAtelic|AspPostNone;
	AspectPostClitic = Repetitive | Intensive | AspPostClitNone ; -- 2
    NumberPost = ObjSg | ObjPl | ObjNone ; -- 3
    Motion = GoingMot | Straight | Together | Inside | Against | Apart | MotNone ; -- 4
    Direction = Outward | Upward | Inward | Toward | Backward | Downward | AbsentDir | DirNone ; -- 5
    CliticIndObj = IndObj1sg | IndObj2sg | IndObj1pl | IndObj2pl | IndObj3 | ClIndNone ; -- 6
    CliticPlural = PlurNiwak | PlurWaji | PlurBoth | ClPlurNone ; -- 7

	--PARAMS QUALITY

	QualIntransVerb = True|False; --a param to control if the quality is realized as a noun or as an intransitive verb
	
  oper
	
--Types 
--NOUN
		--TO DO
		--Still need to implement types for other 2 sufixes (diminutive and nominalizer)
		--In the near future, I might have to include inherent features of the noun (case??might enherit from the verb); gender seems not to be a feature of the noun
	Noun : Type = {
		s : Alienability=>PsorPers => PsorNum => SufClassifier =>Number =>Str; 
		 g:Gender};
 	NounPhrase : Type = {
		s : Str;
		g : Gender;
		n : Number;
		};
	
--VERB
	
	{-Verb : Type ={ s: AspectPre => Negation => Mood => Person Number=>Reflexive=>Directional=>Valence=>AspectPost=>NumberPost=>Motion=>Direction=>CliticIndObj=>CliticPlural=>Str}; 
	-}
	
	--paramset variables
	NounParamSet :Type ={
		alienability : Alienability;
		psorPers : PsorPers;
		psorNum : PsorNum;
		sufClassifier : SufClassifier;
		number : Number
	};

	defaultNounParamSet : NounParamSet ={
		alienability = NoAlnbl;
		psorPers = NoPoss;
		psorNum = NoPsorNum;
		sufClassifier = UndefClassfr;
		number = Sg
	};

	customNounParamSet :  NounParamSet ={
		alienability = NoAlnbl;
		psorPers = NoPoss;
		psorNum = PsorSg;
		sufClassifier = UndefClassfr;
		number = Sg
	};

	customNounParamSet2 :  NounParamSet ={
		alienability = Inlnbl;
		psorPers = PsorP1;
		psorNum = PsorSg;
		sufClassifier = UndefClassfr;
		number = Sg
	};

	SIMPLEKIND_KBC = Noun ;
	KIND_KBC = Noun ;  --What would be realized as Adjective (quality) in english is realized as noun or intransitive verb
	
	--QUAL_KBC : Type = { v : Verb ; n : Noun };
		
	{-mkQualKbc = overload {
		mkQualKbc :Str-> Verb = \lema-> mkVerb lema ;
		mkQualKbc :Str -> Gender -> Noun = \lema,gender-> mkNoun lema gender ;
	} ;-}

	{-mkQualKbc : Str -> Gender -> QualIntransVerb -> QUAL_KBC = \root, g, isVerb ->
      case isVerb of {
        True => {
          v = mkVerb root ;
          n = { s = table { _ => table { _ => table { _ => table { _ => table { _ => "" } } } } } ; g = g } -- Noun vazio
        } ;
        False => {
          v = { s = table { _ => table { _ => "" } } } ; -- Verb vazio
          n = mkNoun root g
        }
      } ;-}

    -- STATE_KBC extends mkQualKbc with an additional field
    --STATE_KBC : QualIntransVerb -> Type = \qiv ->
       -- mkQualKbc qiv ** {l : Level};
	
	ITEM_KBC = NounPhrase;
	ALNBL:Type = {s : Alienability=>Str}; --type for alienability prefix
	PSORPREF: Type = {s : PsorPers => PsorNum =>  Str}; -- Type for possessor prefixes
	CLASSFSUFIX:Type ={s : SufClassifier=>Str};   --Type for classifier sufixes
	NUMBERSUFIX : Type = {s : Number => Str};  --Type for number sufixes
	POLARITY: Type = {s: Polarity => Str};

--mk/* funs

	--helper function to get Noun
	getNounForm : Noun -> NounParamSet -> Str =  --
		\noun, params ->
			let fullparams = {
				alienability = case params.alienability of { NoAlnbl => defaultNounParamSet.alienability ; _ => params.alienability };
				psorPers = case params.psorPers of { NoPoss => defaultNounParamSet.psorPers ; _ => params.psorPers};
				psorNum = case params.psorNum of { PsorSg => defaultNounParamSet.psorNum ; _ => params.psorNum};
				sufClassifier = case params.sufClassifier of { UndefClassfr => defaultNounParamSet.sufClassifier ; _ => params.sufClassifier};
				number = case params.number of { Sg => defaultNounParamSet.number ; _ => params.number}

			};
			in
			noun.s ! fullparams.alienability
			   ! fullparams.psorPers
			   ! fullparams.psorNum
			   ! fullparams.sufClassifier
			   ! fullparams.number;
	
	
	demonstDet = overload{  --Functions to linearize Noun Groups (Dmonstratives + Noun) -- I might have to revise it (implementing other Cat function on ABSTRACT GRA.gf)
		
		demonstDet : Number -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Presence -> Position -> NounParamSet->Noun -> NounPhrase =
			\n, ka, niGida, niGini, niGidi, niGina, niGijo, naGada, naGani, naGadi, naGana, naGajo, niGidiwa, presence, position, nounparams, k_noun-> {
			s = case n of {
			Sg => case k_noun.g of {
				Masc => case presence of {
				Absent  => ka;
				Present => case position of {
					Standing => niGida;
					Sitting  => niGini;
					Lying    => niGidi;
					Coming   => niGina;
					Going    => niGijo
				}
				};
				Fem => case presence of {
				Absent  => ka;
				Present => case position of {
					Standing => naGada;
					Sitting  => naGani;
					Lying    => naGadi;
					Coming   => naGana;
					Going    => naGajo
				}
				}
			};
			Pl => niGidiwa
			} ++ (getNounForm k_noun nounparams);

			g = k_noun.g;  -- Default gender
			n = n          -- Default number
		};

		demonstDet :  Number-> Presence -> Position -> NounParamSet -> Noun -> NounPhrase =
			\ n, pres, posit,nounparams , k_noun-> {
			s=
			let
				genderMorph = case k_noun.g of {
				Masc => "niGi";
				Fem  => "naGa"
				};

				presencePositionMorph = case <pres, posit> of {
				<Absent, _>      => ("ca"|"ka");   --I have to check if ica and ika are the same (which is the demonstrative?)
				<Present, Standing> => "da";
				<Present, Sitting>   => "ni";
				<Present, Lying>     => "di";
				<Present, Coming>    => "na";
				<Present, Going>     => "jo"
				};

				pluralMorph = "niGidiwa";

				-- Usa a função auxiliar com parâmetros padrão
				nounForm = getNounForm k_noun nounparams;
			in

			-- Resolve final string based on number
			
			case n of {
				Sg => genderMorph + presencePositionMorph ++ nounForm;
				Pl => pluralMorph ++ nounForm
				};
				g = k_noun.g;  -- Default gender
				n = n;  -- Default number
			
			}

		};
		
	
	mkAlienability : ALNBL =  --helper mk to realization of the alienability sufix;
		{s = table {Alnbl=>"n";_=>""};
		};
	mkNumSuf : NUMBERSUFIX =            ---helper function to superficial realization of the kbc number sufix
		{
			s = table{
				Sg => "";
				Pl=>("adi") --still a general realization of the variations of plural sufixes (will implement specificities: it is lexically oriented)
			--Pl=>("adi"|"pi"|"Ga"|"dodi"|"ali"|"tedi")	
			}
		};
	mkPolarity : POLARITY = {
		s= table {
			Pos => "teste_posit";
			Neg => "teste_neg"
			
		}
	};
	mkPsorPref :  PSORPREF =  -- helper to Make possessor prefixes (possessor person and number to str)
		{
			s = table {
			PsorP1 => table {
				NoPsorNum => ""; -- Handle NoPsorNum explicitly
				PsorSg => "i";
				PsorPl => "God"
				  
			};
			PsorP2 => table {
				NoPsorNum => "";
				PsorSg => "Gad";  -- Assuming singular
				PsorPl => "Gad"  -- Assuming plural
				   -- Handle NoPsorNum explicitly
			};
			PsorP3 => table {
				NoPsorNum => "";
				PsorSg => "l";
				PsorPl => "l"
				   -- Handle NoPsorNum explicitly
			};
			Undef => table {
				NoPsorNum => "";
				PsorSg => "e";    -- Undefined person suffix, singular
				PsorPl => "e"    -- Undefined person suffix, plural
				   -- Handle NoPsorNum explicitly
			};
			NoPoss => table {
				_ => ""           -- No possessor implies no prefix
			}
			};
		};

	
	mkClassSuf : CLASSFSUFIX =  --helper function to make the classifier sufix 
		{
			s = table {
				UndefClassfr => "";
				AnimPlant => "nigo";      -- Animals/plants
				Cultivated => "ija";         -- Cultivated plants
				Instrum => "GanGa";       -- Instruments
				ActorNmnlzr => "Gikajo" -- Actor nominalizer
				 --;
				--_ => ""                 -- Default (no classifier)

			};
		};
	adjustAlienability : Str -> Str -> Str = \marker, next ->
		case marker of {
		"n" => case next of {
			"n" + _ => "" ; -- Remove "n" if root starts with "n"
			_ => "n"
		} ;
		_ => marker -- Keep other markers unchanged
		} ;
	
	mkNoun : Str -> Gender -> Noun = \root, g ->   --I may have to implement inherent features as gender, case ... -- ( Alienability=>PsorPers => PsorNum => SufClassifier =>Number=> Str)
		{
			s = table {
				alienability => table {
					psorPers => table {
						psorNum => table {
							sufClassifier => table {
								sufixNumber =>
									let
										possPrefix = (mkPsorPref).s ! psorPers ! psorNum;   -- Prefixo possessivo
										alienabilityMarker = (mkAlienability).s ! alienability;  -- Marcador de alienabilidade
										sufixClassifier = (mkClassSuf).s ! sufClassifier;  -- Sufixo do classificador
										sufixNumber = (mkNumSuf).s ! sufixNumber; -- Sufixo de número
										--Adjusting Alienability prefix
										adjustedAlienabilityMarker = adjustAlienability alienabilityMarker root ;
										-- Adjusting psor prefix
										adjustedPsorPrefix = case possPrefix of {
											"l" => pre {
												("t"|"d"|"d:"|"n"|"n:"|"l"|"l:"|"T"|"D"|"D:"|"N"|"N:"|"L"|"L:") => "";  -- Remove "l-" para consoantes alveolares
												_ => "l"  -- Mantém "l-" caso contrário
											};
											"God" => pre {
												("b"|"c"|"d"|"f"|"g"|"h"|"j"|"k"|"l"|"m"|"n"|"p"|"q"|"r"|"s"|"t"|
												"v"|"w"|"x"|"y"|"z"|"B"|"C"|"D"|"F"|"G"|"H"|"J"|"K"|"L"|"M"|"N"|
												"P"|"Q"|"R"|"S"|"T"|"V"|"W"|"X"|"Y"|"Z") => "Go";
												_ => "God"
											};
											"Gad" => pre {
												("b"|"c"|"d"|"f"|"g"|"h"|"j"|"k"|"l"|"m"|"n"|"p"|"q"|"r"|"s"|"t"|
												"v"|"w"|"x"|"y"|"z"|"B"|"C"|"D"|"F"|"G"|"H"|"J"|"K"|"L"|"M"|"N"|
												"P"|"Q"|"R"|"S"|"T"|"V"|"W"|"X"|"Y"|"Z") => "Ga";
												_ => "Gad"
											};
											_ => possPrefix  -- Caso padrão
										};
									in
										-- Retorna a palavra composta
										adjustedPsorPrefix + adjustedAlienabilityMarker + root + sufixClassifier + sufixNumber --REVISE: have to check for the plural sufix concat morphemic rules (eg., a + adi = adi) 
							}
						}
					}
				}
			};
			g = g -- Gênero
		};


	mkKindKbc : SIMPLEKIND_KBC -> KIND_KBC = \sk -> {s = sk.s; g = sk.g};
	--PATTERNS
		vowel : pattern Str = #("a"|"e"|"i"|"o"|"u"|"A"|"E"|"I"|"O"|"U") ;
		consonant : pattern Str = #("b"|"c"|"d"|"f"|"g"|"h"|"j"|"k"|"l"|"m"|"n"|"p"|"q"|"r"|"s"|"t"|"v"|"w"|"x"|"y"|"z"|"B"|"C"|"D"|"F"|"G"|"H"|"J"|"K"|"L"|"M"|"N"|"P"|"Q"|"R"|"S"|"T"|"V"|"W"|"X"|"Y"|"Z") ;
		alveolar_consonant:  pattern Str = #("t"|"d"|"d:"|"n"|"n:"|"l"|"l:"|"T"|"D"|"D:"|"N"|"N:"|"L"|"L:") ; -- Need to check if this pattern for alveolar (dental?) is
		--PATTERNS dont seem to be working as expected
	
	mkProperNameKbc : Str-> Gender -> SIMPLEKIND_KBC = \name, gender -> mkNoun name gender ; --pretty much the same as using mkNoun directly (I migh have to adapt this function in future)
	

	-- MORPHOLOGICAL OPERATIONS OF THE VERB

    mkAspectPre : { s : AspectPre => Str } = 
		{s = table { Completive => "jaG" ; Incompletive => "baGa" ; Durative => "banaGa" ; AspPreNone => "" }
		} ; --(-7)
	
	mkNegationMain : { s : NegationMain => Str } = {
		s = table { NegMain => "aG" ;  PositMain => "" }
		} ; --(-6) 

	mkNegationSubord:{s: NegationSubord=> Str } = {
		s= table{ NegSub => "daGa" ; NegCondImp => "nGa" ; PositSubord => ""} }; --THIS NEGATION TAKES EFFECT ON THE CLAUSE RANK (NOT AS MORPHEME OF VERB)
	
	mkMood : { s : Mood => Str } = {
      s = table {Cond => "dGa" ; Desid => "domaGa"; _ => ""  }
    } ;--(-5)
	mkAspectPost : { s : AspectPost => Str } = {
      s = table { Telic => "g" ; Atelic => "" ; ExclusiveAtelic => "d" ; AspPostNone => "" }
    } ; --(+2)

	mkAspectPostClitic : { s : AspectPostClitic => Str } = {
      s = table {Repetitive => "ak" ; Intensive => "bigi" ; AspPostClitNone => "" }
    } ; --(+4)

    mkPerson : Person => Valence => Person =>Str=> Str = \subj, val, obj, root -> -- Hierarquia
      case <val, subj, obj> of {
		<Trans, P1, P3> => case root of { 
			("t"|"d"|"n") + _ => "i" ; _ => "y" } ;
		<Trans, _, P3> => case root of { 
          ("p"|"b"|"t"|"d"|"k"|"g") + _ => "" ; 
          "a" + _ => "w" ; 
          "n" + _ => "a" ; 
          _ => "y" 
        } ;
--PAREI AQUI




        <Intrans, P1, PersonNone> => case root of { ("t"|"d"|"n") + _ => "i" ; _ => "j" } ;
        <Intrans, P2, PersonNone> => "a-" ;
        <Intrans, P3, PersonNone> => case root of { 
          ("p"|"b"|"t"|"d"|"k"|"g") + _ => "" ; 
          "a" + _ => "w" ; 
          "n" + _ => "a" ; 
          _ => "y" 
        } ;
        <Intrans, P3Unacc, PersonNone> => "n" ;
        <Intrans, Impers, PersonNone> => "eti" ;
        
		
		
		
		
        
		
		
		
		<Trans, P2, P1> => "a" ;
        <Trans, _, P1> => "i" ;
        <Trans, _, P2> => "Ga" ;
        
        _ => ""
      } ;

    {-
    
    mkNumberPre : { s : Number => Str } = {
      s = table { Sg => "" ; Pl => "di-" } -- Placeholder
    } ;
    mkReflexive : { s : Reflexive => Str } = {
      s = table { Refl => "si-" ; NonRefl => "" }
    } ;
    mkDirectional : { s : Directional => Str } = {
      s = table { Hither => "n-" ; NoDir => "" }
    } ;
    mkValence : { s : Valence => Str } = {
      s = table { Trans => "-ti" ; Intrans => "" } -- Placeholder
    } ;
    
    mkNumberPost : { s : NumberPost => Str } = {
      s = table { ObjSg => "" ; ObjPl => "-di" ; ObjNone => "" }
    } ;
    mkMotion : { s : Motion => Str } = {
      s = table { 
        Going => "+jo" ; 
        Straight => "+ko" ; 
        Together => "+wag" ; 
        Inside => "+n" ; 
        Against => "+get" ; 
        Apart => "+kowak" ; 
        MotNone => "" 
      }
    } ;
    mkDirection : { s : Direction => Str } = {
      s = table { 
        Outward => "+ke" ; 
        Upward => "+bigim" ; 
        Inward => "+w" ; -- ou +wei, ajustar
        Toward => "+gi:" ; 
        Backward => "+we" ; 
        Downward => "+nigi" ; -- ou +n:, ajustar
        Absent => "+ka" ; 
        DirNone => "" 
      }
    } ;
    mkCliticIndObj : { s : CliticIndObj => Str } = {
      s = table { 
        IndObj1sg => "+i" ; 
        IndObj2sg => "+Ga" ; 
        IndObj1pl => "+Go" ; 
        IndObj2pl => "+Ga+i" ; 
        IndObj3 => "+e" ; 
        ClIndNone => "" 
      }
    } ;
    mkCliticPlural : { s : CliticPlural => Str } = {
      s = table { 
        PlurNiwak => "+niwak" ; 
        PlurWaji => "+waji" ; 
        PlurBoth => "+niwak+waji" ; 
        ClPlurNone => "" 
      }
    } ;
	
	
	-- Ajuste fonológico para clíticos
    adjustClitic : Str -> Str -> Str = \base, clitic ->
      case clitic of {
        ("+jo"|"+ko"|"+wag"|"+n"|"+get"|"+kowak"|"+ke"|"+bigim"|"+w"|"+gi:"|"+we"|"+nigi"|"+ka"|"+i"|"+Ga"|"+Go"|"+Ga+i"|"+e"|"+niwak"|"+waji"|"+niwak+waji") => 
          case base of { 
            _ + ("m"|"n"|"l"|"r") => base ++ clitic ; -- Deleção de sonorante implícita
            _ => base ++ clitic 
          } ;
        _ => base ++ clitic
      } ;

    -- Construtor de Verbos
    mkVerb : Str -> Verb = \root -> {
      s = table {
        aspPre => table {
          neg => table {
            mood => table {
              pers => table {
                numPre => table {
                  refl => table {
                    dir => table {
                      val => table {
                        aspPost => table {
                          numPost => table {
                            mot => table {
                              dirCl => table {
                                clInd => table {
                                  clPlur =>
                                    let
                                      -- Prefixos (-7 a -1)
                                      aspectPre = (mkAspectPre).s ! aspPre ;
                                      negation = (mkNegation).s ! neg ;
                                      moodMark = (mkMood).s ! mood ;
                                      person = mkPerson pers val (case val of { Trans => P3 ; _ => None }) ;
                                      numberPre = (mkNumberPre).s ! numPre ;
                                      reflexive = (mkReflexive).s ! refl ;
                                      directional = (mkDirectional).s ! dir ;
                                      -- Sufixos/Clíticos (1 a 7)
                                      valence = (mkValence).s ! val ;
                                      aspectPostMark = (mkAspectPost).s ! aspPost ;
                                      numberPostMark = (mkNumberPost).s ! numPost ;
                                      motion = (mkMotion).s ! mot ;
                                      direction = (mkDirection).s ! dirCl ;
                                      cliticIndObj = (mkCliticIndObj).s ! clInd ;
                                      cliticPlural = (mkCliticPlural).s ! clPlur ;
                                      -- Aplicar ajustes fonológicos
                                      rootWithAspect = adjustClitic root aspectPostMark ;
                                      rootWithMotion = adjustClitic rootWithAspect motion ;
                                      rootWithDirection = adjustClitic rootWithMotion direction ;
                                      rootWithClitics = adjustClitic rootWithDirection cliticIndObj
                                    in
                                      aspectPre + negation + moodMark + numberPre + 
                                      person + reflexive + directional + rootWithClitics + 
                                      valence + numberPostMark + motion + direction + cliticPlural
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    } ;
	
	
	-}
	
	
	--TESTING AREAAAAA
	{-Man = (mkNoun "Gonel:e:giwa" Sg Masc).s!NoAlnbl!NoPoss!NoPsorNum!UndefClassfr;-}
	--Teste = getNounForm (mkNoun "Gonel:e:giwa" Masc) defaultNounParamSet;
	--Teste2 = getNounForm (mkNoun "Gonel:e:giwa" Masc) customNounParamSet;
    --This kind = demonstDet kind Masc Present Standing;
	--Teste3 = mkNoun "Gonel:e:giwa" Masc;
	TesteDem = demonstDet Sg  "ka" "ida" "ini" "idi" "ina" "ijo" "ada" "ani" "adi" "ana" "ajo" "idiwa" Present Coming defaultNounParamSet (mkNoun "Gonelegiwa" Masc );
	TesteDem2 = demonstDet Sg Present Coming defaultNounParamSet (mkNoun "Gonelegiwa" Masc );
	teste_getnoun = getNounForm ( mkNoun "Gonelegiwa" Masc) customNounParamSet;
	teste_getnoun = getNounForm ( mkNoun "Gonelegiwa" Masc) customNounParamSet;
	

	{-mkNoun2 : Str -> Gender -> Alienability -> PsorPers -> PsorNum -> SufClassifier -> Number -> Noun = 
    \root, g, alienabilityParam, psorPersParam, psorNumParam, sufClassifierParam, nParam ->   --I may have to implement inherent features as gender, case ... -- ( Alienability=>PsorPers => PsorNum => SufClassifier =>Number=> Str)
			{
				s = table {
					alienability => table {
						psorPers => table {
							psorNum => table {
								sufClassifier => table {
									sufixNumber =>
										let
											possPrefix = (mkPsorPref).s ! psorPers ! psorNum;   -- Prefixo possessivo
											alienabilityMarker = (mkAlienability).s ! alienability;  -- Marcador de alienabilidade
											sufixClassifier = (mkClassSuf).s ! sufClassifier;  -- Sufixo do classificador
											sufixNumber = (mkNumSuf).s ! sufixNumber; -- Sufixo de número

											-- Ajuste do prefixo possessivo
											adjustedPsorPrefix = case possPrefix of {
												"l" => pre {
													("t"|"d"|"d:"|"n"|"n:"|"l"|"l:"|"T"|"D"|"D:"|"N"|"N:"|"L"|"L:") => "";  -- Remove "l-" para consoantes alveolares
													_ => "l"  -- Mantém "l-" caso contrário
												};
												"God" => pre {
													("b"|"c"|"d"|"f"|"g"|"h"|"j"|"k"|"l"|"m"|"n"|"p"|"q"|"r"|"s"|"t"|
													"v"|"w"|"x"|"y"|"z"|"B"|"C"|"D"|"F"|"G"|"H"|"J"|"K"|"L"|"M"|"N"|
													"P"|"Q"|"R"|"S"|"T"|"V"|"W"|"X"|"Y"|"Z") => "Go";
													_ => "God"
												};
												"Gad" => pre {
													("b"|"c"|"d"|"f"|"g"|"h"|"j"|"k"|"l"|"m"|"n"|"p"|"q"|"r"|"s"|"t"|
													"v"|"w"|"x"|"y"|"z"|"B"|"C"|"D"|"F"|"G"|"H"|"J"|"K"|"L"|"M"|"N"|
													"P"|"Q"|"R"|"S"|"T"|"V"|"W"|"X"|"Y"|"Z") => "Ga";
													_ => "Gad"
												};
												_ => possPrefix  -- Caso padrão
											};
										in
											-- Retorna a palavra composta
											adjustedPsorPrefix + alienabilityMarker + root + sufixClassifier + sufixNumber
								}
							}
						}
					}
				};
				g = g -- Gênero
			};-}
 
  {-
	mkNoun : Str -> Gender -> Noun = \root, g ->   --I may have to implement inherent features as gender, case ... -- ( Alienability=>PsorPers => PsorNum => SufClassifier =>Number=> Str)
			{
				s = table {
					alienability => table {
						psorPers => table {
							psorNum => table {
								sufClassifier => table {
									sufixNumber =>
										let
											possPrefix = (mkPsorPref).s ! psorPers ! psorNum;   -- Prefixo possessivo
											alienabilityMarker = (mkAlienability).s ! alienability;  -- Marcador de alienabilidade
											sufixClassifier = (mkClassSuf).s ! sufClassifier;  -- Sufixo do classificador
											sufixNumber = (mkNumSuf).s ! sufixNumber; -- Sufixo de número

											-- Ajuste do prefixo possessivo
											adjustedPsorPrefix = case possPrefix of {
												"l" => pre {
													("t"|"d"|"d:"|"n"|"n:"|"l"|"l:"|"T"|"D"|"D:"|"N"|"N:"|"L"|"L:") => "";  -- Remove "l-" para consoantes alveolares
													_ => "l"  -- Mantém "l-" caso contrário
												};
												"God" => pre {
													("b"|"c"|"d"|"f"|"g"|"h"|"j"|"k"|"l"|"m"|"n"|"p"|"q"|"r"|"s"|"t"|
													"v"|"w"|"x"|"y"|"z"|"B"|"C"|"D"|"F"|"G"|"H"|"J"|"K"|"L"|"M"|"N"|
													"P"|"Q"|"R"|"S"|"T"|"V"|"W"|"X"|"Y"|"Z") => "Go";
													_ => "God"
												};
												"Gad" => pre {
													("b"|"c"|"d"|"f"|"g"|"h"|"j"|"k"|"l"|"m"|"n"|"p"|"q"|"r"|"s"|"t"|
													"v"|"w"|"x"|"y"|"z"|"B"|"C"|"D"|"F"|"G"|"H"|"J"|"K"|"L"|"M"|"N"|
													"P"|"Q"|"R"|"S"|"T"|"V"|"W"|"X"|"Y"|"Z") => "Ga";
													_ => "Gad"
												};
												_ => possPrefix  -- Caso padrão
											};
										in
											-- Retorna a palavra composta
											adjustedPsorPrefix + alienabilityMarker + root + sufixClassifier + sufixNumber
								}
							}
						}
					}
				};
				g = g -- Gênero
			};
	-}
	

	{-
	OPERS YRL
	FORM : Type = Case => NClass => Str ;
	DETNOUN : Type = {dem: Str; head: NForm => Str} ;
	KIND_YRL : Type = {s: Number => NForm => Str} ;
	SIMPLEKIND : Type = KIND_YRL ** {nc: NClass} ;
	QUAL_YRL : Type = {s : Number => Person => Str; c: Class} ** {v: Verbal} ** {nc: NClass};
	LOCATION : Type = QUAL_YRL;
	PROPERTY : Type = QUAL_YRL ;
	STATE_YRL : Type = QUAL_YRL ** {l: Level }  ;
	ITEM_YRL:Type={s:Class=>Level=>FORM; n: Number; p: Person}**{pos: POS};
	VAR_YRL : Type = ITEM_YRL ;
	NONVAR_YRL : Type = {s: DETNOUN ; n : Number; p: Person} ** {pos: POS} ;
	DEF_ITEM_YRL : Type = {s: Str ; n: Number ; p: Person ; pf: PsorForm}  ;
	POSSPRO : Type = {s : NClass => Str ; n: Number ; p: Person ; pf: PsorForm }  ;
	POSSKIND : Type = KIND ;
	DEF : Type = EXPR ** {n: Number};

	
	NumPersPsorForm: Number => Person => PsorForm = 
		   table {
			Sg => table { P3 => SG3 ; P1|P2 => NSG3} ; 
			Pl => table { P1|P2|P3 => NSG3}
			} ;

	POSNumPersPsorForm: POS => Number => Person => PsorForm = 
	       	  table {
			Pron => table {
			     	      Sg => table { P3 => SG3 ; P1|P2 => NSG3} ; 
			      	      Pl => table { P1|P2|P3 => NSG3}
				      };
			Noun => \\num,pers => NSG3
			      } ;

	mkPossPron = overload {  TODO: second variant is not really necessary, see function calls in GraYrl.gf
    		   mkPossPron : Number -> Person -> POSSPRO = \num,pers -> let n: Number = num; p: Person = pers; pf: PsorForm = NumPersPsorForm!n!p ; f : NClass => Str = SecondClassPron ! n ! p in {s = f; n = n ; p = p ; pf=pf } ;
		   mkPossPron : POSSPRO =  {s = table {NCI => "i" ; NCS => "" }; n = Sg ; p = P3 ; pf = SG3 } ;
		   } ;

		   mkAdvLoc : Str -> LOCATION = \form -> {s= \\n,p => form ; c = C1 ; v = False ; nc = NCI};

		  mkPP : Str -> NClass -> ITEM_YRL -> LOCATION = \postp,nc,item -> let np :  FORM  = item.s ! C1 ! Ind in {s= \\n,p => np ! Gen ! nc ++ postp ; c = C1 ; v = False ; nc = nc};

	mkLocYrl = overload { 
		 mkLocYrl : Str -> ITEM_YRL -> LOCATION = \postp,item -> 
		   mkPP postp NCI item;

		  mkLocYrl : Str -> NClass -> ITEM_YRL -> LOCATION = \lemma,nc,item ->
 		  let 
		stem: Str = StemRLemma lemma; 
		pf: PsorForm = POSNumPersPsorForm ! item.pos 
		    	       		      ! item.n 
					      ! item.p;		
		form: Str = table {SG3 => "s" + stem; NSG3 => lemma} ! pf 
		in mkPP form nc item ;
		  };
    

    mkPropYrl : QUAL_YRL -> QUAL_YRL = \qual -> {s = qual.s ; c = qual.c ; v = qual.v ; nc = qual.nc} ;
   
    properNameYrl : Str -> SIMPLEKIND = \name -> regNounYrl name ;
    
    detYrl = overload {
    	   detYrl : DEF -> SIMPLEKIND -> DEF_ITEM_YRL = \d,k -> {s = d.s ++ k.s ! d.n ! NAbs ; n = d.n ; p = P3 ; pf = NSG3 } ;

	    detYrl : Str -> Number -> POSSKIND -> DEF_ITEM_YRL = \f,n,k -> {s = f ++  k.s ! n ; n = n ; p = P3 ; pf = NSG3 } ;
	    
	   detYrl : Number -> Str -> KIND_YRL -> NONVAR_YRL = 
           	  \num,det,kind -> let  forms: NForm => Str = ExtractNounForms num kind in
		  {s = {dem=det ; head = forms } ; 
		  n = num ; 
		  p = P3 ;
		  pos = Noun };
    	   detYrl : Str -> KIND_YRL -> NONVAR_YRL = 
           	  \det,kind -> let 
		  nsg: NForm => Str = ExtractNounForms Sg kind;
		  npl: NForm => Str = ExtractNounForms Pl kind;
		  x: DETNOUN = {dem = det + "-itá" ; head = nsg } ; 
		  y: DETNOUN = {dem = det  ; head = npl } 
		  in 
		  {s = x | y ; n = Pl; p = P3 ; pos = Noun } ;
	};

	ExtractNounForms: Number -> KIND_YRL -> NForm => Str = \num,kind -> let ks: Number => NForm => Str = kind.s in ks ! num ;

	mkItemYrl = overload {

TODO: simplify this (overload group with only one element)

    mkItemYrl : VAR_YRL -> ITEM_YRL = \var -> {s = var.s ; n = var.n ; p = var.p ; pos = var.pos } ;

    mkItemYrl : NONVAR_YRL -> ITEM_YRL = \nonvar -> {s = \\c,l,cs,nc => (nonvar.s).dem ++ ((nonvar.s).head ! NAbs) ; n =nonvar.n; p = nonvar.p ; pos = nonvar.pos } ;
 
    	      };

    mkKindYrl : SIMPLEKIND -> KIND_YRL = \sk -> {s = sk.s } ;
    mkKindYrl_ : POSSKIND -> KIND_YRL = \pk -> let sgForm: Str = pk.s ! Sg ; plForm: Str = pk.s ! Pl in {s = table {Sg => \\nf => sgForm ; Pl => \\nf => plForm} } ;

    pronYrl : Number -> Person -> Str -> ITEM_YRL = \num,pers,form -> {s = pronForm num pers form ; n = num; p = pers; pos = Pron } ;

    pronForm : Number -> Person -> Str -> Class => Level => FORM = \num, pers, form -> 
    let 
    fullPron : FORM = 
    	      
    	      table {Nom => \\nc => form; Gen => SecondClassPron ! num ! pers }
	      ; 
    emptyPron : FORM = \\cs,nc => "" 
    in 
       table { 
    	     C1 => table {Stage => fullPron | emptyPron ; Ind => fullPron };
	     C2 => table {_ => fullPron | emptyPron }}
    ;
    
    regNounYrl : Str -> SIMPLEKIND = 
      \n -> {s= table {Sg => \\f => n ; Pl => \\f => (n + "-itá")} ; nc = NCI };

      TODO: this operation seems redundant, since belo,bela,belos,belasorm the same job
      StemRLemma : Str -> Str = \lemma -> case lemma of {
      		  "r" + stem => stem ;
      		  _ => lemma
      		  };
      ExtractStem_old : Str -> Str = \lemma -> case lemma of {
      		  pref@("t"|"s") + stem => stem ;
      		  _ => lemma
      		  };

	ExtractStem : Str -> Str = \lemma -> case lemma of {
      		 "t"|"s"|"r" + stem => stem ;
      		  _ => lemma
      		  };
		  
      GetInfix : Str -> Str = \stem -> case stem of {
      		  "a"|"e"|"i"|"o"|"u" + rest => "" ;
      		  _ => "a"
      		  };

mkRelPrefN : Str -> Str -> Str -> SIMPLEKIND = \taiti,raiti,saiti -> let  pl: Str = "-itá" in
		  {s= table {
    		      Sg => table {
		      	 NRel SG3 => saiti ; 
		      	 NRel NSG3 => raiti; 
		      	 NAbs => taiti } ; 
    		      Pl => table {
		      	 NRel SG3 => saiti + pl; 
		      	 NRel NSG3 => raiti + pl ;
		      	 NAbs => taiti + pl }
		} 
	; nc = NCS } ;

		  RelPrefNoun = overload {
		  	      RelPrefNoun : Str -> SIMPLEKIND = \taiti -> let stem: Str = ExtractStem	taiti ; inf : Str = GetInfix stem ; raiti: Str = "r" + inf + stem ; saiti: Str = "s" + inf + stem in mkRelPrefN taiti raiti saiti ;

	RelPrefNoun : Str -> Str -> SIMPLEKIND = \taiti,saiti -> let stem: Str = ExtractStem	taiti ; raiti: Str = "r" + stem in mkRelPrefN taiti raiti saiti ;
	
	RelPrefNoun : Str -> Str -> Str -> SIMPLEKIND = \taiti,raiti, saiti -> mkRelPrefN taiti raiti saiti ;
		      
	};


{-
    RelPrefNoun : Str -> Str -> SIMPLEKIND = \uka,ruka -> let suka: Str = "s" + uka ; pl: Str = "-itá" in {s= table {
    		Sg => table {
		   NRel SG3 => suka ; 
		   NRel NSG3 => ruka; 
		   NAbs => uka } ; 
    		Pl => table {
		   NRel SG3 => suka + pl; 
		   NRel NSG3 => ruka + pl ;
		   NAbs => uka + pl }
		} 
	; nc = NCS } ;

	adjYrl = overload {

	adjYrl : Str -> Class -> QUAL_YRL = 
      \a,c -> {s = \\n,p => a; c=c ; v = True ; nc = NCI} ;

      adjYrl : Str -> NClass -> QUAL_YRL = 
      \ruri,nc -> let suri: Str = "s" + (StemRLemma ruri) in 
      	       {s = table { 
	       	    	  Sg => table {P3 => suri; _ => ruri}; 
	       	    	  Pl => table {_ => ruri} 
			  }; 
	       c=C2 ; v = True ; nc = nc} ;

      };



YrlCopula : Number -> Person -> Level -> Class -> Str -> Verbal -> NClass -> Str -> Str = \n,p,l,c,prop,verbal,nc, pol -> let pron: Str = (ChoosePron n p nc).s ! c ; cop : Str = (ChooseCopula n p).s ! l in pol ++ table { True => pron ++ prop ++ cop ; False => cop ++ pron ++ prop } ! verbal;

ChooseCopula : Number -> Person -> {s: Level => Str} = \n,p -> {s=table {
Stage => StageLevelCopula.s ! n ! p ;
Ind => ""
}};


SecondClassPron :  Number => Person => NClass => Str = table {
		Sg => table { P1 => \\nc => "se" ;
		      	      P2 => \\nc => "ne" ;
			      P3 => table {NCI => "i"; NCS => ""}
				};
		Pl => table { P1 => \\nc => "iané" ;
		      	      P2 => \\nc => "pe" ;
			      P3 => \\nc => "aintá"|"ta"		      	    
			      }
};

ChoosePron : Number -> Person -> NClass -> {s: Class => Str} = \num,pers,nc -> {s=table{
C1 => "";
C2 => SecondClassPron ! num ! pers ! nc
}};


StageLevelCopula : {s: Number => Person => Str} = mkRegVerbYrl "iku";

mkRegVerbYrl : Str -> {s: Number => Person => Str} =
        \x -> {s=
          table {
	  	Sg => table {
		   P1 => "a" + x; 
		   P2 => "re" + x; 
		   P3 => "u" + x
		   } ; 
	  	 Pl => table {
		    P1 => "ia" + x; 
		    P2 => "pe" + x; 
		    P3 => "u" + x
		    }
		    } };
TODO: mkRegVerb returns record, mkIrrVerb returns table; uniformize this

-}
}