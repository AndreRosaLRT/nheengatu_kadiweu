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
	


	--PARAMS verb


	-- Aspect pre(-7)
	AspectPre = ACompl | AIncompl | ADur | ANone ;
	-- Negation (-6)
	Negation = NegMain | NegSub | NegCondImp | NNone ; -- CLITIC?{ NegMain => "aG" -main clause ; NegSub => "daGa" subordinate (less conditional); NegCondImp => "nGa" --conditional and imperative ;  NNone => "" }

	-- Mood (-5)
	Mood = MDes | MCond | MNone ;

	-- Number prefix (-4) 
	NumberPre = N3Pl | NImpers | NPlOpt | NPreNone ;

	-- Person (-3)
	Person = P1 | P2 | P3 | Impers | PNone ;
	Number = Sg | Pl ;

	Case = Nom | Acc | Dat | Erg | Abs | None ;

	ValencyType = Unacc | Unerg | Trans ;

	-- Reflexive (-2)
	Reflexive = Refl | RNone ;

	-- Direction "Hither" (-1)
	Hither = HitherPos | HNone ;

	-- Valency (+1)
	Valency = VGad|VTi|VKan |VQan |VKon |VGon|VGegi|VGan|VGen|VQen|VGod|VNone ;
	--Valency = causePos|causeNeg|becomePos|becomeNeg|VNone;
	-- Aspecto sufix (+2)
	AspectPost = AAtel | ATel | APNone ;

	
	-- Clitics (+4 a +7)
	RelClitic = RelT | RelNone ;           -- t- ou nada
	DirI      = Jo | Co | WaDir | NDir | Get | B | DirINone ; -- Direção I
	DirII     = Ce | Bigim | W | GiDir | We | Nigi | Cwac | Ca | DirIINone ; -- Direção II
	PronClitic = I | Ga | E | Go | PronNone ; -- Pronominais
	SemRole   = WaDat | Dom | C | Locom | D | GiMeta | SemNone ; -- Papéis Semânticos

	--PARAMS QUALITY

	--QualIntransVerb = Bool; --a param to control if the quality is realized as a noun or as an intransitive verb
	
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
	
	{-Verb : Type = {
		s : AspectPre => Negation => Mood => NumberPre => Person => Number => ValencyType => Person => Number => Reflexive => Hither => Str => Valency => AspectPost => NumberPost => Clitic4 => Clitic5 => Clitic6 => Clitic7 => Str
		} ;-}
			
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

	--VERB ROOTS
	VERB_ROOT: Type = {s: Str; valency: Valency=> Bool};
	alEpe : VERB_ROOT = {s = "al:epe"; valency = table {VGad=>True; _=> False}};
	ataGa : VERB_ROOT = {s = "ataGa"; valency = table {VTi=>True;_=>False}};
	adon : VERB_ROOT ={s = "ad:on"; valency = table {VGad =>True;_=>False}};
	
	
	--CLITICS
	Clitic4 : Type = { rel : Bool ; rep : Bool ; p3 : Bool } ;
	
	Clitic5 : Type = {
		rel     : RelClitic ;
		dirI    : DirI ;
		dirII   : DirII ;
		pron    : PronClitic ;
		semRole : SemRole
  	} ;
	exampleClitic5 : Clitic5 = {
		rel     = RelNone ;      -- t- RelT | RelNone 
		dirI    = Jo ;        -- +jo  Jo | Co | WaDir | NDir | Get | B | DirINone ; -- Direção I
		dirII   = Ce ;        -- +ce Ce | Bigim | W | GiDir | We | Nigi | Cwac | Ca | DirIINone ; -- Direção II
		pron    = PronNone ; -- +i I | Ga | E | Go | PronNone ; -- Pronominais
		semRole = SemNone       -- -wa WaDat | Dom | C | Locom | D | GiMeta | SemNone ; -- Papéis Semânticos
	} ;

	Clitic6 : Type = {
		rel  : Bool ;  -- t (relativo)
		pl   : Bool    -- niwac (plural)
	} ;

	Clitic7 : Type = {
		rel  : Bool ;  -- t (relativo)
		pl   : Bool    -- niwac (plural)
	} ;


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
	--FUNCTION BASED
	mkAspPre : AspectPre -> Str =
		\asp -> case asp of {
			ACompl => "jaG" ;
			AIncompl => "baGa" ;
			ADur => "banaGa" ;
			ANone => ""
		} ; --(-7)
    
    mkNegation : Negation -> Str  = 
		\neg -> case neg of {
			NegMain => "aG" ;
			NegSub => "daGa" ;
			NegCondImp => "nGa" ;
			NNone => ""
		} ; --IN THE CASE OF SUBORDINATES (OR ALL CASES?), IT TAKES EFFECT ON THE CLAUSE RANK (NOT AS MORPHEME OF VERB? but is somehow considered a morpheme)
       
	 mkMood : Mood -> Str =
		\mood -> case mood of {
			MDes => "domGa" ;
			MCond => "dGa" ;
			_ => ""
		} ;--(-5)
    
	-- Posição -4: Derivado de Person e Number do sujeito
	mkNumPre : Person -> Number -> Str =
		\p, n -> case <p, n> of {
			<P3, Pl> => "o" ;      -- P3 plural subject
			<Impers, _> => "et" ;  -- Impersonal
			<_, Pl> => "gi" ;      -- Plural optional?
			_ => ""                -- Nenhum
		} ;

	mkPerson : ValencyType -> Person -> Number -> Case -> Person -> Number -> Case -> Str -> Str =
		\val, subj, numSubj, caseSubj, obj, numObj, caseObj, root ->
			 
			 case <val, subj, numSubj, caseSubj, obj, numObj, caseObj> of {
			-- Unacusative
			<Unacc, P1, _, Abs, PNone, _, _> => case root of { 
				("t"|"d"|"n"|"T"|"D"|"N") + _ => "i"  ; -- 1sg subject before coronals -- 1pl subject before coronals ("-Ga" later)
				_ => "j"  } ;  -- -- 1sg subject elsewhere -- 1pl subject elsewhere ("-Ga" later)
			<Unacc, P2, _, Abs, PNone, _, _> => "a"|"A"  ;  -- 2sg subject -- 2pl subject ("-i" later)
			<Unacc, P3, Sg, Abs, PNone, _, _> => case root of { 
				("p"|"b"|"t"|"d"|"k"|"g"|"P"|"B"|"T"|"D"|"K"|"G") + _ => root ;
				 "a"|"A" + _ => "w"  ;
				  "n"|"N" + _ => "a"  ;
				   _ => "y"  } ;  -- 3sg
			<Unacc, P3, Pl, Abs, PNone, _, _> => "n"  ;  -- 3pl
			<Unacc, Impers, _, Abs, PNone, _, _> => "eti"  ;  -- impersonal
			
			-- Unergativo
			<Unerg, P1, _, Nom, PNone, _, _> => case root of { ("t"|"d"|"n"|"T"|"D"|"N") + _ => "i"  ; _ => "j"  } ;  -- 1sg/pl
			<Unerg, P2, _, Nom, PNone, _, _> => "a"|"A"  ;  -- 2sg/pl
			<Unerg, P3, _, Nom, PNone, _, _> => case root of { 
				("p"|"b"|"t"|"d"|"k"|"g"|"P"|"B"|"T"|"D"|"K"|"G") + _ => root ; 
				"a"|"A" + _ => "w"  ; 
				"n"|"N" + _ => "a"  ;
				 _ => "y"  } ;  -- 3sg
			<Unerg, Impers, _, Nom, PNone, _, _> => "eti"  ;  -- impessoal
			
			
			-- Transitivo
			----Hierarchy: 1pl.OBJ > 2sg./pl. SUBJ > 1sg.OBJ > 1sg./pl.SUBJ > 3sg./pl.SUBJ > 3sg./pl. OBJ.
			
			<Trans, P1, Sg, Nom, P1, Sg, Acc> => "i"  ;  -- 1sg > 1sg (reflexivo)
			<Trans, P1, _, Nom, P2, _, Acc> => "Ga"  ;  -- 1 > 2sg
			<Trans, P1, _, Nom, P3, _, Acc> => case root of { 
				("t"|"d"|"n"|"T"|"D"|"N") + _ => "i"  ;
				 _ => "j"  } ;  -- 1 > 3
			<Trans, P1, Pl, Nom, P1, Sg, Acc> => "i"  ;  -- 1sg OBJ > 1sg/PL SUB (=1sg OBJECT)
			<Trans, P1, Sg, Nom, P1, Pl, Acc> => "Go"  ;  --  (object prefix)
			<Trans, P1, Pl, Nom, P1, Pl, Acc> => "Go"  ;  -- 1pl > 1pl (reflexivo)
			
			<Trans, P2, _, Nom, P1, Sg, Acc> => "a"  ;  -- 2 > 1sg
			<Trans, P2, _, Nom, P1, Pl, Acc> => "Go"  ;  -- 2 > 1pl
			<Trans, P2, _, Nom, P2, Sg, Acc> => "a"  ;  -- 2 > 2sg
			<Trans, P2, _, Nom, P2, Pl, Acc> => "a"  ;  -- 2 > 2pl
			<Trans, P2, _, Nom, P3, _, Acc> => "a"  ;  -- 2 > 3
			
			<Trans, P3, _, Erg, P1, Sg, Acc> => "i"  ;  -- 3 > 1sg
			<Trans, P3, _, Erg, P1, Pl, Acc> => "Go"  ;  -- 3 > 1pl
			<Trans, P3, _, Erg, P2, _, Acc> => "Ga"  ;  -- 3 > 2
			<Trans, P3, _, Erg, P3, _, Acc> => case root of { ("p"|"b"|"t"|"d"|"k"|"g"|"P"|"B"|"T"|"D"|"K"|"G") + _ => root ; "a"|"A" + _ => "w"  ; "n"|"N" + _ => "a"  ; _ => "y"  } ;  -- 3 > 3
			<Trans, Impers, _, Nom, P1, Sg, Acc> => "eti-i"  ;  -- imp > 1sg
			<Trans, Impers, _, Nom, P1, Pl, Acc> => "eti-Go"  ;  -- imp > 1pl
			<Trans, Impers, _, Nom, P2, _, Acc> => "eti-Ga"  ;  -- imp > 2
			<Trans, Impers, _, Nom, P3, _, Acc> => "eti"  ;  -- imp > 3
			_ => root  -- padrão
		} ;

	mkRefl : Reflexive -> Str =
		\refl -> case refl of {
			Refl => "n" ;
			RNone => ""
		} ;
	mkHither : Hither -> Str =
		\hither -> case hither of {
			HitherPos => "n" ;
			HNone => ""
		} ;
	{-mkVal : Valency -> Str =  --CHECK ORGANIZING PRINCIPLE OF VALENCY pg 110
		\val -> case val of {
			VCon => "con" ;
			VCen => "cen" ;
			VKEn => "ken" ;
			VGen => "Gen" ;
			VGad => "Gad" ;
			VGod => "God" ;
			VGegi => "Gegi" ;
			VNone => ""
		} ;

		mkVal : Valency -> Str =  --CHECK ORGANIZING PRINCIPLE OF VALENCY pg 110
		\val -> case val of {
			causePos => "Gad"|"ti" ;
			causeNeg => "Gegi" ;
			becomePos => "Gan"|"Gen"|"qen"|"God" ;
			becomeNeg => "kan"|"qan"|"kon"|"qon" ;
			_ => ""
		} ;-}

	mkVal : VERB_ROOT -> Valency -> Str = \verb_root, val ->
		case verb_root.valency!val of {
			True => table{
				VGad  => "Gad" ;
				VTi   => "ti" ;
				VKan  => "kan" ;
				VQan  => "qan" ;
				VKon  => "kon" ;
				VGon  => "gon" ;
				VGegi => "Gegi" ;
				VGan  => "Gan" ;
				VGen  => "Gen" ;
				VQen  => "qen" ;
				VGod  => "God" ;
				VNone => ""

			}!val;
			False => ""
		};
	mkAspPost : AspectPost -> Str =
		\aspPost -> case aspPost of {
			AAtel => "d";
			ATel =>"g";
			_ => ""

		};

	
	mkNumPost : VERB_ROOT -> Person -> Number -> Person -> Number -> Str =
    \root, subj, numSubj, obj, numObj -> 
      case <subj, numSubj, obj, numObj> of {
        <P1, Pl, _, _> => "Ga" ;    -- P1 plural sujeito
        <P3, Pl, _, _> => "Ga" ;    -- P3 plural sujeito
        <P2, _, _, _> => "i" ;      -- P2 (qualquer número) sujeito
        <_, Sg, P1, Pl> => "Ga" ;   -- P1 plural objeto (sujeito singular)
        <_, Sg, P2, Pl> => "i" ;    -- P2 plural objeto (sujeito singular)
        _ => ""                     -- Nenhum sufixo
      }  ;

	--  Clitic 4
	mkClitic4 : Clitic4 -> Str =
    \cl4 -> 
      (if_then_Str cl4.rel "t" "") ++
      (if_then_Str cl4.rep "ak" "") ++
      (if_then_Str cl4.p3  "e"  "") ;
	
	mkClitic5 : Clitic5 -> Str =
    \cl5 ->
      (case cl5.rel of {
         RelT    => "t" ;
         RelNone => ""
       }) ++
      (case cl5.pron of {
         I      => "i" ;
         Ga     => "Ga" ;
         E      => "e" ;
         Go     => "Go" ;
         PronNone => ""
       }) ++
      (case cl5.dirI of {
         Jo     => "jo" ;
         Co     => "co" ;
         WaDir  => "wa" ;
         NDir   => "n" ;
         Get    => "get" ;
         B      => "b" ;
         DirINone => ""
       }) ++
      (case cl5.dirII of {
         Ce     => "ce" ;
         Bigim  => "bigim" ;
         W      => "w" ;
         GiDir  => "gi" ;
         We     => "we" ;
         Nigi   => "nigi" ;
         Cwac   => "cwac" ;
         Ca     => "ca" ;
         DirIINone => ""
       }) ++
      (case cl5.semRole of {
         WaDat  => "wa" ;
         Dom    => "dom" ;
         C      => "c" ;
         Locom  => "locom" ;
         D      => "d" ;
         GiMeta => "gi" ;
         SemNone => ""
       }) ;
	
	mkClitic6 : Clitic6 -> Str =
		\cl6 ->
		(if_then_Str cl6.rel "t"     "") ++
		(if_then_Str cl6.pl  "niwac" "") ;
			
	
	
	{-
   

	

    mkAspectPost : { s : AspectPost => Str } = {
      s = table { Telic => "g" ; Atelic => "" ; ExclusiveAtelic => "d" ; AspPostNone => "" }
    } ; --(+2)

    mkAspectPostClitic : { s : AspectPostClitic => Str } = {
      s = table {Repetitive => "ak" ; Intensive => "bigi" ; AspPostClitNone => "" }
    } ; --(+4)

    {-mkAspectPostClitic2 : AspectPostClitic -> Str = \asp ->
    case asp of {Repetitive => "ak" ; Intensive => "bigi" ; AspPostClitNone => "" }
    ; -}

	
	--mkVerb : Str -> Verb =
		
	--record type version
	{-mkVerb : { s : AspectPre => Negation => Mood => NumberPre => Person => Number => ValencyType => Person => Number => Reflexive => Hither => Str => Valency => AspectPost => NumberPost => Clitic4 => Clitic5 => Clitic6 => Clitic7 => Str } = {
		s = table {
			aspPre => table {
			neg => table {
				mood => table {
				numPre => table {
					subj => table {
					numSubj => table {
						val => table {
						obj => table {
							numObj => table {
							refl => table {
								hither => table {
								root => table {
									Valency => table {
									aspPost => table {
										numPost => table {
										cl4 => table {
											cl5 => table {
											cl6 => table {
												cl7 => 
												let
													-- Posição -7: Aspecto Pré-Radical
													aspPreStr = case aspPre of {
													ACompl   => "jaG" ;
													AIncompl => "baGa" ;
													ADur     => "banaGa" ;
													ANone    => ""
													} ;

													-- Posição -6: Negação
													negStr = case neg of {
													N1    => "nGa" ;
													N2    => "daGa" ;
													N3    => "aG" ;
													NNone => ""
													} ;

													-- Posição -5: Modo
													moodStr = case mood of {
													MDes  => "domGa" ;
													MCond => "dGa" ;
													MNone => ""
													} ;

													-- Posição -4: Número Pré-Radical
													numPreStr = case numPre of {
													N3Pl    => "o" ;
													NImpers => "et" ;
													NPlOpt  => "gi" ;
													NPreNone => ""
													} ;

													-- Posição -3: Pessoa (baseado em mkPerson ajustado)
													personStr = case <val, subj, numSubj, obj, numObj> of {
													<Intrans, P1, _, PersonNone, _> => case root of {
														("t"|"d"|"n"|"T"|"D"|"N") + _ => "i" ; _ => "j"
													} ;
													<Intrans, P2, _, PersonNone, _> => "a" ;
													<Intrans, P3, _, PersonNone, _> => case root of {
														("p"|"b"|"t"|"d"|"k"|"g"|"P"|"B"|"T"|"D"|"K"|"G") + _ => "" ; "a" + _ => "w" ; "n" + _ => "a" ; _ => "y"
													} ;
													<Intrans, P3Unacc, Pl, PersonNone, _> => "n" ;
													<Intrans, Impers, _, PersonNone, _> => "eti" ;
													<Trans, P1, Sg, P1, Sg> => "i" ;
													<Trans, P1, Pl, P1, Pl> => "Go" ;
													<Trans, P1, Pl, P1, Sg> => "i" ;
													<Trans, P1, Sg, P1, Pl> => "Go" ;
													<Trans, P1, _, P2, Sg> => "Ga" ;
													<Trans, P1, _, P2, Pl> => "Ga" ;
													<Trans, P1, _, P3, _> => case root of {
														("t"|"d"|"n"|"T"|"D"|"N") + _ => "i" ; _ => "j"
													} ;
													<Trans, P2, _, P1, Sg> => "a" ;
													<Trans, P2, _, P1, Pl> => "Go" ;
													<Trans, P2, _, P2, Sg> => "a" ;
													<Trans, P2, _, P2, Pl> => "a" ;
													<Trans, P2, _, P3, _> => "a" ;
													<Trans, P3, _, P1, Sg> => "i" ;
													<Trans, P3, _, P1, Pl> => "Go" ;
													<Trans, P3, _, P2, _> => "Ga" ;
													<Trans, P3, _, P3, _> => case root of {
														("p"|"b"|"t"|"d"|"k"|"g"|"P"|"B"|"T"|"D"|"K"|"G") + _ => "" ; "a" + _ => "w" ; "n" + _ => "a" ; _ => "y"
													} ;
													<Trans, Impers, _, P1, Sg> => "eti-i" ;
													<Trans, Impers, _, P1, Pl> => "eti-Go" ;
													<Trans, Impers, _, P2, Sg> => "eti-Ga" ;
													<Trans, Impers, _, P2, Pl> => "eti-Ga" ;
													<Trans, Impers, _, P3, _> => "eti" ;
													_ => ""
													} ;

													-- Posição -2: Reflexivo
													reflStr = case refl of {
													Refl  => "n" ;
													RNone => ""
													} ;

													-- Posição -1: Hither
													hitherStr = case hither of {
													Hith => "n" ;
													HNone  => ""
													} ;

													-- Posição 0: Radical
													rootStr = root ;

													-- Posição +1: Valência
													valStr = case Valency of {
													VCon  => "con" ;
													VCen  => "cen" ;
													VKEn  => "ken" ;
													VGen  => "Gen" ;
													VGad  => "Gad" ;
													VGod  => "God" ;
													VGegi => "Gegi" ;
													VNone => ""
													} ;

													-- Posição +2: Aspecto Pós-Radical
													aspPostStr = case aspPost of {
													AAtel => "d" ;
													ATel  => "g" ;
													APNone => ""
													} ;

													-- Posição +3: Número Pós-Radical
													numPostStr = case numPost of {
													N1Pl  => "Ga" ;
													N2Pl  => "i" ;
													N12Pl => "gi" ;
													NPNone => ""
													} ;

													-- Posição +4: Clíticos
													cl4Str = case cl4 of {
													C4Rel  => "t" ;
													C4Rep  => "ak" ;
													C4P3   => "e" ;
													C4None => ""
													} ;

													-- Posição +5: Clíticos (simplificado)
													cl5Str = case cl5 of {
													C5Rel   => "t" ;
													C5DirI  => "jo" ;
													C5DirII => "ce" ;
													C5Pron  => "i" ;
													C5Sem   => "wa" ;
													C5None  => ""
													} ;

													-- Posição +6: Clíticos
													cl6Str = case cl6 of {
													C6Rel  => "t" ;
													C6Pl   => "niwac" ;
													C6None => ""
													} ;

													-- Posição +7: Clíticos
													cl7Str = case cl7 of {
													C7Rel  => "t" ;
													C7Pl   => "waji" ;
													C7None => ""
													} ;

													-- Concatenação final
													verb = aspPreStr ++ negStr ++ moodStr ++ numPreStr ++ personStr ++ reflStr ++ hitherStr ++ rootStr ++ 
														valStr ++ aspPostStr ++ numPostStr ++ cl4Str ++ cl5Str ++ cl6Str ++ cl7Str
												in
													verb
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
				}
				}
			}
			}
		};-}
   
    
	
	
	
	
	
	
	
	
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