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
	--Person = P1 | P2 | P3; 
	--Number = Sg|Pl ; 
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

	--VARIABLES

	-- Person (-3)
	--Person = P1 | P2 | P3 | Impers | PNone ; 
    --Number = Sg | Pl ;

	--INHERENTS

	-- Aspect pre(-7)
	AspectPre = ACompl | AIncompl | ADur | ANone ;
	-- Negation (-6)
	Negation = NegMain | NegSub | NegCondImp | NNone ; -- CLITIC?{ NegMain => "aG" -main clause ; NegSub => "daGa" subordinate (less conditional); NegCondImp => "nGa" --conditional and imperative ;  NNone => "" }
	-- Mood (-5)
	Mood = MDes | MCond | MNone ;
	Case = Nom | Acc | Dat | Erg | Abs | None ;
	ValencyType = Unacc | Unerg | Trans| Ditrans ;
	-- Reflexive (-2)
	Reflexive = Refl | RNone ;
	-- Direction "Hither" (-1)
	Hither = HitherPos | HNone ;
	-- Valency (+1)
	ValencyClit = VGad|VTi|VKan |VQan |VKon |VGon|VGegi|VGan|VGen|VQen|VGod|VNone ;
	--Valency = causePos|causeNeg|becomePos|becomeNeg|VNone;
	-- Aspecto sufix (+2)
	AspectPost = AAtel | ATel | APNone ;

	-- Clitics (+4 a +7)
	RelClitic = RelT | RelNone ;           -- t- ou nada
	DirI      = GoingDirI | GoingStraight | GoingTogether | GoingInside | GoingAgainst | Intens | DirINone ; -- Direção I
	DirII     = Outward | Upward | Inward | Toward | Backward | Downward | Apart | AbsentDirII | DirIINone ; -- Direção II
	PronClitic = I | Ga | E | Go | PronNone ; -- Pronominais
	SemRole   = WaDative | DomBenefect | CAllative | LoAdessive | Theme | GiGoal | SemNone ; -- Papéis Semânticos

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
	Verb : Type = {
      s : Person => Number => Person => Number => Person => Number => Str; 
	  vtype : ValencyType ;  -- Tipo de valência (unacc, unerg, trans, Ditrans)
      valencyClit : ValencyClit;    -- Valência inerente
      refl : Reflexive ;     -- Reflexividade inerente
      hither : Hither ;      -- Direção inerente
      aspPre : AspectPre ;   -- Aspecto prefixal inerente
      neg : Negation ;       -- Negação inerente
      aspPost : AspectPost ; -- Aspecto sufixal inerente
      mood : Mood ;
	  case_ : Case  -- Sujeito e objeto variáveis
    } ;
	
				
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
	
	QUAL_KBC : Type = { v : Verb ; n : Noun };
	STATE_KBC : Type = QUAL_KBC ** {l: Level }  ;
		
    --STATE_KBC : QualIntransVerb -> Type = \qiv ->
       -- mkQualKbc qiv ** {l : Level};
	
	ITEM_KBC = NounPhrase;
	ALNBL:Type = {s : Alienability=>Str}; --type for alienability prefix
	PSORPREF: Type = {s : PsorPers => PsorNum =>  Str}; -- Type for possessor prefixes
	CLASSFSUFIX:Type ={s : SufClassifier=>Str};   --Type for classifier sufixes
	NUMBERSUFIX : Type = {s : Number => Str};  --Type for number sufixes
	POLARITY: Type = {s: Polarity => Str};

	--VERB ROOTS
	VERB_ROOT : Type = {
      s : Str ;              -- Raiz lexical
      valencyClit : ValencyClit=> Bool ;    -- Valência inerente
      refl : Reflexive ;     -- Reflexividade inerente
      hither : Hither ;      -- Direção inerente
      aspPre : AspectPre ;   -- Aspecto prefixal inerente
      neg : Negation ;       -- Negação inerente
      aspPost : AspectPost ; -- Aspecto sufixal inerente
      mood : Mood ;
	  case_ : Case          -- Modo inerente
     } ;
	

	--verb root types
	--alEpe: verbo com valência VGad
	alEpe : VERB_ROOT = {
    s = "al:epe" ;
    valencyClit = table { VGad => True ; _ => False } ;
    refl = RNone ;           -- Sem reflexividade
    hither = HNone ;         -- Sem direção "hither"
    aspPre = ANone ;         -- Sem aspecto prefixal
    neg = NNone ;            -- Sem negação
    aspPost = APNone ;       -- Sem aspecto sufixal
    mood = MNone ;           -- Modo neutro
    case_ = None             -- Caso neutro
  } ;
	--ataGa : VERB_ROOT = {s = "ataGa"; valency = table {VTi=>True;_=>False}};
	--adon : VERB_ROOT ={s = "ad:on"; valency = table {VGad =>True;_=>False}};
	
	
	--CLITICS
	CliticsRec : Type = {
      cl4 : {rel : Bool ; rep : Bool ; p3 : Bool} ;
      cl5 : {rel : RelClitic ; pers : Person ; num : Number ; dirI : DirI ; dirII : DirII ; semRole : SemRole} ;
      cl6 : {rel : Bool ; pl : Bool} ;
      cl7 : {rel : Bool ; pl : Bool}
    } ;


		-- Verb vazio padrão
	emptyVerb : Verb = {
		s = table { _ => table { _ => table { _ => table { _ => table { _ => table { _ => "" } } } } } } ;
		vtype = Unacc ;
		valencyClit = VNone ;
		refl = RNone ;
		hither = HNone ;
		aspPre = ANone ;
		neg = NNone ;
		aspPost = APNone ;
		mood = MNone ;
		case_ = None
		} ;

	-- Noun vazio padrão
	emptyNoun : Noun = {
		s = table { _ => table { _ => table { _ => table { _ => table { _ => "" } } } } } ;
		g = Masc -- Gênero padrão
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

	mkPerson : ValencyType -> Person -> Number -> Case -> Person -> Number -> Case -> Str -> Str =  --CHECK: if each case is possible
		\val, subj, numSubj, caseSubj, obj, numObj, caseObj, root ->
			 
			 case <val, subj, numSubj, caseSubj, obj, numObj, caseObj> of {
			-- Unacusative
			<Unacc, P1, _, Abs, PNone, _, _> => case root of { 
				("t"|"d"|"n"|"T"|"D"|"N") + _ => "i"  ; -- 1sg subject before coronals -- 1pl subject before coronals ("-Ga" later)
				_ => "j"  } ;  -- -- 1sg subject elsewhere -- 1pl subject elsewhere ("-Ga" later)
			<Unacc, P2, _, Abs, PNone, _, _> => "a"  ;  -- 2sg subject -- 2pl subject ("-i" later)
			<Unacc, P3, Sg, Abs, PNone, _, _> => case root of { 
				("p"|"b"|"t"|"d"|"k"|"g"|"P"|"B"|"T"|"D"|"K"|"G") + _ => "" ;
				 "a"|"A" + _ => "w"  ;
				  "n"|"N" + _ => "a"  ;
				   _ => "y"  } ;  -- 3sg
			<Unacc, P3, Pl, Abs, PNone, _, _> => "n"  ;  -- 3pl
			<Unacc, Impers, _, Abs, PNone, _, _> => "eti"  ;  -- impersonal
			
			-- Unergativo
			<Unerg, P1, _, Nom, PNone, _, _> => case root of { ("t"|"d"|"n"|"T"|"D"|"N") + _ => "i"  ; _ => "j"  } ;  -- 1sg/pl
			<Unerg, P2, _, Nom, PNone, _, _> => "a"  ;  -- 2sg/pl
			<Unerg, P3, _, Nom, PNone, _, _> => case root of { 
				("p"|"b"|"t"|"d"|"k"|"g"|"P"|"B"|"T"|"D"|"K"|"G") + _ => "" ; 
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
			<Trans, Impers, _, Nom, P1, Sg, Acc> => "etii"  ;  -- imp > 1sg
			<Trans, Impers, _, Nom, P1, Pl, Acc> => "etiGo"  ;  -- imp > 1pl
			<Trans, Impers, _, Nom, P2, _, Acc> => "etiGa"  ;  -- imp > 2
			<Trans, Impers, _, Nom, P3, _, Acc> => "eti"  ;  -- imp > 3
			_ => ""  -- padrão
		} ;

	mkPersonClitic : ValencyType -> Person -> Number -> Str = \vtype, pers, num ->
		case vtype of {
		Unacc  => case <pers, num> of { -- Objeto indireto opcional (e.g., dativo)
			<P1, Sg> => "i" ;
			<P1, Pl> => "Go" ;
			<P2, _>  => "Ga" ;
			<P3, _>  => "e" ;
			_        => ""
		} ;
		Unerg  => case <pers, num> of { -- Objeto indireto opcional
			<P1, Sg> => "i" ;
			<P1, Pl> => "Go" ;
			<P2, _>  => "Ga" ;
			<P3, _>  => "e" ;
			_        => ""
		} ;
		Trans  => case <pers, num> of { -- Objeto indireto opcional
			<P1, Sg> => "i" ;
			<P1, Pl> => "Go" ;
			<P2, _>  => "Ga" ;
			<P3, _>  => "e" ;
			_        => ""
		} ;
		Ditrans => case <pers, num> of { -- Objeto indireto obrigatório
			<P1, Sg> => "i" ;
			<P1, Pl> => "Go" ;
			<P2, _>  => "Ga" ;
			<P3, _>  => "e" ;
			<PNone, _> => "" ;--"Ditransitive verbs require an indirect object" ;
			<Impers, _> => ""
		}
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
	

	mkVal : VERB_ROOT->  ValencyClit -> Str = \verb_root, val ->
		case verb_root.valencyClit!val of {
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

	
	mkNumPost : Person -> Number -> Person -> Number -> Str =
		\subj, numSubj, obj, numObj -> 
		case <subj, numSubj, obj, numObj> of {
			<P1, Pl, _, _> => "Ga" ;    -- P1 plural sujeito
			<P3, Pl, _, _> => "Ga" ;    -- P3 plural sujeito
			<P2, _, _, _> => "i" ;      -- P2 (qualquer número) sujeito
			<_, Sg, P1, Pl> => "Ga" ;   -- P1 plural objeto (sujeito singular)
			<_, Sg, P2, Pl> => "i" ;    -- P2 plural objeto (sujeito singular)
			_ => ""                     -- Nenhum sufixo
		}  ;

	--  Clitic 4
	mkClitics : ValencyType -> CliticsRec -> Str = \vtype, cl -> 
      (if_then_Str cl.cl4.rel "t" "" ++ if_then_Str cl.cl4.rep "ak" "" ++ if_then_Str cl.cl4.p3 "e" "") ++
      (case cl.cl5.rel of { RelT => "t" ; RelNone => "" }) ++
      	mkPersonClitic vtype cl.cl5.pers cl.cl5.num ++
      (case cl.cl5.dirI of { GoingDirI => "jo" ; GoingStraight => "co" ; GoingTogether => "wa" ; GoingInside => "n" ; GoingAgainst => "get" ; Intens => "b" ; DirINone => "" }) ++ 
      (case cl.cl5.dirII of { Outward => "ce" ; Upward => "bigim" ; Inward => "w" ; Toward => "gi" ; Backward => "we" ; Downward => "nigi" ; Apart => "cwac" ; AbsentDirII => "ca" ; DirIINone => "" }) ++
	  (case cl.cl5.semRole of { WaDative => "wa" ; DomBenefect => "dom" ; CAllative => "c" ; LoAdessive => "locom" ; Theme => "d" ; GiGoal => "gi" ; SemNone => "" }) ++
      
	  
	  (if_then_Str cl.cl6.rel "t" "" ++ if_then_Str cl.cl6.pl "niwac" "") ++
      (if_then_Str cl.cl7.rel "t" "" ++ if_then_Str cl.cl7.pl "waji" "");
	

		-- Função auxiliar para criar VERB_ROOT
	mkVerbRoot : Str -> VERB_ROOT = \root -> {
		s = root ;
		valencyClit = table { VNone => True ; _ => False } ; -- Padrão sem valência específica
		refl = RNone ;
		hither = HNone ;
		aspPre = ANone ;
		neg = NNone ;
		aspPost = APNone ;
		mood = MNone ;
		case_ = None
		} ;
	
	
	-- Função principal do paradigma verbal
  	mkVerb : VERB_ROOT -> ValencyType -> ValencyClit -> CliticsRec -> Verb =
		\root, vtype, val, cl -> {
			s = table {
			subj => table {
				subjNum => table {
				obj => table {
					objNum => table {
					indObj => table {
						indObjNum =>
						let
						aspPreStr  = mkAspPre root.aspPre ;
						negStr     = mkNegation root.neg ;
						numPreStr  = mkNumPre subj subjNum ;
						personStr  = mkPerson vtype subj subjNum (case vtype of { Unacc => Abs ; _ => Nom }) 
													obj objNum Acc root.s ;
						reflStr    = case vtype of { Trans | Ditrans => mkRefl root.refl ; _ => "" } ;
						hitherStr  = mkHither root.hither ;
						rootStr    = root.s ;
						valStr     = mkVal root val ; -- Removido o case, valência sempre aplicada
						aspPostStr = mkAspPost root.aspPost ;
						moodStr    = mkMood root.mood ;
						numPostStr = case vtype of { 
							Trans | Ditrans => mkNumPost subj subjNum obj objNum ; 
							_ => mkNumPost subj subjNum PNone Sg 
						} ;
						cliticStr  = mkClitics vtype {cl4 = cl.cl4 ; 
														cl5 = {rel = cl.cl5.rel ; pers = indObj ; num = indObjNum ; 
															dirI = cl.cl5.dirI ; dirII = cl.cl5.dirII ; semRole = cl.cl5.semRole} ; 
														cl6 = cl.cl6 ; cl7 = cl.cl7}
						in
						aspPreStr ++ negStr ++ numPreStr ++ personStr ++ reflStr ++ hitherStr ++ 
						rootStr ++ valStr ++ aspPostStr ++ moodStr ++ numPostStr ++ cliticStr
					}
					}
				}
				}
			}
			} ;
			vtype = vtype ;
			valencyClit = val ;
			refl = root.refl ;
			hither = root.hither ;
			aspPre = root.aspPre ;
			neg = root.neg ;
			aspPost = root.aspPost ;
			mood = root.mood ;
			case_ = root.case_
		} ;

	
	
	
	--QUALITY
	mkQualKbc : Str -> Gender -> Bool -> ValencyClit -> QUAL_KBC = \root, g, isVerb, val ->
		case isVerb of {
			True => {
			v = mkVerb (mkVerbRoot root ** {valencyClit = table { val => True ; _ => False }}) Unacc val 
						{cl4={rel=False;rep=False;p3=False}; 
						cl5={rel=RelNone;pers=PNone;num=Sg;dirI=DirINone;dirII=DirIINone;semRole=SemNone}; 
						cl6={rel=False;pl=False}; 
						cl7={rel=False;pl=False}} ;
			n = emptyNoun
			} ;
			False => {
			v = emptyVerb ;
			n = mkNoun root g
			}
		} ;

	{-
	
	
	mkQualKbc = overload {
		mkQualKbc :VERB_ROOT-> Verb = \verb_root-> mkVerb verb_root ;
		mkQualKbc :Str -> Gender -> Noun = \lema,gender-> mkNoun lema gender ;
	} ;
	
	mkQualKbc : Str -> Gender -> QualIntransVerb -> QUAL_KBC = \root, g, isVerb ->
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

	{-





	--TESTING AREAAAAA
	{-Man = (mkNoun "Gonel:e:giwa" Sg Masc).s!NoAlnbl!NoPoss!NoPsorNum!UndefClassfr;-}
	--Teste = getNounForm (mkNoun "Gonel:e:giwa" Masc) defaultNounParamSet;
	--Teste2 = getNounForm (mkNoun "Gonel:e:giwa" Masc) customNounParamSet;
    --This kind = demonstDet kind Masc Present Standing;
	--Teste3 = mkNoun "Gonel:e:giwa" Masc;
	--Test_verb = (mkVerb alEpe).s ! AIncompl ! NegSub ! MDes ! N3Pl ! P3 ! Pl ! Unacc ! PNone ! Sg ! Refl ! HitherPos ! VGad ! AAtel ! {rel=True;rep=False;p3=False} ! {rel=RelT;pron=I;dirI=Jo;dirII=Ce;semRole=WaDat} ! {rel=True;pl=True} !{rel=True;pl=True};
	--Alepe = mkVerb AIncompl NegSub MDes N3Pl P3 Pl Unacc PNone Sg Refl HitherPos VGad AAtel {rel=True;rep=False;p3=False} {rel=RelT;pron=I;dirI=Jo;dirII=Ce;semRole=WaDat} {rel=True;pl=True} {rel=True;pl=True} alEpe ;
	
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
	

	
}