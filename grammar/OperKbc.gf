 {-Extension GrammKbc (Kadiwéu) ofGrammYEP - a multilingual computational grammar for Nheengatu, English, and Portuguese
 (c) 2020 Leonel Figueiredo de Alencar
 Licensed under the terms of the GNU Lesser General Public License, version 2.1
 See LICENSE or visit the URL
 https://www.gnu.org/licenses/old-licenses/lgpl-2.1.en.html

-}

resource OperKbc = open Oper, Prelude, Predef in { 
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
	Distance = Close|Far ;-- CHECK: I need to check with professors if this is right

	--PARAMS FOR THE (PREDICATE): 
	--POLARITY
	
	


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
		n : Number
		};
			
--VERB
	Verb : Type = {
      s : Person => Number => Person => Number => Person => Number => Str; 
	  vtype : ValencyType ;  -- Tipo de valência (unacc, unerg, trans, Ditrans)
      valencyClit : ValencyClit;    -- Valência inerente
      refl : Reflexive ;     -- Reflexividade inerente
      hither : Hither ;      -- Direção inerente
      aspPre : AspectPre ;   -- Aspecto prefixal inerente
           -- Negação inerente
      aspPost : AspectPost ; -- Aspecto sufixal inerente
      mood : Mood ;
	  case_ : Case  -- Sujeito e objeto variáveis
    } ;
	Verb_compound : Type = {
		verb_pos : Verb;
		verb_neg : Verb
	};
				
	--paramset variables
	NounParamSet :Type ={
		alienability : Alienability;
		psorPers : PsorPers;
		psorNum : PsorNum;
		sufClassifier : SufClassifier
	};

	defaultNounParamSet : NounParamSet ={
		alienability = NoAlnbl;
		psorPers = NoPoss;
		psorNum = NoPsorNum;
		sufClassifier = UndefClassfr
	};

	customNounParamSet :  NounParamSet ={
		alienability = NoAlnbl;
		psorPers = NoPoss;
		psorNum = PsorSg;
		sufClassifier = UndefClassfr
	};

	customNounParamSet2 :  NounParamSet ={
		alienability = Inlnbl;
		psorPers = PsorP1;
		psorNum = PsorSg;
		sufClassifier = UndefClassfr
	};

	customNounParamSet3 :  NounParamSet ={
		alienability = Alnbl;
		psorPers = NoPoss;
		psorNum = NoPsorNum;
		sufClassifier = AnimPlant
	};
	customNounParamSet4 :  NounParamSet ={
		alienability = Alnbl;
		psorPers = NoPoss;
		psorNum = NoPsorNum;
		sufClassifier = UndefClassfr
	};

	SIMPLEKIND_KBC = Noun ;
	KIND_KBC = Noun ;  --What would be realized as Adjective (quality) in english is realized as noun or intransitive verb
	
	QUAL_KBC : Type = { verb : Verb_compound ; noun : Noun; isVerbal: Bool };
	STATE_KBC : Type = QUAL_KBC ** {l: Level }  ;
		
    --STATE_KBC : QualIntransVerb -> Type = \qiv ->
       -- mkQualKbc qiv ** {l : Level};
	
	ITEM_KBC = NounPhrase;
	ALNBL:Type = {s : Alienability=>Str}; --type for alienability prefix
	PSORPREF: Type = {s : PsorPers => PsorNum =>  Str}; -- Type for possessor prefixes
	CLASSFSUFIX:Type ={s : SufClassifier => Number => Str};   --Type for classifier sufixes
	NUMBERSUFIX : Type = {s : Number => Str};  --Type for number sufixes
	--POLARITY: Type = {s: Polarity => Str};

	--VERB ROOTS
	VERB_ROOT : Type = {
      s : Str ;              -- Raiz lexical
      valencyClit : ValencyClit=> Bool ;    -- Valência inerente
      refl : Reflexive ;     -- Reflexividade inerente
      hither : Hither ;      -- Direção inerente
      aspPre : AspectPre ;   -- Aspecto prefixal inerente
            -- Negação inerente
      aspPost : AspectPost ; -- Aspecto sufixal inerente
      mood : Mood ;
	  case_ : Case          -- Modo inerente
     } ;
	

	--verb root types
	--alepe: verbo com valência VGad;
	alepe : VERB_ROOT = {
    s = "al:epe" ;
    valencyClit = table { VGad => True ; _ => False } ;
    refl = RNone ;           -- Sem reflexividade
    hither = HNone ;         -- Sem direção "hither"
    aspPre = ANone ;         -- Sem aspecto prefixal
                -- Sem negação
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

	cliticsRec_test_params : CliticsRec = {
      cl4 = {rel = False ; rep = False ; p3 = False} ;
      cl5 = {rel = RelNone ; pers = PNone ; num = Sg ; dirI = DirINone ; dirII = DirIINone ; semRole = SemNone} ;
      cl6 = {rel = False ; pl = False} ;
      cl7 = {rel = False ; pl = False}
    } ;

	cliticsRec_test_params_2 : CliticsRec = {
        cl4 = {rel = True; rep = False; p3 = True}; -- t- (relacional), e- (terceira pessoa)
        cl5 = {rel = RelT; pers = P1; num = Sg; dirI = GoingDirI; dirII = Outward; semRole = WaDative}; -- t- (rel), i (P1 Sg), jo (DirI), ce (DirII), wa (dativo)
        cl6 = {rel = False; pl = True}; -- niwac (plural)
        cl7 = {rel = True; pl = False} -- t- (relacional)
      };

		-- Verb vazio padrão
	emptyVerb : Verb = {
		s = table { _ => table { _ => table { _ => table { _ => table { _ => table { _ => "" } } } } } } ;
		vtype = Unacc ;
		valencyClit = VNone ;
		refl = RNone ;
		hither = HNone ;
		aspPre = ANone ;
	
		aspPost = APNone ;
		mood = MNone ;
		case_ = None
		} ;

	emptyVerbCompound : Verb_compound = {
		verb_pos = emptyVerb;
		verb_neg = emptyVerb
	};

	-- Noun vazio padrão
	emptyNoun : Noun = {
		s = table { _ => table { _ => table { _ => table { _ => table { _ => "" } } } } } ;
		g = Masc -- Gênero padrão
		} ;
	
	--mk/* funs

	--helper function to get Noun
	getNounForm : Noun -> NounParamSet -> Number -> Str = \noun, params, num ->
 	 noun.s ! params.alienability
         ! params.psorPers
         ! params.psorNum
         ! params.sufClassifier
         ! num;	
	{-getNounForm : Noun -> NounParamSet -> Str =  --
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
	-}
	
	
	mkAlienability : ALNBL =  --helper mk to realization of the alienability sufix;
		{s = table {Alnbl=>"n";_=>""};
		};
	
	

	endsInVowel : Str -> Bool = \s ->
		case s of {
		_ + ("a"|"e"|"i"|"o"|"u"|"A"|"E"|"I"|"O"|"U") => True ;
			_ => False
		};
		
	mkNumSuf : Str -> NUMBERSUFIX = \root -> {
		s = table {
			Sg => "";
			Pl => let
					-- Verifica se a raiz termina em vogal
					isVowelEnd = endsInVowel root;
					-- Escolhe sufixo com base em padrões
					suf = case root of {
					("eke"|"kwe"|"sana")=> "adi"; -- Exemplo: "cachorro", "coisa", "casa"
					("pila"|"taba")  => "pi";      
					("abidaǤa"|"koda")=> "Ǥa";   
					("waka"|"jema")=> "dodi";    
					("nodajo"|"lapi")  => "al:i";
					_ => "adi"
					}
				in
					case <isVowelEnd, suf> of {
					<True, "adi">  => "di";
					<True, "al:i"> => ":i";
					_ => suf
					}
		}
		};
	{-mkPolarity : POLARITY = {
		s= table {
			Pos => "teste_posit";
			Neg => "teste_neg"
			
		}
	};-}
	mkPsorPref :  PSORPREF =  -- helper to Make possessor prefixes (possessor person and number to str)
		{
			s = table {
			PsorP1 => table {
				NoPsorNum => ""; -- Handle NoPsorNum explicitly
				PsorSg => "i";
				PsorPl => "Ǥod"
				  
			};
			PsorP2 => table {
				NoPsorNum => "";
				PsorSg => "Ǥad";  -- Assuming singular
				PsorPl => "Ǥad"  -- Assuming plural
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

	
	mkClassSuf : CLASSFSUFIX = {
		s = table {
			UndefClassfr => table {
			_ => ""
			};
			AnimPlant => table {
			Sg => "nigo";
			Pl => "co"
			};
			Cultivated => table {
			Sg => ""; -- Não usado no singular
			Pl => "ija"
			};
			Instrum => table {
			_ => "ǤanǤa"
			};
			ActorNmnlzr => table {
			_ => "Ǥikajo:"
			}
		}
		};
	adjustAlienability : Str -> Str -> Str = \marker, next ->
		case marker of {
		"n" => case next of {
			"n" + _ => "" ; -- Remove "n" if root starts with "n"
			_ => "n"
		} ;
		_ => marker -- Keep other markers unchanged
		} ;
	
	mkNoun : Str -> Gender -> Noun = \root, g -> {
		s = table {
			alienability => table {
			psorPers => table {
				psorNum => table {
				sufClassifier => table {
					sufixNumber =>
					let
						possPrefix = (mkPsorPref).s ! psorPers ! psorNum;
						alienabilityMarker = (mkAlienability).s ! alienability;
						sufixClassifier = (mkClassSuf).s ! sufClassifier ! sufixNumber;
						sufixNumberStr = (mkNumSuf root).s ! sufixNumber;
						adjustedAlienabilityMarker = adjustAlienability alienabilityMarker root;
						adjustedPsorPrefix = case possPrefix of {
						"l" => pre {
							("t"|"d"|"d:"|"n"|"n:"|"l"|"l:"|"T"|"D"|"D:"|"N"|"N:"|"L"|"L:") => "";
							_ => "l"
						};
						"Ǥod" => pre {
							("b"|"c"|"d"|"f"|"g"|"h"|"j"|"k"|"l"|"m"|"n"|"p"|"q"|"r"|"s"|"t"|
							"v"|"w"|"x"|"y"|"z"|"B"|"C"|"D"|"F"|"G"|"Ǥ"|"H"|"J"|"K"|"L"|"M"|"N"|
							"P"|"Q"|"R"|"S"|"T"|"V"|"W"|"X"|"Y"|"Z") => "Go";
							_ => "God"
						};
						"Ǥad" => pre {
							("b"|"c"|"d"|"f"|"g"|"h"|"j"|"k"|"l"|"m"|"n"|"p"|"q"|"r"|"s"|"t"|
							"v"|"w"|"x"|"y"|"z"|"B"|"C"|"D"|"F"|"G"|"H"|"J"|"K"|"L"|"M"|"N"|
							"P"|"Q"|"R"|"S"|"T"|"V"|"W"|"X"|"Y"|"Z") => "Ǥa";
							_ => "Ǥad"
						};
						_ => possPrefix
						};
					in
						case <sufClassifier, sufixNumber> of {
						<Cultivated, Sg> => adjustedPsorPrefix + adjustedAlienabilityMarker + root;
						_ => adjustedPsorPrefix + adjustedAlienabilityMarker + root + sufixClassifier + sufixNumberStr
						}
				}
				}
			}
			}
		};
		g = g
		};


	mkKindKbc : SIMPLEKIND_KBC -> KIND_KBC = \sk -> {s = sk.s; g = sk.g};
	--PATTERNS
		vowel : pattern Str = #("a"|"e"|"i"|"o"|"u"|"A"|"E"|"I"|"O"|"U") ;
		consonant : pattern Str = #("b"|"c"|"d"|"f"|"g"|"h"|"j"|"k"|"l"|"m"|"n"|"p"|"q"|"r"|"s"|"t"|"v"|"w"|"x"|"y"|"z"|"B"|"C"|"D"|"F"|"G"|"H"|"J"|"K"|"L"|"M"|"N"|"P"|"Q"|"R"|"S"|"T"|"V"|"W"|"X"|"Y"|"Z") ;
		alveolar_consonant:  pattern Str = #("t"|"d"|"d:"|"n"|"n:"|"l"|"l:"|"T"|"D"|"D:"|"N"|"N:"|"L"|"L:") ; -- Need to check if this pattern for alveolar (dental?) is
		--PATTERNS dont seem to be working as expected
			-- Padrões para sufixos plurais
	


	mkProperNameKbc : Str-> Gender -> SIMPLEKIND_KBC = \name, gender -> mkNoun name gender ; --pretty much the same as using mkNoun directly (I migh have to adapt this function in future)
	

	-- MORPHOLOGICAL OPERATIONS OF THE VERB
	--FUNCTION BASED
	mkAspPre : AspectPre -> Str =
		\asp -> case asp of {
			ACompl => "jaǤ" ;
			AIncompl => "baǤa" ;
			ADur => "banaǤa" ;
			ANone => ""
		} ; --(-7)
    
    mkNegation : Negation -> Str  = 
		\neg -> case neg of {
			NegMain => "aǤ" ;
			NegSub => "daǤa" ;
			NegCondImp => "nǤa" ;
			NNone => ""
		} ; --IN THE CASE OF SUBORDINATES (OR ALL CASES?), IT TAKES EFFECT ON THE CLAUSE RANK (NOT AS MORPHEME OF VERB? but is somehow considered a morpheme)
       
	 mkMood : Mood -> Str =
		\mood -> case mood of {
			MDes => "domǤa" ;
			MCond => "dǤa" ;
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
			<Trans, P1, _, Nom, P2, _, Acc> => "Ǥa"  ;  -- 1 > 2sg
			<Trans, P1, _, Nom, P3, _, Acc> => case root of { 
				("t"|"d"|"n"|"T"|"D"|"N") + _ => "i"  ;
				 _ => "j"  } ;  -- 1 > 3
			<Trans, P1, Pl, Nom, P1, Sg, Acc> => "i"  ;  -- 1sg OBJ > 1sg/PL SUB (=1sg OBJECT)
			<Trans, P1, Sg, Nom, P1, Pl, Acc> => "Ǥo"  ;  --  (object prefix)
			<Trans, P1, Pl, Nom, P1, Pl, Acc> => "Ǥo"  ;  -- 1pl > 1pl (reflexivo)
			
			<Trans, P2, _, Nom, P1, Sg, Acc> => "a"  ;  -- 2 > 1sg
			<Trans, P2, _, Nom, P1, Pl, Acc> => "Ǥo"  ;  -- 2 > 1pl
			<Trans, P2, _, Nom, P2, Sg, Acc> => "a"  ;  -- 2 > 2sg
			<Trans, P2, _, Nom, P2, Pl, Acc> => "a"  ;  -- 2 > 2pl
			<Trans, P2, _, Nom, P3, _, Acc> => "a"  ;  -- 2 > 3
			
			<Trans, P3, _, Erg, P1, Sg, Acc> => "i"  ;  -- 3 > 1sg
			<Trans, P3, _, Erg, P1, Pl, Acc> => "Ǥo"  ;  -- 3 > 1pl
			<Trans, P3, _, Erg, P2, _, Acc> => "Ǥa"  ;  -- 3 > 2
			<Trans, P3, _, Erg, P3, _, Acc> => case root of { ("p"|"b"|"t"|"d"|"k"|"g"|"P"|"B"|"T"|"D"|"K"|"G") + _ => root ; "a"|"A" + _ => "w"  ; "n"|"N" + _ => "a"  ; _ => "y"  } ;  -- 3 > 3
			<Trans, Impers, _, Nom, P1, Sg, Acc> => "etii"  ;  -- imp > 1sg
			<Trans, Impers, _, Nom, P1, Pl, Acc> => "etiǤo"  ;  -- imp > 1pl
			<Trans, Impers, _, Nom, P2, _, Acc> => "etiǤa"  ;  -- imp > 2
			<Trans, Impers, _, Nom, P3, _, Acc> => "eti"  ;  -- imp > 3
			_ => ""  -- padrão
		} ;

	mkPersonClitic : ValencyType -> Person -> Number -> Str = \vtype, pers, num ->
		case vtype of {
		Unacc  => case <pers, num> of { -- Objeto indireto opcional (e.g., dativo)
			<P1, Sg> => "i" ;
			<P1, Pl> => "Ǥo" ;
			<P2, _>  => "Ǥa" ;
			<P3, _>  => "e" ;
			_        => ""
		} ;
		Unerg  => case <pers, num> of { -- Objeto indireto opcional
			<P1, Sg> => "i" ;
			<P1, Pl> => "Ǥo" ;
			<P2, _>  => "Ǥa" ;
			<P3, _>  => "e" ;
			_        => ""
		} ;
		Trans  => case <pers, num> of { -- Objeto indireto opcional
			<P1, Sg> => "i" ;
			<P1, Pl> => "Ǥo" ;
			<P2, _>  => "Ǥa" ;
			<P3, _>  => "e" ;
			_        => ""
		} ;
		Ditrans => case <pers, num> of { -- Objeto indireto obrigatório
			<P1, Sg> => "i" ;
			<P1, Pl> => "Ǥo" ;
			<P2, _>  => "Ǥa" ;
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
				VGad  => "Ǥad" ;
				VTi   => "ti" ;
				VKan  => "kan" ;
				VQan  => "qan" ;
				VKon  => "kon" ;
				VGon  => "gon" ;
				VGegi => "Ǥegi" ;
				VGan  => "Ǥan" ;
				VGen  => "Ǥen" ;
				VQen  => "qen" ;
				VGod  => "Ǥod" ;
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
			<P1, Pl, _, _> => "Ǥa" ;    -- P1 plural sujeito
			<P3, Pl, _, _> => "Ǥa" ;    -- P3 plural sujeito
			<P2, _, _, _> => "i" ;      -- P2 (qualquer número) sujeito
			<_, Sg, P1, Pl> => "Ǥa" ;   -- P1 plural objeto (sujeito singular)
			<_, Sg, P2, Pl> => "i" ;    -- P2 plural objeto (sujeito singular)
			_ => ""                     -- Nenhum sufixo
		}  ;

	--  Clitic 4
	mkClitics : ValencyType -> CliticsRec -> Str = \vtype, cl -> 
      (if_then_Str cl.cl4.rel "t" "" + if_then_Str cl.cl4.rep "ak" "" + if_then_Str cl.cl4.p3 "e" "") +
      (case cl.cl5.rel of { RelT => "t" ; RelNone => "" }) +
      	mkPersonClitic vtype cl.cl5.pers cl.cl5.num +
      (case cl.cl5.dirI of { GoingDirI => "jo" ; GoingStraight => "co" ; GoingTogether => "wa" ; GoingInside => "n" ; GoingAgainst => "get" ; Intens => "b" ; DirINone => "" }) +
      (case cl.cl5.dirII of { Outward => "ce" ; Upward => "bigim" ; Inward => "w" ; Toward => "gi" ; Backward => "we" ; Downward => "nigi" ; Apart => "cwac" ; AbsentDirII => "ca" ; DirIINone => "" }) +
	  (case cl.cl5.semRole of { WaDative => "wa" ; DomBenefect => "dom" ; CAllative => "c" ; LoAdessive => "locom" ; Theme => "d" ; GiGoal => "gi" ; SemNone => "" }) +
      
	  
	  (if_then_Str cl.cl6.rel "t" ""+ if_then_Str cl.cl6.pl "niwac" "") +
      (if_then_Str cl.cl7.rel "t" "" + if_then_Str cl.cl7.pl "waji" "");
	

		-- Função auxiliar para criar VERB_ROOT
	mkVerbRoot : Str -> VERB_ROOT = \root -> {
		s = root ;
		valencyClit = table { VNone => True ; _ => False } ; -- Padrão sem valência específica
		refl = RNone ;
		hither = HNone ;
		aspPre = ANone ;
		
		aspPost = APNone ;
		mood = MNone ;
		case_ = None
		} ;
	
	
	-- Função principal do paradigma verbal
  	mkVerb : VERB_ROOT -> ValencyType -> ValencyClit -> CliticsRec -> Negation ->Verb =
		\root, vtype, val, cl, neg -> {
			s = table {
			subj => table {
				subjNum => table {
				obj => table {
					objNum => table {
					indObj => table {
						indObjNum =>
						let
						aspPreStr  = mkAspPre root.aspPre ;
						negStr     = mkNegation neg ;
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
						aspPreStr + negStr + numPreStr + personStr + reflStr  + hitherStr + 
						rootStr + valStr + aspPostStr + moodStr + numPostStr+ cliticStr
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
	mkVerbCompound : VERB_ROOT -> ValencyType -> ValencyClit -> CliticsRec -> Verb_compound =
		\root, vtype, val, cl -> {
			verb_pos = mkVerb root vtype val cl NNone;
			verb_neg = mkVerb root vtype val cl NegMain
		};
	
	 --VERB_ROOT -> ValencyType -> ValencyClit -> CliticsRec 
	
	--QUALITY
	mkQualKbc : Str -> Gender -> Bool -> ValencyClit ->QUAL_KBC = \root, g, isVerb, val -> {
		verb = case isVerb of {
			True => mkVerbCompound (mkVerbRoot root ** {valencyClit = table { val => True ; _ => False }}) Unacc val  
					{cl4={rel=False;rep=False;p3=False}; 
					cl5={rel=RelNone;pers=PNone;num=Sg;dirI=DirINone;dirII=DirIINone;semRole=SemNone}; 
					cl6={rel=False;pl=False}; 
					cl7={rel=False;pl=False}};
			False => emptyVerbCompound
		};
		noun = case isVerb of {
			True => emptyNoun;
			False => mkNoun root g
		};
		isVerbal = isVerb
		};
	
	
	
	
		{-mkNounPhraseWithQual : Presence -> Position->Distance-> Str -> Number -> Gender -> NounParamSet -> Str -> Bool -> ValencyClit -> NounParamSet ->Negation-> NounPhrase = 
			\pres, pos,dist, nounRoot, n, g, nounParams, qualRoot, isVerb, val, qualParams -> {
				s = 
				let
				genderMorph = case g of {
					Masc => "i";
					Fem  => "a"
				};
				presencePositionMorph = case <pres, pos> of {
					<Absent, _>       => "ca";
					<Present, Standing> => "da";
					<Present, Sitting>  => "ni";
					<Present, Lying>    => "di";
					<Present, Coming>   => "na";
					<Present, Going>    => "jo"
				};
				distanceMorph = case dist of {
					Close => "nǤ";
					_ => "" -- CHECK if there is no realization of other distance other than close
				};
				pluralMorph = "idiwa";
				det = case n of {
					Sg => distanceMorph + genderMorph + presencePositionMorph;
					Pl => distanceMorph + genderMorph + presencePositionMorph+ pluralMorph
				};
				noun = mkNoun nounRoot g;
				nounForm = getNounForm noun nounParams n;
				qual = case qualRoot of {
					"" => { verb = emptyVerb; noun = emptyNoun };
					_ => mkQualKbc qualRoot g isVerb val
				};
				qualForm = case qual.verb.s ! PNone ! Sg ! PNone ! Sg ! PNone ! Sg of {
					"" => case qualRoot of {
					"" => "";
					_ => getNounForm qual.noun qualParams n
					};
					verb => verb
				};
				in
				det ++ nounForm ++ qualForm;
				g = g;
				n = n
			};
			-}
			
		mkNounPhrase : Presence -> Position->Distance-> Noun -> Number ->  NounParamSet -> NounPhrase = 
			\pres, pos,dist, noun, n, nounParams-> {
				s = 
				let
				genderMorph = case noun.g of {
					Masc => "i";
					Fem  => "a"
				};
				presencePositionMorph = case <pres, pos> of {
					<Absent, _>       => "ca";
					<Present, Standing> => "da";
					<Present, Sitting>  => "ni";
					<Present, Lying>    => "di";
					<Present, Coming>   => "na";
					<Present, Going>    => "jo"
				};
				distanceMorph = case dist of {
					Close => "nǤ";
					_ => "" -- CHECK if there is no realization of other distance other than close
				};
				pluralMorph = case noun.g of {
					Masc => "idiwa";
					Fem => "idiwa"
				};
				det = case n of {
					Sg => distanceMorph + genderMorph + presencePositionMorph;
					Pl => distanceMorph + pluralMorph
				};
				
				nounForm = getNounForm noun nounParams n;
				
				in
				det ++ nounForm ;
				g = noun.g;
				n = n
			};
	 mkItemKbc : NounPhrase -> ITEM_KBC = \nonvar -> {s = nonvar.s ; n =nonvar.n; g = nonvar.g} ;
	
	teste_getnoun = getNounForm ( mkNoun "Gonelegiwa" Masc) customNounParamSet2;



	--TESTING AREAAAAA
	--Man = (mkNoun "Gonel:e:giwa" Sg Masc).s!NoAlnbl!NoPoss!NoPsorNum!UndefClassfr;
	--Teste = getNounForm (mkNoun "Gonel:e:giwa" Masc) defaultNounParamSet;
	--Teste2 = getNounForm (mkNoun "Gonel:e:giwa" Masc) customNounParamSet;
    --This kind = demonstDet kind Masc Present Standing;
	--Teste3 = mkNoun "Gonel:e:giwa" Masc;
	--Test_verb = (mkVerb alEpe).s ! AIncompl ! NegSub ! MDes ! N3Pl ! P3 ! Pl ! Unacc ! PNone ! Sg ! Refl ! HitherPos ! VGad ! AAtel ! {rel=True;rep=False;p3=False} ! {rel=RelT;pron=I;dirI=Jo;dirII=Ce;semRole=WaDat} ! {rel=True;pl=True} !{rel=True;pl=True};
	--Alepe = mkVerb AIncompl NegSub MDes N3Pl P3 Pl Unacc PNone Sg Refl HitherPos VGad AAtel {rel=True;rep=False;p3=False} {rel=RelT;pron=I;dirI=Jo;dirII=Ce;semRole=WaDat} {rel=True;pl=True} {rel=True;pl=True} alEpe ;
	
	{-TesteDem = demonstDet Sg  "ka" "ida" "ini" "idi" "ina" "ijo" "ada" "ani" "adi" "ana" "ajo" "idiwa" Present Coming defaultNounParamSet (mkNoun "Gonelegiwa" Masc );
	TesteDem2 = demonstDet Sg Present Coming defaultNounParamSet (mkNoun "Gonelegiwa" Masc );
	teste_getnoun = getNounForm ( mkNoun "Gonelegiwa" Masc) customNounParamSet;
	teste_getnoun = getNounForm ( mkNoun "Gonelegiwa" Masc) customNounParamSet;-}
	
 	 teste_verb_compound = mkVerbCompound alepe Unacc VNone cliticsRec_test_params;
										

	

	
}