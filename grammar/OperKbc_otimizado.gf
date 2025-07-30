

resource OperKbc_otimizado = open Oper, Prelude, Predef in { 
flags coding=utf8 ;
param
    
	--PARAMS KBC

	-- PARAMS FOR THE NOUN
	Alienability = Alnbl | Inlnbl | NoAlnbl ;
	SufClassifier = AnimPlant| Cultivated | Instrum | ActorNmnlzr | UndefClassfr ;
	PsorNum = PsorSg|PsorPl|NoPsorNum ;
	PsorPers = PsorP1|PsorP2|PsorP3|Undef|NoPoss;

	-- PARAMS FOR THE DEMONSTRATIVES
	Position = Standing|Sitting|Lying|Coming|Going;
	Presence = Absent|Present;
	Distance = Close|Far ;

	--PARAMS verb
	AspectPre = ACompl | AIncompl | ADur | ANone ;
	Negation = NegMain | NegSub | NegCondImp | NNone ;
	Mood = MDes | MCond | MNone ;
	Case = Nom | Acc | Dat | Erg | Abs | None ;
	ValencyType = Unacc | Unerg | Trans| Ditrans ;
	Reflexive = Refl | RNone ;
	Hither = HitherPos | HNone ;
	ValencyClit = VGad|VTi|VKan |VQan |VKon |VGon|VGegi|VGan|VGen|VQen|VGod|VNone ;
	AspectPost = Atel | Tel | AspNone ;

	-- Clitics (+4 a +7)
	RelClitic = RelT | RelNone ;
	DirI      = GoingDirI | GoingStraight | GoingTogether | GoingInside | GoingAgainst | Intens | DirINone ;
	DirII     = Outward | Upward | Inward | Toward | Backward | Downward | Apart | AbsentDirII | DirIINone ;
	PronClitic = I | Ga | E | Go | PronNone ;
	SemRole   = WaDative | DomBenefect | CAllative | LoAdessive | Theme | GiGoal | SemNone ;

  oper
	

--NOUNs
-- (Otimizado: sem tabela 's')
	Noun : Type = {
		root : Str;  -- Armazena a raiz diretamente
		g : Gender
	};

	-- Noun vazio padrão (Otimizado)
	emptyNoun : Noun = {
		root = "";
		g = Masc
		} ;

	NOUN_RECORD: Type = {noun_pos:Number => Str; noun_neg:Number => Str; g:Gender};
	
	SIMPLEKIND_KBC = Noun ;
	
	KIND_KBC = Noun ; 
	
 	NounPhrase : Type = {
		s : Str;
		g : Gender;
		n : Number
	};
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

	
	mkAlienabilityStr : Alienability -> Str = \alien ->
        case alien of {
            Alnbl => "n";
            _ => ""
        };

	mkPsorPrefStr : PsorPers -> PsorNum -> Str = \pers, num ->
        case <pers, num> of {
            <PsorP1, PsorSg> => "i";
            <PsorP1, PsorPl> => "ǥod";
            <PsorP2, _> => "ǥad";
            <PsorP3, _> => "l";
            <Undef, _> => "e";
            _ => ""
        };

	mkClassSufStr : SufClassifier -> Number -> Str = \classif, num ->
        case <classif, num> of {
            <AnimPlant, Sg> => "nigo";
            <AnimPlant, Pl> => "co";
            <Cultivated, Pl> => "ija";
            <Instrum, _> => "ǥanǥa";
            <ActorNmnlzr, _> => "ǥikajo:";
            _ => ""
        };

	endsInVowel : Str -> Bool = \s ->
		case s of {
		_ + ("a"|"e"|"i"|"o"|"u"|"A"|"E"|"I"|"O"|"U") => True ;
			_ => False
		};

	mkNumSufStr : Str -> Number -> Str = \root, num ->
        case num of {
            Sg => "";
            Pl => let
                    isVowelEnd = endsInVowel root;
                    suf = case root of {
                        ("eke"|"kwe"|"sana")=> "adi";
                        ("pila"|"taba")  => "pi";      
                        ("abidaǥa"|"koda")=> "ǥa";   
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
        };

	adjustAlienability : Str -> Str -> Str = \marker, next ->
		case marker of {
		"n" => case next of {
			"n" + _ => "" ;
			_ => "n"
		} ;
		_ => marker
		} ;

    isAlveolar : Str -> Bool = \s ->
        case s of {
            ("t"|"d"|"d:"|"n"|"n:"|"l"|"l:"|"T"|"D"|"D:"|"N"|"N:"|"L"|"L:") + _ => True;
            _ => False
        };

	adjustPsorPrefix : Str -> Str -> Str = \prefix, root ->
        case prefix of {
            "l" => case isAlveolar root of {
                True => "";
                False => "l"
            };
            "ǥod" => case endsInVowel root of {
                True => "God";
                False => "Go"
            };
            "ǥad" => case endsInVowel root of {
                True => "ǥad";
                False => "ǥa"
            };
            _ => prefix
        };

	mkAlienabilityStr : Alienability -> Str = \alien ->
        case alien of {
            Alnbl => "n";
            _ => ""
        };

	mkPsorPrefStr : PsorPers -> PsorNum -> Str = \pers, num ->
        case <pers, num> of {
            <PsorP1, PsorSg> => "i";
            <PsorP1, PsorPl> => "ǥod";
            <PsorP2, _> => "ǥad";
            <PsorP3, _> => "l";
            <Undef, _> => "e";
            _ => ""
        };

	mkClassSufStr : SufClassifier -> Number -> Str = \classif, num ->
        case <classif, num> of {
            <AnimPlant, Sg> => "nigo";
            <AnimPlant, Pl> => "co";
            <Cultivated, Pl> => "ija";
            <Instrum, _> => "ǥanǥa";
            <ActorNmnlzr, _> => "ǥikajo:";
            _ => ""
        };

	endsInVowel : Str -> Bool = \s ->
		case s of {
		_ + ("a"|"e"|"i"|"o"|"u"|"A"|"E"|"I"|"O"|"U") => True ;
			_ => False
		};

	mkNumSufStr : Str -> Number -> Str = \root, num ->
        case num of {
            Sg => "";
            Pl => let
                    isVowelEnd = endsInVowel root;
                    suf = case root of {
                        ("eke"|"kwe"|"sana")=> "adi";
                        ("pila"|"taba")  => "pi";      
                        ("abidaǥa"|"koda")=> "ǥa";   
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
        };

	adjustAlienability : Str -> Str -> Str = \marker, next ->
		case marker of {
		"n" => case next of {
			"n" + _ => "" ;
			_ => "n"
		} ;
		_ => marker
		} ;

    isAlveolar : Str -> Bool = \s ->
        case s of {
            ("t"|"d"|"d:"|"n"|"n:"|"l"|"l:"|"T"|"D"|"D:"|"N"|"N:"|"L"|"L:") + _ => True;
            _ => False
        };

	adjustPsorPrefix : Str -> Str -> Str = \prefix, root ->
        case prefix of {
            "l" => case isAlveolar root of {
                True => "";
                False => "l"
            };
            "ǥod" => case endsInVowel root of {
                True => "God";
                False => "Go"
            };
            "ǥad" => case endsInVowel root of {
                True => "ǥad";
                False => "ǥa"
            };
            _ => prefix
        };

	--CLITICS (Mantido como antes)
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
        cl4 = {rel = True; rep = False; p3 = True};
        cl5 = {rel = RelT; pers = P1; num = Sg; dirI = GoingDirI; dirII = Outward; semRole = WaDative};
        cl6 = {rel = False; pl = True};
        cl7 = {rel = True; pl = False}
      };

	mkProperNameKbc : Str-> Gender -> Noun = \name, gender -> 
		mkNoun name gender;
-- Função getNounForm: retorna a string com o substantivo
	getNounRoot : Noun -> Str = \noun -> noun.root;
	getNounForm : Noun -> NounParamSet -> NOUN_RECORD = \noun, params->

		{
			noun_pos = table {
				Sg => let
					possPrefix = mkPsorPrefStr params.psorPers params.psorNum;
					alienabilityMarker = mkAlienabilityStr params.alienability;
					sufixClassifier = mkClassSufStr params.sufClassifier Sg;
					sufixNumberStr = mkNumSufStr (getNounRoot noun) Sg;
					
					adjustedAlienabilityMarker = adjustAlienability alienabilityMarker (getNounRoot noun);
					adjustedPsorPrefix = adjustPsorPrefix possPrefix (getNounRoot noun);
				in	
				
					glue adjustedPsorPrefix (glue adjustedAlienabilityMarker  ( glue (getNounRoot noun)  (glue sufixClassifier sufixNumberStr)));
			

				Pl => let
					possPrefix = mkPsorPrefStr params.psorPers params.psorNum;
					alienabilityMarker = mkAlienabilityStr params.alienability;
					sufixClassifier = mkClassSufStr params.sufClassifier Pl;
					sufixNumberStr = mkNumSufStr (getNounRoot noun) Pl;
					
					adjustedAlienabilityMarker = adjustAlienability alienabilityMarker (getNounRoot noun);
					adjustedPsorPrefix = adjustPsorPrefix possPrefix (getNounRoot noun);
				in	
					 glue adjustedPsorPrefix (glue adjustedAlienabilityMarker (glue (getNounRoot noun) (glue sufixClassifier sufixNumberStr)))
				};
			noun_neg = table {
				Sg => let
					possPrefix = mkPsorPrefStr params.psorPers params.psorNum;
					alienabilityMarker = mkAlienabilityStr params.alienability;
					sufixClassifier = mkClassSufStr params.sufClassifier Sg;
					sufixNumberStr = mkNumSufStr (getNounRoot noun) Sg;
					
					adjustedAlienabilityMarker = adjustAlienability alienabilityMarker (getNounRoot noun);
					adjustedPsorPrefix = adjustPsorPrefix possPrefix (getNounRoot noun);
				in	
					 glue "aǥ" (glue adjustedPsorPrefix (glue adjustedAlienabilityMarker  ( glue (getNounRoot noun)  (glue sufixClassifier sufixNumberStr))));

				Pl => let
					possPrefix = mkPsorPrefStr params.psorPers params.psorNum;
					alienabilityMarker = mkAlienabilityStr params.alienability;
					sufixClassifier = mkClassSufStr params.sufClassifier Pl;
					sufixNumberStr = mkNumSufStr (getNounRoot noun) Pl;
					
					adjustedAlienabilityMarker = adjustAlienability alienabilityMarker (getNounRoot noun);
					adjustedPsorPrefix = adjustPsorPrefix possPrefix (getNounRoot noun);
				in	
					 glue "aǥ" (glue adjustedPsorPrefix (glue adjustedAlienabilityMarker (glue (getNounRoot noun) (glue sufixClassifier sufixNumberStr))))
				};
				g = noun.g

			};
	-- mkNoun otimizado: apenas armazena a raiz e o gênero
	mkNoun : Str -> Gender -> Noun = \root, g -> {
		root = root;
		g = g
	};

-- Funções auxiliares para acessar campos de KIND_KBC (Noun)
  --- getKindRoot : KIND_KBC -> Str = \kind -> kind.root; 

	--NOUN_RECORD: Type = {noun_pos:Number => Str; noun_neg:Number => Str; g:Gender};

--    getKindGender : KIND_KBC -> Gender = \kind -> kind.g;

	--mkKindKbc : SIMPLEKIND_KBC -> KIND_KBC = \sk -> {s = sk.s; g = sk.g};

----------------------------------------------	

--VERB (Otimizado: sem tabela 's')
--VERB ROOTS (Mantido como antes)
	VERB_ROOT : Type = {
      s : Str ;              
      valencyClit : ValencyClit;    
      refl : Reflexive ;     
      hither : Hither ;      
      aspPre : AspectPre ;   
      aspPost : AspectPost ; 
      mood : Mood ;
	  case_ : Case          
     } ;
	
	Verb : Type = {
      root : VERB_ROOT; -- Armazena a raiz verbal (que contém a string e parâmetros inerentes do verb root)
	  vtype : ValencyType ; --inerente do verbo 
    } ;

	Verb_compound : Type = {
		verb_pos : Verb;
		verb_neg : Verb -- A negação será tratada na função de cálculo
	};
				
	-- exemplo verb ROOT
	alepe_root : VERB_ROOT = {
    s = "al:epe" ;
    valencyClit = VNone ;
    refl = RNone ;
    hither = HNone ;
    aspPre = ANone ;
    aspPost = AspNone ;
    mood = MNone ;
    case_ = None
  } ;

  alepe : Verb = mkVerb alepe_root Unacc; 
	

	-- Verb vazio padrão (Otimizado)
	emptyVerb : Verb = {
		root = { -- Raiz vazia
            s = "" ;
            valencyClit = VNone ;
            refl = RNone ;
            hither = HNone ;
            aspPre = ANone ;
            aspPost = AspNone ;
            mood = MNone ;
            case_ = None
        };
		vtype = Unacc
		} ;

-- MORPHOLOGICAL OPERATIONS OF THE VERB (Funções auxiliares retornam Str)
	mkAspPreStr : AspectPre -> Str =
		\asp -> case asp of {
			ACompl => "jaǥ" ;
			AIncompl => "baǥa" ;
			ADur => "banaǥa" ;
			ANone => ""
		} ;
    
    mkNegationStr : Negation -> Str  = 
		\neg -> case neg of {
			NegMain => "aǥ" ;
			NegSub => "daǥa" ;
			NegCondImp => "nǥa" ;
			NNone => ""
		} ;
       
	mkMoodStr : Mood -> Str =
		\mood -> case mood of {
			MDes => "domǥa" ;
			MCond => "dǥa" ;
			_ => ""
		} ;
    
	mkNumPreStr : Person -> Number -> Str =
		\p, n -> case <p, n> of {
			<P3, Pl> => "o" ;
			<Impers, _> => "et" ;
			<_, Pl> => "gi" ;
			_ => ""
		} ;
	mkPersonClitic : ValencyType -> Person -> Number -> Str = \vtype, pers, num ->
		case vtype of {
		Unacc  => case <pers, num> of { -- Objeto indireto opcional (e.g., dativo)
			<P1, Sg> => "i" ;
			<P1, Pl> => "ǥo" ;
			<P2, _>  => "ǥa" ;
			<P3, _>  => "e" ;
			_        => ""
		} ;
		Unerg  => case <pers, num> of { -- Objeto indireto opcional
			<P1, Sg> => "i" ;
			<P1, Pl> => "ǥo" ;
			<P2, _>  => "ǥa" ;
			<P3, _>  => "e" ;
			_        => ""
		} ;
		Trans  => case <pers, num> of { -- Objeto indireto opcional
			<P1, Sg> => "i" ;
			<P1, Pl> => "ǥo" ;
			<P2, _>  => "ǥa" ;
			<P3, _>  => "e" ;
			_        => ""
		} ;
		Ditrans => case <pers, num> of { -- Objeto indireto obrigatório
			<P1, Sg> => "i" ;
			<P1, Pl> => "ǥo" ;
			<P2, _>  => "ǥa" ;
			<P3, _>  => "e" ;
			<PNone, _> => "" ;--"Ditransitive verbs require an indirect object" ;
			<Impers, _> => ""
		}
		} ;



	mkPersonStr :ValencyType -> Person -> Number -> Case -> Person -> Number -> Case -> Str -> Str =  --CHECK: if each case is possible
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
			<Trans, P1, _, Nom, P2, _, Acc> => "ǥa"  ;  -- 1 > 2sg
			<Trans, P1, _, Nom, P3, _, Acc> => case root of { 
				("t"|"d"|"n"|"T"|"D"|"N") + _ => "i"  ;
				 _ => "j"  } ;  -- 1 > 3
			<Trans, P1, Pl, Nom, P1, Sg, Acc> => "i"  ;  -- 1sg OBJ > 1sg/PL SUB (=1sg OBJECT)
			<Trans, P1, Sg, Nom, P1, Pl, Acc> => "ǥo"  ;  --  (object prefix)
			<Trans, P1, Pl, Nom, P1, Pl, Acc> => "ǥo"  ;  -- 1pl > 1pl (reflexivo)
			
			<Trans, P2, _, Nom, P1, Sg, Acc> => "a"  ;  -- 2 > 1sg
			<Trans, P2, _, Nom, P1, Pl, Acc> => "ǥo"  ;  -- 2 > 1pl
			<Trans, P2, _, Nom, P2, Sg, Acc> => "a"  ;  -- 2 > 2sg
			<Trans, P2, _, Nom, P2, Pl, Acc> => "a"  ;  -- 2 > 2pl
			<Trans, P2, _, Nom, P3, _, Acc> => "a"  ;  -- 2 > 3
			
			<Trans, P3, _, Erg, P1, Sg, Acc> => "i"  ;  -- 3 > 1sg
			<Trans, P3, _, Erg, P1, Pl, Acc> => "ǥo"  ;  -- 3 > 1pl
			<Trans, P3, _, Erg, P2, _, Acc> => "ǥa"  ;  -- 3 > 2
			<Trans, P3, _, Erg, P3, _, Acc> => case root of { ("p"|"b"|"t"|"d"|"k"|"g"|"P"|"B"|"T"|"D"|"K"|"G") + _ => root ; "a"|"A" + _ => "w"  ; "n"|"N" + _ => "a"  ; _ => "y"  } ;  -- 3 > 3
			<Trans, Impers, _, Nom, P1, Sg, Acc> => "etii"  ;  -- imp > 1sg
			<Trans, Impers, _, Nom, P1, Pl, Acc> => "etiǥo"  ;  -- imp > 1pl
			<Trans, Impers, _, Nom, P2, _, Acc> => "etiǥa"  ;  -- imp > 2
			<Trans, Impers, _, Nom, P3, _, Acc> => "eti"  ;  -- imp > 3
			_ => ""  -- padrão
		} ;

	mkReflStr : Reflexive -> Str =
		\refl -> case refl of {
			Refl => "je" ;
			_ => ""
		} ;

	mkHitherStr : Hither -> Str =
		\hither -> case hither of {
			HitherPos => "ni" ;
			_ => ""
		} ;


	mkValStr : ValencyClit -> Str = 
        \val ->  case val of {
                    VGad => "ǥad";
                    VTi => "ti";
                    VKan => "kan";
                    VQan => "qan";
                    VKon => "kon";
                    VGon => "gon";
                    VGegi => "gegi";
                    VGan => "gan";
                    VGen => "gen";
                    VQen => "qen";
                    VGod => "god";
                    _ => ""
            };

	mkAspPostStr : AspectPost -> Str =
		\asp -> case asp of {
			Atel => "ne" ;  --Aspect Atelic
			Tel => "pa" ; 		
			_ => ""
		} ;

	mkNumSufVerbStr : Person -> Number -> Str =
		\p, n -> case <p, n> of {
			<P1, Pl> => "ǥa" ;
			<P2, Pl> => "i" ;
			_ => ""
		} ;

	mkCliticsStr : ValencyType -> CliticsRec -> Str = \vtype, cl -> 
      (if_then_Str cl.cl4.rel "t" "" + if_then_Str cl.cl4.rep "ak" "" + if_then_Str cl.cl4.p3 "e" "") +
      (case cl.cl5.rel of { RelT => "t" ; RelNone => "" }) +
      	mkPersonClitic vtype cl.cl5.pers cl.cl5.num +
      (case cl.cl5.dirI of { GoingDirI => "jo" ; GoingStraight => "co" ; GoingTogether => "wa" ; GoingInside => "n" ; GoingAgainst => "get" ; Intens => "b" ; DirINone => "" }) +
      (case cl.cl5.dirII of { Outward => "ce" ; Upward => "bigim" ; Inward => "w" ; Toward => "gi" ; Backward => "we" ; Downward => "nigi" ; Apart => "cwac" ; AbsentDirII => "ca" ; DirIINone => "" }) +
	  (case cl.cl5.semRole of { WaDative => "wa" ; DomBenefect => "dom" ; CAllative => "c" ; LoAdessive => "locom" ; Theme => "d" ; GiGoal => "gi" ; SemNone => "" }) + 
	  (if_then_Str cl.cl6.rel "t" ""+ if_then_Str cl.cl6.pl "niwac" "") +
      (if_then_Str cl.cl7.rel "t" "" + if_then_Str cl.cl7.pl "waji" "");

	VERB_RECORD: Type = {verb_pos: Str; verb_neg: Str};

	-- mkVerb otimizado: apenas armazena a raiz e o tipo de valência
	mkVerb : VERB_ROOT -> ValencyType -> Verb = \root, vtype -> {
        root = root;
        vtype = vtype
    };


	mkVerbRoot : Str -> VERB_ROOT = \root -> {
		s = root ;
		valencyClit = VNone ; -- Padrão sem valência específica
		refl = RNone ;
		hither = HNone ;
		aspPre = ANone ;
		
		aspPost = AspNone ;
		mood = MNone ;
		case_ = None
		} ;

-- Função getVerbForm otimizada: calcula a forma usando 'glue'
	getVerbForm : Verb -> Person -> Number -> Person -> Number -> Person -> Number -> 	CliticsRec -> VERB_RECORD = 
			\verb, subjP, subjN, obj1P, obj1N, obj2P, obj2N, clitics ->
				let
					root = verb.root;
					vtype = verb.vtype;
					
					-- Obter morfemas como strings
					aspPre = mkAspPreStr root.aspPre;
					negMainNegStr = mkNegationStr NegMain;
					negMainPosStr = mkNegationStr NNone;
					mood = mkMoodStr root.mood;
					numPre = mkNumPreStr subjP subjN;
					person = mkPersonStr vtype subjP subjN (root.case_) obj1P obj1N (root.case_) root.s;
					refl = mkReflStr root.refl;
					hither = mkHitherStr root.hither;
					val = mkValStr verb.root.valencyClit; -- Chamada corrigida
					aspPost = mkAspPostStr root.aspPost;
					numSuf = mkNumSufVerbStr subjP subjN;
					cliticsStr = mkCliticsStr vtype clitics;

					pos =  aspPre + negMainPosStr + mood + numPre + person + refl + hither + root.s + val + aspPost + numSuf + cliticsStr;
					neg = aspPre + negMainNegStr + mood + numPre + person + refl + hither + root.s + val + aspPost + numSuf + cliticsStr;
				in
				{verb_pos = pos;
				verb_neg = neg};
					


--------QUALITY
	QUAL_KBC : Type = { 
        verb : Verb ; 
	    noun : Noun; 
        isVerbal: Bool 
    };
	QUAL_RECORD: Type = {qual_verb:VERB_RECORD; qual_noun: NOUN_RECORD; isVerbal: Bool};

	STATE_KBC : Type = QUAL_RECORD ** {l: Level }  ;
		
	ITEM_KBC = NounPhrase;

	mkQualKbc : Str -> Gender -> Bool -> ValencyClit -> NounParamSet -> QUAL_KBC = \rootStr, g, isVerb, val, _ -> {
		verb = case isVerb of {
            True => mkVerb (mkVerbRoot rootStr ** {s = rootStr}) Unacc; 
            False => emptyVerb
        };
		
        noun = case isVerb of {
            True => emptyNoun;
            False => mkNoun rootStr g
        };
        isVerbal = isVerb};


-- Funções auxiliares para acessar campos de QUAL
    
	getQualForm : QUAL_KBC -> QUAL_RECORD = \st -> 
		case st.isVerbal of {
		True => 
			{ qual_verb = getVerbForm st.verb P3 Sg PNone Sg PNone Sg cliticsRec_test_params ;
			qual_noun = {noun_pos =table{ _=>""} ; noun_neg = table{_=>""}; g = Masc };
			isVerbal=st.isVerbal  -- Valor padrão para NOUN_RECORD
			} ;
		False => 
			{ qual_verb = {verb_pos = "" ; verb_neg = ""} ;  -- Valor padrão para VERB_RECORD
			qual_noun = getNounForm st.noun defaultNounParamSet;
			isVerbal=st.isVerbal
			}
		} ;

----------------------------------------------------------------------

--NOUNPHRASE

-- mkNounPhrase otimizado: usa funções auxiliares e cálculo
    mkNounPhrase : Presence -> Position -> Distance -> Noun -> Number -> NounParamSet -> NounPhrase = 
        \pres, pos, dist, kind, n, params -> {
            s = let
                --kindRoot = getKindRoot kind;
                --kindGender = getKindGender kind;
                distanceMorph = case dist of {
                    Close => "nǥ";
                    _ => ""
                };
                genderMorph = case kind.g of {
                    Masc => "i";
                    Fem => "a"
                };
                presencePositionMorph = case <pres, pos> of {
                    <Absent, _> => "ca";
                    <Present, Standing> => "da";
                    <Present, Sitting> => "ni";
                    <Present, Lying> => "di";
                    <Present, Coming> => "na";
                    <Present, Going> => "jo"
                };
                
                demonstrative = case n of {
                    Sg => distanceMorph + genderMorph + presencePositionMorph;
                    Pl =>  distanceMorph + "idiwa"
                };
                -- Usa a função getNounForm que calcula a forma
                nounForm = (getNounForm kind params).noun_pos!n;
                np = demonstrative ++ nounForm
            in
                np;
            g = kind.g;
            n = n
        };

-- Funções auxiliares para acessar campos de NounPhrase (evita acesso direto)
    getNounPhraseString : NounPhrase -> Str = \np -> np.s;
    getNounPhraseGender : NounPhrase -> Gender = \np -> np.g;
    getNounPhraseNumber : NounPhrase -> Number = \np -> np.n;


    mkItemKbc : NounPhrase -> ITEM_KBC = \nd -> nd; -- Assumindo NonDeitic = ITEM_KBC = NounPhrase

{--- Funções auxiliares para demonstrativos (usando mkNounPhrase)
    mkThisKind : KIND_KBC -> ITEM_KBC = \kind -> mkNounPhrase Present Standing Close kind Sg customNounParamSet;
    mkThatKind : KIND_KBC -> ITEM_KBC = \kind -> mkNounPhrase Present Standing Far kind Sg customNounParamSet;
    mkTheseKind : KIND_KBC -> ITEM_KBC = \kind -> mkNounPhrase Present Standing Close kind Pl customNounParamSet;
    mkThoseKind : KIND_KBC -> ITEM_KBC = \kind -> mkNounPhrase Present Standing Far kind Pl customNounParamSet;-}

}