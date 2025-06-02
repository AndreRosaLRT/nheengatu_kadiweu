-- OperKbc.gf otimizado: Corrigido erro de sintaxe em mkValStr

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
	AspectPost = AAtel | ATel | APNone ;

	-- Clitics (+4 a +7)
	RelClitic = RelT | RelNone ;
	DirI      = GoingDirI | GoingStraight | GoingTogether | GoingInside | GoingAgainst | Intens | DirINone ;
	DirII     = Outward | Upward | Inward | Toward | Backward | Downward | Apart | AbsentDirII | DirIINone ;
	PronClitic = I | Ga | E | Go | PronNone ;
	SemRole   = WaDative | DomBenefect | CAllative | LoAdessive | Theme | GiGoal | SemNone ;

  oper
	
--Types 
--NOUN (Otimizado: sem tabela 's')
	Noun : Type = {
		root : Str;  -- Armazena a raiz diretamente
		g : Gender
	};
 	NounPhrase : Type = {
		s : Str;
		g : Gender;
		n : Number
	};
			
--VERB (Otimizado: sem tabela 's')
	Verb : Type = {
      root : VERB_ROOT; -- Armazena a raiz verbal (que contém a string e parâmetros inerentes)
	  vtype : ValencyType ;
    } ;
	Verb_compound : Type = {
		verb_pos : Verb;
		verb_neg : Verb -- A negação será tratada na função de cálculo
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

	SIMPLEKIND_KBC = NOUN_RECORD ;
	KIND_KBC = Noun ; 
	
	-- QUAL_KBC e STATE_KBC precisam ser ajustados para usar os novos Noun e Verb
	QUAL_KBC : Type = { 
        verb : Verb ; 
	    noun : Noun; 
        isVerbal: Bool 
    };

	STATE_KBC : Type = QUAL_RECORD ** {l: Level }  ;
		
	ITEM_KBC = NounPhrase;

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
	
	alepe : VERB_ROOT = {
    s = "al:epe" ;
    valencyClit = VNone ;
    refl = RNone ;
    hither = HNone ;
    aspPre = ANone ;
    aspPost = APNone ;
    mood = MNone ;
    case_ = None
  } ;
	
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

	-- Verb vazio padrão (Otimizado)
	emptyVerb : Verb = {
		root = { -- Raiz vazia
            s = "" ;
            valencyClit = VNone ;
            refl = RNone ;
            hither = HNone ;
            aspPre = ANone ;
            aspPost = APNone ;
            mood = MNone ;
            case_ = None
        };
		vtype = Unacc
		} ;

	emptyVerbCompound : Verb_compound = {
		verb_pos = emptyVerb;
		verb_neg = emptyVerb
	};

	-- Noun vazio padrão (Otimizado)
	emptyNoun : Noun = {
		root = "";
		g = Masc
		} ;
	
-- Funções auxiliares para obter morfemas (retornam Str diretamente)

	mkAlienabilityStr : Alienability -> Str = \alien ->
        case alien of {
            Alnbl => "n";
            _ => ""
        };

	mkPsorPrefStr : PsorPers -> PsorNum -> Str = \pers, num ->
        case <pers, num> of {
            <PsorP1, PsorSg> => "i";
            <PsorP1, PsorPl> => "Ǥod";
            <PsorP2, _> => "Ǥad";
            <PsorP3, _> => "l";
            <Undef, _> => "e";
            _ => ""
        };

	mkClassSufStr : SufClassifier -> Number -> Str = \classif, num ->
        case <classif, num> of {
            <AnimPlant, Sg> => "nigo";
            <AnimPlant, Pl> => "co";
            <Cultivated, Pl> => "ija";
            <Instrum, _> => "ǤanǤa";
            <ActorNmnlzr, _> => "Ǥikajo:";
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
            "Ǥod" => case endsInVowel root of {
                True => "God";
                False => "Go"
            };
            "Ǥad" => case endsInVowel root of {
                True => "Ǥad";
                False => "Ǥa"
            };
            _ => prefix
        };

-- Função getNounForm otimizada: calcula a forma usando 'glue'

	getNounForm : Noun -> NounParamSet -> NOUN_RECORD = \noun, params->

		{
			noun_pos = table {
				Sg => let
					possPrefix = mkPsorPrefStr params.psorPers params.psorNum;
					alienabilityMarker = mkAlienabilityStr params.alienability;
					sufixClassifier = mkClassSufStr params.sufClassifier Sg;
					sufixNumberStr = mkNumSufStr noun.root Sg;
					
					adjustedAlienabilityMarker = adjustAlienability alienabilityMarker noun.root;
					adjustedPsorPrefix = adjustPsorPrefix possPrefix noun.root;
				in	
				
					glue adjustedPsorPrefix (glue adjustedAlienabilityMarker (glue noun.root (glue sufixClassifier sufixNumberStr)));
			

				Pl => let
					possPrefix = mkPsorPrefStr params.psorPers params.psorNum;
					alienabilityMarker = mkAlienabilityStr params.alienability;
					sufixClassifier = mkClassSufStr params.sufClassifier Pl;
					sufixNumberStr = mkNumSufStr noun.root Pl;
					
					adjustedAlienabilityMarker = adjustAlienability alienabilityMarker noun.root;
					adjustedPsorPrefix = adjustPsorPrefix possPrefix noun.root;
				in	
					glue adjustedPsorPrefix (glue adjustedAlienabilityMarker (glue noun.root (glue sufixClassifier sufixNumberStr)))
				};
			noun_neg = table {
				Sg => let
					possPrefix = mkPsorPrefStr params.psorPers params.psorNum;
					alienabilityMarker = mkAlienabilityStr params.alienability;
					sufixClassifier = mkClassSufStr params.sufClassifier Sg;
					sufixNumberStr = mkNumSufStr noun.root Sg;
					
					adjustedAlienabilityMarker = adjustAlienability alienabilityMarker noun.root;
					adjustedPsorPrefix = adjustPsorPrefix possPrefix noun.root;
				in	
					glue "aǤ" (glue adjustedPsorPrefix (glue adjustedAlienabilityMarker (glue noun.root (glue sufixClassifier sufixNumberStr))));
			

				Pl => let
					possPrefix = mkPsorPrefStr params.psorPers params.psorNum;
					alienabilityMarker = mkAlienabilityStr params.alienability;
					sufixClassifier = mkClassSufStr params.sufClassifier Pl;
					sufixNumberStr = mkNumSufStr noun.root Pl;
					
					adjustedAlienabilityMarker = adjustAlienability alienabilityMarker noun.root;
					adjustedPsorPrefix = adjustPsorPrefix possPrefix noun.root;
				in	
					glue "aǤ" (glue adjustedPsorPrefix (glue adjustedAlienabilityMarker (glue noun.root (glue sufixClassifier sufixNumberStr))))
				};
				g = noun.g

			};

      












-- mkNoun otimizado: apenas armazena a raiz e o gênero
	mkNoun : Str -> Gender -> Noun = \root, g -> {
		root = root;
		g = g
	};

	mkKindKbc : Noun -> KIND_KBC = \sk -> 
		mkNoun sk.root sk.g ; -- Já que SIMPLEKIND_KBC = Noun e KIND_KBC = Noun

	mkProperNameKbc : Str-> Gender -> SIMPLEKIND_KBC = \name, gender -> 
	{
	noun_pos = table {_=>name};
	noun_neg = table {_=>name};
	g=gender		
	};
	
-- MORPHOLOGICAL OPERATIONS OF THE VERB (Funções auxiliares retornam Str)
	mkAspPreStr : AspectPre -> Str =
		\asp -> case asp of {
			ACompl => "jaǤ" ;
			AIncompl => "baǤa" ;
			ADur => "banaǤa" ;
			ANone => ""
		} ;
    
    mkNegationStr : Negation -> Str  = 
		\neg -> case neg of {
			NegMain => "aǤ" ;
			NegSub => "daǤa" ;
			NegCondImp => "nǤa" ;
			NNone => ""
		} ;
       
	 mkMoodStr : Mood -> Str =
		\mood -> case mood of {
			MDes => "domǤa" ;
			MCond => "dǤa" ;
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

	-- mkValStr corrigido para usar case...of em vez de if...then
	mkValStr : VERB_ROOT -> ValencyClit -> Str = 
        \root, val ->  case val of {
                    VGad => "Ǥad";
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
			AAtel => "ne" ;
			ATel => "pa" ;
			_ => ""
		} ;

	mkNumSufVerbStr : Person -> Number -> Str =
		\p, n -> case <p, n> of {
			<P1, Pl> => "Ǥa" ;
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

-- Função getVerbForm otimizada: calcula a forma usando 'glue'
	VERB_RECORD: Type = {verb_pos: Str; verb_neg:Str};
	NOUN_RECORD: Type = {noun_pos:Number => Str; noun_neg:Number=>Str; g:Gender};
	QUAL_RECORD: Type = {qual_verb:VERB_RECORD; qual_noun: NOUN_RECORD; isVerbal: Bool};

	getVerbForm : Verb -> Person -> Number -> Person -> Number -> Person -> Number -> CliticsRec -> VERB_RECORD = 
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
                val = mkValStr root verb.root.valencyClit; -- Chamada corrigida
                aspPost = mkAspPostStr root.aspPost;
                numSuf = mkNumSufVerbStr subjP subjN;
                cliticsStr = mkCliticsStr vtype clitics;

				pos = glue aspPre (
                glue negMainPosStr (
                glue mood (
                glue numPre (
                glue person (
                glue refl (
                glue hither (
                glue root.s (
                glue val (
                glue aspPost (
                glue numSuf cliticsStr
                ))))))))));
				neg=glue aspPre (
                glue negMainNegStr (
                glue mood (
                glue numPre (
                glue person (
                glue refl (
                glue hither (
                glue root.s (
                glue val (
                glue aspPost (
                glue numSuf cliticsStr
                ))))))))));
            in
               {verb_pos = pos;
			   verb_neg = neg};
				
-- mkVerb otimizado: apenas armazena a raiz e o tipo de valência
	mkVerb : VERB_ROOT -> ValencyType -> Verb = \root, vtype -> {
        root = root;
        vtype = vtype
    };

-- mkVerbCompound otimizado
	mkVerbCompound : VERB_ROOT -> ValencyType -> Verb_compound = \root, vtype -> {
        verb_pos = mkVerb root vtype;
        verb_neg = mkVerb root vtype -- A negação é tratada em getVerbForm
    };
	mkVerbRoot : Str -> VERB_ROOT = \root -> {
		s = root ;
		valencyClit = VNone ; -- Padrão sem valência específica
		refl = RNone ;
		hither = HNone ;
		aspPre = ANone ;
		
		aspPost = APNone ;
		mood = MNone ;
		case_ = None
		} ;
-- mkQualKbc otimizado
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

-- Funções auxiliares para acessar campos de NounPhrase (evita acesso direto)
    getNounPhraseString : NounPhrase -> Str = \np -> np.s;
    getNounPhraseGender : NounPhrase -> Gender = \np -> np.g;
    getNounPhraseNumber : NounPhrase -> Number = \np -> np.n;

-- Funções auxiliares para acessar campos de KIND_KBC (Noun)
   -- getKindRoot : KIND_KBC -> Str = \kind -> kind.root;
--    getKindGender : KIND_KBC -> Gender = \kind -> kind.g;

-- Funções auxiliares para acessar campos de STATE_KBC
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
-- mkNounPhrase otimizado: usa funções auxiliares e cálculo
    mkNounPhrase : Presence -> Position -> Distance -> Noun -> Number -> NounParamSet -> NounPhrase = 
        \pres, pos, dist, kind, n, params -> {
            s = let
                --kindRoot = getKindRoot kind;
                --kindGender = getKindGender kind;
                distanceMorph = case dist of {
                    Close => "nǤ";
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
                pluralMorph = "idiwa";
                demonstrative = case n of {
                    Sg => glue distanceMorph (glue genderMorph presencePositionMorph);
                    Pl => glue distanceMorph pluralMorph
                };
                -- Usa a função getNounForm que calcula a forma
                nounForm = (getNounForm kind params).noun_pos!n;
                np = glue demonstrative nounForm
            in
                np;
            g = kind.g;
            n = n
        };

    mkItemKbc : NounPhrase -> ITEM_KBC = \nd -> nd; -- Assumindo NonDeitic = ITEM_KBC = NounPhrase

{--- Funções auxiliares para demonstrativos (usando mkNounPhrase)
    mkThisKind : KIND_KBC -> ITEM_KBC = \kind -> mkNounPhrase Present Standing Close kind Sg customNounParamSet;
    mkThatKind : KIND_KBC -> ITEM_KBC = \kind -> mkNounPhrase Present Standing Far kind Sg customNounParamSet;
    mkTheseKind : KIND_KBC -> ITEM_KBC = \kind -> mkNounPhrase Present Standing Close kind Pl customNounParamSet;
    mkThoseKind : KIND_KBC -> ITEM_KBC = \kind -> mkNounPhrase Present Standing Far kind Pl customNounParamSet;-}

}