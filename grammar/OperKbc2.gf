{-Extension GrammKbc (Kadiwéu) ofGrammYEP - a multilingual computational grammar for Nheengatu, English, and Portuguese
 (c) 2020 Leonel Figueiredo de Alencar
 Licensed under the terms of the GNU Lesser General Public License, version 2.1
 See LICENSE or visit the URL
 https://www.gnu.org/licenses/old-licenses/lgpl-2.1.en.html
-}

resource OperKbc2 = open Oper, Prelude in { 
flags coding=utf8 ;
param
    
	--PARAMS KBC

	-- PARAMS FOR THE NOUN
	
	--Person = P1 | P2 | P3; 
	--Number = Sg|Pl ; 
	Alienability = Alnbl | Inlnbl | NoAlnbl ; --(NOT APPLICABLE) ; --Alienable or not (prefix ) (CLASS I or II in the thesis) -- could be a Bool?
	SufClassifier = AnimPlant | Cultivated | Instrum | ActorNmnlzr | UndefClassfr ; --(Animals or plants | cultivated plants | Instruments | actor in nominalizations - e.g. speaker )
	PsorNum = PsorSg | PsorPl | NoPsorNum ;
	PsorPers = PsorP1 | PsorP2 | PsorP3 | Undef | NoPoss; --(NOT APPLICABLE) ;

	-- PARAMS FOR THE DEMONSTRATIVES
	Position = Standing | Sitting | Lying | Coming | Going;
	Presence = Absent | Present;
	Distance = Close | Far ;-- CHECK: I need to check with professors if this is right

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
	ValencyType = Unacc | Unerg | Trans | Ditrans ;
	-- Reflexive (-2)
	Reflexive = Refl | RNone ;
	-- Direction "Hither" (-1)
	Hither = HitherPos | HNone ;
	-- Valency (+1)
	ValencyClit = VGad | VTi | VKan | VQan | VKon | VGon | VGegi | VGan | VGen | VQen | VGod | VNone ;
	--Valency = causePos | causeNeg | becomePos | becomeNeg | VNone;
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
	

--NOUN/NOUNPHRASE AREA
	SIMPLEKIND_KBC = Noun ;
	KIND_KBC = Noun ;  --What would be realized as Adjective (quality) in english is realized as noun or intransitive verb
	
	
    --STATE_KBC : QualIntransVerb -> Type = \qiv ->
       -- mkQualKbc qiv ** {l : Level};
	
	ITEM_KBC = NounPhrase;
	ALNBL: Type = {s : Alienability => Str}; --type for alienability prefix
	PSORPREF: Type = {s : PsorPers => PsorNum => Str}; -- Type for possessor prefixes
	CLASSFSUFIX: Type = {s : SufClassifier => Number => Str};   --Type for classifier sufixes
	NUMBERSUFIX : Type = {s : Number => Str};  --Type for number sufixes
	--POLARITY: Type = {s: Polarity => Str};

		
		--TO DO
		--Still need to implement types for other 2 sufixes (diminutive and nominalizer)
		--In the near future, I might have to include inherent features of the noun (case??might enherit from the verb); gender seems not to be a feature of the noun
	Noun : Type = {
		base : BaseNounRec;
		getForm : NounFeatures -> Str; -- Função explícita com ->
		g : Gender
		};
  
	BaseNounRec : Type = {
		root : Str;
		gender : Gender
		-- Outros traços inerentes fixos, se houver
		};
	NounFeatures : Type = {
		alien : Alienability;
		psorP : PsorPers;
		psorN : PsorNum;
		sufClass : SufClassifier;
		num : Number
		};

	generateNounForm : BaseNounRec -> NounFeatures -> Str = \baseNoun, feats ->
		let
		possPrefix = (mkPsorPref).s ! feats.psorP ! feats.psorN;
		alienabilityMarker = (mkAlienability).s ! feats.alien;
		sufixClassifier = (mkClassSuf).s ! feats.sufClass ! feats.num;
		sufixNumberStr = (mkNumSuf baseNoun.root).s ! feats.num;
		adjustedAlienabilityMarker = adjustAlienability alienabilityMarker baseNoun.root;

		-- Lógica de ajuste para possPrefix (mantida como no seu original)
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
		case <feats.sufClass, feats.num> of {
			<Cultivated, Sg> => adjustedPsorPrefix + adjustedAlienabilityMarker + baseNoun.root;
			_ => adjustedPsorPrefix + adjustedAlienabilityMarker + baseNoun.root + sufixClassifier + sufixNumberStr
		};

		-- Noun vazio padrão
		emptyNoun : Noun = {
		base = { root = ""; gender = Masc };
		getForm = \feats -> ""; -- Função explícita que aceita NounFeatures
		g = Masc
		};

	mkNoun : Str -> Gender -> Noun = \rootStr, genderVal ->
		let
		baseRecord = { root = rootStr; gender = genderVal };
		in
		{
			base = baseRecord;
			getForm = \features -> generateNounForm baseRecord features;
			g = genderVal
		};

	-- Ajustar getNounForm
	getNounForm : Noun -> NounParamSet -> Number -> Str = \noun, params, num ->
		let
		features : NounFeatures = {
			alien = params.alienability;
			psorP = params.psorPers;
			psorN = params.psorNum;
			sufClass = params.sufClassifier;
			num = num
		};
		in
		noun.getForm features;

 	NounPhrase : Type = {
		s : Str;
		g : Gender;
		n : Number
		};
	
				
	--paramset variables
	NounParamSet : Type = {
		alienability : Alienability;
		psorPers : PsorPers;
		psorNum : PsorNum;
		sufClassifier : SufClassifier
		};

	defaultNounParamSet : NounParamSet = {
		alienability = NoAlnbl;
		psorPers = NoPoss;
		psorNum = NoPsorNum;
		sufClassifier = UndefClassfr
		};

	customNounParamSet : NounParamSet = {
		alienability = NoAlnbl;
		psorPers = NoPoss;
		psorNum = PsorSg;
		sufClassifier = UndefClassfr
		};

	customNounParamSet2 : NounParamSet = {
		alienability = Alnbl;
		psorPers = PsorP1;
		psorNum = PsorPl;
		sufClassifier = UndefClassfr
		};

	customNounParamSet3 : NounParamSet = {
		alienability = Alnbl;
		psorPers = NoPoss;
		psorNum = NoPsorNum;
		sufClassifier = AnimPlant
		};
	customNounParamSet4 : NounParamSet = {
		alienability = Alnbl;
		psorPers = NoPoss;
		psorNum = NoPsorNum;
		sufClassifier = UndefClassfr
		};
	customNounParamSet5: NounParamSet = {
		alienability = Alnbl;
		psorPers = PsorP2;
		psorNum = PsorPl;
		sufClassifier = AnimPlant
		};

	
	--mk/* funs
	mkAlienability : ALNBL = {s = table {Alnbl => "n"; _ => ""}};

	endsInVowel : Str -> Bool = \s ->
		case s of {
		_ + ("a"|"e"|"i"|"o"|"u"|"A"|"E"|"I"|"O"|"U") => True ;
		_ => False
		};
		
	mkNumSuf : Str -> NUMBERSUFIX = \root -> {
		s = table {
			Sg => "";
			Pl => let
					isVowelEnd = endsInVowel root;
					suf = case root of {
					("eke"|"kwe"|"sana") => "adi"; -- Exemplo: "cachorro", "coisa", "casa"
					("pila"|"taba") => "pi";      
					("abidaǤa"|"koda") => "Ǥa";   
					("waka"|"jema") => "dodi";    
					("nodajo"|"lapi") => "al:i";
					_ => "adi"
					}
				in
					case <isVowelEnd, suf> of {
					<True, "adi"> => "di";
					<True, "al:i"> => ":i";
					_ => suf
					}
		}
		};

	mkPsorPref : PSORPREF = {
		s = table {
		PsorP1 => table {
			NoPsorNum => ""; 
			PsorSg => "i";
			PsorPl => "Ǥod"
		};
		PsorP2 => table {
			NoPsorNum => "";
			PsorSg => "Ǥad";  
			PsorPl => "Ǥad"  
		};
		PsorP3 => table {
			NoPsorNum => "";
			PsorSg => "l";
			PsorPl => "l"
		};
		Undef => table {
			NoPsorNum => "";
			PsorSg => "e";    
			PsorPl => "e"    
		};
		NoPoss => table {
			_ => ""           
		}
		};
		};

	mkClassSuf : CLASSFSUFIX = {
		s = table {
			UndefClassfr => table {_ => ""};
			AnimPlant => table {Sg => "nigo"; Pl => "co"};
			Cultivated => table {Sg => ""; Pl => "ija"};
			Instrum => table {_ => "ǤanǤa"};
			ActorNmnlzr => table {_ => "Ǥikajo:"}
			}
		};

	startsWith : Str -> Str -> Bool = \prefix, str ->
		case str of {
		prefix + _ => True;
		_ => False
		};

	adjustAlienability : Str -> Str -> Str = \marker, next ->
		case marker of {
		"n" => case startsWith "n" next of {
			True => "";
			False => "n"
		};
		_ => marker
		};

	vowel : pattern Str = #("a"|"e"|"i"|"o"|"u"|"A"|"E"|"I"|"O"|"U") ;
	consonant : pattern Str = #("b"|"c"|"d"|"f"|"g"|"h"|"j"|"k"|"l"|"m"|"n"|"p"|"q"|"r"|"s"|"t"|"v"|"w"|"x"|"y"|"z"|"B"|"C"|"D"|"F"|"G"|"Ǥ"|"H"|"J"|"K"|"L"|"M"|"N"|"P"|"Q"|"R"|"S"|"T"|"V"|"W"|"X"|"Y"|"Z") ;
	alveolar_consonant : pattern Str = #("t"|"d"|"d:"|"n"|"n:"|"l"|"l:"|"T"|"D"|"D:"|"N"|"N:"|"L"|"L:") ;

	mkProperNameKbc : Str -> Gender -> SIMPLEKIND_KBC = \name, gender -> mkNoun name gender ;

	

--VERB AREA
	--VERB ROOTS
	VERB_ROOT : Type = {
		s : Str ;              -- Raiz lexical
		valencyClit : ValencyClit ;    -- Valência inerente
		refl : Reflexive ;     -- Reflexividade inerente
		hither : Hither ;      -- Direção inerente
		aspPre : AspectPre ;   -- Aspecto prefixal inerente
		--neg : Negation ;       -- Negação inerente
		aspPost : AspectPost ; -- Aspecto sufixal inerente
		mood : Mood ;
		case_ : Case          -- Modo inerente
		} ;

	Verb : Type = {
		base : BaseVerbRec;
		getForm : VerbFeatures -> Str; -- Função que gera a string da forma verbal

		-- Propriedades inerentes expostas para conveniência (derivadas de base)
		vtype : ValencyType;
		inherentRootValClit : ValencyClit; -- Valência da VERB_ROOT
		appliedValClit : ValencyClit;    -- Valência aplicada a esta instância
		refl : Reflexive;
		hither : Hither;
		aspPre : AspectPre;
		aspPost : AspectPost;
		mood : Mood;
		rootCase : Case
		};
	BaseVerbRec : Type = {
		root : VERB_ROOT;       -- A raiz lexical com seus traços inerentes
		vtype : ValencyType;    -- O tipo de valência geral (Unacc, Trans, etc.)
		appliedValClit : ValencyClit -- O clítico de valência a ser efetivamente usado nesta instância do verbo
		};

	-- VERB FEATURES (para a função getForm)
	VerbFeatures : Type = {
		subj : Person;
		subjNum : Number;
		obj : Person;         -- Usar PNone para verbos sem objeto direto
		objNum : Number;      -- Usar Sg (ou um valor padrão) se obj for PNone
		indObj : Person;      -- Usar PNone para verbos sem objeto indireto
		indObjNum : Number;   -- Usar Sg se indObj for PNone
		neg : Negation;       -- Negação para esta forma específica
		clitics : CliticsRec  -- Estrutura de clíticos para esta forma
		};
	CliticsRec : Type = {
      cl4 : {rel : Bool ; rep : Bool ; p3 : Bool} ;
      cl5 : {rel : RelClitic ; pers : Person ; num : Number ; dirI : DirI ; dirII : DirII ; semRole : SemRole} ;
      cl6 : {rel : Bool ; pl : Bool} ;
      cl7 : {rel : Bool ; pl : Bool}
    	} ;
	--alEpe: verbo com valência VGad

	alepe : VERB_ROOT = {
		s = "al:epe" ;
		valencyClit = VGad ;
		refl = RNone ;           -- Sem reflexividade
		hither = HNone ;         -- Sem direção "hither"
		aspPre = ANone ;         -- Sem aspecto prefixal
		--neg = NNone ;            -- Sem negação
		aspPost = APNone ;       -- Sem aspecto sufixal
		mood = MNone ;           -- Modo neutro
		case_ = None             -- Caso neutro
		} ;
	--ataGa : VERB_ROOT = {s = "ataGa"; valency = table {VTi=>True;_=>False}};
	--adon : VERB_ROOT ={s = "ad:on"; valency = table {VGad =>True;_=>False}};
	
	
	--CLITICS
	

	CliticsRec_test_params : CliticsRec = {
      cl4 = {rel = False ; rep = False ; p3 = False} ;
      cl5 = {rel = RelNone ; pers = PNone ; num = Sg ; dirI = DirINone ; dirII = DirIINone ; semRole = SemNone} ;
      cl6 = {rel = False ; pl = False} ;
      cl7 = {rel = False ; pl = False}
    	} ;

	CliticsRec_test_params_2 : CliticsRec = {
        cl4 = {rel = True; rep = False; p3 = True}; -- t- (relacional), e- (terceira pessoa)
        cl5 = {rel = RelT; pers = P1; num = Sg; dirI = GoingDirI; dirII = Outward; semRole = WaDative}; -- t- (rel), i (P1 Sg), jo (DirI), ce (DirII), wa (dativo)
        cl6 = {rel = False; pl = True}; -- niwac (plural)
        cl7 = {rel = True; pl = False} -- t- (relacional)
      };

	
	
	-- MORPHOLOGICAL OPERATIONS OF THE VERB
	mkAspPre : AspectPre -> Str = \asp -> case asp of {
		ACompl => "jaǤ" ;
		AIncompl => "baǤa" ;
		ADur => "banaǤa" ;
		ANone => ""
		};

	mkNegation : Negation -> Str = \neg -> case neg of {
		NegMain => "aǤ" ;
		NegSub => "daǤa" ;
		NegCondImp => "nǤa" ;
		NNone => ""
		};

	mkMood : Mood -> Str = \mood -> case mood of {
		MDes => "domǤa" ;
		MCond => "dǤa" ;
		_ => ""
		};

	mkNumPre : Person -> Number -> Str = \p, n -> case <p, n> of {
		<P3, Pl> => "o" ;
		<Impers, _> => "et" ;
		<_, Pl> => "gi" ;
		_ => ""
		};

	mkPerson : ValencyType -> Person -> Number -> Case -> Person -> Number -> Case -> Str -> Str = \val, subj, numSubj, caseSubj, obj, numObj, caseObj, root ->
		case <val, subj, numSubj, caseSubj, obj, numObj, caseObj> of {
			-- Unacusative
			<Unacc, P1, _, Abs, PNone, _, _> => case root of {("t"|"d"|"n"|"T"|"D"|"N") + _ => "i"; _ => "j"};
			<Unacc, P2, _, Abs, PNone, _, _> => "a";
			<Unacc, P3, Sg, Abs, PNone, _, _> => case root of {("p"|"b"|"t"|"d"|"k"|"g"|"P"|"B"|"T"|"D"|"K"|"G") + _ => ""; "a"|"A" + _ => "w"; "n"|"N" + _ => "a"; _ => "y"};
			<Unacc, P3, Pl, Abs, PNone, _, _> => "n";
			<Unacc, Impers, _, Abs, PNone, _, _> => "eti";
			
			-- Unergativo
			<Unerg, P1, _, Nom, PNone, _, _> => case root of {("t"|"d"|"n"|"T"|"D"|"N") + _ => "i"; _ => "j"};
			<Unerg, P2, _, Nom, PNone, _, _> => "a";
			<Unerg, P3, _, Nom, PNone, _, _> => case root of {("p"|"b"|"t"|"d"|"k"|"g"|"P"|"B"|"T"|"D"|"K"|"G") + _ => ""; "a"|"A" + _ => "w"; "n"|"N" + _ => "a"; _ => "y"};
			<Unerg, Impers, _, Nom, PNone, _, _> => "eti";
			
			-- Transitivo
			<Trans, P1, Sg, Nom, P1, Sg, Acc> => "i";
			<Trans, P1, _, Nom, P2, _, Acc> => "Ǥa";
			<Trans, P1, _, Nom, P3, _, Acc> => case root of {("t"|"d"|"n"|"T"|"D"|"N") + _ => "i"; _ => "j"};
			<Trans, P1, Pl, Nom, P1, Sg, Acc> => "i";
			<Trans, P1, Sg, Nom, P1, Pl, Acc> => "Ǥo";
			<Trans, P1, Pl, Nom, P1, Pl, Acc> => "Ǥo";
			
			<Trans, P2, _, Nom, P1, Sg, Acc> => "a";
			<Trans, P2, _, Nom, P1, Pl, Acc> => "Ǥo";
			<Trans, P2, _, Nom, P2, Sg, Acc> => "a";
			<Trans, P2, _, Nom, P2, Pl, Acc> => "a";
			<Trans, P2, _, Nom, P3, _, Acc> => "a";
			
			<Trans, P3, _, Erg, P1, Sg, Acc> => "i";
			<Trans, P3, _, Erg, P1, Pl, Acc> => "Ǥo";
			<Trans, P3, _, Erg, P2, _, Acc> => "Ǥa";
			<Trans, P3, _, Erg, P3, _, Acc> => case root of {("p"|"b"|"t"|"d"|"k"|"g"|"P"|"B"|"T"|"D"|"K"|"G") + _ => root; "a"|"A" + _ => "w"; "n"|"N" + _ => "a"; _ => "y"};
			<Trans, Impers, _, Nom, P1, Sg, Acc> => "etii";
			<Trans, Impers, _, Nom, P1, Pl, Acc> => "etiǤo";
			<Trans, Impers, _, Nom, P2, _, Acc> => "etiǤa";
			<Trans, Impers, _, Nom, P3, _, Acc> => "eti";
			_ => ""
		};

	mkPersonClitic : ValencyType -> Person -> Number -> Str = \vtype, pers, num ->
		case vtype of {
		Unacc => case <pers, num> of {<P1, Sg> => "i"; <P1, Pl> => "Ǥo"; <P2, _> => "Ǥa"; <P3, _> => "e"; _ => ""};
		Unerg => case <pers, num> of {<P1, Sg> => "i"; <P1, Pl> => "Ǥo"; <P2, _> => "Ǥa"; <P3, _> => "e"; _ => ""};
		Trans => case <pers, num> of {<P1, Sg> => "i"; <P1, Pl> => "Ǥo"; <P2, _> => "Ǥa"; <P3, _> => "e"; _ => ""};
		Ditrans => case <pers, num> of {<P1, Sg> => "i"; <P1, Pl> => "Ǥo"; <P2, _> => "Ǥa"; <P3, _> => "e"; <PNone, _> => ""; <Impers, _> => ""}
		};

	mkRefl : Reflexive -> Str = \refl -> case refl of {Refl => "n"; RNone => ""};
	mkHither : Hither -> Str = \hither -> case hither of {HitherPos => "n"; HNone => ""};

	mkVal : VERB_ROOT -> ValencyClit -> Str = \verb_root, val ->
		case val of {
			VGad => "Ǥad" ;
			VTi => "ti" ;
			VKan => "kan" ;
			VQan => "qan" ;
			VKon => "kon" ;
			VGon => "gon" ;
			VGegi => "Ǥegi" ;
			VGan => "Ǥan" ;
			VGen => "Ǥen" ;
			VQen => "qen" ;
			VGod => "Ǥod" ;
			VNone => "" ;
			_ => ""
		};

	mkAspPost : AspectPost -> Str = \aspPost -> case aspPost of {AAtel => "d"; ATel => "g"; _ => ""};

	mkNumPost : Person -> Number -> Person -> Number -> Str = \subj, numSubj, obj, numObj -> 
		case <subj, numSubj, obj, numObj> of {
			<P1, Pl, _, _> => "Ǥa" ;
			<P3, Pl, _, _> => "Ǥa" ;
			<P2, _, _, _> => "i" ;
			<_, Sg, P1, Pl> => "Ǥa" ;
			<_, Sg, P2, Pl> => "i" ;
			<_, Pl, _, _> => "gi";
			_ => ""
		};

	mkClitics : ValencyType -> CliticsRec -> Str = \vtype, cl -> 
      (if_then_Str cl.cl4.rel "t" "" + if_then_Str cl.cl4.rep "ak" "" + if_then_Str cl.cl4.p3 "e" "") +
      (case cl.cl5.rel of {RelT => "t"; RelNone => ""}) +
      mkPersonClitic vtype cl.cl5.pers cl.cl5.num +
      (case cl.cl5.dirI of {GoingDirI => "jo"; GoingStraight => "co"; GoingTogether => "wa"; GoingInside => "n"; GoingAgainst => "get"; Intens => "b"; DirINone => ""}) +
      (case cl.cl5.dirII of {Outward => "ce"; Upward => "bigim"; Inward => "w"; Toward => "gi"; Backward => "we"; Downward => "nigi"; Apart => "cwac"; AbsentDirII => "ca"; DirIINone => ""}) +
      (case cl.cl5.semRole of {WaDative => "wa"; DomBenefect => "dom"; CAllative => "c"; LoAdessive => "locom"; Theme => "d"; GiGoal => "gi"; SemNone => ""}) +
      (if_then_Str cl.cl6.rel "t" "" + if_then_Str cl.cl6.pl "niwac" "") +
      (if_then_Str cl.cl7.rel "t" "" + if_then_Str cl.cl7.pl "waji" "");

-- GENERATE VERB FORM (Nova operação central)
	generateVerbForm : BaseVerbRec -> VerbFeatures -> Str = \baseRec, feats ->
		let
		-- Morfemas baseados nos traços inerentes da VERB_ROOT em BaseVerbRec
		aspPreStr  = mkAspPre baseRec.root.aspPre ;
		reflStr    = case baseRec.vtype of {Trans | Ditrans => mkRefl baseRec.root.refl ; _ => ""} ; -- Reflexivo só para (di)transitivos
		hitherStr  = mkHither baseRec.root.hither ;
		rootStr    = baseRec.root.s ;
		valStr     = mkVal baseRec.root baseRec.appliedValClit ; -- Usa o ValencyClit aplicado
		aspPostStr = mkAspPost baseRec.root.aspPost ;
		moodStr    = mkMood baseRec.root.mood ;

		-- Morfemas baseados nos VerbFeatures (flexionais)
		negStr     = mkNegation feats.neg ;
		numPreStr  = mkNumPre feats.subj feats.subjNum ;
		personStr  = mkPerson baseRec.vtype feats.subj feats.subjNum (case baseRec.vtype of {Unacc => Abs ; _ => Nom})
							feats.obj feats.objNum Acc baseRec.root.s ; -- Passa a string da raiz para mkPerson
		numPostStr = case baseRec.vtype of {
						Trans | Ditrans => mkNumPost feats.subj feats.subjNum feats.obj feats.objNum ;
						_ => mkNumPost feats.subj feats.subjNum PNone Sg -- Objeto PNone para Unacc/Unerg
					} ;
		cliticStr  = mkClitics baseRec.vtype feats.clitics ;
		in
		aspPreStr + negStr + numPreStr + personStr + reflStr + hitherStr +
		rootStr + valStr + aspPostStr + moodStr + numPostStr + cliticStr;

-- MKVERB (Refatorado)
	mkVerb : VERB_ROOT -> ValencyType -> ValencyClit -> Verb =
		\v_root, v_type, applied_val_clit ->
		let
			baseRecord : BaseVerbRec = {
			root = v_root;
			vtype = v_type;
			appliedValClit = applied_val_clit
			};
		in
			{
			base = baseRecord;
			getForm = \features -> generateVerbForm baseRecord features;

			-- Campos inerentes expostos
			vtype = v_type;
			inherentRootValClit = v_root.valencyClit;
			appliedValClit = applied_val_clit;
			refl = v_root.refl;
			hither = v_root.hither;
			aspPre = v_root.aspPre;
			aspPost = v_root.aspPost;
			mood = v_root.mood;
			rootCase = v_root.case_
			};

-- EMPTY VERB (Refatorado)
	emptyVerb : Verb =
		let
		emptyRoot : VERB_ROOT = {
			s = ""; valencyClit = VNone; refl = RNone; hither = HNone;
			aspPre = ANone; aspPost = APNone; mood = MNone; case_ = None
		};
		emptyBaseRec : BaseVerbRec = {
			root = emptyRoot; vtype = Unacc; appliedValClit = VNone
		};
		in
		{
			base = emptyBaseRec;
			getForm = \_ -> ""; -- Sempre retorna string vazia

			vtype = Unacc;
			inherentRootValClit = VNone;
			appliedValClit = VNone;
			refl = RNone;
			hither = HNone;
			aspPre = ANone;
			aspPost = APNone;
			mood = MNone;
			rootCase = None
		};

-- ACTION RECORD (Substituto para Verb_compound)
	ActionRec : Type = {
		verb_generator : Verb;             -- O objeto Verb refatorado que pode gerar formas
		fixed_clitics : CliticsRec;        -- Clíticos que são "fixos" para esta ação lexical
		-- Expor traços inerentes da ação se necessário para lincat Action em GraKbc
		vtype : ValencyType;
		appliedValClit : ValencyClit;
		-- ... outros traços do Verb.base.root ou Verb.base podem ser expostos aqui ...
		refl : Reflexive;
		hither : Hither;
		aspPre : AspectPre;
		aspPost : AspectPost;
		mood : Mood
		};

-- MK_ACTION_REC (Substituto para mkVerbCompound)
  mkActionRec : VERB_ROOT -> ValencyType -> ValencyClit -> CliticsRec -> ActionRec =
    \v_root, v_type, applied_val_clit, clitics_for_action ->
      let
        verb_obj = mkVerb v_root v_type applied_val_clit;
      in
        {
          verb_generator = verb_obj;
          fixed_clitics = clitics_for_action;
          -- Preenchendo os campos inerentes expostos em ActionRec
          vtype = verb_obj.vtype;
          appliedValClit = verb_obj.appliedValClit;
          refl = verb_obj.refl;
          hither = verb_obj.hither;
          aspPre = verb_obj.aspPre;
          aspPost = verb_obj.aspPost;
          mood = verb_obj.mood
        };

-- EMPTY ACTION_REC (Substituto para empty_Verb_compound)
  	emptyActionRec : ActionRec =
		let
		empty_verb_gen = emptyVerb;
		in
		{
			verb_generator = empty_verb_gen;
			fixed_clitics = CliticsRec_test_params; -- Clíticos padrão
			vtype = empty_verb_gen.vtype;
			appliedValClit = empty_verb_gen.appliedValClit;
			refl = empty_verb_gen.refl;
			hither = empty_verb_gen.hither;
			aspPre = empty_verb_gen.aspPre;
			aspPost = empty_verb_gen.aspPost;
			mood = empty_verb_gen.mood
		};
	
	mkVerbRoot : Str -> VERB_ROOT = \root -> {
		s = root ;
		valencyClit = VNone ;
		refl = RNone ;
		hither = HNone ;
		aspPre = ANone ;
		aspPost = APNone ;
		mood = MNone ;
		case_ = None
		};

	{-mkV1 : VERB_ROOT -> ValencyType -> ValencyClit -> CliticsRec -> Negation -> V1 =
		\root, vtype, val, cl, neg ->
			{
			s = table {
				subj => table {
				subjNum =>
							let
							aspPreStr = mkAspPre root.aspPre ;
							negStr = mkNegation neg ;
							numPreStr = mkNumPre subj subjNum ;
							personStr = mkPerson vtype subj subjNum (case vtype of {Unacc => Abs; _ => Nom}) PNone Sg Acc root.s ;
							reflStr = case vtype of {Trans | Ditrans => mkRefl root.refl; _ => ""} ;
							hitherStr = mkHither root.hither ;
							rootStr = root.s ;
							valStr = mkVal root val ;
							aspPostStr = mkAspPost root.aspPost ;
							moodStr = mkMood root.mood ;
							numPostStr = mkNumPost subj subjNum PNone Sg ;
							cliticStr = mkClitics vtype {cl4 = cl.cl4; cl5 = {rel = cl.cl5.rel; pers = PNone; num = Sg; dirI = cl.cl5.dirI; dirII = cl.cl5.dirII; semRole = cl.cl5.semRole}; cl6 = cl.cl6; cl7 = cl.cl7}
							in
							aspPreStr + negStr + numPreStr + personStr + reflStr + hitherStr +
							rootStr + valStr + aspPostStr + moodStr + numPostStr + cliticStr
				}
			} ;
			vtype = vtype ;
			valencyClit = val ;
			refl = root.refl ;
			hither = root.hither ;
			aspPre = root.aspPre ;
			neg = neg ;
			aspPost = root.aspPost ;
			mood = root.mood ;
			case_ = root.case_
			};-}

	-------



---QUALITY AREA
	QUAL_KBC : Type = { verb : ActionRec ; noun : Noun; isVerbal: Bool };
	--STATE_KBC : Type = QUAL_KBC ** {l: Level }  ;
	STATE_KBC : Type = QUAL_KBC ** {l: Level } ;

-- MK_QUAL_KBC (Atualizado para usar mkActionRec e emptyActionRec)
	mkQualKbc : Str -> Gender -> Bool -> ValencyClit -> QUAL_KBC = \root_str, g, is_verb, val_clit -> {
		verb = case is_verb of {
			True => mkActionRec (mkVerbRoot root_str ** {valencyClit = val_clit}) Unacc val_clit
					CliticsRec_test_params; -- Clíticos de teste/padrão para qualidades verbais
			False => emptyActionRec
		};
		noun = case is_verb of {
			True => emptyNoun;
			False => mkNoun root_str g
		};
		isVerbal = is_verb
		};

	mkNounPhraseWithQual:Presence->Position->Distance->Str->Number->Gender->NounParamSet->Str->Bool->ValencyClit->NounParamSet->NounPhrase =
		 \pres,pos,dist,nounRoot,n,g,nounParams,qualRoot,isVerb_param,val,qualParams -> {s=let genderMorph=case g of{Masc=>"i";Fem=>"a"};presencePositionMorph=case<pres,pos>of{<Absent,_>=>"ca";<Present,Standing>=>"da";<Present,Sitting>=>"ni";<Present,Lying>=>"di";<Present,Coming>=>"na";<Present,Going>=>"jo"};distanceMorph=case dist of{Close=>"nǤ";_=>""};pluralMorph=case g of{Masc=>"idiwa";Fem=>"idiwa"};det=case n of{Sg=>distanceMorph+genderMorph+presencePositionMorph;Pl=>distanceMorph+pluralMorph};noun_obj=mkNoun nounRoot g;nounForm=getNounForm noun_obj nounParams n;qual:QUAL_KBC=case qualRoot of{""=>{verb=emptyActionRec;noun=emptyNoun;isVerbal=False};_=>mkQualKbc qualRoot g isVerb_param val};qualForm=case qual.isVerbal of{True=>let verb_gen=qual.verb.verb_generator;feats:VerbFeatures={subj=P3;subjNum=n;obj=PNone;objNum=Sg;indObj=PNone;indObjNum=Sg;neg=NNone;clitics=qual.verb.fixed_clitics};in verb_gen.getForm feats;False=>case qualRoot of{""=>"";_=>getNounForm qual.noun qualParams n}};in det++nounForm++qualForm;g=g;n=n};
	
	
	mkNounPhrase:Presence->Position->Distance->Noun->Number->NounParamSet->NounPhrase = 
		\pres,pos,dist,noun_obj,n,nounParams->{s=let genderMorph=case noun_obj.g of{Masc=>"i";Fem=>"a"};presencePositionMorph=case<pres,pos>of{<Absent,_>=>"ca";<Present,Standing>=>"da";<Present,Sitting>=>"ni";<Present,Lying>=>"di";<Present,Coming>=>"na";<Present,Going>=>"jo"};distanceMorph=case dist of{Close=>"nǤ";_=>""};pluralMorph=case noun_obj.g of{Masc=>"idiwa";Fem=>"idiwa"};det=case n of{Sg=>distanceMorph+genderMorph+presencePositionMorph;Pl=>distanceMorph+pluralMorph};nounForm=getNounForm noun_obj nounParams n;in det++nounForm;g=noun_obj.g;n=n};
	
	
	mkKindKbc:SIMPLEKIND_KBC->KIND_KBC = \sk->{base=sk.base;getForm=sk.getForm;g=sk.g};

 	mkItemKbc : NounPhrase -> ITEM_KBC = \nonvar -> {s = nonvar.s ; n =nonvar.n; g = nonvar.g} ;
	
	
	
	mkSimpleKind : Str -> Gender -> SIMPLEKIND_KBC = \root, gen ->
		let
		noun : Noun = mkNoun root gen; -- Usa mkNoun para obter um Noun completo
		in
		{ base = noun.base; -- Usa o base fornecido por mkNoun
			getForm = \features -> applyNounForm noun features.num; -- Ajuste aqui
			g = gen }; -- Mantém o gênero fornecido

	mkKindFromStr : Str -> Gender -> KIND_KBC = \str, gen ->
		let
		baseRec : BaseNounRec = {
			root = str; -- Define o root como a string fornecida
			gender = gen -- Define o gender como o fornecido
			-- Outros traços, se houver, podem ser deixados como valores padrão
		};
		in
		{ base = baseRec; -- Usa o BaseNounRec construído
			getForm = \features -> str; -- Retorna a string diretamente, ignorando features
			g = gen };
		applyNounForm : Noun -> Number -> Str = \noun, num ->
		let
			features : NounFeatures = {
			alien = NoAlnbl;
			psorP = NoPoss;
			psorN = NoPsorNum;
			sufClass = UndefClassfr;
			num = num
			};
		in
			noun.getForm features;

    applyVerbForm : Verb -> VerbFeatures -> Str = \verb, feats ->
      verb.getForm feats;
	
	
	
	-- Testes
	--1
	test_action = mkActionRec alepe Ditrans VNone CliticsRec_test_params;
	verb_gen = test_action.verb_generator;
	features_1sg_3sg = {subj=P1; subjNum=Sg; obj=P3; objNum=Sg; indObj=PNone; indObjNum=Sg; neg=NNone; clitics=test_action.fixed_clitics};
	Str_test = verb_gen.getForm features_1sg_3sg;

	--2
	test_action2 = mkActionRec alepe Unacc VNone CliticsRec_test_params_2;
	verb_gen2 = test_action2.verb_generator;
	features = {subj=P1; subjNum=Sg; obj=PNone; objNum=Sg; indObj=PNone; indObjNum=Sg; neg=NegMain; clitics=test_action2.fixed_clitics};
	Str_test2 = verb_gen2.getForm features;
	
}