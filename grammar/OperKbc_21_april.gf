-- Versão atualizada do OperKbc com funções auxiliares para encapsular acesso a campos

resource OperKbc_21_april = open Oper, Prelude, Predef in { 
flags coding=utf8 ;
param
    
	--PARAMS KBC - Simplificados e reorganizados

	-- PARAMS FOR THE NOUN
	
	--Person = P1 | P2 | P3; 
	--Number = Sg|Pl ; 
	Alienability = Alnbl | Inlnbl ; -- Poderia ser simplificado para Bool em uma versão futura
	SufClassifier = AnimPlant | Cultivated | Instrum | ActorNmnlzr | UndefClassfr ; 
	PsorNum = PsorSg | PsorPl | NoPsorNum ;
	PsorPers = PsorP1 | PsorP2 | PsorP3 | Undef | NoPoss; 

	-- PARAMS FOR THE DEMONSTRATIVES
	Position = Standing | Sitting | Lying | Coming | Going;
	Presence = Absent | Present;
	Distance = Close | Far ;

	--PARAMS FOR THE (PREDICATE): 
	--POLARITY
	SForm = SPos | SNeg;
	
	--PARAMS verb

	-- Aspect pre(-7)
	AspectPre = ACompl | AIncompl | ADur | ANone ;
	-- Negation (-6)
	Negation = NegMain | NegSub | NegCondImp | NNone ; 
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
	-- Aspecto sufix (+2)
	AspectPost = AAtel | ATel | APNone ;

	-- Clitics (+4 a +7) - Simplificados
	RelClitic = RelT | RelNone ;           
	DirI = GoingDirI | GoingStraight | GoingTogether | GoingInside | GoingAgainst | Intens | DirINone ; 
	DirII = Outward | Upward | Inward | Toward | Backward | Downward | Apart | AbsentDirII | DirIINone ; 
	PronClitic = I | Ga | E | Go | PronNone ; 
	SemRole = WaDative | DomBenefect | CAllative | LoAdessive | Theme | GiGoal | SemNone ; 

  oper
	
	-- Tipos simplificados para morfologia
	ALNBL : Type = {
		alnbl : Str;
		inlnbl : Str
	};
	
	PSORPREF : Type = {
		p1sg : Str;
		p1pl : Str;
		p2sg : Str;
		p2pl : Str;
		p3sg : Str;
		p3pl : Str;
		undef_sg : Str;
		undef_pl : Str;
		no_poss : Str
	};

	CLASSFSUFIX : Type = {
		anim_sg : Str;
		anim_pl : Str;
		cult_sg : Str;
		cult_pl : Str;
		instrum : Str;
		actor : Str;
		undef : Str
	};

	NUMBERSUFIX : Type = {s : Number => Str};
	
	-- Tipo de substantivo otimizado
	Noun : Type = {
		possInfo : PSORPREF;
		classInfo : CLASSFSUFIX;
		alienInfo : ALNBL;
		root : Str;
		g : Gender
	};
	
	NounPhrase : Type = {
		s : Str;
		g : Gender;
		n : Number
	};
			
	-- Tipo de verbo otimizado com tabela para formas verbais
	Verb : Type = {
		verbRoot : VERB_ROOT;
		vtype : ValencyType;
		valencyClit : ValencyClit;
		refl : Reflexive;
		hither : Hither;
		aspPre : AspectPre;
		neg : Negation;
		aspPost : AspectPost;
		mood : Mood;
		case_ : Case;
		
		-- Tabela para formas verbais em vez de função
		forms : Person => Number => Person => Number => Person => Number => Str
	};

	Verb_compound : Type = {
		verb_pos : Verb;
		verb_neg : Verb
	};

	-- Tipo para raiz verbal
	VERB_ROOT : Type = {
		s : Str;
		valencyClit : ValencyClit;
		refl : Reflexive;
		hither : Hither;
		aspPre : AspectPre;
		aspPost : AspectPost;
		mood : Mood;
		case_ : Case
	};
	
	-- Tipo simplificado para clíticos
	CliticsRec : Type = {
		cl4 : {rel : Bool; rep : Bool; p3 : Bool};
		cl5 : {rel : RelClitic; pers : Person; num : Number; dirI : DirI; dirII : DirII; semRole : SemRole};
		cl6 : {rel : Bool; pl : Bool};
		cl7 : {rel : Bool; pl : Bool}
	};

	-- Parâmetros para substantivos
	NounParamSet : Type = {
		alienability : Alienability;
		psorPers : PsorPers;
		psorNum : PsorNum;
		sufClassifier : SufClassifier
	};

	-- Tipos para gramática concreta
	SIMPLEKIND_KBC = Noun;
	KIND_KBC = Noun;
	
	QUAL_KBC : Type = {
		-- Campos para armazenar dados morfológicos
		root : Str;
		g : Gender;
		isVerbal : Bool;
		
		-- Formas pré-calculadas para uso em linearização
		posForm : Str;  -- Forma positiva (singular)
		negForm : Str;  -- Forma negativa (singular)
	};
	
	STATE_KBC : Type = QUAL_KBC ** {l: Level};
		
	ITEM_KBC = NounPhrase;

	-- Funções para criar componentes morfológicos
	
	mkAlienability : ALNBL = {
		alnbl = "n";
		inlnbl = ""
	};
	
	mkPsorPref : PSORPREF = {
		p1sg = "i";
		p1pl = "Ǥod";
		p2sg = "Ǥad";
		p2pl = "Ǥad";
		p3sg = "l";
		p3pl = "l";
		undef_sg = "e";
		undef_pl = "e";
		no_poss = ""
	};

	mkClassSuf : CLASSFSUFIX = {
		anim_sg = "nigo";
		anim_pl = "co";
		cult_sg = "";
		cult_pl = "ija";
		instrum = "ǤanǤa";
		actor = "Ǥikajo:";
		undef = ""
	};

	-- Funções auxiliares para acessar valores
	getAlienability : Alienability -> Str = \alien ->
		case alien of {
			Alnbl => mkAlienability.alnbl;
			Inlnbl => mkAlienability.inlnbl
		};
	
	getPsorPrefix : PsorPers -> PsorNum -> Str = \pers, num ->
		case <pers, num> of {
			<PsorP1, PsorSg> => mkPsorPref.p1sg;
			<PsorP1, PsorPl> => mkPsorPref.p1pl;
			<PsorP2, PsorSg> => mkPsorPref.p2sg;
			<PsorP2, PsorPl> => mkPsorPref.p2pl;
			<PsorP3, PsorSg> => mkPsorPref.p3sg;
			<PsorP3, PsorPl> => mkPsorPref.p3pl;
			<Undef, PsorSg> => mkPsorPref.undef_sg;
			<Undef, PsorPl> => mkPsorPref.undef_pl;
			<NoPoss, _> => mkPsorPref.no_poss;
			_ => ""
		};
	
	getClassSuffix : SufClassifier -> Number -> Str = \cls, num ->
		case <cls, num> of {
			<AnimPlant, Sg> => mkClassSuf.anim_sg;
			<AnimPlant, Pl> => mkClassSuf.anim_pl;
			<Cultivated, Sg> => mkClassSuf.cult_sg;
			<Cultivated, Pl> => mkClassSuf.cult_pl;
			<Instrum, _> => mkClassSuf.instrum;
			<ActorNmnlzr, _> => mkClassSuf.actor;
			<UndefClassfr, _> => mkClassSuf.undef
		};
	-- Conversão de Predef.PBool pra Prelude.Bool
    toBool : PBool -> Bool = \pbool ->
        case pbool of {
            PTrue => True;
            PFalse => False
        };
	getNumberSuffix : Str -> Number -> Str = \root, num ->
		case num of {
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
		};
	getKindRoot : KIND_KBC -> Str = \k -> k.root ;
	getKindGender : KIND_KBC -> Gender = \k -> k.g ;
	-- Função para criar conjuntos de parâmetros personalizados
	mkNounParamSet : Alienability -> PsorPers -> PsorNum -> SufClassifier -> NounParamSet =
		\a, pp, pn, sc -> {
			alienability = a;
			psorPers = pp;
			psorNum = pn;
			sufClassifier = sc
		};

	-- Definir apenas as constantes realmente necessárias
	defaultNounParamSet : NounParamSet = mkNounParamSet Inlnbl NoPoss NoPsorNum UndefClassfr;
	customNounParamSet : NounParamSet = mkNounParamSet Inlnbl NoPoss PsorSg UndefClassfr;
	customNounParamSet4 : NounParamSet = mkNounParamSet Inlnbl NoPoss PsorPl UndefClassfr;

	customNounParamSet2 : NounParamSet = mkNounParamSet Inlnbl PsorP2 PsorPl AnimPlant;
	customNounParamSet3 : NounParamSet = mkNounParamSet Alnbl NoPoss PsorSg UndefClassfr;

	-- Clíticos padrão para testes
	CliticsRec_test_params : CliticsRec = {
		cl4 = {rel = False; rep = False; p3 = False};
		cl5 = {rel = RelNone; pers = PNone; num = Sg; dirI = DirINone; dirII = DirIINone; semRole = SemNone};
		cl6 = {rel = False; pl = False};
		cl7 = {rel = False; pl = False}
	};

	CliticsRec_test_params_2 : CliticsRec = {
        cl4 = {rel = True; rep = False; p3 = True}; -- t- (relacional), e- (terceira pessoa)
        cl5 = {rel = RelT; pers = P1; num = Sg; dirI = GoingDirI; dirII = Outward; semRole = WaDative}; -- t- (rel), i (P1 Sg), jo (DirI), ce (DirII), wa (dativo)
        cl6 = {rel = False; pl = True}; -- niwac (plural)
        cl7 = {rel = True; pl = False} -- t- (relacional)
      };

	-- Verbo vazio padrão
	emptyVerb : Verb = {
		verbRoot = {
			s = "";
			valencyClit = VNone;
			refl = RNone;
			hither = HNone;
			aspPre = ANone;
			aspPost = APNone;
			mood = MNone;
			case_ = None
		};
		vtype = Unacc;
		valencyClit = VNone;
		refl = RNone;
		hither = HNone;
		aspPre = ANone;
		neg = NNone;
		aspPost = APNone;
		mood = MNone;
		case_ = None;
		
		forms = table {
			_ => table {
				_ => table {
					_ => table {
						_ => table {
							_ => table {
								_ => ""
							}
						}
					}
				}
			}
		}
	};

	-- Substantivo vazio padrão - CORRIGIDO com campos vazios
	emptyNoun : Noun = {
		possInfo = {
			p1sg = "";
			p1pl = "";
			p2sg = "";
			p2pl = "";
			p3sg = "";
			p3pl = "";
			undef_sg = "";
			undef_pl = "";
			no_poss = ""
		};
		classInfo = {
			anim_sg = "";
			anim_pl = "";
			cult_sg = "";
			cult_pl = "";
			instrum = "";
			actor = "";
			undef = ""
		};
		alienInfo = {
			alnbl = "";
			inlnbl = ""
		};
		root = "";
		g = Masc
	};

	empty_Verb_compound : Verb_compound = {
		verb_pos = emptyVerb;
		verb_neg = emptyVerb
	};
	
	-- Funções auxiliares para morfologia
	
	-- Função para verificar se uma string termina em vogal
	endsInVowel : Str -> Bool = \s ->
		case s of {
			_ + ("a"|"e"|"i"|"o"|"u"|"A"|"E"|"I"|"O"|"U") => True;
			_ => False
		};
	
	-- Função para verificar se um caractere é alveolar
	isAlveolar : Str -> Bool = \s -> case s of {
		_ + ("t"|"d"|"d:"|"n"|"n:"|"l"|"l:"|"T"|"D"|"D:"|"N"|"N:"|"L"|"L:") => True;
		_ => False
	};

	-- Função para verificar se um caractere é consoante
	isConsonant : Str -> Bool = \s -> case s of {
		_ + ("b"|"c"|"d"|"f"|"g"|"h"|"j"|"k"|"l"|"m"|"n"|"p"|"q"|"r"|"s"|"t"|
		"v"|"w"|"x"|"y"|"z"|"B"|"C"|"D"|"F"|"G"|"H"|"J"|"K"|"L"|"M"|"N"|
		"P"|"Q"|"R"|"S"|"T"|"V"|"W"|"X"|"Y"|"Z") => True;
		_ => False
	};
	
	
	
	-- Função para ajustar o prefixo do possuidor
	adjustPsorPrefix : Str -> Str -> Str = \prefix, root ->
		case prefix of {
			"l" => case isAlveolar root of {
				True => "";
				False => "l"
			};
			"Ǥod" => case isConsonant root of {
				True => "Go";
				False => "God"
			};
			"Ǥad" => case isConsonant root of {
				True => "Ǥa";
				False => "Ǥad"
			};
			_ => prefix
		};
	
	-- Função para ajustar o marcador de alienabilidade
	adjustAlienability : Str -> Str -> Str = \marker, root ->
        case marker of {
            "n" => if_then_Str (toBool (eqStr (take 1 root) "n")) "" "n";
            _ => marker
        };
	
	-- Função auxiliar para obter a forma do substantivo
	getNounForm : Noun -> NounParamSet -> Number -> Str = \noun, params, num ->
		let
			possPrefix = getPsorPrefix params.psorPers params.psorNum;
			alienabilityMarker = getAlienability params.alienability;
			sufixClassifier = getClassSuffix params.sufClassifier num;
			sufixNumberStr = getNumberSuffix (getKindRoot noun) num;
			
			-- Ajuste do prefixo do possuidor
			adjustedPsorPrefix = adjustPsorPrefix possPrefix (getKindRoot noun);
			
			-- Ajuste do marcador de alienabilidade
			adjustedAlienabilityMarker = adjustAlienability alienabilityMarker (getKindRoot noun)
		in
			glue adjustedPsorPrefix (glue adjustedAlienabilityMarker (glue (getKindRoot noun) (glue sufixClassifier sufixNumberStr)))
			;
	
	-- Função otimizada para criar substantivos
	mkNoun : Str -> Gender -> Noun = \root, g -> {
		possInfo = mkPsorPref;
		classInfo = mkClassSuf;
		alienInfo = mkAlienability;
		root = root;
		g = g
	};
	
	-- NOVAS FUNÇÕES AUXILIARES PARA ENCAPSULAR ACESSO A CAMPOS
	
	-- Função para obter a string de um substantivo
	getNounString : Noun -> NounParamSet -> Number -> Str = \noun, params, num ->
		getNounForm noun params num;
	
	-- Função para obter o gênero de um substantivo
	getNounGender : Noun -> Gender = \noun -> noun.g;
	
	-- Função para criar um KIND_KBC a partir de um SIMPLEKIND_KBC
	mkKindKbc : SIMPLEKIND_KBC -> KIND_KBC = \sk -> sk;
	mkKind = mkKindKbc;
	-- Função para criar um SIMPLEKIND_KBC a partir de um nome próprio
	mkProperNameKbc : Str -> Gender -> SIMPLEKIND_KBC = \name, gender -> 
		mkNoun name gender;
	
	-- FUNÇÕES PARA VERBOS - Modularizadas
	
	-- Funções auxiliares para morfologia verbal
	
	mkAspPre : AspectPre -> Str = \asp -> case asp of {
		ACompl => "jaǤ";
		AIncompl => "baǤa";
		ADur => "banaǤa";
		ANone => ""
	};
	
	mkNegation : Negation -> Str = \neg -> case neg of {
		NegMain => "aǤ";
		NegSub => "daǤa";
		NegCondImp => "nǤa";
		NNone => ""
	};
	
	mkMood : Mood -> Str = \mood -> case mood of {
		MDes => "domǤa";
		MCond => "dǤa";
		_ => ""
	};
	
	mkNumPre : Person -> Number -> Str = \p, n -> case <p, n> of {
		<P3, Pl> => "o";      -- P3 plural subject
		<Impers, _> => "et";  -- Impersonal
		<_, Pl> => "gi";      -- Plural optional?
		_ => ""               -- Nenhum
	};
	
	-- Funções específicas para cada tipo de valência
	
	mkPersonUnacc : Person -> Number -> Str -> Str = \subj, num, root ->
		case <subj, num> of {
			<P1, _> => case root of { 
				("t"|"d"|"n"|"T"|"D"|"N") + _ => "i"; 
				_ => "j" 
			};
			<P2, _> => "a";
			<P3, Sg> => case root of { 
				("p"|"b"|"t"|"d"|"k"|"g"|"P"|"B"|"T"|"D"|"K"|"G") + _ => ""; 
				"a"|"A" + _ => "w"; 
				"n"|"N" + _ => "a"; 
				_ => "y" 
			};
			<P3, Pl> => "n";
			<Impers, _> => "eti";
			_ => ""
		};
	
	mkPersonUnerg : Person -> Number -> Str -> Str = \subj, num, root ->
		case <subj, num> of {
			<P1, _> => case root of { 
				("t"|"d"|"n"|"T"|"D"|"N") + _ => "i"; 
				_ => "j" 
			};
			<P2, _> => "a";
			<P3, _> => case root of { 
				("p"|"b"|"t"|"d"|"k"|"g"|"P"|"B"|"T"|"D"|"K"|"G") + _ => ""; 
				"a"|"A" + _ => "w"; 
				"n"|"N" + _ => "a"; 
				_ => "y" 
			};
			<Impers, _> => "eti";
			_ => ""
		};
	
	mkPersonTrans : Person -> Number -> Person -> Number -> Str -> Str = 
		\subj, subjNum, obj, objNum, root ->
			case <subj, subjNum, obj, objNum> of {
				<P1, Sg, P1, Sg> => "i";  -- 1sg > 1sg (reflexivo)
				<P1, _, P2, _> => "Ǥa";  -- 1 > 2sg
				<P1, _, P3, _> => case root of { 
					("t"|"d"|"n"|"T"|"D"|"N") + _ => "i";
					_ => "j" 
				};
				<P1, Pl, P1, Sg> => "i";  -- 1sg OBJ > 1sg/PL SUB (=1sg OBJECT)
				<P1, Sg, P1, Pl> => "Ǥo";  --  (object prefix)
				<P1, Pl, P1, Pl> => "Ǥo";  -- 1pl > 1pl (reflexivo)
				<P2, _, P1, Sg> => "a";  -- 2 > 1sg
				<P2, _, P1, Pl> => "Ǥo";  -- 2 > 1pl
				<P2, _, P2, Sg> => "a";  -- 2 > 2sg
				<P2, _, P2, Pl> => "a";  -- 2 > 2pl
				<P2, _, P3, _> => "a";  -- 2 > 3
				<P3, _, P1, Sg> => "i";  -- 3 > 1sg
				<P3, _, P1, Pl> => "Ǥo";  -- 3 > 1pl
				<P3, _, P2, _> => "Ǥa";  -- 3 > 2
				<P3, _, P3, _> => case root of { 
					("p"|"b"|"t"|"d"|"k"|"g"|"P"|"B"|"T"|"D"|"K"|"G") + _ => root; 
					"a"|"A" + _ => "w"; 
					"n"|"N" + _ => "a"; 
					_ => "y" 
				};
				<Impers, _, P1, Sg> => "etii";  -- imp > 1sg
				<Impers, _, P1, Pl> => "etiǤo";  -- imp > 1pl
				<Impers, _, P2, _> => "etiǤa";  -- imp > 2
				<Impers, _, P3, _> => "eti";  -- imp > 3
				_ => ""
			};
	
	-- Função principal que delega para as funções específicas
	mkPerson : ValencyType -> Person -> Number -> Case -> Person -> Number -> Case -> Str -> Str =  
		\val, subj, numSubj, caseSubj, obj, numObj, caseObj, root ->
			case val of {
				Unacc => mkPersonUnacc subj numSubj root;
				Unerg => mkPersonUnerg subj numSubj root;
				Trans => mkPersonTrans subj numSubj obj numObj root;
				Ditrans => mkPersonTrans subj numSubj obj numObj root -- Simplificado para este exemplo
			};
	
	mkPersonClitic : ValencyType -> Person -> Number -> Str = \vtype, pers, num ->
		case vtype of {
			Unacc | Unerg | Trans => case <pers, num> of {
				<P1, Sg> => "i";
				<P1, Pl> => "Ǥo";
				<P2, _> => "Ǥa";
				<P3, _> => "e";
				_ => ""
			};
			Ditrans => case <pers, num> of {
				<P1, Sg> => "i";
				<P1, Pl> => "Ǥo";
				<P2, _> => "Ǥa";
				<P3, _> => "e";
				<PNone, _> => "";
				<Impers, _> => ""
			}
		};
	
	mkRefl : Reflexive -> Str = \refl -> case refl of {
		Refl => "n";
		RNone => ""
	};
	
	mkHither : Hither -> Str = \hither -> case hither of {
		HitherPos => "n";
		HNone => ""
	};
	
	mkVal : VERB_ROOT -> ValencyClit -> Str = \verb_root, val ->
		case val of {
			VGad => "Ǥad";
			VTi => "ti";
			VKan => "kan";
			VQan => "qan";
			VKon => "kon";
			VGon => "Ǥon";
			VGegi => "Ǥegi";
			VGan => "Ǥan";
			VGen => "Ǥen";
			VQen => "qen";
			VGod => "Ǥod";
			VNone => ""
		};
	
	mkAspPost : AspectPost -> Str = \asp -> case asp of {
		AAtel => "aǤa";
		ATel => "a";
		APNone => ""
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
	
	-- Função para criar raiz verbal
	mkVerbRoot : Str -> VERB_ROOT = \s -> {
		s = s;
		valencyClit = VNone;
		refl = RNone;
		hither = HNone;
		aspPre = ANone;
		aspPost = APNone;
		mood = MNone;
		case_ = None
	};
	
	-- Função para calcular a forma verbal
	calcVerbForm : Verb -> Person -> Number -> Person -> Number -> Person -> Number -> CliticsRec -> Str = 
		\verb, subj, subjNum, obj, objNum, obj2, obj2Num, cl ->
			let
				root = verb.verbRoot.s;
				
				-- Componentes do verbo
				aspPre = mkAspPre verb.aspPre;
				neg = mkNegation verb.neg;
				mood = mkMood verb.mood;
				numPre = mkNumPre subj subjNum;
				person = mkPerson verb.vtype subj subjNum verb.case_ obj objNum None root;
				refl = mkRefl verb.refl;
				hither = mkHither verb.hither;
				val = mkVal verb.verbRoot verb.valencyClit;
				aspPost = mkAspPost verb.aspPost;
				numPostStr = case verb.vtype of {
					Trans | Ditrans => mkNumPost subj subjNum obj objNum ;
					_ => mkNumPost subj subjNum PNone Sg
					} ;
				cliticStr  = mkClitics verb.vtype { cl4 = cl.cl4 ;
								cl5 = {rel = cl.cl5.rel ; pers = obj2 ; num = obj2Num ;
										dirI = cl.cl5.dirI ; dirII = cl.cl5.dirII ; semRole = cl.cl5.semRole} ;
								cl6 = cl.cl6 ; cl7 = cl.cl7 };
				-- Concatenação dos componentes
				verbForm = glue aspPre (glue neg (glue mood (glue numPre (glue person (glue refl (glue hither (glue root (glue (glue (glue val aspPost) numPostStr) cliticStr ))))))))
			in
				verbForm;

				
	
	-- Função para criar verbo
	mkVerb : VERB_ROOT -> ValencyType -> ValencyClit -> CliticsRec -> Negation -> Verb =
		\root, vtype, val, cl, neg -> {
			verbRoot = root;
			vtype = vtype;
			valencyClit = val;
			refl = root.refl;
			hither = root.hither;
			aspPre = root.aspPre;
			neg = neg;
			aspPost = root.aspPost;
			mood = root.mood;
			case_ = root.case_;
			
			-- Pré-calcular todas as formas verbais possíveis
			forms = table {
				subj => table {
					subjNum => table {
						obj => table {
							objNum => table {
								obj2 => table {
									obj2Num => calcVerbForm {
                                        verbRoot = root;
                                        vtype = vtype;
                                        valencyClit = val;
                                        refl = root.refl;
                                        hither = root.hither;
                                        aspPre = root.aspPre;
                                        neg = neg;
                                        aspPost = root.aspPost;
                                        mood = root.mood;
                                        case_ = root.case_;
                                        forms = table {_ => table {_ => table {_ => table {_ => table {_ => table {_ => ""}}}}}}
                                    } subj subjNum obj objNum obj2 obj2Num cl
								}
							}
						}
					}
				}
			}
		};
	
	-- Função para criar verbo composto (positivo e negativo)
	mkVerbCompound : VERB_ROOT -> ValencyType -> ValencyClit -> CliticsRec -> Verb_compound =
		\root, vtype, val, cl -> {
			verb_pos = mkVerb root vtype val cl NNone;
			verb_neg = mkVerb root vtype val cl NegMain
		};
	
	-- Função para criar qualidades (verificar possibilidades de inserir os parâmetros do verbo)
	mkQualKbc : Str -> Gender -> Bool -> ValencyClit -> NounParamSet -> QUAL_KBC = \root, g, isVerb, val, noun_params -> {
		root = root;
		g = g;
		isVerbal = isVerb;
		
		-- Usa a função existente mkVerbCompound e extrai as formas necessárias
		posForm = case isVerb of {
			True => 
				let 
					verbRoot = mkVerbRoot root ** {valencyClit = val};
					verbComp = mkVerbCompound verbRoot Unacc val 
							{cl4={rel=False;rep=False;p3=False}; 
							cl5={rel=RelNone;pers=PNone;num=Sg;dirI=DirINone;dirII=DirIINone;semRole=SemNone}; 
							cl6={rel=False;pl=False}; 
							cl7={rel=False;pl=False}};
					-- Extrai a forma positiva do verbo
					verbForm = verbComp.verb_pos.forms ! P3 ! Sg ! PNone ! Sg ! PNone ! Sg
				in verbForm;
			False => 
				let noun = mkNoun root g;
					nounForm = getNounForm noun noun_params Sg
				in nounForm
		};
		
		negForm = case isVerb of {
			True => 
				let 
					verbRoot = mkVerbRoot root ** {valencyClit = val};
					verbComp = mkVerbCompound verbRoot Unacc val 
							{cl4={rel=False;rep=False;p3=False}; 
							cl5={rel=RelNone;pers=PNone;num=Sg;dirI=DirINone;dirII=DirIINone;semRole=SemNone}; 
							cl6={rel=False;pl=False}; 
							cl7={rel=False;pl=False}};
					-- Extrai a forma negativa do verbo
					verbForm = verbComp.verb_neg.forms ! P3 ! Sg ! PNone ! Sg ! PNone ! Sg
				in verbForm;
			False => 
				glue "aǤ" (let noun = mkNoun root g;
						nounForm = getNounForm noun customNounParamSet Sg
					in nounForm)
		}
	};
	
	-- NOVAS FUNÇÕES AUXILIARES PARA ENCAPSULAR ACESSO A CAMPOS DE QUAL_KBC
	
	-- Função para obter a forma positiva de uma qualidade
	getQualPosForm : QUAL_KBC -> Str = \qual -> qual.posForm;
	
	-- Função para obter a forma negativa de uma qualidade
	getQualNegForm : QUAL_KBC -> Str = \qual -> qual.negForm;
	
	-- Função para verificar se uma qualidade é verbal
	isQualVerbal : QUAL_KBC -> Bool = \qual -> qual.isVerbal;
	
	-- Função para obter o gênero de uma qualidade
	getQualGender : QUAL_KBC -> Gender = \qual -> qual.g;
	
	-- Função para criar sintagmas nominais com demonstrativos
	mkNounPhrase : Presence -> Position -> Distance -> KIND_KBC -> Number -> NounParamSet -> NounPhrase = 
        \pres, pos, dist, kind, n, params -> {
            s = let
                distanceMorph = case dist of {
                    Close => "nǤ";
                    _ => ""
                };
                genderMorph = case getKindGender kind of {
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
                pluralMorph = case n of {
					Sg => "";
					Pl =>"idiwa"

				}; 
                demonstrative = glue distanceMorph (glue genderMorph (glue presencePositionMorph pluralMorph)) 
                ;
                nounForm = getNounForm kind params n;
                np = demonstrative ++ nounForm
            in
                np;
            g = getKindGender kind;
            n = n
        };


	mkDemonstrativeKind : Presence -> Position -> Distance -> KIND_KBC -> Number -> NounParamSet -> ITEM_KBC =
		\pres, pos, dist, kind, n, noun_params ->
			let np = mkNounPhrase pres pos dist kind n noun_params
			in mkItemKbc np;	

	-- NOVA FUNÇÃO AUXILIAR PARA ENCAPSULAR ACESSO A CAMPOS DE NOUNPHRASE
	
	-- Função para obter a string de um sintagma nominal
	getNounPhraseString : NounPhrase -> Str = \np -> np.s;
	
	-- Função para obter o gênero de um sintagma nominal
	getNounPhraseGender : NounPhrase -> Gender = \np -> np.g;
	
	-- Função para obter o número de um sintagma nominal
	getNounPhraseNumber : NounPhrase -> Number = \np -> np.n;
	
	-- Função para criar sintagmas nominais com qualidades
	mkNounPhraseWithQual : Presence -> Position -> Distance -> Noun -> Number -> 
						NounParamSet -> Str -> Bool -> ValencyClit -> NounParamSet -> NounPhrase = 
		\pres, pos, dist, nounRoot, n, nounParams, 
		
		qualRoot, isVerb, val, qualParams -> {
			s = let 
				nounPhrase = mkNounPhrase pres pos dist nounRoot n nounParams;
				qual = (mkQualKbc qualRoot nounRoot.g isVerb val qualParams).posForm;
				
			in
				nounPhrase.s ++ qual;
			g = nounRoot.g;
			n = n
		};
	
	-- Função para criar itens a partir de sintagmas nominais
	mkItemKbc : NounPhrase -> ITEM_KBC = \np -> np;
	
	-- Função para criar verbos específicos
	alepe : VERB_ROOT = mkVerbRoot "al:epe" ** {valencyClit = VGad};
	
	-- NOVAS FUNÇÕES AUXILIARES PARA DEMONSTRATIVOS
	
	-- Função para criar demonstrativo "This"
	mkThisKind : KIND_KBC -> ITEM_KBC = \kind ->
		mkNounPhrase Present Standing Close kind Sg customNounParamSet;
	
	-- Função para criar demonstrativo "That"
	mkThatKind : KIND_KBC -> ITEM_KBC = \kind ->
		mkNounPhrase Present Standing Far kind Sg customNounParamSet;
	
	-- Função para criar demonstrativo "These"
	mkTheseKind : KIND_KBC -> ITEM_KBC = \kind ->
		mkNounPhrase Present Standing Close kind Pl customNounParamSet;
	
	-- Função para criar demonstrativo "Those"
	mkThoseKind : KIND_KBC -> ITEM_KBC = \kind ->
		mkNounPhrase Present Standing Far kind Pl customNounParamSet;

    mkItemNonDeitic: NounPhrase -> ITEM_KBC = \np -> mkItemKbc np;



	teste_noun= mkNoun "Ǥoneleegiwa" Masc;
	test_this = mkNounPhrase Absent Standing Close  (mkNoun "Gonelegiwa" Masc) Sg  customNounParamSet;
	test_that = mkNounPhrase Present Standing Far  (mkNoun "Gonelegiwa" Masc) Sg  customNounParamSet;
	test_these= mkNounPhrase Present Standing Close  (mkNoun "Gonelegiwa" Masc) Pl  customNounParamSet;
  	test_those = mkNounPhrase Present Standing Far  (mkNoun "Gonelegiwa" Masc) Pl  customNounParamSet;
}
