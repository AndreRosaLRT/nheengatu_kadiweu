concrete GraKbc2 of Lex = open OperKbc2, Oper, Prelude in {
  flags coding=utf8 ;

  lincat
    Comment = {s : Str};
    Polarity = {s : Str};
    Kind = KIND_KBC;
    SimpleKind = SIMPLEKIND_KBC;
    Quality = QUAL_KBC;
    Property = QUAL_KBC;
    Location = QUAL_KBC;
    State = { verb : Str ; noun : Str ; isVerbal : Bool ; l : Level };
    Item = ITEM_KBC;
    NonDeitic = ITEM_KBC;
    Deitic = ITEM_KBC;
    Psor = ITEM_KBC;
    PossPro = NounParamSet;
    PossKind = { s : Str ; g : Gender };
    Num = { num_val : Number };
    Action = ActionRec;
    S = {s : Str};

  lin
    -- Polarity
    Yes = {s = ""};
    No = {s = "aǤ"};
{-
    Pred pol item st = {
      s = let
            subject_str : Str = item.s;
            predicate_str : Str = case st.isVerbal of {
              True => st.verb;
              False => let
                        neg_prefix_for_nominal : Str = case pol.s of {
                          "aǤ" => "aǤ ";
                          _ => ""
                        };
                      in neg_prefix_for_nominal ++ st.noun
            };
          in subject_str ++ " " ++ predicate_str
    };

    StageLevelState qual = {
      verb = qual.verb.s; -- Usando o novo campo s
      noun = qual.s;
      isVerbal = qual.isVerbal;
      l = Stage
    };
    IndLevelState qual = {
      verb = qual.verb.s; -- Usando o novo campo s
      noun = qual.s;
      isVerbal = qual.isVerbal;
      l = Ind
    };

    Mod quality kind_mod = {
      s = let
            kind_form = kind_mod.s;
            qual_form : Str = case quality.isVerbal of {
              True => quality.verb.s;
              False => quality.s
            };
          in kind_form ++ " " ++ qual_form;
      g = kind_mod.g
    };

    Poss psor_item possessed_item_np = {
      s = psor_item.s ++ " " ++ possessed_item_np.s;
      g = possessed_item_np.g;
      n = possessed_item_np.n;
      p = P3
    };
    Poss_ psor_params simple_kind_noun = {
      s = let
            noun = mkNoun simple_kind_noun.s simple_kind_noun.g;
            features : NounFeatures = psor_params ** {num = Sg};
            form = noun.getForm features;
          in form;
      g = simple_kind_noun.g
    };
    mkPsor num_arg sk_possessor = {
      s = let
            noun = mkNoun sk_possessor.s sk_possessor.g;
            features : NounFeatures = defaultNounParamSet ** {num = num_arg.num_val};
            form = noun.getForm features;
          in form;
      g = sk_possessor.g;
      n = num_arg.num_val;
      p = P3
    };
    mkPsor_ num_arg pk_possessor = {
      s = let
            noun = mkNoun pk_possessor.s pk_possessor.g;
            features : NounFeatures = defaultNounParamSet ** {num = num_arg.num_val};
            form = noun.getForm features;
          in form;
      g = pk_possessor.g;
      n = num_arg.num_val;
      p = P3
    };

    mkKind sk = sk;
    mkKind_ pk = pk;

    mkItemDeitic deitic_item = deitic_item;
    mkItemNonDeitic non_deitic_item = non_deitic_item;
    mkPropQual qual = qual;
    mkPropLoc loc_qual = loc_qual;
    Very quality = quality;

    TheSG kind_arg = {
      s = let
            noun = mkNoun kind_arg.s kind_arg.g;
            features : NounFeatures = defaultNounParamSet ** {num = Sg};
            form = noun.getForm features;
            det = "nǤi" ++ (case kind_arg.g of {Masc => "i"; Fem => "a"}) ++ "da";
          in det ++ form;
      g = kind_arg.g;
      n = Sg
    };
    ThePL kind_arg = {
      s = let
            noun = mkNoun kind_arg.s kind_arg.g;
            features : NounFeatures = defaultNounParamSet ** {num = Pl};
            form = noun.getForm features;
            det = "nǤi" ++ (case kind_arg.g of {Masc => "idiwa"; Fem => "idiwa"});
          in det ++ form;
      g = kind_arg.g;
      n = Pl
    };
    This kind_arg = {
      s = let
            noun = mkNoun kind_arg.s kind_arg.g;
            features : NounFeatures = defaultNounParamSet ** {num = Sg};
            form = noun.getForm features;
            det = "nǤi" ++ (case kind_arg.g of {Masc => "i"; Fem => "a"}) ++ "da";
          in det ++ form;
      g = kind_arg.g;
      n = Sg
    };
    That kind_arg = {
      s = let
            noun = mkNoun kind_arg.s kind_arg.g;
            features : NounFeatures = defaultNounParamSet ** {num = Sg};
            form = noun.getForm features;
            det = "" ++ (case kind_arg.g of {Masc => "i"; Fem => "a"}) ++ "da";
          in det ++ form;
      g = kind_arg.g;
      n = Sg
    };
    These kind_arg = {
      s = let
            noun = mkNoun kind_arg.s kind_arg.g;
            features : NounFeatures = defaultNounParamSet ** {num = Pl};
            form = noun.getForm features;
            det = "nǤi" ++ (case kind_arg.g of {Masc => "idiwa"; Fem => "idiwa"});
          in det ++ form;
      g = kind_arg.g;
      n = Pl
    };
    Those kind_arg = {
      s = let
            noun = mkNoun kind_arg.s kind_arg.g;
            features : NounFeatures = defaultNounParamSet ** {num = Pl};
            form = noun.getForm features;
            det = "" ++ (case kind_arg.g of {Masc => "idiwa"; Fem => "idiwa"});
          in det ++ form;
      g = kind_arg.g;
      n = Pl
    };

    He = {s = "eǤe"; g = Masc; n = Sg; p = P3};
    She = {s = "eǤe"; g = Fem; n = Sg; p = P3};
    It = {s = "eǤe"; g = Masc; n = Sg; p = P3};
    They = {s = "eǤeadi"; g = Masc; n = Pl; p = P3};
    I = {s = "aji"; g = Masc; n = Sg; p = P1};
    YouSG = {s = "agami"; g = Masc; n = Sg; p = P2};
    YouPL = {s = "agamiadi"; g = Masc; n = Pl; p = P2};
    We = {s = "igidi"; g = Masc; n = Pl; p = P1};

    My = defaultNounParamSet ** {psorP = PsorP1; psorN = PsorSg; alien = Inlnbl};
    Our = defaultNounParamSet ** {psorP = PsorP1; psorN = PsorPl; alien = Inlnbl};
    YourSG = defaultNounParamSet ** {psorP = PsorP2; psorN = PsorSg; alien = Inlnbl};
    YourPL = defaultNounParamSet ** {psorP = PsorP2; psorN = PsorPl; alien = Inlnbl};
    His = defaultNounParamSet ** {psorP = PsorP3; psorN = PsorSg; alien = Inlnbl};
    Her = defaultNounParamSet ** {psorP = PsorP3; psorN = PsorSg; alien = Inlnbl};
    Its = defaultNounParamSet ** {psorP = PsorP3; psorN = PsorSg; alien = Inlnbl};
    Their = defaultNounParamSet ** {psorP = PsorP3; psorN = PsorPl; alien = Inlnbl};

    On item_arg = mkQualKbc "ka" item_arg.g False VNone;
    With item_arg = mkQualKbc "jibe" item_arg.g False VNone;
    In item_arg = mkQualKbc "ka" item_arg.g False VNone;
    Near item_arg = mkQualKbc "iditibene" item_arg.g False VNone;
    Here = mkQualKbc "jena" Masc False VNone;
    There = mkQualKbc "ema" Masc False VNone;

    SG = {num_val = Sg};
    PL = {num_val = Pl};-}

    Man = mkSimpleKind "Ǥoneleegiwa" Masc;
   

    Strong = mkQualKbc "nigicatini" Masc True VNone;

    Alepe = mkActionRec alepe Ditrans VNone CliticsRec_test_params;
}