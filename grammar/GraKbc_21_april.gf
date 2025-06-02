-- Versão corrigida do GraKbc que evita acessos diretos a campos de registros
-- e usa funções auxiliares para todas as operações

concrete GraKbc_21_april of Lex = open Oper, OperKbc_21_april, Prelude in {
flags coding=utf8 ;
lincat
    Comment = {s : Str};
    Polarity = {p : Bool};  -- Usando booleano em vez de string para evitar pattern matching em strings
    Kind = KIND_KBC;
    SimpleKind = SIMPLEKIND_KBC;
    State = STATE_KBC;
    Quality = QUAL_KBC;
    Property = QUAL_KBC;
    Location = QUAL_KBC;
    Item, NonDeitic = ITEM_KBC;
    --Action = Verb_compound; -- COMENTADO

lin
    Yes = {p = True};
    No = {p = False};

    -- Função Pred corrigida usando funções auxiliares para acessar campos
   {- Pred pol item st = {
        s = let
            subject = getNounPhraseString item;  -- Usa função auxiliar em vez de item.s
            predForm = case pol.p of {
                True => getQualPosForm st;  -- Usa função auxiliar em vez de st.posForm
                False => getQualNegForm st  -- Usa função auxiliar em vez de st.negForm
            };
            sentence = subject ++ " " ++ predForm
        in
            sentence
    };-}

    --mkItemNonDeitic nonvar = mkItemKbc nonvar;
    mkPropQual qual = qual;
    IndLevelState qual = qual ** {l = Ind};

    -- Substantivos (SimpleKind) - Usando mkNoun conforme solicitado
    Man = mkNoun "Ǥoneleegiwa" Masc;
    Canoe = mkNoun "niwatece" Fem;
    Food = mkNoun "oligi" Fem;
    Bird = mkNoun "ilaaǤagi" Masc;
    Blood = mkNoun "awodi" Masc;
    Branch = mkNoun "libiwe" Masc;
    Brother_Of_Woman = mkNoun "nioxoa" Masc;
    Child = mkNoun "nigaanigi" Masc;
    City = mkNoun "nigotaǤa" Fem;
    Door = mkNoun "epoagi" Masc;
    Dove = mkNoun "yotibi" Fem;
    Fish = mkNoun "noǤojegi" Masc;
    Grandfather = mkNoun "elokodi" Masc;
    Hook = mkNoun "lomiigo" Fem;
    House = mkNoun "diimigi" Masc;
    Husband = mkNoun "nodawa" Masc;
    Knife = mkNoun "nodaajo" Masc;
    Language = mkNoun "ioladi" Masc;
    Life = mkNoun "ewiǤa" Masc;
    Milk = mkNoun "lotiidi" Masc;
    Nest = mkNoun "libato" Masc;
    Path = mkNoun "naigi" Masc;
    Picture = mkNoun "niwigo" Masc;
    Pit = mkNoun "lolagi" Fem;
    Pumpkin = mkNoun "aboobile" Fem;
    River = mkNoun "akiidi" Masc;
    Seed = mkNoun "lolagi" Fem;
    Daughter_Of_Man, Daughter_Of_Woman = mkNoun "niona" Fem;
    Son_Of_Man, Son_Of_Woman = mkNoun "nionigi" Masc;
    Stone = mkNoun "wetiǤa" Fem;
    Street = mkNoun "ladigodi" Fem;
    Toucan = mkNoun "Gatodi" Masc;
    Tree = mkNoun "niale" Fem;
    Water = mkNoun "ninyoǤodi" Masc;
    Wife = mkNoun "nodawa" Fem;
    Woman = mkNoun "iwaalo" Fem;
    Word = mkNoun "notaǤa" Masc;

    -- Nomes próprios - Usando mkProperNameKbc conforme solicitado
    Antonio = mkProperNameKbc "Antônio" Masc;
    Joanna = mkProperNameKbc "Joana" Fem;
    Maria = mkProperNameKbc "Maria" Fem;
    Pedro = mkProperNameKbc "Pedro" Masc;

  --Qualities (that can be realized by nouns or verbs (and some times, apparently, adjectives))
  --Delicious = mkQualKbc "delicousTestNoun" Masc False;

    Delicious = mkQualKbc "jemiGu" Masc True VNone customNounParamSet; -- Realizado como verbo
    Beautiful = mkQualKbc "ejipe" Masc False VNone customNounParamSet;  -- Realizado como substantivo
    Good = mkQualKbc "poranG" Masc True VNone customNounParamSet;     -- Realizado como verbo
    {-Alive = ;
    Beautiful= ;
    Cheap= ;
    Dirty= ;
    Expensive= ;
    Good= ;
    Happy= ;
    Hard= ;
    Heavy= ;
    Hot= ;
    New= ;
    Red= ;
    teste git

    Round= ;
    Strong= ;-}
    -- Verbos - Comentado para simplificar a compilação inicial
    --Alepe = calcVerbForm;

    -- Função para criar tipos Kind a partir de SimpleKind
    --mkKind sk = mkKindKbc sk;

    -- Demonstrativos - Usando funções auxiliares em vez de acessar campos diretamente
    
   This kind = mkDemonstrativeKind Present Standing Close (mkKind kind) Sg  customNounParamSet;
   -- That kind =mkDemonstrativeKind Present Standing Far (mkKind kind) Sg  customNounParamSet;
   --These kind =mkDemonstrativeKind Present Standing Close (mkKind kind) Pl  customNounParamSet;
    -- Those kind =mkDemonstrativeKind Present Standing Far (mkKind kind) Pl  customNounParamSet;
}
