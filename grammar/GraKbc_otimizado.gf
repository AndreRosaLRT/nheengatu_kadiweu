-- GraKbc.gf otimizado para usar OperKbc_otimizado_calculado.gf
-- Evita acesso direto a campos e pattern matching em strings

concrete GraKbc_otimizado of Lex = open Oper, OperKbc_otimizado, Prelude, Predef in {
 flags coding=utf8 ;
  lincat
    Comment = EXPR; -- Mantido simples
    Polarity = {p : Bool};  -- Usa booleano para evitar pattern matching em strings
    Kind = KIND_KBC;        -- Usa o tipo otimizado de OperKbc
    SimpleKind = Noun; -- Usa o tipo otimizado de OperKbc
    State = STATE_KBC;       -- Usa o tipo otimizado de OperKbc
    Quality = QUAL_RECORD;      -- Usa o tipo otimizado de OperKbc
    Property = QUAL_RECORD;     -- Usa o tipo otimizado de OperKbc
    Location = QUAL_RECORD;     -- Usa o tipo otimizado de OperKbc
    Item, NonDeitic = ITEM_KBC ; -- Usa o tipo otimizado de OperKbc
    -- Action = Verb; -- Comentado conforme solicitado

  lin
    Yes = {p = True};
    No = {p = False};
   
    -- Pred otimizado: usa funções auxiliares para acessar formas
   Pred pol item st =
    {s = 
      let
        subject = item.s ;
        predForm = case st.isVerbal of{
              
              True => case pol.p of {
                True => st.qual_verb.verb_pos;
                False => st.qual_verb.verb_neg
                                    };
              False => case pol.p of {
                True => st.qual_noun.noun_pos!item.n;
                False => st.qual_noun.noun_neg!item.n
                                      }
        };
        sentence = subject ++ predForm
        in
            sentence

      };

    mkItemNonDeitic nonvar =  mkItemKbc nonvar ;
    mkPropQual qual = qual ;
    IndLevelState qual = qual ** {l = Ind} ;

    -- Nouns (SimpleKind): Usa mkNoun conforme solicitado
    Man  =  mkNoun "Ǥoneleegiwa" Masc;
    -- Descomente os outros substantivos conforme necessário, eles usarão mkNoun
    {- Canoe = mkNoun "niwatece" Fem;
    Food = mkNoun ("oligi"|"weenigi")Fem;
    Bird = mkNoun "ilaaǤagi" Masc;
    Blood = mkNoun"awodi" Masc;
    Branch = mkNoun "libiwe" Masc;
    Brother_Of_Woman = mkNoun "nioxoa" Masc; 
    Child = mkNoun "nigaanigi" Masc;
    City = mkNoun "nigotaǤa" Fem ;
    Door = mkNoun "epoagi" Masc;
    Dove = mkNoun "yotibi" Fem ;
    Fish = mkNoun "noǤojegi" Masc;
    Grandfather = mkNoun "elokodi" Masc;
    Hook = mkNoun "lomiigo" Fem ;
    House = mkNoun "diimigi" Masc;
    Husband = mkNoun "nodawa" Masc;
    Knife = mkNoun "nodaajo" Masc ; 
    Language = mkNoun "ioladi" Masc ;
    Life = mkNoun "ewiǤa" Masc; 
    Milk = mkNoun "lotiidi" Masc ;
    Nest = mkNoun "libato" Masc ;
    Path = mkNoun "naigi" Masc ;
    Picture = mkNoun "niwigo" Masc;  
    Pit = mkNoun "lolagi" Fem;  
    Pumpkin = mkNoun "aboobile" Fem; 
    River = mkNoun "akiidi" Masc ;  
    Seed = mkNoun "lolagi" Fem;  
    Daughter_Of_Man, Daughter_Of_Woman = mkNoun "niona" Fem ;  
    Son_Of_Man, Son_Of_Woman = mkNoun "nionigi" Masc;
    Stone = mkNoun "wetiǤa" Fem;
    Street = mkNoun "ladigodi" Fem ;
    Toucan = mkNoun "Gatodi" Masc;
    Tree = mkNoun "niale" Fem;  
    Water = mkNoun "ninyoǤodi" Masc;
    Wife = mkNoun "nodawa" Fem;
    Woman = mkNoun "iwaalo" Fem;  
    Word = mkNoun "notaǤa" Masc;
    Antonio =mkProperNameKbc "Antônio" Masc;
    Joanna = mkProperNameKbc "Joana" Fem;
    Maria = mkProperNameKbc "Maria" Fem;
    Pedro = mkProperNameKbc "Pedro" Masc;-} 
  
    -- Qualities: Usa mkQualKbc conforme solicitado
    Delicious = IndLevelState (getQualForm (mkQualKbc "jemiGu" Masc True VNone customNounParamSet));
    Beautiful = IndLevelState (getQualForm (mkQualKbc "ejipe" Masc False VNone customNounParamSet));
    Good = IndLevelState (getQualForm(mkQualKbc "poranG" Masc True VNone customNounParamSet));

    -- Função para criar tipos Kind a partir de SimpleKind
    mkKind sk = mkKindKbc sk;
  

   {-StageLevelState qual = {
      verb = qual.verb.s; -- Usando o novo campo s
      noun = qual.s;
      isVerbal = qual.isVerbal;
      l = Stage
    };-}
   
    -- Demonstrativos: Usa as funções auxiliares de OperKbc
    This kind = mkNounPhrase Present Standing Close (mkKind kind) Sg customNounParamSet;
    --That kind = mkThatKind kind;
   -- These kind = mkTheseKind kind;
    --Those kind = mkThoseKind kind;

}
