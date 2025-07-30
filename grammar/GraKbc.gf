-- --Extension GrammKbc (Kadiwéu) of GrammYEP - a multilingual computational grammar for Nheengatu, English, and Portuguese
-- (c) 2020 Leonel Figueiredo de Alencar
-- Licensed under the terms of the GNU Lesser General Public License, version 2.1
-- See LICENSE or visit the URL
-- https://www.gnu.org/licenses/old-licenses/lgpl-2.1.en.html



concrete GraKbc of Lex = open Oper, OperKbc, Prelude, Predef in {
 flags coding=utf8 ;
  lincat
  {- ; Psor ;  ; ; 
     ; Location ; Deitic  ; Polarity ; PossPro ; PossKind ; Num ; -}
    Comment = EXPR;
    Polarity = {s : Str}; 
    Kind = KIND_KBC;
    SimpleKind = SIMPLEKIND_KBC;
    State = STATE_KBC;
    Quality =QUAL_KBC;
    --Property =PROPERTY;
    Item, NonDeitic = ITEM_KBC ; --from OperKbc; v
    Action = Verb;
    Property = QUAL_KBC ;  -- Adicionado
    Location = QUAL_KBC ;  -- Adicionado

  lin
    Yes = {s=""};
    No = {s= "aǥ"  } ;
    Pred pol item st =
      let
        subject : Str = item.s ;
        predForm : Str = case st.isVerbal of {
          True => st.verb.s ! P3 ! item.n ! PNone ! Sg ! PNone ! Sg ;
          False => let
                    nounParams : NounParamSet = customNounParamSet ;
                    qualForm : Str = getNounForm st.noun nounParams item.n
                  in qualForm
        } ;
        finalPred : Str = glue pol.s predForm ;
        sentence : Str = subject ++ finalPred
      in
        { s = sentence } ;
    
    mkItemNonDeitic nonvar =  mkItemKbc nonvar ;
    mkPropQual qual = qual ;
    IndLevelState qual = qual ** {l = Ind} ;
    
     --
    --let f: FORM = item.s ! st.c ! st.l ;  pred: Str = YrlCopula item.n item.p st.l st.c (st.s ! item.n ! item.p) st.v st.nc pol.s in
     --{s= f ! Nom ! NCI ++ pred } ;


  --Lin of open word categories: --
    -- Nouns (SimpleKind): !REVIEW: I need some help to check the lexemes for nouns in this
  Man  = mkNoun "ǥoneleegiwa" Masc; -- This is a simple realization  of the noun, as it is possible to realize some other morphemes. I will have to discuss with Leonel how to deal with these cases.
  Canoe = mkNoun "niwatece" Fem;
  Food = mkNoun ("oligi"|"weenigi")Fem; --!check
  Bird = mkNoun "ilaaǥagi" Masc;
  Blood = mkNoun"awodi" Masc; --check: in the dict its Neutral...
  Branch = mkNoun "libiwe" Masc;-- tronco="libatadi"|"nibatadi"; !!check
  Brother_Of_Woman = mkNoun "nioxoa" Masc; 
  Child = mkNoun "nigaanigi" Masc;
  City = mkNoun "nigotaǥa" Fem ;
  Door = mkNoun "epoagi" Masc;
  Dove = mkNoun "yotibi" Fem ;
  Fish = mkNoun "noǥojegi" Masc;
  Grandfather = mkNoun "elokodi" Masc;
  Hook = mkNoun "lomiigo" Fem ;
  House = mkNoun "diimigi" Masc;
  Husband = mkNoun "nodawa" Masc;
  Knife = mkNoun "nodaajo" Masc ; 
  Language = mkNoun "ioladi" Masc ;
  Life = mkNoun "ewiǥa" Masc; 
  Milk = mkNoun "lotiidi" Masc ;
  Nest = mkNoun "libato" Masc ;
  Path = mkNoun "naigi" Masc ;
  Picture = mkNoun "niwigo" Masc;  
  Pit = mkNoun "lolagi" Fem;  
  Pumpkin = mkNoun "aboobile" Fem; 
  River = mkNoun "akiidi" Masc ;  
  Seed = mkNoun "lolagi" Fem;  
  Daughter_Of_Man, Daughter_Of_Woman = mkNoun "niona" Fem ;  
  Son_Of_Man, Son_Of_Woman = mkNoun "nionigi" Masc; --ni-oonigi (fala da mulher)??
  Stone = mkNoun "wetiǥa" Fem;
  Street = mkNoun "ladigodi" Fem ;
  Toucan = mkNoun "Gatodi" Masc;
  Tree = mkNoun "niale" Fem;  
  Water = mkNoun "ninyoǥodi" Masc;
  Wife = mkNoun "nodawa" Fem;
  Woman = mkNoun "iwaalo" Fem;  
  Word = mkNoun "notaǥa" Masc; --check gender
  Antonio =mkProperNameKbc "Antônio" Masc;
  Joanna = mkProperNameKbc "Joana" Fem;
  Maria = mkProperNameKbc "Maria" Fem;
  Pedro = mkProperNameKbc "Pedro" Fem;
  
  --VERBS
  Alepe = (mkVerb alEpe Ditrans VNone {cl4={rel=True;rep=False;p3=False}; cl5={rel=RelT;pers=P1;num=Sg;dirI=GoingDirI;dirII=Outward;semRole=WaDative}; cl6={rel=True;pl=True}; cl7={rel=True;pl=True}});
-- TestBool = testBool True ;
 -- Ant = ;
  
{--  COULD NOT FIND THESE ON DICTIONARY ;  
  Beak = ;  
  Boy = ;
  Community = ;  
  Egg = ; 
  Hog_Plum = ; 
  Tapioca_Cake = ;  
   asdf-}

  --Qualities (that can be realized by nouns or verbs (and some times, apparently, adjectives))
  --Delicious = mkQualKbc "delicousTestNoun" Masc False;

  Delicious = mkQualKbc "jemiGu" Masc True VNone; -- Realizado como verbo
  Beautiful = mkQualKbc "ejipe" Masc False VNone;  -- Realizado como substantivo
  Good = mkQualKbc "poranG" Masc True VNone;      -- Realizado como verbo
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

  mkKind sk = mkKindKbc sk;
  
  
  This kind = mkNounPhrase Present Standing Close (mkKind kind) Sg  customNounParamSet;
  That kind = mkNounPhrase Present Standing Far (mkKind kind) Sg  customNounParamSet;
  These kind = mkNounPhrase Present Standing Close (mkKind kind) Pl  customNounParamSet;
  Those kind = mkNounPhrase Present Standing Far (mkKind kind) Pl  customNounParamSet;
  
  --demonst Number Presence Position NounParamSet Noun = demonstDet Number Presence Position NounParamSet Noun; 
  
  --This = demonstDet Sg  "ka" "ida" "ini" "idi" "ina" "ijo" "ada" "ani" "adi" "ana" "ajo" "idiwa"  Present Coming {alienability = Alnbl;psorPers = PsorP3;psorNum = PsorPl;sufClassifier = AnimPlant;number = Sg}  ; --I have to check this function. I might have to create functions specific to KBC because other parameters other the distance, gender and number apply
 

}