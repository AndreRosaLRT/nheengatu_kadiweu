-- --Extension GrammKbc (Kadiwéu) of GrammYEP - a multilingual computational grammar for Nheengatu, English, and Portuguese
-- (c) 2020 Leonel Figueiredo de Alencar
-- Licensed under the terms of the GNU Lesser General Public License, version 2.1
-- See LICENSE or visit the URL
-- https://www.gnu.org/licenses/old-licenses/lgpl-2.1.en.html



concrete GraKbc of Lex = open Oper, OperKbc, Prelude in {
 flags coding=utf8 ;
  lincat
  {- ; Psor ;  ; ; 
     ; Location ; Deitic  ; Polarity ; PossPro ; PossKind ; Num ; -}
    Comment = EXPR; 
    Kind, SimpleKind = KIND_KBC;
   --
    State = STATE_KBC;
    Quality =QUAL_KBC;
    --Property =PROPERTY;
    Item, NonDeitic = ITEM_KBC ; --from OperKbc; v
    Action = Verb;
   
  lin
    Yes = {s=""};
    No = {s= "aǤ"  } ;
    Pred pol item st =
      let
      -- We extract the subject (item.s is a string, e.g., "inGida Ǥoneleegiwa" for "this man")
        subject : Str = item.s;

        -- We check if the quality is verbal or nominal
        -- If st.verb is not empty, we use the verbal form
        predForm : Str = case st.verb.s ! P3 ! item.n ! PNone ! Sg ! PNone ! Sg of {
          "" =>
        -- If the verbal form is empty, we use the nominal form
            let
            -- We define the parameters for the nominal form
            nounParams : NounParamSet = customNounParamSet;
            -- We obtain the quality form as a noun
            qualForm : Str = getNounForm st.noun nounParams item.n
            in
            -- We return the quality form as a string
            qualForm;
            verbForm =>
          -- If the verbal form is not empty, we use the verb
              verbForm
        };

      -- We apply the polarity
      -- pol.s is already a string (e.g., "" for Pos, "aǤ" for Neg)
      -- In Kadiwéu, negation typically appears as a prefix in the predicate
      finalPred : Str = pol.s ++ predForm;

      -- We construct the final sentence: [Determiner + Noun + Quality]
      sentence : Str = subject ++ finalPred
      in
      -- We return a Comment, which is a record {s : Str}
      {s = sentence};
    
     --
    --let f: FORM = item.s ! st.c ! st.l ;  pred: Str = YrlCopula item.n item.p st.l st.c (st.s ! item.n ! item.p) st.v st.nc pol.s in
     --{s= f ! Nom ! NCI ++ pred } ;


  --Lin of open word categories: --
    -- Nouns (SimpleKind): !REVIEW: I need some help to check the lexemes for nouns in this
  Man  = mkNoun "Ǥoneleegiwa" Masc; -- This is a simple realization  of the noun, as it is possible to realize some other morphemes. I will have to discuss with Leonel how to deal with these cases.
  Canoe = mkNoun "niwatece" Fem;
  Food = mkNoun ("oligi"|"weenigi")Fem; --!check
  Bird = mkNoun "ilaaǤagi" Masc;
  Blood = mkNoun"awodi" Masc; --check: in the dict its Neutral...
  Branch = mkNoun "libiwe" Masc;-- tronco="libatadi"|"nibatadi"; !!check
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
  Son_Of_Man, Son_Of_Woman = mkNoun "nionigi" Masc; --ni-oonigi (fala da mulher)??
  Stone = mkNoun "wetiǤa" Fem;
  Street = mkNoun "ladigodi" Fem ;
  Toucan = mkNoun "Gatodi" Masc;
  Tree = mkNoun "niale" Fem;  
  Water = mkNoun "ninyoǤodi" Masc;
  Wife = mkNoun "nodawa" Fem;
  Woman = mkNoun "iwaalo" Fem;  
  Word = mkNoun "notaǤa" Masc; --check gender
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
  
  
  This kind = mkNounPhrase Present Standing Close kind Sg  customNounParamSet;
  That kind = mkNounPhrase Present Standing Far kind Sg  customNounParamSet;
  These kind = mkNounPhrase Present Standing Close kind Pl  customNounParamSet;
  Those kind = mkNounPhrase Present Standing Far kind Pl  customNounParamSet;
  
  --demonst Number Presence Position NounParamSet Noun = demonstDet Number Presence Position NounParamSet Noun; 
  
  --This = demonstDet Sg  "ka" "ida" "ini" "idi" "ina" "ijo" "ada" "ani" "adi" "ana" "ajo" "idiwa"  Present Coming {alienability = Alnbl;psorPers = PsorP3;psorNum = PsorPl;sufClassifier = AnimPlant;number = Sg}  ; --I have to check this function. I might have to create functions specific to KBC because other parameters other the distance, gender and number apply
 

}
