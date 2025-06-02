concrete GraKbc_minimal of Lex = open Oper, OperKbc_otimizado_final_completo, Prelude in {
  lincat
    Comment = {s : Str};
    Polarity = {s : Str};
    Kind = KIND_KBC;
    SimpleKind = SIMPLEKIND_KBC;
    State = STATE_KBC;
    Quality = QUAL_KBC;
     Property = QUAL_KBC;
    Location = QUAL_KBC;
     Item, NonDeitic = ITEM_KBC;
    Action = Verb_compound;
  lin
    Yes = {s = ""};
    No = {s = "aǤ"};
    
    -- Substantivos básicos apenas para teste
    Man =  mkNoun "Ǥoneleegiwa" Masc ;
 
    
    -- Função simples para criar tipos Kind a partir de SimpleKind
  
}
