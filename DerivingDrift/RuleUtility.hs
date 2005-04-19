module DerivingDrift.RuleUtility(rules) where
import DerivingDrift.RuleUtils
import List 
import GenUtil

rules :: [RuleDef]
rules = [("Query",queryGen, "Utility", "provide a QueryFoo class with 'is', 'has', 'from', and 'get' routines", Nothing) ]


queryGen :: Data -> Doc
queryGen d@D{name = name} = cls $$ text "" $$ ins where
    cls = text "class" <+> text className <+> typeName <+> cargs <+> text "where" $$ block fs 
    ot a b = a <+> text "::" <+> b
    cargs = if null $ vars d then empty else dargs <+> text "|" <+> typeName <+> text "->" <+> dargs 
    dargs =  hsep (map text $ vars d) 
    className = "Query" ++ name
    typeName = text "_x"
    fs = (map is (body d) )
    is Body{constructor = constructor, types = types} = fn $$ dfn $$ ffn where 
        fnName = text $ "is" ++ constructor
        fromName = "from" ++ constructor  
        fn = ot fnName $  typeName <+> rArrow <+> text "Bool"   
        dfn = fnName <+> x <+> text "=" <+> text "isJust" <+> parens (text fromName <+> x)
        ffn = ot (text fromName) $ text "Monad _m =>" <+> typeName <+> rArrow <+> text "_m" <+> tuple (map prettyType types) 

    ins = text "instance" <+> text className <+> parens (text name <+> dargs) <+> dargs <+> text "where" $$ block fromInsts
    fromInsts = map fi (body d)
    fi Body{constructor = constructor, types = types} = fn $$ dfn where 
        fromName = "from" ++ constructor  
        fn = text fromName <+> pattern constructor types <+> text "=" <+> text "return" <+> tuple (varNames types)
        dfn = text fromName <+> blank <+> equals <+> text "fail" <+> tshow fromName

