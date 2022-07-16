

module Output(llvmgenerateoutput,getStringOutput)
where



import DataStructures
import Prettyprinter
import qualified Data.Map as Map
import Data.Functor.Identity
import Control.Monad.State
import Data.Maybe (fromMaybe)



type Names = Map.Map String Int

genuniqueName :: String  -> State Names String
genuniqueName varname =do
    namemap <- get
    case Map.lookup varname namemap of
        Nothing -> do
            put (Map.insert varname 0 namemap)
            return (varname++"0")
        Just ix ->do
                put (Map.insert varname (ix+1) namemap)
                return (varname ++ show (ix+1))

llvmgenerateoutput MkTrue = do
       allnames <- get
       newname <- genuniqueName "tmp"
       return ( pretty "\t%"  <> pretty  newname <+> pretty  "=  fadd double 0.0 , 1.0 \n")
llvmgenerateoutput MkFalse =do
       allnames <- get
       newname <- genuniqueName "tmp"
       return (pretty "\t%"  <> pretty  newname <+> pretty  "=  fadd double 0.0 , 0.0 \n")
llvmgenerateoutput (MkInt theint)= do
       allnames <- get
       newname <- genuniqueName "tmp"
       return ( pretty "\t%"  <> pretty  newname <+> pretty  "=  fadd double 0.0 , " <+> pretty theint <>pretty ".0" <+> pretty "\n")
llvmgenerateoutput (MkVarE varstr)=do
        allnames <- get
        newname <- genuniqueName "tmp"
        return (pretty "\t%"<> pretty  newname <+> pretty  "=  fadd double 0.0 , %" <> pretty varstr <> pretty (fromMaybe 0 (Map.lookup  varstr allnames)) <+> pretty "\n"   )

llvmgenerateoutput (MkPlus expr1 expr2)= do
        resoutput<-llvmgenerateoutput expr1
        allnames <- get
        resoutput2<-llvmgenerateoutput expr2
        allnames2 <-get
        newname <- genuniqueName "tmp"
        return (resoutput <+> resoutput2 <+> pretty "\t%" <> pretty newname <+> pretty "=" <+> pretty "fadd double %tmp" <> pretty (fromMaybe 0 (getval allnames)) <+> pretty ", %tmp" <> pretty (fromMaybe 0 (getval allnames2)) <> pretty "\n")
        where
            getval =  Map.lookup "tmp"
llvmgenerateoutput (MkApplyFunc funcstr (MkListArgs exprs)) =do
        (results,vars)<- llvmgenerateoutputListargs (MkListArgs exprs) []
        newname <- genuniqueName "tmp"
        allnames <- get
        return (results<+>pretty "\t%tmp" <> pretty (fromMaybe 0 (getval allnames))<+>pretty "=call double @" <>pretty funcstr<>pretty "(" <> appendListOfStrings vars <+> pretty ")\n")
             where
                getval =  Map.lookup "tmp"
appendListOfStrings :: Pretty a => [a] -> Doc ann
appendListOfStrings  (x:[]) = pretty "double %" <> pretty x 
appendListOfStrings  (x:xs) = pretty "double %" <> pretty x <+> pretty "," <+> appendListOfStrings xs
appendListOfStrings [] = pretty ""


llvmgenerateoutputListargs :: CArgs -> [String] -> StateT Names Identity (Doc ann, [String])
llvmgenerateoutputListargs (MkListArgs []) varnames = do
        return ((pretty ""),varnames)
llvmgenerateoutputListargs (MkListArgs (x:xs)) varnames= do
        res<-llvmgenerateoutput x
        newname <- genuniqueName "tmpvar"
        allnames <- get
        (res2,rest)<- llvmgenerateoutputListargs (MkListArgs xs) (varnames++[newname])
        return ((res<+> (pretty "\t%"<> pretty  newname <+> pretty  "=  fadd double 0.0 , %" <> pretty "tmp" <> pretty (fromMaybe 0 (Map.lookup  "tmp" allnames)) <+> pretty "\n"   ) <+> res2),rest)
llvmgeneratedeclOrStatement ((Left dec):xs)= do
     res<-llvmgenerateDeclOutput dec
     res2<-llvmgeneratedeclOrStatement xs
     return (res <+> res2)
llvmgeneratedeclOrStatement ((Right st):xs) = do
     res<-llvmgenerateStatement st
     res2<-llvmgeneratedeclOrStatement xs
     return (res <+> res2)
llvmgeneratedeclOrStatement [] = return (pretty "")


llvmgenerateStatement :: CStatement -> StateT Names Identity (Doc ann)
llvmgenerateStatement (ExprStatement expr) = do
    llvmgenerateoutput expr
llvmgenerateStatement (ReturnStatement expr)= do
    strsofar<-llvmgenerateoutput expr
    allnames <- get
    return (strsofar <+> pretty "\t ret double %tmp"<> pretty (fromMaybe 0 (getval allnames)))
        where
            getval =  Map.lookup "tmp"
llvmgenerateStatement (Compoundstatement lst)=do
    llvmgeneratedeclOrStatement lst


llvmgeneratefunctionpart [MkVarInfo str typeofvar] = do 
    newname<-genuniqueName str
    return (pretty "double %" <> pretty newname)
llvmgeneratefunctionpart  ((MkVarInfo str typeofvar):xs) =do 
    newname<-genuniqueName str
    otherres<-llvmgeneratefunctionpart xs
    return (pretty "double %" <> pretty newname <+> pretty "," <> otherres)
llvmgeneratefunctionpart [] = do 
    return (pretty "")
llvmgenerateDeclOutput (MkFunction (MkVarInfo str ty) vars cstatment) = do
    resofvars<-llvmgeneratefunctionpart vars
    res <-llvmgenerateStatement cstatment
    return (pretty "define double @"<> pretty str<+> pretty "(" <+>resofvars  <+> pretty "){\n" <> pretty "entry:\n" <+> res <+> pretty "\n }")
llvmgenerateDeclOutput (MkExternFunc (MkVarInfo str typeofvar ) vars) = do
    resofvars<-llvmgeneratefunctionpart vars
    return (pretty "declare double @"<> pretty str<+> pretty "(" <+>resofvars  <+> pretty ")")

llvmgenerateDeclOutput (MkVarD (MkVarInfo str ty) expr ) = do
    res1<-llvmgenerateoutput expr
    newname <- genuniqueName str
    allnames <- get
    return (res1<+>pretty "\t%"<> pretty  newname <+> pretty  "=  fadd double 0.0 , %tmp"<> pretty (fromMaybe 0 (getval allnames)) <+> pretty "\n"   )
        where
            getval =  Map.lookup "tmp"
llvmgenerateProgramOutput (MkProgram (x:xs)) = do
    allnames <- get
    res<- llvmgenerateDeclOutput x
    res2<-llvmgenerateProgramOutput (MkProgram xs)
    return (res <+>res2 )
llvmgenerateProgramOutput (MkProgram []) = do
    return (pretty "")

getStringOutput expr = pretty "; ModuleID = 'my cool jit' \n" <+> evalState (llvmgenerateProgramOutput expr) Map.empty