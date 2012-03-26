module Data.FlattenedRecords
  (defineFlattenedRecord)
  where

import Data.Char
import Data.Either
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Language.Haskell.TH


data RecordConstructionInformation =
  RecordConstructionInformation Name
                                [(Name, RecordConstructionInformation)]
                                [(Name, Int)]
  deriving (Show)
type RecordInformation =
  ([Name], String, Type, RecordConstructionInformation)
type FieldInformation =
  ([Name], String, Type, Int)


defineFlattenedRecord :: Name -> Q [Dec]
defineFlattenedRecord recordTypeConstructorName = do
  let monadicRecordName =
        mkName $ "Monadic" ++ nameBase recordTypeConstructorName
      monadicRecordPrimeName =
        mkName $ "Monadic" ++ nameBase recordTypeConstructorName ++ "'"
      runPrimeName = mkName $ "run" ++ nameBase monadicRecordName ++ "'"
      runPrimePrimeName = mkName $ "run" ++ nameBase monadicRecordName ++ "''"
      unwrapperExpression =
        InfixE (Just $ VarE runPrimePrimeName)
               (VarE $ mkName ".")
               (Just $ VarE runPrimeName)
      wrapperExpression content =
        AppE (ConE monadicRecordName)
             (AppE (ConE monadicRecordPrimeName)
                   content)
      wrapperPattern content =
        ConP monadicRecordName
             [ConP monadicRecordPrimeName
                   [content]]
  (recordDataConstructorName,
   recordConstruction,
   flattenedFieldTypes,
   accessorDeclarations)
    <- getFlattenedRecordFields monadicRecordName
                                wrapperExpression
                                recordTypeConstructorName
  let monadContentTypeName = mkName "a"
      continuationResultTypeName = mkName "r"
      continuationName = mkName "continuation"
      recordName = mkName "record"
      valueName = mkName "value"
      mName = mkName "m"
      fName = mkName "f"
      kName = mkName "k"
      aName = mkName "a"
      resultName = mkName "result"
      actionName = mkName "action"
      runName = mkName $ "run" ++ nameBase monadicRecordName
      flattenedFieldTemporaryNames =
        map (\(_, fieldIndex) ->
                computeFieldTemporaryName fieldIndex)
            $ zip flattenedFieldTypes [0..]
      flattenedFieldAlternateTemporaryNames =
        map (\(_, fieldIndex) ->
                computeFieldAlternateTemporaryName fieldIndex)
            $ zip flattenedFieldTypes [0..]
      innerNewtypeDeclaration =
        let innerType =
              functionType
               $ [functionType $ [VarT monadContentTypeName]
                                 ++ flattenedFieldTypes
                                 ++ [VarT continuationResultTypeName]]
                 ++ flattenedFieldTypes
                 ++ [VarT continuationResultTypeName]
        in NewtypeD []
                    monadicRecordPrimeName
                    [PlainTV continuationResultTypeName,
                     PlainTV monadContentTypeName]
                    (RecC monadicRecordPrimeName
                          [(runPrimePrimeName, NotStrict, innerType)])
                    []
      outerNewtypeDeclaration =
        let innerType =
              AppT (AppT (ConT monadicRecordPrimeName)
                         (AppT (AppT (TupleT 2)
                                     (VarT monadContentTypeName))
                               (ConT recordTypeConstructorName)))
                   (VarT monadContentTypeName)
        in NewtypeD []
                    monadicRecordName
                    [PlainTV monadContentTypeName]
                    (RecC monadicRecordName
                          [(runPrimeName, NotStrict, innerType)])
                    []
      instanceDeclaration =
        let returnLambdaExpression =
              LamE [VarP continuationName]
                   $ functionCallExpression [VarE continuationName,
                                             VarE valueName]
            returnDeclaration =
              FunD (mkName "return")
                   [Clause [VarP valueName]
                           (NormalB $ wrapperExpression returnLambdaExpression)
                           []]
            bindInnerLambdaExpression =
              LamE [VarP aName]
                   $ strictFunctionCallExpression
                      [unwrapperExpression,
                       strictFunctionCallExpression [VarE fName,
                                                     VarE aName],
                       VarE kName]
            bindOuterLambdaExpression =
              LamE [VarP kName]
                   $ strictFunctionCallExpression [unwrapperExpression,
                                                   VarE mName,
                                                   bindInnerLambdaExpression]
            bindDeclaration =
              FunD
               (mkName ">>=")
               [Clause [VarP mName,
                        VarP fName]
                       (NormalB $ wrapperExpression bindOuterLambdaExpression)
                       []]
        in InstanceD []
                     (AppT (ConT $ mkName "Monad")
                           (ConT monadicRecordName))
                     [returnDeclaration,
                      bindDeclaration]
      runSignatureDeclaration =
        SigD runName
             $ ForallT [PlainTV monadContentTypeName]
                       []
                       $ functionType
                          [AppT (ConT monadicRecordName)
                                (VarT monadContentTypeName),
                           ConT recordTypeConstructorName,
                           tupleType [VarT monadContentTypeName,
                                      ConT recordTypeConstructorName]]
      runDeclaration =
        let lambdaExpression =
              LamE ([VarP resultName]
                    ++ map VarP flattenedFieldTemporaryNames)
                   $ tupleExpression
                      $ [VarE resultName,
                         computeRecordConstructor recordConstruction]
        in FunD runName
                [Clause [wrapperPattern $ VarP actionName,
                         VarP recordName]
                        (NormalB
                          $ functionCallExpression
                             $ [VarE actionName,
                                lambdaExpression]
                               ++ computeRecordDissectors (VarE recordName)
                                                          recordConstruction)
                        []]
  return $ [innerNewtypeDeclaration,
            outerNewtypeDeclaration,
            instanceDeclaration,
            runSignatureDeclaration,
            runDeclaration]
           ++ accessorDeclarations


getFlattenedRecordFields
    :: Name
    -> (Exp -> Exp)
    -> Name
    -> Q (Name, RecordConstructionInformation, [Type], [Dec])
getFlattenedRecordFields monadicRecordName
                         wrapperExpression
                         recordTypeConstructorName = do
  maybeRecordInformation <- do
    maybeOneLevelInformation <-
      getOneLevelRecordInformation recordTypeConstructorName
    case maybeOneLevelInformation of
      Nothing -> return Nothing
      Just (recordDataConstructorName, _) -> do
        getRecursiveRecordInformation recordTypeConstructorName
                                      recordDataConstructorName
        >>= return . (fmap (fixRecursiveRecordInformation
                             $ nameBase recordTypeConstructorName))
  case maybeRecordInformation of
    Nothing -> error $ "Not defined appropriately: "
                       ++ show recordTypeConstructorName
    Just (recordDataConstructorName,
          recordFields,
          immediateFields,
          flatFields) -> do
      let (_, _, _, rootRecordInformation) : _ = recordFields
          temporaryNames = map computeFieldTemporaryName
                               [0 .. length flatFields - 1]
          immediateFieldAccessorDeclarations =
            concat $ map (computeFieldAccessorDeclarations monadicRecordName
                                                           wrapperExpression
                                                           temporaryNames)
                         immediateFields
          recordFieldAccessorDeclarations =
            concat $ map (computeRecordAccessorDeclarations
                           monadicRecordName
                           wrapperExpression
                           temporaryNames)
                         recordFields
          accessorDeclarations =
            concat [immediateFieldAccessorDeclarations,
                    recordFieldAccessorDeclarations]
      return (recordDataConstructorName,
              rootRecordInformation,
              flatFields,
              accessorDeclarations)


computeFieldAccessorDeclarations
    :: Name -> (Exp -> Exp) -> [Name] -> FieldInformation -> [Dec]
computeFieldAccessorDeclarations
    monadicRecordName
    wrapperExpression
    temporaryNames
    (path, accessorBaseName, type', index) =
  let getterName = mkName $ "get" ++ accessorBaseName
      setterName = mkName $ "put" ++ accessorBaseName
      getterSignatureDeclaration =
        computeGetterSignatureDeclaration monadicRecordName getterName type'
      setterSignatureDeclaration =
        computeSetterSignatureDeclaration monadicRecordName setterName type'
      getterDeclaration =
        computeFieldGetterDeclaration wrapperExpression
                                      getterName
                                      index
                                      temporaryNames
      setterDeclaration =
        computeFieldSetterDeclaration wrapperExpression
                                      setterName
                                      index
                                      temporaryNames
  in [getterSignatureDeclaration,
      getterDeclaration,
      setterSignatureDeclaration,
      setterDeclaration]


computeRecordAccessorDeclarations
    :: Name -> (Exp -> Exp) -> [Name] -> RecordInformation -> [Dec]
computeRecordAccessorDeclarations
    monadicRecordName
    wrapperExpression
    temporaryNames
    (path, accessorBaseName, type', construction) =
  let getterName = mkName $ "get" ++ accessorBaseName
      setterName = mkName $ "put" ++ accessorBaseName
      getterSignatureDeclaration =
        computeGetterSignatureDeclaration monadicRecordName getterName type'
      setterSignatureDeclaration =
        computeSetterSignatureDeclaration monadicRecordName setterName type'
      getterDeclaration =
        computeRecordGetterDeclaration wrapperExpression
                                       getterName
                                       construction
                                       temporaryNames
      setterDeclaration =
        computeRecordSetterDeclaration wrapperExpression
                                       setterName
                                       construction
                                       temporaryNames
  in [getterSignatureDeclaration,
      getterDeclaration,
      setterSignatureDeclaration,
      setterDeclaration]


computeGetterSignatureDeclaration :: Name -> Name -> Type -> Dec
computeGetterSignatureDeclaration monadicRecordName getterName typeToGet =
  SigD getterName
       $ AppT (ConT monadicRecordName)
              typeToGet


computeSetterSignatureDeclaration :: Name -> Name -> Type -> Dec
computeSetterSignatureDeclaration monadicRecordName setterName typeToSet =
  SigD setterName
       $ functionType [typeToSet,
                       AppT (ConT monadicRecordName)
                            nullType]


computeFieldGetterDeclaration :: (Exp -> Exp) -> Name -> Int -> [Name] -> Dec
computeFieldGetterDeclaration
    wrapperExpression getterName fieldIndex temporaryNames =
  let continuationName = mkName "continuation"
      lambdaExpression =
        LamE ([VarP continuationName]
              ++ map VarP temporaryNames)
             $ functionCallExpression $ [VarE continuationName,
                                         VarE $ temporaryNames !! fieldIndex]
                                        ++ map VarE temporaryNames
  in FunD getterName
          [Clause []
                  (NormalB $ wrapperExpression lambdaExpression)
                  []]


computeFieldSetterDeclaration :: (Exp -> Exp) -> Name -> Int -> [Name] -> Dec
computeFieldSetterDeclaration
    wrapperExpression setterName fieldIndex temporaryNames =
  let continuationName = mkName "continuation"
      valueName = mkName "value"
      replacedNamesForPattern =
        (map VarP $ take fieldIndex temporaryNames)
        ++ [WildP]
        ++ (map VarP $ drop (fieldIndex + 1) temporaryNames)
      replacedNamesForExpression =
        map VarE
            $ take fieldIndex temporaryNames
              ++ [valueName]
              ++ drop (fieldIndex + 1) temporaryNames
      lambdaExpression =
        LamE ([VarP continuationName]
              ++ replacedNamesForPattern)
             $ functionCallExpression $ [VarE continuationName,
                                         nullExpression]
                                        ++ replacedNamesForExpression
  in FunD setterName
          [Clause [VarP valueName]
                  (NormalB $ wrapperExpression lambdaExpression)
                  []]


computeRecordGetterDeclaration
    :: (Exp -> Exp) -> Name -> RecordConstructionInformation -> [Name] -> Dec
computeRecordGetterDeclaration
    wrapperExpression
    getterName
    construction
    allTemporaryNames =
  let continuationName = mkName "continuation"
      lambdaExpression =
        LamE ([VarP continuationName]
              ++ map VarP allTemporaryNames)
             $ functionCallExpression
                $ [VarE continuationName,
                   computeRecordConstructor construction]
                  ++ map VarE allTemporaryNames
  in FunD getterName
          [Clause []
                  (NormalB $ wrapperExpression lambdaExpression)
                  []]


computeRecordSetterDeclaration
    :: (Exp -> Exp) -> Name -> RecordConstructionInformation -> [Name] -> Dec
computeRecordSetterDeclaration
    wrapperExpression
    setterName
    construction
    allTemporaryNames =
  let continuationName = mkName "continuation"
      recordName = mkName "record"
      relevantFieldIndices = computeRelevantFieldIndices construction
      dissectorMap = Map.fromList
                      $ computeRecordDissectorsAndIndices (VarE recordName)
                                                          construction
      lambdaExpression =
        LamE ([VarP continuationName]
              ++ (map (\(temporaryName, fieldIndex) ->
                          if elem fieldIndex relevantFieldIndices
                            then WildP
                            else VarP temporaryName)
                      $ zip allTemporaryNames
                            [0..]))
             $ functionCallExpression
                $ [VarE continuationName,
                   nullExpression]
                  ++ (map (\(temporaryName, fieldIndex) ->
                              case Map.lookup fieldIndex dissectorMap of
                                Nothing -> VarE temporaryName
                                Just dissector -> dissector)
                          $ zip allTemporaryNames [0..])
  in FunD setterName
          [Clause [VarP recordName]
                  (NormalB $ wrapperExpression lambdaExpression)
                  []]


computeRelevantFieldIndices :: RecordConstructionInformation -> [Int]
computeRelevantFieldIndices
    (RecordConstructionInformation _
                                   recordFields
                                   immediateFields) =
  concat $ concat [map (\(_, fieldConstruction) ->
                           computeRelevantFieldIndices fieldConstruction)
                       recordFields,
                   map (\(_, fieldIndex) -> [fieldIndex])
                           immediateFields]


computeRecordConstructor :: RecordConstructionInformation -> Exp
computeRecordConstructor
    (RecordConstructionInformation dataConstructorName
                                   recordFields
                                   immediateFields) =
  RecConE dataConstructorName
          $ concat [map snd
                        $ sortBy (on compare fst)
                                 $ map (\(fieldName, fieldIndex) ->
                                           (fieldIndex,
                                            (fieldName,
                                             VarE
                                              $ computeFieldTemporaryName
                                                 fieldIndex)))
                                       immediateFields,
                    map (\(fieldName,
                           fieldConstruction) ->
                            (fieldName,
                             computeRecordConstructor fieldConstruction))
                        recordFields]


computeRecordDissectorsAndIndices
    :: Exp -> RecordConstructionInformation -> [(Int, Exp)]
computeRecordDissectorsAndIndices
    recordExpression
    (RecordConstructionInformation _
                                   recordFields
                                   immediateFields) =
  sortBy (on compare fst)
         $ concat
            $ concat [map (\(fieldName, fieldIndex) ->
                              [(fieldIndex,
                                AppE (VarE fieldName)
                                     recordExpression)])
                          immediateFields,
                      map (\(fieldName, fieldConstruction) ->
                              computeRecordDissectorsAndIndices
                               (AppE (VarE fieldName)
                                     recordExpression)
                               fieldConstruction)
                          recordFields]


computeRecordDissectors :: Exp -> RecordConstructionInformation -> [Exp]
computeRecordDissectors recordExpression construction =
  map snd $ computeRecordDissectorsAndIndices recordExpression construction


computeFieldTemporaryName :: Int -> Name
computeFieldTemporaryName fieldIndex =
  mkName $ "field" ++ show fieldIndex


computeFieldAlternateTemporaryName :: Int -> Name
computeFieldAlternateTemporaryName fieldIndex =
  mkName $ "alternateField" ++ show fieldIndex


fixRecursiveRecordInformation
    :: String
    -> (Name, [RecordInformation], [FieldInformation], [Type])
    -> (Name, [RecordInformation], [FieldInformation], [Type])
fixRecursiveRecordInformation
    prefixToRemove
    (dataConstructorName,
     recordInformation,
     fieldInformation,
     flatFieldInformation) =
  let recordInformation' =
        map (\(path, name, type', construction) ->
                (path,
                 if prefixToRemove == titleCase name
                   then titleCase name
                   else removePrefix prefixToRemove $ titleCase name,
                 type',
                 construction))
            recordInformation
      fieldInformation' =
        map (\(path,
               name,
               type',
               index) ->
                (path,
                 if prefixToRemove == titleCase name
                   then titleCase name
                   else removePrefix prefixToRemove $ titleCase name,
                 type',
                 index))
            fieldInformation
  in (dataConstructorName,
      recordInformation',
      fieldInformation',
      flatFieldInformation)


getRecursiveRecordInformation
    :: Name
    -> Name
    -> Q (Maybe (Name,
                 [RecordInformation],
                 [FieldInformation],
                 [Type]))
getRecursiveRecordInformation
    recordTypeConstructorName recordFieldName = do
  maybeRecordInformation <-
    getOneLevelRecordInformation recordTypeConstructorName
  case maybeRecordInformation of
    Nothing -> return Nothing
    Just (recordDataConstructorName, oneLevelFieldInformation) -> do
      subResults <-
        mapM (\(fieldName, fieldType) -> do
                 let treatAsIndividualField =
                       return $ ([],
                                 [([fieldName],
                                   nameBase fieldName,
                                   fieldType,
                                   0)],
                                 [fieldType])
                 case fieldType of
                   ConT fieldTypeName -> do
                     maybeRecursiveInformation <-
                       getRecursiveRecordInformation fieldTypeName
                                                     fieldName
                     case maybeRecursiveInformation of
                       Nothing -> treatAsIndividualField
                       Just (recursiveDataConstructorName,
                             recursiveRecordInformation,
                             recursiveFieldInformation,
                             recursiveFlatFieldInformation) -> do
                         let recordPrefixToRemove =
                               dataConstructorToFieldPrefix
                                $ nameBase recordDataConstructorName
                             recordPrefixToAdd = nameBase recordFieldName
                             fixRecordSubName recordSubName =
                               (titleCase recordPrefixToAdd)
                               ++ removePrefix recordPrefixToRemove
                                               (titleCase recordSubName)
                             fieldSubNamePrefix =
                               dataConstructorToFieldPrefix
                                $ nameBase recursiveDataConstructorName
                             fixFieldSubName fieldSubName =
                               (titleCase $ nameBase fieldName)
                               ++ removePrefix fieldSubNamePrefix
                                               (titleCase fieldSubName)
                         return $ (map (\(fieldSubNames,
                                          fieldSubName,
                                          fieldSubType,
                                          fieldSubConstruction) ->
                                           ([fieldName] ++ fieldSubNames,
                                            fixRecordSubName fieldSubName,
                                            fieldSubType,
                                            fieldSubConstruction))
                                       recursiveRecordInformation,
                                   map (\(fieldSubNames,
                                          fieldSubName,
                                          fieldSubType,
                                          fieldSubIndex) ->
                                           ([fieldName] ++ fieldSubNames,
                                            fixFieldSubName fieldSubName,
                                            fieldSubType,
                                            fieldSubIndex))
                                       recursiveFieldInformation,
                                   recursiveFlatFieldInformation)
                   _ -> treatAsIndividualField)
             oneLevelFieldInformation
      let (recordResults, fieldResults, flatFieldResults, _) =
            foldl' (\(recordResults,
                      fieldResults,
                      flatFieldResults,
                      fieldIndexOffset)
                     (recordInformation,
                      fieldInformation,
                      flatFieldInformation) ->
                       let recordInformationProcessed =
                             map (\(path, name, type', construction) ->
                                     let fixRecordConstruction
                                          (RecordConstructionInformation
                                            dataConstructor
                                            subRecordConstructions
                                            subFieldConstructions) =
                                             RecordConstructionInformation
                                              dataConstructor
                                              (map
                                                (\(fieldSubName,
                                                   subRecordConstruction) ->
                                                    (fieldSubName,
                                                     fixRecordConstruction
                                                      subRecordConstruction))
                                                subRecordConstructions)
                                              (map (\(fieldSubName,
                                                      subFieldConstruction) ->
                                                       (fieldSubName,
                                                         fixFieldConstruction
                                                         subFieldConstruction))
                                                   subFieldConstructions)
                                         fixFieldConstruction index =
                                           fieldIndexOffset + index
                                     in (path,
                                         name,
                                         type',
                                         fixRecordConstruction construction))
                                 recordInformation
                           fieldInformationProcessed =
                             map (\(path, name, type', index) ->
                                     (path,
                                      name,
                                      type',
                                      fieldIndexOffset + index))
                                 fieldInformation
                           fieldCount = length fieldInformation
                           flatFieldInformationProcessed =
                             flatFieldInformation
                       in (recordResults ++ recordInformationProcessed,
                           fieldResults ++ fieldInformationProcessed,
                           flatFieldResults ++ flatFieldInformationProcessed,
                           fieldIndexOffset + fieldCount))
                   ([], [], [], 0)
                   subResults
          thisRecord =
            ([],
             nameBase recordFieldName,
             ConT recordTypeConstructorName,
             RecordConstructionInformation
              recordDataConstructorName
              (mapMaybe
                (\(fieldNames, _, _, fieldConstruction) ->
                    if length fieldNames == 1
                      then Just (head fieldNames,
                                 fieldConstruction)
                      else Nothing)
                recordResults)
              (mapMaybe
                (\(fieldNames, _, _, fieldIndex) ->
                    if length fieldNames == 1
                      then Just (head fieldNames, fieldIndex)
                      else Nothing)
                fieldResults))
      return $ Just (recordDataConstructorName,
                     [thisRecord] ++ recordResults,
                     fieldResults,
                     flatFieldResults)


getOneLevelRecordInformation :: Name -> Q (Maybe (Name, [(Name, Type)]))
getOneLevelRecordInformation recordTypeConstructorName = do
  recordInfo <- reify recordTypeConstructorName
  case recordInfo of
    TyConI (DataD [] _ [] [RecC recordDataConstructorName fields] _) -> do
      return $ Just (recordDataConstructorName,
                     map (\(fieldName, _, fieldType) ->
                             (fieldName, fieldType))
                         fields)
    _ -> return Nothing


titleCase :: String -> String
titleCase "" = ""
titleCase string =
  (toUpper $ head string) : (tail string)


dataConstructorToFieldPrefix :: String -> String
dataConstructorToFieldPrefix dataConstructor =
  let components = unfoldr (\rest ->
                              if rest == ""
                                then Nothing
                                else case elemIndex '_' rest of
                                       Nothing -> Just (rest,
                                                        "")
                                       Just index ->
                                         let (before, rest') =
                                                splitAt index rest
                                         in Just (before,
                                                  drop 1 rest'))
                           dataConstructor
      fixedHeadComponent =
        if all isUpper $ head components
          then titleCase $ map toLower $ head components
          else head components
  in fixedHeadComponent ++ (concat $ tail components)


removePrefix :: String -> String -> String
removePrefix prefix string =
  if (length prefix <= length string)
     && (prefix == take (length prefix) string)
    then drop (length prefix) string
    else error $ "Missing prefix "
                 ++ show prefix
                 ++ " in string "
                 ++ show string


functionType :: [Type] -> Type
functionType types = foldr1 (AppT . AppT ArrowT) types


functionCallExpression :: [Exp] -> Exp
functionCallExpression expressions = foldl1 AppE expressions


strictFunctionCallExpression :: [Exp] -> Exp
strictFunctionCallExpression = functionCallExpression
{-
strictFunctionCallExpression expressions =
  let tempName = mkName "temp"
      seqName = mkName "seq"
  in foldl1 (\a b -> LetE [ValD (VarP tempName)
                                (NormalB b)
                                []]
                          $ AppE (AppE (VarE seqName)
                                       (VarE tempName))
                                 (AppE a
                                       (VarE tempName)))
            expressions-}


tupleType :: [Type] -> Type
tupleType types =
  let n = length types
  in foldl AppT (TupleT n) types


tupleExpression :: [Exp] -> Exp
tupleExpression expressions = TupE expressions


nullType :: Type
nullType = TupleT 0


nullExpression :: Exp
nullExpression = TupE []
