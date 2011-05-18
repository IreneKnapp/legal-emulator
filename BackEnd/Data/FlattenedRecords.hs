module Data.FlattenedRecords
  (defineFlattenedRecord)
  where

import Data.Char
import Language.Haskell.TH

import Debug.Trace


defineFlattenedRecord :: Name -> Q [Dec]
defineFlattenedRecord recordTypeConstructorName = do
  let monadicRecordName =
        mkName $ "Monadic" ++ nameBase recordTypeConstructorName
  (recordDataConstructorName,
   flattenedFieldNames,
   flattenedFieldTypes,
   accessorDeclarations)
    <- getFlattenedRecordFields monadicRecordName recordTypeConstructorName
  let monadContentTypeName = mkName "a"
      continuationResultTypeName = mkName "r"
      continuationName = mkName "continuation"
      recordName = mkName "record"
      valueName = mkName "value"
      aName = mkName "a"
      bName = mkName "b"
      bPrimeName = mkName "b'"
      intermediateName = mkName "intermediate"
      resultName = mkName "result"
      actionName = mkName "action"
      flattenedFieldTemporaryNames =
        map (\(_, fieldIndex) -> mkName $ "field" ++ show fieldIndex)
            $ zip flattenedFieldTypes [0..]
      flattenedFieldAlternateTemporaryNames =
        map (\(_, fieldIndex) -> mkName $ "alternateField" ++ show fieldIndex)
            $ zip flattenedFieldTypes [0..]
      newtypeDeclaration =
        let monadInnerType =
              ForallT
               [PlainTV continuationResultTypeName]
               []
               $ functionType
                  $ [functionType $ [VarT monadContentTypeName]
                                    ++ flattenedFieldTypes
                                    ++ [VarT continuationResultTypeName]]
                    ++ flattenedFieldTypes
                    ++ [VarT continuationResultTypeName]
        in NewtypeD []
                    monadicRecordName
                    [PlainTV monadContentTypeName]
                    (NormalC monadicRecordName [(NotStrict, monadInnerType)])
                    []
      instanceDeclaration =
        let returnLambdaExpression =
              LamE ([VarP continuationName]
                    ++ map VarP flattenedFieldTemporaryNames)
                   $ functionCallExpression
                      $ [VarE continuationName,
                         VarE valueName]
                        ++ map VarE flattenedFieldTemporaryNames
            returnDeclaration =
              FunD (mkName "return")
                   [Clause [VarP valueName]
                           (NormalB
                             $ AppE (ConE monadicRecordName)
                                    returnLambdaExpression)
                           []]
            bindInnerLambdaExpression =
              LamE ([VarP intermediateName]
                    ++ map VarP flattenedFieldAlternateTemporaryNames)
                   $ LetE [ValD (ConP monadicRecordName
                                      [VarP bPrimeName])
                                (NormalB
                                  $ functionCallExpression
                                     [VarE bName,
                                      VarE intermediateName])
                                []]
                          $ functionCallExpression
                             $ [VarE bPrimeName,
                                VarE continuationName]
                               ++ map VarE
                                      flattenedFieldAlternateTemporaryNames
            bindOuterLambdaExpression =
              LamE ([VarP continuationName]
                    ++ map VarP flattenedFieldTemporaryNames)
                   $ functionCallExpression
                      $ [VarE aName,
                         bindInnerLambdaExpression]
                        ++ map VarE flattenedFieldTemporaryNames
            bindDeclaration =
             FunD
              (mkName ">>=")
              [Clause [ConP monadicRecordName
                            [VarP monadContentTypeName],
                       VarP bName]
                      (NormalB
                        $ AppE (ConE monadicRecordName)
                               bindOuterLambdaExpression)
                      []]
        in InstanceD []
                     (AppT (ConT $ mkName "Monad")
                           (ConT monadicRecordName))
                     [returnDeclaration,
                      bindDeclaration]
      runName = mkName $ "run" ++ nameBase monadicRecordName
      runSignatureDeclaration =
        SigD runName
             $ ForallT [PlainTV monadContentTypeName]
                       []
                       $ functionType
                          [AppT (ConT monadicRecordName)
                                (VarT aName),
                           ConT recordTypeConstructorName,
                           tupleType [VarT aName,
                                      ConT recordTypeConstructorName]]
      runDeclaration =
        let lambdaExpression =
              LamE ([VarP resultName]
                    ++ map VarP flattenedFieldTemporaryNames)
                   $ tupleExpression
                      $ [VarE resultName,
                         RecConE recordDataConstructorName
                                 $ map (\(fieldName, fieldTemporaryName) ->
                                           (fieldName,
                                            VarE fieldTemporaryName))
                                       $ zip flattenedFieldNames
                                             flattenedFieldTemporaryNames]
        in FunD runName
                [Clause [ConP monadicRecordName
                              [VarP actionName],
                         VarP recordName]
                        (NormalB
                          $ functionCallExpression
                             $ [VarE actionName,
                                lambdaExpression]
                               ++ map (\fieldName ->
                                         AppE (VarE fieldName)
                                              (VarE recordName))
                                      flattenedFieldNames)
                        []]
  return $ [newtypeDeclaration,
            instanceDeclaration,
            runSignatureDeclaration,
            runDeclaration]
           ++ accessorDeclarations


getFlattenedRecordFields :: Name -> Name -> Q (Name, [Name], [Type], [Dec])
getFlattenedRecordFields monadicRecordName recordTypeConstructorName = do
  maybeRecordInformation <- getRecordInformation recordTypeConstructorName
  case maybeRecordInformation of
    Nothing -> error $ "Not defined appropriately: "
                       ++ show recordTypeConstructorName
    Just (recordDataConstructorName, immediateFields) -> do
      let flattenedFieldNames = map (\(fieldName, _, _) -> fieldName)
                                    immediateFields
          flattenedFieldTypes = map (\(_, fieldType, _) -> fieldType)
                                    immediateFields
          flattenedFieldTemporaryNames =
            map (\(_, _, fieldIndex) -> mkName $ "field" ++ show fieldIndex)
                immediateFields
          overallGetterName =
            mkName $ "get" ++ (nameBase recordTypeConstructorName)
          overallSetterName =
            mkName $ "put" ++ (nameBase recordTypeConstructorName)
          overallType = ConT recordTypeConstructorName
          overallGetterSignatureDeclaration =
            computeGetterSignatureDeclaration monadicRecordName
                                              overallGetterName
                                              overallType
          overallSetterSignatureDeclaration =
            computeSetterSignatureDeclaration monadicRecordName
                                              overallSetterName
                                              overallType
          allFieldIndices =
            map (\(_, _, fieldIndex) -> fieldIndex)
                immediateFields
          overallGetterDeclaration =
            computeMultipleFieldGetterDeclaration monadicRecordName
                                                  overallGetterName
                                                  recordDataConstructorName
                                                  allFieldIndices
                                                  flattenedFieldNames
                                                  flattenedFieldTemporaryNames
          overallSetterDeclaration =
            computeMultipleFieldSetterDeclaration monadicRecordName
                                                  overallSetterName
                                                  allFieldIndices
                                                  flattenedFieldNames
                                                  flattenedFieldTemporaryNames
      accessorDeclarationLists <-
        mapM (\(fieldName, fieldType, fieldIndex) -> do
                 let fixedFieldName =
                       removePrefix (nameBase recordTypeConstructorName)
                                    (fixCase $ nameBase fieldName)
                     getterName = mkName $ "get" ++ fixedFieldName
                     setterName = mkName $ "put" ++ fixedFieldName
                     getterSignatureDeclaration =
                       computeGetterSignatureDeclaration monadicRecordName
                                                         getterName
                                                         fieldType
                     setterSignatureDeclaration =
                       computeSetterSignatureDeclaration monadicRecordName
                                                         setterName
                                                         fieldType
                     getterDeclaration =
                       computeSingleFieldGetterDeclaration
                        monadicRecordName
                        getterName
                        fieldIndex
                        flattenedFieldTemporaryNames
                     setterDeclaration =
                       computeSingleFieldSetterDeclaration
                        monadicRecordName
                        setterName
                        fieldIndex
                        flattenedFieldTemporaryNames
                 return [getterSignatureDeclaration,
                         getterDeclaration,
                         setterSignatureDeclaration,
                         setterDeclaration])
             immediateFields
      let accessorDeclarations =
            [overallGetterSignatureDeclaration,
             overallGetterDeclaration,
             overallSetterSignatureDeclaration,
             overallSetterDeclaration]
            ++ concat accessorDeclarationLists
      return (recordDataConstructorName,
              flattenedFieldNames,
              flattenedFieldTypes,
              accessorDeclarations)


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


computeSingleFieldGetterDeclaration :: Name -> Name -> Int -> [Name] -> Dec
computeSingleFieldGetterDeclaration
    monadicRecordName getterName fieldIndex flattenedFieldTemporaryNames =
  let continuationName = mkName "continuation"
      lambdaExpression =
        LamE ([VarP continuationName]
              ++ map VarP flattenedFieldTemporaryNames)
             $ functionCallExpression $ [VarE continuationName,
                                         VarE $ flattenedFieldTemporaryNames
                                                !! fieldIndex]
                                        ++ map VarE
                                               flattenedFieldTemporaryNames
  in FunD getterName
          [Clause []
                  (NormalB
                    $ AppE (ConE monadicRecordName)
                           lambdaExpression)
                  []]


computeSingleFieldSetterDeclaration :: Name -> Name -> Int -> [Name] -> Dec
computeSingleFieldSetterDeclaration
    monadicRecordName setterName fieldIndex flattenedFieldTemporaryNames =
  let continuationName = mkName "continuation"
      valueName = mkName "value"
      replacedNamesForPattern =
        (map VarP $ take fieldIndex flattenedFieldTemporaryNames)
        ++ [WildP]
        ++ (map VarP $ drop (fieldIndex + 1) flattenedFieldTemporaryNames)
      replacedNamesForExpression =
        map VarE
            $ take fieldIndex flattenedFieldTemporaryNames
              ++ [valueName]
              ++ drop (fieldIndex + 1) flattenedFieldTemporaryNames
      lambdaExpression =
        LamE ([VarP continuationName]
              ++ replacedNamesForPattern)
             $ functionCallExpression $ [VarE continuationName,
                                         nullExpression]
                                        ++ replacedNamesForExpression
  in FunD setterName
          [Clause [VarP valueName]
                  (NormalB
                    $ functionCallExpression [VarE $ mkName "deepseq",
                                              VarE valueName,
                                              AppE (ConE monadicRecordName)
                                                   lambdaExpression])
                  []]


computeMultipleFieldGetterDeclaration
    :: Name -> Name -> Name -> [Int] -> [Name] -> [Name] -> Dec
computeMultipleFieldGetterDeclaration
    monadicRecordName
    getterName
    recordDataConstructorName
    relevantFieldIndices
    flattenedFieldNames
    flattenedFieldTemporaryNames =
  let continuationName = mkName "continuation"
      relevantFieldNames =
        map (\fieldIndex -> flattenedFieldNames !! fieldIndex)
            relevantFieldIndices
      relevantFieldTemporaryNames =
        map (\fieldIndex -> flattenedFieldTemporaryNames !! fieldIndex)
            relevantFieldIndices
      lambdaExpression =
        LamE ([VarP continuationName]
              ++ map VarP flattenedFieldTemporaryNames)
             $ functionCallExpression
                $ [VarE continuationName,
                   RecConE recordDataConstructorName
                           $ map (\(fieldName, fieldTemporaryName) ->
                                      (fieldName,
                                       VarE fieldTemporaryName))
                                 $ zip relevantFieldNames
                                       relevantFieldTemporaryNames]
                  ++ map VarE flattenedFieldTemporaryNames
  in FunD getterName
          [Clause []
                  (NormalB
                    $ AppE (ConE monadicRecordName)
                           lambdaExpression)
                  []]


computeMultipleFieldSetterDeclaration
    :: Name -> Name -> [Int] -> [Name] -> [Name] -> Dec
computeMultipleFieldSetterDeclaration
    monadicRecordName
    overallSetterName
    relevantFieldIndices
    flattenedFieldNames
    flattenedFieldTemporaryNames =
  let continuationName = mkName "continuation"
      recordName = mkName "record"
      lambdaExpression =
        LamE ([VarP continuationName]
              ++ (map (\(fieldTemporaryName, fieldIndex) ->
                          if elem fieldIndex relevantFieldIndices
                            then WildP
                            else VarP fieldTemporaryName)
                      $ zip flattenedFieldTemporaryNames
                            [0..]))
             $ functionCallExpression
                $ [VarE continuationName,
                   nullExpression]
                  ++ (map (\(fieldName, fieldTemporaryName, fieldIndex) ->
                              if elem fieldIndex relevantFieldIndices
                                then AppE (VarE fieldName)
                                          (VarE recordName)
                                else VarE fieldTemporaryName)
                          $ zip3 flattenedFieldNames
                                 flattenedFieldTemporaryNames
                                 [0..])
  in FunD overallSetterName
          [Clause [VarP recordName]
                  (NormalB
                    $ functionCallExpression [VarE $ mkName "deepseq",
                                              VarE recordName,
                                              AppE (ConE monadicRecordName)
                                                   lambdaExpression])
                  []]


getRecordInformation :: Name -> Q (Maybe (Name, [(Name, Type, Int)]))
getRecordInformation recordTypeConstructorName = do
  recordInfo <- reify recordTypeConstructorName
  case recordInfo of
    TyConI (DataD [] _ [] [RecC recordDataConstructorName fields] _) -> do
      return $ Just (recordDataConstructorName,
                     map (\((fieldName, _, fieldType), fieldIndex) ->
                              (fieldName, fieldType, fieldIndex))
                         $ zip fields [0..])
    _ -> return Nothing


fixCase :: String -> String
fixCase accessorName =
  (toUpper $ head accessorName) : (tail accessorName)


removePrefix :: String -> String -> String
removePrefix prefix string =
  if (length prefix < length string)
     && (prefix == take (length prefix) string)
    then drop (length prefix) string
    else error "Missing prefix."


functionType :: [Type] -> Type
functionType types = foldr1 (AppT . AppT ArrowT) types


functionCallExpression :: [Exp] -> Exp
functionCallExpression expressions = foldl1 AppE expressions


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
