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
  let continuationName = mkName "continuation"
      recordName = mkName "record"
      valueName = mkName "value"
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
          overallGetterDeclaration =
            let lambdaExpression =
                  LamE ([VarP continuationName]
                        ++ map VarP flattenedFieldTemporaryNames)
                       $ functionCallExpression
                          $ [VarE continuationName,
                             RecConE
                              recordDataConstructorName
                              $ map (\(fieldName, fieldTemporaryName) ->
                                        (fieldName,
                                         VarE fieldTemporaryName))
                                    $ zip flattenedFieldNames
                                          flattenedFieldTemporaryNames]
                            ++ map VarE flattenedFieldTemporaryNames
            in FunD overallGetterName
                    [Clause []
                            (NormalB
                              $ AppE (ConE monadicRecordName)
                                     lambdaExpression)
                            []]
          overallSetterSignatureDeclaration =
            computeSetterSignatureDeclaration monadicRecordName
                                              overallSetterName
                                              overallType
          overallSetterDeclaration =
            let lambdaExpression =
                  LamE ([VarP continuationName]
                        ++ map VarP flattenedFieldTemporaryNames)
                       $ functionCallExpression
                          $ [VarE continuationName,
                             nullExpression]
                            ++ map (\fieldName ->
                                      AppE (VarE fieldName)
                                           (VarE recordName))
                                   flattenedFieldNames
            in FunD overallSetterName
                    [Clause [VarP recordName]
                            (NormalB
                              $ functionCallExpression
                                 [VarE $ mkName "deepseq",
                                  VarE recordName,
                                  AppE (ConE monadicRecordName)
                                       lambdaExpression])
                            []]
      accessorDeclarationLists <-
        mapM (\(fieldName, fieldType, fieldIndex) -> do
                 let fixedFieldName =
                       removePrefix (nameBase recordTypeConstructorName)
                                    (fixCase $ nameBase fieldName)
                     getterName = mkName $ "get" ++ fixedFieldName
                     getterSignatureDeclaration =
                       computeGetterSignatureDeclaration monadicRecordName
                                                         getterName
                                                         fieldType
                     getterDeclaration =
                       let lambdaExpression =
                             LamE ([VarP continuationName]
                                   ++ map VarP flattenedFieldTemporaryNames)
                                  $ functionCallExpression
                                     $ [VarE continuationName,
                                        VarE
                                         $ flattenedFieldTemporaryNames
                                            !! fieldIndex]
                                       ++ map VarE flattenedFieldTemporaryNames
                       in FunD getterName
                               [Clause []
                                       (NormalB
                                         $ AppE (ConE monadicRecordName)
                                                lambdaExpression)
                                       []]
                     setterName = mkName $ "put" ++ fixedFieldName
                     setterSignatureDeclaration =
                       computeSetterSignatureDeclaration monadicRecordName
                                                         setterName
                                                         fieldType
                     setterDeclaration =
                       let replacedNames =
                             take fieldIndex
                                  flattenedFieldTemporaryNames
                             ++ [valueName]
                             ++ drop (fieldIndex + 1)
                                     flattenedFieldTemporaryNames
                           lambdaExpression =
                             LamE ([VarP continuationName]
                                   ++ map VarP flattenedFieldTemporaryNames)
                                  $ functionCallExpression
                                     $ [VarE continuationName,
                                        nullExpression]
                                       ++ map VarE replacedNames
                       in FunD setterName
                               [Clause [VarP valueName]
                                       (NormalB
                                         $ functionCallExpression
                                            [VarE $ mkName "deepseq",
                                             VarE valueName,
                                             AppE (ConE monadicRecordName)
                                                  lambdaExpression])
                                       []]
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
