module Data.FlattenedRecords
  (defineFlattenedRecord)
  where

import Language.Haskell.TH


defineFlattenedRecord :: Name -> Q [Dec]
defineFlattenedRecord recordName = do
  let monadicRecordName = mkName $ "Monadic" ++ nameBase recordName
      monadContentTypeName = mkName "a"
      continuationResultTypeName = mkName "r"
      continuationName = mkName "continuation"
      stateName = mkName "state"
      statePrimeName = mkName "state'"
      valueName = mkName "value"
      aName = mkName "a"
      bName = mkName "b"
      bPrimeName = mkName "b'"
      intermediateName = mkName "intermediate"
      resultName = mkName "result"
      actionName = mkName "action"
      newtypeDeclaration =
        let monadInnerType =
              ForallT [PlainTV continuationResultTypeName]
                      []
                      $ functionType
                         [functionType [VarT monadContentTypeName,
                                        ConT recordName,
                                        VarT continuationResultTypeName],
                          ConT recordName,
                          VarT continuationResultTypeName]
        in NewtypeD []
                    monadicRecordName
                    [PlainTV monadContentTypeName]
                    (NormalC monadicRecordName [(NotStrict, monadInnerType)])
                    []
      instanceDeclaration =
        let returnLambdaExpression =
              LamE [VarP continuationName,
                    VarP stateName]
                   $ functionCallExpression [VarE continuationName,
                                             VarE valueName,
                                             VarE stateName]
            returnDeclaration =
              FunD (mkName "return")
                   [Clause [VarP valueName]
                           (NormalB
                             $ AppE (ConE monadicRecordName)
                                    returnLambdaExpression)
                           []]
            bindInnerLambdaExpression =
              LamE [VarP intermediateName,
                    VarP statePrimeName]
                   $ LetE [ValD (ConP monadicRecordName
                                      [VarP bPrimeName])
                                (NormalB
                                  $ functionCallExpression
                                     [VarE bName,
                                      VarE intermediateName])
                                []]
                          $ functionCallExpression [VarE bPrimeName,
                                                    VarE continuationName,
                                                    VarE statePrimeName]
            bindOuterLambdaExpression =
              LamE [VarP continuationName,
                    VarP stateName]
                   $ functionCallExpression [VarE aName,
                                             bindInnerLambdaExpression,
                                             VarE stateName]
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
                       $ functionType [AppT (ConT monadicRecordName)
                                            (VarT aName),
                                       ConT recordName,
                                       tupleType [VarT aName,
                                                  ConT recordName]]
      runDeclaration =
        let runLambdaExpression =
              LamE [VarP resultName,
                    VarP statePrimeName]
                   $ tupleExpression [VarE resultName,
                                      VarE statePrimeName]
        in FunD runName
                [Clause [ConP monadicRecordName
                              [VarP actionName],
                         VarP stateName]
                        (NormalB
                          $ functionCallExpression [VarE actionName,
                                                    runLambdaExpression,
                                                    VarE stateName])
                        []]
      getName = mkName $ "get" ++ nameBase recordName
      getSignatureDeclaration =
        SigD getName
             $ AppT (ConT monadicRecordName)
                    (ConT recordName)
      getDeclaration =
        let getLambdaExpression =
              LamE [VarP continuationName,
                    VarP stateName]
                   $ functionCallExpression [VarE continuationName,
                                             VarE stateName,
                                             VarE stateName]
        in FunD getName
                [Clause []
                        (NormalB
                          $ AppE (ConE monadicRecordName)
                                 getLambdaExpression)
                        []]
      putName = mkName $ "put" ++ nameBase recordName
      putSignatureDeclaration =
        SigD putName
             $ functionType [ConT recordName,
                             AppT (ConT monadicRecordName)
                                  (TupleT 0)]
      putDeclaration =
        let putLambdaExpression =
              LamE [VarP continuationName,
                    VarP stateName]
                   $ functionCallExpression [VarE continuationName,
                                             nullExpression,
                                             VarE statePrimeName]
        in FunD putName
                [Clause [VarP statePrimeName]
                        (NormalB
                          $ functionCallExpression
                             [VarE $ mkName "deepseq",
                              VarE statePrimeName,
                              AppE (ConE monadicRecordName)
                                   putLambdaExpression])
                        []]
  return [newtypeDeclaration,
          instanceDeclaration,
          runSignatureDeclaration,
          runDeclaration,
          getSignatureDeclaration,
          getDeclaration,
          putSignatureDeclaration,
          putDeclaration]


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


nullExpression :: Exp
nullExpression = TupE []
