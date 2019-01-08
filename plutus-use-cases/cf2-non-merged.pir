(program
  (let
    (nonrec
    )
    (datatypebind
      (datatype
        (tyvardecl CampaignAction (type))
        
        CampaignAction_match
        (vardecl Collect CampaignAction) (vardecl Refund CampaignAction)
      )
    )
    (let
      (nonrec
      )
      (datatypebind
        (datatype
          (tyvardecl DataScriptHash (type))
          
          DataScriptHash_match
          (vardecl
            DataScriptHash (fun [(con bytestring) (con 32)] DataScriptHash)
          )
        )
      )
      (let
        (nonrec
        )
        (datatypebind
          (datatype
            (tyvardecl RedeemerHash (type))
            
            RedeemerHash_match
            (vardecl RedeemerHash (fun [(con bytestring) (con 32)] RedeemerHash)
            )
          )
        )
        (let
          (nonrec
          )
          (datatypebind
            (datatype
              (tyvardecl TxHash (type))
              
              TxHash_match
              (vardecl TxHash (fun [(con bytestring) (con 32)] TxHash))
            )
          )
          (let
            (nonrec
            )
            (datatypebind
              (datatype
                (tyvardecl ValidatorHash (type))
                
                ValidatorHash_match
                (vardecl
                  ValidatorHash (fun [(con bytestring) (con 32)] ValidatorHash)
                )
              )
            )
            (let
              (nonrec
              )
              (datatypebind
                (datatype
                  (tyvardecl
                    Tuple3 (fun (type) (fun (type) (fun (type) (type))))
                  )
                  (tyvardecl a (type)) (tyvardecl b (type)) (tyvardecl c (type))
                  Tuple3_match
                  (vardecl Tuple3 (fun a (fun b (fun c [[[Tuple3 a] b] c]))))
                )
              )
              (let
                (nonrec
                )
                (datatypebind
                  (datatype
                    (tyvardecl Tuple2 (fun (type) (fun (type) (type))))
                    (tyvardecl a (type)) (tyvardecl b (type))
                    Tuple2_match
                    (vardecl Tuple2 (fun a (fun b [[Tuple2 a] b])))
                  )
                )
                (let
                  (nonrec
                  )
                  (datatypebind
                    (datatype
                      (tyvardecl Unit (type))  Unit_match (vardecl Unit Unit)
                    )
                  )
                  (let
                    (nonrec
                    )
                    (termbind
                      (vardecl error (all a (type) (fun Unit a)))
                      (abs e (type) (lam thunk Unit (error e)))
                    )
                    (let
                      (nonrec
                      )
                      (datatypebind
                        (datatype
                          (tyvardecl Maybe (fun (type) (type)))
                          (tyvardecl a (type))
                          Maybe_match
                          (vardecl Just (fun a [Maybe a]))
                          (vardecl Nothing [Maybe a])
                        )
                      )
                      (let
                        (rec
                        )
                        (datatypebind
                          (datatype
                            (tyvardecl List (fun (type) (type)))
                            (tyvardecl a (type))
                            Nil_match
                            (vardecl Nil [List a])
                            (vardecl Cons (fun a (fun [List a] [List a])))
                          )
                        )
                        (let
                          (nonrec
                          )
                          (datatypebind
                            (datatype
                              (tyvardecl Height (type))
                              
                              Height_match
                              (vardecl
                                Height (fun [(con integer) (con 8)] Height)
                              )
                            )
                          )
                          (let
                            (nonrec
                            )
                            (datatypebind
                              (datatype
                                (tyvardecl PubKey (type))
                                
                                PubKey_match
                                (vardecl
                                  PubKey (fun [(con integer) (con 8)] PubKey)
                                )
                              )
                            )
                            (let
                              (nonrec
                              )
                              (datatypebind
                                (datatype
                                  (tyvardecl PendingTxOutType (type))
                                  
                                  PendingTxOutType_match
                                  (vardecl DataTxOut PendingTxOutType)
                                  (vardecl
                                    PubKeyTxOut (fun PubKey PendingTxOutType)
                                  )
                                )
                              )
                              (let
                                (nonrec
                                )
                                (datatypebind
                                  (datatype
                                    (tyvardecl Signature (type))
                                    
                                    Signature_match
                                    (vardecl
                                      Signature
                                      (fun [(con integer) (con 8)] Signature)
                                    )
                                  )
                                )
                                (let
                                  (nonrec
                                  )
                                  (datatypebind
                                    (datatype
                                      (tyvardecl Value (type))
                                      
                                      Value_match
                                      (vardecl
                                        Value
                                        (fun [(con integer) (con 8)] Value)
                                      )
                                    )
                                  )
                                  (let
                                    (nonrec
                                    )
                                    (datatypebind
                                      (datatype
                                        (tyvardecl
                                          PendingTxOut (type)
                                        )
                                        
                                        PendingTxOut_match
                                        (vardecl
                                          PendingTxOut
                                          (fun Value (fun [Maybe [[Tuple2 ValidatorHash] DataScriptHash]] (fun PendingTxOutType PendingTxOut)))
                                        )
                                      )
                                    )
                                    (let
                                      (nonrec
                                      )
                                      (datatypebind
                                        (datatype
                                          (tyvardecl
                                            Campaign (type)
                                          )
                                          
                                          Campaign_match
                                          (vardecl
                                            Campaign
                                            (fun Height (fun Value (fun Height (fun PubKey Campaign))))
                                          )
                                        )
                                      )
                                      (let
                                        (nonrec
                                        )
                                        (datatypebind
                                          (datatype
                                            (tyvardecl
                                              PendingTxOutRef (type)
                                            )
                                            
                                            PendingTxOutRef_match
                                            (vardecl
                                              PendingTxOutRef
                                              (fun TxHash (fun [(con integer) (con 8)] (fun [List Signature] PendingTxOutRef)))
                                            )
                                          )
                                        )
                                        (let
                                          (nonrec
                                          )
                                          (datatypebind
                                            (datatype
                                              (tyvardecl
                                                PendingTxIn (type)
                                              )
                                              
                                              PendingTxIn_match
                                              (vardecl
                                                PendingTxIn
                                                (fun PendingTxOutRef (fun [Maybe [[Tuple2 ValidatorHash] RedeemerHash]] (fun Value PendingTxIn)))
                                              )
                                            )
                                          )
                                          (let
                                            (nonrec
                                            )
                                            (datatypebind
                                              (datatype
                                                (tyvardecl
                                                  PendingTx (fun (type) (type))
                                                )
                                                (tyvardecl a (type))
                                                PendingTx_match
                                                (vardecl
                                                  PendingTx
                                                  (fun [List PendingTxIn] (fun [List PendingTxOut] (fun Value (fun Value (fun Height (fun [List Signature] (fun a [PendingTx a])))))))
                                                )
                                              )
                                            )
                                            (let
                                              (nonrec
                                              )
                                              (termbind
                                                (vardecl
                                                  addInteger
                                                  (fun [(con integer) (con 8)] (fun [(con integer) (con 8)] [(con integer) (con 8)]))
                                                )
                                                { (builtin addInteger) (con 8) }
                                              )
                                              (let
                                                (nonrec
                                                )
                                                (datatypebind
                                                  (datatype
                                                    (tyvardecl Bool (type))
                                                    
                                                    Bool_match
                                                    (vardecl True Bool)
                                                    (vardecl False Bool)
                                                  )
                                                )
                                                (let
                                                  (nonrec
                                                  )
                                                  (termbind
                                                    (vardecl
                                                      equalsInteger
                                                      (fun [(con integer) (con 8)] (fun [(con integer) (con 8)] Bool))
                                                    )
                                                    (lam
                                                      arg
                                                      [(con integer) (con 8)]
                                                      (lam
                                                        arg
                                                        [(con integer) (con 8)]
                                                        [
                                                          (lam
                                                            b
                                                            (all a (type) (fun a (fun a a)))
                                                            [
                                                              [
                                                                { b Bool } True
                                                              ]
                                                              False
                                                            ]
                                                          )
                                                          [
                                                            [
                                                              {
                                                                (builtin
                                                                  equalsInteger
                                                                )
                                                                (con 8)
                                                              }
                                                              arg
                                                            ]
                                                            arg
                                                          ]
                                                        ]
                                                      )
                                                    )
                                                  )
                                                  (let
                                                    (nonrec
                                                    )
                                                    (termbind
                                                      (vardecl
                                                        greaterThanEqInteger
                                                        (fun [(con integer) (con 8)] (fun [(con integer) (con 8)] Bool))
                                                      )
                                                      (lam
                                                        arg
                                                        [(con integer) (con 8)]
                                                        (lam
                                                          arg
                                                          [(con integer) (con 8)]
                                                          [
                                                            (lam
                                                              b
                                                              (all a (type) (fun a (fun a a)))
                                                              [
                                                                [
                                                                  { b Bool }
                                                                  True
                                                                ]
                                                                False
                                                              ]
                                                            )
                                                            [
                                                              [
                                                                {
                                                                  (builtin
                                                                    greaterThanEqualsInteger
                                                                  )
                                                                  (con 8)
                                                                }
                                                                arg
                                                              ]
                                                              arg
                                                            ]
                                                          ]
                                                        )
                                                      )
                                                    )
                                                    (let
                                                      (nonrec
                                                      )
                                                      (termbind
                                                        (vardecl
                                                          lessThanInteger
                                                          (fun [(con integer) (con 8)] (fun [(con integer) (con 8)] Bool))
                                                        )
                                                        (lam
                                                          arg
                                                          [(con integer) (con 8)]
                                                          (lam
                                                            arg
                                                            [(con integer) (con 8)]
                                                            [
                                                              (lam
                                                                b
                                                                (all a (type) (fun a (fun a a)))
                                                                [
                                                                  [
                                                                    { b Bool }
                                                                    True
                                                                  ]
                                                                  False
                                                                ]
                                                              )
                                                              [
                                                                [
                                                                  {
                                                                    (builtin
                                                                      lessThanInteger
                                                                    )
                                                                    (con 8)
                                                                  }
                                                                  arg
                                                                ]
                                                                arg
                                                              ]
                                                            ]
                                                          )
                                                        )
                                                      )
                                                      (lam
                                                        ds
                                                        Campaign
                                                        (lam
                                                          ds
                                                          CampaignAction
                                                          (lam
                                                            ds
                                                            PubKey
                                                            (lam
                                                              ds
                                                              [PendingTx ValidatorHash]
                                                              [
                                                                {
                                                                  [
                                                                    Campaign_match
                                                                    ds
                                                                  ]
                                                                  Unit
                                                                }
                                                                (lam
                                                                  ds
                                                                  Height
                                                                  (lam
                                                                    ds
                                                                    Value
                                                                    (lam
                                                                      ds
                                                                      Height
                                                                      (lam
                                                                        ds
                                                                        PubKey
                                                                        (let
                                                                          (nonrec
                                                                          )
                                                                          (termbind
                                                                            (vardecl
                                                                              ds
                                                                              [[[Tuple3 [List PendingTxIn]] [List PendingTxOut]] [(con integer) (con 8)]]
                                                                            )
                                                                            [
                                                                              {
                                                                                [
                                                                                  {
                                                                                    PendingTx_match
                                                                                    ValidatorHash
                                                                                  }
                                                                                  ds
                                                                                ]
                                                                                [[[Tuple3 [List PendingTxIn]] [List PendingTxOut]] [(con integer) (con 8)]]
                                                                              }
                                                                              (lam
                                                                                ps
                                                                                [List PendingTxIn]
                                                                                (lam
                                                                                  outs
                                                                                  [List PendingTxOut]
                                                                                  (lam
                                                                                    ds
                                                                                    Value
                                                                                    (lam
                                                                                      ds
                                                                                      Value
                                                                                      (lam
                                                                                        ds
                                                                                        Height
                                                                                        (lam
                                                                                          ds
                                                                                          [List Signature]
                                                                                          (lam
                                                                                            ds
                                                                                            ValidatorHash
                                                                                            [
                                                                                              [
                                                                                                [
                                                                                                  {
                                                                                                    {
                                                                                                      {
                                                                                                        Tuple3
                                                                                                        [List PendingTxIn]
                                                                                                      }
                                                                                                      [List PendingTxOut]
                                                                                                    }
                                                                                                    [(con integer) (con 8)]
                                                                                                  }
                                                                                                  ps
                                                                                                ]
                                                                                                outs
                                                                                              ]
                                                                                              [
                                                                                                {
                                                                                                  [
                                                                                                    Height_match
                                                                                                    ds
                                                                                                  ]
                                                                                                  [(con integer) (con 8)]
                                                                                                }
                                                                                                (lam
                                                                                                  inner
                                                                                                  [(con integer) (con 8)]
                                                                                                  inner
                                                                                                )
                                                                                              ]
                                                                                            ]
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            ]
                                                                          )
                                                                          (let
                                                                            (nonrec
                                                                            )
                                                                            (termbind
                                                                              (vardecl
                                                                                h
                                                                                [(con integer) (con 8)]
                                                                              )
                                                                              [
                                                                                {
                                                                                  [
                                                                                    {
                                                                                      {
                                                                                        {
                                                                                          Tuple3_match
                                                                                          [List PendingTxIn]
                                                                                        }
                                                                                        [List PendingTxOut]
                                                                                      }
                                                                                      [(con integer) (con 8)]
                                                                                    }
                                                                                    ds
                                                                                  ]
                                                                                  [(con integer) (con 8)]
                                                                                }
                                                                                (lam
                                                                                  ps
                                                                                  [List PendingTxIn]
                                                                                  (lam
                                                                                    outs
                                                                                    [List PendingTxOut]
                                                                                    (lam
                                                                                      h
                                                                                      [(con integer) (con 8)]
                                                                                      h
                                                                                    )
                                                                                  )
                                                                                )
                                                                              ]
                                                                            )
                                                                            (let
                                                                              (nonrec
                                                                              )
                                                                              (termbind
                                                                                (vardecl
                                                                                  bad_name
                                                                                  (fun Bool (fun Bool Bool))
                                                                                )
                                                                                (lam
                                                                                  ds
                                                                                  Bool
                                                                                  (lam
                                                                                    ds
                                                                                    Bool
                                                                                    [
                                                                                      [
                                                                                        {
                                                                                          [
                                                                                            Bool_match
                                                                                            ds
                                                                                          ]
                                                                                          Bool
                                                                                        }
                                                                                        ds
                                                                                      ]
                                                                                      False
                                                                                    ]
                                                                                  )
                                                                                )
                                                                              )
                                                                              [
                                                                                [
                                                                                  [
                                                                                    {
                                                                                      [
                                                                                        Bool_match
                                                                                        [
                                                                                          [
                                                                                            [
                                                                                              {
                                                                                                [
                                                                                                  CampaignAction_match
                                                                                                  ds
                                                                                                ]
                                                                                                (fun Unit Bool)
                                                                                              }
                                                                                              (lam
                                                                                                thunk
                                                                                                Unit
                                                                                                [
                                                                                                  [
                                                                                                    bad_name
                                                                                                    [
                                                                                                      [
                                                                                                        greaterThanEqInteger
                                                                                                        h
                                                                                                      ]
                                                                                                      [
                                                                                                        {
                                                                                                          [
                                                                                                            Height_match
                                                                                                            ds
                                                                                                          ]
                                                                                                          [(con integer) (con 8)]
                                                                                                        }
                                                                                                        (lam
                                                                                                          inner
                                                                                                          [(con integer) (con 8)]
                                                                                                          inner
                                                                                                        )
                                                                                                      ]
                                                                                                    ]
                                                                                                  ]
                                                                                                  [
                                                                                                    [
                                                                                                      bad_name
                                                                                                      [
                                                                                                        [
                                                                                                          lessThanInteger
                                                                                                          h
                                                                                                        ]
                                                                                                        [
                                                                                                          {
                                                                                                            [
                                                                                                              Height_match
                                                                                                              ds
                                                                                                            ]
                                                                                                            [(con integer) (con 8)]
                                                                                                          }
                                                                                                          (lam
                                                                                                            inner
                                                                                                            [(con integer) (con 8)]
                                                                                                            inner
                                                                                                          )
                                                                                                        ]
                                                                                                      ]
                                                                                                    ]
                                                                                                    [
                                                                                                      [
                                                                                                        bad_name
                                                                                                        [
                                                                                                          [
                                                                                                            greaterThanEqInteger
                                                                                                            (let
                                                                                                              (nonrec
                                                                                                              )
                                                                                                              (termbind
                                                                                                                (vardecl
                                                                                                                  v
                                                                                                                  (fun PendingTxIn [(con integer) (con 8)])
                                                                                                                )
                                                                                                                (lam
                                                                                                                  ds
                                                                                                                  PendingTxIn
                                                                                                                  [
                                                                                                                    {
                                                                                                                      [
                                                                                                                        PendingTxIn_match
                                                                                                                        ds
                                                                                                                      ]
                                                                                                                      [(con integer) (con 8)]
                                                                                                                    }
                                                                                                                    (lam
                                                                                                                      ds
                                                                                                                      PendingTxOutRef
                                                                                                                      (lam
                                                                                                                        ds
                                                                                                                        [Maybe [[Tuple2 ValidatorHash] RedeemerHash]]
                                                                                                                        (lam
                                                                                                                          ds
                                                                                                                          Value
                                                                                                                          [
                                                                                                                            {
                                                                                                                              [
                                                                                                                                Value_match
                                                                                                                                ds
                                                                                                                              ]
                                                                                                                              [(con integer) (con 8)]
                                                                                                                            }
                                                                                                                            (lam
                                                                                                                              inner
                                                                                                                              [(con integer) (con 8)]
                                                                                                                              inner
                                                                                                                            )
                                                                                                                          ]
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  ]
                                                                                                                )
                                                                                                              )
                                                                                                              (let
                                                                                                                (nonrec
                                                                                                                )
                                                                                                                (termbind
                                                                                                                  (vardecl
                                                                                                                    f
                                                                                                                    (fun PendingTxIn (fun [(con integer) (con 8)] [(con integer) (con 8)]))
                                                                                                                  )
                                                                                                                  (lam
                                                                                                                    i
                                                                                                                    PendingTxIn
                                                                                                                    (lam
                                                                                                                      total
                                                                                                                      [(con integer) (con 8)]
                                                                                                                      [
                                                                                                                        [
                                                                                                                          addInteger
                                                                                                                          total
                                                                                                                        ]
                                                                                                                        [
                                                                                                                          v
                                                                                                                          i
                                                                                                                        ]
                                                                                                                      ]
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                (let
                                                                                                                  (nonrec
                                                                                                                  )
                                                                                                                  (termbind
                                                                                                                    (vardecl
                                                                                                                      eta
                                                                                                                      [(con integer) (con 8)]
                                                                                                                    )
                                                                                                                    (con
                                                                                                                      8 ! 0
                                                                                                                    )
                                                                                                                  )
                                                                                                                  (let
                                                                                                                    (nonrec
                                                                                                                    )
                                                                                                                    (termbind
                                                                                                                      (vardecl
                                                                                                                        eta
                                                                                                                        [List PendingTxIn]
                                                                                                                      )
                                                                                                                      [
                                                                                                                        {
                                                                                                                          [
                                                                                                                            {
                                                                                                                              {
                                                                                                                                {
                                                                                                                                  Tuple3_match
                                                                                                                                  [List PendingTxIn]
                                                                                                                                }
                                                                                                                                [List PendingTxOut]
                                                                                                                              }
                                                                                                                              [(con integer) (con 8)]
                                                                                                                            }
                                                                                                                            ds
                                                                                                                          ]
                                                                                                                          [List PendingTxIn]
                                                                                                                        }
                                                                                                                        (lam
                                                                                                                          ps
                                                                                                                          [List PendingTxIn]
                                                                                                                          (lam
                                                                                                                            outs
                                                                                                                            [List PendingTxOut]
                                                                                                                            (lam
                                                                                                                              h
                                                                                                                              [(con integer) (con 8)]
                                                                                                                              ps
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      ]
                                                                                                                    )
                                                                                                                    [
                                                                                                                      [
                                                                                                                        (let
                                                                                                                          (rec
                                                                                                                          )
                                                                                                                          (termbind
                                                                                                                            (vardecl
                                                                                                                              go
                                                                                                                              (fun [(con integer) (con 8)] (fun [List PendingTxIn] [(con integer) (con 8)]))
                                                                                                                            )
                                                                                                                            (lam
                                                                                                                              cur
                                                                                                                              [(con integer) (con 8)]
                                                                                                                              (lam
                                                                                                                                as
                                                                                                                                [List PendingTxIn]
                                                                                                                                [
                                                                                                                                  [
                                                                                                                                    {
                                                                                                                                      [
                                                                                                                                        {
                                                                                                                                          Nil_match
                                                                                                                                          PendingTxIn
                                                                                                                                        }
                                                                                                                                        as
                                                                                                                                      ]
                                                                                                                                      [(con integer) (con 8)]
                                                                                                                                    }
                                                                                                                                    cur
                                                                                                                                  ]
                                                                                                                                  (lam
                                                                                                                                    a
                                                                                                                                    PendingTxIn
                                                                                                                                    (lam
                                                                                                                                      as
                                                                                                                                      [List PendingTxIn]
                                                                                                                                      [
                                                                                                                                        [
                                                                                                                                          go
                                                                                                                                          [
                                                                                                                                            [
                                                                                                                                              f
                                                                                                                                              a
                                                                                                                                            ]
                                                                                                                                            cur
                                                                                                                                          ]
                                                                                                                                        ]
                                                                                                                                        as
                                                                                                                                      ]
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                ]
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                          go
                                                                                                                        )
                                                                                                                        eta
                                                                                                                      ]
                                                                                                                      eta
                                                                                                                    ]
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          ]
                                                                                                          [
                                                                                                            {
                                                                                                              [
                                                                                                                Value_match
                                                                                                                ds
                                                                                                              ]
                                                                                                              [(con integer) (con 8)]
                                                                                                            }
                                                                                                            (lam
                                                                                                              inner
                                                                                                              [(con integer) (con 8)]
                                                                                                              inner
                                                                                                            )
                                                                                                          ]
                                                                                                        ]
                                                                                                      ]
                                                                                                      (let
                                                                                                        (nonrec
                                                                                                        )
                                                                                                        (termbind
                                                                                                          (vardecl
                                                                                                            signedBy
                                                                                                            (fun Signature Bool)
                                                                                                          )
                                                                                                          (lam
                                                                                                            ds
                                                                                                            Signature
                                                                                                            [
                                                                                                              [
                                                                                                                equalsInteger
                                                                                                                [
                                                                                                                  {
                                                                                                                    [
                                                                                                                      Signature_match
                                                                                                                      ds
                                                                                                                    ]
                                                                                                                    [(con integer) (con 8)]
                                                                                                                  }
                                                                                                                  (lam
                                                                                                                    inner
                                                                                                                    [(con integer) (con 8)]
                                                                                                                    inner
                                                                                                                  )
                                                                                                                ]
                                                                                                              ]
                                                                                                              [
                                                                                                                {
                                                                                                                  [
                                                                                                                    PubKey_match
                                                                                                                    ds
                                                                                                                  ]
                                                                                                                  [(con integer) (con 8)]
                                                                                                                }
                                                                                                                (lam
                                                                                                                  inner
                                                                                                                  [(con integer) (con 8)]
                                                                                                                  inner
                                                                                                                )
                                                                                                              ]
                                                                                                            ]
                                                                                                          )
                                                                                                        )
                                                                                                        (let
                                                                                                          (rec
                                                                                                          )
                                                                                                          (termbind
                                                                                                            (vardecl
                                                                                                              go
                                                                                                              (fun [List Signature] Bool)
                                                                                                            )
                                                                                                            (lam
                                                                                                              l
                                                                                                              [List Signature]
                                                                                                              [
                                                                                                                [
                                                                                                                  {
                                                                                                                    [
                                                                                                                      {
                                                                                                                        Nil_match
                                                                                                                        Signature
                                                                                                                      }
                                                                                                                      l
                                                                                                                    ]
                                                                                                                    Bool
                                                                                                                  }
                                                                                                                  False
                                                                                                                ]
                                                                                                                (lam
                                                                                                                  s
                                                                                                                  Signature
                                                                                                                  (lam
                                                                                                                    r
                                                                                                                    [List Signature]
                                                                                                                    [
                                                                                                                      [
                                                                                                                        [
                                                                                                                          {
                                                                                                                            [
                                                                                                                              Bool_match
                                                                                                                              [
                                                                                                                                signedBy
                                                                                                                                s
                                                                                                                              ]
                                                                                                                            ]
                                                                                                                            (fun Unit Bool)
                                                                                                                          }
                                                                                                                          (lam
                                                                                                                            thunk
                                                                                                                            Unit
                                                                                                                            True
                                                                                                                          )
                                                                                                                        ]
                                                                                                                        (lam
                                                                                                                          thunk
                                                                                                                          Unit
                                                                                                                          [
                                                                                                                            go
                                                                                                                            r
                                                                                                                          ]
                                                                                                                        )
                                                                                                                      ]
                                                                                                                      Unit
                                                                                                                    ]
                                                                                                                  )
                                                                                                                )
                                                                                                              ]
                                                                                                            )
                                                                                                          )
                                                                                                          [
                                                                                                            go
                                                                                                            [
                                                                                                              {
                                                                                                                [
                                                                                                                  {
                                                                                                                    PendingTx_match
                                                                                                                    ValidatorHash
                                                                                                                  }
                                                                                                                  ds
                                                                                                                ]
                                                                                                                [List Signature]
                                                                                                              }
                                                                                                              (lam
                                                                                                                ds
                                                                                                                [List PendingTxIn]
                                                                                                                (lam
                                                                                                                  ds
                                                                                                                  [List PendingTxOut]
                                                                                                                  (lam
                                                                                                                    ds
                                                                                                                    Value
                                                                                                                    (lam
                                                                                                                      ds
                                                                                                                      Value
                                                                                                                      (lam
                                                                                                                        ds
                                                                                                                        Height
                                                                                                                        (lam
                                                                                                                          sigs
                                                                                                                          [List Signature]
                                                                                                                          (lam
                                                                                                                            ds
                                                                                                                            ValidatorHash
                                                                                                                            sigs
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            ]
                                                                                                          ]
                                                                                                        )
                                                                                                      )
                                                                                                    ]
                                                                                                  ]
                                                                                                ]
                                                                                              )
                                                                                            ]
                                                                                            (lam
                                                                                              thunk
                                                                                              Unit
                                                                                              [
                                                                                                [
                                                                                                  bad_name
                                                                                                  [
                                                                                                    [
                                                                                                      greaterThanEqInteger
                                                                                                      h
                                                                                                    ]
                                                                                                    [
                                                                                                      {
                                                                                                        [
                                                                                                          Height_match
                                                                                                          ds
                                                                                                        ]
                                                                                                        [(con integer) (con 8)]
                                                                                                      }
                                                                                                      (lam
                                                                                                        inner
                                                                                                        [(con integer) (con 8)]
                                                                                                        inner
                                                                                                      )
                                                                                                    ]
                                                                                                  ]
                                                                                                ]
                                                                                                [
                                                                                                  [
                                                                                                    bad_name
                                                                                                    (let
                                                                                                      (nonrec
                                                                                                      )
                                                                                                      (termbind
                                                                                                        (vardecl
                                                                                                          pred
                                                                                                          (fun PendingTxOut Bool)
                                                                                                        )
                                                                                                        (lam
                                                                                                          o
                                                                                                          PendingTxOut
                                                                                                          [
                                                                                                            [
                                                                                                              {
                                                                                                                [
                                                                                                                  {
                                                                                                                    Maybe_match
                                                                                                                    PubKey
                                                                                                                  }
                                                                                                                  [
                                                                                                                    {
                                                                                                                      [
                                                                                                                        PendingTxOut_match
                                                                                                                        o
                                                                                                                      ]
                                                                                                                      [Maybe PubKey]
                                                                                                                    }
                                                                                                                    (lam
                                                                                                                      ds
                                                                                                                      Value
                                                                                                                      (lam
                                                                                                                        ds
                                                                                                                        [Maybe [[Tuple2 ValidatorHash] DataScriptHash]]
                                                                                                                        (lam
                                                                                                                          ds
                                                                                                                          PendingTxOutType
                                                                                                                          [
                                                                                                                            [
                                                                                                                              [
                                                                                                                                {
                                                                                                                                  [
                                                                                                                                    PendingTxOutType_match
                                                                                                                                    ds
                                                                                                                                  ]
                                                                                                                                  (fun Unit [Maybe PubKey])
                                                                                                                                }
                                                                                                                                (lam
                                                                                                                                  thunk
                                                                                                                                  Unit
                                                                                                                                  {
                                                                                                                                    Nothing
                                                                                                                                    PubKey
                                                                                                                                  }
                                                                                                                                )
                                                                                                                              ]
                                                                                                                              (lam
                                                                                                                                pk
                                                                                                                                PubKey
                                                                                                                                (lam
                                                                                                                                  thunk
                                                                                                                                  Unit
                                                                                                                                  [
                                                                                                                                    {
                                                                                                                                      Just
                                                                                                                                      PubKey
                                                                                                                                    }
                                                                                                                                    pk
                                                                                                                                  ]
                                                                                                                                )
                                                                                                                              )
                                                                                                                            ]
                                                                                                                            Unit
                                                                                                                          ]
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  ]
                                                                                                                ]
                                                                                                                Bool
                                                                                                              }
                                                                                                              (lam
                                                                                                                pk
                                                                                                                PubKey
                                                                                                                [
                                                                                                                  [
                                                                                                                    equalsInteger
                                                                                                                    [
                                                                                                                      {
                                                                                                                        [
                                                                                                                          PubKey_match
                                                                                                                          pk
                                                                                                                        ]
                                                                                                                        [(con integer) (con 8)]
                                                                                                                      }
                                                                                                                      (lam
                                                                                                                        inner
                                                                                                                        [(con integer) (con 8)]
                                                                                                                        inner
                                                                                                                      )
                                                                                                                    ]
                                                                                                                  ]
                                                                                                                  [
                                                                                                                    {
                                                                                                                      [
                                                                                                                        PubKey_match
                                                                                                                        ds
                                                                                                                      ]
                                                                                                                      [(con integer) (con 8)]
                                                                                                                    }
                                                                                                                    (lam
                                                                                                                      inner
                                                                                                                      [(con integer) (con 8)]
                                                                                                                      inner
                                                                                                                    )
                                                                                                                  ]
                                                                                                                ]
                                                                                                              )
                                                                                                            ]
                                                                                                            False
                                                                                                          ]
                                                                                                        )
                                                                                                      )
                                                                                                      (let
                                                                                                        (nonrec
                                                                                                        )
                                                                                                        (termbind
                                                                                                          (vardecl
                                                                                                            and
                                                                                                            (fun Bool (fun Bool Bool))
                                                                                                          )
                                                                                                          (lam
                                                                                                            a
                                                                                                            Bool
                                                                                                            (lam
                                                                                                              b
                                                                                                              Bool
                                                                                                              [
                                                                                                                [
                                                                                                                  {
                                                                                                                    [
                                                                                                                      Bool_match
                                                                                                                      a
                                                                                                                    ]
                                                                                                                    Bool
                                                                                                                  }
                                                                                                                  b
                                                                                                                ]
                                                                                                                False
                                                                                                              ]
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                        (let
                                                                                                          (nonrec
                                                                                                          )
                                                                                                          (termbind
                                                                                                            (vardecl
                                                                                                              eta
                                                                                                              [List PendingTxOut]
                                                                                                            )
                                                                                                            [
                                                                                                              {
                                                                                                                [
                                                                                                                  {
                                                                                                                    {
                                                                                                                      {
                                                                                                                        Tuple3_match
                                                                                                                        [List PendingTxIn]
                                                                                                                      }
                                                                                                                      [List PendingTxOut]
                                                                                                                    }
                                                                                                                    [(con integer) (con 8)]
                                                                                                                  }
                                                                                                                  ds
                                                                                                                ]
                                                                                                                [List PendingTxOut]
                                                                                                              }
                                                                                                              (lam
                                                                                                                ps
                                                                                                                [List PendingTxIn]
                                                                                                                (lam
                                                                                                                  outs
                                                                                                                  [List PendingTxOut]
                                                                                                                  (lam
                                                                                                                    h
                                                                                                                    [(con integer) (con 8)]
                                                                                                                    outs
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            ]
                                                                                                          )
                                                                                                          [
                                                                                                            (let
                                                                                                              (rec
                                                                                                              )
                                                                                                              (termbind
                                                                                                                (vardecl
                                                                                                                  go
                                                                                                                  (fun [List PendingTxOut] Bool)
                                                                                                                )
                                                                                                                (lam
                                                                                                                  lst
                                                                                                                  [List PendingTxOut]
                                                                                                                  [
                                                                                                                    [
                                                                                                                      {
                                                                                                                        [
                                                                                                                          {
                                                                                                                            Nil_match
                                                                                                                            PendingTxOut
                                                                                                                          }
                                                                                                                          lst
                                                                                                                        ]
                                                                                                                        Bool
                                                                                                                      }
                                                                                                                      True
                                                                                                                    ]
                                                                                                                    (lam
                                                                                                                      x
                                                                                                                      PendingTxOut
                                                                                                                      (lam
                                                                                                                        xs
                                                                                                                        [List PendingTxOut]
                                                                                                                        [
                                                                                                                          [
                                                                                                                            and
                                                                                                                            [
                                                                                                                              pred
                                                                                                                              x
                                                                                                                            ]
                                                                                                                          ]
                                                                                                                          [
                                                                                                                            go
                                                                                                                            xs
                                                                                                                          ]
                                                                                                                        ]
                                                                                                                      )
                                                                                                                    )
                                                                                                                  ]
                                                                                                                )
                                                                                                              )
                                                                                                              go
                                                                                                            )
                                                                                                            eta
                                                                                                          ]
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  ]
                                                                                                  (let
                                                                                                    (nonrec
                                                                                                    )
                                                                                                    (termbind
                                                                                                      (vardecl
                                                                                                        signedBy
                                                                                                        (fun Signature Bool)
                                                                                                      )
                                                                                                      (lam
                                                                                                        ds
                                                                                                        Signature
                                                                                                        [
                                                                                                          [
                                                                                                            equalsInteger
                                                                                                            [
                                                                                                              {
                                                                                                                [
                                                                                                                  Signature_match
                                                                                                                  ds
                                                                                                                ]
                                                                                                                [(con integer) (con 8)]
                                                                                                              }
                                                                                                              (lam
                                                                                                                inner
                                                                                                                [(con integer) (con 8)]
                                                                                                                inner
                                                                                                              )
                                                                                                            ]
                                                                                                          ]
                                                                                                          [
                                                                                                            {
                                                                                                              [
                                                                                                                PubKey_match
                                                                                                                ds
                                                                                                              ]
                                                                                                              [(con integer) (con 8)]
                                                                                                            }
                                                                                                            (lam
                                                                                                              inner
                                                                                                              [(con integer) (con 8)]
                                                                                                              inner
                                                                                                            )
                                                                                                          ]
                                                                                                        ]
                                                                                                      )
                                                                                                    )
                                                                                                    (let
                                                                                                      (rec
                                                                                                      )
                                                                                                      (termbind
                                                                                                        (vardecl
                                                                                                          go
                                                                                                          (fun [List Signature] Bool)
                                                                                                        )
                                                                                                        (lam
                                                                                                          l
                                                                                                          [List Signature]
                                                                                                          [
                                                                                                            [
                                                                                                              {
                                                                                                                [
                                                                                                                  {
                                                                                                                    Nil_match
                                                                                                                    Signature
                                                                                                                  }
                                                                                                                  l
                                                                                                                ]
                                                                                                                Bool
                                                                                                              }
                                                                                                              False
                                                                                                            ]
                                                                                                            (lam
                                                                                                              s
                                                                                                              Signature
                                                                                                              (lam
                                                                                                                r
                                                                                                                [List Signature]
                                                                                                                [
                                                                                                                  [
                                                                                                                    [
                                                                                                                      {
                                                                                                                        [
                                                                                                                          Bool_match
                                                                                                                          [
                                                                                                                            signedBy
                                                                                                                            s
                                                                                                                          ]
                                                                                                                        ]
                                                                                                                        (fun Unit Bool)
                                                                                                                      }
                                                                                                                      (lam
                                                                                                                        thunk
                                                                                                                        Unit
                                                                                                                        True
                                                                                                                      )
                                                                                                                    ]
                                                                                                                    (lam
                                                                                                                      thunk
                                                                                                                      Unit
                                                                                                                      [
                                                                                                                        go
                                                                                                                        r
                                                                                                                      ]
                                                                                                                    )
                                                                                                                  ]
                                                                                                                  Unit
                                                                                                                ]
                                                                                                              )
                                                                                                            )
                                                                                                          ]
                                                                                                        )
                                                                                                      )
                                                                                                      [
                                                                                                        go
                                                                                                        [
                                                                                                          {
                                                                                                            [
                                                                                                              {
                                                                                                                PendingTx_match
                                                                                                                ValidatorHash
                                                                                                              }
                                                                                                              ds
                                                                                                            ]
                                                                                                            [List Signature]
                                                                                                          }
                                                                                                          (lam
                                                                                                            ds
                                                                                                            [List PendingTxIn]
                                                                                                            (lam
                                                                                                              ds
                                                                                                              [List PendingTxOut]
                                                                                                              (lam
                                                                                                                ds
                                                                                                                Value
                                                                                                                (lam
                                                                                                                  ds
                                                                                                                  Value
                                                                                                                  (lam
                                                                                                                    ds
                                                                                                                    Height
                                                                                                                    (lam
                                                                                                                      sigs
                                                                                                                      [List Signature]
                                                                                                                      (lam
                                                                                                                        ds
                                                                                                                        ValidatorHash
                                                                                                                        sigs
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        ]
                                                                                                      ]
                                                                                                    )
                                                                                                  )
                                                                                                ]
                                                                                              ]
                                                                                            )
                                                                                          ]
                                                                                          Unit
                                                                                        ]
                                                                                      ]
                                                                                      (fun Unit Unit)
                                                                                    }
                                                                                    (lam
                                                                                      thunk
                                                                                      Unit
                                                                                      Unit
                                                                                    )
                                                                                  ]
                                                                                  (lam
                                                                                    thunk
                                                                                    Unit
                                                                                    [
                                                                                      {
                                                                                        error
                                                                                        Unit
                                                                                      }
                                                                                      Unit
                                                                                    ]
                                                                                  )
                                                                                ]
                                                                                Unit
                                                                              ]
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              ]
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)