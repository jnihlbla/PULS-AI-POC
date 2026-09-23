      *** EDIT ALLOWED                                                          
      *   operationId: goodsCarrierErrors,                                      
      *   basePath: /fls/apim/inventory/gcs                                     
      *   relativePath: /errors                                                 
      *   method: POST                                                          
             06 ReqHeaders.                                                     
               09 user-key-length               PIC S9999 COMP-5 SYNC.          
               09 user-key                      PIC X(32).                      
             06 ReqBody.                                                        
               09 mfg-length                    PIC S9999 COMP-5 SYNC.          
               09 mfg                           PIC X(20).                      
               09 deliveryNoteNumber-length     PIC S9999 COMP-5 SYNC.          
               09 deliveryNoteNumber            PIC X(20).                      
               09 shipTo-length                 PIC S9999 COMP-5 SYNC.          
               09 shipTo                        PIC X(20).                      
               09 issueDateYear-length          PIC S9999 COMP-5 SYNC.          
               09 issueDateYear                 PIC X(4).                       
                                                                                
               09 errors2-num                   PIC S9(9) COMP-5 SYNC.          
                                                                                
               09 errors OCCURS 10.                                             
                 12 errorCode-length              PIC S9999 COMP-5              
            SYNC.                                                               
                 12 errorCode                     PIC X(10).                    
                 12 errorMessage-length           PIC S9999 COMP-5              
            SYNC.                                                               
                 12 errorMessage                  PIC X(255).                   
                 12 filler                        PIC X(1).                     
                                                                                
