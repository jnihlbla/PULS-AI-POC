      *** EDIT ALLOWED                                                          
      *   operationId: createProduct, basePath: /synq/resources,                
      *   relativePath: /products, method: POST                                 
             06 ReqHeaders.                                                     
               09 user-key-length               PIC S9999 COMP-5 SYNC.          
               09 user-key                      PIC X(64).                      
             06 ReqBody.                                                        
               09 productId-length              PIC S9999 COMP-5 SYNC.          
               09 productId                     PIC X(9).                       
               09 Xowner-length                 PIC S9999 COMP-5 SYNC.          
               09 Xowner                        PIC X(10).                      
                                                                                
               09 description-num               PIC S9(9) COMP-5 SYNC.          
                                                                                
               09 description.                                                  
                 12 description2-length           PIC S9999 COMP-5              
            SYNC.                                                               
                 12 description2                  PIC X(100).                   
                                                                                
               09 productUom2-num               PIC S9(9) COMP-5 SYNC.          
                                                                                
               09 productUom OCCURS 3.                                          
                 12 uomId-length                  PIC S9999 COMP-5              
            SYNC.                                                               
                 12 uomId                         PIC X(2).                     
                                                                                
                 12 imagePath-num                 PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 imagePath.                                                  
                   15 imagePath2-length             PIC S9999 COMP-5            
            SYNC.                                                               
                   15 imagePath2                    PIC X(2048).                
                 12 ratio                         PIC S9(9) COMP-5              
            SYNC.                                                               
                 12 baseUomFlag                   PIC X DISPLAY.                
                 12 pickUomFlag                   PIC X DISPLAY.                
                 12 putawayUomFlag                PIC X DISPLAY.                
                                                                                
                 12 volume-num                    PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 volume                        COMP-2 SYNC.                  
                 12 filler                        PIC X(4).                     
                                                                                
