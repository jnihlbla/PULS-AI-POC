      *** EDIT ALLOWED                                                          
      *   operationId: deleteProduct, basePath: /synq/resources,                
      *   relativePath: /products/{owner}/{productId}, method: DELETE           
             06 ReqPathParameters.                                              
               09 Xowner-length                 PIC S9999 COMP-5 SYNC.          
               09 Xowner                        PIC X(10).                      
               09 productId-length              PIC S9999 COMP-5 SYNC.          
               09 productId                     PIC X(9).                       
             06 ReqHeaders.                                                     
               09 user-key-length               PIC S9999 COMP-5 SYNC.          
               09 user-key                      PIC X(64).                      
                                                                                
