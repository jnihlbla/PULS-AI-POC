      *** EDIT ALLOWED                                                          
      *   operationId: deleteOrder, basePath: /synq/resources,                  
      *   relativePath: /orders/{owner}/{orderId}, method: DELETE               
             06 ReqPathParameters.                                              
               09 Xowner-length                 PIC S9999 COMP-5 SYNC.          
               09 Xowner                        PIC X(10).                      
               09 orderId-length                PIC S9999 COMP-5 SYNC.          
               09 orderId                       PIC X(64).                      
             06 ReqHeaders.                                                     
               09 user-key-length               PIC S9999 COMP-5 SYNC.          
               09 user-key                      PIC X(64).                      
                                                                                
