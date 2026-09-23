      *** EDIT ALLOWED                                                          
      *   operationId: updateSingleOrder, basePath: /synq/resources,            
      *   relativePath: /orders/{owner}/{orderId}, method: PUT                  
             06 ReqPathParameters.                                              
               09 Xowner-length                 PIC S9999 COMP-5 SYNC.          
               09 Xowner                        PIC X(10).                      
               09 orderId-length                PIC S9999 COMP-5 SYNC.          
               09 orderId                       PIC X(64).                      
             06 ReqHeaders.                                                     
               09 user-key-length               PIC S9999 COMP-5 SYNC.          
               09 user-key                      PIC X(64).                      
             06 ReqBody.                                                        
               09 orderId2-length               PIC S9999 COMP-5 SYNC.          
               09 orderId2                      PIC X(64).                      
               09 orderType-length              PIC S9999 COMP-5 SYNC.          
               09 orderType                     PIC X(10).                      
               09 dispatchDate-length           PIC S9999 COMP-5 SYNC.          
               09 dispatchDate                  PIC X(29).                      
               09 priority                      PIC S9(9) COMP-5 SYNC.          
                                                                                
               09 consolidationLoc-num          PIC S9(9) COMP-5 SYNC.          
                                                                                
               09 consolidationLoc.                                             
                 12 consolidationLoc2-length      PIC S9999 COMP-5              
            SYNC.                                                               
                 12 consolidationLoc2             PIC X(64).                    
               09 Xowner2-length                PIC S9999 COMP-5 SYNC.          
               09 Xowner2                       PIC X(10).                      
                                                                                
               09 instruction2-num              PIC S9(9) COMP-5 SYNC.          
                                                                                
               09 instruction OCCURS 3.                                         
                 12 Xtext-length                  PIC S9999 COMP-5              
            SYNC.                                                               
                 12 Xtext                         PIC X(100).                   
                 12 Xtype-length                  PIC S9999 COMP-5              
            SYNC.                                                               
                 12 Xtype                         PIC X(32).                    
                                                                                
               09 orderLine2-num                PIC S9(9) COMP-5 SYNC.          
                                                                                
               09 orderLine OCCURS 999.                                         
                 12 orderLineNumber               PIC S9(9) COMP-5              
            SYNC.                                                               
                 12 productId-length              PIC S9999 COMP-5              
            SYNC.                                                               
                 12 productId                     PIC X(9).                     
                 12 quantityOrdered               COMP-2 SYNC.                  
                                                                                
                 12 attributeValue2-num           PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 attributeValue OCCURS 3.                                    
                   15 name-length                   PIC S9999 COMP-5            
            SYNC.                                                               
                   15 name                          PIC X(10).                  
                                                                                
                   15 Xvalue-num                    PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 Xvalue.                                                   
                     18 Xvalue2-length                PIC S9999 COMP-5          
            SYNC.                                                               
                     18 Xvalue2                       PIC X(10).                
                                                                                
