      *** EDIT ALLOWED                                                          
      *   operationId: createOrder, basePath: /synq/resources,                  
      *   relativePath: /orders/batch, method: POST                             
             06 ReqHeaders.                                                     
               09 user-key-length               PIC S9999 COMP-5 SYNC.          
               09 user-key                      PIC X(64).                      
             06 ReqBody.                                                        
                                                                                
               09 order2-num                    PIC S9(9) COMP-5 SYNC.          
                                                                                
               09 Xorder OCCURS 3.                                              
                 12 orderId-length                PIC S9999 COMP-5              
            SYNC.                                                               
                 12 orderId                       PIC X(64).                    
                 12 orderType-length              PIC S9999 COMP-5              
            SYNC.                                                               
                 12 orderType                     PIC X(10).                    
                 12 dispatchDate-length           PIC S9999 COMP-5              
            SYNC.                                                               
                 12 dispatchDate                  PIC X(29).                    
                 12 priority                      PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 consolidationLoc-num          PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 consolidationLoc.                                           
                   15 consolidationLoc2-length      PIC S9999 COMP-5            
            SYNC.                                                               
                   15 consolidationLoc2             PIC X(64).                  
                 12 Xowner-length                 PIC S9999 COMP-5              
            SYNC.                                                               
                 12 Xowner                        PIC X(10).                    
                                                                                
                 12 instruction2-num              PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 instruction OCCURS 3.                                       
                   15 Xtext-length                  PIC S9999 COMP-5            
            SYNC.                                                               
                   15 Xtext                         PIC X(100).                 
                   15 Xtype-length                  PIC S9999 COMP-5            
            SYNC.                                                               
                   15 Xtype                         PIC X(32).                  
                                                                                
                 12 orderLine2-num                PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 orderLine OCCURS 999.                                       
                   15 orderLineNumber               PIC S9(9) COMP-5            
            SYNC.                                                               
                   15 productId-length              PIC S9999 COMP-5            
            SYNC.                                                               
                   15 productId                     PIC X(9).                   
                   15 quantityOrdered               COMP-2 SYNC.                
                                                                                
                   15 attributeValue2-num           PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 attributeValue OCCURS 3.                                  
                     18 name-length                   PIC S9999 COMP-5          
            SYNC.                                                               
                     18 name                          PIC X(10).                
                                                                                
                     18 Xvalue-num                    PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 Xvalue.                                                 
                       21 Xvalue2-length                PIC S9999               
            COMP-5 SYNC.                                                        
                       21 Xvalue2                       PIC X(10).              
                                                                                
