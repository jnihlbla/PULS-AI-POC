      *** EDIT ALLOWED                                                          
      *   operationId: _b2b_Puls_Lynk_V1_puls_zpurcha_0                         
      *   basePath:                                                             
      *   relativePath: /b2b/Puls/Lynk/V1/puls/zpurchaseorderstatus             
      *   method: POST                                                          
             06 ReqHeaders.                                                     
               09 proxy-key-length              PIC S9999 COMP-5 SYNC.          
               09 proxy-key                     PIC X(255).                     
             06 ReqBody.                                                        
                                                                                
               09 eventName-num                 PIC S9(9) COMP-5 SYNC.          
                                                                                
               09 eventName.                                                    
                 12 eventName2-length             PIC S9999 COMP-5              
            SYNC.                                                               
                 12 eventName2                    PIC X(255).                   
                                                                                
               09 eventType-num                 PIC S9(9) COMP-5 SYNC.          
                                                                                
               09 eventType.                                                    
                 12 eventType2-length             PIC S9999 COMP-5              
            SYNC.                                                               
                 12 eventType2                    PIC X(255).                   
                                                                                
               09 timestamp-num                 PIC S9(9) COMP-5 SYNC.          
                                                                                
               09 timestamp.                                                    
                 12 timestamp2-length             PIC S9999 COMP-5              
            SYNC.                                                               
                 12 timestamp2                    PIC X(255).                   
                                                                                
               09 eventData2-num                PIC S9(9) COMP-5 SYNC.          
                                                                                
               09 eventData.                                                    
                                                                                
                 12 orderId-num                   PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 orderId.                                                    
                   15 orderId2-length               PIC S9999 COMP-5            
            SYNC.                                                               
                   15 orderId2                      PIC X(255).                 
                                                                                
                 12 discrepancyId-num             PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 discrepancyId.                                              
                   15 discrepancyId2-length         PIC S9999 COMP-5            
            SYNC.                                                               
                   15 discrepancyId2                PIC X(255).                 
                                                                                
                 12 eventMessage-num              PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 eventMessage.                                               
                   15 eventMessage2-length          PIC S9999 COMP-5            
            SYNC.                                                               
                   15 eventMessage2                 PIC X(255).                 
                                                                                
                 12 eventCode-num                 PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 eventCode.                                                  
                   15 eventCode2-length             PIC S9999 COMP-5            
            SYNC.                                                               
                   15 eventCode2                    PIC X(255).                 
                                                                                
