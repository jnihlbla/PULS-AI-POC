      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++          
      * This file contains the generated language structure(s) for              
      *  request JSON schema                                                    
      *  'vcop_api_puls_v1_PulsEvent_POST_request.json'.                        
      * This structure was generated using 'DFHJS2LS' at mapping level          
      *  '4.3'.                                                                 
      *                                                                         
      *                                                                         
      *      06 ReqHeaders.                                                     
      *                                                                         
      * Comments for field 'user-key':                                          
      * This field represents the value of JSON schema keyword                  
      *  'ReqHeaders->user-key'.                                                
      * JSON schema description: user-key header.                               
      * JSON schema type: 'string'.                                             
      * This field contains a varying length array of characters or             
      *  binary data.                                                           
      *        09 user-key-length               PIC S9999 COMP-5 SYNC.          
      *        09 user-key                      PIC X(255).                     
      *      06 ReqBody.                                                        
      *                                                                         
      *                                                                         
      * JSON schema keyword 'ReqBody->eventName' is optional. The               
      *  number of instances present is indicated in field                      
      *  'eventName-num'.                                                       
      * There should be at least '0' instance(s).                               
      * There should be at most '1' instance(s).                                
      *        09 eventName-num                 PIC S9(9) COMP-5 SYNC.          
      *                                                                         
      *                                                                         
      *        09 eventName.                                                    
      *                                                                         
      * Comments for field 'eventName2':                                        
      * This field represents the value of JSON schema keyword                  
      *  'ReqBody->eventName'.                                                  
      * JSON schema type: 'string'.                                             
      * This field contains a varying length array of characters or             
      *  binary data.                                                           
      *          12 eventName2-length             PIC S9999 COMP-5              
      *  SYNC.                                                                  
      *          12 eventName2                    PIC X(255).                   
      *                                                                         
      *                                                                         
      * JSON schema keyword 'ReqBody->eventType' is optional. The               
      *  number of instances present is indicated in field                      
      *  'eventType-num'.                                                       
      * There should be at least '0' instance(s).                               
      * There should be at most '1' instance(s).                                
      *        09 eventType-num                 PIC S9(9) COMP-5 SYNC.          
      *                                                                         
      *                                                                         
      *        09 eventType.                                                    
      *                                                                         
      * Comments for field 'eventType2':                                        
      * This field represents the value of JSON schema keyword                  
      *  'ReqBody->eventType'.                                                  
      * JSON schema type: 'string'.                                             
      * This field contains a varying length array of characters or             
      *  binary data.                                                           
      *          12 eventType2-length             PIC S9999 COMP-5              
      *  SYNC.                                                                  
      *          12 eventType2                    PIC X(255).                   
      *                                                                         
      *                                                                         
      * JSON schema keyword 'ReqBody->timestamp' is optional. The               
      *  number of instances present is indicated in field                      
      *  'timestamp-num'.                                                       
      * There should be at least '0' instance(s).                               
      * There should be at most '1' instance(s).                                
      *        09 timestamp-num                 PIC S9(9) COMP-5 SYNC.          
      *                                                                         
      *                                                                         
      *        09 timestamp.                                                    
      *                                                                         
      * Comments for field 'timestamp2':                                        
      * This field represents the value of JSON schema keyword                  
      *  'ReqBody->timestamp'.                                                  
      * JSON schema type: 'string'.                                             
      * This field contains a varying length array of characters or             
      *  binary data.                                                           
      *          12 timestamp2-length             PIC S9999 COMP-5              
      *  SYNC.                                                                  
      *          12 timestamp2                    PIC X(255).                   
      *                                                                         
      *                                                                         
      * JSON schema keyword 'ReqBody->eventData' is optional. The               
      *  number of instances present is indicated in field                      
      *  'eventData2-num'.                                                      
      * There should be at least '0' instance(s).                               
      * There should be at most '1' instance(s).                                
      *        09 eventData2-num                PIC S9(9) COMP-5 SYNC.          
      *                                                                         
      *                                                                         
      *        09 eventData.                                                    
      *                                                                         
      *                                                                         
      * JSON schema keyword 'ReqBody->eventData->orderId' is optional.          
      *  The number of instances present is indicated in field                  
      *  'orderId-num'.                                                         
      * There should be at least '0' instance(s).                               
      * There should be at most '1' instance(s).                                
      *          12 orderId-num                   PIC S9(9) COMP-5              
      *  SYNC.                                                                  
      *                                                                         
      *                                                                         
      *          12 orderId.                                                    
      *                                                                         
      * Comments for field 'orderId2':                                          
      * This field represents the value of JSON schema keyword                  
      *  'ReqBody->eventData->orderId'.                                         
      * JSON schema type: 'string'.                                             
      * This field contains a varying length array of characters or             
      *  binary data.                                                           
      *            15 orderId2-length               PIC S9999 COMP-5            
      *  SYNC.                                                                  
      *            15 orderId2                      PIC X(255).                 
      *                                                                         
      *                                                                         
      * JSON schema keyword 'ReqBody->eventData->discrepancyId' is              
      *  optional. The number of instances present is indicated in              
      *  field 'discrepancyId-num'.                                             
      * There should be at least '0' instance(s).                               
      * There should be at most '1' instance(s).                                
      *          12 discrepancyId-num             PIC S9(9) COMP-5              
      *  SYNC.                                                                  
      *                                                                         
      *                                                                         
      *          12 discrepancyId.                                              
      *                                                                         
      * Comments for field 'discrepancyId2':                                    
      * This field represents the value of JSON schema keyword                  
      *  'ReqBody->eventData->discrepancyId'.                                   
      * JSON schema type: 'string'.                                             
      * This field contains a varying length array of characters or             
      *  binary data.                                                           
      *            15 discrepancyId2-length         PIC S9999 COMP-5            
      *  SYNC.                                                                  
      *            15 discrepancyId2                PIC X(255).                 
      *                                                                         
      *                                                                         
      * JSON schema keyword 'ReqBody->eventData->eventMessage' is               
      *  optional. The number of instances present is indicated in              
      *  field 'eventMessage-num'.                                              
      * There should be at least '0' instance(s).                               
      * There should be at most '1' instance(s).                                
      *          12 eventMessage-num              PIC S9(9) COMP-5              
      *  SYNC.                                                                  
      *                                                                         
      *                                                                         
      *          12 eventMessage.                                               
      *                                                                         
      * Comments for field 'eventMessage2':                                     
      * This field represents the value of JSON schema keyword                  
      *  'ReqBody->eventData->eventMessage'.                                    
      * JSON schema type: 'string'.                                             
      * This field contains a varying length array of characters or             
      *  binary data.                                                           
      *            15 eventMessage2-length          PIC S9999 COMP-5            
      *  SYNC.                                                                  
      *            15 eventMessage2                 PIC X(255).                 
      *                                                                         
      *                                                                         
      * JSON schema keyword 'ReqBody->eventData->eventCode' is                  
      *  optional. The number of instances present is indicated in              
      *  field 'eventCode-num'.                                                 
      * There should be at least '0' instance(s).                               
      * There should be at most '1' instance(s).                                
      *          12 eventCode-num                 PIC S9(9) COMP-5              
      *  SYNC.                                                                  
      *                                                                         
      *                                                                         
      *          12 eventCode.                                                  
      *                                                                         
      * Comments for field 'eventCode2':                                        
      * This field represents the value of JSON schema keyword                  
      *  'ReqBody->eventData->eventCode'.                                       
      * JSON schema type: 'string'.                                             
      * This field contains a varying length array of characters or             
      *  binary data.                                                           
      *            15 eventCode2-length             PIC S9999 COMP-5            
      *  SYNC.                                                                  
      *            15 eventCode2                    PIC X(255).                 
      *                                                                         
      *                                                                         
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++          
                                                                                
             06 ReqHeaders.                                                     
               09 user-key-length               PIC S9999 COMP-5 SYNC.          
               09 user-key                      PIC X(255).                     
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
                                                                                
                                                                                
