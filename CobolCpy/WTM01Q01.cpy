      *** EDIT ALLOWED                                                          
      *   operationId: ParcelTransportController_delete,                        
      *   basePath:                                                             
      *   relativePath: /parceltransport-transport/transport                    
      *   method: DELETE                                                        
             06 ReqHeaders.                                                     
               09 user-key-length               PIC S9999 COMP-5 SYNC.          
               09 user-key                      PIC X(32).                      
             06 ReqBody.                                                        
               09 parcelTransport.                                              
                 12 issuingApplication-length     PIC S9999 COMP-5              
            SYNC.                                                               
                 12 issuingApplication            PIC X(14).                    
                 12 customerIdentifier-length     PIC S9999 COMP-5              
            SYNC.                                                               
                 12 customerIdentifier            PIC X(11).                    
                 12 parcel.                                                     
                   15 parcelIdentifier-length       PIC S9999 COMP-5            
            SYNC.                                                               
                   15 parcelIdentifier              PIC X(22).                  
                                                                                
                                                                                
