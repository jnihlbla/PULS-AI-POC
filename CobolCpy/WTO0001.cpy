             06 ReqHeaders.                                                     
               09 user-key-length               PIC S9999 COMP-5 SYNC.          
               09 user-key                      PIC X(32).                      
             06 ReqBody.                                                        
                                                                                
               09 request2-num                  PIC S9(9) COMP-5 SYNC.          
                                                                                
               09 request.                                                      
                 12 shipmentId-length             PIC S9999 COMP-5              
            SYNC.                                                               
                 12 shipmentId                    PIC X(11).                    
                 12 state-length                  PIC S9999 COMP-5              
            SYNC.                                                               
                 12 state                         PIC X(30).                    
                                                                                
                 12 groupingId-num                PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 groupingId.                                                 
                   15 groupingId2-length            PIC S9999 COMP-5            
            SYNC.                                                               
                   15 groupingId2                   PIC X(20).                  
                                                                                
                 12 transportNumber-num           PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 transportNumber.                                            
                   15 transportNumber2-length       PIC S9999 COMP-5            
            SYNC.                                                               
                   15 transportNumber2              PIC X(3).                   
                                                                                
                 12 receivingDC-num               PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 receivingDC.                                                
                   15 receivingDC2-length           PIC S9999 COMP-5            
            SYNC.                                                               
                   15 receivingDC2                  PIC X(2).                   
                                                                                
                 12 sendingDC-num                 PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 sendingDC.                                                  
                   15 sendingDC2-length             PIC S9999 COMP-5            
            SYNC.                                                               
                   15 sendingDC2                    PIC X(2).                   
                                                                                
                 12 invoiceNumbers-num            PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 invoiceNumbers OCCURS 1500.                                 
                   15 invoiceNumbers2-length        PIC S9999 COMP-5            
            SYNC.                                                               
                   15 invoiceNumbers2               PIC X(7).                   
                   15 filler                        PIC X(1).                   
                                                                                
                 12 consignor-num                 PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 consignor.                                                  
                   15 consignor2-length             PIC S9999 COMP-5            
            SYNC.                                                               
                   15 consignor2                    PIC X(9).                   
                 12 pickup-length                 PIC S9999 COMP-5              
            SYNC.                                                               
                 12 pickup                        PIC X(10).                    
                 12 delivery-length               PIC S9999 COMP-5              
            SYNC.                                                               
                 12 delivery                      PIC X(10).                    
                 12 requestedPickup-length        PIC S9999 COMP-5              
            SYNC.                                                               
                 12 requestedPickup               PIC X(20).                    
                                                                                
                 12 incoterm-num                  PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 incoterm.                                                   
                   15 incoterm2-length              PIC S9999 COMP-5            
            SYNC.                                                               
                   15 incoterm2                     PIC X(35).                  
                                                                                
                 12 volumeInMTQ-num               PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 volumeInMTQ                   PIC S9(9)V9(4)                
            COMP-3.                                                             
                                                                                
                 12 equipment2-num                PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 equipment.                                                  
                                                                                
                   15 sealNo-num                    PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 sealNo OCCURS 2.                                          
                     18 sealNo2-length                PIC S9999 COMP-5          
            SYNC.                                                               
                     18 sealNo2                       PIC X(25).                
                     18 filler                        PIC X(1).                 
                                                                                
                   15 vgmWeightInKGM-num            PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 vgmWeightInKGM                PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                 12 packages2-num                 PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 packages OCCURS 1500.                                       
                   15 packageId-length              PIC S9999 COMP-5            
            SYNC.                                                               
                   15 packageId                     PIC X(22).                  
                   15 packageType-length            PIC S9999 COMP-5            
            SYNC.                                                               
                   15 packageType                   PIC X(6).                   
                                                                                
                   15 packageCode-num               PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 packageCode.                                              
                     18 packageCode2-length           PIC S9999 COMP-5          
            SYNC.                                                               
                     18 packageCode2                  PIC X(8).                 
                                                                                
                   15 packageDescription-num        PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 packageDescription.                                       
                     18 packageDescription2-length    PIC S9999 COMP-5          
            SYNC.                                                               
                     18 packageDescription2           PIC X(20).                
                                                                                
                   15 netWeight-num                 PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 netWeight                     PIC S9(9)V9(4)              
            COMP-3.                                                             
                                                                                
                   15 grossWeight-num               PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 grossWeight                   PIC S9(9)V9(4)              
            COMP-3.                                                             
                                                                                
                   15 Xlength-num                   PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 Xlength                       PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 width-num                     PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 width                         PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 height-num                    PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 height                        PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 parts2-num                    PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 parts OCCURS 300.                                         
                     18 partNumber-length             PIC S9999 COMP-5          
            SYNC.                                                               
                     18 partNumber                    PIC X(9).                 
                                                                                
                     18 partDescription-num           PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 partDescription.                                        
                       21 partDescription2-length       PIC S9999               
            COMP-5 SYNC.                                                        
                       21 partDescription2              PIC X(25).              
                                                                                
                     18 partQty-num                   PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 partQty                       PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 partUOM-num                   PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 partUOM.                                                
                       21 partUOM2-length               PIC S9999               
            COMP-5 SYNC.                                                        
                       21 partUOM2                      PIC X(3).               
                                                                                
                     18 partWeight-num                PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 partWeight                    PIC S9(9)V9(4)            
            COMP-3.                                                             
                                                                                
                     18 dangerous-num                 PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 dangerous                     PIC X DISPLAY.            
                                                                                
                     18 unNumber-num                  PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 unNumber                      PIC S9999 COMP-5          
            SYNC.                                                               
                     18 filler                        PIC X(2).                 
                                                                                
