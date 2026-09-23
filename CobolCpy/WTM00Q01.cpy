      *** EDIT ALLOWED                                                          
      *   operationId: ParcelTransportController_create,                        
      *   basePath:                                                             
      *   relativePath: /parceltransport-transport/transport                    
      *   method: POST                                                          
             06 ReqHeaders.                                                     
               09 user-key-length               PIC S9999 COMP-5 SYNC.          
               09 user-key                      PIC X(32).                      
             06 ReqBody.                                                        
               09 parcelTransport.                                              
                                                                                
                 12 accountIndicator-num          PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 accountIndicator.                                           
                   15 accountIndicator2-length      PIC S9999 COMP-5            
            SYNC.                                                               
                   15 accountIndicator2             PIC X(15).                  
                 12 customerIdentifier-length     PIC S9999 COMP-5              
            SYNC.                                                               
                 12 customerIdentifier            PIC X(11).                    
                                                                                
                 12 customsData2-num              PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 customsData.                                                
                   15 termsOfDelivery-length        PIC S9999 COMP-5            
            SYNC.                                                               
                   15 termsOfDelivery               PIC X(35).                  
                   15 termsOfDeliveryLocat-length   PIC S9999 COMP-5            
            SYNC.                                                               
                   15 termsOfDeliveryLocation       PIC X(35).                  
                 12 dataRouting-length            PIC S9999 COMP-5              
            SYNC.                                                               
                 12 dataRouting                   PIC X(20).                    
                                                                                
                 12 facingDC-num                  PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 facingDC.                                                   
                   15 facingDC2-length              PIC S9999 COMP-5            
            SYNC.                                                               
                   15 facingDC2                     PIC X(2).                   
                                                                                
                 12 issuingApplication-num        PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 issuingApplication.                                         
                   15 issuingApplication2-length    PIC S9999 COMP-5            
            SYNC.                                                               
                   15 issuingApplication2           PIC X(14).                  
                                                                                
                 12 issuingSite-num               PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 issuingSite.                                                
                   15 issuingSite2-length           PIC S9999 COMP-5            
            SYNC.                                                               
                   15 issuingSite2                  PIC X(3).                   
                 12 language-length               PIC S9999 COMP-5              
            SYNC.                                                               
                 12 language                      PIC X(5).                     
                                                                                
                 12 orderType-num                 PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 orderType.                                                  
                   15 orderType2-length             PIC S9999 COMP-5            
            SYNC.                                                               
                   15 orderType2                    PIC X(3).                   
                 12 parcel.                                                     
                   15 dangerousGoods                PIC X DISPLAY.              
                   15 dimensions.                                               
                                                                                
                     18 grossVolumeDm3-num            PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 grossVolumeDm3                PIC S9(9)V9(4)            
            COMP-3.                                                             
                                                                                
                     18 grossWeightKg-num             PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 grossWeightKg                 PIC S9(9)V9(4)            
            COMP-3.                                                             
                                                                                
                     18 heightMm-num                  PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 heightMm                      PIC S9(9)V9(4)            
            COMP-3.                                                             
                                                                                
                     18 lengthMm-num                  PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 lengthMm                      PIC S9(9)V9(4)            
            COMP-3.                                                             
                                                                                
                     18 packagingTareWeightKg-num     PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 packagingTareWeightKg         PIC S9(9)V9(4)            
            COMP-3.                                                             
                                                                                
                     18 widthMm-num                   PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 widthMm                       PIC S9(9)V9(4)            
            COMP-3.                                                             
                                                                                
                   15 packageType-num               PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 packageType.                                              
                     18 packageType2-length           PIC S9999 COMP-5          
            SYNC.                                                               
                     18 packageType2                  PIC X(1).                 
                   15 packagingCode-length          PIC S9999 COMP-5            
            SYNC.                                                               
                   15 packagingCode                 PIC X(8).                   
                   15 packagingType-length          PIC S9999 COMP-5            
            SYNC.                                                               
                   15 packagingType                 PIC X(20).                  
                                                                                
                   15 parcelContents2-num           PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 parcelContents OCCURS 250.                                
                     18 countryOfOrigin-length        PIC S9999 COMP-5          
            SYNC.                                                               
                     18 countryOfOrigin               PIC X(2).                 
                     18 dangerousGoods2               PIC X DISPLAY.            
                                                                                
                     18 dangerousGoodsDetails2-num    PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 dangerousGoodsDetails OCCURS 10.                        
                       21 dgClass-length                PIC S9999               
            COMP-5 SYNC.                                                        
                       21 dgClass                       PIC X(3).               
                                                                                
                       21 dgDescription-num             PIC S9(9)               
            COMP-5 SYNC.                                                        
                                                                                
                       21 dgDescription.                                        
                         24 dgDescription2-length         PIC S9999             
            COMP-5 SYNC.                                                        
                         24 dgDescription2                PIC X(225).           
                       21 dgPulsPSN-length              PIC S9999               
            COMP-5 SYNC.                                                        
                       21 dgPulsPSN                     PIC X(3).               
                                                                                
                       21 dgUNCode-num                  PIC S9(9)               
            COMP-5 SYNC.                                                        
                                                                                
                       21 dgUNCode.                                             
                         24 dgUNCode2-length              PIC S9999             
            COMP-5 SYNC.                                                        
                         24 dgUNCode2                     PIC X(10).            
                       21 language2-length              PIC S9999               
            COMP-5 SYNC.                                                        
                       21 language2                     PIC X(5).               
                                                                                
                       21 modeOfTransport-num           PIC S9(9)               
            COMP-5 SYNC.                                                        
                                                                                
                       21 modeOfTransport.                                      
                         24 modeOfTransport2-length       PIC S9999             
            COMP-5 SYNC.                                                        
                         24 modeOfTransport2              PIC X(2).             
                                                                                
                       21 dgNotes-num                   PIC S9(9)               
            COMP-5 SYNC.                                                        
                                                                                
                       21 dgNotes.                                              
                         24 dgNotes2-length               PIC S9999             
            COMP-5 SYNC.                                                        
                         24 dgNotes2                      PIC X(225).           
                       21 filler                        PIC X(1).               
                     18 hsCode-length                 PIC S9999 COMP-5          
            SYNC.                                                               
                     18 hsCode                        PIC X(9).                 
                     18 orderIdentifier-length        PIC S9999 COMP-5          
            SYNC.                                                               
                     18 orderIdentifier               PIC X(23).                
                                                                                
                     18 orderRegistrationTimeX-num    PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 orderRegistrationTimeX.                                 
                       21 orderRegistrationTim-length   PIC S9999               
            COMP-5 SYNC.                                                        
                       21 orderRegistrationTimeX2       PIC X(20).              
                     18 orderLineReference-length     PIC S9999 COMP-5          
            SYNC.                                                               
                     18 orderLineReference            PIC X(10).                
                                                                                
                     18 partAdditionalDescript-num    PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 partAdditionalDescription.                              
                       21 partAdditionalDescri-length   PIC S9999               
            COMP-5 SYNC.                                                        
                       21 partAdditionalDescription2    PIC X(25).              
                     18 partDescription-length        PIC S9999 COMP-5          
            SYNC.                                                               
                     18 partDescription               PIC X(50).                
                                                                                
                     18 partGrossVolumeDm3-num        PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 partGrossVolumeDm3            PIC S9(9)V9(4)            
            COMP-3.                                                             
                                                                                
                     18 partGrossWeightKg-num         PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 partGrossWeightKg             PIC S9(9)V9(4)            
            COMP-3.                                                             
                     18 partNumber-length             PIC S9999 COMP-5          
            SYNC.                                                               
                     18 partNumber                    PIC X(9).                 
                     18 partOrderValue.                                         
                       21 amount                        PIC S9(9)V9(4)          
            COMP-3.                                                             
                       21 Xcurrency-length              PIC S9999               
            COMP-5 SYNC.                                                        
                       21 Xcurrency                     PIC X(3).               
                     18 quantity                      COMP-2 SYNC.              
                                                                                
                     18 quantityUnitOfMeasure-num     PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 quantityUnitOfMeasure.                                  
                       21 quantityUnitOfMeasur-length   PIC S9999               
            COMP-5 SYNC.                                                        
                       21 quantityUnitOfMeasure2        PIC X(15).              
                     18 filler                        PIC X(3).                 
                   15 parcelIdentifier-length       PIC S9999 COMP-5            
            SYNC.                                                               
                   15 parcelIdentifier              PIC X(22).                  
                                                                                
                 12 plannedDispatchTime-num       PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 plannedDispatchTime.                                        
                   15 plannedDispatchTime2-length   PIC S9999 COMP-5            
            SYNC.                                                               
                   15 plannedDispatchTime2          PIC X(24).                  
                                                                                
                 12 processingParties2-num        PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 processingParties OCCURS 5.                                 
                                                                                
                   15 additionalDeliveryDeta-num    PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 additionalDeliveryDetails OCCURS 3.                       
                     18 additionalDeliveryDe-length   PIC S9999 COMP-5          
            SYNC.                                                               
                     18 additionalDeliveryDetails2    PIC X(255).               
                     18 filler                        PIC X(1).                 
                                                                                
                   15 address2-num                  PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 Xaddress.                                                 
                                                                                
                     18 additionalAddressInfor-num    PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 additionalAddressInformation OCCURS 3.                  
                       21 additionalAddressInf-length   PIC S9999               
            COMP-5 SYNC.                                                        
                       21 additionalAddressInformation2  PIC X(35).             
                       21 filler                        PIC X(1).               
                     18 city-length                   PIC S9999 COMP-5          
            SYNC.                                                               
                     18 city                          PIC X(25).                
                                                                                
                     18 rawCity-num                   PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 rawCity.                                                
                       21 rawCity2-length               PIC S9999               
            COMP-5 SYNC.                                                        
                       21 rawCity2                      PIC X(35).              
                     18 country-length                PIC S9999 COMP-5          
            SYNC.                                                               
                     18 country                       PIC X(2).                 
                     18 name-length                   PIC S9999 COMP-5          
            SYNC.                                                               
                     18 name                          PIC X(35).                
                                                                                
                     18 name2-num                     PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 name2.                                                  
                       21 name22-length                 PIC S9999               
            COMP-5 SYNC.                                                        
                       21 name22                        PIC X(35).              
                                                                                
                     18 position2-num                 PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 Xposition.                                              
                       21 latitude                      COMP-2 SYNC.            
                       21 longitude                     COMP-2 SYNC.            
                     18 postalCode-length             PIC S9999 COMP-5          
            SYNC.                                                               
                     18 postalCode                    PIC X(10).                
                     18 state-length                  PIC S9999 COMP-5          
            SYNC.                                                               
                     18 state                         PIC X(20).                
                     18 streetAddress-length          PIC S9999 COMP-5          
            SYNC.                                                               
                     18 streetAddress                 PIC X(35).                
                                                                                
                     18 streetAddress2-num            PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 streetAddress2.                                         
                       21 streetAddress22-length        PIC S9999               
            COMP-5 SYNC.                                                        
                       21 streetAddress22               PIC X(35).              
                   15 addressType-length            PIC S9999 COMP-5            
            SYNC.                                                               
                   15 addressType                   PIC X(20).                  
                                                                                
                   15 contact2-num                  PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 contact.                                                  
                                                                                
                     18 email-num                     PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 email.                                                  
                       21 email2-length                 PIC S9999               
            COMP-5 SYNC.                                                        
                       21 email2                        PIC X(60).              
                     18 name3-length                  PIC S9999 COMP-5          
            SYNC.                                                               
                     18 name3                         PIC X(40).                
                                                                                
                     18 phone-num                     PIC S9(9) COMP-5          
            SYNC.                                                               
                                                                                
                     18 phone.                                                  
                       21 phone2-length                 PIC S9999               
            COMP-5 SYNC.                                                        
                       21 phone2                        PIC X(20).              
                                                                                
                   15 finalDeliveryPoint-num        PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 finalDeliveryPoint.                                       
                     18 finalDeliveryPoint2-length    PIC S9999 COMP-5          
            SYNC.                                                               
                     18 finalDeliveryPoint2           PIC X(60).                
                                                                                
                   15 locationAlias-num             PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 locationAlias.                                            
                     18 locationAlias2-length         PIC S9999 COMP-5          
            SYNC.                                                               
                     18 locationAlias2                PIC X(15).                
                                                                                
                   15 timeZone-num                  PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 timeZone.                                                 
                     18 timeZone2-length              PIC S9999 COMP-5          
            SYNC.                                                               
                     18 timeZone2                     PIC X(10).                
                                                                                
                   15 vatCode-num                   PIC S9(9) COMP-5            
            SYNC.                                                               
                                                                                
                   15 vatCode.                                                  
                     18 vatCode2-length               PIC S9999 COMP-5          
            SYNC.                                                               
                     18 vatCode2                      PIC X(17).                
                   15 filler                        PIC X(5).                   
                                                                                
                 12 requestedArrivalTime-num      PIC S9(9) COMP-5              
            SYNC.                                                               
                                                                                
                 12 requestedArrivalTime.                                       
                   15 requestedArrivalTime-length   PIC S9999 COMP-5            
            SYNC.                                                               
                   15 requestedArrivalTime2         PIC X(24).                  
                 12 transportProcess-length       PIC S9999 COMP-5              
            SYNC.                                                               
                 12 transportProcess              PIC X(20).                    
                                                                                
                                                                                
