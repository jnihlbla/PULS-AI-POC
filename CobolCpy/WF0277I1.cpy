000100 01  REQU-WF0277I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0277             
000300*                                 ERRONEUS BUNDLES LOCATE                 
000400     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 REQU-IDSYSTEM-SEND   PIC X(4).                                    
000800*                                 VOLVO SÄNDANDE SYSTEM                   
000900*                                 VOLVO SENDING SYSTEM                    
001000     03 REQU-FLASC           PIC X.                                       
001100*                                 ALLMÄN FLAGGA                           
001200*                                 GENERAL FLAG                            
001300     03 REQU-DADATUM         PIC X(8).                                    
001400*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001500*                                 REGISTRATION DATE (YYYYMMDD)            
001600     03 REQU-TIHHMMSS        PIC 9(6).                                    
001700*                                 TIM - MIN - SEK   (HHMMSS)              
001800*                                 HOUR - MINUTE - SEC (HHMMSS)            
001900*** END OF VILMAII-COPY LENGTH= 23 BYTES                                  
