000100 01  REQU-WF0283I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0283             
000300*                                 CURRENCY MAINTENANCE                    
000400     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 REQU-KDVALISO-KEY    PIC X(3).                                    
000800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
000900*                                 CURRENCY CODE BY ISO-STANDARD.          
001000     03 REQU-DASTADAT-KEY    PIC 9(8).                                    
001100*                                 GENERELLT STARTDATUM                    
001200*                                 GENERAL START DATE                      
001300     03 REQU-REVALUTA        PIC 9(3).                                    
001400*                                 OMRÄKNINGSTAL FÖR VALUTA                
001500*                                 CONVERT VALUE FOR CURRENCY CODE         
001600     03 REQU-PRKURS          PIC X(12).                                   
001700*                                 VALUTAKURS                              
001800*                                 CURRENCY EXCHANGE RATE                  
001900*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
