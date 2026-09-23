000100 01  REQU-WF0282I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0282             
000300*                                 PAYMENT INSTRUCTIONS LOCATE             
000400     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 REQU-KDFINDOC-KEY    PIC X(4).                                    
000800*                                 TYP FINANSIELLT DOKUMENT                
000900*                                 FINANCIAL DOCUMENT TYPE                 
001000     03 REQU-KDVALISO-KEY    PIC X(3).                                    
001100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001200*                                 CURRENCY CODE BY ISO-STANDARD.          
001300     03 REQU-KDPARTTY-KEY    PIC X(3).                                    
001400*                                 TYP AV BETALARE                         
001500*                                 TYPE OF FIN.CUSTOMER                    
001600     03 REQU-KDPARTGR-KEY    PIC X(15).                                   
001700*                                 GRUPP AV BETALARE                       
001800*                                 FIN.CUSTOMER GROUP                      
001900*** END OF VILMAII-COPY LENGTH= 29 BYTES                                  
