000100 01  REQU-WF0286I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0286             
000300*                                 DOCUMENT NUMBER SERIE ASSIGNMEN         
000400*                                 T, LOCATE                               
000500     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000600*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000700*                                 LEGAL SELLER IDENTITY                   
000800     03 REQU-KDFINDOC-KEY    PIC X(4).                                    
000900*                                 TYP FINANSIELLT DOKUMENT                
001000*                                 FINANCIAL DOCUMENT TYPE                 
001100     03 REQU-KDPARTTY-KEY    PIC X(3).                                    
001200*                                 TYP AV BETALARE                         
001300*                                 TYPE OF FIN.CUSTOMER                    
001400     03 REQU-KDPARTGR-KEY    PIC X(15).                                   
001500*                                 GRUPP AV BETALARE                       
001600*                                 FIN.CUSTOMER GROUP                      
001700*** END OF VILMAII-COPY LENGTH= 26 BYTES                                  
