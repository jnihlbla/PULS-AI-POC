000100 01  REQU-WF0287I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0287             
000300*                                 DOCUMENT NUMBER SERIE ASSIGNMEN         
000400*                                 T, MAINTENANCE                          
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
001700     03 REQU-IDLANDX3-KEY    PIC X(3).                                    
001800*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
001900*                                 3-LETTER CODE FOR COUNTRY.              
002000     03 REQU-IDLOPNR-KEY     PIC 9(3).                                    
002100*                                 LÖPNUMMER                               
002200*                                 SEQUENCE NUMBER                         
002300*** END OF VILMAII-COPY LENGTH= 32 BYTES                                  
