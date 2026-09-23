000100 01  REQU-W60133I1.                                                       
000200*                                 COPYTEXT FÖR REQU                       
000300*                                 W6I13301                                
000400     03 REQU-ADINLOMR-PRT    PIC X(4).                                    
000500*                                 PRINTERPLACERING                        
000600     03 REQU-IDLEVNR-KOLLI-KEY                                            
000700                             PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER KOLLI                  
000900     03 REQU-IDOKOLLI-KEY    PIC X(9).                                    
001000*                                 ODETTE KOLLINUMMER                      
001100     03 REQU-IDLOPNRM-KEY    PIC X(9).                                    
001200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001300*                                 (0VVDLLLLK)                             
001400     03 REQU-IDDC-KEY        PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 REQU-IDLOPNRM-START  PIC X(9).                                    
001700*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001800*                                 (0VVDLLLLK)                             
001900     03 REQU-INPUT.                                                       
002000        05 REQU-KDCMDVAL     PIC X(3).                                    
002100*                                 GENERELL KOMMANDOKOD                    
002200        05 REQU-KVINLART     PIC X(6).                                    
002300*                                 ANTAL I PARTIRAD                        
002400        05 REQU-ADINLOMR     PIC X(4).                                    
002500*                                 INLEVERANSOMRÅDE                        
002600        05 REQU-IDINLVGN     PIC X(3).                                    
002700*                                 VAGNSIDENTITET                          
002800        05 REQU-ADINLOMR-NXT PIC X(4).                                    
002900*                                 INLEVERANSOMRÅDE NÄSTA                  
003000        05 REQU-KDKLIPRI     PIC X.                                       
003100*                                 PRIORITETSKOD KOLLI                     
003200        05 REQU-FLSATS       PIC X.                                       
003300*                                 SATSARTIKEL                             
003400        05 REQU-IDUSER-IN    PIC X(8).                                    
003500*                                 ANVÄNDARENS SÄKERHETS ID                
003600     03 REQU-KDPRTVAL        PIC X.                                       
003700*                                 PRINTER-VAL KOD                         
003800     03 REQU-KVRADER         PIC 9(5).                                    
003900*                                 ANTAL RADER                             
004000     03 REQU-IDSPRAK         PIC X(2).                                    
004100*                                 2-STÄLLIG ISO SPRÅKKOD                  
004200*** END OF VILMAII-COPY LENGTH= 76 BYTES                                  
