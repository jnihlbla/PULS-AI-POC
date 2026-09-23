000100 01  REQU-W60136I1.                                                       
000200*                                 COPYTEXT FÖR REQU                       
000300*                                 W60136I1                                
000400     03 REQU-IDLOPNRM-KEY    PIC X(9).                                    
000500*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000600*                                 (0VVDLLLLK)                             
000700     03 REQU-IDDC-KEY        PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 REQU-FLPREPRA        PIC X.                                       
001000*                                 FÖRBEHANDLINGSRAPPORTSFLAGGA            
001100     03 REQU-KVAVIS-MOT      PIC X(6).                                    
001200*                                 AVISERAT ANTAL                          
001300     03 REQU-IDANSTNR        PIC X(5).                                    
001400*                                 ANSTÄLLNINGSNUMMER                      
001500     03 REQU-FLSVS           PIC X.                                       
001600*                                 ANGER GENERELLT OM NÅGOT AVSER          
001700*                                 SVS                                     
001800     03 REQU-ADINLOMR-PRT    PIC X(4).                                    
001900*                                 PRINTERPLACERING                        
002000     03 REQU-IDSPRAK         PIC X(2).                                    
002100*                                 2-STÄLLIG ISO SPRÅKKOD                  
002200     03 REQU-KVRADER         PIC 9(5).                                    
002300*                                 ANTAL RADER                             
002400     03 REQU-LINE            OCCURS 500 TIMES.                            
002500        05 REQU-KDFLETI-LINE PIC X(2).                                    
002600*                                 FLAGGA/ETIKETTVAL                       
002700        05 REQU-KVFLETI-LINE PIC X(2).                                    
002800*                                 ANTAL FLAGGOR EL ETIKETTER              
002900        05 REQU-KVINLART-LINE1                                            
003000                             PIC X(6).                                    
003100*                                 ANTAL I PARTIRAD                        
003200        05 REQU-KDKLIPRI-LINE                                             
003300                             PIC X.                                       
003400*                                 PRIORITETSKOD KOLLI                     
003500        05 REQU-FLSATS-LINE  PIC X.                                       
003600*                                 SATSARTIKEL                             
003700        05 REQU-FLPREPKL-LINE                                             
003800                             PIC X.                                       
003900*                                 FÖRPACKAT?                              
004000*** END OF VILMAII-COPY LENGTH= 6535 BYTES                                
