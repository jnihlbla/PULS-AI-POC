000100 01  W9O10301.                                                            
000200*                                 COPYTEXT FÖR MOD W9O10301               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 TRANSAKTIONSIDENTITET                   
000500     03 MESSAGE-RAD1         PIC X(40).                                   
000600*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000700     03 IDARTNR-IN           PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 IDARTNR-UT           PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 STRECK               PIC X.                                       
001200     03 REKSIFFR             PIC X.                                       
001300*                                 KONTROLLSIFFRA                          
001400     03 IDKORTNR-ANT         PIC X(3).                                    
001500*                                 KORTNUMMER                              
001600*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
001700     03 IDARTNR-ERS-ANT      PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 AREA-OUTPUT.                                                      
002000*                                                                         
002100        05 RAD               OCCURS 13 TIMES                              
002200                             INDEXED IX.                                  
002300*                                                                         
002400           07 IDARTNR        PIC Z(8)9.                                   
002500*                                 ARTIKELNUMMER                           
002600           07 DIERS-ERS      PIC -(4)9.9(3).                              
002700*                                 ERSATT ARTIKELANTAL                     
002800           07 KDERS          PIC Z(3)9(2).                                
002900*                                 ERSÄTTNINGSKOD C1     KDERS-002         
003000           07 DIERS-TILLK    PIC -(7)9.9(3).                              
003100*                                                 DIERS-TILLK-003         
003200*                                 ANTAL FÖR TILLKOMMANDE                  
003300           07 TEXT-KOMMENTAR PIC X(32).                                   
003400     03 MESSAGE-RAD23        PIC X(79).                                   
003500*                                 MEDDELANDEFÄLT PÅ RAD 23                
003600*** END COPY W9O10301C0  LENGTH=1026                                      
