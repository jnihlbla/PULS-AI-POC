000100 01  W9O10201.                                                            
000200*                                 COPYTEXT FÖR MOD W9O10201               
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
001700     03 AREA-OUTPUT.                                                      
001800*                                                                         
001900        05 KDERS             PIC Z9(2).                                   
002000*                                 ERSÄTTNINGSKOD                          
002100        05 TIERSDAT          PIC Z(6).                                    
002200*                                                    TIERSDAT-002         
002300*                                 DEF. DATUM FÖR ERSÄTTNING               
002400        05 DIERS-ERS         PIC Z(3)9.9(3).                              
002500*                                 ERSATT ARTIKELANTAL                     
002600        05 RAD               OCCURS 27 TIMES                              
002700                             INDEXED IX.                                  
002800*                                                                         
002900           07 IDARTNR-TILLK  PIC Z(8)9.                                   
003000*                                 TILLKOMMANDE ARTIKELNUMMER              
003100           07 DIERS-TILLK    PIC -(6)9.9(3).                              
003200*                                                 DIERS-TILLK-002         
003300*                                 ANTAL FÖR TILLKOMMANDE                  
003400     03 MESSAGE-RAD23        PIC X(79).                                   
003500*                                 MEDDELANDEFÄLT PÅ RAD 23                
003600*** END COPY W9O10201C0  LENGTH=703                                       
