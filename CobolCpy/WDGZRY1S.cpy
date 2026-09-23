000100 01  RY1S-WDGZRY1S.                                                       
000200*                                 RY1, DEL 2 LÄGGS I SORTAREAN.           
000300*                                 SKAPAS VID AVVIKELSE I PACK-            
000400*                                 NINGSRAPPORTERINGEN ELLER               
000500*                                 ANNULLATIONER. ANVÄNDS VID              
000600*                                 SKAPANDE AV TRANSAKTIONER TILL          
000700*                                 ÖVRIGA SYSTEM.                          
000800     03 RY1S-IDDISTR         PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000     03 RY1S-IDKUNDNR        PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200     03 RY1S-FLVORKO         PIC X.                                       
001300*                                 VOR-KÖ FLAGGA                           
001400     03 RY1S-FLFORBI         PIC X.                                       
001500*                                 FÖRBIORDERFLAGGA                        
001600     03 RY1S-FLOVRLEV        PIC X.                                       
001700*                                 ÖVERLEVERANS                            
001800     03 RY1S-KDPRODSL        PIC S9(3)           COMP-3.                  
001900*                                 PRODUKTSLAG                             
002000     03 RY1S-FILLER          PIC X(5).                                    
002100     03 RY1S-IDSYSTEM        PIC X(4).                                    
002200*                                 SKAPANDE SYSTEMNUMMER                   
002300     03 RY1S-FLLSBOK         PIC X.                                       
002400*                                 LAGERAVBOKNING                          
002500     03 RY1S-FLORDSPE        PIC X.                                       
002600*                                 SPECIALORDERFLAGGA                      
002700     03 RY1S-KDFRAKT         PIC S9(3)           COMP-3.                  
002800*                                 FRAKTSÄTT C1-C2 TILL KUND               
002900     03 RY1S-KDORDKL         PIC S9              COMP-3.                  
003000*                                 ORDERKLASS                              
003100     03 RY1S-KVANNANT        PIC S9(7)           COMP-3.                  
003200*                                 ANNULLERAT ANTAL ARTIKLAR               
003300     03 RY1S-KVSLATT         PIC S9(7)           COMP-3.                  
003400*                                 BERÄKNAD SLATTGRÄNS                     
003500     03 RY1S-IDDC            PIC X(2).                                    
003600*                                 IDENTIFIERARE LAGER                     
003700*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
