000100 01  4312-WDGX4312.                                                       
000200*                                 ORDER UNDER ARBETE                      
000300*                                 ORDERVIS PACKNINGSRAPPORTERING          
000400*                                 NYCKEL: WDGXKEY                         
000500*                                         (IDPRODNR, LOWVALUE)            
000600*                                 SÖKBEGREPP: IDUSER                      
000700     03 4312-IDPRODNR        PIC S9(7)           COMP-3.                  
000800*                                 PRODUKTIONSNUMMER                       
000900     03 4312-LOWVALUE        PIC X(6).                                    
001000     03 4312-IDUSER          PIC X(8).                                    
001100*                                 ANVÄNDARIDENTITET I RACF                
001200     03 4312-IDDISTR         PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400     03 4312-IDKUNDNR        PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600     03 4312-IDKUNDRF        PIC X(10).                                   
001700*                                 KUNDENS REFERENS                        
001800     03 4312-KDORDKL         PIC S9              COMP-3.                  
001900*                                 ORDERKLASS                              
002000     03 4312-KVORDRAD        PIC S9(5)           COMP-3.                  
002100*                                 ANTAL ORDERRADER                        
002200     03 4312-KDFRAKT         PIC S9(3)           COMP-3.                  
002300*                                 FRAKTSÄTT C1-C2 TILL KUND               
002400     03 4312-TIDATUM         PIC S9(7)           COMP-3.                  
002500*                                 DATUM ENLIGT KDDATFORM                  
002600     03 4312-TIKLOCK         PIC S9(9)           COMP-3.                  
002700*                                 KLOCKSLAG (HHMMSSTH)                    
002800     03 4312-IDTRANS         PIC X(4).                                    
002900*                                 TRANSAKTIONSIDENTITET                   
003000     03 4312-KDBEHAND-GRUND  PIC S9              COMP-3.                  
003100*                                 BEHANDLINGSKOD                          
003200*                                  0 = EJ AKTUELL                         
003300*                                  1 = SKALL BEHANDLAS                    
003400*                                  2 = FÄRDIG BEHANDLAD                   
003500*                                  3 = PÅBÖRJAD BEHANDLING                
003600     03 4312-KDBEHAND-AVVIK  PIC S9              COMP-3.                  
003700*                                 BEHANDLINGSKOD                          
003800*                                  0 = EJ AKTUELL                         
003900*                                  1 = SKALL BEHANDLAS                    
004000*                                  2 = FÄRDIG BEHANDLAD                   
004100*                                  3 = PÅBÖRJAD BEHANDLING                
004200     03 4312-KDBEHAND-RAD    PIC S9              COMP-3.                  
004300*                                 BEHANDLINGSKOD                          
004400*                                  0 = EJ AKTUELL                         
004500*                                  1 = SKALL BEHANDLAS                    
004600*                                  2 = FÄRDIG BEHANDLAD                   
004700*                                  3 = PÅBÖRJAD BEHANDLING                
004800     03 4312-KDBEHAND-DEL    PIC S9              COMP-3.                  
004900*                                 BEHANDLINGSKOD                          
005000*                                  0 = EJ AKTUELL                         
005100*                                  1 = SKALL BEHANDLAS                    
005200*                                  2 = FÄRDIG BEHANDLAD                   
005300*                                  3 = PÅBÖRJAD BEHANDLING                
005400     03 4312-KDBEHAND-URS    PIC S9              COMP-3.                  
005500*                                 BEHANDLINGSKOD                          
005600*                                  0 = EJ AKTUELL                         
005700*                                  1 = SKALL BEHANDLAS                    
005800*                                  2 = FÄRDIG BEHANDLAD                   
005900*                                  3 = PÅBÖRJAD BEHANDLING                
006000     03 4312-KDBEHAND-KOL    PIC S9              COMP-3.                  
006100*                                 BEHANDLINGSKOD                          
006200*                                  0 = EJ AKTUELL                         
006300*                                  1 = SKALL BEHANDLAS                    
006400*                                  2 = FÄRDIG BEHANDLAD                   
006500*                                  3 = PÅBÖRJAD BEHANDLING                
006600*** END COPY WDGX4312C0  LENGTH=60                                        
