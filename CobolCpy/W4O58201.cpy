000100 01  MOD-W4O58201.                                                        
000200*                                 MOD-COPYTEXT TILL W40582                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDTRP-IN.                                                     
000800*                                 TRANSPORTIDENTITET                      
000900        05 MOD-IDTRPLOS      PIC X(3).                                    
001000*                                 TRANSPORTLÖSNING                        
001100        05 MOD-IDTRPVAR      PIC X(2).                                    
001200*                                 TRANSPORTLÖSNINGSGRUPP                  
001300     03 MOD-IDTRP-UT.                                                     
001400*                                 TRANSPORTIDENTITET                      
001500        05 MOD-IDTRPLOS      PIC X(3).                                    
001600*                                 TRANSPORTLÖSNING                        
001700        05 MOD-IDTRPVAR      PIC X(2).                                    
001800*                                 TRANSPORTLÖSNINGSGRUPP                  
001900     03 MOD-BETRPDST-IN      PIC X(15).                                   
002000*                                 TRANSPORTDESTINATION                    
002100     03 MOD-BETRPDST-UT      PIC X(15).                                   
002200*                                 TRANSPORTDESTINATION                    
002300     03 MOD-IDDC-IN          PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500     03 MOD-IDDC-UT          PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 MOD-IDTRP-NEXT.                                                   
002800*                                 TRANSPORTIDENTITET                      
002900        05 MOD-IDTRPLOS      PIC X(3).                                    
003000*                                 TRANSPORTLÖSNING                        
003100        05 MOD-IDTRPVAR      PIC X(2).                                    
003200*                                 TRANSPORTLÖSNINGSGRUPP                  
003300     03 MOD-TITRPAVG-NEXT    PIC X(7).                                    
003400*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
003500     03 MOD-IDTRP-ENTER.                                                  
003600*                                 TRANSPORTIDENTITET                      
003700        05 MOD-IDTRPLOS      PIC X(3).                                    
003800*                                 TRANSPORTLÖSNING                        
003900        05 MOD-IDTRPVAR      PIC X(2).                                    
004000*                                 TRANSPORTLÖSNINGSGRUPP                  
004100     03 MOD-TITRPAVG-ENTER   PIC X(7).                                    
004200*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
004300     03 MOD-RAD              OCCURS 14 TIMES.                             
004400        05 MOD-IDTRP-RAD.                                                 
004500*                                 TRANSPORTIDENTITET                      
004600           07 MOD-IDTRPLOS   PIC X(3).                                    
004700*                                 TRANSPORTLÖSNING                        
004800           07 MOD-IDTRPVAR   PIC X(2).                                    
004900*                                 TRANSPORTLÖSNINGSGRUPP                  
005000        05 MOD-KVLASTTI-RAD  PIC Z9.9(2).                                 
005100*                                 TID DET TAR ATT LASTA                   
005200        05 MOD-KVADMFL-RAD   PIC Z9.9(2).                                 
005300*                                 ADM-TID FÖRE LASTNING                   
005400        05 MOD-KVADMEL-RAD   PIC Z9.9(2).                                 
005500*                                 ADM-TID EFTER LASTNING                  
005600        05 MOD-TITRPAVG-RAD  PIC Z(2)BZBZ9B9(2).                          
005700*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
005800        05 MOD-KDFARLIG-RAD  PIC 9.                                       
005900*                                 KOD FÖR FARLIGT GODS                    
006000        05 MOD-VLTRPMIN-RAD  PIC Z(3).                                    
006100*                                 MINSTA TILLÅTNA VOLYM I M3              
006200        05 MOD-BETRPFIR-RAD  PIC X(15).                                   
006300*                                 TRANSPORTFIRMANS NAMN                   
006400     03 MOD-TEMFSINF         PIC X(55).                                   
006500*                                 INFORMATIONSMEDDELANDE                  
006600*** END OF VILMAII-COPY LENGTH= 853 BYTES                                 
