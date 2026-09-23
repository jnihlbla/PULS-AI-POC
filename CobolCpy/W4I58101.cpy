000100 01  MID-W4I58101.                                                        
000200*                                 MID-COPYTEXT FÖR W4058100               
000300     03 MID-IDTRP-IN.                                                     
000400*                                 TRANSPORTIDENTITET                      
000500        05 MID-IDTRPLOS      PIC X(3).                                    
000600*                                 TRANSPORTLÖSNING                        
000700        05 MID-IDTRPVAR      PIC X(2).                                    
000800*                                 TRANSPORTLÖSNINGSGRUPP                  
000900     03 MID-IDTRP-UT.                                                     
001000*                                 TRANSPORTIDENTITET                      
001100        05 MID-IDTRPLOS      PIC X(3).                                    
001200*                                 TRANSPORTLÖSNING                        
001300        05 MID-IDTRPVAR      PIC X(2).                                    
001400*                                 TRANSPORTLÖSNINGSGRUPP                  
001500     03 MID-BETRPDST-IN      PIC X(15).                                   
001600*                                 TRANSPORTDESTINATION                    
001700     03 MID-BETRPDST-UT      PIC X(15).                                   
001800*                                 TRANSPORTDESTINATION                    
001900     03 MID-IDDC-IN          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MID-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MID-TITRPAVG-NEXT    PIC 9(7).                                    
002400*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
002500     03 MID-TITRPAVG-ENTER   PIC 9(7).                                    
002600*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
002700     03 MID-UPPDAT-TRPID.                                                 
002800        05 MID-KVLASTTI-IN   PIC X(5).                                    
002900*                                 TID DET TAR ATT LASTA                   
003000        05 MID-KVADMFL-IN    PIC X(5).                                    
003100*                                 ADM-TID FÖRE LASTNING                   
003200        05 MID-KVADMEL-IN    PIC X(5).                                    
003300*                                 ADM-TID EFTER LASTNING                  
003400        05 MID-BETRPDST      PIC X(15).                                   
003500*                                 TRANSPORTDESTINATION                    
003600        05 MID-FLTABORT-TRP-IN                                            
003700                             PIC X.                                       
003800*                                 BORTTAGSFLAGGA                          
003900     03 MID-UPPDAT-TRPAVG.                                                
004000        05 MID-TITRPAVG-IN   PIC 9(7).                                    
004100*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
004200        05 MID-BETRPFIR-IN   PIC X(15).                                   
004300*                                 TRANSPORTFIRMANS NAMN                   
004400        05 MID-KDFARLIG-IN   PIC 9.                                       
004500*                                 KOD FÖR FARLIGT GODS                    
004600        05 MID-VLTRPMIN-IN   PIC 9(3).                                    
004700*                                 MINSTA TILLÅTNA VOLYM I M3              
004800        05 MID-FLTABORT-AVG-IN                                            
004900                             PIC X.                                       
005000*                                 BORTTAGSFLAGGA                          
005100*** END OF VILMAII-COPY LENGTH= 116 BYTES                                 
