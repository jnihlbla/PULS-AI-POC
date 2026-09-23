000100 01  MID-W4I36901.                                                        
000200*                                 MID-COPYTEXT FÖR W40369                 
000300     03 MID-IDPRC-IN.                                                     
000400*                                 PRODUKTIONSKANAL                        
000500        05 MID-IDPRCBAS      PIC X(3).                                    
000600*                                 PRC-BAS                                 
000700        05 MID-IDPRCVAR      PIC X.                                       
000800*                                 PRC-VARIANT                             
000900     03 MID-IDPRC-UT.                                                     
001000*                                 PRODUKTIONSKANAL                        
001100        05 MID-IDPRCBAS      PIC X(3).                                    
001200*                                 PRC-BAS                                 
001300        05 MID-IDPRCVAR      PIC X.                                       
001400*                                 PRC-VARIANT                             
001500     03 MID-DATUM-IN         PIC 9(6).                                    
001600*                                 DATUM ENLIGT KDDATFORM                  
001700     03 MID-DATUM-UT         PIC 9(6).                                    
001800*                                 DATUM ENLIGT KDDATFORM                  
001900     03 MID-IDDC-IN          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MID-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MID-DATUM-NEXT       PIC 9(6).                                    
002400*                                 DATUM ENLIGT KDDATFORM                  
002500     03 MID-DATUM-ENTER      PIC 9(6).                                    
002600*                                 DATUM ENLIGT KDDATFORM                  
002700     03 MID-RAD.                                                          
002800        05 MID-DATUM         PIC 9(6).                                    
002900*                                 DATUM ENLIGT KDDATFORM                  
003000        05 MID-STAPAC-IN     PIC X(5).                                    
003100*                                 ARBETSDAGENS BÖRJAN (PACKNING)          
003200        05 MID-STOPAC-IN     PIC X(5).                                    
003300*                                 ARBETSDAGENS SLUT (PACKNING)            
003400        05 MID-STAADM-IN     PIC X(5).                                    
003500*                                 ARBETSDAGENS BÖRJAN (ORDERKONT)         
003600        05 MID-STOADM-IN     PIC X(5).                                    
003700*                                 ARBETSDAGENS SLUT (ORDERKONT)           
003800        05 MID-STALAST-IN    PIC X(5).                                    
003900*                                 ARBETSDAGENS BÖRJAN (LASTNING)          
004000        05 MID-STOLAST-IN    PIC X(5).                                    
004100*                                 ARBETSDAGENS SLUT (LASTNING)            
004200*** END OF VILMAII-COPY LENGTH= 72 BYTES                                  
