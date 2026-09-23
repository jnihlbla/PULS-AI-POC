000100 01  MID-W4I36801.                                                        
000200*                                 MID-COPYTEXT FÖR W40368                 
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
001500     03 MID-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MID-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MID-TIDATUM-IN       PIC 9(6).                                    
002000*                                 DATUM ENLIGT KDDATFORM                  
002100     03 MID-TIDATUM-UT       PIC 9(6).                                    
002200*                                 DATUM ENLIGT KDDATFORM                  
002300     03 MID-TIUPDATE-IN      PIC X(6).                                    
002400*                                 DATUM ENLIGT KDDATFORM                  
002500     03 MID-TIUPDATE-UT      PIC 9(6).                                    
002600*                                 DATUM ENLIGT KDDATFORM                  
002700     03 MID-FLTABORT         PIC X.                                       
002800*                                 BORTTAGSFLAGGA                          
002900     03 MID-RAD              OCCURS 3 TIMES.                              
003000        05 MID-STAPAC-IN     PIC X(5).                                    
003100*                                 ARBETSDAGENS BÖRJAN (PACKNING)          
003200        05 MID-STAPAC-UT     PIC X(5).                                    
003300*                                 ARBETSDAGENS BÖRJAN (PACKNING)          
003400        05 MID-STOPAC-IN     PIC X(5).                                    
003500*                                 ARBETSDAGENS SLUT (PACKNING)            
003600        05 MID-STOPAC-UT     PIC X(5).                                    
003700*                                 ARBETSDAGENS SLUT (PACKNING)            
003800        05 MID-STAADM-IN     PIC X(5).                                    
003900*                                 ARBETSDAGENS BÖRJAN (ORDERKONT)         
004000        05 MID-STAADM-UT     PIC X(5).                                    
004100*                                 ARBETSDAGENS BÖRJAN (ORDERKONT)         
004200        05 MID-STOADM-IN     PIC X(5).                                    
004300*                                 ARBETSDAGENS SLUT (ORDERKONT)           
004400        05 MID-STOADM-UT     PIC X(5).                                    
004500*                                 ARBETSDAGENS SLUT (ORDERKONT)           
004600        05 MID-STALAST-IN    PIC X(5).                                    
004700*                                 ARBETSDAGENS BÖRJAN (LASTNING)          
004800        05 MID-STALAST-UT    PIC X(5).                                    
004900*                                 ARBETSDAGENS BÖRJAN (LASTNING)          
005000        05 MID-STOLAST-IN    PIC X(5).                                    
005100*                                 ARBETSDAGENS SLUT (LASTNING)            
005200        05 MID-STOLAST-UT    PIC X(5).                                    
005300*                                 ARBETSDAGENS SLUT (LASTNING)            
