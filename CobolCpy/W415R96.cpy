000100 01  FD-W415F1-KDFD.                                                      
000200*                                 R96. AREA FÖR LÄSNING AV FDKRAV         
000300*                                                                         
000400     03 FD-KDFDKRAV          PIC 9(3).                                    
000500*                                 TRANSPORTFÖRPACKNINGSKOD                
000600     03 FD-TIFTO-F           PIC 9(5).                                    
000700*                                 FAST TID/ORDER    FACKLAGER             
000800     03 FD-TIFTR-F           PIC 9(4).                                    
000900*                                 FAST TID/RAD      FACKLAGER             
001000     03 FD-TIFTO-P           PIC 9(5).                                    
001100*                                 FAST TID/ORDER    PALLLAGER             
001200     03 FD-TIFTR-P           PIC 9(4).                                    
001300*                                 FAST TID/RAD      PALLAGER              
001400     03 FD-REKLI-P           PIC 9V9(2).                                  
001500*                                 KOLLIFAKTOR       PALLAGRET             
001600     03 FD-TIKLI-P           PIC 9(5).                                    
001700*                                 TID/KOLLI         PALLAGRET             
001800     03 FD-TIFTO-G           PIC 9(5).                                    
001900*                                 FAST TID/ORDER    GROVLAGER             
002000     03 FD-REKLI-G           PIC 9V9(2).                                  
002100*                                 KOLLIFAKTOR       GROVLAGRET            
002200     03 FD-TIKLI-G           PIC 9(5).                                    
002300*                                 TID/KOLLI         GROVLAGRET            
002400     03 FD-TIFTR-G1          PIC 9(4).                                    
002500*                                 FAST TID/RAD      GROVLAGER 30          
002600     03 FD-TIFTR-G2          PIC 9(4).                                    
002700*                                 FAST TID/RAD      GROVLAGER 35          
002800*** END COPY W415F1CCC0  LENGTH=50                                        
