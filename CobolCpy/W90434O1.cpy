000100 01  MOD-W90434O1.                                                        
000200*                                 MOD-COPYTEXT FÖR W9043400               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-FILLER           PIC X(9).                                    
000800     03 MOD-FILLER           PIC X(9).                                    
000900     03 MOD-FILLER           PIC X(3).                                    
001000     03 MOD-FILLER           PIC X(3).                                    
001100     03 MOD-FILLER           PIC X(2).                                    
001200     03 MOD-FILLER           PIC X(9).                                    
001300     03 MOD-FILLER           PIC X(25).                                   
001400     03 MOD-FILLER           PIC 9.                                       
001500     03 MOD-FILLER           PIC 9(6).                                    
001600     03 MOD-FILLER           PIC X(2).                                    
001700     03 MOD-FILLER           PIC X(25).                                   
001800     03 MOD-FILLER           PIC X(2).                                    
001900     03 MOD-FILLER           PIC 9.                                       
002000     03 MOD-FILLER           PIC 9(2).                                    
002100     03 MOD-FILLER           PIC 9(4).                                    
002200     03 MOD-IDSTRTYP-UT      PIC X.                                       
002300*                                 STRUKTURTYP                             
002400     03 MOD-FILLER           PIC X(2).                                    
002500     03 MOD-FILLER           PIC X.                                       
002600     03 MOD-FILLER           PIC X(2).                                    
002700     03 MOD-FILLER           PIC X(2).                                    
002800     03 MOD-FILLER           PIC X(2).                                    
002900     03 MOD-FILLER           PIC 9(4).                                    
003000     03 MOD-IDSTRTYP-IN-ATTR PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 MOD-FILLER           PIC X.                                       
003300     03 MOD-FILLER           PIC X(5).                                    
003400     03 MOD-FILLER           PIC 9(2).                                    
003500     03 MOD-FILLER           PIC X(30).                                   
003600     03 MOD-FILLER           OCCURS 5 TIMES                               
003700                             PIC X(10).                                   
003800     03 MOD-FILLER           PIC X(40).                                   
003900     03 MOD-FILLER           PIC X(40).                                   
004000     03 MOD-TESTRNOT-GRUPP   OCCURS 2 TIMES.                              
004100*                                                                         
004200        05 MOD-FILLER        PIC X(2).                                    
004300        05 MOD-FILLER        PIC X(70).                                   
004400     03 MOD-TEMFSINF         PIC X(55).                                   
004500*                                 INFORMATIONSMEDDELANDE                  
004600*** END OF VILMAII-COPY LENGTH= 530 BYTES                                 
