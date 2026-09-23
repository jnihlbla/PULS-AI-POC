000100 01  MID-W4I22501.                                                        
000200     03 MID-IDROLL-IN        PIC X(5).                                    
000300*                                 VOR ROLE ID                             
000400     03 MID-IDDISTR-IN       PIC X(4).                                    
000500*                                 DISTRICT NUMBER                         
000600     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000700*                                 CUSTOMER NO                             
000800     03 MID-IDANSK-IN        PIC X(3).                                    
000900*                                 PROCURER NO.                            
001000     03 MID-IDARTNR-IN       PIC X(9).                                    
001100*                                 PART NUMBER                             
001200     03 MID-KDSORT-IN        PIC X(2).                                    
001300*                                 FIELD FOR SORTING PURPOSE               
001400     03 MID-TEVORMRK-IN      PIC X(2).                                    
001500     03 MID-IDORDNR-VOR      PIC 9(7).                                    
001600*                                 ORDER NUMBER                            
001700     03 MID-INPUT.                                                        
001800        05 MID-INMATNING     OCCURS 13 TIMES.                             
001900           07 MID-CMD        PIC X.                                       
002000           07 MID-TEVORMRK   PIC X(2).                                    
002100     03 MID-RADER            OCCURS 13 TIMES.                             
002200        05 MID-IDDISTR       PIC 9(4).                                    
002300*                                 DISTRICT NUMBER                         
002400        05 MID-IDKUNDNR      PIC 9(6).                                    
002500*                                 CUSTOMER NO                             
002600        05 MID-IDORDNR7      PIC 9(7).                                    
002700*                                 ORDER NUMBER                            
002800        05 MID-IDANSK        PIC 9(3).                                    
002900*                                 PROCURER NO.                            
003000        05 MID-IDARTNR       PIC 9(9).                                    
003100*                                 PART NUMBER                             
003200*** END OF VILMAII-COPY LENGTH= 454 BYTES                                 
