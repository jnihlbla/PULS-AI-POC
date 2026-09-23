000100 01  MID-W4I22601.                                                        
000200     03 MID-IDDISTR-IN       PIC X(4).                                    
000300*                                 DISTRICT NUMBER                         
000400     03 MID-IDDISTR-UT       PIC X(4).                                    
000500*                                 DISTRICT NUMBER                         
000600     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000700*                                 CUSTOMER NO                             
000800     03 MID-IDKUNDNR-UT      PIC X(6).                                    
000900*                                 CUSTOMER NO                             
001000     03 MID-IDARTNR-IN       PIC X(9).                                    
001100*                                 PART NUMBER                             
001200     03 MID-IDARTNR-UT       PIC X(9).                                    
001300*                                 PART NUMBER                             
001400     03 MID-INPUT.                                                        
001500        05 MID-INMATNING     OCCURS 14 TIMES.                             
001600           07 MID-KDCMDVAL   PIC X(3).                                    
001700*                                 GENERAL COMMAND-CODE                    
001800     03 MID-INMATNING        OCCURS 14 TIMES.                             
001900        05 MID-TEVORMRK-SC   PIC X(2).                                    
002000     03 MID-RADER            OCCURS 14 TIMES.                             
002100        05 MID-IDDISTR       PIC 9(4).                                    
002200*                                 DISTRICT NUMBER                         
002300        05 MID-IDKUNDNR      PIC 9(6).                                    
002400*                                 CUSTOMER NO                             
002500        05 MID-IDORDNR7      PIC 9(7).                                    
002600*                                 ORDER NUMBER                            
002700        05 MID-IDARTNR       PIC 9(9).                                    
002800*                                 PART NUMBER                             
002900*** END OF VILMAII-COPY LENGTH= 472 BYTES                                 
