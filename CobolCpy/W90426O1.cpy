000100 01  MOD-W90426O1.                                                        
000200*                                 MOD-COPYTEXT FÖR W9042600               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-FILLER           PIC X(9).                                    
000800     03 MOD-FILLER           PIC X(2).                                    
000900     03 MOD-FILLER           PIC X(9).                                    
001000     03 MOD-FILLER           PIC X(2).                                    
001100     03 MOD-GEMINFO.                                                      
001200        05 MOD-FILLER        PIC X(21).                                   
001300        05 MOD-FILLER        PIC X(9).                                    
001400        05 MOD-FILLER        PIC X(9).                                    
001500        05 MOD-FILLER        PIC Z(4)9.                                   
001600        05 MOD-FILLER        OCCURS 5 TIMES                               
001700                             PIC X.                                       
001800        05 MOD-FILLER        OCCURS 5 TIMES                               
001900                             PIC X.                                       
002000        05 MOD-FILLER        PIC 9(2).                                    
002100        05 MOD-FILLER        PIC 9(2).                                    
002200        05 MOD-FILLER        PIC 9(2).                                    
002300     03 MOD-CDC-INFO.                                                     
002400        05 MOD-FILLER        PIC X(2).                                    
002500        05 MOD-FILLER        PIC Z(6)9.                                   
002600        05 MOD-FILLER        PIC Z(6)9.                                   
002700        05 MOD-FILLER        PIC Z(7)9.                                   
002800        05 MOD-FILLER        PIC Z(6)9.                                   
002900        05 MOD-FILLER        PIC Z(6)9.                                   
003000        05 MOD-FILLER        PIC Z(6)9.                                   
003100        05 MOD-FILLER        PIC Z(6)9.                                   
003200        05 MOD-FILLER        PIC Z(7)9.                                   
003300        05 MOD-FILLER        PIC Z(7)9.                                   
003400        05 MOD-FILLER        PIC 9(2).                                    
003500        05 MOD-FILLER        PIC Z(6)9.                                   
003600     03 MOD-NDC-INFO         OCCURS 5 TIMES.                              
003700        05 MOD-IDDC-NDC      PIC X(2).                                    
003800*                                 IDENTIFIERARE LAGER                     
003900        05 MOD-KVLS-NDC      PIC -(6)9.                                   
004000*                                 LAGERSALDO                              
004100        05 MOD-FILLER        PIC Z(6)9.                                   
004200        05 MOD-FILLER        PIC Z(7)9.                                   
004300        05 MOD-FILLER        PIC Z(6)9.                                   
004400        05 MOD-FILLER        PIC Z(6)9.                                   
004500        05 MOD-FILLER        PIC Z(6)9.                                   
004600        05 MOD-FILLER        PIC Z(6)9.                                   
004700        05 MOD-FILLER        PIC Z(7)9.                                   
004800        05 MOD-FILLER        PIC Z(7)9.                                   
004900        05 MOD-FILLER        PIC Z(6)9.                                   
005000        05 MOD-FILLER        PIC 9(2).                                    
005100        05 MOD-FILLER        PIC Z(6)9.                                   
005200     03 MOD-TEMFSINF         PIC X(55).                                   
005300*                                 INFORMATIONSMEDDELANDE                  
005400*** END OF VILMAII-COPY LENGTH= 678 BYTES                                 
