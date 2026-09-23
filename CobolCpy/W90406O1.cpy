000100 01  W90406O1.                                                            
000200*                                 COPYTEXT FÖR MOD W90406O1.              
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MESSAGE-RAD1         PIC X(40).                                   
000600*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000700     03 FILLER               PIC X(9).                                    
000800     03 RADER-SKIP           PIC 9(3).                                    
000900*                                 ANTAL RADER SOM LÄSES FÖRBI.            
001000     03 FILLER               PIC X(9).                                    
001100     03 FILLER               PIC X.                                       
001200     03 AREA.                                                             
001300*                                 AREA NOLLSTÄLLS MELLAN VARVEN.          
001400        05 FILLER            PIC X.                                       
001500        05 FILLER            OCCURS 14 TIMES                              
001600                             PIC X(5).                                    
001700        05 FILLER            PIC X.                                       
001800        05 FILLER            OCCURS 14 TIMES                              
001900                             PIC X.                                       
002000        05 FILLER            OCCURS 14 TIMES                              
002100                             PIC X.                                       
002200        05 BELEV             OCCURS 14 TIMES                              
002300                             PIC X(30).                                   
002400*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
002500        05 FILLER            PIC X(5).                                    
002600        05 MESSAGE-RAD23     PIC X(79).                                   
002700*                                 MEDDELANDEFÄLT PÅ RAD 23                
002800*** END OF VILMAII-COPY LENGTH= 670 BYTES                                 
