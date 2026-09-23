000100 01  W90405O1.                                                            
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W90405O1                                
000400     03 TRANS-NUMMER.                                                     
000500*                                 TRANSAKTIONS-NUMMER                     
000600        05 TRANS-SIFF-1      PIC X.                                       
000700        05 TRANS-SIFF-2      PIC X.                                       
000800        05 TRANS-SIFF-3      PIC X.                                       
000900        05 TRANS-SIFF-4      PIC X.                                       
001000     03 MESSAGE-RAD1         PIC X(40).                                   
001100*                                 MEDDELANDEFÄLT PÅ RAD 1                 
001200     03 FILLER               PIC X(5).                                    
001300     03 FILLER               PIC X(30).                                   
001400     03 FILLER               PIC 9(9).                                    
001500     03 FILLER               PIC X(5).                                    
001600     03 FILLER               PIC X(30).                                   
001700     03 FILLER               PIC 9(9).                                    
001800     03 BELEV-SPAR           PIC X(30).                                   
001900*                                 SPARAD BENÄMNING                        
002000     03 AREA.                                                             
002100        05 LINES             OCCURS 14 TIMES                              
002200                             INDEXED IX-LINE.                             
002300           07 FILLER         PIC X(2).                                    
002400           07 BELEV          PIC X(30).                                   
002500*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
002600           07 FILLER         PIC 9.                                       
002700           07 FILLER         PIC X.                                       
002800           07 FILLER         PIC 9.                                       
002900           07 IDARTNR        PIC 9(9).                                    
003000*                                 ARTIKELNUMMER                           
003100           07 FILLER         PIC X.                                       
003200        05 LINE23.                                                        
003300           07 MESSAGE-23     PIC X(80).                                   
003400*                                 MEDDELANDEFÄLT PÅ RAD 23                
003500*** END OF VILMAII-COPY LENGTH= 872 BYTES                                 
