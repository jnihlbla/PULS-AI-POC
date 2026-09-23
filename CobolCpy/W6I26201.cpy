000100 01  MID-W6I26201.                                                        
000200*                                 MID TILL FRÅGA/UPPDATERING AV           
000300*                                 BUFFERSALDOBASEN                        
000400     03 MID-IDARTNR-IN       PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-IDARTNR-UT       PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 MID-KDBRIST          PIC 9.                                       
000900*                                 BRIST KOD                               
001000     03 MID-KDPAF            PIC 9.                                       
001100*                                 PÅFYLLNADSKOD                           
001200     03 MID-ADRESS           OCCURS 3 TIMES                               
001300                             INDEXED MID-IX1.                             
001400        05 MID-ADBUFFOMR     PIC 9(2).                                    
001500*                                 BUFFERTOMRÅDE                           
001600        05 MID-ADBUFFGANG    PIC 9(2).                                    
001700*                                 BUFFERT GÅNG                            
001800        05 MID-ADBUFFPL      PIC 9(5).                                    
001900*                                 BUFFERPLATSNUMMER                       
002000     03 MID-F-KVBUFF-PLUS    OCCURS 3 TIMES                               
002100                             INDEXED MID-IX2.                             
002200        05 MID-KVBUFF-F-PLUS PIC 9(7).                                    
002300*                                 FÖRÄDLAT BUFFERSALDO                    
002400     03 MID-F-KVBUFF-MINUS   OCCURS 3 TIMES                               
002500                             INDEXED MID-IX3.                             
002600        05 MID-KVBUFF-F-MINUS                                             
002700                             PIC 9(7).                                    
002800*                                 FÖRÄDLAT BUFFERSALDO                    
002900     03 MID-OF-KVBUFF-PLUS   OCCURS 3 TIMES                               
003000                             INDEXED MID-IX4.                             
003100        05 MID-KVBUFF-OF-PLUS                                             
003200                             PIC 9(7).                                    
003300*                                 BUFFERSALDO OFÖRÄDLAT GODS              
003400     03 MID-OF-KVBUFF-MINUS  OCCURS 3 TIMES                               
003500                             INDEXED MID-IX5.                             
003600        05 MID-KVBUFF-OF-MINUS                                            
003700                             PIC 9(7).                                    
003800*                                 BUFFERSALDO OFÖRÄDLAT GODS              
003900     03 MID-F-KVKOLLI-PLUS   OCCURS 3 TIMES                               
004000                             INDEXED MID-IX6.                             
004100        05 MID-KVKOLLI-F-PLUS                                             
004200                             PIC 9(4).                                    
004300*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
004400     03 MID-F-KVKOLLI-MINUS  OCCURS 3 TIMES                               
004500                             INDEXED MID-IX7.                             
004600        05 MID-KVKOLLI-F-MINUS                                            
004700                             PIC 9(4).                                    
004800*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
004900     03 MID-OF-KVKOLLI-PLUS  OCCURS 3 TIMES                               
005000                             INDEXED MID-IX8.                             
005100        05 MID-KVKOLLI-OF-PLUS                                            
005200                             PIC 9(4).                                    
005300*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
005400     03 MID-OF-KVKOLLI-MINUS OCCURS 3 TIMES                               
005500                             INDEXED MID-IX9.                             
005600        05 MID-KVKOLLI-OF-MINUS                                           
005700                             PIC 9(4).                                    
005800*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
