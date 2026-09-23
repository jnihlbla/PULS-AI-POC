000100 01  W48851.                                                              
000200*                                 RECORD FRÅN W48850-PGM:ET (FSU)         
000300*                                 PÅFYLLNADSTRANSAR, SALDOBAS             
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600*                                 RECORD TYPE                             
000700     03 IDARTNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 IDDC                 PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 KDPAF                PIC S9              COMP-3.                  
001400*                                 PÅFYLLNADSKOD                           
001500*                                 PAFF CODE                               
001600     03 KVBUFF-F             PIC S9(7)           COMP-3.                  
001700*                                 FÖRÄDLAT BUFFERSALDO                    
001800*                                 BUFFER QUANTANTITY PREPARED             
001900     03 KVBUFF-OF            PIC S9(7)           COMP-3.                  
002000*                                 BUFFERSALDO OFÖRÄDLAT GODS              
002100*                                 BUFFER BALANCE UNPREPARED GOODS         
002200     03 TIPAF                PIC S9(7)           COMP-3.                  
002300*                                 PÅFYLLNADSDATUM   (ÅÅMMDD)              
002400*                                 REFILLING DATE    (AAMMDD)              
002500     03 ADBUFFOMR            PIC S9(3)           COMP-3.                  
002600*                                 BUFFERTOMRÅDE                           
002700*                                 BUFFER AREA                             
002800     03 ADBUFFGANG           PIC S9(3)           COMP-3.                  
002900*                                 BUFFERT GÅNG                            
003000     03 ADBUFFPL             PIC S9(5)           COMP-3.                  
003100*                                 BUFFERPLATSNUMMER                       
003200*                                 LOCATION IN BUFFER                      
