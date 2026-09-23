000100 01  W48882.                                                              
000200*                                 RECORD FRÅN W48880-PGM:ET (FSU)         
000300*                                 PÅFYLLNADSTRANSAR, SALDOBAS             
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600*                                 RECORD TYPE                             
000700     03 IDARTNR              PIC 9(9).                                    
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 IDDC                 PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 ADBUFFOMR            PIC 9(2).                                    
001400*                                 BUFFERTOMRÅDE                           
001500*                                 BUFFER AREA                             
001600     03 ADBUFFGANG           PIC 9(2).                                    
001700*                                 BUFFERT GÅNG                            
001800     03 ADBUFFPL             PIC 9(5).                                    
001900*                                 BUFFERPLATSNUMMER                       
002000*                                 LOCATION IN BUFFER                      
002100     03 KVBUFF-F             PIC 9(7).                                    
002200*                                 FÖRÄDLAT BUFFERSALDO                    
002300*                                 BUFFER QUANTANTITY PREPARED             
002400     03 KVBUFF-OF            PIC 9(7).                                    
002500*                                 BUFFERSALDO OFÖRÄDLAT GODS              
002600*                                 BUFFER BALANCE UNPREPARED GOODS         
002700     03 KVKOLLI-F            PIC 9(4).                                    
002800*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
002900*                                 Q PREPARED CASES IN BUFFER              
003000     03 KVKOLLI-OF           PIC 9(4).                                    
003100*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
003200*                                 Q UNPREPARED CASES IN BUFFER            
003300     03 KDBRIST              PIC 9.                                       
003400*                                 BRIST KOD                               
003500*                                 SHORTNESS CODE                          
003600     03 KDPAF                PIC 9.                                       
003700*                                 PÅFYLLNADSKOD                           
003800*                                 PAFF CODE                               
003900     03 DABUFPAF             PIC 9(8).                                    
004000*                                 BUFFERT PÅFYLLNINGS DATUM               
004100*                                 BUFFERT REFILLING DATE                  
004200*** END OF VILMAII-COPY LENGTH= 55 BYTES                                  
