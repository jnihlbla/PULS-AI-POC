000100 01  REQU-WL0105I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WL0105             
000300*                                 LDC BUFFER INFORMATION                  
000400     03 REQU-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 REQU-IDARTNR-KEY     PIC 9(9).                                    
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 REQU-KVBUFF-KEY      PIC 9(7).                                    
001100*                                 FÖRÄDLAT BUFFERSALDO                    
001200*                                 BUFFER QUANTANTITY PREPARED             
001300     03 REQU-ADBUFFOMR-KEY   PIC 9(2).                                    
001400*                                 BUFFERTOMRÅDE                           
001500*                                 BUFFER AREA                             
001600     03 REQU-ADBUFFGANG-KEY  PIC 9(2).                                    
001700*                                 BUFFERT GÅNG                            
001800     03 REQU-ADBUFFPL-KEY    PIC 9(5).                                    
001900*                                 BUFFERPLATSNUMMER                       
002000*                                 LOCATION IN BUFFER                      
002100     03 REQU-ADBUFFOMR-UPD   PIC 9(2).                                    
002200*                                 BUFFERTOMRÅDE                           
002300*                                 BUFFER AREA                             
002400     03 REQU-ADBUFFGANG-UPD  PIC 9(2).                                    
002500*                                 BUFFERT GÅNG                            
002600     03 REQU-ADBUFFPL-UPD    PIC 9(5).                                    
002700*                                 BUFFERPLATSNUMMER                       
002800*                                 LOCATION IN BUFFER                      
002900     03 REQU-KVBUFF-F-UPD    PIC 9(7).                                    
003000*                                 FÖRÄDLAT BUFFERSALDO                    
003100*                                 BUFFER QUANTANTITY PREPARED             
003200     03 REQU-KVKOLLI-F-UPD   PIC 9(4).                                    
003300*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
003400*                                 Q PREPARED CASES IN BUFFER              
003500     03 REQU-KVRADER         PIC 9(5).                                    
003600*                                 ANTAL RADER                             
003700*                                 NUMBER OF LINES                         
003800     03 REQU-RADER-GRP.                                                   
003900        05 REQU-RADER        OCCURS 500 TIMES.                            
004000           07 REQU-KDCMDVAL  PIC X(3).                                    
004100*                                 GENERELL KOMMANDOKOD                    
004200*                                 GENERAL COMMAND-CODE                    
004300           07 REQU-ADBUFFOMR PIC 9(2).                                    
004400*                                 BUFFERTOMRÅDE                           
004500*                                 BUFFER AREA                             
004600           07 REQU-ADBUFFGANG                                             
004700                             PIC 9(2).                                    
004800*                                 BUFFERT GÅNG                            
004900           07 REQU-ADBUFFPL  PIC 9(5).                                    
005000*                                 BUFFERPLATSNUMMER                       
005100*                                 LOCATION IN BUFFER                      
005200           07 REQU-DABUFPAF  PIC 9(8).                                    
005300*                                 BUFFERT PÅFYLLNINGS DATUM               
005400*                                 BUFFERT REFILLING DATE                  
005500           07 REQU-KVBUFF-F-IN                                            
005600                             PIC 9(7).                                    
005700*                                 FÖRÄDLAT BUFFERSALDO                    
005800*                                 BUFFER QUANTANTITY PREPARED             
005900           07 REQU-KVKOLLI-F-IN                                           
006000                             PIC 9(4).                                    
006100*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
006200*                                 Q PREPARED CASES IN BUFFER              
006300     03 REQU-RAD-ONE REDEFINES REQU-RADER-GRP.                            
006400        05 REQU-KDCMDVAL-ONE PIC X(3).                                    
006500*                                 GENERELL KOMMANDOKOD                    
006600*                                 GENERAL COMMAND-CODE                    
006700        05 REQU-ADBUFFOMR-ONE                                             
006800                             PIC 9(2).                                    
006900*                                 BUFFERTOMRÅDE                           
007000*                                 BUFFER AREA                             
007100        05 REQU-ADBUFFGANG-ONE                                            
007200                             PIC 9(2).                                    
007300*                                 BUFFERT GÅNG                            
007400        05 REQU-ADBUFFPL-ONE PIC 9(5).                                    
007500*                                 BUFFERPLATSNUMMER                       
007600*                                 LOCATION IN BUFFER                      
007700        05 REQU-DABUFPAF-ONE PIC 9(8).                                    
007800*                                 BUFFERT PÅFYLLNINGS DATUM               
007900*                                 BUFFERT REFILLING DATE                  
008000        05 REQU-KVBUFF-F-IN-ONE                                           
008100                             PIC 9(7).                                    
008200*                                 FÖRÄDLAT BUFFERSALDO                    
008300*                                 BUFFER QUANTANTITY PREPARED             
008400        05 REQU-KVKOLLI-F-IN-ONE                                          
008500                             PIC 9(4).                                    
008600*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
008700*                                 Q PREPARED CASES IN BUFFER              
008800        05 FILLER            PIC X(15469).                                
008900*** END OF VILMAII-COPY LENGTH= 15552 BYTES                               
