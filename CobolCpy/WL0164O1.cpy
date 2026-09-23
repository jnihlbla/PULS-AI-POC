000100 01  RESP-WL0164O1.                                                       
000200*                                 RESPONS FROM PGM WL0164                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-KDBEHX-KEY      PIC X.                                       
000600*                                 BEHANDLINGSKOD-X                        
000700     03 RESP-IDILIST-KEY     PIC 9(5).                                    
000800*                                 INLÄGGNINGSLISTEIDENTITET               
000900     03 RESP-IDANSTNR-UPD    PIC Z(4)9.                                   
001000*                                 ANSTÄLLNINGSNUMMER                      
001100     03 RESP-KVRADER         PIC Z(4)9.                                   
001200*                                 ANTAL RADER                             
001300     03 RESP-RADER           OCCURS 500 TIMES.                            
001400*                                 RADINFORMATION                          
001500        05 RESP-FLCMD        PIC X.                                       
001600        05 RESP-IDILIST      PIC 9(5).                                    
001700*                                 INLÄGGNINGSLISTEIDENTITET               
001800        05 RESP-TIUPPDAT-ILI PIC 9(6).                                    
001900*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
002000        05 RESP-IDANSTNR-RET-ILI                                          
002100                             PIC Z(4)9.                                   
002200*                                 ANSTÄLLNINGSNUMMER                      
002300        05 RESP-TIUTSKR-ILI  PIC 9(6).                                    
002400*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
002500        05 RESP-IDANSTNR-RET-PRT                                          
002600                             PIC Z(4)9.                                   
002700*                                 ANSTÄLLNINGSNUMMER                      
002800        05 RESP-IDMSG-ERROR-LINE                                          
002900                             PIC X(3).                                    
003000*                                 FELMEDDELANDE ID                        
003100*** END OF VILMAII-COPY LENGTH= 15518 BYTES                               
