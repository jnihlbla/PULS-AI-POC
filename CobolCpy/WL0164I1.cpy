000100 01  REQU-WL0164I1.                                                       
000200*                                 REQUEST TO PGM WL0164                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-KDBEHX-KEY      PIC X.                                       
000600*                                 BEHANDLINGSKOD-X                        
000700     03 REQU-IDILIST-KEY     PIC 9(5).                                    
000800*                                 INLÄGGNINGSLISTEIDENTITET               
000900     03 REQU-IDANSTNR-UPD    PIC 9(5).                                    
001000*                                 ANSTÄLLNINGSNUMMER                      
001100     03 REQU-KVRADER         PIC 9(5).                                    
001200*                                 ANTAL RADER                             
001300     03 REQU-INPUT           OCCURS 500 TIMES.                            
001400*                                 INMATNINGSFÄLT + NYCKELFÄLT             
001500        05 REQU-FLCMD        PIC X.                                       
001600        05 REQU-IDILIST      PIC 9(5).                                    
001700*                                 INLÄGGNINGSLISTEIDENTITET               
001800*** END OF VILMAII-COPY LENGTH= 3018 BYTES                                
