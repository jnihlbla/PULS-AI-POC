000100 01  MID-W0I69101.                                                        
000200*                                 COPYTEXT FÖR MOD W0O69101               
000300     03 MID-IDNODE           PIC X(8).                                    
000400*                                 VTAM NODE-NAMN                          
000500*                                 VTAM NODE NAME                          
000600     03 MID-TIREGDAT         PIC X(6).                                    
000700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000800*                                 REGISTRATION DATE (YYMMDD)              
000900     03 MID-TIKLOCK          PIC X(9).                                    
001000*                                 KLOCKSLAG (TTMMSSTH)                    
001100*                                 TIME OF DAY (HHMMSSTH)                  
001200     03 MID-IDNODE-BACKUP    PIC X(8).                                    
001300*                                 LOGISK BACKUP-PRINTER                   
001400*                                 LOGICAL BACKUP PRINTER                  
001500     03 MID-IDTFX            PIC X(20).                                   
001600*                                 TELEFAXNUMMER                           
001700*                                 FAXNUMBER                               
001800     03 MID-TEFAX            OCCURS 5 TIMES                               
001900                             PIC X(50).                                   
002000*                                 FAX TEXTRAD TILL FÖRSÄTTSBLAD           
002100*                                 FAX INFO LINE                           
002200*** END OF VILMAII-COPY LENGTH= 301 BYTES                                 
