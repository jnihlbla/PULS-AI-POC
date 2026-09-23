000100 01  MOD-W0O69101.                                                        
000200*                                 COPYTEXT FÖR MOD W0O69101               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDNODE           PIC X(8).                                    
001000*                                 VTAM NODE-NAMN                          
001100*                                 VTAM NODE NAME                          
001200     03 MOD-TIREGDAT         PIC X(6).                                    
001300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001400*                                 REGISTRATION DATE (YYMMDD)              
001500     03 MOD-TIKLOCK          PIC X(9).                                    
001600*                                 KLOCKSLAG (TTMMSSTH)                    
001700*                                 TIME OF DAY (HHMMSSTH)                  
001800     03 MOD-IDNODE-BACKUP    PIC X(8).                                    
001900*                                 LOGISK BACKUP-PRINTER                   
002000*                                 LOGICAL BACKUP PRINTER                  
002100     03 MOD-IDTFX            PIC X(20).                                   
002200*                                 TELEFAXNUMMER                           
002300*                                 FAXNUMBER                               
002400     03 MOD-TEFAX            OCCURS 5 TIMES                               
002500                             PIC X(50).                                   
002600*                                 FAX TEXTRAD TILL FÖRSÄTTSBLAD           
002700*                                 FAX INFO LINE                           
002800     03 MOD-TEMFSINF         PIC X(55).                                   
002900*                                 INFORMATIONSMEDDELANDE                  
003000*                                 INFORMATION MESSAGE                     
003100*** END OF VILMAII-COPY LENGTH= 400 BYTES                                 
