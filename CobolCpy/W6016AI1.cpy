000100 01  REQU-W6016AI1.                                                       
000200*                                 COPYTEXT FOR REQU W6016AI1              
000300     03 REQU-KVRADER         PIC 9(5).                                    
000400*                                 NUMBER OF LINES                         
000500     03 REQU-MESSAGES        OCCURS 1000 TIMES.                           
000600        05 REQU-SALDO-IDARTNR                                             
000700                             PIC 9(9).                                    
000800*                                 PART NUMBER                             
000900        05 REQU-SALDO-KVBUFF-F                                            
001000                             PIC 9(7).                                    
001100*                                 BUFFER QUANTANTITY PREPARED             
001200        05 REQU-SALDO-KVBUFF-OF                                           
001300                             PIC 9(7).                                    
001400*                                 BUFFER BALANCE UNPREPARED GOODS         
001500        05 REQU-SALDO-KVKOLLI-F                                           
001600                             PIC 9(4).                                    
001700*                                 Q PREPARED CASES IN BUFFER              
001800        05 REQU-SALDO-KVKOLLI-OF                                          
001900                             PIC 9(4).                                    
002000*                                 Q UNPREPARED CASES IN BUFFER            
002100*** END OF VILMAII-COPY LENGTH= 31005 BYTES                               
