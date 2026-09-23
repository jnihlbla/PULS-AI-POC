000100 01  SALDO-W488009.                                                       
000200*                                 INIT POST TILL                          
000300*                                 SALDOFÖRÄNDRINGAR FRÅN PDP              
000400*                                 PER ARTIKEL KOMMER SALDOPOSTER          
000500*                                 CA 3-4 GÅNGER PER DAG                   
000600     03 FILLER               PIC X.                                       
000700     03 SALDO-TEXT           PIC X(8).                                    
000800     03 FILLER               PIC X.                                       
000900     03 SALDO-IDOVER         PIC X(6).                                    
001000*                                 DATAÖVERFÖRINGSIDENTITET                
001100     03 FILLER               PIC X(64).                                   
001200*** END COPY W488009CC0  LENGTH=80                                        
