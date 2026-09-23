000100 01  KVITTO-W488022.                                                      
000200*                                 KVITTOPOST PÅ MOTTAGNA                  
000300*                                 SALDOFÖRÄNDRINGAR SKALL EV.             
000400*                                 SÄNDAS ÅTER TILL PDP-N                  
000500     03 KVITTO-IDPTYP-022    PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 FILLER               PIC X.                                       
000800     03 KVITTO-KVSALDOPOST-PDP                                            
000900                             PIC 9(5).                                    
001000*                                 ANTAL SALDOPOSTER                       
001100     03 FILLER               PIC X.                                       
001200     03 KVITTO-KVSALDOPOST-IBM                                            
001300                             PIC 9(5).                                    
001400*                                 ANTAL SALDOPOSTER                       
001500     03 FILLER               PIC X.                                       
001600     03 KVITTO-KVSALDOPOST-ART                                            
001700                             PIC 9(5).                                    
001800*                                 ANTAL SALDOPOSTER                       
001900     03 FILLER               PIC X(11).                                   
002000*** END COPY W488022CC0  LENGTH=32                                        
