000100 01  SALDO-W488020.                                                       
000200*                                 ANTALSPOST EFTER SUMMERING              
000300*                                 PER ARTIKEL OCH KDCLAGER                
000400*                                 AV SALDOPOSTER FRÅN PDP                 
000500     03 SALDO-IDPTYP-020     PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 SALDO-KVSALDOPOST-PDP                                             
000800                             PIC S9(5)           COMP-3.                  
000900*                                 ANTAL SALDOPOSTER                       
001000     03 SALDO-KVSALDOPOST-IBM                                             
001100                             PIC S9(5)           COMP-3.                  
001200*                                 ANTAL SALDOPOSTER                       
001300     03 FILLER               PIC X(22).                                   
001400*** END COPY W488020CC0  LENGTH=31                                        
