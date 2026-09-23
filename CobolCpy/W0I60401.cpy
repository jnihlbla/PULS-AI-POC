000100 01  MID-W0I60401.                                                        
000200*                                 MID TILL FRÅGA/UPPDATERING AV           
000300*                                 DELSALDO-ÖVERFÖRING                     
000400     03 MID-KVSALDOPOST-FEL-FOM                                           
000500                             PIC 9(5).                                    
000600*                                 ANTAL SALDOPOSTER                       
000700     03 MID-KVSALDOPOST-FEL-TOM                                           
000800                             PIC 9(5).                                    
000900*                                 ANTAL SALDOPOSTER                       
001000     03 MID-KDSVAR-TABELL.                                                
001100        05 MID-KDSVAR        OCCURS 12 TIMES                              
001200                             PIC X.                                       
001300*                                 SVARSKOD FRÅN SUBPROGRAM                
001400*** END COPY W0I60401C0  LENGTH=22                                        
