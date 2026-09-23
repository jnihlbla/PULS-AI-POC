000100 01  SORT-WDA801.                                                         
000200*                                 RETUR SPÄRRAR                           
000300*                                 BEHANDLIGS KODER                        
000400*                                 FYSISK NYCKEL: BESORTRT                 
000500     03 SORT-BESORTRT        PIC X(20).                                   
000600*                                 SORTIMENT FÖR RETURER                   
000700     03 SORT-ANMORS-BEH      OCCURS 14 TIMES.                             
000800        05 SORT-KDANMORS     PIC X(2).                                    
000900*                                 ORSAK TILL LEVERANSANMÄRKNING           
001000        05 SORT-KDRETBEH     PIC X.                                       
001100*                                 RETUR BEHANDLING PER ANMORSAK           
001200*** END OF VILMAII-COPY LENGTH= 62 BYTES                                  
