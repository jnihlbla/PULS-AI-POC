000100 01  W488L222.                                                            
000200*                                 LÄNKAREA NR 2 FÖR KOMMUNIKATION         
000300*                                 MELLAN W4882200 OCH DESS SUBPGM         
000400*                                 ANVÄNDS VID ANROPEN:                    
000500*                                         LAS-HTR-TID                     
000600*                                         UPPDATERA-HTR-TID               
000700     03 IDHTYP               PIC X(4).                                    
000800*                                 HÄNDELSETYP                             
000900     03 TIUPPTID-KLAR        PIC S9(9)           COMP-3.                  
001000*                                 UPPDATERINGSTID  (TTMMSSTH)             
001100     03 KVSALDOPOST-PDP      PIC S9(5)           COMP-3.                  
001200*                                 ANTAL SALDOPOSTER                       
001300     03 KVSALDOPOST-IBM      PIC S9(5)           COMP-3.                  
001400*                                 ANTAL SALDOPOSTER                       
001500     03 KVSALDOPOST-FEL      PIC S9(5)           COMP-3.                  
001600*                                 ANTAL SALDOPOSTER                       
001700     03 KDTRSTAT             PIC S9              COMP-3.                  
001800*                                 TRANSAKTIONSSTATUS                      
001900     03 KVSALDOPOST-FELTOTAL PIC S9(5)           COMP-3.                  
002000*                                 ANTAL SALDOPOSTER                       
002100*** END COPY W488L222C0  LENGTH=22                                        
