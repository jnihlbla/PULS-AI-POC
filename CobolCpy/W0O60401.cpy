000100 01  MOD-W0O60401.                                                        
000200*                                 MOD TILL FRÅGA/UPPDATERING AV           
000300*                                 DELSALDO-ÖVERFÖRING                     
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 TRANSAKTIONSIDENTITET                   
000600     03 MOD-MESSAGE-RAD1     PIC X(40).                                   
000700*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000800     03 MOD-KVSALDOPOST-FEL-FOM                                           
000900                             PIC 9(5).                                    
001000*                                 ANTAL SALDOPOSTER                       
001100     03 MOD-KVSALDOPOST-FEL-TOM                                           
001200                             PIC 9(5).                                    
001300*                                 ANTAL SALDOPOSTER                       
001400     03 MOD-4804-RAD         OCCURS 4 TIMES.                              
001500        05 MOD-TIREGDAT-START                                             
001600                             PIC 9(6).                                    
001700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001800        05 FILLER            PIC X.                                       
001900        05 MOD-TIUPPTID-START                                             
002000                             PIC 9(8).                                    
002100*                                 UPPDATERINGSTID  (TTMMSSTH)             
002200        05 FILLER            PIC X(3).                                    
002300        05 MOD-TIUPPTID-KLAR PIC 9(8).                                    
002400*                                 UPPDATERINGSTID  (TTMMSSTH)             
002500        05 FILLER            PIC X.                                       
002600        05 MOD-KVSALDOPOST-PDP                                            
002700                             PIC Z(4)9.                                   
002800*                                 ANTAL SALDOPOSTER                       
002900        05 FILLER            PIC X.                                       
003000        05 MOD-KVSALDOPOST-IBM                                            
003100                             PIC Z(4)9.                                   
003200*                                 ANTAL SALDOPOSTER                       
003300        05 FILLER            PIC X.                                       
003400        05 MOD-KVSALDOPOST-FEL                                            
003500                             PIC Z(4)9.                                   
003600*                                 ANTAL SALDOPOSTER                       
003700        05 FILLER            PIC X.                                       
003800        05 MOD-STATUS-TEXT   PIC X(20).                                   
003900     03 MOD-4802-RAD         OCCURS 12 TIMES.                             
004000        05 MOD-KDSVAR-ATTR   PIC X(2).                                    
004100        05 MOD-KDSVAR        PIC X.                                       
004200*                                 SVARSKOD FRÅN SUBPROGRAM                
004300        05 MOD-IDLOPNRF      PIC Z(4)9.                                   
004400*                                 LÖPNUMMER FELTRANS                      
004500        05 MOD-IDARTNR       PIC Z(8)9.                                   
004600*                                 ARTIKELNUMMER                           
004700        05 FILLER            PIC X.                                       
004800        05 MOD-TIREGDAT      PIC 9(6).                                    
004900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005000        05 FILLER            PIC X.                                       
005100        05 MOD-FELTEXT       PIC X(15).                                   
005200     03 MOD-MESSAGE-RAD23    PIC X(61).                                   
005300*                                 MEDDELANDEFÄLT PÅ RAD 23                
005400*** END COPY W0O60401C0  LENGTH=855                                       
