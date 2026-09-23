000100 01  MOD-W4O90101.                                                        
000200*                                 MODCOPYTEXT TILL W40901.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 TRANSAKTIONSIDENTITET                   
000500     03 MOD-MESSAGE-RAD1     PIC X(40).                                   
000600*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDORDNR-IN       PIC X(7).                                    
001600*                                 ORDERNR             IDORDNR-002         
001700     03 MOD-IDORDNR-UT       PIC X(7).                                    
001800*                                 ORDERNR             IDORDNR-002         
001900     03 MOD-IDTECKEN-IN      PIC X.                                       
002000*                                 TECKEN                                  
002100     03 MOD-IDTECKEN-UT      PIC X.                                       
002200*                                 TECKEN                                  
002300     03 MOD-TIORDREG-IN      PIC X(6).                                    
002400*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
002500     03 MOD-TIORDREG-UT      PIC X(6).                                    
002600*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
002700     03 MOD-IDDISTR-SPAR     PIC 9(4).                                    
002800*                                 DISTRIKTNUMMER                          
002900     03 MOD-IDKUNDNR-SPAR    PIC 9(6).                                    
003000*                                 KUNDNUMMER                              
003100     03 MOD-IDORDNR-SPAR     PIC 9(7).                                    
003200*                                 ORDERNR             IDORDNR-002         
003300     03 MOD-TIORDREG-SPAR    PIC 9(6).                                    
003400*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
003500     03 MOD-RAD              OCCURS 13 TIMES.                             
003600*                                 TABELL INNEHÅLLANDE RADER.              
003700        05 MOD-IDKUNDNR-RAD  PIC Z(5)9.                                   
003800*                                 KUNDNUMMER                              
003900        05 FILLER            PIC X(2).                                    
004000        05 MOD-IDORDNR-RAD   PIC Z(6)9.                                   
004100*                                 ORDERNR             IDORDNR-002         
004200        05 FILLER            PIC X(2).                                    
004300        05 MOD-TIORDREG      PIC 9(6).                                    
004400*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
004500        05 FILLER            PIC X.                                       
004600        05 MOD-BEVOLREF      PIC X(10).                                   
004700*                                 VOLVO REFERENS                          
004800        05 FILLER            PIC X(2).                                    
004900        05 MOD-BEVARREF      PIC X(10).                                   
005000*                                 VÅR REFERENS                            
005100        05 FILLER            PIC X(3).                                    
005200        05 MOD-KDORDKL-IMP   PIC 9.                                       
005300*                                 ORDERKLASS FRÅN IMPORTÖREN              
005400        05 FILLER            PIC X(4).                                    
005500        05 MOD-KDORDKL       PIC 9.                                       
005600*                                 ORDERKLASS                              
005700     03 MOD-MESSAGE-RAD23    PIC X(61).                                   
005800*                                 MEDDELANDEFÄLT PÅ RAD 23                
005900*** END COPY W4O90101C0  LENGTH=891                                       
