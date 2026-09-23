000100 01  MOD-W1O52401.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 1524.             
000300*                                 BESTÄLLNING AV KONTROLL-                
000400*                                 PROGRAM INOM KATALOGSYSTEMET            
000500     03 MOD-IDTRANS          PIC X(4).                                    
000600*                                 BILDNUMMER                              
000700     03 MOD-MESSAGE-RAD1     PIC X(40).                                   
000800*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000900     03 MOD-KDBEH-IDJOB-ATTR PIC X(2).                                    
001000*                                 MFS ATTRIBUTFÄLT                        
001100     03 MOD-KDBEH-IDJOB      PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDCATNR-ATTR     PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500     03 MOD-IDCATNR          PIC Z(4)9.                                   
001600*                                 KATALOG-ID                              
001700     03 MOD-SPRAK-KOD.                                                    
001800        05 FILLER            OCCURS 6 TIMES.                              
001900           07 MOD-IDSKYLT-ATTR                                            
002000                             PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200           07 MOD-IDSKYLT    PIC X(3).                                    
002300*                                 NATIONALITETSTECKEN                     
002400*                                 SPRÅKIDENTIFIKATION                     
002500     03 MOD-KDPRTVAL         PIC X(2).                                    
002600*                                 MFS BEHANDLING AV INPUTFÄLT             
002700     03 MOD-TIERSDAT-ATTR    PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-TIERSDAT         PIC X(5).                                    
003000*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
003100     03 MOD-MESSAGE-RAD23    PIC X(61).                                   
003200*                                 MEDDELANDEFÄLT PÅ RAD 23                
003300*** END OF VILMAII-COPY LENGTH= 155 BYTES                                 
