000100 01  MOD-W0O70801.                                                        
000200*                                 COPYTEXT FÖR MOD W0O70801               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 FELMEDDELANDEFÄLT                       
000700     03 MOD-IDRUTIN-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDRUTIN-UT       PIC X(8).                                    
001000*                                 RUTINNAMN (GRUPP AV JOBB)               
001100     03 MOD-IDJOB-IN         PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDJOB-UT         PIC X(8).                                    
001400*                                 JOBBNAMN                                
001500     03 MOD-IDJOB-SKIP       PIC X(8).                                    
001600*                                 JOBBNAMN                                
001700     03 MOD-LINES            OCCURS 11 TIMES                              
001800                             INDEXED MOD-IX-LINE.                         
001900        05 MOD-IDJOB         PIC X(8).                                    
002000*                                 JOBBNAMN                                
002100        05 MOD-TIREGDAT      PIC 9(6).                                    
002200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002300        05 MOD-TIUPPDAT      PIC 9(6).                                    
002400*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
002500        05 MOD-TIUPPTID      PIC 9(6).                                    
002600*                                 UPPDATERINGSTID  (TTMMSSTH)             
002700        05 MOD-KDTRSTAT      PIC 9.                                       
002800*                                 TRANSAKTIONSSTATUS                      
002900        05 MOD-TETRSTAT      PIC X(15).                                   
003000*                                 TRANSAKTIONSSTATUS I KLARTEXT           
003100        05 MOD-BEJOB         PIC X(25).                                   
003200*                                 JOB BESKRIVNING                         
003300     03 MOD-BEFUNK-ATTR      PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 MOD-BEFUNK           PIC X(2).                                    
003600*                                 MFS BEHANDLING AV INPUTFÄLT             
003700     03 MOD-IDJOB-AKTIV-ATTR PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 MOD-IDJOB-AKTIV      PIC X(2).                                    
004000*                                 MFS BEHANDLING AV INPUTFÄLT             
004100     03 MOD-TEMFSINF         PIC X(61).                                   
004200*                                 INFORMATIONSMEDDELANDE                  
004300*** END COPY W0O70801C0  LENGTH=878                                       
