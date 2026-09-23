000100 01  W41261.                                                              
000200*                                 SIGNALFIL FÖR PROFORMA                  
000300*                                 FÖRFALLNA/BORTTAGSMARKERADE EL          
000400*                                 SOM FÖRFALLER INOM 4 VECKOR             
000500     03 IDDISTR              PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 IDKUNDRF             PIC X(10).                                   
001000*                                 KUNDENS REFERENS (ORDERID)              
001100     03 BEBET.                                                            
001200*                                 BETALNINGSANSVARIG NAMN                 
001300        05 BEBETRAD-1        PIC X(35).                                   
001400*                                 DEL AV BETALNINGSANSVARIGS NAMN         
001500        05 BEBETRAD-2        PIC X(35).                                   
001600*                                 DEL AV BETALNINGSANSVARIGS NAMN         
001700     03 IDUSER               PIC X(8).                                    
001800*                                 ANVÄNDARENS SÄKERHETS ID                
001900     03 TIFORDAT             PIC S9(7)           COMP-3.                  
002000*                                 FÖRFALLODATUM                           
002100     03 SUORDV               PIC S9(9)V9(2)      COMP-3.                  
002200*                                 SUMMA ORDERVÄRDE                        
002300     03 FILLER               PIC X.                                       
002400*** END OF VILMAII-COPY LENGTH= 106 BYTES                                 
