000100 01  MOD-W4O40901.                                                        
000200*                                 MODCOPYTEXT TILL W4O409.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN-ATTR     PIC X(2).                                    
000800*                                 MFS ATTRIBUTFÄLT                        
000900     03 MOD-IDDC-IN          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-IDDC-UT          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-KDPROGOI-IN-ATTR PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500     03 MOD-KDPROGOI-IN      PIC X.                                       
001600*                                 TYP PROGNOS ORDERINGÅNG                 
001700     03 MOD-KDPROGOI-UT      PIC X(23).                                   
001800     03 MOD-IDDC-COPY-IN-ATTR                                             
001900                             PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-IDDC-COPY-IN     PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-KDPROGOI-COPY-IN-ATTR                                         
002400                             PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 MOD-KDPROGOI-COPY-IN PIC X.                                       
002700*                                 TYP PROGNOS ORDERINGÅNG                 
002800     03 MOD-DEFAULT-IN-ATTR  PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 MOD-DEFAULT-IN       PIC X.                                       
003100     03 MOD-OUTPUT.                                                       
003200        05 MOD-RAD-IN        OCCURS 12 TIMES.                             
003300           07 MOD-REFTREND-NORM-IN-ATTR                                   
003400                             PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600           07 MOD-REFTREND-NORM-IN                                        
003700                             PIC X(3).                                    
003800*                                 TRENDFAKTOR NORMAL                      
003900           07 MOD-REFTREND-SVAG-IN-ATTR                                   
004000                             PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200           07 MOD-REFTREND-SVAG-IN                                        
004300                             PIC X(3).                                    
004400*                                 TRENDFAKTOR SVAG                        
004500           07 MOD-REFTREND-STARK-IN-ATTR                                  
004600                             PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800           07 MOD-REFTREND-STARK-IN                                       
004900                             PIC X(3).                                    
005000*                                 TRENDFAKTOR STARK                       
005100        05 MOD-RAD-UT        OCCURS 12 TIMES.                             
005200           07 MOD-PERIOD-UT  PIC 9(2).                                    
005300           07 MOD-REFTREND-NORM-UT                                        
005400                             PIC Z(2)9.                                   
005500*                                 TRENDFAKTOR NORMAL                      
005600           07 MOD-REFTREND-SVAG-UT                                        
005700                             PIC Z(2)9.                                   
005800*                                 TRENDFAKTOR SVAG                        
005900           07 MOD-REFTREND-STARK-UT                                       
006000                             PIC Z(2)9.                                   
006100*                                 TRENDFAKTOR STARK                       
006200     03 MOD-IDUSER-UT        PIC X(7).                                    
006300*                                 ANVÄNDARENS SÄKERHETS ID                
006400     03 MOD-TIUPPDAT-UT      PIC 9(6).                                    
006500*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
006600     03 MOD-TEMFSINF         PIC X(55).                                   
006700*                                 INFORMATIONSMEDDELANDE                  
006800*** END OF VILMAII-COPY LENGTH= 466 BYTES                                 
