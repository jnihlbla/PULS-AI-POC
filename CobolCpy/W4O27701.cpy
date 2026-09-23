000100 01  MOD-W4O27701-CTX.                                                    
000200*                                 MOD COPYTEXT FÖR W4027700               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-UT       PIC Z(5).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDKUNDNR-UT      PIC Z(7).                                    
001000*                                 KUNDNUMMER                              
001100     03 MOD-IDKUNDRF-UT      PIC X(10).                                   
001200*                                 KUNDENS REFERENS (ORDERID)              
001300     03 MOD-IDARTNR-UT       PIC Z(8)9.                                   
001400*                                 ARTIKELNUMMER                           
001500     03 MOD-KVANTAL-UT       PIC Z(7).                                    
001600*                                 ANTAL                                   
001700     03 MOD-IDDISTR          PIC X(4).                                    
001800*                                 DISTRIKTNUMMER                          
001900     03 MOD-IDKUNDNR         PIC X(6).                                    
002000*                                 KUNDNUMMER                              
002100     03 MOD-IDORDNR7         PIC X(7).                                    
002200*                                 ORDERNUMMER                             
002300     03 MOD-TIREGDAT-URSP    PIC X(6).                                    
002400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002500     03 MOD-IDARTNR          PIC X(9).                                    
002600*                                 ARTIKELNUMMER                           
002700     03 MOD-TIREGTID-URSP    PIC X(8).                                    
002800*                                 KLOCKSLAG (TTMMSSTH)                    
002900     03 MOD-TIREGDAT-AVV     PIC X(6).                                    
003000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003100     03 MOD-TIREGTID-AVV     PIC X(8).                                    
003200*                                 KLOCKSLAG (TTMMSSTH)                    
003300     03 MOD-TEVORINT         PIC X(230).                                  
003400*                                 VOR MESSAGE I                           
003500     03 MOD-TEVOREXT         PIC X(310).                                  
003600*                                 VOR MESSAGE E                           
003700     03 MOD-TEVORSC          PIC X(310).                                  
003800*                                 VOR MESS. SC                            
003900     03 MOD-TEVORNOT         OCCURS 4 TIMES                               
004000                             PIC X(64).                                   
004100     03 MOD-TELOSNOT-ATTR    PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-TELOSNOT         PIC X(150).                                  
004400*                                 LOS MESSAGE                             
004500     03 MOD-TEMFSINF         PIC X(55).                                   
004600*                                 INFORMATIONSMEDDELANDE                  
004700*** END OF VILMAII-COPY LENGTH= 1449 BYTES                                
