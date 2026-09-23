000100 01  MOD-W4O27601-CTX.                                                    
000200*                                 MOD COPYTEXT FÖR W4027500               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-FLLAEST-ATTR     PIC X(2).                                    
000800*                                 MFS ATTRIBUTFÄLT                        
000900     03 MOD-FLLAEST          PIC X.                                       
001000*                                 JA/NEJ-FLAGGA                           
001100     03 MOD-IDDISTR-UT       PIC Z(5).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MOD-IDKUNDNR-UT      PIC Z(7).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDKUNDRF-UT      PIC X(10).                                   
001600*                                 KUNDENS REFERENS (ORDERID)              
001700     03 MOD-IDARTNR-UT       PIC Z(8)9.                                   
001800*                                 ARTIKELNUMMER                           
001900     03 MOD-KVANTAL-UT       PIC Z(7).                                    
002000*                                 ANTAL                                   
002100     03 MOD-IDDISTR          PIC X(4).                                    
002200*                                 DISTRIKTNUMMER                          
002300     03 MOD-IDKUNDNR         PIC X(6).                                    
002400*                                 KUNDNUMMER                              
002500     03 MOD-IDORDNR7         PIC X(7).                                    
002600*                                 ORDERNUMMER                             
002700     03 MOD-TIREGDAT-URSP    PIC X(6).                                    
002800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002900     03 MOD-IDARTNR          PIC X(9).                                    
003000*                                 ARTIKELNUMMER                           
003100     03 MOD-TIREGTID-URSP    PIC X(8).                                    
003200*                                 KLOCKSLAG (TTMMSSTH)                    
003300     03 MOD-TIREGDAT-AVV     PIC X(6).                                    
003400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003500     03 MOD-TIREGTID-AVV     PIC X(8).                                    
003600*                                 KLOCKSLAG (TTMMSSTH)                    
003700     03 MOD-TEVOREXT         PIC X(310).                                  
003800*                                 VOR MESSAGE E                           
003900     03 MOD-TEVORSC-ATTR     PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-TEVORSC          PIC X(310).                                  
004200*                                 VOR MESS. SC                            
004300     03 MOD-TEVORDEL-ATTR    PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-TEVORDEL         PIC X(310).                                  
004600*                                 VOR MESS. DEL                           
004700     03 MOD-TEMFSINF         PIC X(55).                                   
004800*                                 INFORMATIONSMEDDELANDE                  
004900*** END OF VILMAII-COPY LENGTH= 1128 BYTES                                
