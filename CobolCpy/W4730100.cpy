000100 01  W4730100-CTX.                                                        
000200*                                 PACKNING KOLLI TILL SVENSKA ÅF          
000300*                                 POSTTYP   100                           
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDISTR              PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 IDORDNR7             PIC S9(7)           COMP-3.                  
001100*                                 ORDERNUMMER                             
001200     03 KDORDKL              PIC S9              COMP-3.                  
001300      88 KDORDKL-VOR         VALUE +0.                                    
001400      88 KDORDKL-DAG         VALUE +1.                                    
001500      88 KDORDKL-2           VALUE +2.                                    
001600      88 KDORDKL-SNABB       VALUE +2.                                    
001700      88 KDORDKL-SPECIAL     VALUE +3.                                    
001800      88 KDORDKL-KVANT       VALUE +4.                                    
001900      88 KDORDKL-SATS        VALUE +5.                                    
002000*                                 ORDERKLASS                              
002100*** END OF VILMAII-COPY LENGTH= 18 BYTES                                  
