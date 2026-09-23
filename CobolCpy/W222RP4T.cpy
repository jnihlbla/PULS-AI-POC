000100 01  W222RP4T.                                                            
000200*                                 UPPDATERING ANTALSMÄSSIGA               
000300*                                 PB-JUSTERINGAR                          
000400*                                                                         
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDARTNR              PIC 9(8).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 KDCLAGER             PIC 9.                                       
001000*                                 CENTRALLAGERKOD                         
001100     03 PB-JUSTERINGAR       OCCURS 2 TIMES.                              
001200        05 TIPBJUST-X.                                                    
001300           07 TIPBJUST       PIC 9(4).                                    
001400*                                 DATUM FÖR PB-JUSTERING (ÅÅVV)           
001500        05 KVPB-JUST-X.                                                   
001600           07 KVPB-JUST      PIC 9(6)V9(1).                               
001700*                                 PERIODBEHOVSJUSTERING                   
001800     03 FLABORT-PBJUST       PIC X.                                       
001900*                                 BORTTAG AV BEFINTLIG                    
002000*                                 PB-JUSTERING?                           
002100*** END COPY W222RP4TC0  LENGTH=35                                        
