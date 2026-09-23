000100 01  OUT-W22207.                                                          
000200*                                 OUT-COPYTEXT FÖR W2220700               
000300     03 OUT-IDARTNR          PIC 9(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 OUT-TIAARP           PIC S9(5)           COMP-3.                  
000600*                                 ÅR - REDOVISNINGSPERIOD (ÅÅRP)          
000700*                                 12 PER ÅR (OCKSÅ LOGISTIKPER)           
000800     03 OUT-KVPB-PLAN        PIC S9(6)V9(1)      COMP-3.                  
000900*                                 PLANERAT PERIODBEHOV                    
001000     03 OUT-DAPBPLAN         PIC 9(8).                                    
001100*                                 DATUM KVPB-PLAN GILTIG TOM              
001200     03 OUT-DASEASON         PIC 9(8).                                    
001300*                                 DATUM RESEASON-LEDTID GILTIG TO         
001400*                                 M                                       
001500     03 OUT-RESEASON-PLAN    PIC S9V9(2)         COMP-3.                  
001600*                                 SÄSONGSINDEX                            
001700     03 OUT-KVPB-SATS        PIC S9(6)V9(1)      COMP-3.                  
001800*                                 SATS-PERIODBEHOV                        
001900*** END OF VILMAII-COPY LENGTH= 38 BYTES                                  
