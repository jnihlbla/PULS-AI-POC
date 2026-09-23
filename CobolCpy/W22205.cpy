000100 01  OUT-W22205.                                                          
000200*                                 OUT-COPYTEXT FÖR W2220500               
000300     03 OUT-IDARTNR          PIC 9(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 OUT-TIAAVV           PIC S9(5)           COMP-3.                  
000600*                                 ÅR - VECKA  (ÅÅVV)                      
000700     03 OUT-KVPB-SEP         PIC S9(6)V9(1)      COMP-3.                  
000800*                                 SEPARAT PERIODBEHOV                     
000900     03 OUT-KVPB-SATS        PIC S9(6)V9(1)      COMP-3.                  
001000*                                 SATS-PERIODBEHOV                        
001100     03 OUT-KVPB-TPO         PIC S9(6)V9(1)      COMP-3.                  
001200*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
001300     03 OUT-KVPB-SDC         PIC S9(6)V9(1)      COMP-3.                  
001400*                                 PERIODBEHOV FÖR SAMTL SDC:ER            
001500     03 OUT-KVPB-NDC         PIC S9(6)V9(1)      COMP-3.                  
001600*                                 PERIODBEHOV (PROGNOS)                   
001700     03 OUT-KVPB-TREND       PIC S9(6)V9(1)      COMP-3.                  
001800*                                 PERIODTRENDVÄRDE                        
001900     03 OUT-KVPB-PLAN        PIC S9(6)V9(1)      COMP-3.                  
002000*                                 PLANERAT PERIODBEHOV                    
002100*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
