000100 01  MID-W6I17401.                                                        
000200*                                 MID-COPYTEXT FÖR W60174                 
000300     03 MID-IDLEVNR-IN       PIC X(5).                                    
000400*                                 LEVERANTÖRNUMMER                        
000500     03 MID-IDLEVNR-UT       PIC X(5).                                    
000600*                                 LEVERANTÖRNUMMER                        
000700     03 MID-DASUPREF-IN      PIC 9(6).                                    
000800*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
000900     03 MID-DASUPREF-UT      PIC 9(6).                                    
001000*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001100     03 MID-IDSUPREF-IN      PIC X(10).                                   
001200*                                 LEVERANTöRSREF.                         
001300     03 MID-IDSUPREF-UT      PIC X(10).                                   
001400*                                 LEVERANTöRSREF.                         
001500     03 MID-INPUT            OCCURS 15 TIMES.                             
001600        05 MID-CMD           PIC X.                                       
001700*** END OF VILMAII-COPY LENGTH= 57 BYTES                                  
