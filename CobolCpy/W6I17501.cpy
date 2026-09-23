000100 01  MID-W6I17501.                                                        
000200*                                 MID-COPYTEXT FÖR W60175                 
000300     03 MID-IDLEVNR-IN       PIC X(5).                                    
000400*                                 LEVERANTÖRNUMMER                        
000500     03 MID-IDLEVNR-UT       PIC X(5).                                    
000600*                                 LEVERANTÖRNUMMER                        
000700     03 MID-TISUPREF-IN      PIC X(6).                                    
000800*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
000900     03 MID-TISUPREF-UT      PIC X(6).                                    
001000*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001100     03 MID-IDSUPREF-IN      PIC X(10).                                   
001200*                                 LEVERANTöRSREF.                         
001300     03 MID-IDSUPREF-UT      PIC X(10).                                   
001400*                                 LEVERANTöRSREF.                         
001500     03 MID-IDDISTR-IN       PIC X(4).                                    
001600*                                 DISTRIKTNUMMER                          
001700     03 MID-IDDISTR-UT       PIC X(4).                                    
001800*                                 DISTRIKTNUMMER                          
001900     03 MID-INPUT            OCCURS 15 TIMES.                             
002000        05 MID-CMD           PIC X.                                       
002100*** END OF VILMAII-COPY LENGTH= 65 BYTES                                  
