000100 01  STDP-W416STDP.                                                       
000200*                                 LÄNKAREA TILL W416STDP - BERÄK-         
000300*                                 NING AV STANDARDPRIS FÖR NYA            
000400*                                 SATSARTIKLAR                            
000500     03 STDP-IDSYSTEM        PIC X(4).                                    
000600*                                 SKAPANDE SYSTEMNUMMER                   
000700     03 STDP-IDARTNR-IN      PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 STDP-TIAAMMDD-IN     PIC S9(7)           COMP-3.                  
001000*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001100     03 STDP-PRARTSTD-UT     PIC S9(7)V9(2)      COMP-3.                  
001200*                                 ARTIKELSTANDARDPRIS                     
001300*** END COPY W416STDPC0  LENGTH=18                                        
