000100 01  MID-W5I21301.                                                        
000200*                                 MID-COPYTEXT FÖR W50213                 
000300     03 MID-KDEKHHT-IN       PIC X(3).                                    
000400*                                 EKONOMISK HUVUDHÄNDELSE                 
000500     03 MID-KDEKSHT-IN       PIC X(3).                                    
000600*                                 EKONOMISK SUBHÄNDELSE                   
000700     03 MID-KDEKNIVA-IN      PIC X(5).                                    
000800*                                 EKONOMISK HÄNDELSENIVÅ                  
000900     03 MID-IDSYSMOT-IN      PIC X(6).                                    
001000*                                 PULS MOTTAGANDE SYSTEMNAMN              
001100     03 MID-IDPTYP-IN        PIC X(3).                                    
001200*                                 POSTTYP                                 
001300     03 MID-TABELLRAD        OCCURS 15 TIMES.                             
001400*                                 GRUPP MED TABELL RADER                  
001500        05 MID-CMD           PIC X.                                       
001600*** END OF VILMAII-COPY LENGTH= 35 BYTES                                  
