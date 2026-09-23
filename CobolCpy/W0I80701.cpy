000100 01  W0I80701.                                                            
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W0I80701                                
000400*                                                                         
000500     03 KDEMBTYP-IN          PIC X(3).                                    
000600*                                 EMBALLAGETYP       KDEMBTYP-002         
000700     03 KDEMBTYP-UT          PIC X(3).                                    
000800*                                 EMBALLAGETYP       KDEMBTYP-002         
000900     03 KDUPPD-BEEMBTYP-IN   PIC X.                                       
001000*                                 UPPDATERINGSTYP                         
001100     03 KDUPPD-BEEMBTYP-UT   PIC X.                                       
001200*                                 UPPDATERINGSTYP                         
001300     03 BEEMBTYP-GRUPP.                                                   
001400        05 BEEMBTYP          OCCURS 6 TIMES                               
001500                             PIC X(12).                                   
001600*                                 EMBALLAGETYPSTEXT  BEEMBTYP-002         
001700*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
