000100 01  MID-W2I39101.                                                        
000200*                                 MID-COPYTEXT FOR W2039100               
000300     03 MID-IDTYPE-2391-IN   PIC X.                                       
000400     03 MID-IDDC-REF-2391-IN PIC X(2).                                    
000500*                                 SÄNDANDE LAGER FÖR REFILL               
000600     03 MID-IDTYPE-2391-UT   PIC X(5).                                    
000700     03 MID-IDDC-REF-2391-UT PIC X(2).                                    
000800*                                 SÄNDANDE LAGER FÖR REFILL               
000900     03 MID-GRP              OCCURS 13 TIMES.                             
001000        05 MID-SELECT-2391   PIC X.                                       
001100        05 MID-IDPERSON-2391 PIC 9(3).                                    
001200*                                 PERSONKOD REFILLANSVARIG                
001300     03 MID-IDDC-REF-2391    PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*** END OF VILMAII-COPY LENGTH= 64 BYTES                                  
