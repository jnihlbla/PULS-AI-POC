000100 01  MID-W2I35101.                                                        
000200*                                 MID-COPYTEXT FÖR W2035100               
000300     03 MID-IDTYPE-2351-IN   PIC X.                                       
000400     03 MID-IDTYPE-2351-UT   PIC X(5).                                    
000500     03 MID-IDDC-2351-IN     PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-IDDC-2351-UT     PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-GRP              OCCURS 13 TIMES.                             
001000        05 MID-SELECT-2351   PIC X.                                       
001100        05 MID-IDPERSON-2351 PIC 9(3).                                    
001200*                                 PERSONKOD REFILLANSVARIG                
001300*** END OF VILMAII-COPY LENGTH= 62 BYTES                                  
