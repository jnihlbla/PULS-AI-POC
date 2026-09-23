000100 01  W092X9.                                                              
000200*                                 COPYTEXT TILL PROGRAM W0920800          
000300     03 TIAAMMDD             PIC S9(7)           COMP-3.                  
000400*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
000500     03 TIKLOCK              PIC 9(8).                                    
000600*                                 KLOCKSLAG (HHMMSSTH)                    
000700     03 IDLOGLOP             PIC S9              COMP-3.                  
000800*                                 L÷PNUMMER I LOGGPOST                    
000900*                                 (÷KAS MED 1 VARJE G≈NG                  
001000*                                 F÷R ATT F≈ UNIK NYCKEL)                 
001100     03 IDPTYP               PIC X(3).                                    
001200*                                 POSTTYP                                 
001300*** END COPY W092X9CCC0  LENGTH=16                                        
