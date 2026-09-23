000100 01  W222289.                                                             
000200     03 IDPTYP               PIC X(3).                                    
000300*                                 POSTTYP                                 
000400     03 IDSORTFLT.                                                        
000500        05 RANDKEY           PIC X(4).                                    
000600        05 IDARTNR           PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 TIAAP                PIC S9(3)           COMP-3.                  
000900*                                 ≈R - PLANERINGSPERIOD (≈≈P)             
001000     03 CLAGER-MARKNAD       OCCURS 2 TIMES.                              
001100*                                 1 ORDERING≈NG C1-LAGER/MARKN.           
001200*                                 2 ORDERING≈NG C2-LAGER/MARKN.           
001300        05 KVOI-PROG         PIC S9(7)           COMP-3.                  
001400*                                 PROGNOSP≈VERKANDE ORDERING≈NG           
001500        05 KVOI-SATS         PIC S9(7)           COMP-3.                  
001600*                                 ORDERING≈NG SATSF÷RBRUKNING             
001700*** END COPY W222289CC0  LENGTH=30                                        
