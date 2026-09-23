000100 01  W231215-NY-CTX.                                                      
000200*                                 REGISTER ÷VER ANTAL TILL OCH            
000300*                                 FR≈N LAGER 12 PERIODER BAK≈T            
000400*                                                                         
000500*                                 SENASTE PERIOD HAR INDEX = 1            
000600     03 IDPTYP               PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 IDARTNR              PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 W231215-NY-001-GRP   OCCURS 12 TIMES.                             
001100        05 TIAARP            PIC S9(5)           COMP-3.                  
001200*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
001300*                                 12 PER ≈R                               
001400        05 W231215-002-GRP   OCCURS 2 TIMES.                              
001500           07 KVANTAL-UTLEV  PIC S9(7)           COMP-3.                  
001600*                                 ANTAL                                   
001700           07 KVANTAL-TILL   PIC S9(7)           COMP-3.                  
001800*                                 ANTAL                                   
001900           07 KVANTAL-FRAN   PIC S9(7)           COMP-3.                  
002000*                                 ANTAL                                   
002100*** END OF VILMAII-COPY LENGTH= 332 BYTES                                 
