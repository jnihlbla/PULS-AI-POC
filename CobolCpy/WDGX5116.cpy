000100 01  5116-WDGX5116.                                                       
000200*                                 INVENTERING                             
000300*                                 ARTIKLAR MED UTREDNINGS-                
000400*                                 SALDO                                   
000500*                                 FYSISK NYCKEL: WDGXKEY                  
000600*                                 (IDDC     + IDARTNR +                   
000700*                                  LOW-VALUE)                             
000800     03 5116-IDDC            PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 5116-IDARTNR         PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300*                                 PART NUMBER                             
001400     03 FILLER               PIC X(8).                                    
001500     03 5116-IDPRODNR        PIC S9(7)           COMP-3.                  
001600*                                 PRODUKTIONSNUMMER                       
001700*                                 PRODUCTION-NUMBER                       
001800     03 5116-TIUPPDAT        PIC S9(7)           COMP-3.                  
001900*                                 UPPDATERINGSDATUM  (≈≈MMDD)             
002000*                                 UPDATING DATE     (YYMMDD)              
002100     03 5116-TIUPPTID        PIC S9(9)           COMP-3.                  
002200*                                 UPPDATERINGSTID  (TTMMSSTH)             
002300*                                 UPDATING TIME    (HHMMSSTH)             
002400     03 5116-KDORDKL         PIC S9              COMP-3.                  
002500*                                 ORDERKLASS                              
002600*                                 ORDER CLASS                             
002700     03 5116-IDPW            PIC X(8).                                    
002800*                                 PASSWORD   (L÷SENORD)                   
002900*                                 PASSWORD                                
003000     03 FILLER               PIC X(28).                                   
