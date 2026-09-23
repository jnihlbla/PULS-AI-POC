000100 01  W4260502.                                                            
000200*                                 KVALITET FELF÷RDELN. OMR.               
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600*                                 RECORD TYPE                             
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 IDKVAOMR             PIC X.                                       
001100*                                 KVALITET KONTROLLOMR≈DE                 
001200*                                 QUALITY CONTROL AREA                    
001300     03 TIAARP-FOM           PIC S9(5)           COMP-3.                  
001400*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
001500*                                 FOM 12 PER ≈R                           
001600*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
001700*                                 FROM 12 PER YEAR                        
001800     03 TIAARP-TOM           PIC S9(5)           COMP-3.                  
001900*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
002000*                                 TOM 12 PER ≈R                           
002100*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
002200*                                 TO 12 PER YEAR                          
002300     03 KVKVAFEL             PIC S9(5)           COMP-3.                  
002400*                                 ANTAL FEL F÷R KVALITETSKONTROLL         
002500*                                 NUMBER OF ERROR FOR QUALITY CON         
002600*                                 TROL                                    
002700     03 KVART                PIC S9(7)           COMP-3.                  
002800*                                 ANTAL ARTNR PER BRYTBEGREPP             
002900*                                 NO OF PARTNOS PER TYPE                  
003000     03 KVARTFEL             PIC S9(5)           COMP-3.                  
003100*                                 ANTAL FELAKTIGA ARTIKLAR F÷R KV         
003200*                                 ALITETSKONTROLL                         
003300*                                 NUMBER OF ERROR PARTNUMBER FOR          
003400*                                 QUALITY CONTROL                         
003500*** END OF VILMAII-COPY LENGTH= 22 BYTES                                  
