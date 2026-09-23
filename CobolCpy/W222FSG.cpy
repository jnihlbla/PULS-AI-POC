000010*** EDIT ALLOWED                                                          
000100 01  W222FSG.                                                             
000200*                                 TABELL FAKTORER ONORMAL FÖR-            
000300*                                 SÄLJNING. ANVÄNDS FÖR ATT AV-           
000400*                                 GÖRA OM ONORMAL OI I NÅGON              
000500*                                 PERIOD VID PROGNOSBERÄKNING.            
000510*                                 GRÄNSVÄRDET ANVÄNDS I PROGNOS-          
000520*                                 BERÄKNINGEN ISTÄLLET FÖR DEN            
000530*                                 FAKTISKA ORDERINGÅNGEN.                 
000600*                                 (A * PROGNOS) + B                       
000700*                                                                         
000900        05 FILLER PIC X(15) VALUE '000000050 02512'.                      
001000        05 FILLER PIC X(15) VALUE '000000250 02107'.                      
001100        05 FILLER PIC X(15) VALUE '000001000 01806'.                      
001200        05 FILLER PIC X(15) VALUE '000005000 01604'.                      
001300        05 FILLER PIC X(15) VALUE '000030000 01503'.                      
001400        05 FILLER PIC X(15) VALUE '000100000 01402'.                      
001500        05 FILLER PIC X(15) VALUE '999999999 01201'.                      
002901                                                                          
002991                                                                          
002992 01  FILLER REDEFINES W222FSG.                                            
003100        05 FAKT             OCCURS 7.                                     
003200           07 PRARTSTD-MAX  PIC 9(7)V9(2).                                
003300           07 FILLER        PIC X.                                        
003400           07 A             PIC 9(2)V9.                                   
003500           07 B             PIC 9(2).                                     
003510                                                                          
003600*** END COPY W222FSG     LENGTH=105                                       
