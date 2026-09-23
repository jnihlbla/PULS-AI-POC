000100 01  SEQC-WDP3C1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDP311             
000300*                                 ANSVARIG / FUNKTIONSGRUPP               
000400*                                 FYSISK NYCKEL WDP3C1KY:                 
000500*                                 (IDLANDX2, IDFKNGRF, IDFKNGRT,          
000600*                                  KDARBTYP)                              
000700*                                 SECONDARY KEY WDP3CSEQ:                 
000800*                                 (IDLANDX2, IDFKNGRF, IDFKNGRT)          
000900     03 SEQC-IDLANDX2        PIC X(2).                                    
001000*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001100*                                 2-LETTER CODE FOR COUNTRY               
001200     03 SEQC-IDFKNGRP-FOM    PIC S9(5)           COMP-3.                  
001300*                                 FUNKTIONSGRUPP-FROM                     
001400*                                 FUNCTION-GROUP FROM                     
001500     03 SEQC-IDFKNGRP-TOM    PIC S9(5)           COMP-3.                  
001600*                                 FUNKTIONSGRUPP-TOM                      
001700*                                 FUNCTION-GROUP UP TO                    
001800     03 SEQC-KDARBTYP        PIC X(8).                                    
001900*                                 TYP AV ARBETE                           
002000*                                 CATEGORY OF WORK                        
002100     03 SEQC-IDPERSON        PIC S9(3)           COMP-3.                  
002200*                                 PERSONKOD                               
002300*                                 STAFF CODE                              
002400*** END OF VILMAII-COPY LENGTH= 18 BYTES                                  
