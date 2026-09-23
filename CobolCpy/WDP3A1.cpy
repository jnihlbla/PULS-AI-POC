000100 01  SEQA-WDP3A1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDP311             
000300*                                 ANSVARIG / ARTIKEL                      
000400*                                 FYSISK NYCKEL WDP3A1KY:                 
000500*                                 (IDLANDX2, IDARTNRF, IDARTNRT,          
000600*                                  KDARBTYP)                              
000700*                                 SECONDARY KEY WDP3ASEQ:                 
000800*                                 (IDLANDX2, IDARTNRF, IDARTNRT)          
000900     03 SEQA-IDLANDX2        PIC X(2).                                    
001000*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001100*                                 2-LETTER CODE FOR COUNTRY               
001200     03 SEQA-IDARTNR-FOM     PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER FRÅN OCH MED              
001400*                                 FROM PART NUMBER                        
001500     03 SEQA-IDARTNR-TOM     PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER TILL OCH MED              
001700*                                 TO PART NUMBER                          
001800     03 SEQA-KDARBTYP        PIC X(8).                                    
001900*                                 TYP AV ARBETE                           
002000*                                 CATEGORY OF WORK                        
002100     03 SEQA-IDPERSON        PIC S9(3)           COMP-3.                  
002200*                                 PERSONKOD                               
002300*                                 STAFF CODE                              
002400*** END OF VILMAII-COPY LENGTH= 22 BYTES                                  
