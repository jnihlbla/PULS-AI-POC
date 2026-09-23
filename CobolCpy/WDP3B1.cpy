000100 01  SEQB-WDP3B1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDP311             
000300*                                 ANSVARIG / LEVERANTÖR                   
000400*                                 FYSISK NYCKEL WDP3B1KY:                 
000500*                                 (IDLANDX2, IDLEVNR, KDARBTYP)           
000600*                                 SECONDARY KEY WDP3BSEQ:                 
000700*                                 (IDLANDX2, IDLEVNR)                     
000800     03 SEQB-IDLANDX2        PIC X(2).                                    
000900*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001000*                                 2-LETTER CODE FOR COUNTRY               
001100     03 SEQB-IDLEVNR         PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001400     03 SEQB-KDARBTYP        PIC X(8).                                    
001500*                                 TYP AV ARBETE                           
001600*                                 CATEGORY OF WORK                        
001700     03 SEQB-IDPERSON        PIC S9(3)           COMP-3.                  
001800*                                 PERSONKOD                               
001900*                                 STAFF CODE                              
002000*** END OF VILMAII-COPY LENGTH= 17 BYTES                                  
