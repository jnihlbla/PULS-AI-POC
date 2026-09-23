000100 01  SEQB-WDE4B1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDE411             
000300*                                 FYSISK NYCKEL: WDE4B1KY                 
000400*                                 (IDPRODNR, IDPURAD)                     
000500*                                 SECONDARY KEY: WDE4BSEQ                 
000600*                                 (IDPRODNR, IDPURAD)                     
000700     03 SEQB-IDPRODNR        PIC S9(7)           COMP-3.                  
000800*                                 PRODUKTIONSNUMMER                       
000900*                                 PRODUCTION NUMBER                       
001000     03 SEQB-IDPURAD         PIC S9(5)           COMP-3.                  
001100*                                 RADNUMMER PÅ PACKUNDERLAG               
001200*                                 LINENO IN PACKINGDOCUMENT               
001300     03 SEQB-KVANNANT        PIC S9(7)           COMP-3.                  
001400*                                 ANNULLERAT ANTAL ARTIKLAR               
001500*                                 CANCELLED QUANTITY                      
001600     03 SEQB-KVAVBART        PIC S9(7)           COMP-3.                  
001700*                                 AVBOKAT ANTAL ARTIKLAR                  
001800*                                 ALLOCATED QUANTITY                      
001900     03 SEQB-KVBEART         PIC S9(7)           COMP-3.                  
002000*                                 BESTÄLLT ANTAL STYCKEN                  
002100*                                 ORDERED QUANTITY                        
002200     03 SEQB-IDPLKLST        PIC S9(3)           COMP-3.                  
002300*                                 PLOCKLISTNUMMER                         
002400*                                 PICKING LIST NUMBER                     
002500*** END OF VILMAII-COPY LENGTH= 21 BYTES                                  
