000100 01  SEQB-WDE6B1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDE601             
000300*                                 EXIT: INDEX FINNS NÄR                   
000400*                                 KDORDSTA  = 3                           
000500*                                 FYSISK NYCKEL: WDE6B1KY                 
000600*                                 (KDPERSON, DABEGPAC, KDORDSTA,          
000700*                                  IDPRODNR)                              
000800*                                 SECONDRY KEY: WDE6BSEQ                  
000900*                                 (KDPERSON, DABEGPAC, KDORDSTA)          
001000     03 SEQB-KDPERSON        PIC S9(3)           COMP-3.                  
001100*                                 PERSONKOD                               
001200*                                 STAFF CODE                              
001300     03 SEQB-DABEGPAC        PIC 9(8).                                    
001400*                                 BEGÄRD PACKNINGSDAG  (YYYYMMDD)         
001500*                                 REQUESTED PACKING DATE                  
001600     03 SEQB-KDORDSTA        PIC S9              COMP-3.                  
001700*                                 VOLVOORDERSTATUS                        
001800*                                 VOLVO ORDER STATUS                      
001900     03 SEQB-IDPRODNR        PIC S9(7)           COMP-3.                  
002000*                                 PRODUKTIONSNUMMER                       
002100*                                 PRODUCTION NUMBER                       
002200*** END OF VILMAII-COPY LENGTH= 15 BYTES                                  
