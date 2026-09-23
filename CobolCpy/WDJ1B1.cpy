000100 01  SEQB-WDJ1B1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDJ101             
000300*                                 SATSSTRUKTUWREGISTER                    
000400*                                 FYSISK NYCKEL: WDJ1B1KY                 
000500*                                 (IDTSPEC + IDARTNR)                     
000600*                                 SEKUNDÄR NYCKEL: WDJ1BSEQ               
000700*                                 (IDTSPEC + IDARTNR)                     
000800     03 SEQB-IDTSPEC         PIC X(15).                                   
000900*                                 LIKARTADE STRUKTURER                    
001000*                                 SIMILAR STRUCTURES                      
001100     03 SEQB-IDARTNR         PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300*                                 PART NUMBER                             
001400*** END COPY WDJ1B1CCC0  LENGTH=20                                        
