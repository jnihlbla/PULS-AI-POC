000100 01  SEQD-WDJ1D1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDJ101             
000300*                                 SATSSTRUKTUWREGISTER                    
000400*                                 FYSISK NYCKEL: WDJ1D1KY                 
000500*                                  (IDUSER + IDARTNR)                     
000600*                                 SEKUNDÄR NYCKEL: WDJ1DSEQ               
000700*                                  (IDUSER + IDARTNR)                     
000800     03 SEQD-IDUSER          PIC X(8).                                    
000900*                                 ANVÄNDARENS SÄKERHETS ID                
001000*                                 USER SECURITY-IDENTITY                  
001100     03 SEQD-IDARTNR         PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300*                                 PART NUMBER                             
001400*** END COPY WDJ1D1CCC0  LENGTH=13                                        
