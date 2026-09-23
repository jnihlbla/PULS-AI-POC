000100 01  SEQD-WDD2D1.                                                         
000200*                                 NYA ARTIKLAR FRÅN PV OCH LV             
000300*                                 SEKUNDÄRT INDEX TILL WDD201             
000400*                                 BASLAGER MARKNADS KÖ                    
000500*                                 FYSISK NYCKEL: WDD2D1KY                 
000600*                                  (IDPROJ, KDBASLM,                      
000700*                                   IDFKNGRP, IDARTNR)                    
000800*                                 SÖKFÄLT: IDFKNGRP, KDBPSR               
000900*                                 SECONDARY NYCKEL: WDD2DSEQ              
001000*                                  (IDPROJ, KDBASLM)                      
001100     03 SEQD-IDPROJ          PIC X(4).                                    
001200*                                 PARTS PROJEKTIDENTITET                  
001300*                                 PARTS PROJECT IDENTITY                  
001400     03 SEQD-KDBASLM         PIC X(6).                                    
001500*                                 BASLAGERMARKNAD                         
001600*                                 BASIC STOCK MARKET                      
001700     03 SEQD-IDFKNGRP        PIC S9(5)           COMP-3.                  
001800*                                 FUNKTIONSGRUPP                          
001900*                                 FUNCTION GROUP                          
002000     03 SEQD-IDARTNR         PIC S9(9)           COMP-3.                  
002100*                                 ARTIKELNUMMER                           
002200*                                 PART NUMBER                             
002300     03 SEQD-KDBPSR          PIC S9              COMP-3.                  
002400*                                 BASLAGERFÖRSLAGSNIVÅ                    
002500*                                 BASIC PART STOCK RECOMMENDATION         
002600*** END COPY WDD2D1CCC0  LENGTH=19                                        
