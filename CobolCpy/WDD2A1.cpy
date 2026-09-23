000100 01  SEQA-WDD2A1.                                                         
000200*                                 NYA ARTIKLAR FRÅN PV OCH LV             
000300*                                 SEKUNDÄRT INDEX TILL WDD201             
000400*                                 BEREDARE KÖ                             
000500*                                 FYSISK NYCKEL: WDD2A1KY                 
000600*                                  (IDBERED, IDAO, IDARTNR)               
000700*                                 SECONDARY NYCKEL: WDD2ASEQ              
000800*                                  (IDBERED)                              
000900     03 SEQA-IDBERED         PIC S9(3)           COMP-3.                  
001000*                                 BEREDARENUMMER                          
001100     03 SEQA-IDAO            PIC X(10).                                   
001200*                                 ÄNDRINGSORDERNUMMER                     
001300*                                 DESIGN CHANGE NOTICE                    
001400     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
001500*                                 ARTIKELNUMMER                           
001600*                                 PART NUMBER                             
001700     03 SEQA-IDPROJ          PIC X(4).                                    
001800*                                 PARTS PROJEKTIDENTITET                  
001900*                                 PARTS PROJECT IDENTITY                  
002000     03 SEQA-IDPROJK         PIC X(4).                                    
002100*                                 PROJEKTIDENTITET KONSTRUKTION           
002200*                                 PROJECT IDENTITY KONSTRUCTION           
002300     03 SEQA-TISLUBER        PIC S9(7)           COMP-3.                  
002400*                                 BEREDNINGS SLUT                         
002500*                                 STOP-TIME PARTS PLANNING                
002600*** END COPY WDD2A1CCC0  LENGTH=29                                        
