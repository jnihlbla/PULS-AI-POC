000100 01  SEQE-WDD2E1-CTX.                                                     
000200*                                 NYA ARTIKLAR FRÅN PV OCH LV             
000300*                                 SEKUNDÄRT INDEX TILL WDD201             
000400*                                 FLPISK INGÅNG                           
000500*                                 FYSISK NYCKEL: WDD2E1KY                 
000600*                                  (FLPISK, DAFINLEV, IDINK,              
000700*                                  (IDANSK, IDPROJ, IDARTNR)              
000800*                                 SÖKFÄLT: KDANSKQ,                       
000900*                                 SECONDARY NYCKEL: WDD2ESEQ              
001000*                                  (FLPISK)                               
001100     03 SEQE-FLPISK          PIC X.                                       
001200*                                 PISK ARTIKEL                            
001300*                                 FAST PART (PISK)                        
001400     03 SEQE-IDANSK          PIC S9(3)           COMP-3.                  
001500*                                 ANSKAFFARNUMMER                         
001600*                                 PROCURER NO.                            
001700     03 SEQE-DAFINLEV        PIC 9(8).                                    
001800*                                 PUBLICERINGSDATUM  (AAAAMMDD)           
001900*                                 DATE 1:ST GOODS REC (YYYYMMDD)          
002000     03 SEQE-IDINK           PIC X(4).                                    
002100*                                 INKÖPARNUMMER                           
002200*                                 PURCHASE IDENTIFICATION NUMBER          
002300     03 SEQE-IDPROJ          PIC X(4).                                    
002400*                                 PARTS PROJEKTIDENTITET                  
002500*                                 PARTS PROJECT IDENTITY                  
002600     03 SEQE-IDARTNR         PIC S9(9)           COMP-3.                  
002700*                                 ARTIKELNUMMER                           
002800*                                 PART NUMBER                             
002900     03 SEQE-KDANSKQ         PIC X.                                       
003000*                                 KOD FÖR ANSKAFFARE KÖ                   
003100*                                 PROCURER QUEUE CODE                     
003200*** END OF VILMAII-COPY LENGTH= 25 BYTES                                  
