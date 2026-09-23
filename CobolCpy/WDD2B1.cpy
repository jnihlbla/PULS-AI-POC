000100 01  SEQB-WDD2B1-CTX.                                                     
000200*                                 NYA ARTIKLAR FRÅN PV OCH LV             
000300*                                 SEKUNDÄRT INDEX TILL WDD201             
000400*                                 FLPISK INGÅNG                           
000500*                                 FYSISK NYCKEL: WDD2B1KY                 
000600*                                  (FLPISK, DAFINLEV, IDANSK              
000700*                                  (IDAO, IDPROJ, IDARTNR)                
000800*                                 SÖKFÄLT: KDANSKQ,                       
000900*                                 SECONDARY NYCKEL: WDD2BSEQ              
001000*                                  (FLPISK)                               
001100     03 SEQB-FLPISK          PIC X.                                       
001200*                                 PISK ARTIKEL                            
001300*                                 FAST PART (PISK)                        
001400     03 SEQB-IDANSK          PIC S9(3)           COMP-3.                  
001500*                                 ANSKAFFARNUMMER                         
001600*                                 PROCURER NO.                            
001700     03 SEQB-DAFINLEV        PIC 9(8).                                    
001800*                                 PUBLICERINGSDATUM  (AAAAMMDD)           
001900*                                 DATE 1:ST GOODS REC (YYYYMMDD)          
002000     03 SEQB-IDAO            PIC X(10).                                   
002100*                                 ÄNDRINGSORDERNUMMER                     
002200*                                 DESIGN CHANGE NOTICE                    
002300     03 SEQB-IDPROJ          PIC X(4).                                    
002400*                                 PARTS PROJEKTIDENTITET                  
002500*                                 PARTS PROJECT IDENTITY                  
002600     03 SEQB-IDARTNR         PIC S9(9)           COMP-3.                  
002700*                                 ARTIKELNUMMER                           
002800*                                 PART NUMBER                             
002900     03 SEQB-KDANSKQ         PIC X.                                       
003000*                                 KOD FÖR ANSKAFFARE KÖ                   
003100*                                 PROCURER QUEUE CODE                     
003200     03 SEQB-TIMOTSI         PIC S9(7)           COMP-3.                  
003300*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003400*                                 YEAR - MONTH - DAY  (YYMMDD)            
003500*** END OF VILMAII-COPY LENGTH= 35 BYTES                                  
