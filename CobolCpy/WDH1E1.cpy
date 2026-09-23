000100 01  SEQE-WDH1E1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDH1               
000300*                                 INVENTERINGSREGISTER                    
000400*                                 FYSISK-NYCKEL: WDH1E1KY                 
000500*                                  (IDDC + DAREGDAT-SORT +                
000600*                                   KDINVCAT + IDARTNR  +                 
000700*                                   TISEGKEY            )                 
000800*                                 SEKUNDÄR NYCKEL: WDH1ESEQ               
000900*                                  (IDDC + DAREGDAT-SORT  )               
001000     03 SEQE-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 SEQE-DAREGDAT-SORT   PIC 9(8).                                    
001400*                                 DATUM FÖR SORTERING                     
001500*                                 DATES FOR SORTING                       
001600     03 SEQE-KDINVKAT        PIC S9(3)           COMP-3.                  
001700*                                 INVENTERINGSKATEGORI                    
001800*                                 STOCKTAKING CATEGORY                    
001900     03 SEQE-IDARTNR         PIC S9(9)           COMP-3.                  
002000*                                 ARTIKELNUMMER                           
002100*                                 PART NUMBER                             
002200     03 SEQE-TISEGKEY        PIC S9(9)           COMP-3.                  
002300*                                 TEKNISK SEG-NYCKEL ÅÅÅÅMMDDL            
002400*                                 TECHNICAL SEGMENT KEY                   
002500     03 SEQE-DAREGDAT        PIC S9(9)           COMP-3.                  
002600*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002700*                                 REGISTRATION DATE (YYYYMMDD)            
002800     03 SEQE-DAREGDAT-PR1    PIC S9(9)           COMP-3.                  
002900*                                 DATUM FÖR FÖRSTA PRINTNING              
003000*                                 DATE FOR FIRST PRINTING                 
003100     03 SEQE-DAREGDAT-PR2    PIC S9(9)           COMP-3.                  
003200*                                 DATUM FÖR ANDRA  PRINTNING              
003300*                                 DATE FOR SECOND PRINT                   
003400     03 SEQE-DAREGDAT-PR3    PIC S9(9)           COMP-3.                  
003500*                                 DATUM FÖR TREDJE PRINTNING              
003600*                                 DATE FOR THIRD PRINT                    
003700     03 SEQE-DAREGDAT-CRE    PIC S9(9)           COMP-3.                  
003800*                                 DATUM NÄR INVENTERING PÅBÖRJAS          
003900*                                 CREATION DATE                           
004000     03 SEQE-FILLER          PIC X(10).                                   
004100*** END OF VILMAII-COPY LENGTH= 57 BYTES                                  
