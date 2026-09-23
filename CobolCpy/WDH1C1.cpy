000100 01  SEQC-WDH1C1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDH1               
000300*                                 INVENTERINGSREGISTER                    
000400*                                 FYSISK-NYCKEL: WDH1C1KY                 
000500*                                  (IDDC + KDINVCAT +                     
000600*                                   DAREGDAT-SORT + IDARTNR +             
000700*                                   TISEGKEY                              
000800*                                 SEKUNDÄR NYCKEL: WDH1CSEQ               
000900*                                  (IDDC + KDINVCAT +                     
001000*                                  DAREGDAT-SORT                          
001100     03 SEQC-IDDC            PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 SEQC-KDINVKAT        PIC S9(3)           COMP-3.                  
001500*                                 INVENTERINGSKATEGORI                    
001600*                                 STOCKTAKING CATEGORY                    
001700     03 SEQC-DAREGDAT-SORT   PIC 9(8).                                    
001800*                                 DATUM FÖR SORTERING                     
001900*                                 DATES FOR SORTING                       
002000     03 SEQC-IDARTNR         PIC S9(9)           COMP-3.                  
002100*                                 ARTIKELNUMMER                           
002200*                                 PART NUMBER                             
002300     03 SEQC-TISEGKEY        PIC S9(9)           COMP-3.                  
002400*                                 TEKNISK SEG-NYCKEL ÅÅÅÅMMDDL            
002500*                                 TECHNICAL SEGMENT KEY                   
002600     03 SEQC-DAREGDAT        PIC S9(9)           COMP-3.                  
002700*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002800*                                 REGISTRATION DATE (YYYYMMDD)            
002900     03 SEQC-DAREGDAT-PR1    PIC S9(9)           COMP-3.                  
003000*                                 DATUM FÖR FÖRSTA PRINTNING              
003100*                                 DATE FOR FIRST PRINTING                 
003200     03 SEQC-DAREGDAT-PR2    PIC S9(9)           COMP-3.                  
003300*                                 DATUM FÖR ANDRA  PRINTNING              
003400*                                 DATE FOR SECOND PRINT                   
003500     03 SEQC-DAREGDAT-PR3    PIC S9(9)           COMP-3.                  
003600*                                 DATUM FÖR TREDJE PRINTNING              
003700*                                 DATE FOR THIRD PRINT                    
003800     03 SEQC-DAREGDAT-CRE    PIC S9(9)           COMP-3.                  
003900*                                 DATUM NÄR INVENTERING PÅBÖRJAS          
004000*                                 CREATION DATE                           
004100     03 SEQC-FILLER          PIC X(10).                                   
004200*** END OF VILMAII-COPY LENGTH= 57 BYTES                                  
