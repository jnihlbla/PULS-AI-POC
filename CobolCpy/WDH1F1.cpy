000100 01  SEQF-WDH1F1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDH1               
000300*                                 INVENTERINGSREGISTER                    
000400*                                 FYSISK-NYCKEL: WDH1F1KY                 
000500*                                  (IDUSER + IDDC + KDINVCAT +            
000600*                                  DAREGDAT-SORT + TISEGKEY +             
000700*                                   IDARTNR + KDSEGKEY )                  
000800*                                 SEKUNDÄR NYCKEL: WDH1FSEQ               
000900*                                  (IDUSER                   )            
001000     03 SEQF-IDUSER          PIC X(8).                                    
001100*                                 ANVÄNDARENS SÄKERHETS ID                
001200*                                 USER SECURITY-IDENTITY                  
001300     03 SEQF-IDDC            PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 SEQF-KDINVKAT        PIC S9(3)           COMP-3.                  
001700*                                 INVENTERINGSKATEGORI                    
001800*                                 STOCKTAKING CATEGORY                    
001900     03 SEQF-DAREGDAT-SORT   PIC 9(8).                                    
002000*                                 DATUM FÖR SORTERING                     
002100*                                 DATES FOR SORTING                       
002200     03 SEQF-TISEGKEY        PIC S9(9)           COMP-3.                  
002300*                                 TEKNISK SEG-NYCKEL ÅÅÅÅMMDDL            
002400*                                 TECHNICAL SEGMENT KEY                   
002500     03 SEQF-IDARTNR         PIC S9(9)           COMP-3.                  
002600*                                 ARTIKELNUMMER                           
002700*                                 PART NUMBER                             
002800     03 SEQF-KDSEGKEY        PIC X.                                       
002900*                                 TEKNISK SEGMENT-NYCKEL                  
003000*                                 TECHNICAL SEGMENT KEY                   
003100*** END OF VILMAII-COPY LENGTH= 31 BYTES                                  
