000100 01  SEQD-WDH1D1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDH1               
000300*                                 INVENTERINGSREGISTER                    
000400*                                 FYSISK-NYCKEL: WDH1D1KY                 
000500*                                  (IDUSER + IDDC +                       
000600*                                   DAREGDAT-SORT + KDINVKAT +            
000700*                                   TISEGKEY + IDARTNR +KDSEGKEY          
000800*                                 SEKUNDÄR NYCKEL: WDH1DSEQ               
000900*                                  (IDUSER                                
001000     03 SEQD-IDUSER          PIC X(8).                                    
001100*                                 ANVÄNDARENS SÄKERHETS ID                
001200*                                 USER SECURITY-IDENTITY                  
001300     03 SEQD-IDDC            PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 SEQD-DAREGDAT-SORT   PIC 9(8).                                    
001700*                                 DATUM FÖR SORTERING                     
001800*                                 DATES FOR SORTING                       
001900     03 SEQD-KDINVKAT        PIC S9(3)           COMP-3.                  
002000*                                 INVENTERINGSKATEGORI                    
002100*                                 STOCKTAKING CATEGORY                    
002200     03 SEQD-TISEGKEY        PIC S9(9)           COMP-3.                  
002300*                                 TEKNISK SEG-NYCKEL ÅÅÅÅMMDDL            
002400*                                 TECHNICAL SEGMENT KEY                   
002500     03 SEQD-IDARTNR         PIC S9(9)           COMP-3.                  
002600*                                 ARTIKELNUMMER                           
002700*                                 PART NUMBER                             
002800     03 SEQD-KDSEGKEY        PIC X.                                       
002900*                                 TEKNISK SEGMENT-NYCKEL                  
003000*                                 TECHNICAL SEGMENT KEY                   
003100*** END OF VILMAII-COPY LENGTH= 31 BYTES                                  
