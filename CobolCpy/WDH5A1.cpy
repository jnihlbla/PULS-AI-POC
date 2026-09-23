000100 01  SEQA-WDH5A1.                                                         
000200*                                 EKONOMISKA HÄNDELSER                    
000300*                                 SEKUNDÄRT INDEX TILL WDH531             
000400*                                 IDSYSMOT-INGÅNG                         
000500*                                 FYSISK-NYCKEL: WDH5A1KY                 
000600*                                 (IDSYSMOT + IDPTYP + IDSEKVNR +         
000700*                                 IDFTG+KDEKHHT+KDEKSHT+KDEKNIVA)         
000800*                                 SEKUNDÄR NYCKEL: WDH5ASEQ               
000900*                                 (IDSYSMOT + IDPTYP + IDSEKVNR)          
001000     03 SEQA-IDSYSMOT        PIC X(6).                                    
001100*                                 PULS MOTTAGANDE SYSTEMNAMN              
001200*                                 PULS RECEIVING SYSTEM NAME              
001300     03 SEQA-IDPTYP          PIC X(3).                                    
001400*                                 POSTTYP                                 
001500*                                 RECORD TYPE                             
001600     03 SEQA-IDSEKVNR        PIC S9(3)           COMP-3.                  
001700*                                 GENERELLT SEKVENSNUMMER                 
001800*                                 GENERAL SEQUENCE NUMBER                 
001900     03 SEQA-IDFTG           PIC 9(2).                                    
002000*                                 FÖRETAGSID EKONOM REDOVISNING           
002100*                                 COMPANY IDENTITY ACCOUNTING             
002200     03 SEQA-KDEKHHT         PIC X(3).                                    
002300*                                 EKONOMISK HUVUDHÄNDELSE                 
002400*                                 ECONOMIC MAIN EVENT                     
002500     03 SEQA-KDEKSHT         PIC X(3).                                    
002600*                                 EKONOMISK SUBHÄNDELSE                   
002700*                                 ECONOMIC SUB EVENT                      
002800     03 SEQA-KDEKNIVA        PIC X(5).                                    
002900*                                 EKONOMISK HÄNDELSENIVÅ                  
003000*                                 ECONOMICAL EVENT LEVEL                  
003100*** END OF VILMAII-COPY LENGTH= 24 BYTES                                  
