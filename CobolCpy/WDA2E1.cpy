000100 01  SEQE-WDA2E1.                                                         
000200*                                 LEVERANSANMÄRKNING                      
000300*                                 ILISTA                                  
000400*                                 EXIT: INDEX FINNS NÄR                   
000500*                                 IDILIST > 0                             
000600*                                 FYSISK NYCKEL: WDA2E1KY                 
000700*                                  (IDDC                +                 
000800*                                   IDILIST  + ADLAGOMR +                 
000900*                                   ADGANG   + ADPLATS  +                 
001000*                                   IDLEVANM + IDARTNR  +                 
001100*                                   IDRADNR  )                            
001200*                                 SEKUNDÄR NYCKEL: WDA2ESEQ               
001300*                                  (IDDC                +                 
001400*                                   IDILIST  + ADLAGOMR +                 
001500*                                   ADGANG   + ADPLATS)                   
001600     03 SEQE-IDDC-RET        PIC X(2).                                    
001700*                                 MOTTAGANDE LAGER FÖR RETURER            
001800*                                 RECEIVING WAREHOUSE FOR RETURNS         
001900     03 SEQE-IDILIST         PIC 9(5).                                    
002000*                                 INLÄGGNINGSLISTEIDENTITET               
002100*                                 REPORTINGLIST-IDENTITY                  
002200     03 SEQE-ADLAGOMR        PIC S9(3)           COMP-3.                  
002300*                                 LAGEROMRÅDE                             
002400*                                 AREA                                    
002500     03 SEQE-ADGANG          PIC S9(3)           COMP-3.                  
002600*                                 GÅNG                                    
002700*                                 AISLE                                   
002800     03 SEQE-ADPLATS         PIC S9(5)           COMP-3.                  
002900*                                 LAGERPLATSNUMMER                        
003000*                                 LOCATION                                
003100     03 SEQE-IDLEVANM.                                                    
003200*                                 LEVERANSANMÄRKNINGSIDENTITET            
003300*                                 DISCREPANCY REPORT IDENTITY             
003400        05 SEQE-IDDISTR      PIC S9(5)           COMP-3.                  
003500*                                 DISTRIKTNUMMER                          
003600*                                 DISTRICT NUMBER                         
003700        05 SEQE-IDKUNDNR     PIC S9(7)           COMP-3.                  
003800*                                 KUNDNUMMER                              
003900*                                 CUSTOMER NO                             
004000        05 SEQE-IDRAPPNR     PIC 9(7).                                    
004100*                                 RAPPORT NUMMER                          
004200*                                 DISCREPANCY REPORT NUMBER               
004300     03 SEQE-IDARTNR         PIC S9(9)           COMP-3.                  
004400*                                 ARTIKELNUMMER                           
004500*                                 PART NUMBER                             
004600     03 SEQE-IDRADNR         PIC S9(5)           COMP-3.                  
004700*                                 RADNUMMER                               
004800*                                 LINE NO                                 
004900     03 SEQE-IDANSTNR-RET    PIC S9(5)           COMP-3.                  
005000*                                 ANSTÄLLNINGSNUMMER RETURAVDELN.         
005100*                                 EMPLOYEE NUMBER RETURNDEPT.             
005200     03 SEQE-TIUTSKR         PIC S9(7)           COMP-3.                  
005300*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
005400*                                 PRINTING DATE  (YYMMDD)                 
005500     03 SEQE-TIUPPDAT-ILI    PIC S9(7)           COMP-3.                  
005600*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
005700*                                 UPDATING DATE     (YYMMDD)              
005800*** END OF VILMAII-COPY LENGTH= 47 BYTES                                  
