000100 01  URV1-WDM311.                                                         
000200*                                 URVALSREGISTER                          
000300*                                 ARTIKELSTATISTIK                        
000400*                                 URVAL1 SEGMENT                          
000500*                                 FYSISK-NYCKEL: KDSEGKEY                 
000600     03 URV1-KDSEGKEY        PIC X.                                       
000700*                                 TEKNISK SEGMENT-NYCKEL                  
000800*                                 TECHNICAL SEGMENT KEY                   
000900     03 URV1-KDPRODSL        OCCURS 7 TIMES                               
001000                             PIC S9(3)           COMP-3.                  
001100*                                 PRODUKTSLAG                             
001200*                                 PRODUCT GROUP                           
001300     03 URV1-KDVVKL          OCCURS 5 TIMES                               
001400                             PIC S9              COMP-3.                  
001500*                                 VOLYMVÄRDESKLASS                        
001600*                                 VOLUME VALUE CLASS                      
001700     03 URV1-MARKN-GRP       OCCURS 8 TIMES.                              
001800*                                 GRUPPNIVÅ MARKNADER                     
001900        05 URV1-KDMARK-FOM   PIC S9(3)           COMP-3.                  
002000*                                 MARKNADSKOD                             
002100*                                 MARKET CODE                             
002200        05 URV1-KDMARK-TOM   PIC S9(3)           COMP-3.                  
002300*                                 MARKNADSKOD                             
002400*                                 MARKET CODE                             
002500     03 URV1-IDKONCNR        OCCURS 8 TIMES                               
002600                             PIC S9(3)           COMP-3.                  
002700*                                 KONCERNNUMMER                           
002800*                                 CONCERN NO                              
002900     03 URV1-DISTR-GRP       OCCURS 4 TIMES.                              
003000*                                 GRUPPNIVÅ DISTRIKT                      
003100        05 URV1-IDDISTR-FOM  PIC S9(5)           COMP-3.                  
003200*                                 DISTRIKTNUMMER                          
003300*                                 DISTRICT NUMBER                         
003400        05 URV1-IDDISTR-TOM  PIC S9(5)           COMP-3.                  
003500*                                 DISTRIKTNUMMER                          
003600*                                 DISTRICT NUMBER                         
003700     03 URV1-IDLEVNR         OCCURS 8 TIMES                               
003800                             PIC X(5).                                    
003900*                                 LEVERANTÖRNUMMER                        
004000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004100     03 URV1-IDLKTO          OCCURS 8 TIMES                               
004200                             PIC S9(7)           COMP-3.                  
004300*                                 LAGERKONTO (FFHHHUU)                    
004400*                                 STOCK ACCOUNT (CCMMMSS)                 
004500     03 URV1-ANSK-GRP        OCCURS 4 TIMES.                              
004600*                                 GRUPPNIVÅ ANSKAFFARE                    
004700        05 URV1-IDANSK-FOM   PIC S9(3)           COMP-3.                  
004800*                                 ANSKAFFARNUMMER                         
004900*                                 PROCURER NO.                            
005000        05 URV1-IDANSK-TOM   PIC S9(3)           COMP-3.                  
005100*                                 ANSKAFFARNUMMER                         
005200*                                 PROCURER NO.                            
005300     03 URV1-TIFSGVV-FOM     PIC S9(5)           COMP-3.                  
005400*                                 FÖRSÄLJNINGSVECKA ARTIKEL (FOM)         
005500*                                 PART SALES WEEK (FROM)                  
005600     03 URV1-TIFSGVV-TOM     PIC S9(5)           COMP-3.                  
005700*                                 FÖRSÄLJNINGSVECKA ARTIKEL (TOM)         
005800*                                 PART SALES WEEK (TO)                    
005900     03 URV1-IDPTYP          PIC X(3).                                    
006000*                                 POSTTYP                                 
006100*                                 RECORD TYPE                             
006200     03 URV1-KDPRTYP         PIC X.                                       
006300*                                 TYP AV PRISTILLÄMPNING                  
006400*                                 TYPE OF PRICING                         
006500     03 URV1-KDNIVA          PIC S9(3)           COMP-3.                  
006600*                                 NIVÅ NUMMER                             
006700     03 URV1-KDSVAR          PIC X.                                       
006800*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
006900*                                 RETURN CODE FROM PROGRAM                
007000     03 URV1-FILLER          PIC X(7).                                    
007100*** END OF VILMAII-COPY LENGTH= 200 BYTES                                 
