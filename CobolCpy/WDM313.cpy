000100 01  URV2-WDM313.                                                         
000200*                                 URVALSREGISTER                          
000300*                                 ARTIKELSTATISTIK                        
000400*                                 URVAL2 SEGMENT                          
000500*                                 FYSISK-NYCKEL: KDSEGKEY                 
000600     03 URV2-KDSEGKEY        PIC X.                                       
000700*                                 TEKNISK SEGMENT-NYCKEL                  
000800*                                 TECHNICAL SEGMENT KEY                   
000900     03 URV2-MARKN-GRP       OCCURS 8 TIMES.                              
001000*                                 GRUPPNIVÅ MARKNAD                       
001100        05 URV2-KDMARK-BUDG-FOM                                           
001200                             PIC S9(3)           COMP-3.                  
001300*                                 MARKNADSKOD BUDGET 96 MARKNADER         
001400*                                 MARKET CODE BUDGET (96 MARKETS)         
001500        05 URV2-KDMARK-BUDG-TOM                                           
001600                             PIC S9(3)           COMP-3.                  
001700*                                 MARKNADSKOD BUDGET 96 MARKNADER         
001800*                                 MARKET CODE BUDGET (96 MARKETS)         
001900     03 URV2-IDKONCNR        OCCURS 8 TIMES                               
002000                             PIC S9(3)           COMP-3.                  
002100*                                 KONCERNNUMMER                           
002200*                                 CONCERN NO                              
002300     03 URV2-DISTR-GRP       OCCURS 4 TIMES.                              
002400*                                 GRUPPNIVÅ DISTRIKT                      
002500        05 URV2-IDDISTR-FOM  PIC S9(5)           COMP-3.                  
002600*                                 DISTRIKTNUMMER                          
002700*                                 DISTRICT NUMBER                         
002800        05 URV2-IDDISTR-TOM  PIC S9(5)           COMP-3.                  
002900*                                 DISTRIKTNUMMER                          
003000*                                 DISTRICT NUMBER                         
003100     03 URV2-TIFSGVV-FOM     PIC S9(5)           COMP-3.                  
003200*                                 FÖRSÄLJNINGSVECKA ARTIKEL (FOM)         
003300*                                 PART SALES WEEK (FROM)                  
003400     03 URV2-TIFSGVV-TOM     PIC S9(5)           COMP-3.                  
003500*                                 FÖRSÄLJNINGSVECKA ARTIKEL (TOM)         
003600*                                 PART SALES WEEK (TO)                    
003700     03 URV2-KDPRTYP         PIC X.                                       
003800*                                 TYP AV PRISTILLÄMPNING                  
003900*                                 TYPE OF PRICING                         
004000     03 URV2-IDPTYP          PIC X(3).                                    
004100*                                 POSTTYP                                 
004200*                                 RECORD TYPE                             
004300*** END COPY WDM313CCC0  LENGTH=83                                        
