000100 01  URV1-WDM411.                                                         
000200*                                 URVALSREGISTER                          
000300*                                 URVAL1 SEGMENT                          
000400*                                 FYSISK-NYCKEL: KDSEGKEY                 
000500     03 URV1-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 URV1-ANSK-GRP.                                                    
000900*                                 GRUPPNIVÅ ANSKAFFARE                    
001000        05 URV1-IDANSK-FOM   PIC S9(3)           COMP-3.                  
001100*                                 ANSKAFFARNUMMER                         
001200*                                 PROCURER NO.                            
001300        05 URV1-IDANSK-TOM   PIC S9(3)           COMP-3.                  
001400*                                 ANSKAFFARNUMMER                         
001500*                                 PROCURER NO.                            
001600     03 URV1-DISTR-GRP.                                                   
001700*                                 GRUPPNIVÅ DISTRIKT                      
001800        05 URV1-IDDISTR-FOM  PIC S9(5)           COMP-3.                  
001900*                                 DISTRIKTNUMMER                          
002000*                                 DISTRICT NUMBER                         
002100        05 URV1-IDDISTR-TOM  PIC S9(5)           COMP-3.                  
002200*                                 DISTRIKTNUMMER                          
002300*                                 DISTRICT NUMBER                         
002400     03 URV1-IDLEVNR         PIC X(5).                                    
002500*                                 LEVERANTÖRNUMMER                        
002600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002700     03 URV1-BASL-GRP.                                                    
002800*                                 GRUPPNIVÅ BASLAGER                      
002900        05 URV1-KDBASLM-FOM  PIC X(6).                                    
003000*                                 BASLAGERMARKNAD                         
003100*                                 BASIC STOCK MARKET                      
003200        05 URV1-KDBASLM-TOM  PIC X(6).                                    
003300*                                 BASLAGERMARKNAD                         
003400*                                 BASIC STOCK MARKET                      
003500     03 URV1-KDPRODSL        PIC S9(3)           COMP-3.                  
003600*                                 PRODUKTSLAG                             
003700*                                 PRODUCT GROUP                           
003800     03 URV1-KDSORT1         PIC S9              COMP-3.                  
003900*                                 SORTERINGSKOD                           
004000*                                 CODE FOR SORTING                        
004100     03 URV1-TPO-GRP.                                                     
004200*                                 GRUPPNIVÅ TPOTYPER                      
004300        05 URV1-KDTPOTYP-FOM PIC S9              COMP-3.                  
004400*                                 TYP AV TIDPLANERAD ORDER                
004500*                                 TYPE OF TIME PLANNED ORDER              
004600        05 URV1-KDTPOTYP-TOM PIC S9              COMP-3.                  
004700*                                 TYP AV TIDPLANERAD ORDER                
004800*                                 TYPE OF TIME PLANNED ORDER              
004900     03 URV1-TITPO-GRP.                                                   
005000*                                 GRUPPNIVÅ TPOTIDER                      
005100        05 URV1-TITPO-FOM    PIC S9(7)           COMP-3.                  
005200*                                 PLANERAD ORDERDATUM                     
005300*                                 PLANNED ORDER DATE                      
005400        05 URV1-TITPO-TOM    PIC S9(7)           COMP-3.                  
005500*                                 PLANERAD ORDERDATUM                     
005600*                                 PLANNED ORDER DATE                      
005700     03 URV1-FILLER          PIC X(9).                                    
005800*** END OF VILMAII-COPY LENGTH= 50 BYTES                                  
