000100 01  1126-WDGX1126.                                                       
000200*                                 BASLAGER                                
000300*                                 PROJEKTSTRUKTURER                       
000400*                                 MARKNADSINFORMATION                     
000500*                                 FYSISK NYCKEL: WDGXKEY                  
000600*                                 (KDBASLM + LOW-VALUE)                   
000700     03 1126-KDBASLM         PIC X(6).                                    
000800*                                 BASLAGERMARKNAD                         
000900*                                 BASIC STOCK MARKET                      
001000     03 1126-LOW-VALUE       PIC X(9).                                    
001100     03 1126-IDDISTR         OCCURS 6 TIMES                               
001200                             PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400*                                 DISTRICT NUMBER                         
001500     03 1126-KVBLKIT         PIC S9(3)           COMP-3.                  
001600*                                 ÅTERFÖRSÄLJARSATSER PER MARKNAD         
001700*                                 QUANTITY DEALER KITS                    
001800     03 1126-REBLFORD        OCCURS 6 TIMES                               
001900                             PIC S9(3)           COMP-3.                  
002000*                                 PROCENTFÖRDELNING PER DISTRIKT          
002100*                                 PERCENTAGE PER DISTRICT                 
002200     03 1126-TIMARKORD       PIC S9(7)           COMP-3.                  
002300*                                 ORDERSLÄPP FRÅN BASLAGER                
002400*                                 ORDER RELEASE FROM BASIC STOCK          
002500     03 1126-TIBASORD        PIC S9(7)           COMP-3.                  
002600*                                 ORDERGENERERING FRÅN BASLAGER           
002700*                                 BASIC STOCK ORDER RELEASE TIME          
002800     03 1126-TIPROJSTO       PIC S9(7)           COMP-3.                  
002900*                                 PROJEKTSTOPP BASLAGER                   
003000*                                 PROJECT STOP BASIC STOCK                
003100     03 1126-TISTAMREG       PIC S9(7)           COMP-3.                  
003200*                                 STARTTID 4 VECKORS REGELN               
003300*                                 START TIME 4 WEEKS RULES                
003400     03 FILLER               PIC X(2).                                    
003500*** END COPY WDGX1126C0  LENGTH=65                                        
