000100 01  3172-WDGX3172.                                                       
000200*                                 BYTES PROFORMAFAKTURA                   
000300*                                 FAKTURA-SEGMENT                         
000400*                                 FYSISK NYCKEL: IDFAKT                   
000500     03 3172-IDFAKT          PIC S9(7)           COMP-3.                  
000600*                                 FAKTURANUMMER                           
000700*                                 INVOICE NO.                             
000800     03 3172-DAANKDAG        PIC 9(8).                                    
000900*                                 ANKOMSTDAG                              
001000*                                 RECEIVING DATE                          
001100     03 3172-DASNDDAT        PIC 9(8).                                    
001200*                                 SÄNDNINGSDATUM   (ÅÅÅÅMMDD)             
001300*                                 SHIPPING DATE   (YYYYMMDD)              
001400     03 3172-IDDC-REC        PIC X(2).                                    
001500*                                 MOTTAGANDE LAGER                        
001600*                                 RECEIVING WAREHOUSE                     
001700     03 3172-IDDC-SEND       PIC X(2).                                    
001800*                                 SÄNDANDE LAGER                          
001900*                                 SENDING WAREHOUSE                       
002000     03 3172-KDTRSTAT        PIC S9              COMP-3.                  
002100*                                 TRANSAKTIONSSTATUS                      
002200*                                 TRANSACTIONS-STATUS                     
002300     03 3172-SUFKTNTO        PIC S9(11)V9(2)     COMP-3.                  
002400*                                 FAKTURERAT VARUVÄRDE NETTO              
002500*                                 INVOICED GOODSVALUE NET                 
002600     03 3172-VKORDBTO-FAKT   PIC S9(6)V9(1)      COMP-3.                  
002700*                                 ORDERVIKT BRUTTO PER FAKTURA            
002800*                                 ORDER WEIGHT GROSS PER INVOICE          
002900     03 3172-VLORDBTO-FAKT   PIC S9(4)V9(3)      COMP-3.                  
003000*                                 ORDERVOLYM BRUTTO PER FAKTURA           
003100*                                 ORDER VOLUME GROSS PER INVOICE          
003200*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
