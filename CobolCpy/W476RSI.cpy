000100 01  RSI-W476RSI.                                                         
000200*                                 INFO ABOUT SHIPMENT,                    
000300*                                 DDI MARKETS                             
000400*                                 RECORD TYPE RSI                         
000500     03 RSI-IDPTYP           PIC X(3).                                    
000600*                                 RECORD TYPE                             
000700     03 RSI-IDDC             PIC X(2).                                    
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 RSI-IDSHIPM          PIC 9(7).                                    
001000*                                 SHIPMENT NO                             
001100     03 RSI-TISKEPPN         PIC 9(6).                                    
001200*                                 SHIPPING DATE    (YYMMDD)               
001300     03 RSI-IDDISTR          PIC 9(4).                                    
001400*                                 DISTRICT NUMBER                         
001500     03 RSI-IDKUNDNR         PIC 9(6).                                    
001600*                                 CUSTOMER NO                             
001700     03 RSI-IDORDNR7         PIC 9(7).                                    
001800*                                 ORDER NUMBER                            
001900     03 RSI-FLCOD            PIC X.                                       
002000*                                 CASH ON DELIVERY CUSTOMER               
002100     03 RSI-KDORDKL          PIC 9.                                       
002200*                                 ORDER CLASS                             
002300     03 RSI-IDKOLLI          PIC 9(5).                                    
002400*                                 CASE NUMBER                             
002500     03 RSI-IDARTNR          PIC 9(9).                                    
002600*                                 PART NUMBER                             
002700     03 RSI-REKSIFFR         PIC 9.                                       
002800*                                 PART NO CHECK DIGIT                     
002900     03 RSI-PRARTNTO-LOC     PIC 9(7)V9(2).                               
003000*                                 NET PRICE EACH LOCAL CURRENCY           
003100     03 RSI-FLARTSTD         PIC X.                                       
003200*                                 INDICATES PRARTSTD                      
003300     03 RSI-KVBEART          PIC 9(6).                                    
003400*                                 ORDERED QUANTITY                        
003500     03 RSI-KVLEVART         PIC 9(7).                                    
003600*                                 DELIVERED QUANTITY                      
003700     03 RSI-KDDSP            PIC 9.                                       
003800*                                 AFFECT ON DSP                           
003900     03 RSI-BERADREF         PIC X(10).                                   
004000*                                 CUSTOMERS ITEM REF.                     
004100     03 RSI-IDRONR           PIC 9(5).                                    
004200*                                 BACK ORDER NO                           
004300     03 RSI-FLIHOP           PIC X.                                       
004400     03 RSI-BEVOLREF         PIC X(10).                                   
004500*                                 VOLVO REFERENCE                         
004600     03 RSI-FILLER           PIC X(3).                                    
004700*** END OF VILMAII-COPY LENGTH= 105 BYTES                                 
