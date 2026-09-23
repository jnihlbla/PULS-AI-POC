000100 01  4468-WDGX4468.                                                       
000200*                                 NDC BILL OF LADING                      
000300*                                 FYSISK NYCKEL: KY4468                   
000400*                                  (IDDISTR + IDKUNDNR + IDKUNDRF         
000500*                                   + IDPRODNR + IDKOLLI)                 
000600     03 4468-IDDISTR         PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800*                                 DISTRICT NUMBER                         
000900     03 4468-IDKUNDNR        PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100*                                 CUSTOMER NO                             
001200     03 4468-IDKUNDRF        PIC X(10).                                   
001300*                                 KUNDENS REFERENS (ORDERID)              
001400*                                 CUSTOMER REFERENCE (ORDER ID)           
001500     03 4468-IDPRODNR        PIC S9(7)           COMP-3.                  
001600*                                 PRODUKTIONSNUMMER                       
001700*                                 PRODUCTION NUMBER                       
001800     03 4468-IDKOLLI         PIC S9(5)           COMP-3.                  
001900*                                 KOLLINUMMER                             
002000*                                 CASE NUMBER                             
002100     03 4468-IDPSN           OCCURS 10 TIMES                              
002200                             PIC 9(3).                                    
002300*                                 PROPER SHIPPING NAME                    
002400*                                 PROPER SHIPPING NAME                    
002500     03 4468-KDORDKL         PIC S9              COMP-3.                  
002600*                                 ORDERKLASS                              
002700*                                 ORDER CLASS                             
002800     03 4468-KDKOLLI         PIC X(8).                                    
002900*                                 KOLLIKOD                                
003000*                                 KOLLI CODE                              
003100     03 4468-VKORDBTO-KOLLI  PIC S9(6)V9(1)      COMP-3.                  
003200*                                 ORDERVIKT BRUTTO PER KOLLI              
003300*                                 ORDER WEIGHT GROSS PER CASE             
003400     03 4468-VLORDBTO-KOLLI  PIC S9(4)V9(3)      COMP-3.                  
003500*                                 ORDERVOLYM BRUTTO KOLLI                 
003600*                                 ORDER VOL GR/CASE                       
003700*** END OF VILMAII-COPY LENGTH= 71 BYTES                                  
