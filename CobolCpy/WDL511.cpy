000100 01  FAKC-WDL511.                                                         
000200*                                 INVOICE HISTORY                         
000300*                                 INVOICE CASES                           
000400*                                 FYSISK NYCKEL: WDL511KY                 
000500*                                 (IDPRODNR + IDKOLLI)                    
000600     03 FAKC-IDPRODNR        PIC S9(7)           COMP-3.                  
000700*                                 PRODUKTIONSNUMMER                       
000800*                                 PRODUCTION NUMBER                       
000900     03 FAKC-IDKOLLI         PIC S9(5)           COMP-3.                  
001000*                                 KOLLINUMMER                             
001100*                                 CASE NUMBER                             
001200     03 FAKC-IDGMTREF.                                                    
001300*                                 GODSMOTTAGAREREFERENS                   
001400*                                 GOODS RECEIVER REFERENS                 
001500        05 FAKC-IDDISTR      PIC S9(5)           COMP-3.                  
001600*                                 DISTRIKTNUMMER                          
001700*                                 DISTRICT NUMBER                         
001800        05 FAKC-IDKUNDNR     PIC S9(7)           COMP-3.                  
001900*                                 KUNDNUMMER                              
002000*                                 CUSTOMER NO                             
002100        05 FAKC-IDKUNDRF-GRP.                                             
002200*                                 KUNDENS REFERENS (ORDERID)              
002300*                                 CUSTOMER REFERENCE (ORDER ID)           
002400           07 FAKC-IDKUNDRF  PIC X(10).                                   
002500*                                 KUNDENS REFERENS (ORDERID)              
002600*                                 CUSTOMER REFERENCE (ORDER ID)           
002700           07 FAKC-IDORDNR5-FILLER REDEFINES FAKC-IDKUNDRF.               
002800              09 FAKC-IDORDNR5                                            
002900                             PIC 9(5).                                    
003000*                                 ORDERNUMMER                             
003100*                                 ORDER NUMBER                            
003200              09 FILLER      PIC X(5).                                    
003300           07 FAKC-IDORDNR7-FILLER REDEFINES FAKC-IDKUNDRF.               
003400              09 FAKC-IDORDNR7                                            
003500                             PIC 9(7).                                    
003600*                                 ORDERNUMMER                             
003700*                                 ORDER NUMBER                            
003800              09 FILLER      PIC X(3).                                    
003900     03 FAKC-TIORDREG        PIC S9(7)           COMP-3.                  
004000*                                 ORDERREGISTRERINGSDATUM  ≈≈MMDD         
004100*                                 ORDER REGISTRATION DATE  YYMMDD         
004200     03 FAKC-IDSHIPM         PIC 9(7).                                    
004300*                                 SKEPPNINGSNUMMER                        
004400*                                 SHIPMENT NO                             
004500     03 FAKC-TISKEPPN        PIC S9(7)           COMP-3.                  
004600*                                 SKEPPNINGSDATUM  (≈≈MMDD)               
004700*                                 SHIPPING DATE    (YYMMDD)               
004800     03 FAKC-TIFAKT          PIC S9(7)           COMP-3.                  
004900*                                 FAKTURERINGSDATUM (≈≈MMDD)              
005000*                                 INVOICING DATE   (YYMMDD)               
005100     03 FAKC-KDFRAKT         PIC S9(3)           COMP-3.                  
005200*                                 FRAKTSƒTT DC TILL KUND                  
005300*                                 FREIGHT CODE                            
005400     03 FAKC-KDKOLLI         PIC X(8).                                    
005500*                                 KOLLIKOD                                
005600*                                 KOLLI CODE                              
005700     03 FAKC-KDORDKL         PIC S9              COMP-3.                  
005800*                                 ORDERKLASS                              
005900*                                 ORDER CLASS                             
006000     03 FAKC-VKORDBTO-KOLLI  PIC S9(6)V9(1)      COMP-3.                  
006100*                                 ORDERVIKT BRUTTO PER KOLLI              
006200*                                 ORDER WEIGHT GROSS PER CASE             
006300     03 FAKC-VKORDNTO-KOLLI  PIC S9(6)V9(1)      COMP-3.                  
006400*                                 ORDERVIKT NETTO PER KOLLI               
006500*                                 ORDER WEIGHT NET PER CASE               
006600*** END OF VILMAII-COPY LENGTH= 62 BYTES                                  
