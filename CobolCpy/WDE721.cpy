000100 01  SKOR-WDE721.                                                         
000200*                                 SAMLINGSKOLLI REGISTER                  
000300*                                 ORDER SEGMENT                           
000400*                                 FYSISK NYCKEL: WDE721KY:                
000500*                                 (IDDISTR + IDKUNDNR +                   
000600*                                  IDORDNR7 + IDKOLLI)                    
000700*                                                                         
000800     03 SKOR-IDDISTR         PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000*                                 DISTRICT NUMBER                         
001100     03 SKOR-IDKUNDNR        PIC S9(7)           COMP-3.                  
001200*                                 KUNDNUMMER                              
001300*                                 CUSTOMER NO                             
001400     03 SKOR-IDORDNR7        PIC 9(7).                                    
001500*                                 ORDERNUMMER                             
001600*                                 ORDER NUMBER                            
001700     03 SKOR-IDKOLLI         PIC S9(5)           COMP-3.                  
001800*                                 KOLLINUMMER                             
001900*                                 CASE NUMBER                             
002000     03 SKOR-IDPRODNR        PIC S9(7)           COMP-3.                  
002100*                                 PRODUKTIONSNUMMER                       
002200*                                 PRODUCTION NUMBER                       
002300     03 SKOR-IDDC            PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500*                                 WAREHOUSE IDENTIFIER                    
002600     03 SKOR-IDTRPTNR        PIC S9(3)           COMP-3.                  
002700*                                 TRANSPORTIDENTITET                      
002800*                                 TRANSPORT IDENTITY                      
002900     03 SKOR-KDFRAKT         PIC S9(3)           COMP-3.                  
003000*                                 FRAKTSÄTT DC TILL KUND                  
003100*                                 FREIGHT CODE                            
003200     03 SKOR-VKORDNTO-KOLLI  PIC S9(6)V9(1)      COMP-3.                  
003300*                                 ORDERVIKT NETTO PER KOLLI               
003400*                                 ORDER WEIGHT NET PER CASE               
003500     03 SKOR-VKORDBTO-KOLLI  PIC S9(6)V9(1)      COMP-3.                  
003600*                                 ORDERVIKT BRUTTO PER KOLLI              
003700*                                 ORDER WEIGHT GROSS PER CASE             
003800     03 SKOR-VLORDBTO-KOLLI  PIC S9(4)V9(3)      COMP-3.                  
003900*                                 ORDERVOLYM BRUTTO KOLLI                 
004000*                                 ORDER VOL GR/CASE                       
004100*** END OF VILMAII-COPY LENGTH= 39 BYTES                                  
