000100 01  LOR-WDQ221.                                                          
000200*                                 ORDERHUVUDSREGISTER KÖ                  
000300*                                 LAGEROMRÅDE MED ORDERRAD                
000400*                                 FYSISK NYCKEL: ADLAGOMR                 
000500     03 LOR-ADLAGOMR         PIC S9(3)           COMP-3.                  
000600*                                 LAGEROMRÅDE                             
000700*                                 AREA                                    
000800     03 LOR-IDPRC.                                                        
000900*                                 PRODUKTIONSKANAL                        
001000*                                 PRODUCTION CHANNEL                      
001100        05 LOR-IDPRCBAS      PIC X(3).                                    
001200*                                 PRC-BAS                                 
001300*                                 PRC-BASIC                               
001400        05 LOR-IDPRCVAR      PIC X.                                       
001500*                                 PRC-VARIANT                             
001600*                                 PRC-VARIANT                             
001700     03 LOR-KVANTART         PIC S9(5)           COMP-3.                  
001800*                                 ANTAL-ARTIKLAR                          
001900*                                 QUANTITY PARTS                          
002000     03 LOR-KVRADER          PIC S9(5)           COMP-3.                  
002100*                                 ANTAL RADER                             
002200*                                 NUMBER OF LINES                         
002300     03 LOR-SUORDV           PIC S9(9)V9(2)      COMP-3.                  
002400*                                 SUMMA ORDERVÄRDE                        
002500*                                 TOTAL ORDER VALUE                       
002600     03 LOR-VKORDNTO         PIC S9(6)V9(1)      COMP-3.                  
002700*                                 ORDERVIKT NETTO (KG)                    
002800*                                 WEIGHT PER ORDER NETTO (KG)             
002900     03 LOR-VLORDNTO         PIC S9(4)V9(3)      COMP-3.                  
003000*                                 ORDERVOLYM NETTO (M3)                   
003100*                                 NET VOLUME PER ORDER (M3)               
003200     03 LOR-DEAL-PR-SUM.                                                  
003300*                                 DEALERPRIS (HUVUD)                      
003400        05 LOR-SUORDV-LOC    PIC S9(9)V9(2)      COMP-3.                  
003500*                                 ORDERVÄRDE SLUTKUNDPRIS                 
003600*                                 I LOKAL VALUTA                          
003700*                                 ORDER VALUE, CUSTOMER PRICE             
003800*                                 IN LOCAL CURRENCY                       
003900        05 LOR-SUORDV-LOCPREL                                             
004000                             PIC S9(9)V9(2)      COMP-3.                  
004100*                                 ORDERVÄRDE PREL SLUT-                   
004200*                                 KUNDPRIS, LOKAL VALUTA                  
004300*                                 ORDER VALUE, PREL CUSTOMER              
004400*                                 PRICE IN LOCAL CURRENCY                 
004500        05 LOR-KDVALISO      PIC X(3).                                    
004600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004700*                                 CURRENCY CODE BY ISO-STANDARD.          
004800*** END OF VILMAII-COPY LENGTH= 41 BYTES                                  
