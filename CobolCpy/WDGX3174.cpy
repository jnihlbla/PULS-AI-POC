000100 01  3174-WDGX3174.                                                       
000200*                                 BYTES PROFORMAFAKTURA                   
000300*                                 KOLLI-SEGMENT                           
000400*                                 FYSISK NYCKEL: IDKOLLI                  
000500     03 3174-IDKOLLI         PIC S9(5)           COMP-3.                  
000600*                                 KOLLINUMMER                             
000700*                                 CASE NUMBER                             
000800     03 3174-KDTRSTAT        PIC S9              COMP-3.                  
000900*                                 TRANSAKTIONSSTATUS                      
001000*                                 TRANSACTIONS-STATUS                     
001100     03 3174-VKORDBTO-KOLLI  PIC S9(6)V9(1)      COMP-3.                  
001200*                                 ORDERVIKT BRUTTO PER KOLLI              
001300*                                 ORDER WEIGHT GROSS PER CASE             
001400     03 3174-VLORDBTO-KOLLI  PIC S9(4)V9(3)      COMP-3.                  
001500*                                 ORDERVOLYM BRUTTO KOLLI                 
001600*                                 ORDER VOL GR/CASE                       
001700*** END OF VILMAII-COPY LENGTH= 12 BYTES                                  
