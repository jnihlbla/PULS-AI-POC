000100 01  4102-WDGX4102.                                                       
000200*                                 RETUR AV RETUR                          
000300*                                 FYSISK NYCKEL: (IDKUNDNR +              
000400*                                 + IDRAPP + IDKOLLI + IDARTNR )          
000500     03 4102-IDKUNDNR        PIC S9(7)           COMP-3.                  
000600*                                 KUNDNUMMER                              
000700*                                 CUSTOMER NO                             
000800     03 4102-IDRAPP          PIC X(10).                                   
000900*                                 RAPPORT ID                              
001000*                                 REPORT ID                               
001100     03 4102-IDKOLLI         PIC S9(5)           COMP-3.                  
001200*                                 KOLLINUMMER                             
001300*                                 CASE NUMBER                             
001400     03 4102-IDARTNR         PIC S9(9)           COMP-3.                  
001500*                                 ARTIKELNUMMER                           
001600*                                 PART NUMBER                             
001700     03 4102-DAREGDAT        PIC 9(8).                                    
001800*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001900*                                 REGISTRATION DATE (YYYYMMDD)            
002000     03 4102-DARFSDAT        PIC 9(8).                                    
002100*                                 KLART FÖR TRANSPORT ÅÅÅÅMMDD            
002200*                                 READY FOR SHIPMENT  YYYYMMDD            
002300     03 4102-IDAVS           PIC X(20).                                   
002400*                                 IDENTITET PÅ DEN PERSON SOM             
002500*                                 SKICKAT IVÄG GODS                       
002600     03 4102-KDFEL           PIC S9(3)           COMP-3.                  
002700*                                 FELKOD                                  
002800     03 4102-KDKOLLI         PIC X(8).                                    
002900*                                 KOLLIKOD                                
003000*                                 KOLLI CODE                              
003100     03 4102-KVANTAL         PIC S9(7)           COMP-3.                  
003200*                                 ANTAL                                   
003300*                                 NUMBER                                  
003400     03 4102-PRARTBTO        PIC S9(7)V9(2)      COMP-3.                  
003500*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
003600*                                 GROSS SALES PRICE (SEK)                 
003700     03 4102-TENOTE          PIC X(40).                                   
003800*                                 NOTERINGSFÄLT                           
003900*                                 NOTE FIELD                              
004000     03 4102-VKORDBTO-KOLLI  PIC S9(6)V9(1)      COMP-3.                  
004100*                                 ORDERVIKT BRUTTO PER KOLLI              
004200*                                 ORDER WEIGHT GROSS PER CASE             
004300     03 4102-FLFAKT          PIC X.                                       
004400*                                 FAKTURERINGSFLAGGA                      
004500*                                 FLAG FOR INVOICE                        
004600     03 4102-FILLER          PIC X(3).                                    
004700*** END OF VILMAII-COPY LENGTH= 125 BYTES                                 
