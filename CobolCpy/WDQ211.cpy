000100 01  DIRL-WDQ211.                                                         
000200*                                 ORDERHUVUDSREGISTER KÖ                  
000300*                                 DIREKTLEVERANTÖRS SEGMENT               
000400*                                 FYSISK NYCKEL: WDQ211KY                 
000500*                                   (IDDC + IDLEVNR)                      
000600     03 DIRL-IDDC            PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 DIRL-IDLEVNR         PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001200     03 DIRL-KDORDSTA        PIC X(2).                                    
001300*                                 VOLVOORDERSTATUS                        
001400*                                 VOLVO ORDER STATUS                      
001500     03 DIRL-KDVIA           PIC X(2).                                    
001600*                                 KOD FöR LEVERANS VIA                    
001700*                                 CODE FOR DELIVERY VIA                   
001800     03 DIRL-KVDAGAR-DIFF    PIC S9(3)           COMP-3.                  
001900*                                 TRANSPORT DAY DIFF. (+/- CONTRA         
002000*                                  CDC)                                   
002100     03 DIRL-KVRADER         PIC S9(5)           COMP-3.                  
002200*                                 ANTAL RADER                             
002300*                                 NUMBER OF LINES                         
002400     03 DIRL-SUORDV          PIC S9(9)V9(2)      COMP-3.                  
002500*                                 SUMMA ORDERVÄRDE                        
002600*                                 TOTAL ORDER VALUE                       
002700     03 DIRL-TISKEPPN-DDC    PIC S9(7)           COMP-3.                  
002800*                                 SKEPPNINGSDATUM DLEV (ÅÅMMDD)           
002900*                                 SHIPPING DATE DIR.LEV. (YYMMDD)         
003000     03 DIRL-VKORDNTO        PIC S9(6)V9(1)      COMP-3.                  
003100*                                 ORDERVIKT NETTO (KG)                    
003200*                                 WEIGHT PER ORDER NETTO (KG)             
003300     03 DIRL-VLORDNTO        PIC S9(4)V9(3)      COMP-3.                  
003400*                                 ORDERVOLYM NETTO (M3)                   
003500*                                 NET VOLUME PER ORDER (M3)               
003600     03 DIRL-DEAL-PR-SUM.                                                 
003700*                                 DEALERPRIS (HUVUD)                      
003800        05 DIRL-SUORDV-LOC   PIC S9(9)V9(2)      COMP-3.                  
003900*                                 ORDERVÄRDE SLUTKUNDPRIS                 
004000*                                 I LOKAL VALUTA                          
004100*                                 ORDER VALUE, CUSTOMER PRICE             
004200*                                 IN LOCAL CURRENCY                       
004300        05 DIRL-SUORDV-LOCPREL                                            
004400                             PIC S9(9)V9(2)      COMP-3.                  
004500*                                 ORDERVÄRDE SLUTKUNDPRIS                 
004600*                                 I LOKAL VALUTA                          
004700*                                 ORDER VALUE, CUSTOMER PRICE             
004800*                                 IN LOCAL CURRENCY                       
004900        05 DIRL-KDVALISO     PIC X(3).                                    
005000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005100*                                 CURRENCY CODE BY ISO-STANDARD.          
005200*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
