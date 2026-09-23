000100 01  SGMT-WDE111.                                                         
000200*                                 TRANSPORTRELEASEREGISTER                
000300*                                 GODSMOTTAGARE                           
000400*                                 FYSISK NYCKEL: WDE111KY                 
000500*                                 (IDDISTR, IDKUNDNR)                     
000600     03 SGMT-IDDISTR         PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800*                                 DISTRICT NUMBER                         
000900     03 SGMT-IDKUNDNR        PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100*                                 CUSTOMER NO                             
001200     03 SGMT-FLCOD           PIC X.                                       
001300*                                 KONTANTBETALANDE KUND                   
001400*                                 CASH ON DELIVERY CUSTOMER               
001500     03 SGMT-IDDC            PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700*                                 WAREHOUSE IDENTIFIER                    
001800     03 SGMT-IDPARTNR        PIC X(9).                                    
001900*                                 PARTNERNUMMER                           
002000*                                 PARTNER NO                              
002100     03 SGMT-KDFKBIL         PIC S9              COMP-3.                  
002200*                                 INSTRUKTION KUNDBILAGA                  
002300*                                 INSTR.CUST.ENCLOSURE                    
002400     03 SGMT-KDFORSKN        PIC S9(3)           COMP-3.                  
002500*                                 FÖRSÄKRANSKOD                           
002600*                                 DECLARATION CODE                        
002700     03 SGMT-KDLEVVIL        PIC S9              COMP-3.                  
002800*                                 LEVERANSVILLKOR                         
002900*                                 TERMS OF DELIVERY                       
003000     03 SGMT-KDORDKL-MAX     PIC S9              COMP-3.                  
003100*                                 HÖGSTA ORDERKLASS I SKEPPNING           
003200*                                 HIGHEST ORDER CLASS OF SHIPMENT         
003300     03 SGMT-KDVALISO        PIC X(3).                                    
003400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003500*                                 CURRENCY CODE BY ISO-STANDARD.          
003600     03 SGMT-PRKURS          PIC S9(6)V9(5)      COMP-3.                  
003700*                                 VALUTAKURS                              
003800*                                 CURRENCY EXCHANGE RATE                  
003900     03 SGMT-TISKEPPN-9KOMPL PIC S9(7)           COMP-3.                  
004000*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
004100*                                 9 KOMPLIMENT  9999999 - AAMMDD          
004200*                                 SHIPPING DATE    (YYMMDD)               
004300*                                 9 COMPLIMENT                            
004400*** END OF VILMAII-COPY LENGTH= 37 BYTES                                  
