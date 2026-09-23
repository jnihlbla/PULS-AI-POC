000100 01  BILL-WDE201.                                                         
000200*                                 TRANSPORTRELEASEREGISTER                
000300*                                 FAKTURA -> BILL-IT                      
000400*                                 TRANSPORT                               
000500*                                 FYSISK NYCKEL: IDSHIPM                  
000600     03 BILL-IDSHIPM         PIC 9(7).                                    
000700*                                 SKEPPNINGSNUMMER                        
000800*                                 SHIPMENT NO                             
000900     03 BILL-IDDC            PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 BILL-IDLANDX3-SEND   PIC X(3).                                    
001300*                                 LANDKOD SÄNDANDE LAND                   
001400*                                 COUNTRY CODE SENDING COUNTRY            
001500     03 BILL-IDLEVNR         PIC X(5).                                    
001600*                                 LEVERANTÖRNUMMER                        
001700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001800     03 BILL-KDFINDOC        PIC X(4).                                    
001900*                                 TYP FINANSIELLT DOKUMENT                
002000*                                 FINANCIAL DOCUMENT TYPE                 
002100     03 BILL-TISKEPPN        PIC S9(7)           COMP-3.                  
002200*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
002300*                                 SHIPPING DATE    (YYMMDD)               
002400     03 BILL-TISKPTID        PIC S9(7)           COMP-3.                  
002500*                                 SKEPPNINGSTID                           
002600     03 BILL-IDDC-EXP        PIC X(2).                                    
002700*                                 DC FÖR STUDS FLÖDE VID EXPORT           
002800*                                 DC FOR BOUNCE FLOW WHEN EXPORT          
002900*** END OF VILMAII-COPY LENGTH= 31 BYTES                                  
