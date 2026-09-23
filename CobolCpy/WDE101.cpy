000100 01  SHIP-WDE101.                                                         
000200*                                 TRANSPORTRELEASEREGISTER                
000300*                                 TRANSPORT                               
000400*                                 FYSISK NYCKEL: IDSHIPM                  
000500     03 SHIP-IDSHIPM         PIC 9(7).                                    
000600*                                 SKEPPNINGSNUMMER                        
000700*                                 SHIPMENT NO                             
000800     03 SHIP-IDTRPTNR        PIC S9(3)           COMP-3.                  
000900*                                 TRANSPORTIDENTITET                      
001000*                                 TRANSPORT IDENTITY                      
001100     03 SHIP-IDLBBET         PIC X(12).                                   
001200*                                 LASTBÄRARBETECKNING                     
001300*                                 TRAILER NUMBER                          
001400     03 SHIP-IDDC            PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600*                                 WAREHOUSE IDENTIFIER                    
001700     03 SHIP-IDLANDX3-SEND   PIC X(3).                                    
001800*                                 LANDKOD SÄNDANDE LAND                   
001900*                                 COUNTRY CODE SENDING COUNTRY            
002000     03 SHIP-KDFINDOC        PIC X(4).                                    
002100*                                 TYP FINANSIELLT DOKUMENT                
002200*                                 FINANCIAL DOCUMENT TYPE                 
002300     03 SHIP-TISKEPPN        PIC S9(7)           COMP-3.                  
002400*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
002500*                                 SHIPPING DATE    (YYMMDD)               
002600     03 SHIP-TISKPTID        PIC S9(7)           COMP-3.                  
002700*                                 SKEPPNINGSTID                           
002800     03 SHIP-KVANTEX         PIC S9              COMP-3.                  
002900*                                 ANTAL EXEMPLAR                          
003000     03 SHIP-FLSKRIV-NU      PIC X.                                       
003100*                                 J/Y = SKRIV BEGÄRD LISTA                
003200*                                 J/Y = PRINT REPORT NOW                  
003300     03 SHIP-KDKLAR          PIC X.                                       
003400*                                 STATUS FÖR SKEPPNING                    
003500*                                 STATUS OF SHIPMENT                      
003600     03 SHIP-IDDC-EXP        PIC X(2).                                    
003700*                                 DC FÖR STUDS FLÖDE VID EXPORT           
003800*                                 DC FOR BOUNCE FLOW WHEN EXPORT          
003900     03 SHIP-KDFAKSTA-EXP    PIC X.                                       
004000*                                 DUBBELFAKTURERING STATUS                
004100*                                 STATUS CODE DOUBLE INVOICING            
004200     03 SHIP-BELEVVIL        PIC X(35).                                   
004300*                                 LEVERANSVILLKOR                         
004400*                                 DELIVERY TERMS                          
004500     03 SHIP-IDSYSTEM        PIC X(4).                                    
004600*                                 VOLVO VCCS SYSTEMNUMMER                 
004700*                                 VOLVO VCCS SYSTEM NUMBER                
004800     03 SHIP-SUNTO-TOT       PIC S9(11)V9(2)     COMP-3.                  
004900*                                 TOTAL SALES AMOUNT EXCL. VAT            
005000     03 SHIP-KDVALISO-BET    PIC X(3).                                    
005100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005200*                                 CURRENCY CODE BY ISO-STANDARD.          
005300     03 SHIP-PRKURS-BET      PIC S9(6)V9(5)      COMP-3.                  
005400*                                 VALUTAKURS                              
005500*                                 CURRENCY EXCHANGE RATE                  
005600     03 SHIP-FLFARLIG        PIC X.                                       
005700*                                 FARLIGT GODS-FLAGGA                     
005800*                                 DENGEROUS GOODS FLAG                    
005900     03 SHIP-KDVALISO-EXP    PIC X(3).                                    
006000*                                 VALUTAKOD I EXP.FLÖDE(LOK. VAL)         
006100*                                 CURRENCY FOR EXPORT (LOC. CURR)         
006200     03 SHIP-SUORDV-EXP      PIC S9(9)V9(2)      COMP-3.                  
006300*                                 SUMMA ORDERVÄRDE EXPORTFLÖDE            
006400*                                 TOTAL ORDER VALUE EXPORT FLOW           
006500     03 SHIP-SUORDV-FAKT     PIC S9(9)V9(2)      COMP-3.                  
006600*                                 FAKTURERAT ORDERVÄRDE                   
006700*                                 INVOICED ORDER VALUE                    
006800     03 SHIP-VKORDBTO-FAKT   PIC S9(6)V9(1)      COMP-3.                  
006900*                                 ORDERVIKT BRUTTO PER FAKTURA            
007000*                                 ORDER WEIGHT GROSS PER INVOICE          
007100     03 SHIP-VLORDBTO-FAKT   PIC S9(4)V9(3)      COMP-3.                  
007200*                                 ORDERVOLYM BRUTTO PER FAKTURA           
007300*                                 ORDER VOLUME GROSS PER INVOICE          
007400*** END OF VILMAII-COPY LENGTH= 123 BYTES                                 
