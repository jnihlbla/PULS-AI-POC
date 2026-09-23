000100 01  W475894.                                                             
000200*                                 FAKTURA-RAD                             
000300*                                                                         
000400*                                 INVOICE LINE                            
000500*                                                                         
000600     03 IDPTYP               PIC X(3).                                    
000700*                                 POSTTYP                                 
000800*                                 RECORD TYPE                             
000900     03 IDFAKT               PIC S9(7)           COMP-3.                  
001000*                                 FAKTURANUMMER                           
001100*                                 INVOICE NO.                             
001200     03 IDDISTR              PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400*                                 DISTRICT NUMBER                         
001500     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001600*                                 KUNDNUMMER                              
001700*                                 CUSTOMER NO                             
001800     03 IDLANDX2             PIC X(2).                                    
001900*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002000*                                 2-LETTER CODE FOR COUNTRY               
002100     03 IDPRODNR             PIC S9(7)           COMP-3.                  
002200*                                 PRODUKTIONSNUMMER                       
002300*                                 PRODUCTION NUMBER                       
002400     03 KDFAKTYP             PIC X.                                       
002500*                                 FAKTURATYP                              
002600*                                 INVOICE TYPE                            
002700     03 TIFAKT               PIC S9(7)           COMP-3.                  
002800*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
002900*                                 INVOICING DATE   (YYMMDD)               
003000     03 KDFRAKT              PIC S9(3)           COMP-3.                  
003100*                                 FRAKTSÄTT DC TILL KUND                  
003200*                                 FREIGHT CODE                            
003300     03 SUORDV-FAKT          PIC S9(9)V9(2)      COMP-3.                  
003400*                                 FAKTURERAT ORDERVÄRDE                   
003500*                                 INVOICED ORDER VALUE                    
003600     03 IDSTATNR             PIC S9(9)           COMP-3.                  
003700*                                 STATISTISKT NUMMER                      
003800*                                 1 = NORSKT                              
003900*                                 2 = ENGELSKT                            
004000*                                 3 = BELGISKT                            
004100*                                 4 = PERUANSKT                           
004200*                                 5 = SVENSKT                             
004300*                                 6 =                                     
004400*                                 STATISTICAL NO.                         
004500     03 KDARTURS             PIC X(2).                                    
004600*                                 ARTIKELURSPRUNGSKOD                     
004700*                                 COUNTRY OF ORIGIN                       
004800     03 KDSRA                PIC S9(3)           COMP-3.                  
004900*                                 SRA-KOD                                 
005000*                                 SRA CODE                                
005100     03 KVLEVART             PIC S9(7)           COMP-3.                  
005200*                                 LEVERERAT ANTAL STYCK                   
005300*                                 DELIVERED QUANTITY                      
005400     03 VKLEV                PIC S9(6)V9(1)      COMP-3.                  
005500*                                 ORDER-VIKT NETTO (KG)                   
005600*                                 ORDER WEIGHT NET (KG)                   
005700     03 SUFAKT               PIC S9(9)V9(2)      COMP-3.                  
005800*                                 SUMMA FAKTURERAT BELOPP                 
005900*                                 TOTAL INVOICED AMOUNT                   
006000     03 SUEEC                PIC S9(9)V9(2)      COMP-3.                  
006100*                                 SUMMA FÖRS.PRIS EEC-URSPRUNG            
006200*                                 SUM NET PRICE EEC ORIGIN                
006300     03 SUEFTA               PIC S9(9)V9(2)      COMP-3.                  
006400*                                 SUMMA FÖRSÄLJN.PRIS EFTA-URSP           
006500*                                 SUM EFTA ORIG                           
006600     03 SUOEVR               PIC S9(9)V9(2)      COMP-3.                  
006700*                                 SUMMERING EJ EEC ELLER EFTA             
006800*                                 SUM-REM.                                
006900     03 IDARTNR              PIC S9(9)           COMP-3.                  
007000*                                 ARTIKELNUMMER                           
007100*                                 PART NUMBER                             
007200*** END OF VILMAII-COPY LENGTH= 79 BYTES                                  
