000100 01  W4758TU-CTX.                                                         
000200*                                 FAKTURA- OCH KREDITRADINFO TILL         
000300*                                 SDC TULLISTOR                           
000400*                                                                         
000500*                                 INVOICE LINE AND CREDIT LINE            
000600*                                 INFORMATION TO THE SDC                  
000700*                                 CUSTOMS INFORMATION LISTS               
000800*                                                                         
000900     03 IDPTYP               PIC X(3).                                    
001000*                                 POSTTYP                                 
001100*                                 RECORD TYPE                             
001200     03 IDDC                 PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400*                                 WAREHOUSE IDENTIFIER                    
001500     03 IDFAKT               PIC S9(7)           COMP-3.                  
001600*                                 FAKTURANUMMER                           
001700*                                 INVOICE NO.                             
001800     03 IDDISTR              PIC S9(5)           COMP-3.                  
001900*                                 DISTRIKTNUMMER                          
002000*                                 DISTRICT NUMBER                         
002100     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
002200*                                 KUNDNUMMER                              
002300*                                 CUSTOMER NO                             
002400     03 IDLANDX2             PIC X(2).                                    
002500*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002600*                                 2-LETTER CODE FOR COUNTRY               
002700     03 IDPRODNR             PIC S9(7)           COMP-3.                  
002800*                                 PRODUKTIONSNUMMER                       
002900*                                 PRODUCTION NUMBER                       
003000     03 IDKUNDRF             PIC X(10).                                   
003100*                                 KUNDENS REFERENS (ORDERID)              
003200*                                 CUSTOMER REFERENCE (ORDER ID)           
003300     03 KDFAKTYP             PIC X.                                       
003400*                                 FAKTURATYP                              
003500*                                 INVOICE TYPE                            
003600     03 TIFAKT               PIC S9(7)           COMP-3.                  
003700*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
003800*                                 INVOICING DATE   (YYMMDD)               
003900     03 KDFRAKT              PIC S9(3)           COMP-3.                  
004000*                                 FRAKTSÄTT DC TILL KUND                  
004100*                                 FREIGHT CODE                            
004200     03 SUORDV-FAKT          PIC S9(9)V9(2)      COMP-3.                  
004300*                                 FAKTURERAT ORDERVÄRDE                   
004400*                                 INVOICED ORDER VALUE                    
004500     03 SUORDV-FAKT-LOC      PIC S9(9)V9(2)      COMP-3.                  
004600*                                 FAKTURERAT ORDERVÄRDE                   
004700*                                 INVOICED ORDER VALUE                    
004800     03 VKORDBTO-FAKT        PIC S9(6)V9(1)      COMP-3.                  
004900*                                 ORDERVIKT BRUTTO PER FAKTURA            
005000*                                 ORDER WEIGHT GROSS PER INVOICE          
005100     03 IDSTATNR             PIC S9(9)           COMP-3.                  
005200*                                 STATISTISKT NUMMER                      
005300*                                 1 = NORSKT                              
005400*                                 2 = ENGELSKT                            
005500*                                 3 = BELGISKT                            
005600*                                 4 = PERUANSKT                           
005700*                                 5 = SVENSKT                             
005800*                                 6 =                                     
005900*                                 STATISTICAL NO.                         
006000     03 KDARTURS             PIC X(2).                                    
006100*                                 ARTIKELURSPRUNGSKOD                     
006200*                                 COUNTRY OF ORIGIN                       
006300     03 KDSORT               PIC X(2).                                    
006400*                                 SORT-KOD                                
006500*                                 UNIT OF MEASURE                         
006600     03 KDSRA                PIC S9(3)           COMP-3.                  
006700*                                 SRA-KOD                                 
006800*                                 SRA CODE                                
006900     03 KVLEVART             PIC S9(7)           COMP-3.                  
007000*                                 LEVERERAT ANTAL STYCK                   
007100*                                 DELIVERED QUANTITY                      
007200     03 VKLEV                PIC S9(6)V9(1)      COMP-3.                  
007300*                                 ORDER-VIKT NETTO (KG)                   
007400*                                 ORDER WEIGHT NET (KG)                   
007500     03 SUFAKT               PIC S9(9)V9(2)      COMP-3.                  
007600*                                 SUMMA FAKTURERAT BELOPP                 
007700*                                 TOTAL INVOICED AMOUNT                   
007800     03 SUFAKT-LOC           PIC S9(9)V9(2)      COMP-3.                  
007900*                                 SUMMA FAKTURERAT BELOPP                 
008000*                                 TOTAL INVOICED AMOUNT                   
008100     03 SUEEC                PIC S9(9)V9(2)      COMP-3.                  
008200*                                 SUMMA FÖRS.PRIS EEC-URSPRUNG            
008300*                                 SUM NET PRICE EEC ORIGIN                
008400     03 SUEEC-LOC            PIC S9(9)V9(2)      COMP-3.                  
008500*                                 SUMMA FÖRS.PRIS EEC-URSPRUNG            
008600*                                 SUM NET PRICE EEC ORIGIN                
008700     03 SUEFTA               PIC S9(9)V9(2)      COMP-3.                  
008800*                                 SUMMA FÖRSÄLJN.PRIS EFTA-URSP           
008900*                                 SUM EFTA ORIG                           
009000     03 SUEFTA-LOC           PIC S9(9)V9(2)      COMP-3.                  
009100*                                 SUMMA FÖRSÄLJN.PRIS EFTA-URSP           
009200*                                 SUM EFTA ORIG                           
009300     03 SUOEVR               PIC S9(9)V9(2)      COMP-3.                  
009400*                                 SUMMERING EJ EEC ELLER EFTA             
009500*                                 SUM-REM.                                
009600     03 SUOEVR-LOC           PIC S9(9)V9(2)      COMP-3.                  
009700*                                 SUMMERING EJ EEC ELLER EFTA             
009800*                                 SUM-REM.                                
009900     03 IDARTNR              PIC S9(9)           COMP-3.                  
010000*                                 ARTIKELNUMMER                           
010100*                                 PART NUMBER                             
010200*** END OF VILMAII-COPY LENGTH= 127 BYTES                                 
