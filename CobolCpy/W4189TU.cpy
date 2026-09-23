000100 01  W4189TU.                                                             
000200*                                 FAKTURA- OCH KREDITRADINFO TILL         
000300*                                 DDC TULLISTOR                           
000400*                                                                         
000500*                                 INVOICE LINE AND CREDIT LINE            
000600*                                 INFORMATION TO THE DDC                  
000700*                                 CUSTOMS INFORMATION LISTS               
000800*                                                                         
000900     03 IDPTYP               PIC X(3).                                    
001000*                                 POSTTYP                                 
001100*                                 RECORD TYPE                             
001200     03 IDDC                 PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400*                                 WAREHOUSE IDENTIFIER                    
001500     03 IDVAT-DDC            PIC X(17).                                   
001600*                                 MOMSREGISTRERINGSNUMMER DDC             
001700*                                 VAT REGISTRATION NUMBER DDC             
001800     03 IDLANDX2             PIC X(2).                                    
001900*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002000*                                 2-LETTER CODE FOR COUNTRY               
002100     03 IDVAT                PIC X(17).                                   
002200*                                 MOMSREGISTRERINGSNUMMER                 
002300*                                 VAT REGISTRATION NUMBER                 
002400     03 IDFAKT               PIC S9(7)           COMP-3.                  
002500*                                 FAKTURANUMMER                           
002600*                                 INVOICE NO.                             
002700     03 IDDISTR              PIC S9(5)           COMP-3.                  
002800*                                 DISTRIKTNUMMER                          
002900*                                 DISTRICT NUMBER                         
003000     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
003100*                                 KUNDNUMMER                              
003200*                                 CUSTOMER NO                             
003300     03 IDPRODNR             PIC S9(7)           COMP-3.                  
003400*                                 PRODUKTIONSNUMMER                       
003500*                                 PRODUCTION NUMBER                       
003600     03 IDKUNDRF             PIC X(10).                                   
003700*                                 KUNDENS REFERENS (ORDERID)              
003800*                                 CUSTOMER REFERENCE (ORDER ID)           
003900     03 KDFAKTYP             PIC X.                                       
004000*                                 FAKTURATYP                              
004100*                                 INVOICE TYPE                            
004200     03 TIFAKT               PIC S9(7)           COMP-3.                  
004300*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
004400*                                 INVOICING DATE   (YYMMDD)               
004500     03 KDFRAKT              PIC S9(3)           COMP-3.                  
004600*                                 FRAKTSÄTT DC TILL KUND                  
004700*                                 FREIGHT CODE                            
004800     03 SUORDV-FAKT          PIC S9(9)V9(2)      COMP-3.                  
004900*                                 FAKTURERAT ORDERVÄRDE                   
005000*                                 INVOICED ORDER VALUE                    
005100     03 SUORDV-FAKT-LOC      PIC S9(9)V9(2)      COMP-3.                  
005200*                                 FAKTURERAT ORDERVÄRDE                   
005300*                                 INVOICED ORDER VALUE                    
005400     03 SUORDV-FAKT-DDC      PIC S9(9)V9(2)      COMP-3.                  
005500*                                 FAKTURERAT ORDERVÄRDE                   
005600*                                 INVOICED ORDER VALUE                    
005700     03 VKORDBTO-FAKT        PIC S9(6)V9(1)      COMP-3.                  
005800*                                 ORDERVIKT BRUTTO PER FAKTURA            
005900*                                 ORDER WEIGHT GROSS PER INVOICE          
006000     03 IDSTATNR             PIC S9(9)           COMP-3.                  
006100*                                 STATISTISKT NUMMER                      
006200*                                 1 = NORSKT                              
006300*                                 2 = ENGELSKT                            
006400*                                 3 = BELGISKT                            
006500*                                 4 = PERUANSKT                           
006600*                                 5 = SVENSKT                             
006700*                                 6 =                                     
006800*                                 STATISTICAL NO.                         
006900     03 KDARTURS             PIC X(2).                                    
007000*                                 ARTIKELURSPRUNGSKOD                     
007100*                                 COUNTRY OF ORIGIN                       
007200     03 KDSORT               PIC X(2).                                    
007300*                                 SORT-KOD                                
007400*                                 UNIT OF MEASURE                         
007500     03 KDSRA                PIC S9(3)           COMP-3.                  
007600*                                 SRA-KOD                                 
007700*                                 SRA CODE                                
007800     03 KVLEVART             PIC S9(7)           COMP-3.                  
007900*                                 LEVERERAT ANTAL STYCK                   
008000*                                 DELIVERED QUANTITY                      
008100     03 VKLEV                PIC S9(6)V9(1)      COMP-3.                  
008200*                                 ORDER-VIKT NETTO (KG)                   
008300*                                 ORDER WEIGHT NET (KG)                   
008400     03 SUFKTBEL             PIC S9(9)V9(2)      COMP-3.                  
008500*                                 SUMMA FAKTURERAT BELOPP                 
008600*                                 TOTAL INVOICED AMOUNT                   
008700     03 SUFKTBEL-LOC         PIC S9(9)V9(2)      COMP-3.                  
008800*                                 SUMMA FAKTURERAT BELOPP                 
008900*                                 TOTAL INVOICED AMOUNT                   
009000     03 SUFKTBEL-DDC         PIC S9(9)V9(2)      COMP-3.                  
009100*                                 SUMMA FAKTURERAT BELOPP                 
009200*                                 TOTAL INVOICED AMOUNT                   
009300     03 SUEEC                PIC S9(9)V9(2)      COMP-3.                  
009400*                                 SUMMA FÖRS.PRIS EEC-URSPRUNG            
009500*                                 SUM NET PRICE EEC ORIGIN                
009600     03 SUEEC-LOC            PIC S9(9)V9(2)      COMP-3.                  
009700*                                 SUMMA FÖRS.PRIS EEC-URSPRUNG            
009800*                                 SUM NET PRICE EEC ORIGIN                
009900     03 SUEEC-DDC            PIC S9(9)V9(2)      COMP-3.                  
010000*                                 SUMMA FÖRS.PRIS EEC-URSPRUNG            
010100*                                 SUM NET PRICE EEC ORIGIN                
010200     03 SUEFTA               PIC S9(9)V9(2)      COMP-3.                  
010300*                                 SUMMA FÖRSÄLJN.PRIS EFTA-URSP           
010400*                                 SUM EFTA ORIG                           
010500     03 SUEFTA-LOC           PIC S9(9)V9(2)      COMP-3.                  
010600*                                 SUMMA FÖRSÄLJN.PRIS EFTA-URSP           
010700*                                 SUM EFTA ORIG                           
010800     03 SUEFTA-DDC           PIC S9(9)V9(2)      COMP-3.                  
010900*                                 SUMMA FÖRSÄLJN.PRIS EFTA-URSP           
011000*                                 SUM EFTA ORIG                           
011100     03 SUOEVR               PIC S9(9)V9(2)      COMP-3.                  
011200*                                 SUMMERING EJ EEC ELLER EFTA             
011300*                                 SUM-REM.                                
011400     03 SUOEVR-LOC           PIC S9(9)V9(2)      COMP-3.                  
011500*                                 SUMMERING EJ EEC ELLER EFTA             
011600*                                 SUM-REM.                                
011700     03 SUOEVR-DDC           PIC S9(9)V9(2)      COMP-3.                  
011800*                                 SUMMERING EJ EEC ELLER EFTA             
011900*                                 SUM-REM.                                
012000     03 IDARTNR              PIC S9(9)           COMP-3.                  
012100*                                 ARTIKELNUMMER                           
012200*                                 PART NUMBER                             
012300*** END OF VILMAII-COPY LENGTH= 191 BYTES                                 
