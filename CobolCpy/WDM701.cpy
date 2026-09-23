000100 01  HUV-WDM701-CTX.                                                      
000200*                                 TULLSYSTEM                              
000300*                                 TULL-HUVUD UPPGIFTER                    
000400*                                 FYSISK NYCKEL: WDM701KY                 
000500*                                 IDFAKT  + IDORDNR7 +                    
000600*                                 IDKOLLI + IDPRODNR                      
000700     03 HUV-IDFAKT           PIC S9(7)           COMP-3.                  
000800*                                 FAKTURANUMMER                           
000900*                                 INVOICE NO.                             
001000     03 HUV-IDORDNR7         PIC S9(7)           COMP-3.                  
001100*                                 ORDERNUMMER                             
001200*                                 ORDER NUMBER                            
001300     03 HUV-IDKOLLI          PIC S9(5)           COMP-3.                  
001400*                                 KOLLINUMMER                             
001500*                                 CASE NUMBER                             
001600     03 HUV-IDPRODNR         PIC S9(7)           COMP-3.                  
001700*                                 PRODUKTIONSNUMMER                       
001800*                                 PRODUCTION NUMBER                       
001900     03 HUV-ADKOPARE-GRP.                                                 
002000*                                 KÖPARE ADRESS                           
002100*                                 BUYER ADDRESS                           
002200        05 HUV-ADKOPARE-RAD1 PIC X(35).                                   
002300*                                 KÖPARE ADRESS RAD 1                     
002400*                                 BUYER ADDRESS LINE 1                    
002500        05 HUV-ADKOPARE-RAD2 PIC X(35).                                   
002600*                                 KÖPARE ADRESS RAD 2                     
002700*                                 BUYER ADDRESS LINE 2                    
002800     03 HUV-BEKOPARE-GRP.                                                 
002900*                                 KÖPARE NAMN                             
003000*                                 BUYER NAME                              
003100        05 HUV-BEKOPARE-RAD1 PIC X(35).                                   
003200*                                 KÖPARE NAMN RAD 1                       
003300*                                 BUYER NAME LINE 1                       
003400        05 HUV-BEKOPARE-RAD2 PIC X(35).                                   
003500*                                 KÖPARE NAMN RAD 2                       
003600*                                 BUYER NAME LINE 2                       
003700     03 HUV-IDDISTR          PIC S9(5)           COMP-3.                  
003800*                                 DISTRIKTNUMMER                          
003900*                                 DISTRICT NUMBER                         
004000     03 HUV-IDKUNDNR         PIC S9(7)           COMP-3.                  
004100*                                 KUNDNUMMER                              
004200*                                 CUSTOMER NO                             
004300     03 HUV-IDSKEPPN         PIC S9(7)           COMP-3.                  
004400*                                 SKEPPNINGSNUMMER                        
004500*                                 SHIPMENT NO                             
004600     03 HUV-IDTULL.                                                       
004700*                                 IDENTITET TULL SÄNDNING                 
004800*                                 IDENTITY CUSTOMS TRANSMISSION           
004900        05 HUV-IDTULFTG      PIC X(2).                                    
005000*                                 IDENTIFIERARE TULLANDE FÖRETAG          
005100*                                                                         
005200*                                 IDENTIFIER COMPANY TO CUSTOMS           
005300        05 HUV-IDTULLNR      PIC 9(7).                                    
005400*                                 NUMMERSERIE INGÅENDE I TULLID           
005500*                                                                         
005600*                                 SERIAL NUMBER IN CUSTOMS ID             
005700        05 HUV-RETULKS       PIC 9.                                       
005800*                                 KONTROLLSIFFRA TULLID                   
005900*                                 CHECK DIGIT CUSTOMS ID                  
006000     03 HUV-IDUSER           PIC X(8).                                    
006100*                                 ANVÄNDARENS SÄKERHETS ID                
006200*                                 USER SECURITY-IDENTITY                  
006300     03 HUV-KDFAKTYP         PIC X.                                       
006400*                                 FAKTURATYP                              
006500*                                 INVOICE TYPE                            
006600     03 HUV-KDFRAKT          PIC S9(3)           COMP-3.                  
006700*                                 FRAKTSÄTT DC TILL KUND                  
006800*                                 FREIGHT CODE                            
006900     03 HUV-KDORDKL          PIC S9              COMP-3.                  
007000*                                 ORDERKLASS                              
007100*                                 ORDER CLASS                             
007200     03 HUV-TIFAKT           PIC S9(7)           COMP-3.                  
007300*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
007400*                                 INVOICING DATE   (YYMMDD)               
007500     03 HUV-FLORDSPE         PIC X.                                       
007600*                                 SPECIALORDERFLAGGA                      
007700*                                 SPECIAL ORDER FLAG                      
007800     03 HUV-IDBOKN           PIC X(15).                                   
007900*                                 BOKNINGSNUMMER                          
008000*                                 BOOKING NUMBER                          
008100     03 HUV-IDLBBET          PIC X(12).                                   
008200*                                 LASTBÄRARBETECKNING                     
008300*                                 TRAILER NUMBER                          
008400     03 HUV-PRKURS           PIC S9(6)V9(5)      COMP-3.                  
008500*                                 VALUTAKURS                              
008600*                                 CURRENCY EXCHANGE RATE                  
008700     03 HUV-BELEVVIL         PIC X(35).                                   
008800*                                 LEVERANSVILLKOR                         
008900*                                 DELIVERY TERMS                          
009000     03 HUV-FLCONTAIN        PIC X.                                       
009100*                                 CONTAINER FULL ELLER INTE J/N           
009200*                                 SHOWS IF CONTAINER FULL  J/N            
009300     03 HUV-IDDC             PIC X(2).                                    
009400*                                 IDENTIFIERARE LAGER                     
009500*                                 WAREHOUSE IDENTIFIER                    
009600     03 HUV-IDPARTNR         PIC X(9).                                    
009700*                                 FINANCIELL KUND                         
009800*                                 FINANCIAL CUST                          
009900     03 HUV-KDEMBTYP         PIC S9              COMP-3.                  
010000*                                 EMBALLAGETYP                            
010100*                                 PACKAGE TYPE                            
010200     03 HUV-KDTRPTYP         PIC 9.                                       
010300*                                 TRANSPORTMEDEL FÖR GODS                 
010400*                                 WAY OF TRANSPORTATION OF GOODS          
010500     03 HUV-PRAVDRAG         PIC S9(7)V9(2)      COMP-3.                  
010600*                                 AVDRAGSBELOPP                           
010700*                                 DEDUCTION                               
010800     03 HUV-PREMBHNT         PIC S9(7)V9(2)      COMP-3.                  
010900*                                 EMBALLAGE O HANTERINGSKOST              
011000*                                 PACKING O HANDL COSTS                   
011100     03 HUV-PRFRAKT          PIC S9(7)V9(2)      COMP-3.                  
011200*                                 FRAKTKOSTNAD                            
011300*                                 FREIGHT COST                            
011400     03 HUV-PRFOERS          PIC S9(7)V9(2)      COMP-3.                  
011500*                                 FÖRSÄKRINGSPREMIE                       
011600*                                 INSURANCE FEE                           
011700     03 HUV-PRLEGKST         PIC S9(7)V9(2)      COMP-3.                  
011800*                                 LEGALISERINSKOSTNAD                     
011900*                                 LEGALIZATION FEE                        
012000     03 HUV-VKORDBTO-KOLLI   PIC S9(6)V9(1)      COMP-3.                  
012100*                                 ORDERVIKT BRUTTO PER KOLLI              
012200*                                 ORDER WEIGHT GROSS PER CASE             
012300     03 HUV-TIREGDAT         PIC S9(7)           COMP-3.                  
012400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
012500*                                 REGISTRATION DATE (YYMMDD)              
012600     03 HUV-TIREGTID         PIC S9(7)           COMP-3.                  
012700*                                 REGISTRERINGSTID                        
012800*                                 GENERAL REGISTRATION TIME               
012900     03 HUV-FLKLAR           PIC X.                                       
013000*                                 AVSLUTNINGSMARKERING                    
013100*                                 FINISHED FLAG                           
013200     03 HUV-IDFORDREG        PIC X(30).                                   
013300*                                 FORDON REG. NUMMER                      
013400*                                 VEHICLE REG. NUMBER                     
013500     03 HUV-IDSIGILL         PIC X(15).                                   
013600*                                 SIGILL IDENTITET                        
013700*                                 SEAL IDENTITY                           
013800*** END OF VILMAII-COPY LENGTH= 358 BYTES                                 
