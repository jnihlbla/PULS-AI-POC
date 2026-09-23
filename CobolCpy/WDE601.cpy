000100 01  VORD-WDE601.                                                         
000200*                                 KOLLIREGISTER                           
000300*                                 ORDERHUVUD PRODUKTIONSORDER             
000400*                                 FYSISK NYCKEL: IDPRODNR                 
000500     03 VORD-IDPRODNR        PIC S9(7)           COMP-3.                  
000600*                                 PRODUKTIONSNUMMER                       
000700*                                 PRODUCTION NUMBER                       
000800     03 VORD-BEGMRK.                                                      
000900*                                 GODSMÄRKE                               
001000*                                 GOODS MARKING                           
001100        05 VORD-BEGMRK-RAD1  PIC X(30).                                   
001200*                                 GODSMÄRKE  RAD1                         
001300*                                 GOODS MARKING  LINE1                    
001400        05 VORD-BEGMRK-RAD2  PIC X(30).                                   
001500*                                 GODSMÄRKE  RAD2                         
001600*                                 GOODS MARKING  LINE2                    
001700     03 VORD-IDDISTR         PIC S9(5)           COMP-3.                  
001800*                                 DISTRIKTNUMMER                          
001900*                                 DISTRICT NUMBER                         
002000     03 VORD-IDKUNDNR        PIC S9(7)           COMP-3.                  
002100*                                 KUNDNUMMER                              
002200*                                 CUSTOMER NO                             
002300     03 VORD-KDORDKL         PIC S9              COMP-3.                  
002400*                                 ORDERKLASS                              
002500*                                 ORDER CLASS                             
002600     03 VORD-IDLOTNR         PIC S9(3)           COMP-3.                  
002700*                                 VAGN-NUMMER                             
002800*                                 ORDERLOT NUMBER                         
002900     03 VORD-KDORDLOT        PIC X(2).                                    
003000*                                 ORDERLOTTSALTERNATIV                    
003100*                                 ORDER LOT ALTERNATIVE                   
003200     03 VORD-KDPERSON        PIC S9(3)           COMP-3.                  
003300*                                 PERSONKOD                               
003400*                                 STAFF CODE                              
003500     03 VORD-ADLEVPL         PIC S9(3)           COMP-3.                  
003600*                                 LEVERANSPLATS                           
003700*                                 DELIVERY PLACE                          
003800     03 VORD-FLCONTL         PIC X.                                       
003900*                                 KONTINUERLIG LASTNING ?                 
004000*                                 CONTINOUS LOADING ?                     
004100     03 VORD-FLDIRLEV        PIC X.                                       
004200*                                 DIREKTLEVERANS ?                        
004300*                                 DIRECT DELIVERY ?                       
004400     03 VORD-FLSPARR         PIC X.                                       
004500*                                 SPÄRRAD ORDER ?                         
004600*                                 BLOCKED ORDER ?                         
004700     03 VORD-FLMANORD        PIC X.                                       
004800*                                 MANUELL ORDER EFTERAVBOKNING            
004900*                                 MANUAL ORDER                            
005000     03 VORD-IDDC            PIC X(2).                                    
005100*                                 IDENTIFIERARE LAGER                     
005200*                                 WAREHOUSE IDENTIFIER                    
005300     03 VORD-KDFAKTYP        PIC X.                                       
005400*                                 FAKTURATYP                              
005500*                                 INVOICE TYPE                            
005600     03 VORD-KDFRAKT         PIC S9(3)           COMP-3.                  
005700*                                 FRAKTSÄTT DC TILL KUND                  
005800*                                 FREIGHT CODE                            
005900     03 VORD-KDMETOD         PIC S9              COMP-3.                  
006000*                                 PLOCK METOD                             
006100*                                 PICKING METHOD                          
006200     03 VORD-KDORDSTA        PIC S9              COMP-3.                  
006300*                                 VOLVOORDERSTATUS                        
006400*                                 VOLVO ORDER STATUS                      
006500     03 VORD-KVKOLLI         PIC S9(5)           COMP-3.                  
006600*                                 ANTAL KOLLI                             
006700*                                 NBR OF CASES                            
006800     03 VORD-KVKOLLI-FAKT    PIC S9(5)           COMP-3.                  
006900*                                 ANTAL FAKTURERADE KOLLIN                
007000*                                 QUANTITY CASES INVOICED                 
007100     03 VORD-KVKOLLI-LAST    PIC S9(5)           COMP-3.                  
007200*                                 ANTAL LASTNINGSRAPPORTERADE             
007300*                                 KOLLIN                                  
007400     03 VORD-KVKOLLI-FL      PIC S9(5)           COMP-3.                  
007500*                                 ANTAL FAKTURERADE ELLER                 
007600*                                 LASTNINGSRAPPORTERADE KOLLIN            
007700     03 VORD-KVKOLPAC        PIC S9(5)           COMP-3.                  
007800*                                 ANTAL PACK RAPPORTERADE KOLLI           
007900*                                 NBR OF PACK REPORTED CASES              
008000     03 VORD-KVORDRAD        PIC S9(5)           COMP-3.                  
008100*                                 ANTAL ORDERRADER                        
008200*                                 NUMBER OF ORDER LINES                   
008300     03 VORD-KVORDRAD-PACK   PIC S9(5)           COMP-3.                  
008400*                                 ANTAL PACKADE ORDERRADER                
008500     03 VORD-SUORDV          PIC S9(9)V9(2)      COMP-3.                  
008600*                                 SUMMA ORDERVÄRDE                        
008700*                                 TOTAL ORDER VALUE                       
008800     03 VORD-SUORDV-PACK     PIC S9(9)V9(2)      COMP-3.                  
008900*                                 SUMMA PACKAT ORDERVÄRDE                 
009000*                                 TOTAL PACKED ORDER VALUE                
009100     03 VORD-SUORDV-FL       PIC S9(9)V9(2)      COMP-3.                  
009200*                                 ORDERVÄRDE FAKTURERAT ELLER             
009300*                                 LASTAT                                  
009400     03 VORD-TIREGTID        PIC S9(7)           COMP-3.                  
009500*                                 REGISTRERINGSTID                        
009600*                                 GENERAL REGISTRATION TIME               
009700     03 VORD-DABEGPAC        PIC 9(8).                                    
009800*                                 BEGÄRD PACKNINGSDAG  (YYYYMMDD)         
009900*                                 REQUESTED PACKING DATE                  
010000     03 VORD-TIFAKT-SK       PIC S9(7)           COMP-3.                  
010100*                                 FAKTURADATUM SENASTE KOLLI              
010200     03 VORD-TILASTN-SK      PIC S9(7)           COMP-3.                  
010300*                                 LASTNINGSDATUM SENASTE KOLLI            
010400     03 VORD-TIPACKN-SK      PIC S9(7)           COMP-3.                  
010500*                                 PACKNINGSDATUM SENASTE KOLLI            
010600*                                 PACKING DATE LATEST CASE                
010700     03 VORD-TIUTSTID        PIC S9(7)           COMP-3.                  
010800*                                 UTSKRIFTSTID (TTMMSS)                   
010900*                                 TIME OF PRINTING (HHMMSS)               
011000     03 VORD-TIUTSKR         PIC S9(7)           COMP-3.                  
011100*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
011200*                                 PRINTING DATE  (YYMMDD)                 
011300     03 VORD-VKORDBTO        PIC S9(6)V9(1)      COMP-3.                  
011400*                                 ORDERVIKT BRUTTO (KG)                   
011500*                                 GROSS WEIGHT (KG)                       
011600     03 VORD-VKORDBTO-FL     PIC S9(6)V9(1)      COMP-3.                  
011700*                                 ORDERVIKT BRUTTO FAKT/LASTAT            
011800     03 VORD-VKORDNTO        PIC S9(6)V9(1)      COMP-3.                  
011900*                                 ORDERVIKT NETTO (KG)                    
012000*                                 WEIGHT PER ORDER NETTO (KG)             
012100     03 VORD-VLORDBTO        PIC S9(4)V9(3)      COMP-3.                  
012200*                                 ORDERVOLYM BRUTTO (M3)                  
012300*                                 GROSS VOLUME PER ORDER (M3)             
012400     03 VORD-VLORDBTO-FL     PIC S9(4)V9(3)      COMP-3.                  
012500*                                 BRUTTOVOLYM ORDER FAKT/LASTAT           
012600     03 VORD-VLORDNTO        PIC S9(4)V9(3)      COMP-3.                  
012700*                                 ORDERVOLYM NETTO (M3)                   
012800*                                 NET VOLUME PER ORDER (M3)               
012900     03 VORD-FLAUTFAK        PIC X.                                       
013000*                                 AUTOMATFAKTURERING ?                    
013100*                                 AUTOMATIC INVOICING ?                   
013200     03 VORD-FLFRAKTS        PIC X.                                       
013300*                                 FRAKTSEDEL FLAGGA                       
013400*                                 FREIGHT BILL FLAG                       
013500     03 VORD-IDPRODNR-SAMP   PIC S9(7)           COMP-3.                  
013600*                                 PRODUKTIONSNUMMER SAMPACKNING           
013700     03 VORD-DARFS           PIC 9(12).                                   
013800*                                 KLART FÖR TRANSPORT                     
013900*                                 READY FOR SHIPMENT YYYYMMDDHHMM         
014000     03 VORD-IDLEVNR         PIC X(5).                                    
014100*                                 LEVERANTÖRNUMMER                        
014200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
014300     03 VORD-KDVIA           PIC X(2).                                    
014400*                                 KOD FöR LEVERANS VIA                    
014500*                                 CODE FOR DELIVERY VIA                   
014600     03 VORD-DEAL-PR-SUM.                                                 
014700*                                 DEALERPRIS (HUVUD)                      
014800        05 VORD-SUORDV-LOC   PIC S9(9)V9(2)      COMP-3.                  
014900*                                 ORDERVÄRDE SLUTKUNDPRIS                 
015000*                                 I LOKAL VALUTA                          
015100*                                 ORDER VALUE, CUSTOMER PRICE             
015200*                                 IN LOCAL CURRENCY                       
015300        05 VORD-SUORDV-LOCPREL                                            
015400                             PIC S9(9)V9(2)      COMP-3.                  
015500*                                 ORDERVÄRDE PREL SLUT-                   
015600*                                 KUNDPRIS, LOKAL VALUTA                  
015700*                                 ORDER VALUE, PREL CUSTOMER              
015800*                                 PRICE IN LOCAL CURRENCY                 
015900        05 VORD-KDVALISO     PIC X(3).                                    
016000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
016100*                                 CURRENCY CODE BY ISO-STANDARD.          
016200     03 VORD-SUORDV-PACK-LOC PIC S9(9)V9(2)      COMP-3.                  
016300*                                 SUMMA PACKAT ORDERVDE LOKAL VAL         
016400*                                 TOTAL PACKED ORDER VALUE LOC C          
016500     03 VORD-SUORDV-PACK-LOCPREL                                          
016600                             PIC S9(9)V9(2)      COMP-3.                  
016700*                                 SA PACKAT ORDERVDE LOK VAL PREL         
016800*                                 TOT PACKED ORDER VAL LOC C PREL         
016900     03 VORD-SUORDV-FL-LOC   PIC S9(9)V9(2)      COMP-3.                  
017000*                                 ORDERVÄRDE FAKTURERAT ELLER             
017100*                                 LASTAT, KUNDENS VALUTA                  
017200     03 VORD-SUORDV-FL-LOCPREL                                            
017300                             PIC S9(9)V9(2)      COMP-3.                  
017400*                                 ORDERVÄRDE FAKTURERAT ELLER             
017500*                                 LASTAT, KUNDENS VALUTA, PRELIMI         
017600*                                 NÄRT PRIS                               
017700     03 VORD-SUORDV-EXP      PIC S9(9)V9(2)      COMP-3.                  
017800*                                 SUMMA ORDERVÄRDE EXPORTFLÖDE            
017900*                                 TOTAL ORDER VALUE EXPORT FLOW           
018000     03 VORD-KDVALISO-EXP    PIC X(3).                                    
018100*                                 VALUTAKOD I EXP.FLÖDE(LOK. VAL)         
018200*                                 CURRENCY FOR EXPORT (LOC. CURR)         
018300     03 VORD-IDDC-EXP        PIC X(2).                                    
018400*                                 DC FÖR STUDS FLÖDE VID EXPORT           
018500*                                 DC FOR BOUNCE FLOW WHEN EXPORT          
018600*** END OF VILMAII-COPY LENGTH= 261 BYTES                                 
