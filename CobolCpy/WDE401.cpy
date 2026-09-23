000100 01  KORD-WDE401.                                                         
000200*                                 KUNDORDERREGISTER                       
000300*                                 ORDERHUVUD (KUNDORDER)                  
000400*                                 FYSISK NYCKEL: WDE401KY                 
000500*                                 (IDGMTREF, IDPRODNR, IDPLKLST)          
000600     03 KORD-IDGMTREF.                                                    
000700*                                 GODSMOTTAGAREREFERENS                   
000800*                                 GOODS RECEIVER REFERENS                 
000900        05 KORD-IDDISTR      PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100*                                 DISTRICT NUMBER                         
001200        05 KORD-IDKUNDNR     PIC S9(7)           COMP-3.                  
001300*                                 KUNDNUMMER                              
001400*                                 CUSTOMER NO                             
001500        05 KORD-IDKUNDRF-GRP.                                             
001600*                                 KUNDENS REFERENS (ORDERID)              
001700*                                 CUSTOMER REFERENCE (ORDER ID)           
001800           07 KORD-IDKUNDRF  PIC X(10).                                   
001900*                                 KUNDENS REFERENS (ORDERID)              
002000*                                 CUSTOMER REFERENCE (ORDER ID)           
002100           07 KORD-IDORDNR5-FILLER REDEFINES KORD-IDKUNDRF.               
002200              09 KORD-IDORDNR5                                            
002300                             PIC 9(5).                                    
002400*                                 ORDERNUMMER                             
002500*                                 ORDER NUMBER                            
002600              09 FILLER      PIC X(5).                                    
002700           07 KORD-IDORDNR7-FILLER REDEFINES KORD-IDKUNDRF.               
002800              09 KORD-IDORDNR7                                            
002900                             PIC 9(7).                                    
003000*                                 ORDERNUMMER                             
003100*                                 ORDER NUMBER                            
003200              09 FILLER      PIC X(3).                                    
003300     03 KORD-IDPRODNR        PIC S9(7)           COMP-3.                  
003400*                                 PRODUKTIONSNUMMER                       
003500*                                 PRODUCTION NUMBER                       
003600     03 KORD-IDPLKLST        PIC S9(3)           COMP-3.                  
003700*                                 PLOCKLISTNUMMER                         
003800*                                 PICKING LIST NUMBER                     
003900     03 KORD-FLLSBOK         PIC X.                                       
004000*                                 LAGERAVBOKNING                          
004100*                                 STOCKUPDATING                           
004200     03 KORD-FLORDSPE        PIC X.                                       
004300*                                 SPECIALORDERFLAGGA                      
004400*                                 SPECIAL ORDER FLAG                      
004500     03 KORD-FLOVRLEV        PIC X.                                       
004600*                                 ÖVERLEVERANS                            
004700*                                 OVER DELIVERY                           
004800     03 KORD-FLPAFEL         PIC X.                                       
004900*                                 PACKNINGSKONTROLL FELFLAGGA             
005000*                                 PACKING CONTROL ERROR FLAG              
005100     03 KORD-IDARTNR-SATS    PIC S9(9)           COMP-3.                  
005200*                                 ARTIKELNUMMER FÖR SATS                  
005300*                                 KIT PART NUMBER                         
005400     03 KORD-IDORDER         PIC S9(7)           COMP-3.                  
005500*                                 VOLVO PARTS ORDERNUMMER                 
005600*                                 VOLVO PARTS ORDER NUMBER                
005700     03 KORD-IDUSER          PIC X(8).                                    
005800*                                 ANVÄNDARENS SÄKERHETS ID                
005900*                                 USER SECURITY-IDENTITY                  
006000     03 KORD-IDDC            PIC X(2).                                    
006100*                                 IDENTIFIERARE LAGER                     
006200*                                 WAREHOUSE IDENTIFIER                    
006300     03 KORD-KDFAKPAP        PIC S9              COMP-3.                  
006400*                                 INSTRUKTION OM FAKTURAUTSKRIFT          
006500     03 KORD-KDFAKTYP        PIC X.                                       
006600*                                 FAKTURATYP                              
006700*                                 INVOICE TYPE                            
006800     03 KORD-KDFDKRAV        PIC S9(3)           COMP-3.                  
006900*                                 TRANSPORTFÖRPACKNINGSKOD                
007000*                                 PACKING CODE                            
007100     03 KORD-KDFRAKT         PIC S9(3)           COMP-3.                  
007200*                                 FRAKTSÄTT DC TILL KUND                  
007300*                                 FREIGHT CODE                            
007400     03 KORD-KDORDKL         PIC S9              COMP-3.                  
007500*                                 ORDERKLASS                              
007600*                                 ORDER CLASS                             
007700     03 KORD-KDPAKOLL        PIC S9              COMP-3.                  
007800*                                 LÅSKOD VID PACKNINGSKONTROLL            
007900*                                 LOCK CODE FOR PACKING CONTROL           
008000     03 KORD-KDPERSON        PIC S9(3)           COMP-3.                  
008100*                                 PERSONKOD                               
008200*                                 STAFF CODE                              
008300     03 KORD-KDROPACK        PIC X.                                       
008400*                                 FRISLÄPPNINGSKOD RO/DO                  
008500*                                 CONSOLIDATION BO/DO                     
008600     03 KORD-KVBEART-SATS    PIC S9(7)           COMP-3.                  
008700*                                 BESTÄLLT ANTAL SATS ARTIKEL             
008800*                                 ORD. QUANT IN KIT PART                  
008900     03 KORD-KVORDRAD        PIC S9(5)           COMP-3.                  
009000*                                 ANTAL ORDERRADER                        
009100*                                 NUMBER OF ORDER LINES                   
009200     03 KORD-KVORDRAD-LEVPL  PIC S9(5)           COMP-3.                  
009300*                                 ANTAL ORDERRADER                        
009400*                                 LEVERANSPLATS                           
009500     03 KORD-KVORDRAD-PACK   PIC S9(5)           COMP-3.                  
009600*                                 ANTAL PACKADE ORDERRADER                
009700     03 KORD-KVORDRAD-VO     PIC S9(5)           COMP-3.                  
009800*                                 ANTAL ORDERRADER I VOLVOORDER           
009900     03 KORD-KVORDRAD-VO-LEVPL                                            
010000                             PIC S9(5)           COMP-3.                  
010100*                                 ANTAL ORDERRADER                        
010200*                                 LEVERANSPLATS                           
010300     03 KORD-SUORDV          PIC S9(9)V9(2)      COMP-3.                  
010400*                                 SUMMA ORDERVÄRDE                        
010500*                                 TOTAL ORDER VALUE                       
010600     03 KORD-SUORDV-LEVPL    PIC S9(9)V9(2)      COMP-3.                  
010700*                                 ORDERVÄRDE LEVPLATS                     
010800     03 KORD-TIBEGPAC        PIC S9(7)           COMP-3.                  
010900*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
011000*                                 REQUESTED PACKING DATE (YYMMDD)         
011100     03 KORD-TIUTSKR         PIC S9(7)           COMP-3.                  
011200*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
011300*                                 PRINTING DATE  (YYMMDD)                 
011400     03 KORD-TIORDREG        PIC S9(7)           COMP-3.                  
011500*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
011600*                                 ORDER REGISTRATION DATE  YYMMDD         
011700     03 KORD-VKORDNTO        PIC S9(6)V9(1)      COMP-3.                  
011800*                                 ORDERVIKT NETTO (KG)                    
011900*                                 WEIGHT PER ORDER NETTO (KG)             
012000     03 KORD-VLORDNTO        PIC S9(4)V9(3)      COMP-3.                  
012100*                                 ORDERVOLYM NETTO (M3)                   
012200*                                 NET VOLUME PER ORDER (M3)               
012300     03 KORD-DEAL-PR-SUM.                                                 
012400*                                 DEALERPRIS (HUVUD)                      
012500        05 KORD-SUORDV-LOC   PIC S9(9)V9(2)      COMP-3.                  
012600*                                 ORDERVÄRDE SLUTKUNDPRIS                 
012700*                                 I LOKAL VALUTA                          
012800*                                 ORDER VALUE, CUSTOMER PRICE             
012900*                                 IN LOCAL CURRENCY                       
013000        05 KORD-SUORDV-LOCPREL                                            
013100                             PIC S9(9)V9(2)      COMP-3.                  
013200*                                 ORDERVÄRDE PREL SLUT-                   
013300*                                 KUNDPRIS, LOKAL VALUTA                  
013400*                                 ORDER VALUE, PREL CUSTOMER              
013500*                                 PRICE IN LOCAL CURRENCY                 
013600        05 KORD-KDVALISO     PIC X(3).                                    
013700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
013800*                                 CURRENCY CODE BY ISO-STANDARD.          
013900     03 KORD-SUORDV-LEVPL-LOC                                             
014000                             PIC S9(9)V9(2)      COMP-3.                  
014100*                                 ORDERVÄRDE LEVPLATS, KUNDENS VA         
014200*                                 LUTA                                    
014300     03 KORD-SUORDV-LEVPL-LOCPREL                                         
014400                             PIC S9(9)V9(2)      COMP-3.                  
014500*                                 ORDERVÄRDE LEVPLATS                     
014600*                                 KUNDENS VALUTA, PREL PRIS               
014700     03 KORD-SUORDV-EXP      PIC S9(9)V9(2)      COMP-3.                  
014800*                                 SUMMA ORDERVÄRDE EXPORTFLÖDE            
014900*                                 TOTAL ORDER VALUE EXPORT FLOW           
015000     03 KORD-KDVALISO-EXP    PIC X(3).                                    
015100*                                 VALUTAKOD I EXP.FLÖDE(LOK. VAL)         
015200*                                 CURRENCY FOR EXPORT (LOC. CURR)         
015300*** END OF VILMAII-COPY LENGTH= 144 BYTES                                 
