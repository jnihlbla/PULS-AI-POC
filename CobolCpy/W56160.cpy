000100 01  EKHT-W56160.                                                         
000200*                                 LOGGDATA TILL SAP US                    
000300     03 EKHT-IDPGM           PIC X(8).                                    
000400*                                 PROGRAM IDENTITET                       
000500*                                 PROGRAM INTENTITY                       
000600     03 EKHT-TIREGDAT        PIC S9(7)           COMP-3.                  
000700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000800*                                 REGISTRATION DATE (YYMMDD)              
000900     03 EKHT-TIKLOCK         PIC S9(9)           COMP-3.                  
001000*                                 KLOCKSLAG (TTMMSSTH)                    
001100*                                 TIME OF DAY (HHMMSSTH)                  
001200     03 EKHT-IDSEKVNR        PIC S9(3)           COMP-3.                  
001300*                                 GENERELLT SEKVENSNUMMER                 
001400*                                 GENERAL SEQUENCE NUMBER                 
001500     03 EKHT-IDCPYTXT.                                                    
001600*                                 COPYTEXT IDENTITET                      
001700*                                 IDENTITY OF A COPYTEXT                  
001800        05 EKHT-CT-IDSYSTEM  PIC X(4).                                    
001900*                                 VOLVO VCCS SYSTEMNUMMER                 
002000*                                 VOLVO VCCS SYSTEM NUMBER                
002100        05 EKHT-CT-IDPTYP    PIC X(3).                                    
002200*                                 POSTTYP                                 
002300*                                 RECORD TYPE                             
002400        05 EKHT-CT-IDVTYP    PIC X.                                       
002500*                                 POSTTYPSVERSION                         
002600*                                 RECORD TYPE VERSION                     
002700     03 EKHT-BEVAT           PIC X(2).                                    
002800*                                 MOMSKODSBENÄMNING                       
002900*                                 VAT CODE DESCRIPTION                    
003000     03 EKHT-DAVERDAT        PIC 9(8).                                    
003100*                                 VERIFIKATIONSDATUM (ÅÅÅÅMMDD)           
003200*                                 VERIFICATION DATE (YYYYMMDD)            
003300     03 EKHT-FLLSBOK         PIC X.                                       
003400*                                 LAGERAVBOKNING                          
003500*                                 STOCKUPDATING                           
003600     03 EKHT-IDANALYS        PIC X(12).                                   
003700*                                 ANALYSNUMMER                            
003800*                                 ANALYSIS NUMBER                         
003900     03 EKHT-IDARTNR         PIC S9(9)           COMP-3.                  
004000*                                 ARTIKELNUMMER                           
004100*                                 PART NUMBER                             
004200     03 EKHT-IDDC-SEND       PIC X(2).                                    
004300*                                 SÄNDANDE LAGER                          
004400*                                 SENDING WAREHOUSE                       
004500     03 EKHT-IDDC-REC        PIC X(2).                                    
004600*                                 MOTTAGANDE LAGER                        
004700*                                 RECEIVING WAREHOUSE                     
004800     03 EKHT-IDDISTR         PIC S9(5)           COMP-3.                  
004900*                                 DISTRIKTNUMMER                          
005000*                                 DISTRICT NUMBER                         
005100     03 EKHT-IDKONTO         PIC S9(11)          COMP-3.                  
005200*                                 KONTO                                   
005300*                                 ACCOUNT                                 
005400     03 EKHT-IDKST           PIC X(10).                                   
005500*                                 KOSTNADSSTÄLLE                          
005600*                                 COST CENTRE                             
005700     03 EKHT-IDKUNDNR        PIC S9(7)           COMP-3.                  
005800*                                 KUNDNUMMER                              
005900*                                 CUSTOMER NO                             
006000     03 EKHT-IDTRANS         PIC X(4).                                    
006100*                                 BILDNUMMER                              
006200*                                 SCREEN NUMBER                           
006300     03 EKHT-IDVERGL         PIC X(10).                                   
006400*                                 VERIFIKATIONSID FÖR HUVUDBOKEN          
006500*                                 VERIFICATION IDENTITY FOR THE           
006600*                                 GENERAL LEDGER                          
006700     03 EKHT-KDANMORS        PIC X(2).                                    
006800*                                 ORSAK TILL LEVERANSANMÄRKNING           
006900*                                 DISCREPANCY REPORT REASON CODE          
007000     03 EKHT-KDEKHHT         PIC X(3).                                    
007100*                                 EKONOMISK HUVUDHÄNDELSE                 
007200*                                 ECONOMIC MAIN EVENT                     
007300     03 EKHT-KDEKSHT         PIC X(3).                                    
007400*                                 EKONOMISK SUBHÄNDELSE                   
007500*                                 ECONOMIC SUB EVENT                      
007600     03 EKHT-KDEKNIVA        PIC X(5).                                    
007700*                                 EKONOMISK HÄNDELSENIVÅ                  
007800*                                 ECONOMICAL EVENT LEVEL                  
007900     03 EKHT-KDFRAKT         PIC S9(3)           COMP-3.                  
008000*                                 FRAKTSÄTT DC TILL KUND                  
008100*                                 FREIGHT CODE                            
008200     03 EKHT-KDPRODSL        PIC S9(3)           COMP-3.                  
008300*                                 PRODUKTSLAG                             
008400*                                 PRODUCT GROUP                           
008500     03 EKHT-KDPSLLOC        PIC 9(2).                                    
008600*                                 PRODUKTSLAG LOKALT                      
008700*                                 PRODUCT GROUP LOCAL                     
008800     03 EKHT-KDVALISO        PIC X(3).                                    
008900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
009000*                                 CURRENCY CODE BY ISO-STANDARD.          
009100     03 EKHT-KVANTAL         PIC S9(7)           COMP-3.                  
009200*                                 ANTAL                                   
009300*                                 NUMBER                                  
009400     03 EKHT-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
009500*                                 ARTIKELPRIS NETTO                       
009600*                                 NET PRICE EACH   (FOB NET)              
009700     03 EKHT-PRARTSJK        PIC S9(7)V9(2)      COMP-3.                  
009800*                                 ARTIKELNS SJÄLVKOSTNAD                  
009900*                                 COST OF SALES                           
010000     03 EKHT-PRARTSTD        PIC S9(7)V9(2)      COMP-3.                  
010100*                                 ARTIKELSTANDARDPRIS                     
010200*                                 STANDARD PRICE                          
010300     03 EKHT-PRDIRLON        PIC S9(4)V9(3)      COMP-3.                  
010400*                                 DIREKT LÖN                              
010500*                                 SURCHARGE COSTS                         
010600     03 EKHT-PRDMTRL         PIC S9(6)V9(3)      COMP-3.                  
010700*                                 DIREKT MATERIAL                         
010800*                                 SURCHARGE PACKING MATERIAL              
010900     03 EKHT-PRINK           PIC S9(7)V9(2)      COMP-3.                  
011000*                                 INKÖPSPRIS                              
011100*                                 PURCHASE PRICE                          
011200     03 EKHT-PRKURS          PIC S9(6)V9(5)      COMP-3.                  
011300*                                 VALUTAKURS                              
011400*                                 CURRENCY EXCHANGE RATE                  
011500     03 EKHT-PRLANDCO        PIC S9(7)V9(2)      COMP-3.                  
011600*                                 LANDING COST                            
011700*                                 LANDING COST                            
011800     03 EKHT-PROVRPAL        PIC S9(4)V9(3)      COMP-3.                  
011900*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
012000*                                 REMAINING OVERHEAD SURCHARGE            
012100     03 EKHT-SUBEL           PIC S9(9)V9(2).                              
012200*                                 SUMMABELOPP                             
012300*                                 SUM AMOUNT                              
012400     03 EKHT-SUVAT           PIC S9(11)V9(2)     COMP-3.                  
012500*                                 MOMSVÄRDE PER MOMSKOD                   
012600*                                 VAT VALUE PER VAT CODE                  
012700     03 EKHT-DAAVIDAT        PIC 9(8).                                    
012800*                                 AVISERINGSDATUM (YYYYMMDD)              
012900*                                 ADVICE NOTE DATE                        
013000     03 EKHT-IDAVINR         PIC S9(7)           COMP-3.                  
013100*                                 AVI-NUMMER                              
013200*                                 ADVICE NOTE NUMBER                      
013300     03 EKHT-IDLEVNR         PIC X(5).                                    
013400*                                 LEVERANTÖRNUMMER                        
013500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
013600     03 EKHT-KDAVVTYP        PIC S9              COMP-3.                  
013700*                                 AVVIKELSETYP                            
013800*                                 1=POSITIV.  2=NEGATIV                   
013900*                                 TYPE OF DISCREPANCY                     
014000*                                 1=POSITIVE. 2=NEGATIVE                  
014100     03 EKHT-KDRT            PIC S9(3)           COMP-3.                  
014200*                                 REDOVISNINGSTYP                         
014300*                                 TYPE OF ACCOUNTING                      
014400     03 EKHT-KVANTMOT        PIC S9(7)           COMP-3.                  
014500*                                 ANTAL MOTTAGET                          
014600*                                 QUANTITY RECEIVED                       
014700     03 EKHT-KVAVIS          PIC S9(7)           COMP-3.                  
014800*                                 AVISERAT ANTAL                          
014900*                                 QUANTITY NOTIFIED                       
015000     03 EKHT-KDSORT          PIC X(2).                                    
015100*                                 SORT-KOD                                
015200*                                 UNIT OF MEASURE                         
015300     03 EKHT-KDTRADP         PIC X(4).                                    
015400*                                 TRADING PARTNER                         
015500*                                 TRADING PARTNER                         
015600     03 EKHT-FLOVRLEV        PIC X.                                       
015700*                                 ÖVERLEVERANS                            
015800*                                 OVER DELIVERY                           
015900     03 EKHT-IDORDNR5        PIC S9(5)           COMP-3.                  
016000*                                 ORDERNUMMER                             
016100*                                 ORDER NUMBER                            
016200     03 EKHT-IDUSER          PIC X(8).                                    
016300*                                 ANVÄNDARENS SÄKERHETS ID                
016400*                                 USER SECURITY-IDENTITY                  
016500     03 EKHT-PRHEMTAG        PIC S9(7)V9(2)      COMP-3.                  
016600*                                 HEMTAGNINGSKOSTNAD                      
016700*                                 TRANSPORT COST                          
016800     03 EKHT-FLDCET          PIC X.                                       
016900*                                 DC 91 EXCHANGE TERMINAL                 
017000*                                 DC 91 EXCHANGE TERMINAL                 
017100     03 EKHT-IDKUNDRF        PIC X(10).                                   
017200*                                 KUNDENS REFERENS (ORDERID)              
017300*                                 CUSTOMER REFERENCE (ORDER ID)           
017400     03 EKHT-IDFAKT-EXP      PIC X(7).                                    
017500*                                 FAKTNR NR.1 I EXPORTFLÖDET              
017600*                                 INVOICE NO 1 IN EXPORT FLOW             
017700     03 FILLER               PIC X(12).                                   
017800*** END OF VILMAII-COPY LENGTH= 265 BYTES                                 
