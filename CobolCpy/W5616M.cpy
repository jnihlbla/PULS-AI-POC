000100 01  MAIL-W5616M.                                                         
000200*                                 FELSKICK I DISPLAYFORMAT                
000300     03 MAIL-IDPGM           PIC X(8).                                    
000400*                                 PROGRAM IDENTITET                       
000500*                                 PROGRAM INTENTITY                       
000600     03 MAIL-SEMICOLON-1     PIC X.                                       
000700     03 MAIL-TIREGDAT        PIC 9(6).                                    
000800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000900*                                 REGISTRATION DATE (YYMMDD)              
001000     03 MAIL-SEMICOLON-2     PIC X.                                       
001100     03 MAIL-TIKLOCK         PIC Z(7)9.                                   
001200*                                 KLOCKSLAG (TTMMSSTH)                    
001300*                                 TIME OF DAY (HHMMSSTH)                  
001400     03 MAIL-SEMICOLON-3     PIC X.                                       
001500     03 MAIL-IDSEKVNR        PIC 9(3).                                    
001600*                                 GENERELLT SEKVENSNUMMER                 
001700*                                 GENERAL SEQUENCE NUMBER                 
001800     03 MAIL-SEMICOLON-4     PIC X.                                       
001900     03 MAIL-IDCPYTXT.                                                    
002000*                                 COPYTEXT IDENTITET                      
002100*                                 IDENTITY OF A COPYTEXT                  
002200        05 MAIL-CT-IDSYSTEM  PIC X(4).                                    
002300*                                 VOLVO VCCS SYSTEMNUMMER                 
002400*                                 VOLVO VCCS SYSTEM NUMBER                
002500        05 MAIL-CT-IDPTYP    PIC X(3).                                    
002600*                                 POSTTYP                                 
002700*                                 RECORD TYPE                             
002800        05 MAIL-CT-IDVTYP    PIC X.                                       
002900*                                 POSTTYPSVERSION                         
003000*                                 RECORD TYPE VERSION                     
003100     03 MAIL-SEMICOLON-5     PIC X.                                       
003200     03 MAIL-BEVAT           PIC X(2).                                    
003300*                                 MOMSKODSBENÄMNING                       
003400*                                 VAT CODE DESCRIPTION                    
003500     03 MAIL-SEMICOLON-6     PIC X.                                       
003600     03 MAIL-DAVERDAT        PIC 9(8).                                    
003700*                                 VERIFIKATIONSDATUM (ÅÅÅÅMMDD)           
003800*                                 VERIFICATION DATE (YYYYMMDD)            
003900     03 MAIL-SEMICOLON-7     PIC X.                                       
004000     03 MAIL-FLLSBOK         PIC X.                                       
004100*                                 LAGERAVBOKNING                          
004200*                                 STOCKUPDATING                           
004300     03 MAIL-SEMICOLON-8     PIC X.                                       
004400     03 MAIL-IDANALYS        PIC X(12).                                   
004500*                                 ANALYSNUMMER                            
004600*                                 ANALYSIS NUMBER                         
004700     03 MAIL-SEMICOLON-9     PIC X.                                       
004800     03 MAIL-IDARTNR         PIC X(9).                                    
004900*                                 ARTIKELNUMMER                           
005000*                                 PART NUMBER                             
005100     03 MAIL-SEMICOLON-10    PIC X.                                       
005200     03 MAIL-IDDC-SEND       PIC X(2).                                    
005300*                                 SÄNDANDE LAGER                          
005400*                                 SENDING WAREHOUSE                       
005500     03 MAIL-SEMICOLON-11    PIC X.                                       
005600     03 MAIL-IDDC-REC        PIC X(2).                                    
005700*                                 MOTTAGANDE LAGER                        
005800*                                 RECEIVING WAREHOUSE                     
005900     03 MAIL-SEMICOLON-12    PIC X.                                       
006000     03 MAIL-IDDISTR         PIC Z(3)9.                                   
006100*                                 DISTRIKTNUMMER                          
006200*                                 DISTRICT NUMBER                         
006300     03 MAIL-SEMICOLON-13    PIC X.                                       
006400     03 MAIL-IDKONTO         PIC Z(9)9.                                   
006500*                                 KONTO                                   
006600*                                 ACCOUNT                                 
006700     03 MAIL-SEMICOLON-14    PIC X.                                       
006800     03 MAIL-IDKST           PIC X(10).                                   
006900*                                 KOSTNADSSTÄLLE                          
007000*                                 COST CENTRE                             
007100     03 MAIL-SEMICOLON-15    PIC X.                                       
007200     03 MAIL-IDKUNDNR        PIC Z(5)9.                                   
007300*                                 KUNDNUMMER                              
007400*                                 CUSTOMER NO                             
007500     03 MAIL-SEMICOLON-16    PIC X.                                       
007600     03 MAIL-IDTRANS         PIC X(4).                                    
007700*                                 BILDNUMMER                              
007800*                                 SCREEN NUMBER                           
007900     03 MAIL-SEMICOLON-17    PIC X.                                       
008000     03 MAIL-IDVERGL         PIC X(10).                                   
008100*                                 VERIFIKATIONSID FÖR HUVUDBOKEN          
008200*                                 VERIFICATION IDENTITY FOR THE           
008300*                                 GENERAL LEDGER                          
008400     03 MAIL-SEMICOLON-18    PIC X.                                       
008500     03 MAIL-KDANMORS        PIC X(2).                                    
008600*                                 ORSAK TILL LEVERANSANMÄRKNING           
008700*                                 DISCREPANCY REPORT REASON CODE          
008800     03 MAIL-SEMICOLON-19    PIC X.                                       
008900     03 MAIL-KDEKHHT         PIC X(3).                                    
009000*                                 EKONOMISK HUVUDHÄNDELSE                 
009100*                                 ECONOMIC MAIN EVENT                     
009200     03 MAIL-SEMICOLON-20    PIC X.                                       
009300     03 MAIL-KDEKSHT         PIC X(3).                                    
009400*                                 EKONOMISK SUBHÄNDELSE                   
009500*                                 ECONOMIC SUB EVENT                      
009600     03 MAIL-SEMICOLON-21    PIC X.                                       
009700     03 MAIL-KDEKNIVA        PIC X(5).                                    
009800*                                 EKONOMISK HÄNDELSENIVÅ                  
009900*                                 ECONOMICAL EVENT LEVEL                  
010000     03 MAIL-SEMICOLON-22    PIC X.                                       
010100     03 MAIL-KDFRAKT         PIC Z9.                                      
010200*                                 FRAKTSÄTT DC TILL KUND                  
010300*                                 FREIGHT CODE                            
010400     03 MAIL-SEMICOLON-23    PIC X.                                       
010500     03 MAIL-KDPRODSL        PIC Z9.                                      
010600*                                 PRODUKTSLAG                             
010700*                                 PRODUCT GROUP                           
010800     03 MAIL-SEMICOLON-24    PIC X.                                       
010900     03 MAIL-KDPSLLOC        PIC 9(2).                                    
011000*                                 PRODUKTSLAG LOKALT                      
011100*                                 PRODUCT GROUP LOCAL                     
011200     03 MAIL-SEMICOLON-25    PIC X.                                       
011300     03 MAIL-KDVALISO        PIC X(3).                                    
011400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
011500*                                 CURRENCY CODE BY ISO-STANDARD.          
011600     03 MAIL-SEMICOLON-26    PIC X.                                       
011700     03 MAIL-KVANTAL         PIC -(5)9.                                   
011800*                                 ANTAL                                   
011900*                                 NUMBER                                  
012000     03 MAIL-SEMICOLON-27    PIC X.                                       
012100     03 MAIL-PRARTNTO        PIC Z(6)9.9(2).                              
012200*                                 ARTIKELPRIS NETTO                       
012300*                                 NET PRICE EACH   (FOB NET)              
012400     03 MAIL-SEMICOLON-28    PIC X.                                       
012500     03 MAIL-PRARTSJK        PIC Z(6)9.9(2).                              
012600*                                 ARTIKELNS SJÄLVKOSTNAD                  
012700*                                 COST OF SALES                           
012800     03 MAIL-SEMICOLON-29    PIC X.                                       
012900     03 MAIL-PRARTSTD        PIC Z(6)9.9(2).                              
013000*                                 ARTIKELSTANDARDPRIS                     
013100*                                 STANDARD PRICE                          
013200     03 MAIL-SEMICOLON-30    PIC X.                                       
013300     03 MAIL-PRDIRLON        PIC Z(3)9.9(3).                              
013400*                                 DIREKT LÖN                              
013500*                                 SURCHARGE COSTS                         
013600     03 MAIL-SEMICOLON-31    PIC X.                                       
013700     03 MAIL-PRDMTRL         PIC Z(5)9.9(3).                              
013800*                                 DIREKT MATERIAL                         
013900*                                 SURCHARGE PACKING MATERIAL              
014000     03 MAIL-SEMICOLON-32    PIC X.                                       
014100     03 MAIL-PRINK           PIC Z(6)9.9(2).                              
014200*                                 INKÖPSPRIS                              
014300*                                 PURCHASE PRICE                          
014400     03 MAIL-SEMICOLON-33    PIC X.                                       
014500     03 MAIL-PRKURS          PIC Z(5)9.9(5).                              
014600*                                 VALUTAKURS                              
014700*                                 CURRENCY EXCHANGE RATE                  
014800     03 MAIL-SEMICOLON-34    PIC X.                                       
014900     03 MAIL-PRLANDCO        PIC Z(5)9.9(2).                              
015000*                                 LANDING COST                            
015100*                                 LANDING COST                            
015200     03 MAIL-SEMICOLON-35    PIC X.                                       
015300     03 MAIL-PROVRPAL        PIC Z(3)9.9(3).                              
015400*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
015500*                                 REMAINING OVERHEAD SURCHARGE            
015600     03 MAIL-SEMICOLON-36    PIC X.                                       
015700     03 MAIL-SUBEL           PIC Z(9).9(2).                               
015800*                                 SUMMABELOPP                             
015900*                                 SUM AMOUNT                              
016000     03 MAIL-SEMICOLON-37    PIC X.                                       
016100     03 MAIL-SUVAT           PIC Z(10)9.9(2).                             
016200*                                 MOMSVÄRDE PER MOMSKOD                   
016300*                                 VAT VALUE PER VAT CODE                  
016400     03 MAIL-SEMICOLON-38    PIC X.                                       
016500     03 MAIL-DAAVIDAT        PIC 9(8).                                    
016600*                                 AVISERINGSDATUM (YYYYMMDD)              
016700*                                 ADVICE NOTE DATE                        
016800     03 MAIL-SEMICOLON-39    PIC X.                                       
016900     03 MAIL-IDAVINR         PIC Z(6)9.                                   
017000*                                 AVI-NUMMER                              
017100*                                 ADVICE NOTE NUMBER                      
017200     03 MAIL-SEMICOLON-40    PIC X.                                       
017300     03 MAIL-IDLEVNR         PIC X(5).                                    
017400*                                 LEVERANTÖRNUMMER                        
017500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
017600     03 MAIL-SEMICOLON-41    PIC X.                                       
017700     03 MAIL-KDAVVTYP        PIC 9.                                       
017800*                                 AVVIKELSETYP                            
017900*                                 1=POSITIV.  2=NEGATIV                   
018000*                                 TYPE OF DISCREPANCY                     
018100*                                 1=POSITIVE. 2=NEGATIVE                  
018200     03 MAIL-SEMICOLON-42    PIC X.                                       
018300     03 MAIL-KDRT            PIC Z9.                                      
018400*                                 REDOVISNINGSTYP                         
018500*                                 TYPE OF ACCOUNTING                      
018600     03 MAIL-SEMICOLON-43    PIC X.                                       
018700     03 MAIL-KVANTMOT        PIC Z(5)9.                                   
018800*                                 ANTAL MOTTAGET                          
018900*                                 QUANTITY RECEIVED                       
019000     03 MAIL-SEMICOLON-44    PIC X.                                       
019100     03 MAIL-KVAVIS          PIC Z(5)9.                                   
019200*                                 AVISERAT ANTAL                          
019300*                                 QUANTITY NOTIFIED                       
019400     03 MAIL-SEMICOLON-45    PIC X.                                       
019500     03 MAIL-KDSORT          PIC X(2).                                    
019600*                                 SORT-KOD                                
019700*                                 UNIT OF MEASURE                         
019800     03 MAIL-SEMICOLON-46    PIC X.                                       
019900     03 MAIL-KDTRADP         PIC X(4).                                    
020000*                                 TRADING PARTNER                         
020100*                                 TRADING PARTNER                         
020200     03 MAIL-SEMICOLON-47    PIC X.                                       
020300     03 MAIL-FLOVRLEV        PIC X.                                       
020400*                                 ÖVERLEVERANS                            
020500*                                 OVER DELIVERY                           
020600     03 MAIL-SEMICOLON-48    PIC X.                                       
020700     03 MAIL-IDORDNR5        PIC X(5).                                    
020800*                                 ORDERNUMMER                             
020900*                                 ORDER NUMBER                            
021000     03 MAIL-SEMICOLON-49    PIC X.                                       
021100     03 MAIL-IDUSER          PIC X(8).                                    
021200*                                 ANVÄNDARENS SÄKERHETS ID                
021300*                                 USER SECURITY-IDENTITY                  
021400     03 MAIL-SEMICOLON-50    PIC X.                                       
021500     03 MAIL-IDREF           PIC X(15).                                   
021600*                                 REFERENS ID                             
021700*                                 REFERENCE ID                            
021800     03 MAIL-SEMICOLON-51    PIC X.                                       
021900     03 MAIL-BEFELSAP        PIC X(20).                                   
022000*                                 FELTEXT FÖR SAP-TRANSAKTIONER           
022100*                                 ERROR TEXT FOR SAP TRANSACTIONS         
022200     03 MAIL-SEMICOLON-52    PIC X.                                       
022300     03 MAIL-FLKLAR          PIC X.                                       
022400*                                 AVSLUTNINGSMARKERING                    
022500*                                 FINISHED FLAG                           
022600     03 MAIL-SEMICOLON-53    PIC X.                                       
022700     03 MAIL-PRHEMTAG        PIC Z(6)9.9(2).                              
022800*                                 HEMTAGNINGSKOSTNAD                      
022900*                                 TRANSPORT COST                          
023000     03 MAIL-SEMICOLON-54    PIC X.                                       
023100     03 MAIL-FLDCET          PIC X.                                       
023200*                                 DC 91 EXCHANGE TERMINAL                 
023300*                                 DC 91 EXCHANGE TERMINAL                 
023400     03 MAIL-SEMICOLON-55    PIC X.                                       
023500     03 MAIL-SPACE2          PIC X(3).                                    
023600     03 MAIL-SEMICOLON-56    PIC X.                                       
023700*** END OF VILMAII-COPY LENGTH= 415 BYTES                                 
