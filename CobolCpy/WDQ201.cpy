000100 01  OHUV-WDQ201.                                                         
000200*                                 ORDERHUVUDSREGISTER KÖ                  
000300*                                 ROT SEGMENT                             
000400*                                 FYSISK NYCKEL: IDORDER                  
000500     03 OHUV-IDORDER         PIC S9(7)           COMP-3.                  
000600*                                 VOLVO PARTS ORDERNUMMER                 
000700*                                 VOLVO PARTS ORDER NUMBER                
000800     03 OHUV-ADBET.                                                       
000900*                                 BETALNINGSANSVARIG ADRESS               
001000*                                 ADDRESS OF PAYER                        
001100        05 OHUV-ADBETRAD-1   PIC X(35).                                   
001200*                                 ADRESSRAD BETALNINGSANSVARIG            
001300*                                 PART OF FINANCIAL CUSTOMER ADDR         
001400*                                 ESS                                     
001500        05 OHUV-ADBETRAD-2   PIC X(35).                                   
001600*                                 ADRESSRAD BETALNINGSANSVARIG            
001700*                                 PART OF FINANCIAL CUSTOMER ADDR         
001800*                                 ESS                                     
001900     03 OHUV-IDMAIL-FILLER REDEFINES OHUV-ADBET.                          
002000        05 OHUV-IDMAIL       PIC X(60).                                   
002100*                                 MAIL ADRESS                             
002200*                                 MAIL ADDRESS                            
002300        05 FILLER            PIC X(10).                                   
002400     03 OHUV-ADGMT.                                                       
002500*                                 GODSMOTTAGARADRESS                      
002600*                                 GOODS RECEIVER ADDRESS                  
002700        05 OHUV-ADGMT-GATA   PIC X(35).                                   
002800*                                 GODSMOTTAGARADRESS GATA                 
002900*                                 GOODS RECEIVER ADDRESS STREET           
003000        05 OHUV-ADGMT-PADR   PIC X(35).                                   
003100*                                 GODSMOTTAGARADRESS POSTADRESS           
003200*                                 GOODS RECEIVER ADDRESS TOWN             
003300        05 OHUV-ADPOST-PNRORT REDEFINES OHUV-ADGMT-PADR.                  
003400*                                 POSTNUMMER + ORT                        
003500*                                 POSTAL CODE + CITY                      
003600           07 OHUV-ADPOSTNR  PIC X(10).                                   
003700*                                 POSTNUMMER I ADRESS                     
003800*                                 POSTAL CODE IN ADDRESS                  
003900           07 OHUV-ADCITY    PIC X(25).                                   
004000*                                 BENÄMNING PÅ STAD                       
004100*                                 CITY                                    
004200        05 OHUV-ADPOST-ORTPNR REDEFINES OHUV-ADGMT-PADR.                  
004300*                                 ORT + POSTNUMMER                        
004400*                                 CITY + POSTAL CODE                      
004500           07 OHUV-ADCITY    PIC X(25).                                   
004600*                                 BENÄMNING PÅ STAD                       
004700*                                 CITY                                    
004800           07 OHUV-ADPOSTNR  PIC X(10).                                   
004900*                                 POSTNUMMER I ADRESS                     
005000*                                 POSTAL CODE IN ADDRESS                  
005100        05 OHUV-ADGMT-LAND   PIC X(35).                                   
005200*                                 GODSMOTTAGARADRESS LAND                 
005300*                                 GOODS RECEIVER ADDRESS COUNTRY          
005400     03 OHUV-BEBET.                                                       
005500*                                 BETALNINGSANSVARIG NAMN                 
005600*                                 NAME OF PAYER                           
005700        05 OHUV-BEBETRAD-1   PIC X(35).                                   
005800*                                 DEL AV BETALNINGSANSVARIGS NAMN         
005900*                                 PART OF FINANCIAL CUSTOMER NAME         
006000        05 OHUV-BEBETRAD-2   PIC X(35).                                   
006100*                                 DEL AV BETALNINGSANSVARIGS NAMN         
006200*                                 PART OF FINANCIAL CUSTOMER NAME         
006300        05 OHUV-BETELNR-FILLER REDEFINES OHUV-BEBETRAD-2.                 
006400           07 OHUV-BETELNR   PIC X(20).                                   
006500*                                 TELEFONNUMMER                           
006600*                                 TELEPHONE NUMBER                        
006700           07 FILLER         PIC X(15).                                   
006800     03 OHUV-BEGMT.                                                       
006900*                                 GODSMOTTAGARNAMN                        
007000*                                 GOODS RECEIVER NAME                     
007100        05 OHUV-BEGMT-RAD1   PIC X(35).                                   
007200*                                 GODSMOTTAGARNAMN RAD 1                  
007300*                                 GOODS RECEIVER NAME LINE 1              
007400        05 OHUV-BEGMT-RAD2   PIC X(35).                                   
007500*                                 GODSMOTTAGARNAMN RAD 2                  
007600*                                 GOODS RECEIVER NAME LINE 2              
007700     03 OHUV-BEKUNDRF        PIC X(15).                                   
007800*                                 KUNDENS REFERENS                        
007900*                                 CUSTOMERS REFERENCE                     
008000     03 OHUV-BELAGINS-GRP.                                                
008100*                                 LAGERINSTRUKTIONER                      
008200*                                 WAREHOUSE INSTRUCTIONS                  
008300        05 OHUV-BELAGINS-DEL1                                             
008400                             PIC X(60).                                   
008500*                                 DEL AV LAGERINSTRUKTION                 
008600*                                 PART OF WAREHOUSE INSTRUCTIONS          
008700        05 OHUV-BELAGINS-DEL2                                             
008800                             PIC X(60).                                   
008900*                                 DEL AV LAGERINSTRUKTION                 
009000*                                 PART OF WAREHOUSE INSTRUCTIONS          
009100     03 OHUV-BEVARREF        PIC X(10).                                   
009200*                                 VÅR REFERENS                            
009300*                                 OUR REFERENCE                           
009400     03 OHUV-FLAUTFAK        PIC X.                                       
009500*                                 AUTOMATFAKTURERING ?                    
009600*                                 AUTOMATIC INVOICING ?                   
009700     03 OHUV-FLAUTPAC        PIC X.                                       
009800*                                 AUTOMATISK PACKRAPPORTERING             
009900*                                 AUTOMATIC PACKREPORTING                 
010000     03 OHUV-FLBORT          PIC X.                                       
010100*                                 BORTTAGNINGSFLAGGA                      
010200     03 OHUV-FLEMBORD        PIC X.                                       
010300*                                 EMBALLAGEORDER ?                        
010400     03 OHUV-FLFORBI         PIC X.                                       
010500*                                 FÖRBIORDERFLAGGA                        
010600*                                                                         
010700     03 OHUV-FLKLAR          PIC X.                                       
010800*                                 AVSLUTNINGSMARKERING                    
010900*                                 FINISHED FLAG                           
011000     03 OHUV-FLLSBOK         PIC X.                                       
011100*                                 LAGERAVBOKNING                          
011200*                                 STOCKUPDATING                           
011300     03 OHUV-FLOBTRAN        PIC X.                                       
011400*                                 ORDERBEKRÄFTELSETRANSAKTION             
011500*                                 ORDERCONFIRMATIONTRANSACTION            
011600     03 OHUV-FLORDSPE        PIC X.                                       
011700*                                 SPECIALORDERFLAGGA                      
011800*                                 SPECIAL ORDER FLAG                      
011900     03 OHUV-FLOVRLEV        PIC X.                                       
012000*                                 ÖVERLEVERANS                            
012100*                                 OVER DELIVERY                           
012200     03 OHUV-FLPRELRO        PIC X.                                       
012300*                                 PRELIMINÄR RESTORDERFLAGGA              
012400*                                 PRELIMINAR BACK ORDER FLAG              
012500     03 OHUV-FLPRERS         PIC X.                                       
012600*                                 PRISERSÄTTNINGSFLAGGA                   
012700*                                 PRICE REPLACEMENT FLAG                  
012800     03 OHUV-FLRESTN         PIC X.                                       
012900*                                 RESTNOTERING ?                          
013000*                                 BACKORDERED ?                           
013100     03 OHUV-FLVORKO         PIC X.                                       
013200*                                 VOR-KÖ FLAGGA                           
013300*                                                                         
013400     03 OHUV-IDANALYS        PIC X(12).                                   
013500*                                 ANALYSNUMMER                            
013600*                                 ANALYSIS NUMBER                         
013700     03 OHUV-IDBIPREF        PIC X(7).                                    
013800*                                 BIPACKNINGSREFERENS                     
013900*                                                                         
014000     03 OHUV-IDDC-TVS        PIC X(2).                                    
014100*                                 DISTRIBUTIONCENTER                      
014200*                                 TVÅNGSSTYRNING                          
014300*                                 DISTRIBUTION CENTER STEERING            
014400     03 OHUV-IDDEPOT         PIC X(2).                                    
014500*                                 TRANSPORT DEPOT                         
014600*                                 TRANSPORT DEPOT                         
014700     03 OHUV-IDFTG           PIC 9(2).                                    
014800*                                 FÖRETAGSID EKONOM REDOVISNING           
014900*                                 COMPANY IDENTITY ACCOUNTING             
015000     03 OHUV-IDGMTREF.                                                    
015100*                                 GODSMOTTAGAREREFERENS                   
015200*                                 GOODS RECEIVER REFERENS                 
015300        05 OHUV-IDDISTR      PIC S9(5)           COMP-3.                  
015400*                                 DISTRIKTNUMMER                          
015500*                                 DISTRICT NUMBER                         
015600        05 OHUV-IDKUNDNR     PIC S9(7)           COMP-3.                  
015700*                                 KUNDNUMMER                              
015800*                                 CUSTOMER NO                             
015900        05 OHUV-IDKUNDRF-GRP.                                             
016000*                                 KUNDENS REFERENS (ORDERID)              
016100*                                 CUSTOMER REFERENCE (ORDER ID)           
016200           07 OHUV-IDKUNDRF  PIC X(10).                                   
016300*                                 KUNDENS REFERENS (ORDERID)              
016400*                                 CUSTOMER REFERENCE (ORDER ID)           
016500           07 OHUV-IDORDNR5-FILLER REDEFINES OHUV-IDKUNDRF.               
016600              09 OHUV-IDORDNR5                                            
016700                             PIC 9(5).                                    
016800*                                 ORDERNUMMER                             
016900*                                 ORDER NUMBER                            
017000              09 FILLER      PIC X(5).                                    
017100           07 OHUV-IDORDNR7-FILLER REDEFINES OHUV-IDKUNDRF.               
017200              09 OHUV-IDORDNR7                                            
017300                             PIC 9(7).                                    
017400*                                 ORDERNUMMER                             
017500*                                 ORDER NUMBER                            
017600              09 FILLER      PIC X(3).                                    
017700     03 OHUV-IDKAMPRF        PIC S9(7)           COMP-3.                  
017800*                                 KAMPANJREFERENS                         
017900*                                 CAMPAIGN REFERENCE                      
018000     03 OHUV-IDKONTO         PIC S9(11)          COMP-3.                  
018100*                                 KONTO                                   
018200*                                 ACCOUNT                                 
018300     03 OHUV-IDKST           PIC X(10).                                   
018400*                                 KOSTNADSSTÄLLE                          
018500*                                 COST CENTRE                             
018600     03 OHUV-IDRFTAB         PIC X(3).                                    
018700*                                 RANSONERINGSFAKTORTABELL                
018800*                                 TABLE WITH RATIONING FACTORS            
018900     03 OHUV-IDROUTE         PIC X.                                       
019000*                                 TRANSPORT ROUTE                         
019100*                                 TRANSPORT ROUTE                         
019200     03 OHUV-IDSKYLT         PIC X(3).                                    
019300*                                 NATIONALITETSTECKEN                     
019400*                                 SPRÅKIDENTIFIKATION                     
019500*                                 NATIONALITY SIGN                        
019600*                                 LANGUAGE IDENTIFIER                     
019700     03 OHUV-IDSYSTEM        PIC X(4).                                    
019800*                                 VOLVO VCCS SYSTEMNUMMER                 
019900*                                 VOLVO VCCS SYSTEM NUMBER                
020000     03 OHUV-IDUSER          PIC X(8).                                    
020100*                                 ANVÄNDARENS SÄKERHETS ID                
020200*                                 USER SECURITY-IDENTITY                  
020300     03 OHUV-IDZON           PIC X(2).                                    
020400*                                 TRANSPORTVÄG (RUTT,ZON)                 
020500*                                 TRANSPORT ROUTE (ZONE)                  
020600     03 OHUV-KDFAKTYP        PIC X.                                       
020700*                                 FAKTURATYP                              
020800*                                 INVOICE TYPE                            
020900     03 OHUV-KDORDING        PIC S9              COMP-3.                  
021000*                                 UPPDATERING ORDERINGÅNG                 
021100*                                 ORDER STATISTICS                        
021200     03 OHUV-KDORDKL         PIC S9              COMP-3.                  
021300*                                 ORDERKLASS                              
021400*                                 ORDER CLASS                             
021500     03 OHUV-KDTPOTYP        PIC S9              COMP-3.                  
021600*                                 TYP AV TIDPLANERAD ORDER                
021700*                                 TYPE OF TIME PLANNED ORDER              
021800     03 OHUV-KDTULLVE        PIC S9              COMP-3.                  
021900*                                 TYP AV PRIS PÅ TULLFAKTURA              
022000*                                 TYPE OF PRICE ON CUSTOMS INVOIC         
022100*                                 E                                       
022200     03 OHUV-KDVRINFO        PIC S9              COMP-3.                  
022300*                                 PÅVERKAN I VR/DSP SYSTEM                
022400*                                 VR/DSP UP-DATE                          
022500     03 OHUV-KVDAGAR-DOW     PIC S9(3)           COMP-3.                  
022600*                                 ANTAL DAGAR FÖRE DC CLEARING            
022700*                                 NUMBER OF DAYS REFERRAL                 
022800     03 OHUV-RESLATT         PIC S9(3)           COMP-3.                  
022900*                                 SLATTGRÄNS                              
023000*                                 DROP CODE                               
023100     03 OHUV-TIREGDAT        PIC S9(7)           COMP-3.                  
023200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
023300*                                 REGISTRATION DATE (YYMMDD)              
023400     03 OHUV-TIREGTID        PIC S9(7)           COMP-3.                  
023500*                                 REGISTRERINGSTID                        
023600*                                 GENERAL REGISTRATION TIME               
023700     03 OHUV-TIREGDAT-STO    PIC S9(7)           COMP-3.                  
023800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
023900*                                 REGISTRATION DATE (YYMMDD)              
024000     03 OHUV-TIREGTID-STO    PIC S9(7)           COMP-3.                  
024100*                                 REGISTRERINGSTID                        
024200*                                 GENERAL REGISTRATION TIME               
024300     03 OHUV-TITPO           PIC S9(7)           COMP-3.                  
024400*                                 PLANERAD ORDERDATUM                     
024500*                                 PLANNED ORDER DATE                      
024600     03 OHUV-IDDEPT          PIC 9(2).                                    
024700*                                 AVDELNING I VERKSTAD                    
024800*                                 DEPARTMENT IN GARRAGE                   
024900     03 OHUV-KDORDTYP-LDC    PIC X(2).                                    
025000*                                 ORDERTYP HOS DEALER                     
025100     03 OHUV-TIREPDAT        PIC S9(7)           COMP-3.                  
025200*                                 REPAIR DATE                             
025300*                                 REPAIR DATE                             
025400     03 OHUV-TIREGDAT-9KOMPL PIC S9(7)           COMP-3.                  
025500*                                 DATUMETS 9-KOMPLEMENT                   
025600*                                 DATES 9-COMPLEMENT                      
025700     03 OHUV-FLORDTIL        PIC X.                                       
025800*                                 TVINGANDE TILÄGG ORDER                  
025900*                                 MANDATORY ORDER ADDITION                
026000     03 OHUV-IDDC-PRIM       PIC X(2).                                    
026100*                                 PRIMÄRT LEVERERANDE LAGER               
026200*                                 PRIMARY DELIVERING DC                   
026300     03 OHUV-KVORDTIL        PIC S9(3)           COMP-3.                  
026400*                                 ANTAL HOPSLAGNA ORDER                   
026500*                                 QUANTITY OF COSOLIDATED ORDERS          
026600     03 OHUV-IDLEVNR-EJLS    PIC X(5).                                    
026700*                                 LEVERANTÖR SPEC. ORDER EJ LS            
026800*                                 SUPPLIER FOR SPEC. ORDER NO LS          
026900     03 OHUV-FLSOFT          PIC X.                                       
027000*                                 FLAGGA SOFTVARA                         
027100*                                 SOFTWARE MARK                           
027200     03 OHUV-FLVORFK         PIC X.                                       
027300*                                 VOR-FRAKTKOD FRÅN KLASS 1               
027400*                                 VOR-FREIGHTCODE FROM CLASS 1            
027500     03 OHUV-IDBILREG        PIC X(10).                                   
027600*                                 BILENS REGISTRERINGSNUMMER              
027700*                                 CAR REGISTRATION NUMBER                 
027800     03 OHUV-IDGROSS         PIC 9(3).                                    
027900*                                 GROSSIST KUNDNUMMER FRÅN TACDIS         
028000*                                 BRANCH NUMBER FROM TACDIS               
028100     03 OHUV-IDCISNR         PIC X(12).                                   
028200*                                 CIS NUMMER                              
028300*                                 CIS NUMBER                              
028400     03 OHUV-IDVIN           PIC X(17).                                   
028500*                                 VIN ID FORDON                           
028600*                                 VEHICLE VIN ID                          
028700*** END OF VILMAII-COPY LENGTH= 657 BYTES                                 
