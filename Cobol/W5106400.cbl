000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5106400.                                                
000300 AUTHOR.         JONNY SANDSTEN.                                          
000400 DATE-WRITTEN.   98/05/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET LÄSER IN UPPDATERINGS-POSTER FRÅN FIL W51062          
001000*        DESSA POSTER LÄGGS SEDAN UPP PÅ DATABASEN WDR9                   
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WLSAPA (WDR9)                              
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- INFIL FRÅN PGM W5106300                                    
002200     SELECT W51062                     ASSIGN TO W51064D1.                
002300     SKIP2                                                                
002400     SELECT W5106M                     ASSIGN TO W51064D2.                
002500     SKIP2                                                                
002510     SELECT W5106A                     ASSIGN TO W51064D3.                
002520     SKIP2                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W51062                                                               
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300*01  -COPY WDR901      -L.                                                
003400     SKIP3                                                                
003500 FD  W5106M                                                               
003600     RECORDING       V                                                    
003700     BLOCK CONTAINS  0.                                                   
003800 01  W5106M-001              PIC X(999).                                  
003900     SKIP3                                                                
003910 FD  W5106A                                                               
003920     RECORDING       F                                                    
003930     BLOCK CONTAINS  0.                                                   
003940*01  -COPY WDR901      -L.                                                
003950     SKIP3                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'W5106400'.            
004300 01  CHKP-VAR.                                                            
004400     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004500     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004600     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004700     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004800     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004900     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200     SKIP2                                                                
005300 01  FELTEXT.                                                             
005400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005600                                                                          
005700 77  W51062-EOF-SW               PIC X       VALUE 'N'.                   
005800     88  END-OF-W51062                       VALUE 'J'.                   
005900                                                                          
005901 77  W5106A-EOF-SW               PIC X       VALUE 'N'.                   
005902     88  END-OF-W5106A                       VALUE 'J'.                   
005903                                                                          
005910 77  HEADER-SW                   PIC X       VALUE 'N'.                   
005920     88  HEADER-DONE                         VALUE 'J'.                   
005930     88  HEADER-NOT-DONE                     VALUE 'N'.                   
005940                                                                          
006000     EJECT                                                                
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006500     EJECT                                                                
006600*    --- PARAMETRAR TILL POSTSUM                                          
006700*                                                                         
006800*01  -COPY W0005   -PRE  POSTSUM-                                         
006900     EJECT                                                                
007000 01  IN1-AREA-START              PIC X(24)   VALUE                        
007100                                             'IN1-AREA-START'.            
007200*01  AREA -COPY WDR901     -PRE IN1-                                      
007300*    05   -COPY W510EKHA   -PRE IN1- -RED IN1-FIL-WDR901-DATA             
007400     EJECT                                                                
007500                                                                          
007510 01  IN2-AREA-START              PIC X(24)   VALUE                        
007520                                             'IN2-AREA-START'.            
007530*01  AREA -COPY WDR901     -PRE IN2-                                      
007540*    05   -COPY W510EKHA   -PRE IN2- -RED IN2-FIL-WDR901-DATA             
007550     EJECT                                                                
007560                                                                          
007600 01  TEXT-AREA.                                                           
007700     03  HEAD-LINE.                                                       
007800         05  FILLER          PIC X(7)   VALUE                             
007900            'PROGRAM'.                                                    
008000         05  FILLER          PIC X(1)    VALUE ';'.                       
008100         05  FILLER          PIC X(13)   VALUE                            
008200            'REGISTER DATE'.                                              
008300         05  FILLER          PIC X(1)    VALUE ';'.                       
008400         05  FILLER          PIC X(13)   VALUE                            
008500            'REGISTER TIME'.                                              
008600         05  FILLER          PIC X(1)    VALUE ';'.                       
008700         05  FILLER          PIC X(15)   VALUE                            
008800            'SEQUENCE NUMBER'.                                            
008900         05  FILLER          PIC X(1)    VALUE ';'.                       
009000         05  FILLER          PIC X(17)   VALUE                            
009100            'IDENTITY COPYTEXT'.                                          
009200         05  FILLER          PIC X(1)    VALUE ';'.                       
009300         05  FILLER          PIC X(8)   VALUE                             
009400            'VAT CODE'.                                                   
009500         05  FILLER          PIC X(1)    VALUE ';'.                       
009600         05  FILLER          PIC X(17)   VALUE                            
009700            'VERIFICATION DATE'.                                          
009800         05  FILLER          PIC X(1)    VALUE ';'.                       
009900         05  FILLER          PIC X(13)  VALUE                             
010000            'STOCKUPDATING'.                                              
010100         05  FILLER          PIC X(1)    VALUE ';'.                       
010200         05  FILLER          PIC X(15)  VALUE                             
010300            'ANALYSIS NUMBER'.                                            
010400         05  FILLER          PIC X(1)    VALUE ';'.                       
010500         05  FILLER          PIC X(11)  VALUE                             
010600            'PART NUMBER'.                                                
010700         05  FILLER          PIC X(1)    VALUE ';'.                       
010800         05  FILLER          PIC X(17)  VALUE                             
010900            'SENDING WAREHOUSE'.                                          
011000         05  FILLER          PIC X(1)    VALUE ';'.                       
011100         05  FILLER          PIC X(19)  VALUE                             
011200            'RECEIVING WAREHOUSE'.                                        
011300         05  FILLER          PIC X(1)    VALUE ';'.                       
011400         05  FILLER          PIC X(15)  VALUE                             
011500            'DISTRICT NUMBER'.                                            
011600         05  FILLER          PIC X(1)    VALUE ';'.                       
011700         05  FILLER          PIC X(7)   VALUE                             
011800            'ACCOUNT'.                                                    
011900         05  FILLER          PIC X(1)    VALUE ';'.                       
012000         05  FILLER          PIC X(11)  VALUE                             
012100            'COST CENTRE'.                                                
012200         05  FILLER          PIC X(1)    VALUE ';'.                       
012300         05  FILLER          PIC X(11)  VALUE                             
012400            'CUSTOMER NO'.                                                
012500         05  FILLER          PIC X(1)    VALUE ';'.                       
012600         05  FILLER          PIC X(13)  VALUE                             
012700            'SCREEN NUMBER'.                                              
012800         05  FILLER          PIC X(1)    VALUE ';'.                       
012900         05  FILLER          PIC X(19)  VALUE                             
013000            'VERIFICATION NUMBER'.                                        
013100         05  FILLER          PIC X(1)    VALUE ';'.                       
013200         05  FILLER          PIC X(23)  VALUE                             
013300            'DISCREPANCY REASON CODE'.                                    
013400         05  FILLER          PIC X(1)    VALUE ';'.                       
013500         05  FILLER          PIC X(10)  VALUE                             
013600            'MAIN EVENT'.                                                 
013700         05  FILLER          PIC X(1)    VALUE ';'.                       
013800         05  FILLER          PIC X(9)   VALUE                             
013900            'SUB EVENT'.                                                  
014000         05  FILLER          PIC X(1)    VALUE ';'.                       
014100         05  FILLER          PIC X(11)  VALUE                             
014200            'EVENT LEVEL'.                                                
014300         05  FILLER          PIC X(1)    VALUE ';'.                       
014400         05  FILLER          PIC X(12)  VALUE                             
014500            'FREIGHT CODE'.                                               
014600         05  FILLER          PIC X(1)    VALUE ';'.                       
014700         05  FILLER          PIC X(13)  VALUE                             
014800            'PRODUCT GROUP'.                                              
014900         05  FILLER          PIC X(1)    VALUE ';'.                       
015000         05  FILLER          PIC X(19)  VALUE                             
015100            'PRODUCT GROUP LOCAL'.                                        
015200         05  FILLER          PIC X(1)    VALUE ';'.                       
015300         05  FILLER          PIC X(13)  VALUE                             
015400            'CURRENCY CODE'.                                              
015500         05  FILLER          PIC X(1)    VALUE ';'.                       
015600         05  FILLER          PIC X(15)  VALUE                             
015700            'QUANTITY NUMBER'.                                            
015800         05  FILLER          PIC X(1)    VALUE ';'.                       
015900         05  FILLER          PIC X(14)  VALUE                             
016000            'NET PRICE EACH'.                                             
016100         05  FILLER          PIC X(1)    VALUE ';'.                       
016200         05  FILLER          PIC X(13)  VALUE                             
016300            'COST OF SALES'.                                              
016400         05  FILLER          PIC X(1)    VALUE ';'.                       
016500         05  FILLER          PIC X(14)  VALUE                             
016600            'STANDARD PRICE'.                                             
016700         05  FILLER          PIC X(1)    VALUE ';'.                       
016800         05  FILLER          PIC X(15)  VALUE                             
016900            'SURCHARGE COSTS'.                                            
017000         05  FILLER          PIC X(1)    VALUE ';'.                       
017100         05  FILLER          PIC X(17)  VALUE                             
017200            'SURCHARGE PACKING'.                                          
017300         05  FILLER          PIC X(1)    VALUE ';'.                       
017400         05  FILLER          PIC X(14)  VALUE                             
017500            'PURCHASE PRICE'.                                             
017600         05  FILLER          PIC X(1)    VALUE ';'.                       
017700         05  FILLER          PIC X(22)  VALUE                             
017800            'CURRENCY EXCHANGE RATE'.                                     
017900         05  FILLER          PIC X(1)    VALUE ';'.                       
018000         05  FILLER          PIC X(12)  VALUE                             
018100            'LANDING COST'.                                               
018200         05  FILLER          PIC X(1)    VALUE ';'.                       
018300         05  FILLER          PIC X(18)  VALUE                             
018400            'OVERHEAD SURCHARGE'.                                         
018500         05  FILLER          PIC X(1)    VALUE ';'.                       
018600         05  FILLER          PIC X(10)  VALUE                             
018700            'SUM AMOUNT'.                                                 
018800         05  FILLER          PIC X(1)    VALUE ';'.                       
018900         05  FILLER          PIC X(9)   VALUE                             
019000            'VAT VALUE'.                                                  
019100         05  FILLER          PIC X(1)    VALUE ';'.                       
019200         05  FILLER          PIC X(16)  VALUE                             
019300            'ADVICE NOTE DATE'.                                           
019400         05  FILLER          PIC X(1)    VALUE ';'.                       
019500         05  FILLER          PIC X(18)  VALUE                             
019600            'ADVICE NOTE NUMBER'.                                         
019700         05  FILLER          PIC X(1)    VALUE ';'.                       
019800         05  FILLER          PIC X(15)  VALUE                             
019900            'SUPPLIER NUMBER'.                                            
020000         05  FILLER          PIC X(1)    VALUE ';'.                       
020100         05  FILLER          PIC X(19)  VALUE                             
020200            'TYPE OF DISCREPANCY'.                                        
020300         05  FILLER          PIC X(1)    VALUE ';'.                       
020400         05  FILLER          PIC X(18)  VALUE                             
020500            'TYPE OF ACCOUNTING'.                                         
020600         05  FILLER          PIC X(1)    VALUE ';'.                       
020700         05  FILLER          PIC X(17)  VALUE                             
020800            'QUANTITY RECEIVED'.                                          
020900         05  FILLER          PIC X(1)    VALUE ';'.                       
021000         05  FILLER          PIC X(17)  VALUE                             
021100            'QUANTITY NOTIFIED'.                                          
021200         05  FILLER          PIC X(1)    VALUE ';'.                       
021300         05  FILLER          PIC X(15)  VALUE                             
021400            'UNIT OF MEASURE'.                                            
021500         05  FILLER          PIC X(1)    VALUE ';'.                       
021600         05  FILLER          PIC X(15)  VALUE                             
021700            'TRADING PARTNER'.                                            
021800         05  FILLER          PIC X(1)    VALUE ';'.                       
021900         05  FILLER          PIC X(13)  VALUE                             
022000            'OVER DELIVERY'.                                              
022100         05  FILLER          PIC X(1)    VALUE ';'.                       
022200         05  FILLER          PIC X(12)  VALUE                             
022300            'ORDER NUMBER'.                                               
022400         05  FILLER          PIC X(1)    VALUE ';'.                       
022500         05  FILLER          PIC X(13)  VALUE                             
022600            'USER IDENTITY'.                                              
022700         05  FILLER          PIC X(1)    VALUE ';'.                       
022800         05  FILLER          PIC X(12)  VALUE                             
022900            'REFERENCE ID'.                                               
023000         05  FILLER          PIC X(1)    VALUE ';'.                       
023100         05  FILLER          PIC X(10)  VALUE                             
023200            'ERROR TEXT'.                                                 
023300         05  FILLER          PIC X(1)    VALUE ';'.                       
023400         05  FILLER          PIC X(13)  VALUE                             
023500            'FINISHED FLAG'.                                              
023600         05  FILLER          PIC X(1)    VALUE ';'.                       
023700         05  FILLER          PIC X(14)  VALUE                             
023800            'TRANSPORT COST'.                                             
023900         05  FILLER          PIC X(1)    VALUE ';'.                       
024000         05  FILLER          PIC X(17)  VALUE                             
024100            'EXCHANGE TERMINAL'.                                          
024200         05  FILLER          PIC X(1)    VALUE ';'.                       
024300         05  FILLER          PIC X(16)  VALUE                             
024400            'FILLER'.                                                     
024500         05  FILLER          PIC X(1)    VALUE ';'.                       
024600                                                                          
024700     03 ROW-LINE.                                                         
024800*        05   -COPY W5106M   -PRE UT1-                                    
024900     EJECT                                                                
025000                                                                          
025100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025200     SKIP3                                                                
025300 01  NYCKLAR-TILL-DLI.                                                    
025400     03  W-WDR901KY-X.                                                    
025500         05  W-IDPGM             PIC X(8)    VALUE SPACE.                 
025600         05  W-DAREGDAT          PIC 9(8)    VALUE ZERO.                  
025700         05  W-TIKLOCK           PIC S9(9)   COMP-3.                      
025800         05  W-IDSEKVNR          PIC S9(3)   COMP-3.                      
025900     SKIP2                                                                
026000*    --- STATUS-KOD FRÅN IMS                                              
026100 01  STATUS-WS                   PIC XX.                                  
026200     88  SEGMENT-FINNS                       VALUE '  '.                  
026300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
026400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
026600     88  IMS-EJ-OK                           VALUE 'XD'.                  
026700     SKIP2                                                                
026800 01  GODK-STATUSKODER.                                                    
026900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027000     SKIP3                                                                
027100 01  SSA1                        PIC X(64).                               
027200 01  SSA2                        PIC X(64).                               
027300     EJECT                                                                
027400*    --- IMS FUNKTIONSKODER                                               
027500*01  -COPY W0003                                                          
027600     EJECT                                                                
027700*    ---  DLI INPUT-OUTPUT AREA                                           
027800                                                                          
027900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLSAPA01'.                    
028000 01  DLI-IO-WLSAPA01.                                                     
028100*    03  -COPY WDR901   -PRE SAPA-                                        
028200*    05  -COPY W510EKHA -PRE SAPA- -RED SAPA-FIL-WDR901-DATA              
028300                                                                          
028400     EJECT                                                                
028500 LINKAGE SECTION.                                                         
028600                                                                          
028700*01  -COPY W0009   -PRE MSG-                                              
028800                                                                          
028900*01  -COPY W0008  -PRE SAPA-                                              
029000     05  FILLER                  PIC X.                                   
029100     EJECT                                                                
029200 PROCEDURE DIVISION  USING MSG-PCB SAPA-PCB.                              
029300 MAIN SECTION.                                                            
029400     ENTRY 'DLITCBL' USING MSG-PCB SAPA-PCB.                              
029500                                                                          
029600     SKIP2                                                                
029700     PERFORM A-INIT                                                       
029800                                                                          
029900     PERFORM S01-LAES-W51062                                              
030000     PERFORM UNTIL END-OF-W51062                                          
030100       IF CHKP-ANT > CHKP-MAX                                             
030200         PERFORM X-TAG-CHECKPOINT                                         
030300       END-IF                                                             
030400       MOVE IN1-AREA TO DLI-IO-WLSAPA01                                   
030500       PERFORM B-LADDA-WDR9                                               
030600       ADD +1 TO CHKP-ANT                                                 
030700       PERFORM C-CREATE-MAIL                                              
030800       PERFORM S01-LAES-W51062                                            
030900     END-PERFORM                                                          
031000                                                                          
031010     PERFORM S01-LAES-W5106A                                              
031020     PERFORM UNTIL END-OF-W5106A                                          
031030       IF CHKP-ANT > CHKP-MAX                                             
031040         PERFORM X-TAG-CHECKPOINT                                         
031050       END-IF                                                             
031060       MOVE IN2-AREA TO DLI-IO-WLSAPA01                                   
031070       PERFORM B-LADDA-WDR9                                               
031080       ADD +1 TO CHKP-ANT                                                 
031091       PERFORM S01-LAES-W5106A                                            
031092     END-PERFORM                                                          
031093                                                                          
031100     PERFORM Z-FINIT                                                      
031200                                                                          
031300     MOVE ZERO TO RETURN-CODE                                             
031400     GOBACK                                                               
031500     .                                                                    
031600     EJECT                                                                
031700 A-INIT SECTION.                                                          
031800                                                                          
031900     PERFORM IMS-RESTART                                                  
032000                                                                          
032100     OPEN INPUT W51062                                                    
032101     OPEN INPUT W5106A                                                    
032110     OPEN OUTPUT W5106M                                                   
032200                                                                          
032300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
032400     .                                                                    
032500     EJECT                                                                
032600 B-LADDA-WDR9 SECTION.                                                    
032700     PERFORM IMS-ISRT-SAPA                                                
032800     PERFORM UNTIL SEGMENT-FINNS                                          
032810       IF SAPA-FIL-IDSEKVNR = 999                                         
032820         ADD +1 TO SAPA-FIL-TIKLOCK                                       
032830         MOVE 0 TO SAPA-FIL-IDSEKVNR                                      
032840       END-IF                                                             
032900       ADD +1 TO SAPA-FIL-IDSEKVNR                                        
033000       PERFORM IMS-ISRT-SAPA                                              
033100     END-PERFORM                                                          
033300     .                                                                    
033400     EJECT                                                                
033410                                                                          
033420 C-CREATE-MAIL SECTION.                                                   
033430     IF HEADER-DONE                                                       
033440       CONTINUE                                                           
033450     ELSE                                                                 
033460       WRITE W5106M-001 FROM HEAD-LINE                                    
033470       MOVE JA TO HEADER-SW                                               
033480     END-IF                                                               
033490     MOVE IN1-FIL-IDPGM               TO UT1-MAIL-IDPGM                   
033491     MOVE IN1-FIL-DAREGDAT            TO UT1-MAIL-DAREGDAT                
033492     MOVE IN1-FIL-TIKLOCK             TO UT1-MAIL-TIKLOCK                 
033493     MOVE IN1-FIL-IDSEKVNR            TO UT1-MAIL-IDSEKVNR                
033494     MOVE IN1-FIL-IDCPYTXT            TO UT1-MAIL-IDCPYTXT                
033495     MOVE IN1-EKH-BEVAT               TO UT1-MAIL-BEVAT                   
033496     MOVE IN1-EKH-DAVERDAT            TO UT1-MAIL-DAVERDAT                
033497     MOVE IN1-EKH-FLLSBOK             TO UT1-MAIL-FLLSBOK                 
033498     MOVE IN1-EKH-IDANALYS            TO UT1-MAIL-IDANALYS                
033499     MOVE IN1-EKH-IDARTNR             TO UT1-MAIL-IDARTNR                 
033500     MOVE IN1-EKH-IDDC-SEND           TO UT1-MAIL-IDDC-SEND               
033501     MOVE IN1-EKH-IDDC-REC            TO UT1-MAIL-IDDC-REC                
033502     MOVE IN1-EKH-IDDISTR             TO UT1-MAIL-IDDISTR                 
033503     MOVE IN1-EKH-IDKONTO             TO UT1-MAIL-IDKONTO                 
033504     MOVE IN1-EKH-IDKST               TO UT1-MAIL-IDKST                   
033505     MOVE IN1-EKH-IDKUNDNR            TO UT1-MAIL-IDKUNDNR                
033506     MOVE IN1-EKH-IDTRANS             TO UT1-MAIL-IDTRANS                 
033507     MOVE IN1-EKH-IDVERGL             TO UT1-MAIL-IDVERGL                 
033508     MOVE IN1-EKH-KDANMORS            TO UT1-MAIL-KDANMORS                
033509     MOVE IN1-EKH-KDEKHHT             TO UT1-MAIL-KDEKHHT                 
033510     MOVE IN1-EKH-KDEKSHT             TO UT1-MAIL-KDEKSHT                 
033511     MOVE IN1-EKH-KDEKNIVA            TO UT1-MAIL-KDEKNIVA                
033512     MOVE IN1-EKH-KDFRAKT             TO UT1-MAIL-KDFRAKT                 
033513     MOVE IN1-EKH-KDPRODSL            TO UT1-MAIL-KDPRODSL                
033514     MOVE IN1-EKH-KDPSLLOC            TO UT1-MAIL-KDPSLLOC                
033515     MOVE IN1-EKH-KDVALISO            TO UT1-MAIL-KDVALISO                
033516     MOVE IN1-EKH-KVANTAL             TO UT1-MAIL-KVANTAL                 
033517     MOVE IN1-EKH-PRARTNTO            TO UT1-MAIL-PRARTNTO                
033518     MOVE IN1-EKH-PRARTSJK            TO UT1-MAIL-PRARTSJK                
033519     MOVE IN1-EKH-PRARTSTD            TO UT1-MAIL-PRARTSTD                
033520     MOVE IN1-EKH-PRDIRLON            TO UT1-MAIL-PRDIRLON                
033521     MOVE IN1-EKH-PRDMTRL             TO UT1-MAIL-PRDMTRL                 
033522     MOVE IN1-EKH-PRINK               TO UT1-MAIL-PRINK                   
033523     MOVE IN1-EKH-PRKURS              TO UT1-MAIL-PRKURS                  
033524     MOVE IN1-EKH-PRLANDCO            TO UT1-MAIL-PRLANDCO                
033525     MOVE IN1-EKH-PROVRPAL            TO UT1-MAIL-PROVRPAL                
033526     MOVE IN1-EKH-SUBEL               TO UT1-MAIL-SUBEL                   
033527     MOVE IN1-EKH-SUVAT               TO UT1-MAIL-SUVAT                   
033528     MOVE IN1-EKH-DAAVIDAT            TO UT1-MAIL-DAAVIDAT                
033529     MOVE IN1-EKH-IDAVINR             TO UT1-MAIL-IDAVINR                 
033530     MOVE IN1-EKH-IDLEVNR             TO UT1-MAIL-IDLEVNR                 
033531     MOVE IN1-EKH-KDAVVTYP            TO UT1-MAIL-KDAVVTYP                
033532     MOVE IN1-EKH-KDRT                TO UT1-MAIL-KDRT                    
033533     MOVE IN1-EKH-KVANTMOT            TO UT1-MAIL-KVANTMOT                
033534     MOVE IN1-EKH-KVAVIS              TO UT1-MAIL-KVAVIS                  
033535     MOVE IN1-EKH-KDSORT              TO UT1-MAIL-KDSORT                  
033536     MOVE IN1-EKH-KDTRADP             TO UT1-MAIL-KDTRADP                 
033537     IF IN1-EKH-FLOVRLEV > SPACE                                          
033538       MOVE IN1-EKH-FLOVRLEV          TO UT1-MAIL-FLOVRLEV                
033539     ELSE                                                                 
033540       MOVE SPACE                     TO UT1-MAIL-FLOVRLEV                
033541     END-IF                                                               
033542     MOVE IN1-EKH-IDORDNR5            TO UT1-MAIL-IDORDNR5                
033543     MOVE IN1-FIL-IDUSER              TO UT1-MAIL-IDUSER                  
033545     IF IN1-EKH-IDREF > SPACE                                             
033546       MOVE IN1-EKH-IDREF             TO UT1-MAIL-IDREF                   
033547     ELSE                                                                 
033548       MOVE SPACE                     TO UT1-MAIL-IDREF                   
033549     END-IF                                                               
033550     MOVE IN1-EKH-BEFELSAP            TO UT1-MAIL-BEFELSAP                
033551     MOVE IN1-EKH-FLKLAR              TO UT1-MAIL-FLKLAR                  
033552     MOVE IN1-EKH-PRHEMTAG            TO UT1-MAIL-PRHEMTAG                
033553     MOVE IN1-EKH-FLDCET              TO UT1-MAIL-FLDCET                  
033554     MOVE SPACE                       TO UT1-MAIL-SPACE2                  
033555     MOVE ';' TO UT1-MAIL-SEMICOLON-1                                     
033556     MOVE ';' TO UT1-MAIL-SEMICOLON-2                                     
033557     MOVE ';' TO UT1-MAIL-SEMICOLON-3                                     
033558     MOVE ';' TO UT1-MAIL-SEMICOLON-4                                     
033559     MOVE ';' TO UT1-MAIL-SEMICOLON-5                                     
033560     MOVE ';' TO UT1-MAIL-SEMICOLON-6                                     
033561     MOVE ';' TO UT1-MAIL-SEMICOLON-7                                     
033562     MOVE ';' TO UT1-MAIL-SEMICOLON-8                                     
033563     MOVE ';' TO UT1-MAIL-SEMICOLON-9                                     
033564     MOVE ';' TO UT1-MAIL-SEMICOLON-10                                    
033565     MOVE ';' TO UT1-MAIL-SEMICOLON-11                                    
033566     MOVE ';' TO UT1-MAIL-SEMICOLON-12                                    
033567     MOVE ';' TO UT1-MAIL-SEMICOLON-13                                    
033568     MOVE ';' TO UT1-MAIL-SEMICOLON-14                                    
033569     MOVE ';' TO UT1-MAIL-SEMICOLON-15                                    
033570     MOVE ';' TO UT1-MAIL-SEMICOLON-16                                    
033571     MOVE ';' TO UT1-MAIL-SEMICOLON-17                                    
033572     MOVE ';' TO UT1-MAIL-SEMICOLON-18                                    
033573     MOVE ';' TO UT1-MAIL-SEMICOLON-19                                    
033574     MOVE ';' TO UT1-MAIL-SEMICOLON-20                                    
033575     MOVE ';' TO UT1-MAIL-SEMICOLON-21                                    
033576     MOVE ';' TO UT1-MAIL-SEMICOLON-22                                    
033577     MOVE ';' TO UT1-MAIL-SEMICOLON-23                                    
033578     MOVE ';' TO UT1-MAIL-SEMICOLON-24                                    
033579     MOVE ';' TO UT1-MAIL-SEMICOLON-25                                    
033580     MOVE ';' TO UT1-MAIL-SEMICOLON-26                                    
033581     MOVE ';' TO UT1-MAIL-SEMICOLON-27                                    
033582     MOVE ';' TO UT1-MAIL-SEMICOLON-28                                    
033583     MOVE ';' TO UT1-MAIL-SEMICOLON-29                                    
033584     MOVE ';' TO UT1-MAIL-SEMICOLON-30                                    
033585     MOVE ';' TO UT1-MAIL-SEMICOLON-31                                    
033586     MOVE ';' TO UT1-MAIL-SEMICOLON-32                                    
033587     MOVE ';' TO UT1-MAIL-SEMICOLON-33                                    
033588     MOVE ';' TO UT1-MAIL-SEMICOLON-34                                    
033589     MOVE ';' TO UT1-MAIL-SEMICOLON-35                                    
033590     MOVE ';' TO UT1-MAIL-SEMICOLON-36                                    
033591     MOVE ';' TO UT1-MAIL-SEMICOLON-37                                    
033592     MOVE ';' TO UT1-MAIL-SEMICOLON-38                                    
033593     MOVE ';' TO UT1-MAIL-SEMICOLON-39                                    
033594     MOVE ';' TO UT1-MAIL-SEMICOLON-40                                    
033595     MOVE ';' TO UT1-MAIL-SEMICOLON-41                                    
033596     MOVE ';' TO UT1-MAIL-SEMICOLON-42                                    
033597     MOVE ';' TO UT1-MAIL-SEMICOLON-43                                    
033598     MOVE ';' TO UT1-MAIL-SEMICOLON-44                                    
033599     MOVE ';' TO UT1-MAIL-SEMICOLON-45                                    
033600     MOVE ';' TO UT1-MAIL-SEMICOLON-46                                    
033601     MOVE ';' TO UT1-MAIL-SEMICOLON-47                                    
033602     MOVE ';' TO UT1-MAIL-SEMICOLON-48                                    
033603     MOVE ';' TO UT1-MAIL-SEMICOLON-49                                    
033604     MOVE ';' TO UT1-MAIL-SEMICOLON-50                                    
033605     MOVE ';' TO UT1-MAIL-SEMICOLON-51                                    
033606     MOVE ';' TO UT1-MAIL-SEMICOLON-52                                    
033607     MOVE ';' TO UT1-MAIL-SEMICOLON-53                                    
033608     MOVE ';' TO UT1-MAIL-SEMICOLON-54                                    
033609     MOVE ';' TO UT1-MAIL-SEMICOLON-55                                    
033610     MOVE ';' TO UT1-MAIL-SEMICOLON-56                                    
033611     WRITE W5106M-001 FROM ROW-LINE                                       
033612     .                                                                    
033613     EJECT                                                                
033614                                                                          
033615 Z-FINIT SECTION.                                                         
033620                                                                          
033700     CLOSE W51062                                                         
033701     CLOSE W5106A                                                         
033710     CLOSE W5106M                                                         
033800     SKIP2                                                                
033900     MOVE 'S' TO POSTSUM-OPKOD                                            
034000     CALL POSTSUM USING POSTSUM-PARM                                      
034100     .                                                                    
034200     EJECT                                                                
034300 S01-LAES-W51062  SECTION.                                                
034400     SKIP2                                                                
034500     READ W51062 INTO IN1-AREA                                            
034600     AT END                                                               
034700        SET END-OF-W51062 TO TRUE                                         
034800     NOT AT END                                                           
034900        MOVE 'W51062'   TO POSTSUM-FDNAMN                                 
035000        MOVE 'W51064D1' TO POSTSUM-DDNAMN2                                
035100        MOVE 'UPPD'     TO POSTSUM-TRANSTYP                               
035200        CALL POSTSUM USING POSTSUM-PARM                                   
035300     END-READ                                                             
035400     .                                                                    
035500     EJECT                                                                
035510 S01-LAES-W5106A  SECTION.                                                
035520     SKIP2                                                                
035530     READ W5106A INTO IN2-AREA                                            
035540     AT END                                                               
035550        SET END-OF-W5106A TO TRUE                                         
035560     NOT AT END                                                           
035570        MOVE 'W5106A'   TO POSTSUM-FDNAMN                                 
035580        MOVE 'W51064D3' TO POSTSUM-DDNAMN2                                
035590        MOVE 'UPPD'     TO POSTSUM-TRANSTYP                               
035591        CALL POSTSUM USING POSTSUM-PARM                                   
035592     END-READ                                                             
035593     .                                                                    
035594     EJECT                                                                
035600 X-TAG-CHECKPOINT   SECTION.                                              
035700                                                                          
035800     PERFORM IMS-CHECKPOINT                                               
035900     MOVE ZERO TO CHKP-ANT                                                
036000     .                                                                    
036100     SKIP3                                                                
036200* --- IMS SEKTIONER ---                                                   
036300                                                                          
036400     SKIP3                                                                
036500 IMS-ISRT-SAPA SECTION.                                                   
036600                                                                          
036700     MOVE 'WLSAPA01 ' TO SSA1                                             
036800     MOVE '  II' TO GODK-STATUSKODER                                      
036900     CALL CBLTDLI USING ISRT SAPA-PCB DLI-IO-WLSAPA01 SSA1                
037000     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
037100     PERFORM IMS-STATUSKONTROLL                                           
037200     .                                                                    
037300     EJECT                                                                
037400 IMS-RESTART SECTION.                                                     
037500     SKIP2                                                                
037600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
037700     MOVE '  ' TO GODK-STATUSKODER                                        
037800     CALL CBLTDLI USING XRST MSG-PCB                                      
037900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
038000                        CHKP-AREA-LENGTH CHKP-AREA                        
038100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
038200     PERFORM IMS-STATUSKONTROLL                                           
038300     .                                                                    
038400     SKIP3                                                                
038500 IMS-CHECKPOINT SECTION.                                                  
038600     SKIP2                                                                
038700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
038800     MOVE '  XD' TO GODK-STATUSKODER                                      
038900     CALL CBLTDLI USING CHKP MSG-PCB                                      
039000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
039100                        CHKP-AREA-LENGTH CHKP-AREA                        
039200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039300     PERFORM IMS-STATUSKONTROLL                                           
039400                                                                          
039500     IF IMS-EJ-OK                                                         
039600       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
039700       DISPLAY FELTEXT                                                    
039800       CALL FELLOG                                                        
039900     END-IF                                                               
040000     .                                                                    
040100     EJECT                                                                
040200 IMS-STATUSKONTROLL SECTION.                                              
040300     SKIP2                                                                
040400     SET STATUS-IX TO 1                                                   
040500     SEARCH GODK-STATUS                                                   
040600       AT END                                                             
040700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
040800           DELIMITED BY SIZE INTO FELTEXT                                 
040900         DISPLAY FELTEXT                                                  
041000         CALL FELLOG                                                      
041100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
041200         CONTINUE                                                         
041300     END-SEARCH                                                           
041400     .                                                                    
