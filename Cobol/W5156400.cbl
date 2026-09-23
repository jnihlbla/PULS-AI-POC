000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5156400.                                                
000300 AUTHOR.         HÅKAN BOHLIN.                                            
000400 DATE-WRITTEN.   20170823.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET LÄSER IN UPPDATERINGS-POSTER FRÅN FIL W5156F          
001000*        DESSA POSTER LÄGGS SEDAN UPP PÅ DATABASEN                        
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDR8                                       
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- INFIL FRÅN PGM W5156300                                    
002200     SELECT W5156F                     ASSIGN TO W51564D1.                
002300     SKIP2                                                                
002310*          --- INFIL FRÅN PGM W5156300                                    
002320     SELECT W5156B                     ASSIGN TO W51564D2.                
002330     SKIP2                                                                
002400*          --- UTFIL FRÅN PGM W5156400                                    
002500     SELECT W5156M                     ASSIGN TO W51564D3.                
002600     SKIP2                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W5156F                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400*01  -COPY WDR801      -L.                                                
003500     SKIP3                                                                
003510 FD  W5156B                                                               
003520     RECORDING       F                                                    
003530     BLOCK CONTAINS  0.                                                   
003540*01  -COPY WDR801      -L.                                                
003550     SKIP3                                                                
003600 FD  W5156M                                                               
003700     RECORDING       V                                                    
003800     BLOCK CONTAINS  0.                                                   
003900 01  W5156M-001              PIC X(999).                                  
004000     SKIP3                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W5156400'.            
004400 01  CHKP-VAR.                                                            
004500     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004600     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004700     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004800     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004900     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005000     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300     SKIP2                                                                
005400 01  FELTEXT.                                                             
005500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005700                                                                          
005800 77  W5156F-EOF-SW               PIC X       VALUE 'N'.                   
005900     88  END-OF-W5156F                       VALUE 'J'.                   
006000                                                                          
006001 77  W5156B-EOF-SW               PIC X       VALUE 'N'.                   
006002     88  END-OF-W5156B                       VALUE 'J'.                   
006003                                                                          
006010 77  HEADER-SW                   PIC X       VALUE 'N'.                   
006020     88  HEADER-DONE                         VALUE 'J'.                   
006021     88  HEADER-NOT-DONE                     VALUE 'N'.                   
006030                                                                          
006100     EJECT                                                                
006200 01  DYNAMISKA-SUBPROGRAM.                                                
006300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL POSTSUM                                          
006800*                                                                         
006900*01  -COPY W0005   -PRE  POSTSUM-                                         
007000     EJECT                                                                
007100 01  IN1-AREA-START              PIC X(24)   VALUE                        
007200                                             'IN1-AREA-START'.            
007300*01  AREA -COPY WDR801     -PRE IN1-                                      
007400*    05   -COPY W510EKHA   -PRE IN1- -RED IN1-FIL-WDR801-DATA             
007500     EJECT                                                                
007600                                                                          
007610 01  IN2-AREA-START              PIC X(24)   VALUE                        
007620                                             'IN2-AREA-START'.            
007630*01  AREA -COPY WDR801     -PRE IN2-                                      
007640*    05   -COPY W510EKHA   -PRE IN2- -RED IN2-FIL-WDR801-DATA             
007650     EJECT                                                                
007660                                                                          
007700 01  TEXT-AREA.                                                           
007800     03  HEAD-LINE.                                                       
007900         05  FILLER          PIC X(7)   VALUE                             
008000            'PROGRAM'.                                                    
008110         05  FILLER          PIC X(1)    VALUE ';'.                       
008200         05  FILLER          PIC X(13)   VALUE                            
008300            'REGISTER DATE'.                                              
008410         05  FILLER          PIC X(1)    VALUE ';'.                       
008500         05  FILLER          PIC X(13)   VALUE                            
008600            'REGISTER TIME'.                                              
008710         05  FILLER          PIC X(1)    VALUE ';'.                       
008800         05  FILLER          PIC X(15)   VALUE                            
008900            'SEQUENCE NUMBER'.                                            
009010         05  FILLER          PIC X(1)    VALUE ';'.                       
009100         05  FILLER          PIC X(17)   VALUE                            
009200            'IDENTITY COPYTEXT'.                                          
009310         05  FILLER          PIC X(1)    VALUE ';'.                       
009400         05  FILLER          PIC X(8)   VALUE                             
009500            'VAT CODE'.                                                   
009610         05  FILLER          PIC X(1)    VALUE ';'.                       
009700         05  FILLER          PIC X(17)   VALUE                            
009800            'VERIFICATION DATE'.                                          
009910         05  FILLER          PIC X(1)    VALUE ';'.                       
010000         05  FILLER          PIC X(13)  VALUE                             
010100            'STOCKUPDATING'.                                              
010210         05  FILLER          PIC X(1)    VALUE ';'.                       
010300         05  FILLER          PIC X(15)  VALUE                             
010400            'ANALYSIS NUMBER'.                                            
010510         05  FILLER          PIC X(1)    VALUE ';'.                       
010600         05  FILLER          PIC X(11)  VALUE                             
010700            'PART NUMBER'.                                                
010810         05  FILLER          PIC X(1)    VALUE ';'.                       
010900         05  FILLER          PIC X(17)  VALUE                             
011000            'SENDING WAREHOUSE'.                                          
011110         05  FILLER          PIC X(1)    VALUE ';'.                       
011200         05  FILLER          PIC X(19)  VALUE                             
011300            'RECEIVING WAREHOUSE'.                                        
011410         05  FILLER          PIC X(1)    VALUE ';'.                       
011500         05  FILLER          PIC X(15)  VALUE                             
011600            'DISTRICT NUMBER'.                                            
011710         05  FILLER          PIC X(1)    VALUE ';'.                       
011800         05  FILLER          PIC X(7)   VALUE                             
011900            'ACCOUNT'.                                                    
012010         05  FILLER          PIC X(1)    VALUE ';'.                       
012100         05  FILLER          PIC X(11)  VALUE                             
012200            'COST CENTRE'.                                                
012310         05  FILLER          PIC X(1)    VALUE ';'.                       
012400         05  FILLER          PIC X(11)  VALUE                             
012500            'CUSTOMER NO'.                                                
012610         05  FILLER          PIC X(1)    VALUE ';'.                       
012700         05  FILLER          PIC X(13)  VALUE                             
012800            'SCREEN NUMBER'.                                              
012910         05  FILLER          PIC X(1)    VALUE ';'.                       
013000         05  FILLER          PIC X(19)  VALUE                             
013100            'VERIFICATION NUMBER'.                                        
013210         05  FILLER          PIC X(1)    VALUE ';'.                       
013300         05  FILLER          PIC X(23)  VALUE                             
013400            'DISCREPANCY REASON CODE'.                                    
013510         05  FILLER          PIC X(1)    VALUE ';'.                       
013600         05  FILLER          PIC X(10)  VALUE                             
013700            'MAIN EVENT'.                                                 
013810         05  FILLER          PIC X(1)    VALUE ';'.                       
013900         05  FILLER          PIC X(9)   VALUE                             
014000            'SUB EVENT'.                                                  
014110         05  FILLER          PIC X(1)    VALUE ';'.                       
014200         05  FILLER          PIC X(11)  VALUE                             
014300            'EVENT LEVEL'.                                                
014410         05  FILLER          PIC X(1)    VALUE ';'.                       
014500         05  FILLER          PIC X(12)  VALUE                             
014600            'FREIGHT CODE'.                                               
014710         05  FILLER          PIC X(1)    VALUE ';'.                       
014800         05  FILLER          PIC X(13)  VALUE                             
014900            'PRODUCT GROUP'.                                              
015010         05  FILLER          PIC X(1)    VALUE ';'.                       
015100         05  FILLER          PIC X(19)  VALUE                             
015200            'PRODUCT GROUP LOCAL'.                                        
015310         05  FILLER          PIC X(1)    VALUE ';'.                       
015400         05  FILLER          PIC X(13)  VALUE                             
015500            'CURRENCY CODE'.                                              
015610         05  FILLER          PIC X(1)    VALUE ';'.                       
015700         05  FILLER          PIC X(15)  VALUE                             
015800            'QUANTITY NUMBER'.                                            
015910         05  FILLER          PIC X(1)    VALUE ';'.                       
016000         05  FILLER          PIC X(14)  VALUE                             
016100            'NET PRICE EACH'.                                             
016210         05  FILLER          PIC X(1)    VALUE ';'.                       
016300         05  FILLER          PIC X(13)  VALUE                             
016400            'COST OF SALES'.                                              
016510         05  FILLER          PIC X(1)    VALUE ';'.                       
016600         05  FILLER          PIC X(14)  VALUE                             
016700            'STANDARD PRICE'.                                             
016810         05  FILLER          PIC X(1)    VALUE ';'.                       
016900         05  FILLER          PIC X(15)  VALUE                             
017000            'SURCHARGE COSTS'.                                            
017110         05  FILLER          PIC X(1)    VALUE ';'.                       
017200         05  FILLER          PIC X(17)  VALUE                             
017300            'SURCHARGE PACKING'.                                          
017410         05  FILLER          PIC X(1)    VALUE ';'.                       
017500         05  FILLER          PIC X(14)  VALUE                             
017600            'PURCHASE PRICE'.                                             
017710         05  FILLER          PIC X(1)    VALUE ';'.                       
017800         05  FILLER          PIC X(22)  VALUE                             
017900            'CURRENCY EXCHANGE RATE'.                                     
018010         05  FILLER          PIC X(1)    VALUE ';'.                       
018100         05  FILLER          PIC X(12)  VALUE                             
018200            'LANDING COST'.                                               
018310         05  FILLER          PIC X(1)    VALUE ';'.                       
018400         05  FILLER          PIC X(18)  VALUE                             
018500            'OVERHEAD SURCHARGE'.                                         
018610         05  FILLER          PIC X(1)    VALUE ';'.                       
018700         05  FILLER          PIC X(10)  VALUE                             
018800            'SUM AMOUNT'.                                                 
018910         05  FILLER          PIC X(1)    VALUE ';'.                       
019000         05  FILLER          PIC X(9)   VALUE                             
019100            'VAT VALUE'.                                                  
019210         05  FILLER          PIC X(1)    VALUE ';'.                       
019300         05  FILLER          PIC X(16)  VALUE                             
019400            'ADVICE NOTE DATE'.                                           
019510         05  FILLER          PIC X(1)    VALUE ';'.                       
019600         05  FILLER          PIC X(18)  VALUE                             
019700            'ADVICE NOTE NUMBER'.                                         
019810         05  FILLER          PIC X(1)    VALUE ';'.                       
019900         05  FILLER          PIC X(15)  VALUE                             
020000            'SUPPLIER NUMBER'.                                            
020110         05  FILLER          PIC X(1)    VALUE ';'.                       
020200         05  FILLER          PIC X(19)  VALUE                             
020300            'TYPE OF DISCREPANCY'.                                        
020410         05  FILLER          PIC X(1)    VALUE ';'.                       
020500         05  FILLER          PIC X(18)  VALUE                             
020600            'TYPE OF ACCOUNTING'.                                         
020710         05  FILLER          PIC X(1)    VALUE ';'.                       
020800         05  FILLER          PIC X(17)  VALUE                             
020900            'QUANTITY RECEIVED'.                                          
021010         05  FILLER          PIC X(1)    VALUE ';'.                       
021100         05  FILLER          PIC X(17)  VALUE                             
021200            'QUANTITY NOTIFIED'.                                          
021310         05  FILLER          PIC X(1)    VALUE ';'.                       
021400         05  FILLER          PIC X(15)  VALUE                             
021500            'UNIT OF MEASURE'.                                            
021610         05  FILLER          PIC X(1)    VALUE ';'.                       
021700         05  FILLER          PIC X(15)  VALUE                             
021800            'TRADING PARTNER'.                                            
021910         05  FILLER          PIC X(1)    VALUE ';'.                       
022000         05  FILLER          PIC X(13)  VALUE                             
022100            'OVER DELIVERY'.                                              
022210         05  FILLER          PIC X(1)    VALUE ';'.                       
022300         05  FILLER          PIC X(12)  VALUE                             
022400            'ORDER NUMBER'.                                               
022510         05  FILLER          PIC X(1)    VALUE ';'.                       
022600         05  FILLER          PIC X(13)  VALUE                             
022700            'USER IDENTITY'.                                              
022810         05  FILLER          PIC X(1)    VALUE ';'.                       
022900         05  FILLER          PIC X(12)  VALUE                             
023000            'REFERENCE ID'.                                               
023110         05  FILLER          PIC X(1)    VALUE ';'.                       
023200         05  FILLER          PIC X(10)  VALUE                             
023300            'ERROR TEXT'.                                                 
023410         05  FILLER          PIC X(1)    VALUE ';'.                       
023500         05  FILLER          PIC X(13)  VALUE                             
023600            'FINISHED FLAG'.                                              
023710         05  FILLER          PIC X(1)    VALUE ';'.                       
023800         05  FILLER          PIC X(14)  VALUE                             
023900            'TRANSPORT COST'.                                             
024010         05  FILLER          PIC X(1)    VALUE ';'.                       
024100         05  FILLER          PIC X(17)  VALUE                             
024200            'EXCHANGE TERMINAL'.                                          
024310         05  FILLER          PIC X(1)    VALUE ';'.                       
024400         05  FILLER          PIC X(16)  VALUE                             
024500            'FILLER'.                                                     
024610         05  FILLER          PIC X(1)    VALUE ';'.                       
024700                                                                          
024800     03 ROW-LINE.                                                         
024900*        05   -COPY W5156M   -PRE UT1-                                    
025000     EJECT                                                                
025100                                                                          
025200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025300     SKIP3                                                                
025400 01  NYCKLAR-TILL-DLI.                                                    
025500     03  W-WDR901KY-X.                                                    
025600         05  W-IDPGM             PIC X(8)    VALUE SPACE.                 
025700         05  W-DAREGDAT          PIC 9(8)    VALUE ZERO.                  
025800         05  W-TIKLOCK           PIC S9(9)   COMP-3.                      
025900         05  W-IDSEKVNR          PIC S9(3)   COMP-3.                      
026000     SKIP2                                                                
026100*    --- STATUS-KOD FRÅN IMS                                              
026200 01  STATUS-WS                   PIC XX.                                  
026300     88  SEGMENT-FINNS                       VALUE '  '.                  
026400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
026500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
026700     88  IMS-EJ-OK                           VALUE 'XD'.                  
026800     SKIP2                                                                
026900 01  GODK-STATUSKODER.                                                    
027000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027100     SKIP3                                                                
027200 01  SSA1                        PIC X(64).                               
027300 01  SSA2                        PIC X(64).                               
027400     EJECT                                                                
027500*    --- IMS FUNKTIONSKODER                                               
027600*01  -COPY W0003                                                          
027700     EJECT                                                                
027800*    ---  DLI INPUT-OUTPUT AREA                                           
027900                                                                          
028000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR801'.                      
028100 01  DLI-IO-WDR801.                                                       
028200*    03  -COPY WDR801   -PRE WDR8-                                        
028300*    05  -COPY W510EKHA -PRE WDR8- -RED WDR8-FIL-WDR801-DATA              
028400                                                                          
028500     EJECT                                                                
028600 LINKAGE SECTION.                                                         
028700                                                                          
028800*01  -COPY W0009   -PRE MSG-                                              
028900                                                                          
029000*01  -COPY W0008  -PRE WDR8-                                              
029100     05  FILLER                  PIC X.                                   
029200     EJECT                                                                
029300 PROCEDURE DIVISION  USING MSG-PCB WDR8-PCB.                              
029400 MAIN SECTION.                                                            
029500     ENTRY 'DLITCBL' USING MSG-PCB WDR8-PCB.                              
029600                                                                          
029700     SKIP2                                                                
029800     PERFORM A-INIT                                                       
029900                                                                          
030000     PERFORM S01-LAES-W5156F                                              
030100     PERFORM UNTIL END-OF-W5156F                                          
030200       IF CHKP-ANT > CHKP-MAX                                             
030300         PERFORM X-TAG-CHECKPOINT                                         
030400       END-IF                                                             
030500       MOVE IN1-AREA TO DLI-IO-WDR801                                     
030600       PERFORM B-LADDA-WDR8                                               
030700       ADD +1 TO CHKP-ANT                                                 
030800       PERFORM C-CREATE-MAIL                                              
030900       PERFORM S01-LAES-W5156F                                            
031000     END-PERFORM                                                          
031100                                                                          
031110     PERFORM S01-LAES-W5156B                                              
031120     PERFORM UNTIL END-OF-W5156B                                          
031130       IF CHKP-ANT > CHKP-MAX                                             
031140         PERFORM X-TAG-CHECKPOINT                                         
031150       END-IF                                                             
031160       MOVE IN2-AREA TO DLI-IO-WDR801                                     
031170       PERFORM B-LADDA-WDR8                                               
031180       ADD +1 TO CHKP-ANT                                                 
031191       PERFORM S01-LAES-W5156B                                            
031192     END-PERFORM                                                          
031193                                                                          
031200     PERFORM Z-FINIT                                                      
031300                                                                          
031400     MOVE ZERO TO RETURN-CODE                                             
031500     GOBACK                                                               
031600     .                                                                    
031700     EJECT                                                                
031800 A-INIT SECTION.                                                          
031900                                                                          
032000     PERFORM IMS-RESTART                                                  
032100                                                                          
032200     OPEN INPUT  W5156F                                                   
032201     OPEN INPUT  W5156B                                                   
032210     OPEN OUTPUT W5156M                                                   
032300                                                                          
032310     MOVE NEJ TO HEADER-SW                                                
032400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
032500     .                                                                    
032600     EJECT                                                                
032700 B-LADDA-WDR8 SECTION.                                                    
032800     PERFORM IMS-ISRT-WDR8                                                
032900     PERFORM UNTIL SEGMENT-FINNS                                          
033000       ADD +1 TO WDR8-FIL-IDSEKVNR                                        
033100       PERFORM IMS-ISRT-WDR8                                              
033200     END-PERFORM                                                          
033400     .                                                                    
033500     EJECT                                                                
033501                                                                          
033510 C-CREATE-MAIL SECTION.                                                   
033511     IF HEADER-DONE                                                       
033512       CONTINUE                                                           
033513     ELSE                                                                 
033515       WRITE W5156M-001 FROM HEAD-LINE                                    
033516       MOVE JA TO HEADER-SW                                               
033517     END-IF                                                               
033520     MOVE IN1-FIL-IDPGM               TO UT1-MAIL-IDPGM                   
033530     MOVE IN1-FIL-TIREGDAT            TO UT1-MAIL-TIREGDAT                
033540     MOVE IN1-FIL-TIKLOCK             TO UT1-MAIL-TIKLOCK                 
033550     MOVE IN1-FIL-IDSEKVNR            TO UT1-MAIL-IDSEKVNR                
033551     MOVE IN1-FIL-IDCPYTXT            TO UT1-MAIL-IDCPYTXT                
033553     MOVE IN1-EKH-BEVAT               TO UT1-MAIL-BEVAT                   
033554     MOVE IN1-EKH-DAVERDAT            TO UT1-MAIL-DAVERDAT                
033555     MOVE IN1-EKH-FLLSBOK             TO UT1-MAIL-FLLSBOK                 
033556     MOVE IN1-EKH-IDANALYS            TO UT1-MAIL-IDANALYS                
033557     MOVE IN1-EKH-IDARTNR             TO UT1-MAIL-IDARTNR                 
033558     MOVE IN1-EKH-IDDC-SEND           TO UT1-MAIL-IDDC-SEND               
033559     MOVE IN1-EKH-IDDC-REC            TO UT1-MAIL-IDDC-REC                
033560     MOVE IN1-EKH-IDDISTR             TO UT1-MAIL-IDDISTR                 
033561     MOVE IN1-EKH-IDKONTO             TO UT1-MAIL-IDKONTO                 
033562     MOVE IN1-EKH-IDKST               TO UT1-MAIL-IDKST                   
033563     MOVE IN1-EKH-IDKUNDNR            TO UT1-MAIL-IDKUNDNR                
033564     MOVE IN1-EKH-IDTRANS             TO UT1-MAIL-IDTRANS                 
033565     MOVE IN1-EKH-IDVERGL             TO UT1-MAIL-IDVERGL                 
033566     MOVE IN1-EKH-KDANMORS            TO UT1-MAIL-KDANMORS                
033567     MOVE IN1-EKH-KDEKHHT             TO UT1-MAIL-KDEKHHT                 
033568     MOVE IN1-EKH-KDEKSHT             TO UT1-MAIL-KDEKSHT                 
033569     MOVE IN1-EKH-KDEKNIVA            TO UT1-MAIL-KDEKNIVA                
033570     MOVE IN1-EKH-KDFRAKT             TO UT1-MAIL-KDFRAKT                 
033571     MOVE IN1-EKH-KDPRODSL            TO UT1-MAIL-KDPRODSL                
033572     MOVE IN1-EKH-KDPSLLOC            TO UT1-MAIL-KDPSLLOC                
033573     MOVE IN1-EKH-KDVALISO            TO UT1-MAIL-KDVALISO                
033574     MOVE IN1-EKH-KVANTAL             TO UT1-MAIL-KVANTAL                 
033575     MOVE IN1-EKH-PRARTNTO            TO UT1-MAIL-PRARTNTO                
033576     MOVE IN1-EKH-PRARTSJK            TO UT1-MAIL-PRARTSJK                
033577     MOVE IN1-EKH-PRARTSTD            TO UT1-MAIL-PRARTSTD                
033578     MOVE IN1-EKH-PRDIRLON            TO UT1-MAIL-PRDIRLON                
033579     MOVE IN1-EKH-PRDMTRL             TO UT1-MAIL-PRDMTRL                 
033580     MOVE IN1-EKH-PRINK               TO UT1-MAIL-PRINK                   
033581     MOVE IN1-EKH-PRKURS              TO UT1-MAIL-PRKURS                  
033582     MOVE IN1-EKH-PRLANDCO            TO UT1-MAIL-PRLANDCO                
033583     MOVE IN1-EKH-PROVRPAL            TO UT1-MAIL-PROVRPAL                
033584     MOVE IN1-EKH-SUBEL               TO UT1-MAIL-SUBEL                   
033585     MOVE IN1-EKH-SUVAT               TO UT1-MAIL-SUVAT                   
033586     MOVE IN1-EKH-DAAVIDAT            TO UT1-MAIL-DAAVIDAT                
033587     MOVE IN1-EKH-IDAVINR             TO UT1-MAIL-IDAVINR                 
033588     MOVE IN1-EKH-IDLEVNR             TO UT1-MAIL-IDLEVNR                 
033589     MOVE IN1-EKH-KDAVVTYP            TO UT1-MAIL-KDAVVTYP                
033590     MOVE IN1-EKH-KDRT                TO UT1-MAIL-KDRT                    
033591     MOVE IN1-EKH-KVANTMOT            TO UT1-MAIL-KVANTMOT                
033592     MOVE IN1-EKH-KVAVIS              TO UT1-MAIL-KVAVIS                  
033593     MOVE IN1-EKH-KDSORT              TO UT1-MAIL-KDSORT                  
033594     MOVE IN1-EKH-KDTRADP             TO UT1-MAIL-KDTRADP                 
033595     MOVE IN1-EKH-FLOVRLEV            TO UT1-MAIL-FLOVRLEV                
033596     MOVE IN1-EKH-IDORDNR5            TO UT1-MAIL-IDORDNR5                
033597*    MOVE IN1-EKH-IDUSER              TO UT1-MAIL-IDUSER                  
033598     MOVE SPACE                       TO UT1-MAIL-IDUSER                  
033599     MOVE IN1-EKH-IDREF               TO UT1-MAIL-IDREF                   
033600     MOVE IN1-EKH-BEFELSAP            TO UT1-MAIL-BEFELSAP                
033601     MOVE IN1-EKH-FLKLAR              TO UT1-MAIL-FLKLAR                  
033602     MOVE IN1-EKH-PRHEMTAG            TO UT1-MAIL-PRHEMTAG                
033603     MOVE IN1-EKH-FLDCET              TO UT1-MAIL-FLDCET                  
033604     MOVE SPACE                       TO UT1-MAIL-SPACE2                  
033605     MOVE ';' TO UT1-MAIL-SEMICOLON-1                                     
033606     MOVE ';' TO UT1-MAIL-SEMICOLON-2                                     
033607     MOVE ';' TO UT1-MAIL-SEMICOLON-3                                     
033608     MOVE ';' TO UT1-MAIL-SEMICOLON-4                                     
033609     MOVE ';' TO UT1-MAIL-SEMICOLON-5                                     
033610     MOVE ';' TO UT1-MAIL-SEMICOLON-6                                     
033611     MOVE ';' TO UT1-MAIL-SEMICOLON-7                                     
033612     MOVE ';' TO UT1-MAIL-SEMICOLON-8                                     
033613     MOVE ';' TO UT1-MAIL-SEMICOLON-9                                     
033614     MOVE ';' TO UT1-MAIL-SEMICOLON-10                                    
033615     MOVE ';' TO UT1-MAIL-SEMICOLON-11                                    
033616     MOVE ';' TO UT1-MAIL-SEMICOLON-12                                    
033617     MOVE ';' TO UT1-MAIL-SEMICOLON-13                                    
033618     MOVE ';' TO UT1-MAIL-SEMICOLON-14                                    
033619     MOVE ';' TO UT1-MAIL-SEMICOLON-15                                    
033620     MOVE ';' TO UT1-MAIL-SEMICOLON-16                                    
033621     MOVE ';' TO UT1-MAIL-SEMICOLON-17                                    
033622     MOVE ';' TO UT1-MAIL-SEMICOLON-18                                    
033623     MOVE ';' TO UT1-MAIL-SEMICOLON-19                                    
033624     MOVE ';' TO UT1-MAIL-SEMICOLON-20                                    
033625     MOVE ';' TO UT1-MAIL-SEMICOLON-21                                    
033626     MOVE ';' TO UT1-MAIL-SEMICOLON-22                                    
033627     MOVE ';' TO UT1-MAIL-SEMICOLON-23                                    
033628     MOVE ';' TO UT1-MAIL-SEMICOLON-24                                    
033629     MOVE ';' TO UT1-MAIL-SEMICOLON-25                                    
033630     MOVE ';' TO UT1-MAIL-SEMICOLON-26                                    
033631     MOVE ';' TO UT1-MAIL-SEMICOLON-27                                    
033632     MOVE ';' TO UT1-MAIL-SEMICOLON-28                                    
033633     MOVE ';' TO UT1-MAIL-SEMICOLON-29                                    
033634     MOVE ';' TO UT1-MAIL-SEMICOLON-30                                    
033635     MOVE ';' TO UT1-MAIL-SEMICOLON-31                                    
033636     MOVE ';' TO UT1-MAIL-SEMICOLON-32                                    
033637     MOVE ';' TO UT1-MAIL-SEMICOLON-33                                    
033638     MOVE ';' TO UT1-MAIL-SEMICOLON-34                                    
033639     MOVE ';' TO UT1-MAIL-SEMICOLON-35                                    
033640     MOVE ';' TO UT1-MAIL-SEMICOLON-36                                    
033641     MOVE ';' TO UT1-MAIL-SEMICOLON-37                                    
033642     MOVE ';' TO UT1-MAIL-SEMICOLON-38                                    
033643     MOVE ';' TO UT1-MAIL-SEMICOLON-39                                    
033644     MOVE ';' TO UT1-MAIL-SEMICOLON-40                                    
033645     MOVE ';' TO UT1-MAIL-SEMICOLON-41                                    
033646     MOVE ';' TO UT1-MAIL-SEMICOLON-42                                    
033647     MOVE ';' TO UT1-MAIL-SEMICOLON-43                                    
033648     MOVE ';' TO UT1-MAIL-SEMICOLON-44                                    
033649     MOVE ';' TO UT1-MAIL-SEMICOLON-45                                    
033650     MOVE ';' TO UT1-MAIL-SEMICOLON-46                                    
033651     MOVE ';' TO UT1-MAIL-SEMICOLON-47                                    
033652     MOVE ';' TO UT1-MAIL-SEMICOLON-48                                    
033653     MOVE ';' TO UT1-MAIL-SEMICOLON-49                                    
033654     MOVE ';' TO UT1-MAIL-SEMICOLON-50                                    
033655     MOVE ';' TO UT1-MAIL-SEMICOLON-51                                    
033656     MOVE ';' TO UT1-MAIL-SEMICOLON-52                                    
033657     MOVE ';' TO UT1-MAIL-SEMICOLON-53                                    
033658     MOVE ';' TO UT1-MAIL-SEMICOLON-54                                    
033659     MOVE ';' TO UT1-MAIL-SEMICOLON-55                                    
033660     MOVE ';' TO UT1-MAIL-SEMICOLON-56                                    
033663     WRITE W5156M-001 FROM ROW-LINE                                       
033664     .                                                                    
033665     EJECT                                                                
033666                                                                          
033670 Z-FINIT SECTION.                                                         
033700                                                                          
033800     CLOSE W5156F                                                         
033801     CLOSE W5156B                                                         
033810     CLOSE W5156M                                                         
033900     SKIP2                                                                
034000     MOVE 'S' TO POSTSUM-OPKOD                                            
034100     CALL POSTSUM USING POSTSUM-PARM                                      
034200     .                                                                    
034300     EJECT                                                                
034400 S01-LAES-W5156F  SECTION.                                                
034500     SKIP2                                                                
034600     READ W5156F INTO IN1-AREA                                            
034700     AT END                                                               
034800        SET END-OF-W5156F TO TRUE                                         
034900     NOT AT END                                                           
035000        MOVE 'W5156F'   TO POSTSUM-FDNAMN                                 
035100        MOVE 'W51564D1' TO POSTSUM-DDNAMN2                                
035200        MOVE 'UPPD'     TO POSTSUM-TRANSTYP                               
035300        CALL POSTSUM USING POSTSUM-PARM                                   
035400     END-READ                                                             
035500     .                                                                    
035600     EJECT                                                                
035610 S01-LAES-W5156B  SECTION.                                                
035620     SKIP2                                                                
035630     READ W5156B INTO IN2-AREA                                            
035640     AT END                                                               
035650        SET END-OF-W5156B TO TRUE                                         
035660     NOT AT END                                                           
035670        MOVE 'W5156B'   TO POSTSUM-FDNAMN                                 
035680        MOVE 'W51564D2' TO POSTSUM-DDNAMN2                                
035690        MOVE 'UPPD'     TO POSTSUM-TRANSTYP                               
035691        CALL POSTSUM USING POSTSUM-PARM                                   
035692     END-READ                                                             
035693     .                                                                    
035694     EJECT                                                                
035700 X-TAG-CHECKPOINT   SECTION.                                              
035800                                                                          
035900     PERFORM IMS-CHECKPOINT                                               
036000     MOVE ZERO TO CHKP-ANT                                                
036100     .                                                                    
036200     SKIP3                                                                
036300* --- IMS SEKTIONER ---                                                   
036400                                                                          
036500     SKIP3                                                                
036600 IMS-ISRT-WDR8 SECTION.                                                   
036700                                                                          
036800     MOVE 'WDR801   ' TO SSA1                                             
036900     MOVE '  II' TO GODK-STATUSKODER                                      
037000     CALL CBLTDLI USING ISRT WDR8-PCB DLI-IO-WDR801 SSA1                  
037100     MOVE WDR8-STATUS-CODE TO STATUS-WS                                   
037200     PERFORM IMS-STATUSKONTROLL                                           
037300     .                                                                    
037400     EJECT                                                                
037500 IMS-RESTART SECTION.                                                     
037600     SKIP2                                                                
037700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
037800     MOVE '  ' TO GODK-STATUSKODER                                        
037900     CALL CBLTDLI USING XRST MSG-PCB                                      
038000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
038100                        CHKP-AREA-LENGTH CHKP-AREA                        
038200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
038300     PERFORM IMS-STATUSKONTROLL                                           
038400     .                                                                    
038500     SKIP3                                                                
038600 IMS-CHECKPOINT SECTION.                                                  
038700     SKIP2                                                                
038800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
038900     MOVE '  XD' TO GODK-STATUSKODER                                      
039000     CALL CBLTDLI USING CHKP MSG-PCB                                      
039100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
039200                        CHKP-AREA-LENGTH CHKP-AREA                        
039300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039400     PERFORM IMS-STATUSKONTROLL                                           
039500                                                                          
039600     IF IMS-EJ-OK                                                         
039700       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
039800       DISPLAY FELTEXT                                                    
039900       CALL FELLOG                                                        
040000     END-IF                                                               
040100     .                                                                    
040200     EJECT                                                                
040300 IMS-STATUSKONTROLL SECTION.                                              
040400     SKIP2                                                                
040500     SET STATUS-IX TO 1                                                   
040600     SEARCH GODK-STATUS                                                   
040700       AT END                                                             
040800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
040900           DELIMITED BY SIZE INTO FELTEXT                                 
041000         DISPLAY FELTEXT                                                  
041100         CALL FELLOG                                                      
041200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
041300         CONTINUE                                                         
041400     END-SEARCH                                                           
041500     .                                                                    
