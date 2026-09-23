000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6118900.                                                
000300 AUTHOR.         KUMAR LOVISH.                                            
000400 DATE-WRITTEN.   19/07/23.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        FIL W61188  (FRÅN PROGRAM W6118800 )                             
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDL2                                       
001100*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 -  - OM RETURKOD FRÅN SORT                                 
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- FIL MED BORT-SELEKTERADE POSTER                            
002400     SELECT W61188-IN                  ASSIGN TO W61189D1.                
002500     SKIP2                                                                
002600*          --- UTFILE                                                     
002700     SELECT W61188-LIST                ASSIGN TO W61189D2.                
002800     SKIP2                                                                
002900*          --- SNOWFLAKE                                                  
003000     SELECT W61188-SNOW                ASSIGN TO W61189D3.                
003100     SKIP2                                                                
003200*          --- SORTERINGSFIL                                              
003300     SELECT SORTFIL                    ASSIGN TO W61189DS.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W61188-IN                                                            
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  -COPY W6118801    -PRE FD- -L.                                       
004400     SKIP3                                                                
004500 FD  W61188-LIST                                                          
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900 01 UT-LISTA          PIC X(143).                                         
005000     SKIP2                                                                
005100 FD  W61188-SNOW                                                          
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400                                                                          
005500 01 UT-SNOW           PIC X(430).                                         
005600     SKIP2                                                                
005700 SD  SORTFIL.                                                             
005800                                                                          
005900*01  -COPY W6118801      -PRE SORT-.                                      
006000     EJECT                                                                
006100 WORKING-STORAGE SECTION.                                                 
006200                                                                          
006300 77  IDPGM                       PIC X(8)    VALUE 'W6118900'.            
006400 77  JA                          PIC X       VALUE 'J'.                   
006500 77  NEJ                         PIC X       VALUE 'N'.                   
006600 77  NY-SIDA                     PIC S9(5)   VALUE +99 COMP-3.            
006700 77  SORTFIL-EOF                 PIC X(1)    VALUE 'N'.                   
006800 01  FORSTA-POST                 PIC X       VALUE 'J'.                   
006900 77  W-CHKP-RAKNARE              PIC S9(5)   VALUE +0    COMP-3.          
007000 77  W-CHKP-MAX                  PIC S9(5)   VALUE +800  COMP-3.          
007100 77  CHKP-ID                     PIC X(8)    VALUE 'W6118900'.            
007200 77  MSG-IO-AREA-LENGTH-1        PIC S9(9)   VALUE +32  COMP SYNC.        
007300 77  MSG-IO-AREA-1               PIC X(32)   VALUE SPACE.                 
007400 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
007500 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
007600                                                                          
007700 01  W.                                                                   
007800     03  DAGENS-DATUM            PIC 9(6).                                
007900     03  ACK-RADANT              PIC S9(5)  COMP-3  VALUE ZERO.           
008000     03  ACK-SIDANT              PIC S9(5)  COMP-3  VALUE ZERO.           
008100                                                                          
008200     03  BLANKRAD                PIC X(121)         VALUE SPACE.          
008300     EJECT                                                                
008400 01  RUB1-RAD.                                                            
008500      05 RUB1-LIST-NAMN          PIC X(11) VALUE ' W61189-001'.           
008600      05 FILLER                  PIC X(9) VALUE SPACE.                    
008700      05 FILLER                  PIC X(4) VALUE 'VCCS'.                   
008800      05 FILLER                  PIC X(16) VALUE SPACE.                   
008900      05 FILLER                  PIC X(45)                                
009000     VALUE 'BORTSELEKTERADE POSTER UR INLEV-HISTORIKREG'.                 
009100      05 FILLER                  PIC X(5)  VALUE SPACE.                   
009200      05 FILLER                  PIC X(6)  VALUE 'DC: 11'.                
009300      05 FILLER                  PIC X(4)  VALUE SPACE.                   
009400      05 FILLER                  PIC X(6)  VALUE 'DATE: '.                
009500      05 RUB1-DATUM              PIC 9(6)  VALUE ZERO.                    
009600     EJECT                                                                
009700 01  RUB2-RAD.                                                            
009800     03 FILLER                   PIC X(4)        VALUE SPACE.             
009900     03 FILLER                   PIC X(6)        VALUE 'ARTNR '.          
010000     03 FILLER                   PIC X(4)        VALUE 'DC  '.            
010100     03 FILLER                   PIC X(6)        VALUE 'TT    '.          
010200     03 FILLER                   PIC X(5)        VALUE 'LÖPNR'.           
010300     03 FILLER                   PIC X(6)        VALUE '  DAT.'.          
010400     03 FILLER                   PIC X(4)        VALUE ' PP '.            
010500     03 FILLER                   PIC X(3)        VALUE 'RP '.             
010600     03 FILLER                   PIC X(3)        VALUE 'RT '.             
010700     03 FILLER                   PIC X(7)        VALUE 'AVSDAT '.         
010800     03 FILLER                   PIC X(6)        VALUE 'LEVNR '.          
010900     03 FILLER                   PIC X(4)        VALUE SPACE.             
011000     03 FILLER                   PIC X(6)        VALUE ' KONTO'.          
011100     03 FILLER                   PIC X(2)        VALUE SPACE.             
011200     03 FILLER                   PIC X(7)        VALUE ' AVINR '.         
011300     03 FILLER                   PIC X(6)        VALUE 'AVVKV '.          
011400     03 FILLER                   PIC X(7)        VALUE ' MOTKV '.         
011500     03 FILLER                   PIC X(7)        VALUE 'FÖRDEL '.         
011600     03 FILLER                   PIC X(7)        VALUE ' RETUR '.         
011700     03 FILLER                   PIC X(3)        VALUE 'AV '.             
011800     03 FILLER                   PIC X(2)        VALUE 'KV'.              
011900     03 FILLER                   PIC X(5)        VALUE ' DIST'.           
012000     03 FILLER                   PIC X(7)        VALUE ' KUNDNR'.         
012100     03 FILLER                   PIC X(8)        VALUE '  FAKTNR'.        
012200     03 FILLER                   PIC X(6)        VALUE ' ORDNR'.          
012300     03 FILLER                   PIC X(7)        VALUE ' PRODNR'.         
012400     EJECT                                                                
012500 01  RUB3-RAD.                                                            
012600     03 FILLER                   PIC X(9)  VALUE 'REPORT_ID'.             
012700     03 FILLER                   PIC X(1)  VALUE ';'.                     
013000     03 FILLER                   PIC X(11) VALUE 'REPORT_NAME'.           
013100     03 FILLER                   PIC X(1)  VALUE ';'.                     
013200     03 FILLER                   PIC X(11) VALUE 'REPORT_DATE'.           
013300     03 FILLER                   PIC X(1)  VALUE ';'.                     
013400     03 FILLER                   PIC X(11) VALUE 'PART_NUMBER'.           
013500     03 FILLER                   PIC X(1)  VALUE ';'.                     
013600     03 FILLER                   PIC X(19)                                
013700                                 VALUE 'DISTRUBUTION_CENTRE'.             
013800     03 FILLER                   PIC X(1)  VALUE ';'.                     
013900     03 FILLER                   PIC X(16)                                
014000                                 VALUE 'TRANSACTION_TYPE'.                
014100     03 FILLER                   PIC X(1)  VALUE ';'.                     
014200     03 FILLER                   PIC X(23)                                
014300                                 VALUE 'RECEIVING_REPORT_NUMBER'.         
014400     03 FILLER                   PIC X(1)  VALUE ';'.                     
014500     03 FILLER                   PIC X(15)                                
014600                                 VALUE 'ADJUSTMENT_DATE'.                 
014700     03 FILLER                   PIC X(1)  VALUE ';'.                     
014800     03 FILLER                   PIC X(15)                                
014900                                 VALUE 'PLANNING_PERIOD'.                 
015000     03 FILLER                   PIC X(1)  VALUE ';'.                     
015100     03 FILLER                   PIC X(17)                                
015200                                 VALUE 'ACCOUNTING_PERIOD'.               
015300     03 FILLER                   PIC X(1)  VALUE ';'.                     
015400     03 FILLER                   PIC X(15)                                
015500                                 VALUE 'ACCOUNTING_TYPE'.                 
015600     03 FILLER                   PIC X(1)  VALUE ';'.                     
015700     03 FILLER                   PIC X(16)                                
015800                                 VALUE 'ADVICE_NOTE_DATE'.                
015900     03 FILLER                   PIC X(1)  VALUE ';'.                     
016000     03 FILLER                   PIC X(6)  VALUE 'INFO_1'.                
016100     03 FILLER                   PIC X(1)  VALUE ';'.                     
016200     03 FILLER                   PIC X(6)  VALUE 'INFO_2'.                
016400     03 FILLER                   PIC X(1)  VALUE ';'.                     
016500     03 FILLER                   PIC X(18)                                
016600                                 VALUE 'ADVICE_NOTE_NUMBER'.              
016700     03 FILLER                   PIC X(1)  VALUE ';'.                     
016800     03 FILLER                   PIC X(15)                                
016900                                 VALUE 'ADVICE_QUANTITY'.                 
017000     03 FILLER                   PIC X(1)  VALUE ';'.                     
017100     03 FILLER                   PIC X(17)                                
017200                                 VALUE 'RECEIVED_QUANTITY'.               
017300     03 FILLER                   PIC X(1)  VALUE ';'.                     
017400     03 FILLER                   PIC X(14)                                
017500                                 VALUE 'SPLIT_QUANTITY'.                  
017600     03 FILLER                   PIC X(1)  VALUE ';'.                     
017700     03 FILLER                   PIC X(15)                                
017800                                 VALUE 'RETURN_QUANTITY'.                 
017900     03 FILLER                   PIC X(1)  VALUE ';'.                     
018000     03 FILLER                   PIC X(23)                                
018100                                 VALUE 'QUANTITY_DEVIATION_CODE'.         
018200     03 FILLER                   PIC X(1)  VALUE ';'.                     
018300     03 FILLER                   PIC X(22)                                
018400                                 VALUE 'QUALITY_DEVIATION_CODE'.          
018500     03 FILLER                   PIC X(1)  VALUE ';'.                     
018600     03 FILLER                   PIC X(8)  VALUE 'DISTRICT'.              
018700     03 FILLER                   PIC X(1)  VALUE ';'.                     
018800     03 FILLER                   PIC X(15)                                
018900                                 VALUE 'CUSTOMER_NUMBER'.                 
019000     03 FILLER                   PIC X(1)  VALUE ';'.                     
019100     03 FILLER                   PIC X(15)                                
019200                                 VALUE 'DOCUMENT_NUMBER'.                 
019300     03 FILLER                   PIC X(1)  VALUE ';'.                     
019400     03 FILLER                   PIC X(12)                                
019500                                 VALUE 'ORDER_NUMBER'.                    
019600     03 FILLER                   PIC X(1)  VALUE ';'.                     
019700     03 FILLER                   PIC X(17)                                
019800                                 VALUE 'PRODUCTION_NUMBER'.               
019900     EJECT                                                                
020000 01  DET1-RAD.                                                            
020100     03 DET1-IDARTNR             PIC Z(8)9.                               
020200     03 FILLER                   PIC X(1)         VALUE SPACE.            
020300     03 DET1-IDDC                PIC X(2).                                
020400     03 FILLER                   PIC X(1)         VALUE SPACE.            
020500     03 DET1-IDPTYP              PIC X(3).                                
020600     03 DET1-IDLOPNRM            PIC Z9(8).                               
020700     03 FILLER                   PIC X(1)         VALUE SPACE.            
020800     03 DET1-TIAAVVD             PIC 9(5).                                
020900     03 DET1-PP                  PIC Z(3).                                
021000     03 DET1-RP                  PIC Z(3).                                
021100     03 FILLER                   PIC X(1)         VALUE SPACE.            
021200     03 DET1-KDRT                PIC 9(2).                                
021300     03 FILLER                   PIC X(1)         VALUE SPACE.            
021400     03 DET1-TIAVSDAT            PIC 9(6).                                
021500     03 FILLER                   PIC X(1)         VALUE SPACE.            
021600     03 DET1-IDLEVNR             PIC X(5).                                
021700     03 DET1-IDKONTO             PIC Z(11).                               
021800     03 DET1-IDAVINR             PIC Z(7)9.                               
021900     03 DET1-KVAVIS              PIC Z(5)9.                               
022000     03 DET1-KVANTMOT            PIC -----9.                              
022100     03 DET1-KVFORDEL            PIC Z(6).                                
022200     03 DET1-KVRETUR             PIC Z(7).                                
022300     03 DET1-KDAVVANT            PIC Z(3).                                
022400     03 DET1-KDAVVKV             PIC Z(3).                                
022500     03 DET1-IDDISTR             PIC Z(5).                                
022600     03 DET1-IDKUNDNR            PIC Z(7).                                
022700     03 DET1-IDFAKT              PIC Z(8).                                
022800     03 DET1-IDORDNR5            PIC Z(6).                                
022900     03 DET1-IDPRODNR            PIC Z(7).                                
023000     EJECT                                                                
023100                                                                          
023200 01  DET1-RAD-C.                                                          
023300     03 FILLER                   PIC X(10) VALUE 'W61189-001'.            
023400     03 FILLER                   PIC X(1)         VALUE ';'.              
023700     03 FILLER                   PIC X(45)                                
023800            VALUE 'BORTSELEKTERADE POSTER UR INLEV-HISTORIKREG'.          
023900     03 FILLER                   PIC X(1)         VALUE ';'.              
024000     03 DET1-DATUM-C             PIC 9(6).                                
024100     03 FILLER                   PIC X(1)         VALUE ';'.              
024200     03 DET1-IDARTNR-C           PIC Z(8)9.                               
024300     03 FILLER                   PIC X(1)         VALUE ';'.              
024400     03 DET1-IDDC-C              PIC X(2).                                
024500     03 FILLER                   PIC X(1)         VALUE ';'.              
024600     03 DET1-IDPTYP-C            PIC X(3).                                
024700     03 FILLER                   PIC X(1)         VALUE ';'.              
024800     03 DET1-IDLOPNRM-C          PIC Z9(8).                               
024900     03 FILLER                   PIC X(1)         VALUE ';'.              
025000     03 DET1-TIAAMMDD-C          PIC 9(8).                                
025100     03 FILLER                   PIC X(1)         VALUE ';'.              
025200     03 DET1-PP-C                PIC Z(2)9.                               
025300     03 FILLER                   PIC X(1)         VALUE ';'.              
025400     03 DET1-RP-C                PIC Z(2)9.                               
025500     03 FILLER                   PIC X(1)         VALUE ';'.              
025600     03 DET1-KDRT-C              PIC 9(2).                                
025700     03 FILLER                   PIC X(1)         VALUE ';'.              
025800     03 DET1-TIAVSDAT-C          PIC 9(8).                                
025900     03 FILLER                   PIC X(1)         VALUE ';'.              
026000     03 DET1-IDLEVNR-C           PIC X(5).                                
026100     03 FILLER                   PIC X(1)         VALUE ';'.              
026200     03 DET1-IDKONTO-C           PIC Z(10)9.                              
026300     03 FILLER                   PIC X(1)         VALUE ';'.              
026400     03 DET1-IDAVINR-C           PIC Z(7)9.                               
026500     03 FILLER                   PIC X(1)         VALUE ';'.              
026600     03 DET1-KVAVIS-C            PIC Z(5)9.                               
026700     03 FILLER                   PIC X(1)         VALUE ';'.              
026800     03 DET1-KVANTMOT-C          PIC -----9.                              
026900     03 FILLER                   PIC X(1)         VALUE ';'.              
027000     03 DET1-KVFORDEL-C          PIC Z(5)9.                               
027100     03 FILLER                   PIC X(1)         VALUE ';'.              
027200     03 DET1-KVRETUR-C           PIC Z(6)9.                               
027300     03 FILLER                   PIC X(1)         VALUE ';'.              
027400     03 DET1-KDAVVANT-C          PIC Z(2)9.                               
027500     03 FILLER                   PIC X(1)         VALUE ';'.              
027600     03 DET1-KDAVVKV-C           PIC Z(2)9.                               
027700     03 FILLER                   PIC X(1)         VALUE ';'.              
027800     03 DET1-IDDISTR-C           PIC Z(4)9.                               
027900     03 FILLER                   PIC X(1)         VALUE ';'.              
028000     03 DET1-IDKUNDNR-C          PIC Z(6)9.                               
028100     03 FILLER                   PIC X(1)         VALUE ';'.              
028200     03 DET1-IDFAKT-C            PIC Z(7)9.                               
028300     03 FILLER                   PIC X(1)         VALUE ';'.              
028400     03 DET1-IDORDNR5-C          PIC Z(5)9.                               
028500     03 FILLER                   PIC X(1)         VALUE ';'.              
028600     03 DET1-IDPRODNR-C          PIC Z(6)9.                               
028700     EJECT                                                                
028800                                                                          
028900 77  W61188-EOF-SW               PIC X       VALUE 'N'.                   
029000     88  END-OF-W61188                       VALUE 'J'.                   
029100     EJECT                                                                
029200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
029300 01  FILLER REDEFINES DAGENS-DATUM.                                       
029400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
029500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
029600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
029700     EJECT                                                                
029800 01  DYNAMISKA-SUBPROGRAM.                                                
029900*                                                                         
030000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
030100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
030200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
030300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
030400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
030500     SKIP2                                                                
030600*    --- PARAMETRAR TILL ABEND                                            
030700                                                                          
030800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
030900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
031000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
031100     SKIP2                                                                
031200 01  FELTEXT.                                                             
031300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
031400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
031500     EJECT                                                                
031600*    --- PARAMETRAR TILL POSTSUM                                          
031700*                                                                         
031800*01  -COPY W0005   -PRE  POSTSUM-                                         
031900     EJECT                                                                
032000*01  -COPY WDATAREA                                                       
032100     EJECT                                                                
032200 01  IN-AREA-START               PIC X(24)   VALUE                        
032300                                 'IN-AREA-START  '.                       
032400     SKIP2                                                                
032500                                                                          
032600*01  AREA -COPY W6118801     -PRE IN-                                     
032700     EJECT                                                                
032800 01  UT-AREA-START               PIC X(24)   VALUE                        
032900                                 'UT-AREA-START  '.                       
033000     SKIP2                                                                
033100                                                                          
033200*01  AREA -COPY W6118801     -PRE UT-                                     
033300     EJECT                                                                
033400 01  WSORT-AREA-START            PIC X(24)   VALUE                        
033500                                  'WSORT-AREA-START  '.                   
033600     SKIP2                                                                
033700                                                                          
033800*01  AREA  -PRE WSORT-  -COPY W6118801                                    
033900 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
034000     EJECT                                                                
034100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
034200*                                                                         
034300     EJECT                                                                
034400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
034500     SKIP3                                                                
034600 01  NYCKLAR-TILL-DLI.                                                    
034700     03  W-IDARTNR-X.                                                     
034800         05  W-IDARTNR           PIC S9(9)   COMP-3.                      
034900 01  W-DAINLEV-X.                                                         
035000     03  W-DAINLEV               PIC 9(16).                               
035100     SKIP2                                                                
035200*    --- STATUS-KOD FRÅN IMS                                              
035300 01  STATUS-WS                   PIC XX.                                  
035400     88  SEGMENT-FINNS                       VALUE '  '.                  
035500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
035600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
035700     SKIP2                                                                
035800 01  GODK-STATUSKODER.                                                    
035900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
036000     SKIP3                                                                
036100 01  SSA1                        PIC X(64).                               
036200 01  SSA2                        PIC X(64).                               
036300     EJECT                                                                
036400*    --- IMS FUNKTIONSKODER                                               
036500*01  -COPY W0003                                                          
036600     EJECT                                                                
036700*    ---  DLI INPUT-OUTPUT AREA                                           
036800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL201'.                      
036900 01  DLI-IO-WDL211.                                                       
037000*    03  -COPY WDL211                                                     
037100     EJECT                                                                
037200 LINKAGE SECTION.                                                         
037300                                                                          
037400*01  -COPY W0009  -PRE MSG-                                               
037500                                                                          
037600*01  -COPY W0008  -PRE WDL2-                                              
037700     05  FILLER                  PIC X.                                   
037800     EJECT                                                                
037900 PROCEDURE DIVISION  USING MSG-PCB WDL2-PCB.                              
038000 MAIN SECTION.                                                            
038100     ENTRY 'DLITCBL' USING MSG-PCB WDL2-PCB.                              
038200                                                                          
038300     PERFORM A-INIT                                                       
038400                                                                          
038500     SORT SORTFIL ASCENDING                                               
038600                  SORT-IDARTNR                                            
038700                  SORT-DAINLEV                                            
038800                  SORT-TIAAVVD                                            
038900                  SORT-IDLOPNRM                                           
039000                                                                          
039100          USING   W61188-IN                                               
039200                                                                          
039300          OUTPUT PROCEDURE B-BEARBETNING                                  
039400                                                                          
039500     IF SORT-RETURN > ZERO                                                
039600        DISPLAY '***  W6118900  - FEL VID SORTERING'                      
039700        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
039800                                                                          
039900     ELSE                                                                 
040000        PERFORM Z-FINIT                                                   
040100                                                                          
040200        MOVE ZERO TO RETURN-CODE                                          
040300                                                                          
040400        GOBACK                                                            
040500                                                                          
040600     END-IF                                                               
040700     .                                                                    
040800     EJECT                                                                
040900 A-INIT SECTION.                                                          
041000                                                                          
041100     PERFORM IMS-RESTART                                                  
041200     OPEN OUTPUT W61188-LIST                                              
041300                 W61188-SNOW                                              
041400                                                                          
041500     ACCEPT DAGENS-DATUM FROM DATE                                        
041600     MOVE DAGENS-DATUM  TO RUB1-DATUM                                     
041700                           DET1-DATUM-C                                   
041800                                                                          
041900     MOVE NY-SIDA       TO ACK-RADANT                                     
042000     MOVE ZERO          TO W-CHKP-RAKNARE                                 
042100     .                                                                    
042200     EJECT                                                                
042300 B-BEARBETNING SECTION.                                                   
042400                                                                          
042500     PERFORM S01-LAS-SORTERAD-W61188                                      
042600                                                                          
042700     PERFORM UNTIL SORTFIL-EOF = JA                                       
042800                                                                          
042900         IF  FORSTA-POST = 'J'                                            
043000             PERFORM BA-SKRIV-RUB                                         
043100         END-IF                                                           
043200                                                                          
043300         PERFORM BB-REDIGERA-DET                                          
043400                                                                          
043500         MOVE DAGENS-DATUM  TO DET1-DATUM-C                               
043600         WRITE UT-LISTA FROM DET1-RAD                                     
043700         WRITE UT-SNOW  FROM DET1-RAD-C                                   
043800                                                                          
043900         PERFORM BC-DELETE-WDL211                                         
044000                                                                          
044100         PERFORM S01-LAS-SORTERAD-W61188                                  
044200                                                                          
044300     END-PERFORM                                                          
044400     .                                                                    
044500     EJECT                                                                
044600 BA-SKRIV-RUB SECTION.                                                    
044700                                                                          
044800     WRITE UT-LISTA  FROM RUB1-RAD AFTER PAGE                             
044900     WRITE UT-LISTA  FROM RUB2-RAD AFTER 2                                
045000     WRITE UT-LISTA  FROM BLANKRAD AFTER 1                                
045100     WRITE UT-SNOW   FROM RUB3-RAD                                        
045200     MOVE 'N'             TO FORSTA-POST                                  
045300     .                                                                    
045400     EJECT                                                                
045500 BB-REDIGERA-DET SECTION.                                                 
045600                                                                          
045700     MOVE WSORT-TIAAVVD             TO DAT-I-TIDATUM                      
045800     MOVE 'AAVVD'                   TO DAT-KDDATFORM                      
045900                                                                          
046000     CALL WDATKONV                  USING DAT-KDDATFORM                   
046100                                          DAT-I-TIDATUM                   
046200                                          DAT-O-TIDATUM                   
046300                                          DAT-KDSVAR                      
046400                                                                          
046500     MOVE SPACE                     TO DET1-RAD                           
046600     INITIALIZE DET1-RAD-C                                                
046700     MOVE DAT-TIP                   TO DET1-PP                            
046800                                       DET1-PP-C                          
046900     MOVE DAT-TIRP                  TO DET1-RP                            
047000                                       DET1-RP-C                          
047100     MOVE WSORT-TIAAVVD             TO DET1-TIAAVVD                       
047110     IF DAT-TIAAMMDD(1:2) < 50                                            
047120        MOVE '20'                   TO DET1-TIAAMMDD-C(1:2)               
047130     ELSE                                                                 
047140        MOVE '19'                   TO DET1-TIAAMMDD-C(1:2)               
047150     END-IF                                                               
047200     MOVE DAT-TIAAMMDD              TO DET1-TIAAMMDD-C(3:6)               
047300     MOVE WSORT-IDLOPNRM            TO DET1-IDLOPNRM                      
047400                                       DET1-IDLOPNRM-C                    
047500     MOVE WSORT-IDARTNR             TO DET1-IDARTNR                       
047600                                       DET1-IDARTNR-C                     
047700     MOVE WSORT-IDDC                TO DET1-IDDC                          
047800                                       DET1-IDDC-C                        
047900     MOVE WSORT-IDPTYP              TO DET1-IDPTYP                        
048000                                       DET1-IDPTYP-C                      
048100     MOVE WSORT-IDLEVNR             TO DET1-IDLEVNR                       
048200                                       DET1-IDLEVNR-C                     
048300                                                                          
048400     IF  WSORT-IDPTYP = 'R32'                                             
048500         MOVE WSORT-KDRT            TO DET1-KDRT                          
048600                                       DET1-KDRT-C                        
048700         MOVE WSORT-TIAVSDAT        TO DET1-TIAVSDAT                      
048710         IF WSORT-TIAVSDAT(1:2) < 50                                      
048720            MOVE '20'               TO DET1-TIAVSDAT-C(1:2)               
048730         ELSE                                                             
048740            MOVE '19'               TO DET1-TIAVSDAT-C(1:2)               
048750         END-IF                                                           
048800         MOVE WSORT-TIAVSDAT        TO DET1-TIAVSDAT-C(3:6)               
048900         MOVE WSORT-IDKONTO         TO DET1-IDKONTO                       
049000                                       DET1-IDKONTO-C                     
049100         MOVE WSORT-IDAVINR         TO DET1-IDAVINR                       
049200                                       DET1-IDAVINR-C                     
049300         MOVE WSORT-KVAVIS          TO DET1-KVAVIS                        
049400                                       DET1-KVAVIS-C                      
049500         MOVE WSORT-KVANTMOT        TO DET1-KVANTMOT                      
049600                                       DET1-KVANTMOT-C                    
049700         MOVE WSORT-KVFORDEL        TO DET1-KVFORDEL                      
049800                                       DET1-KVFORDEL-C                    
049900         MOVE WSORT-KVRETUR         TO DET1-KVRETUR                       
050000                                       DET1-KVRETUR-C                     
050100         MOVE WSORT-KDAVVANT        TO DET1-KDAVVANT                      
050200                                       DET1-KDAVVANT-C                    
050300         MOVE WSORT-KDAVVKV         TO DET1-KDAVVKV                       
050400                                       DET1-KDAVVKV-C                     
050500                                                                          
050600     ELSE                                                                 
050700         IF  WSORT-IDPTYP = 'R33' OR 'R34'                                
050800             MOVE WSORT-KDRT        TO DET1-KDRT                          
050900                                       DET1-KDRT-C                        
051000             MOVE WSORT-TIAVSDAT    TO DET1-TIAVSDAT                      
051010             IF WSORT-TIAVSDAT(1:2) < 50                                  
051020                MOVE '20'           TO DET1-TIAVSDAT-C(1:2)               
051030             ELSE                                                         
051040                MOVE '19'           TO DET1-TIAVSDAT-C(1:2)               
051050             END-IF                                                       
051100             MOVE WSORT-TIAVSDAT    TO DET1-TIAVSDAT-C(3:6)               
051200             MOVE WSORT-IDKONTO     TO DET1-IDKONTO                       
051300                                       DET1-IDKONTO-C                     
051400             MOVE WSORT-IDAVINR     TO DET1-IDAVINR                       
051500                                       DET1-IDAVINR-C                     
051600             MOVE WSORT-KVAVIS      TO DET1-KVANTMOT                      
051700                                       DET1-KVANTMOT-C                    
051800             MOVE WSORT-IDDISTR     TO DET1-IDDISTR                       
051900                                       DET1-IDDISTR-C                     
052000             MOVE WSORT-IDKUNDNR    TO DET1-IDKUNDNR                      
052100                                       DET1-IDKUNDNR-C                    
052200             MOVE WSORT-IDFAKT      TO DET1-IDFAKT                        
052300                                       DET1-IDFAKT-C                      
052400             MOVE WSORT-IDKUNDRF(1:5) TO DET1-IDORDNR5                    
052500                                         DET1-IDORDNR5-C                  
052600             MOVE WSORT-IDPRODNR    TO DET1-IDPRODNR                      
052700                                       DET1-IDPRODNR-C                    
052800         ELSE                                                             
052900             IF  WSORT-IDPTYP = 'R40'                                     
053000                 MOVE WSORT-IDORDNR TO DET1-IDAVINR                       
053100                                       DET1-IDAVINR-C                     
053200                 MOVE WSORT-KVRETUR TO DET1-KVANTMOT                      
053300                                       DET1-KVANTMOT-C                    
053400             END-IF                                                       
053500         END-IF                                                           
053600     END-IF                                                               
053700     .                                                                    
053800     SKIP3                                                                
053900 BC-DELETE-WDL211 SECTION.                                                
054000                                                                          
054100     MOVE WSORT-IDARTNR      TO W-IDARTNR                                 
054200     MOVE WSORT-DAINLEV      TO W-DAINLEV                                 
054300                                                                          
054400     PERFORM IMS-GHU-WDL211                                               
054500                                                                          
054600     IF SEGMENT-FINNS                                                     
054700*       PERFORM IMS-DLET-WDL211                                           
054800        ADD +1      TO W-CHKP-RAKNARE                                     
054900     END-IF                                                               
055000                                                                          
055100     IF W-CHKP-RAKNARE                 >  W-CHKP-MAX                      
055200        PERFORM IMS-CHECKPOINT                                            
055300        MOVE ZERO                      TO W-CHKP-RAKNARE                  
055400     END-IF                                                               
055500     .                                                                    
055600     SKIP3                                                                
055700                                                                          
055800 S01-LAS-SORTERAD-W61188 SECTION.                                         
055900                                                                          
056000     RETURN SORTFIL   INTO WSORT-AREA                                     
056100     AT END                                                               
056200       MOVE JA TO SORTFIL-EOF                                             
056300     END-RETURN                                                           
056400     .                                                                    
056500     SKIP3                                                                
056600 Z-FINIT SECTION.                                                         
056700                                                                          
056800     CLOSE W61188-LIST                                                    
056900           W61188-SNOW                                                    
057000     .                                                                    
057100* --- IMS SEKTIONER ---                                                   
057200                                                                          
057300     EJECT                                                                
057400 IMS-RESTART           SECTION.                                           
057500                                                                          
057600     MOVE SPACE TO MSG-IO-AREA-1                                          
057700     MOVE '  ' TO GODK-STATUSKODER                                        
057800     CALL CBLTDLI USING XRST MSG-PCB                                      
057900                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
058000                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
058100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
058200     PERFORM IMS-STATUSKONTROLL                                           
058300     .                                                                    
058400                                                                          
058500     SKIP3                                                                
058600 IMS-CHECKPOINT        SECTION.                                           
058700                                                                          
058800     MOVE CHKP-ID TO MSG-IO-AREA-1                                        
058900     MOVE '  XD' TO GODK-STATUSKODER                                      
059000     CALL CBLTDLI USING CHKP MSG-PCB                                      
059100                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
059200                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
059300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
059400     PERFORM IMS-STATUSKONTROLL                                           
059500     .                                                                    
059600     EJECT                                                                
059700 IMS-GHU-WDL211 SECTION.                                                  
059800     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
059900          DELIMITED BY SIZE INTO SSA1                                     
060000     STRING 'WDL211  (DAINLEV  =' W-DAINLEV-X ')'                         
060100             DELIMITED BY SIZE INTO SSA2                                  
060200     MOVE '  GE' TO GODK-STATUSKODER                                      
060300     CALL CBLTDLI USING GHU WDL2-PCB DLI-IO-WDL211 SSA1 SSA2              
060400     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
060500     PERFORM IMS-STATUSKONTROLL                                           
060600     .                                                                    
060700     EJECT                                                                
060800 IMS-DLET-WDL211    SECTION.                                              
060900     MOVE SPACE  TO GODK-STATUSKODER                                      
061000     CALL CBLTDLI USING DLET WDL2-PCB DLI-IO-WDL211                       
061100                                                                          
061200     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
061300     PERFORM IMS-STATUSKONTROLL                                           
061400     .                                                                    
061500     EJECT                                                                
061600 IMS-STATUSKONTROLL SECTION.                                              
061700                                                                          
061800     SET STATUS-IX TO 1                                                   
061900     SEARCH GODK-STATUS                                                   
062000       AT END                                                             
062100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
062200           DELIMITED BY SIZE INTO FELTEXT                                 
062300         DISPLAY FELTEXT                                                  
062400         CALL FELLOG                                                      
062500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
062600         CONTINUE                                                         
062700     END-SEARCH                                                           
062800     .                                                                    
