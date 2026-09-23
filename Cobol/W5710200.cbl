000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5710200.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   11/10/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        THIS PROGRAM IS USED TO PRINT AND DOWNLOAD THE ACS REPORT        
001000*        . DATA IS SENT TO D&P.                                           
001100*                                                                         
001200*        THE PROGRAM READS     WDB6                                       
001300*        THE PROGRAM READS     WDR2                                       
001400*        THE PROGRAM READS     WDD8                                       
001500*        THE PROGRAM UPDATES   WDJ7                                       
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002410*          --- SYSIN FROM JCL                                             
002420     SELECT INDATA                     ASSIGN TO SYSIN.                   
002500*          --- LIST OF DC'S THAT HAVE OPTED FOR ACS                       
002600     SELECT W571D1                     ASSIGN TO W57102D1.                
002610*          --- DC VALUE                                                   
002620     SELECT W571D2                     ASSIGN TO W57102D2.                
002700     SKIP2                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003110 FD INDATA                                                                
003120     LABEL RECORD STANDARD                                                
003130     RECORDING  F                                                         
003140     BLOCK CONTAINS 0.                                                    
003150 01  INPOST                  PIC X(80).                                   
003160                                                                          
003200 FD  W571D1                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  -COPY W57101      -L.                                                
003601                                                                          
003610 FD W571D2                                                                
003620     LABEL RECORD STANDARD                                                
003630     RECORDING  F                                                         
003640     BLOCK CONTAINS 0.                                                    
003650 01 W571D2-RECORD            PIC X(80).                                   
003700     SKIP3                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000 77  IDPGM                       PIC X(8)    VALUE 'W5710200'.            
004100 77  KDRC-DISPLAY                PIC Z(5).                                
004200 77  WS-SUVALINV-INC             PIC 9(11)V9(2) VALUE ZERO.               
004300 77  WS-SUVALINV-EXCL            PIC 9(11)V9(2) VALUE ZERO.               
004400 77  WS-KVINVART-INC             PIC S9(7) VALUE ZERO.                    
004500 77  WS-KVINVART-EXCL            PIC S9(7) VALUE ZERO.                    
004600 77  WS-KVINVART                 PIC S9(7) VALUE ZERO.                    
004610 77  WS-TIINVDAT                 PIC S9(7) VALUE ZERO.                    
004700 77  WS-SUVALINV                 PIC 9(11)V9(2) VALUE ZERO.               
004800 77  WS-REQTYDEV-INC             PIC 9(3)V9(4) VALUE ZERO.                
004810 77  WS-REQTYDEV-INC1            PIC 9(3)V9(2) VALUE ZERO.                
004900 77  WS-REQTYDEV-EXCL            PIC 9(3)V9(4) VALUE ZERO.                
004910 77  WS-REQTYDEV-EXCL1           PIC 9(3)V9(2) VALUE ZERO.                
005000 77  WS-REVALDEV-INC             PIC 9(3)V9(4) VALUE ZERO.                
005010 77  WS-REVALDEV-INC1            PIC 9(3)V9(2) VALUE ZERO.                
005100 77  WS-REVALDEV-EXCL            PIC 9(3)V9(4) VALUE ZERO.                
005101 77  WS-REVALDEV-EXCL1           PIC 9(3)V9(2) VALUE ZERO.                
005110 77  WS-NO-EXCL-PART             PIC X(41) VALUE                          
005120               'NO EXCLUDED PARTS'.                                       
005200 77  WS-ADDRESS                  PIC X(25)   VALUE                        
005300               'CARPARTS.PULS.ACSDOWNLOAD'.                               
005400 77  WZ04-001-IDCOM              PIC S9(9)   COMP VALUE +0.               
005402 77  YES                         PIC X       VALUE 'J'.                   
005403 77  NOO                         PIC X       VALUE 'N'.                   
005404                                                                          
005405 01  FILLER                      PIC X(10)   VALUE 'INAREA'.              
005410 01  INAREA.                                                              
005420     03 WS-RECV-IDDC             PIC X(2)    VALUE SPACE.                 
005430     03 FILLER                   PIC X(78)   VALUE SPACE.                 
005500 01  CHKP-VAR.                                                            
005600     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
005700     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
005800     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005900     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
006000     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
006100     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
006500     SKIP2                                                                
006600 01  ERROR-TEXT.                                                          
006700     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
006800     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006900                                                                          
007000 77  W571D1-EOF-SW               PIC X       VALUE 'N'.                   
007100     88  END-OF-W571D1                       VALUE 'Y'.                   
007110 77  INDATA-EOF-SW               PIC X       VALUE 'N'.                   
007120     88  END-OF-INDATA                       VALUE 'Y'.                   
007200 77  WS-PASS-SW                  PIC X       VALUE 'N'.                   
007300     88  WS-PASS                             VALUE 'Y'.                   
007400 77  WS-FIRST-TIME-SW            PIC X       VALUE 'N'.                   
007500     88  WS-FIRST-TIME                       VALUE 'Y'.                   
007600 77  WS-EXCL-PART-SW             PIC X       VALUE 'N'.                   
007700     88  WS-EXCL-PART                        VALUE 'Y'.                   
007710 77  WS-DONE-SW                  PIC X       VALUE 'N'.                   
007720     88  WS-DONE                             VALUE 'Y'.                   
007800     EJECT                                                                
007900 77  WS-CURRENT-DATE             PIC X(8)    VALUE SPACES.                
008000 77  WS-CURRENT-TIME             PIC 9(6)    VALUE ZERO.                  
008100 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
008200 01  FILLER REDEFINES TODAYS-DATE.                                        
008300     03  TODAYS-DATE-YEAR        PIC 9(2).                                
008400     03  TODAYS-DATE-MONTH       PIC 9(2).                                
008500     03  TODAYS-DATE-DAY         PIC 9(2).                                
008600     EJECT                                                                
008700 01  GENERAL-SUBPROGRAMS.                                                 
008800*                                                                         
008900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009200     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
009300     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
009400     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
009410     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009500     EJECT                                                                
009600*    --- PARAMETRAR TILL POSTSUM                                          
009700*                                                                         
009800*01  -COPY W0005   -PRE  POSTSUM-                                         
009900     EJECT                                                                
010000*01  -COPY WL01TIDZ                                                       
010100     EJECT                                                                
010200*    --- AREAS FOR COMMUNICATION                                          
010300 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
010400*01  -COPY WZ01SEND                                                       
010500     EJECT                                                                
011000                                                                          
011700 01  W571D1-AREA-START           PIC X(24)   VALUE                        
011800                                             'W571D1-AREA-START'.         
011900     SKIP2                                                                
012000                                                                          
012100*01  AREA -COPY W57101     -PRE W57101-                                   
012200     EJECT                                                                
012211 01  W571D2-AREA-START           PIC X(24)   VALUE                        
012212                                             'W571D2-AREA-START'.         
012220 01  W571D2-AREA.                                                         
012230     03 WS-IDDC                  PIC X(2)    VALUE SPACE.                 
012240     03 FILLER                   PIC X(78)   VALUE SPACE.                 
012250*                                                                         
012300 01  HDR-AREA.                                                            
012400*   03  -COPY WZ01REQU -PRE HDR-                                          
012500*   03  -COPY WZ04HDR                                                     
012600*                                                                         
012700 01  LINE-AREA                   PIC X(24)    VALUE 'LINE-AREA'.          
012800 01  DOC-LINE-AREA.                                                       
012900*    03 -COPY W5710201 -PRE LINE-                                         
013000     EJECT                                                                
014100                                                                          
014110 01  FILLER                      PIC X(16)  VALUE 'WDATAREA'.             
014120*01  -COPY WDATAREA                                                       
014130                                                                          
014200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014300     SKIP3                                                                
014400 01  KEYS-TILL-DLI.                                                       
014500     03  W-IDDC-B6-X.                                                     
014600         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
014810     03  W-WDGXKEY-X.                                                     
014820          05 W-IDHTYP            PIC X(4)    VALUE '5103'.                
014830          05 W-LOWVALUE          PIC X(26)   VALUE LOW-VALUE.             
014840     03  W-IDDC-5104-X.                                                   
014850         05  W-IDDC-5104         PIC X(2)    VALUE SPACE.                 
014900     03  W-WDJ701-X.                                                      
015000         05  W-WDJ701-IDDC       PIC X(2)    VALUE SPACE.                 
015100         05  W-WDJ701-IDARTNR    PIC S9(9)   VALUE ZERO   COMP-3.         
015600     SKIP2                                                                
015700*    --- STATUS-KOD FRÅN IMS                                              
015800 01  STATUS-WS                   PIC XX.                                  
015900     88  SEGMENT-FOUND                       VALUE '  '.                  
016000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
016100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
016200     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
016300     88  IMS-NOT-OK                          VALUE 'XD'.                  
016400     SKIP2                                                                
016500 01  GOOD-STATUSCODES.                                                    
016600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016700     SKIP3                                                                
016800 01  SSA1                        PIC X(64).                               
016900 01  SSA2                        PIC X(64).                               
017000     EJECT                                                                
017100*    --- IMS FUNCTION CODES                                               
017200*01  -COPY W0003                                                          
017300     EJECT                                                                
017400*    ---  DLI INPUT-OUTPUT AREA                                           
017500                                                                          
017600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
017700 01  DLI-IO-WDB601.                                                       
017800*    03  -COPY WDB601                                                     
017900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5104'.                    
018000 01  DLI-IO-WDGX5104.                                                     
018100*    03  -COPY WDGX5104                                                   
018200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ701'.                      
018300 01  DLI-IO-WDJ701.                                                       
018400*    03  -COPY WDJ701                                                     
018800                                                                          
018900     EJECT                                                                
019000 LINKAGE SECTION.                                                         
019100                                                                          
019200*01  -COPY W0009   -PRE MSG-                                              
019300*01  -COPY W0009   -PRE DAP-                                              
019400                                                                          
019500*01  -COPY W0008  -PRE WDB6-                                              
019600     05  FILLER                  PIC X.                                   
019700                                                                          
019800*01  -COPY W0008  -PRE 5104-                                              
019900     05  FILLER                  PIC X.                                   
020000                                                                          
020100*01  -COPY W0008  -PRE WDJ7-                                              
020200     05  FILLER                  PIC X.                                   
020500     EJECT                                                                
020600 PROCEDURE DIVISION  USING MSG-PCB DAP-PCB 5104-PCB WDJ7-PCB              
020700                                           WDB6-PCB.                      
020800 MAIN SECTION.                                                            
020900     ENTRY 'DLITCBL' USING MSG-PCB DAP-PCB 5104-PCB WDJ7-PCB              
021000                                           WDB6-PCB.                      
021100                                                                          
021800     PERFORM A-INIT                                                       
021900     PERFORM B-GET-DATE-TIME-FROM-WDB6                                    
022000     PERFORM C-GET-DATA-FROM-WDR2                                         
022200                                                                          
022400     PERFORM G-DELETE-DUMMY-RECORD                                        
022401     MOVE WS-RECV-IDDC  TO WS-IDDC                                        
022410     PERFORM S06-WRITE-W571D2                                             
022500                                                                          
022600     PERFORM Z-FINIT                                                      
022700                                                                          
022800     MOVE ZERO TO RETURN-CODE                                             
022900     GOBACK                                                               
023000     .                                                                    
023100     EJECT                                                                
023200 A-INIT SECTION.                                                          
023300     SKIP2                                                                
023400                                                                          
023500     PERFORM IMS-RESTART                                                  
023600                                                                          
023710     OPEN INPUT INDATA                                                    
023720     READ INDATA NEXT RECORD INTO INAREA                                  
023730       AT END                                                             
023731          SET END-OF-INDATA            TO TRUE                            
023740     END-READ                                                             
023750     CLOSE INDATA                                                         
023760                                                                          
023770     UNSTRING INAREA DELIMITED BY SPACE INTO WS-RECV-IDDC                 
023800                                                                          
023810     OPEN INPUT W571D1                                                    
023820     OPEN OUTPUT W571D2                                                   
023900     MOVE WS-RECV-IDDC                 TO W-IDDC-B6                       
024000                                          W-IDDC-5104                     
024100                                          W-WDJ701-IDDC                   
024200     MOVE FUNCTION CURRENT-DATE (1:8)  TO WS-CURRENT-DATE                 
024300     MOVE FUNCTION CURRENT-DATE (9:6)  TO WS-CURRENT-TIME                 
024400     MOVE IDPGM                        TO POSTSUM-PROGNAMN                
024500     .                                                                    
024600     EJECT                                                                
024700*-----------------------------------------------------------------        
024800*  GET THE LOCAL DATE AND TIME USING THE TIMEZONE VALUE PRESENT IN        
024900*  WDB6 DATABASE.                                                         
025000*-----------------------------------------------------------------        
025100 B-GET-DATE-TIME-FROM-WDB6 SECTION.                                       
025200                                                                          
025300     PERFORM IMS-GU-WDB601                                                
025400     IF SEGMENT-FOUND                                                     
025500        MOVE '011'                 TO MSGI-KDCALL                         
025600        MOVE DCS-IDTIDZON          TO MSGI-IDTIDZON                       
025600        MOVE DCS-IDDC              TO MSGI-IDDC                           
025700        MOVE WS-CURRENT-DATE(3:6)  TO MSGI-TILOKDAT                       
025800        MOVE WS-CURRENT-TIME(1:4)  TO MSGI-TILOKTID                       
025900        CALL WL01TIDZ   USING      MSGI-WL01TIDZ                          
026000                                                                          
026010        STRING WS-CURRENT-DATE(1:2)                                       
026020               MSGI-TILOKDAT DELIMITED BY SIZE                            
026030                                    INTO LINE-OUT-TIDATETIME(1:8)         
026033                                                                          
026040        STRING MSGI-TILOKTID WS-CURRENT-TIME(5:2)                         
026050               DELIMITED BY SIZE    INTO LINE-OUT-TIDATETIME(9:6)         
026500     END-IF                                                               
026600     .                                                                    
026700     EJECT                                                                
026800*-----------------------------------------------------------------        
026900*  READ THE WDR2 DATABASE TO DETERMINE THE TYPE OF INVENTORY PROCE        
027000*  A - SIMULATION ; D- DOWNLOAD                                           
027100*-----------------------------------------------------------------        
027200 C-GET-DATA-FROM-WDR2 SECTION.                                            
027300     PERFORM IMS-GU-WDGX5104                                              
027400     IF SEGMENT-FOUND                                                     
027500        EVALUATE 5104-KDACS                                               
027600          WHEN 'A'                                                        
027700            PERFORM CA-SIMULATION                                         
027800          WHEN 'D'                                                        
027900            PERFORM CB-DOWNLOAD                                           
027901            MOVE WS-RECV-IDDC TO W-IDDC-5104                              
027910            PERFORM IMS-GHU-WDGX5104                                      
027920            IF SEGMENT-FOUND                                              
028000               MOVE WS-CURRENT-DATE TO 5104-DASTADAT                      
028010               PERFORM IMS-REPL-WDGX5104                                  
028020            END-IF                                                        
028100        END-EVALUATE                                                      
028200     END-IF                                                               
028300     .                                                                    
028400     EJECT                                                                
028500*-----------------------------------------------------------------        
028600* SIMULATE THE INVENTORY SELECTION PROCESS                                
028700*-----------------------------------------------------------------        
028800 CA-SIMULATION SECTION.                                                   
028810     MOVE NOO  TO WS-DONE-SW                                              
028900     PERFORM S05-READ-W571D1                                              
029000     PERFORM UNTIL END-OF-W571D1                                          
029010        IF WS-DONE                                                        
029020          CONTINUE                                                        
029030        ELSE                                                              
029100          PERFORM D-SELECT-57101-DATA                                     
029210        END-IF                                                            
029220        PERFORM S05-READ-W571D1                                           
029300     END-PERFORM                                                          
029400     PERFORM E-CREATE-HEADER                                              
029500     PERFORM F-CREATE-LINE                                                
029600     .                                                                    
029700     EJECT                                                                
029800*-----------------------------------------------------------------        
029900* START THE DOWNLOAD PROCESS                                              
030000*-----------------------------------------------------------------        
030100 CB-DOWNLOAD SECTION.                                                     
030400*** DELETE ALL RECORDS FROM WDJ7 DATABASE FOR THE INPUT IDDC              
030500     PERFORM IMS-GHN-WDJ701                                               
030600     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                      
030700        IF ACS-IDDC = WS-RECV-IDDC                                        
030800           MOVE ACS-IDDC         TO W-WDJ701-IDDC                         
030900           MOVE ACS-IDARTNR      TO W-WDJ701-IDARTNR                      
031000           PERFORM IMS-DLET-WDJ701                                        
031100           ADD +1                TO CHKP-ANT                              
031200           IF CHKP-ANT > CHKP-MAX                                         
031300              PERFORM X-TAKE-CHECKPOINT                                   
031400           END-IF                                                         
031500        END-IF                                                            
031600        PERFORM IMS-GHN-WDJ701-IN-LOOP                                    
031700     END-PERFORM                                                          
031800*** CREATE A DUMMY RECORD FOR THE INPUT IDDC                              
031900     PERFORM CBA-WRITE-DUMMY-RECORD                                       
032000**  FOR EVERY PART FROM W57101 FILE, DO A SELECTION.                      
032110     PERFORM S05-READ-W571D1                                              
032200     PERFORM UNTIL END-OF-W571D1                                          
032210       IF WS-DONE                                                         
032220          CONTINUE                                                        
032230       ELSE                                                               
032300          PERFORM D-SELECT-57101-DATA                                     
032400          PERFORM CBB-DOWNLOAD-ACS                                        
032510       END-IF                                                             
032520       PERFORM S05-READ-W571D1                                            
032600     END-PERFORM                                                          
032710**  FOR ALL THE NON-EXCLUDED PARTS, CREATE A LIST                         
032800     PERFORM E-CREATE-HEADER                                              
032900     PERFORM F-CREATE-LINE                                                
033000     .                                                                    
033100     EJECT                                                                
033200*-----------------------------------------------------------------        
033300* INSERT A DUMMY RECORD IN WDJ7 DATABASE BEFORE STARTING THE              
033400* DOWNLOAD PROCESS.                                                       
033500*-----------------------------------------------------------------        
033600 CBA-WRITE-DUMMY-RECORD SECTION.                                          
033700                                                                          
033800     MOVE SPACE            TO ACS-WDJ701                                  
033900     MOVE WS-RECV-IDDC     TO ACS-IDDC                                    
034000     MOVE +999999999       TO ACS-IDARTNR                                 
034100     MOVE ZERO             TO ACS-PRAVCOST                                
034200                              ACS-KVLS                                    
034300                              ACS-ADLAGOMR                                
034400                              ACS-ADGANG                                  
034500                              ACS-ADPLATS                                 
034600                              ACS-TIORDREG                                
034700                              ACS-TIINVDAT                                
034800                              ACS-TIAVCOST                                
034900                              ACS-TIRETUR-BEORD                           
035000                              ACS-TISKROT-BEORD                           
035100                              ACS-KDPRODSL                                
035200                              ACS-KDPSLLOC                                
035300                              ACS-KVPCOUNT                                
035400                              ACS-KVRCOUNT                                
035410                              ACS-KVTCOUNT                                
035500                              ACS-TIREGDAT-PCOUNT                         
035600                              ACS-TIREGDAT-PCOUNT-REG                     
035700                              ACS-TIREGDAT-RCOUNT                         
035800                              ACS-TIREGDAT-RCOUNT-REG                     
035801                              ACS-TIREGDAT-TCOUNT                         
035810                              ACS-TIREGDAT-TCOUNT-REG                     
035900                              ACS-TIREGTID-PCOUNT                         
036000                              ACS-TIREGTID-PCOUNT-REG                     
036100                              ACS-TIREGTID-RCOUNT                         
036200                              ACS-TIREGTID-RCOUNT-REG                     
036210                              ACS-TIREGTID-TCOUNT                         
036220                              ACS-TIREGTID-TCOUNT-REG                     
036300     PERFORM IMS-ISRT-WDJ701                                              
036400     .                                                                    
036500*-----------------------------------------------------------------        
036600* DOWLOAD THE INVENTORY SELECTION INTO WD7 DATABASE                       
036700*-----------------------------------------------------------------        
036800 CBB-DOWNLOAD-ACS SECTION.                                                
036900*** INSERT RECORD INTO WDJ7  DATABASE THAT PASS FILTER FROM W57101        
037000     IF WS-PASS AND W57101-ACS-PRAVCOST > 0                               
037100        PERFORM CBBA-MOVE-TO-WDJ701                                       
037200        PERFORM IMS-ISRT-WDJ701                                           
037300        ADD +1             TO CHKP-ANT                                    
037400        IF CHKP-ANT > CHKP-MAX                                            
037500           PERFORM X-TAKE-CHECKPOINT                                      
037600        END-IF                                                            
037700     END-IF                                                               
038500     .                                                                    
038600*-----------------------------------------------------------------        
038700*  POPULATE THE WDJ7 DATABASE FIELDS                                      
038800*-----------------------------------------------------------------        
038900 CBBA-MOVE-TO-WDJ701 SECTION.                                             
039000     MOVE W57101-ACS-IDARTNR       TO ACS-IDARTNR                         
039100     MOVE W57101-ACS-IDDC          TO ACS-IDDC                            
039200     MOVE W57101-ACS-ADLAGOMR      TO ACS-ADLAGOMR                        
039300     MOVE W57101-ACS-ADGANG        TO ACS-ADGANG                          
039400     MOVE W57101-ACS-ADPLATS       TO ACS-ADPLATS                         
039500     MOVE W57101-ACS-BEART         TO ACS-BEART                           
039600     MOVE W57101-ACS-PRAVCOST      TO ACS-PRAVCOST                        
039700     MOVE W57101-ACS-KVLS          TO ACS-KVLS                            
039800     MOVE W57101-ACS-TIORDREG      TO ACS-TIORDREG                        
039900     MOVE W57101-ACS-TIINVDAT      TO ACS-TIINVDAT                        
040000     MOVE W57101-ACS-TIAVCOST      TO ACS-TIAVCOST                        
040100     MOVE W57101-ACS-TIRETUR-BEORD TO ACS-TIRETUR-BEORD                   
040200     MOVE W57101-ACS-TISKROT-BEORD TO ACS-TISKROT-BEORD                   
040300     MOVE W57101-ACS-KDPRODSL      TO ACS-KDPRODSL                        
040400     MOVE W57101-ACS-KDPSLLOC      TO ACS-KDPSLLOC                        
040500     MOVE W57101-ACS-KVPCOUNT      TO ACS-KVPCOUNT                        
040600     MOVE W57101-ACS-KVRCOUNT      TO ACS-KVRCOUNT                        
040610     MOVE 0                        TO ACS-KVTCOUNT                        
040700     MOVE W57101-ACS-IDUSER-PCOUNT TO ACS-IDUSER-PCOUNT                   
040800     MOVE W57101-ACS-IDUSER-PCOUNT-REG                                    
040900                                   TO ACS-IDUSER-PCOUNT-REG               
041000     MOVE W57101-ACS-IDUSER-RCOUNT TO ACS-IDUSER-RCOUNT                   
041100     MOVE W57101-ACS-IDUSER-RCOUNT-REG                                    
041200                                   TO ACS-IDUSER-RCOUNT-REG               
041300     MOVE W57101-ACS-TIREGDAT-PCOUNT                                      
041400                                   TO ACS-TIREGDAT-PCOUNT                 
041500     MOVE W57101-ACS-TIREGDAT-PCOUNT-REG                                  
041600                                   TO ACS-TIREGDAT-PCOUNT-REG             
041700     MOVE W57101-ACS-TIREGDAT-RCOUNT                                      
041800                                   TO ACS-TIREGDAT-RCOUNT                 
042100     MOVE W57101-ACS-TIREGTID-PCOUNT                                      
042200                                   TO ACS-TIREGTID-PCOUNT                 
042300     MOVE W57101-ACS-TIREGTID-PCOUNT-REG                                  
042400                                   TO ACS-TIREGTID-PCOUNT-REG             
042500     MOVE W57101-ACS-TIREGTID-RCOUNT                                      
042600                                   TO ACS-TIREGTID-RCOUNT                 
042700     MOVE W57101-ACS-TIREGTID-RCOUNT-REG                                  
042800                                   TO ACS-TIREGTID-RCOUNT-REG             
042810     MOVE 0                        TO ACS-TIREGTID-TCOUNT                 
042820                                      ACS-TIREGDAT-TCOUNT                 
042840                                      ACS-TIREGTID-TCOUNT-REG             
042841                                      ACS-TIREGDAT-TCOUNT-REG             
042850     MOVE 0                        TO ACS-IDACSNR-P                       
042860                                      ACS-IDACSNR-R                       
042870                                      ACS-IDACSNR-T                       
042880     MOVE NOO                      TO ACS-FLKLAR                          
042900     .                                                                    
051500*-----------------------------------------------------------------        
051600* SELECT THE PARTS FROM THE W57101 FILE THAT ARE TO BE INCLUDED OR        
051700* EXCLUDED FROM THE INVENTORY DOWNLOAD SELECTION LIST.                    
051800*-----------------------------------------------------------------        
051900 D-SELECT-57101-DATA SECTION.                                             
052000     MOVE NOO          TO WS-PASS-SW                                      
052100                          WS-EXCL-PART-SW                                 
052210     IF W57101-ACS-KVLS = 0                                               
052211*** SHOULDN'T INCLUDE PARTS WITH NO STOCKBALANCE                          
052220       ADD +1      TO WS-KVINVART-EXCL                                    
052230       SET WS-EXCL-PART   TO TRUE                                         
052250     ELSE                                                                 
052260       IF W57101-ACS-KVLS < 0                                             
052261       AND W57101-ACS-PRAVCOST NOT = 0                                    
052262*** CAN'T CALCULATE THE VALUE WHEN NEGATIVE BALANCE                       
052263*** BUT THEY SHOULD BE INVENTORED                                         
052270         ADD +1      TO WS-KVINVART-INC                                   
052280         SET WS-PASS TO TRUE                                              
052290       ELSE                                                               
052300         IF 5104-FLNOHAND = NOO                                           
052310** WHEN THE INVENTORY OF INACTIVE PARTS IS TO BE SKIPPED:                 
052400** IF THE LAST ACTIVITY DATE OF THE PARTS IS GREATER THAN THE             
052500** DATE OF THE LAST INVENTORY, INCLUDE THE PARTS IN THE LIST              
052510           MOVE 'AAVVD '      TO DAT-KDDATFORM                            
052511           MOVE W57101-ACS-TIINVDAT TO DAT-I-TIDATUM                      
052512           IF W57101-ACS-TIINVDAT > ZERO                                  
052520             CALL  WDATKONV  USING DAT-KDDATFORM                          
052530                                   DAT-I-TIDATUM                          
052540                                   DAT-O-TIDATUM                          
052550                                   DAT-KDSVAR                             
052560             IF DAT-KDSVAR-OK                                             
052570                 CONTINUE                                                 
052580             ELSE                                                         
052590                 CALL  FELLOG                                             
052591             END-IF                                                       
052592             MOVE DAT-TIAAMMDD TO WS-TIINVDAT                             
052593           ELSE                                                           
052594             MOVE ZERO         TO WS-TIINVDAT                             
052595           END-IF                                                         
052600           IF W57101-ACS-TIORDREG  > WS-TIINVDAT                          
052700           OR W57101-ACS-TIAVCOST  > WS-TIINVDAT                          
052800           OR W57101-ACS-TIRETUR-BEORD > WS-TIINVDAT                      
052900           OR W57101-ACS-TISKROT-BEORD > WS-TIINVDAT                      
053000** CALCULATE THE NUMBER OF PARTS THAT ARE TO BE INCLUDED                  
053101             IF W57101-ACS-PRAVCOST NOT = 0                               
053102             AND 5104-PRAVCOST < W57101-ACS-PRAVCOST                      
053110               COMPUTE WS-SUVALINV-INC = WS-SUVALINV-INC +                
053120                       (W57101-ACS-KVLS * W57101-ACS-PRAVCOST)            
053200               ADD +1  TO WS-KVINVART-INC                                 
053300               SET WS-PASS TO TRUE                                        
053400             ELSE                                                         
053500** CALCULATE THE VALUE OF THE PART                                        
053600** (THE STOCK BALANCE * THE AVERAGE COST OF THE PART)                     
053700               IF 5104-SUARTAVG <                                         
053800                  W57101-ACS-KVLS * W57101-ACS-PRAVCOST                   
053900                 COMPUTE WS-SUVALINV-INC = WS-SUVALINV-INC +              
054000                         (W57101-ACS-KVLS * W57101-ACS-PRAVCOST)          
054010                 ADD +1  TO WS-KVINVART-INC                               
054100                 SET WS-PASS TO TRUE                                      
054110               ELSE                                                       
054111                 COMPUTE WS-SUVALINV-EXCL = WS-SUVALINV-EXCL +            
054112                         (W57101-ACS-KVLS * W57101-ACS-PRAVCOST)          
054120                 ADD +1  TO WS-KVINVART-EXCL                              
054130                 SET WS-EXCL-PART TO TRUE                                 
054200               END-IF                                                     
054300             END-IF                                                       
054310           ELSE                                                           
054311             COMPUTE WS-SUVALINV-EXCL = WS-SUVALINV-EXCL +                
054312                     (W57101-ACS-KVLS * W57101-ACS-PRAVCOST)              
054320             ADD +1  TO WS-KVINVART-EXCL                                  
054330             SET WS-EXCL-PART TO TRUE                                     
054400           END-IF                                                         
054500         END-IF                                                           
054600                                                                          
054700** INVENTORY THE INACTIVE PARTS:                                          
054800         IF 5104-FLNOHAND = YES                                           
054900** CALCULATE THE NUMBER OF PARTS THAT ARE TO BE INCLUDED                  
055000           IF 5104-PRAVCOST < W57101-ACS-PRAVCOST                         
055010             COMPUTE WS-SUVALINV-INC = WS-SUVALINV-INC +                  
055020                     (W57101-ACS-KVLS * W57101-ACS-PRAVCOST)              
055100             ADD +1    TO WS-KVINVART-INC                                 
055200             SET WS-PASS TO TRUE                                          
055300           ELSE                                                           
055400**     CALCULATE THE VALUE OF THE PART                                    
055500             IF 5104-SUARTAVG <                                           
055510                W57101-ACS-KVLS * W57101-ACS-PRAVCOST                     
055600               COMPUTE WS-SUVALINV-INC = WS-SUVALINV-INC +                
055700                       (W57101-ACS-KVLS * W57101-ACS-PRAVCOST)            
055710               ADD +1    TO WS-KVINVART-INC                               
055800               SET WS-PASS TO TRUE                                        
055810             ELSE                                                         
055811               COMPUTE WS-SUVALINV-EXCL = WS-SUVALINV-EXCL +              
055812                       (W57101-ACS-KVLS * W57101-ACS-PRAVCOST)            
055820               ADD +1    TO WS-KVINVART-EXCL                              
055840               SET WS-EXCL-PART TO TRUE                                   
055900             END-IF                                                       
056000           END-IF                                                         
056100         END-IF                                                           
056110       END-IF                                                             
057200     END-IF                                                               
057700     .                                                                    
057800                                                                          
057900*-----------------------------------------------------------------        
058000* CREATE HEADER OF THE INVENTORY DOWNLOAD SELECTION LIST                  
058100*-----------------------------------------------------------------        
058200 E-CREATE-HEADER SECTION.                                                 
058300     MOVE 1                          TO HDR-REQU-IDMSGVER                 
058400     MOVE 'E'                        TO HDR-REQU-KDPGMACT                 
058500     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
058600                                                                          
058700     MOVE SPACE                      TO HDR-IDOUTREC                      
058800     MOVE WS-RECV-IDDC               TO HDR-IDOUTREC                      
058900     MOVE 'W57102-001'               TO HDR-IDOUTTYPE                     
059000     MOVE WS-CURRENT-DATE            TO HDR-IDLIST                        
059100                                                                          
059200     PERFORM S01-SEND-OPEN                                                
059300     MOVE SEND-IDCOM                 TO WZ04-001-IDCOM                    
059400*HDR                                                                      
059500     PERFORM S02-PUT-HEADER                                               
059600     .                                                                    
059700     SKIP3                                                                
059800*-----------------------------------------------------------------        
059900* CREATE REPORT LINES OF THE INVENTORY DOWNLOAD SELECTION LIST            
060000*-----------------------------------------------------------------        
060100 F-CREATE-LINE SECTION.                                                   
060200     MOVE WS-RECV-IDDC               TO LINE-OUT-IDDC                     
060300     MOVE 5104-PRAVCOST              TO LINE-OUT-PRAVCOST                 
060400     MOVE 5104-SUARTAVG              TO LINE-OUT-SUARTAVG                 
060410     IF 5104-FLNOHAND = YES                                               
060420        MOVE 'Y'                     TO LINE-OUT-FLNOHAND                 
060430     ELSE                                                                 
060500        MOVE 5104-FLNOHAND           TO LINE-OUT-FLNOHAND                 
060510     END-IF                                                               
060520     IF 5104-FLBLINDCO = YES                                              
060530        MOVE 'Y'                     TO LINE-OUT-FLBLINDCO                
060540     ELSE                                                                 
060550        MOVE 5104-FLBLINDCO          TO LINE-OUT-FLBLINDCO                
060560     END-IF                                                               
060570     MOVE WS-SUVALINV-INC            TO LINE-OUT-SUVALINV-INC             
060580     MOVE WS-SUVALINV-EXCL           TO LINE-OUT-SUVALINV-EXCL            
060590     MOVE WS-KVINVART-INC            TO LINE-OUT-KVINVART-INC             
060600     MOVE WS-KVINVART-EXCL           TO LINE-OUT-KVINVART-EXCL            
060700     MOVE 5104-PRAVCOST-DEV1         TO LINE-OUT-PRAVCOST-DEV1            
060710     COMPUTE LINE-OUT-REQTYDEV-1 = 5104-REQTYDEV-1 * 100                  
060900     MOVE 5104-PRAVCOST-DEV2         TO LINE-OUT-PRAVCOST-DEV2            
060910     COMPUTE LINE-OUT-REQTYDEV-2 = 5104-REQTYDEV-2 * 100                  
061100** CALCULATE THE TOTAL NUMBER OF PARTS                                    
061200     COMPUTE WS-KVINVART = WS-KVINVART-INC + WS-KVINVART-EXCL             
061400** CALCULATE THE TOTAL VALUE OF PARTS                                     
061500     COMPUTE WS-SUVALINV = WS-SUVALINV-INC + WS-SUVALINV-EXCL             
061800** CALCULATE THE PERCENTAGE OF INCLUDED PARTS                             
061810     IF WS-KVINVART = ZERO                                                
061811       MOVE ZERO TO WS-REQTYDEV-INC1                                      
061820     ELSE                                                                 
061900       COMPUTE WS-REQTYDEV-INC ROUNDED =                                  
062000               WS-KVINVART-INC / WS-KVINVART  * 100                       
062021                                                                          
062030       COMPUTE WS-REQTYDEV-INC1 ROUNDED = WS-REQTYDEV-INC                 
062100     END-IF                                                               
062200** CALCULATE THE PERCENTAGE OF EXCLUDED PARTS                             
062210     IF WS-KVINVART = ZERO                                                
062220       MOVE ZERO TO WS-REQTYDEV-EXCL1                                     
062230     ELSE                                                                 
062300       COMPUTE WS-REQTYDEV-EXCL ROUNDED =                                 
062400               WS-KVINVART-EXCL / WS-KVINVART * 100                       
062401                                                                          
062410       COMPUTE WS-REQTYDEV-EXCL1 ROUNDED = WS-REQTYDEV-EXCL               
062500     END-IF                                                               
062600** CALCULATE THE PERCENTAGE OF VALUE OF INCLUDED PARTS                    
062610     IF WS-SUVALINV = ZERO                                                
062620       MOVE ZERO TO WS-REVALDEV-INC1                                      
062630     ELSE                                                                 
062700       COMPUTE WS-REVALDEV-INC ROUNDED =                                  
062800               WS-SUVALINV-INC / WS-SUVALINV * 100                        
062801                                                                          
062810       COMPUTE WS-REVALDEV-INC1 ROUNDED = WS-REVALDEV-INC                 
062900     END-IF                                                               
063000** CALCULATE THE PERCENTAGE OF VALUE OF EXCLUDED PARTS                    
063010     IF WS-SUVALINV = ZERO                                                
063020       MOVE ZERO TO WS-REVALDEV-EXCL1                                     
063030     ELSE                                                                 
063100       COMPUTE WS-REVALDEV-EXCL ROUNDED =                                 
063200               WS-SUVALINV-EXCL / WS-SUVALINV * 100                       
063210       COMPUTE WS-REVALDEV-EXCL1 ROUNDED = WS-REVALDEV-EXCL               
063300     END-IF                                                               
063400     MOVE WS-KVINVART       TO LINE-OUT-KVINVART                          
063500     MOVE WS-SUVALINV       TO LINE-OUT-SUVALINV                          
063600     MOVE WS-REQTYDEV-INC1  TO LINE-OUT-REQTYDEV-INC                      
063700     MOVE WS-REQTYDEV-EXCL1 TO LINE-OUT-REQTYDEV-EXCL                     
063800     MOVE WS-REVALDEV-INC1  TO LINE-OUT-REVALDEV-INC                      
063803     MOVE WS-REVALDEV-EXCL1 TO LINE-OUT-REVALDEV-EXCL                     
064000                                                                          
064100     PERFORM S03-PUT-REPORT-LINE                                          
064110     MOVE WZ04-001-IDCOM                  TO SEND-IDCOM                   
064200     PERFORM S04-SEND-CLOSE                                               
064300     .                                                                    
064400     EJECT                                                                
064500 G-DELETE-DUMMY-RECORD SECTION.                                           
064600     MOVE WS-RECV-IDDC           TO W-WDJ701-IDDC                         
064700     MOVE +999999999             TO W-WDJ701-IDARTNR                      
064800     PERFORM IMS-GHU-WDJ701                                               
064900     IF SEGMENT-FOUND                                                     
065000        PERFORM IMS-DLET-WDJ701                                           
065100     END-IF                                                               
065200     .                                                                    
065300 Z-FINIT SECTION.                                                         
065400     CLOSE W571D1                                                         
065410           W571D2                                                         
065500     SKIP2                                                                
065600     MOVE 'S' TO POSTSUM-OPKOD                                            
065700     CALL POSTSUM USING POSTSUM-PARM                                      
065800     .                                                                    
065900     EJECT                                                                
066000*    --- DISPATCHER-SECTIONS                                              
070300 S01-SEND-OPEN SECTION.                                                   
070400     MOVE 'CARPARTS.DAP.DISTRDOC'         TO SEND-ADDISPABS               
070500     MOVE 'OPEN'                          TO SEND-KDFUNC                  
070600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
070700                         SEND-OPEN-AREA                                   
070800     IF SEND-KDRC > ZERO                                                  
070900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
071000       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
071100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
071200       DISPLAY ERROR-TEXT                                                 
071300       CALL FELLOG                                                        
071400     END-IF                                                               
071500                                                                          
071600     .                                                                    
071700     SKIP3                                                                
071800 S02-PUT-HEADER SECTION.                                                  
071900                                                                          
072000     MOVE 'PUT'                           TO SEND-KDFUNC                  
072100     MOVE WZ04-001-IDCOM                  TO SEND-IDCOM                   
072200     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
072300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
072400                         SEND-KVDLEN                                      
072500                         HDR-AREA                                         
072600     IF SEND-KDRC > ZERO                                                  
072700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
072800       STRING 'WZ01SEND PUT-HDR ERROR RC=' KDRC-DISPLAY                   
072900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
073000       DISPLAY ERROR-TEXT                                                 
073100       CALL FELLOG                                                        
073200     END-IF                                                               
073300     .                                                                    
073400     EJECT                                                                
075200 S03-PUT-REPORT-LINE    SECTION.                                          
075300                                                                          
075400     MOVE 'PUT'                           TO SEND-KDFUNC                  
075500     MOVE WZ04-001-IDCOM                  TO SEND-IDCOM                   
075600     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
075700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
075800                         SEND-KVDLEN                                      
075900                         DOC-LINE-AREA                                    
076000     IF SEND-KDRC > ZERO                                                  
076100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
076200       STRING 'WZ01SEND PUT-LINE ERROR RC=' KDRC-DISPLAY                  
076300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
076400       DISPLAY ERROR-TEXT                                                 
076500       CALL FELLOG                                                        
076600     END-IF                                                               
076700     MOVE 'W57102' TO POSTSUM-FDNAMN                                      
076800     MOVE 'DAP1' TO POSTSUM-DDNAMN2                                       
076900     MOVE 'LINE1'     TO POSTSUM-TRANSTYP                                 
077000     CALL POSTSUM USING POSTSUM-PARM                                      
077100     .                                                                    
077200     SKIP3                                                                
079400 S04-SEND-CLOSE SECTION.                                                  
079500                                                                          
079600     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
079800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
079900                                                                          
080000                                                                          
080100     IF SEND-KDRC > ZERO                                                  
080200       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
080300       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
080400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
080500       DISPLAY ERROR-TEXT                                                 
080600       CALL FELLOG                                                        
080700     END-IF                                                               
080800     .                                                                    
080900 S05-READ-W571D1  SECTION.                                                
081000     SKIP2                                                                
081100     READ W571D1 INTO W57101-AREA                                         
081200     AT END                                                               
081300        SET END-OF-W571D1 TO TRUE                                         
081400                                                                          
081500     NOT AT END                                                           
081510        IF W57101-ACS-IDDC NOT = WS-RECV-IDDC                             
081520           SET WS-DONE TO TRUE                                            
081521        ELSE                                                              
081522           MOVE 'N' TO WS-DONE-SW                                         
081530        END-IF                                                            
081600        MOVE 'W571D1'     TO POSTSUM-FDNAMN                               
081700        MOVE 'W57102D1'   TO POSTSUM-DDNAMN2                              
081800        MOVE 'IN  '       TO POSTSUM-TRANSTYP                             
081900        CALL POSTSUM USING POSTSUM-PARM                                   
082000     END-READ                                                             
082100     .                                                                    
082200     EJECT                                                                
082210 S06-WRITE-W571D2 SECTION.                                                
082220                                                                          
082230     WRITE W571D2-RECORD FROM W571D2-AREA                                 
082240                                                                          
082250     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
082260     MOVE 'W571D2'   TO POSTSUM-FDNAMN                                    
082270     MOVE 'W57102D2' TO POSTSUM-DDNAMN2                                   
082280     CALL POSTSUM USING POSTSUM-PARM                                      
082290     .                                                                    
082300 X-TAKE-CHECKPOINT   SECTION.                                             
082400                                                                          
082500     PERFORM IMS-CHECKPOINT                                               
082600     MOVE ZERO TO CHKP-ANT                                                
082700     .                                                                    
082800     EJECT                                                                
082900* --- IMS SECTIONS  ---                                                   
083000                                                                          
083100     EJECT                                                                
083200 IMS-GU-WDB601 SECTION.                                                   
083300                                                                          
083400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
083500          DELIMITED BY SIZE INTO SSA1                                     
083600     MOVE '  GE' TO GOOD-STATUSCODES                                      
083700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
083800     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
083900     PERFORM IMS-STATUSCHECK                                              
084000     .                                                                    
084100     EJECT                                                                
084200 IMS-GU-WDGX5104 SECTION.                                                 
084300                                                                          
084510     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
084520          DELIMITED BY SIZE INTO SSA1                                     
084530     STRING 'WDGX5104(IDDC     =' W-IDDC-5104-X ')'                       
084540          DELIMITED BY SIZE INTO SSA2                                     
084600     MOVE '  GE' TO GOOD-STATUSCODES                                      
084700     CALL CBLTDLI USING GU 5104-PCB DLI-IO-WDGX5104 SSA1 SSA2             
084800     MOVE 5104-STATUS-CODE TO STATUS-WS                                   
084900     PERFORM IMS-STATUSCHECK                                              
085000     .                                                                    
085100     EJECT                                                                
085200 IMS-GHN-WDJ701 SECTION.                                                  
085300                                                                          
085400     MOVE 'WDJ701  ' TO SSA1                                              
085500     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
085600     CALL CBLTDLI USING GHN WDJ7-PCB DLI-IO-WDJ701 SSA1                   
085700     MOVE WDJ7-STATUS-CODE TO STATUS-WS                                   
085800     PERFORM IMS-STATUSCHECK                                              
085900     .                                                                    
086000     SKIP3                                                                
086100 IMS-GHN-WDJ701-IN-LOOP SECTION.                                          
086200                                                                          
086300     STRING  'WDJ701  (WDJ701KY >' W-WDJ701-X ')'                         
086400              DELIMITED BY SIZE INTO SSA1                                 
086500     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
086600     CALL CBLTDLI USING GHN WDJ7-PCB DLI-IO-WDJ701 SSA1                   
086700     MOVE WDJ7-STATUS-CODE TO STATUS-WS                                   
086800     PERFORM IMS-STATUSCHECK                                              
086900     .                                                                    
087000     SKIP3                                                                
087100 IMS-GHU-WDJ701 SECTION.                                                  
087200                                                                          
087300     STRING  'WDJ701  (WDJ701KY =' W-WDJ701-X ')'                         
087400              DELIMITED BY SIZE INTO SSA1                                 
087500     MOVE '  GE'           TO GOOD-STATUSCODES                            
087600     CALL CBLTDLI USING GHU  WDJ7-PCB DLI-IO-WDJ701 SSA1                  
087700     MOVE WDJ7-STATUS-CODE TO STATUS-WS                                   
087800     PERFORM IMS-STATUSCHECK                                              
087900     .                                                                    
088000     SKIP3                                                                
088100 IMS-ISRT-WDJ701 SECTION.                                                 
088200                                                                          
088300     MOVE 'WDJ701 ' TO SSA1                                               
088400     MOVE '  II' TO GOOD-STATUSCODES                                      
088500     CALL CBLTDLI USING ISRT WDJ7-PCB DLI-IO-WDJ701 SSA1                  
088600     MOVE WDJ7-STATUS-CODE TO STATUS-WS                                   
088700     PERFORM IMS-STATUSCHECK                                              
088800     .                                                                    
088900     SKIP3                                                                
089000 IMS-DLET-WDJ701 SECTION.                                                 
089100                                                                          
089200     MOVE '  ' TO GOOD-STATUSCODES                                        
089300     CALL CBLTDLI USING DLET WDJ7-PCB DLI-IO-WDJ701                       
089400     MOVE WDJ7-STATUS-CODE TO STATUS-WS                                   
089500     PERFORM IMS-STATUSCHECK                                              
089600     .                                                                    
089700     EJECT                                                                
091910 IMS-GHU-WDGX5104 SECTION.                                                
091920                                                                          
091961     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
091962          DELIMITED BY SIZE INTO SSA1                                     
091963     STRING 'WDGX5104(IDDC     =' W-IDDC-5104-X ')'                       
091964          DELIMITED BY SIZE INTO SSA2                                     
091970     MOVE '  ' TO GOOD-STATUSCODES                                        
091980     CALL CBLTDLI USING GHU 5104-PCB DLI-IO-WDGX5104 SSA1 SSA2            
091990     MOVE 5104-STATUS-CODE    TO STATUS-WS                                
091991     PERFORM IMS-STATUSCHECK                                              
091992     .                                                                    
091993 IMS-REPL-WDGX5104 SECTION.                                               
091994                                                                          
091995     MOVE '  '                TO GOOD-STATUSCODES                         
091996     CALL CBLTDLI USING REPL 5104-PCB DLI-IO-WDGX5104                     
091997     MOVE 5104-STATUS-CODE    TO STATUS-WS                                
091998     PERFORM IMS-STATUSCHECK                                              
091999     .                                                                    
092000 IMS-RESTART SECTION.                                                     
092100     SKIP2                                                                
092200     MOVE SPACE           TO CHKP-MSG-IO-AREA                             
092300     MOVE '  '            TO GOOD-STATUSCODES                             
092400     CALL CBLTDLI USING XRST MSG-PCB                                      
092500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
092600                        CHKP-AREA-LENGTH CHKP-AREA                        
092700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
092800     PERFORM IMS-STATUSCHECK                                              
092900     .                                                                    
093000     SKIP3                                                                
093100 IMS-CHECKPOINT SECTION.                                                  
093200     SKIP2                                                                
093300     MOVE IDPGM           TO CHKP-MSG-IO-AREA                             
093400     MOVE WS-RECV-IDDC    TO CHKP-AREA                                    
093500     MOVE '  XD'          TO GOOD-STATUSCODES                             
093600     CALL CBLTDLI USING CHKP MSG-PCB                                      
093700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
093800                        CHKP-AREA-LENGTH CHKP-AREA                        
093900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
094000     PERFORM IMS-STATUSCHECK                                              
094100                                                                          
094200     IF IMS-NOT-OK                                                        
094300       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE' TO                     
094400                                   ERROR-TEXT-STR                         
094500       DISPLAY ERROR-TEXT                                                 
094600       CALL FELLOG                                                        
094700     END-IF                                                               
094800     .                                                                    
094900     EJECT                                                                
095000 IMS-STATUSCHECK SECTION.                                                 
095100     SKIP2                                                                
095200     SET STATUS-IX TO 1                                                   
095300     SEARCH GOOD-STATUS                                                   
095400       AT END                                                             
095500         STRING 'INVALID STATUS CODE FROM IMS: ' STATUS-WS                
095600           DELIMITED BY SIZE INTO ERROR-TEXT                              
095700         DISPLAY ERROR-TEXT                                               
095800         CALL FELLOG                                                      
095900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
096000         CONTINUE                                                         
096100     END-SEARCH                                                           
096200     .                                                                    
