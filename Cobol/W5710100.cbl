001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W5710100.                                                
001400 AUTHOR.         ARCHANA BHAT.                                            
001500 DATE-WRITTEN.   11/09/27.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNCTION:                                                            
002000*        THIS PROGRAM CREATES A FILE OF ALL THE DC'S WHICH HAVE           
002100*        OPTED FOR ACS INVENTORY                                          
002200*                                                                         
002300*        THE PROGRAM READS     WDB6                                       
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300*          --- W01184                                                     
003400     SELECT W01184                     ASSIGN TO W57101D1.                
003401     SKIP2                                                                
003402*          --- W01160                                                     
003410     SELECT W01160                     ASSIGN TO W57101D2.                
003411     SKIP2                                                                
003412*          --- W01174                                                     
003420     SELECT W01174                     ASSIGN TO W57101D3.                
003421     SKIP2                                                                
003422*          --- OUTPUT FILE WITH A LIST OF DC'S WITH ACS                   
003430     SELECT W57101                     ASSIGN TO W57101D4.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W01184                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  -COPY W01184 -PRE  W01184-  -L.                                      
004401                                                                          
004410 FD  W01160                                                               
004420     RECORDING       F                                                    
004430     BLOCK CONTAINS  0.                                                   
004440                                                                          
004450*01  -COPY W01160 -PRE  W01164-  -L.                                      
004460                                                                          
004470 FD  W01174                                                               
004480     RECORDING       F                                                    
004490     BLOCK CONTAINS  0.                                                   
004491                                                                          
004492*01  -COPY W01174 -PRE  W01174-  -L.                                      
004493                                                                          
004494 FD  W57101                                                               
004495     RECORDING       F                                                    
004496     BLOCK CONTAINS  0.                                                   
004497                                                                          
004498*01  POST -COPY W57101 -PRE  W57101-  -L.                                 
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800 77  IDPGM                       PIC X(8)    VALUE 'W5710100'.            
004900 77  YES                         PIC X       VALUE 'J'.                   
005000 77  NOO                         PIC X       VALUE 'N'.                   
005100     SKIP2                                                                
005200 01  ERROR-TEXT.                                                          
005300     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
005400     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005500                                                                          
005501 77  WS-IDARTNR                  PIC S9(9) COMP-3 VALUE ZERO.             
005502 77  WS-ACS-BEART                PIC X(25)   VALUE SPACE.                 
005503 77  WS-ACS-KDPRODSL             PIC S9(3) COMP-3 VALUE ZERO.             
005504 77  WS-ACS-KDPSLLOC             PIC 9(2)  COMP-3 VALUE ZERO.             
005505 77  WS-IDDC-CNT                 PIC 9(3)    VALUE ZERO.                  
005510 77  WS-DONE-SW                  PIC X       VALUE 'N'.                   
005520     88 WS-DONE                              VALUE 'Y'.                   
005530 77  WS-W01160-DONE-SW           PIC X       VALUE 'N'.                   
005540     88 WS-W01160-DONE                       VALUE 'Y'.                   
005550 77  WS-W01174-DONE-SW           PIC X       VALUE 'N'.                   
005560     88 WS-W01174-DONE                       VALUE 'Y'.                   
005600 77  W01184-EOF-SW               PIC X       VALUE 'N'.                   
005700     88  END-OF-W01184                       VALUE 'Y'.                   
005710 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
005720     88  END-OF-W01160                       VALUE 'Y'.                   
005730 77  W01174-EOF-SW               PIC X       VALUE 'N'.                   
005740     88  END-OF-W01174                       VALUE 'Y'.                   
005800     EJECT                                                                
005900 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006000 01  FILLER REDEFINES TODAYS-DATE.                                        
006100     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006200     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006300     03  TODAYS-DATE-DAY         PIC 9(2).                                
006400     EJECT                                                                
006500 01  GENERAL-SUBPROGRAMS.                                                 
006600*                                                                         
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL POSTSUM                                          
007200*                                                                         
007300*01  -COPY W0005   -PRE  POSTSUM-                                         
007400     EJECT                                                                
007500 01  W01184-AREA-START           PIC X(24)   VALUE                        
007600                                 'W01184-AREA-START'.                     
007900*01  AREA -COPY W01184     -PRE W01184-                                   
008000                                                                          
008010 01  W01160-AREA-START           PIC X(24)   VALUE                        
008020                                 'W01160-AREA-START'.                     
008030*01  AREA -COPY W01160     -PRE W01160-                                   
008040                                                                          
008050 01  W01174-AREA-START           PIC X(24)   VALUE                        
008060                                 'W01174-AREA-START'.                     
008070*01  AREA -COPY W01174     -PRE W01174-                                   
008090 01  W57101-AREA-START           PIC X(24)   VALUE                        
008091                                 'W57101-AREA-START'.                     
008092*01  AREA -COPY W57101     -PRE W57101-                                   
008100     EJECT                                                                
008200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008300     SKIP3                                                                
008400 01  KEYS-TO-DLI.                                                         
008710     03  W-IDARTNR-X.                                                     
008720         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
008730     03  W-KDSEGKEY-X.                                                    
008740         05  W-KDSEGKEY          PIC X(1)     VALUE '1'.                  
008750                                                                          
008800*    --- STATUS-KOD FROM IMS                                              
008900 01  STATUS-WS                   PIC XX.                                  
009000     88  SEGMENT-FOUND                      VALUE '  ' 'GA' 'GK'.         
009100     88  SEGMENT-FOUND-EXISTS               VALUE 'II'.                   
009200     88  SEGMENT-MISSING                    VALUE 'GE'.                   
009300     88  SEGMENT-NOMORE                     VALUE 'GB'.                   
009400     88  IMS-NOT-OK                         VALUE 'XD'.                   
009500     SKIP2                                                                
009600 01  GOOD-STATUSCODES.                                                    
009700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009800     SKIP3                                                                
009810 01  WDB601-TABLE.                                                        
009820     03  WS-IDDC-TBL OCCURS 1 TO 150 TIMES DEPENDING ON                   
009830                     WS-IDDC-CNT INDEXED BY WS-TBL-IX.                    
009831         05 WS-TBL-IDDC          PIC X(2).                                
009832         05 WS-TBL-FLINVACS      PIC X.                                   
009833         05 WS-TBL-AVCOST-DC     PIC X.                                   
009840*                                                                         
009900 01  SSA1                        PIC X(64).                               
010000 01  SSA2                        PIC X(64).                               
010100     EJECT                                                                
010200*    --- IMS FUNCTION CODES                                               
010300*01  -COPY W0003                                                          
010400     EJECT                                                                
010500*    ---  DLI INPUT-OUTPUT AREA                                           
010600                                                                          
010700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
010800 01  DLI-IO-WDB601.                                                       
010900*    03  -COPY WDB601                                                     
010910                                                                          
010920 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
010930 01  DLI-IO-WDK611.                                                       
010940*    03  -COPY WDK611                                                     
011000     EJECT                                                                
011100 LINKAGE SECTION.                                                         
011200                                                                          
011300*01  -COPY W0009   -PRE MSG-                                              
011310     05  FILLER                  PIC X.                                   
011400                                                                          
011500*01  -COPY W0008  -PRE WDB6-                                              
011600     05  FILLER                  PIC X.                                   
011610                                                                          
011620*01  -COPY W0008  -PRE WDK6-                                              
011630     05  FILLER                  PIC X.                                   
011700     EJECT                                                                
011800 PROCEDURE DIVISION  USING MSG-PCB WDB6-PCB WDK6-PCB.                     
011900 MAIN SECTION.                                                            
012000     ENTRY 'DLITCBL' USING MSG-PCB WDB6-PCB WDK6-PCB.                     
012100                                                                          
012200     SKIP2                                                                
012300     PERFORM A-INIT                                                       
012400     PERFORM S01-READ-W01184                                              
012410     PERFORM S02-READ-W01160                                              
012420     PERFORM S03-READ-W01174                                              
012430                                                                          
012440     PERFORM B-LOAD-IDDC-TBL                                              
012441       UNTIL WS-DONE                                                      
012460                                                                          
012500     PERFORM UNTIL END-OF-W01184                                          
012501       PERFORM C-CHECK-IDDC                                               
012502       PERFORM S01-READ-W01184                                            
013300     END-PERFORM                                                          
013400                                                                          
013500                                                                          
013600     PERFORM Z-FINIT                                                      
013700                                                                          
013800     MOVE ZERO TO RETURN-CODE                                             
013900     GOBACK                                                               
014000     .                                                                    
014100     EJECT                                                                
014200 A-INIT SECTION.                                                          
014300     SKIP2                                                                
014400                                                                          
014500     OPEN INPUT  W01184                                                   
014510                 W01160                                                   
014520                 W01174                                                   
014530     OPEN OUTPUT W57101                                                   
014600                                                                          
014700     SET WS-TBL-IX TO 1                                                   
014800     MOVE IDPGM      TO POSTSUM-PROGNAMN                                  
014900     .                                                                    
015000     EJECT                                                                
015001                                                                          
015002*-----------------------------------------------------------------        
015004* DO A SEQUENTIAL READ OF THE WDB601 DB UNTIL THE END OF DATABASE.        
015005* FOR EVERY SUCCESFUL READ, INCREMENT THE IDDC COUNTER. THIS WILL         
015006* USED TO DETERMINE THE SIZE OF THE WDB601 INTERNAL TABLE.                
015008* LOAD THE IDDC AND FLINVACS DATA FETCHED FROM WDB601 INTO AN             
015009* INTERNAL TABLE.                                                         
015010*-----------------------------------------------------------------        
015011 B-LOAD-IDDC-TBL SECTION.                                                 
015012                                                                          
015013     PERFORM IMS-GN-WDB601                                                
015014     IF SEGMENT-NOMORE OR SEGMENT-MISSING                                 
015015        SET WS-DONE        TO TRUE                                        
015016     ELSE                                                                 
015017        ADD +1             TO WS-IDDC-CNT                                 
015018        MOVE DCS-IDDC      TO WS-TBL-IDDC(WS-TBL-IX)                      
015019        MOVE DCS-FLINVACS  TO WS-TBL-FLINVACS(WS-TBL-IX)                  
015020        IF DCS-LAND-NON-VCC-OWNED OR DCS-USA OR DCS-CANADA                
015021          MOVE YES         TO WS-TBL-AVCOST-DC(WS-TBL-IX)                 
015022        ELSE                                                              
015023          MOVE NOO         TO WS-TBL-AVCOST-DC(WS-TBL-IX)                 
015024        END-IF                                                            
015025        SET  WS-TBL-IX UP BY 1                                            
015026     END-IF                                                               
015027     .                                                                    
015028     EJECT                                                                
015029                                                                          
015030*-----------------------------------------------------------------        
015031* FOR EACH IDDC OF W01184 FILE, CHECK IF IT IS PRESENT IN THE WDB6        
015032* INTERNAL TABLE. IF PRESENT AND THE FLINVACS FLAG IS SET TO YES,         
015033* PROCESS THE IDDC. IF NOT PRESENT IN THE TABLE, MOVE ON TO THE           
015034* NEXT IDDC.                                                              
015035*-----------------------------------------------------------------        
015036 C-CHECK-IDDC   SECTION.                                                  
015037     SET WS-TBL-IX TO 1                                                   
015038     SEARCH WS-IDDC-TBL                                                   
015039       AT END                                                             
015040         CONTINUE                                                         
015041       WHEN ((WS-TBL-IDDC(WS-TBL-IX) = W01184-SLAG-IDDC)                  
015042        AND WS-TBL-FLINVACS(WS-TBL-IX) = YES)                             
015043         PERFORM D-PROCESS-IDDC                                           
015044     END-SEARCH                                                           
015045     .                                                                    
015046     EJECT                                                                
015047                                                                          
015048*-----------------------------------------------------------------        
015049* FOR EVERY PART NUMBER OF A DC FROM THE W01184 FILE, READ THE            
015050* W01160 FILE TO FIND A MATCHING PART NUMBER. IF FOUND, READ THE          
015051* W01174 TO FIND A MATCHING PART NUMBER. IF FOUND,                        
015052* CREATE OUTPUT FILE IN W57101 LAYOUT. IF PART NUMBER IS NOT FOUND        
015053* ON W01160 AND W01174 FILES, BLANK OUT THE CORRESPONDING FIELDS.         
015054*-----------------------------------------------------------------        
015055 D-PROCESS-IDDC SECTION.                                                  
015056                                                                          
015057     MOVE NOO TO WS-W01160-DONE-SW                                        
015058                 WS-W01174-DONE-SW                                        
015059                                                                          
015060     IF W01184-SLAG-IDARTNR = WS-IDARTNR                                  
015061        MOVE WS-ACS-KDPRODSL   TO W57101-ACS-KDPRODSL                     
015062        MOVE WS-ACS-KDPSLLOC   TO W57101-ACS-KDPSLLOC                     
015063        MOVE WS-ACS-BEART      TO W57101-ACS-BEART                        
015064        PERFORM E-CREATE-OUTPUT                                           
015065     ELSE                                                                 
015066        PERFORM UNTIL END-OF-W01160 OR WS-W01160-DONE                     
015067          EVALUATE TRUE                                                   
015068           WHEN W01160-CLAG-IDARTNR = W01184-SLAG-IDARTNR                 
015069              MOVE W01184-SLAG-IDARTNR     TO WS-IDARTNR                  
015070              SET WS-W01160-DONE           TO TRUE                        
015071              MOVE W01160-CLAG-KDPRODSL    TO W57101-ACS-KDPRODSL         
015072                                              WS-ACS-KDPRODSL             
015073              MOVE W01160-CLAG-KDPSLLOC    TO W57101-ACS-KDPSLLOC         
015074                                              WS-ACS-KDPSLLOC             
015075              PERFORM UNTIL END-OF-W01174 OR WS-W01174-DONE               
015076                IF W01174-IDARTNR = W01184-SLAG-IDARTNR                   
015077                   MOVE W01174-BEART (10) TO W57101-ACS-BEART             
015078                                             WS-ACS-BEART                 
015079                   PERFORM E-CREATE-OUTPUT                                
015080                   SET WS-W01174-DONE     TO TRUE                         
015081                ELSE IF W01174-IDARTNR > W01184-SLAG-IDARTNR              
015082                   MOVE SPACES            TO W57101-ACS-BEART             
015083                                             WS-ACS-BEART                 
015084                   PERFORM E-CREATE-OUTPUT                                
015085                   SET WS-W01174-DONE     TO TRUE                         
015086                END-IF                                                    
015087                END-IF                                                    
015088                PERFORM S03-READ-W01174                                   
015089              END-PERFORM                                                 
015090                                                                          
015091           WHEN W01160-CLAG-IDARTNR > W01184-SLAG-IDARTNR                 
015092              SET WS-W01160-DONE           TO TRUE                        
015093              MOVE ZEROS                   TO W57101-ACS-KDPRODSL         
015094                                              W57101-ACS-KDPSLLOC         
015095                                              WS-ACS-KDPRODSL             
015096                                              WS-ACS-KDPSLLOC             
015097               PERFORM UNTIL END-OF-W01174 OR WS-W01174-DONE              
015098                  IF W01174-IDARTNR = W01184-SLAG-IDARTNR                 
015099                     MOVE W01174-BEART (10) TO W57101-ACS-BEART           
015100                                               WS-ACS-BEART               
015101                     PERFORM E-CREATE-OUTPUT                              
015102                     SET WS-W01174-DONE     TO TRUE                       
015103                  ELSE IF W01174-IDARTNR > W01184-SLAG-IDARTNR            
015104                     MOVE SPACES            TO W57101-ACS-BEART           
015105                                               WS-ACS-BEART               
015106                     PERFORM E-CREATE-OUTPUT                              
015107                     SET WS-W01174-DONE     TO TRUE                       
015108                  END-IF                                                  
015109                  END-IF                                                  
015110                  PERFORM S03-READ-W01174                                 
015111               END-PERFORM                                                
015112           END-EVALUATE                                                   
015113           PERFORM S02-READ-W01160                                        
015114        END-PERFORM                                                       
015115     END-IF                                                               
015116     .                                                                    
015117     EJECT                                                                
015118                                                                          
015119 E-CREATE-OUTPUT SECTION.                                                 
015120                                                                          
015121     MOVE W01184-SLAG-IDARTNR    TO W57101-ACS-IDARTNR                    
015122                                    W-IDARTNR                             
015123     MOVE W01184-SLAG-IDDC       TO W57101-ACS-IDDC                       
015124     MOVE W01184-SLAG-KVLS       TO W57101-ACS-KVLS                       
015125     IF WS-TBL-AVCOST-DC(WS-TBL-IX) = YES                                 
015126       MOVE W01184-SLAG-PRAVCOST TO W57101-ACS-PRAVCOST                   
015127       MOVE W01184-SLAG-TIAVCOST TO W57101-ACS-TIAVCOST                   
015128     ELSE                                                                 
015129       PERFORM IMS-GU-WDK611                                              
             IF SEGMENT-FOUND                                                   
015130        MOVE CLAG-PRARTSTD        TO W57101-ACS-PRAVCOST                  
015131        MOVE ZERO                 TO W57101-ACS-TIAVCOST                  
             ELSE                                                               
015130        MOVE ZERO                 TO W57101-ACS-PRAVCOST                  
015131                                     W57101-ACS-TIAVCOST                  
             END-IF                                                             
015132     END-IF                                                               
015133     MOVE W01184-SLAG-ADART      TO W57101-ACS-ADART                      
015134     MOVE W01184-SLAG-TIORDREG   TO W57101-ACS-TIORDREG                   
015135     MOVE W01184-SLAG-TIINVDAT   TO W57101-ACS-TIINVDAT                   
015136     MOVE W01184-SLAG-TIRETUR-BEORD                                       
015137                                 TO W57101-ACS-TIRETUR-BEORD              
015138     MOVE W01184-SLAG-TISKROT    TO W57101-ACS-TISKROT-BEORD              
015139     MOVE SPACE                  TO W57101-ACS-IDUSER-PCOUNT              
015140                                    W57101-ACS-IDUSER-RCOUNT              
015141                                    W57101-ACS-IDUSER-PCOUNT-REG          
015142                                    W57101-ACS-IDUSER-RCOUNT-REG          
015143     MOVE ZERO                   TO W57101-ACS-TIREGDAT-PCOUNT            
015144                                    W57101-ACS-TIREGDAT-PCOUNT-REG        
015145                                    W57101-ACS-TIREGDAT-RCOUNT            
015146                                    W57101-ACS-TIREGDAT-RCOUNT-REG        
015147                                    W57101-ACS-TIREGTID-PCOUNT            
015148                                    W57101-ACS-TIREGTID-PCOUNT-REG        
015149                                    W57101-ACS-TIREGTID-RCOUNT            
015150                                    W57101-ACS-TIREGTID-RCOUNT-REG        
015151                                    W57101-ACS-KVPCOUNT                   
015152                                    W57101-ACS-KVRCOUNT                   
015153                                                                          
015154     PERFORM S11-WRITE-W57101                                             
015155     .                                                                    
015156     EJECT                                                                
015160 Z-FINIT SECTION.                                                         
015200                                                                          
015300                                                                          
015400     CLOSE W01184                                                         
015410           W01160                                                         
015420           W01174                                                         
015430           W57101                                                         
015500     SKIP2                                                                
015600     MOVE 'S' TO POSTSUM-OPKOD                                            
015700     CALL POSTSUM USING POSTSUM-PARM                                      
015800     .                                                                    
015900     EJECT                                                                
016000 S01-READ-W01184  SECTION.                                                
016100     SKIP2                                                                
016200     READ W01184 INTO W01184-AREA                                         
016300     AT END                                                               
016400        MOVE +99999999     TO W01184-SLAG-IDARTNR                         
016500        SET END-OF-W01184  TO TRUE                                        
016600                                                                          
016700     NOT AT END                                                           
016800        MOVE 'W01184'      TO POSTSUM-FDNAMN                              
016900        MOVE 'W57101D1'    TO POSTSUM-DDNAMN2                             
017100        CALL POSTSUM USING POSTSUM-PARM                                   
017200     END-READ                                                             
017300     .                                                                    
017400     EJECT                                                                
017410 S02-READ-W01160  SECTION.                                                
017420     SKIP2                                                                
017430     READ W01160 INTO W01160-AREA                                         
017440     AT END                                                               
017450        MOVE +99999999     TO W01160-CLAG-IDARTNR                         
017460        SET END-OF-W01160  TO TRUE                                        
017470                                                                          
017480     NOT AT END                                                           
017490        MOVE 'W01160'      TO POSTSUM-FDNAMN                              
017491        MOVE 'W57101D1'    TO POSTSUM-DDNAMN2                             
017493        CALL POSTSUM USING POSTSUM-PARM                                   
017494     END-READ                                                             
017495     .                                                                    
017496     EJECT                                                                
017497 S03-READ-W01174  SECTION.                                                
017498     SKIP2                                                                
017499     READ W01174 INTO W01174-AREA                                         
017500     AT END                                                               
017501        MOVE +99999999     TO W01174-IDARTNR                              
017502        SET END-OF-W01174  TO TRUE                                        
017503                                                                          
017504     NOT AT END                                                           
017505        MOVE 'W01174'      TO POSTSUM-FDNAMN                              
017506        MOVE 'W57101D1'    TO POSTSUM-DDNAMN2                             
017508        CALL POSTSUM USING POSTSUM-PARM                                   
017509     END-READ                                                             
017510     .                                                                    
017511     EJECT                                                                
017512 S11-WRITE-W57101 SECTION.                                                
017513                                                                          
017514     WRITE W57101-POST FROM W57101-AREA                                   
017515                                                                          
017516     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
017517     MOVE 'W57101'   TO POSTSUM-FDNAMN                                    
017518     MOVE 'W57101D4' TO POSTSUM-DDNAMN2                                   
017519     CALL POSTSUM USING POSTSUM-PARM                                      
017520     .                                                                    
017521     EJECT                                                                
017522                                                                          
017530* --- IMS SECTIONS  ---                                                   
017600                                                                          
017700     EJECT                                                                
017800 IMS-GN-WDB601 SECTION.                                                   
017900                                                                          
018200     MOVE '  GEGBGAGK' TO GOOD-STATUSCODES                                
018300     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-WDB601                         
018400     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
018500     PERFORM IMS-STATUSCHECK                                              
018600     .                                                                    
018610     EJECT                                                                
018620 IMS-GU-WDK611   SECTION.                                                 
018630                                                                          
018640     STRING 'WDK601  (IDARTNR = ' W-IDARTNR-X ')'                         
018650          DELIMITED BY SIZE INTO SSA1                                     
018660     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
018670          DELIMITED BY SIZE INTO SSA2                                     
018680     MOVE '  GE' TO GOOD-STATUSCODES                                      
018690     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
018691     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
018692     PERFORM IMS-STATUSCHECK                                              
018693     .                                                                    
018700     EJECT                                                                
018800 IMS-STATUSCHECK SECTION.                                                 
018900     SKIP2                                                                
019000     SET STATUS-IX TO 1                                                   
019100     SEARCH GOOD-STATUS                                                   
019200       AT END                                                             
019300         STRING ' INVALID STATUS FROM IMS: ' STATUS-WS                    
019400           DELIMITED BY SIZE INTO ERROR-TEXT                              
019500         DISPLAY ERROR-TEXT                                               
019600         CALL FELLOG                                                      
019700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
019800         CONTINUE                                                         
019900     END-SEARCH                                                           
020000     .                                                                    
