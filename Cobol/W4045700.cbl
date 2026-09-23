000100**********************************************************                
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4045700.                                                
000400 AUTHOR.         MAMATHA SHETTY.                                          
000500 DATE-WRITTEN.   25/02/05.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        THE PROGRAM UPDATES   WDR2                                       
001000*        SHIPPING DOCUMENT PARAMETERS-FREIGHT AND INSURANCE COST          
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSACTION: W4T457 W4T457U                                      
001400*        MID:         W4I45701                                            
001500*                                                                         
001600*    OUTDATA.                                                             
001700*        MOD:         W4O45701                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W4045700'.            
002510 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
002520 77  IMS-SECTION                 PIC X(16)   VALUE SPACE.                 
002600                                                                          
002700 01  FILLER                  PIC X(16) VALUE   'KOLLA HÄR------:'.        
002900 01  WS-TEXT                 PIC X(40) VALUE                              
003000                       '----+----+----+----+----+----+----+----+'.        
003100*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003404 01  WS-RETRPFAC-AIR2           PIC 9(3)V9(2) VALUE ZERO.                 
003405 01  WS-RETRPFAC-ROAD2          PIC 9(3)V9(2) VALUE ZERO.                 
003406 01  WS-RETRPFAC-BOAT2          PIC 9(3)V9(2) VALUE ZERO.                 
003407 01  WS-RETRPFAC-AIR            PIC 9(1)V9(4) VALUE ZERO.                 
003408 01  WS-RETRPFAC-ROAD           PIC 9(1)V9(4) VALUE ZERO.                 
003409 01  WS-RETRPFAC-BOAT           PIC 9(1)V9(4) VALUE ZERO.                 
003704 01  WS-RETRPFAC-AIR1           PIC 9(3)V9(2) VALUE ZERO.                 
003706 01  WS-RETRPFAC-ROAD1          PIC 9(3)V9(2) VALUE ZERO.                 
003707 01  WS-RETRPFAC-BOAT1          PIC 9(3)V9(2) VALUE ZERO.                 
004404 77  WS-PRWEIGHT-AIR            PIC 9(5)   VALUE ZERO.                    
004405 77  WS-PRWEIGHT-BOAT           PIC 9(5)   VALUE ZERO.                    
004407 77  WS-PRWEIGHT-ROAD           PIC 9(5)   VALUE ZERO.                    
004408 77  WS-RULE-CNT1               PIC 9(2)   VALUE ZERO.                    
004409 77  WS-RULE-CNT2               PIC 9(2)   VALUE ZERO.                    
004410 77  WS-RULE-CNT3               PIC 9(2)   VALUE ZERO.                    
004411 77  WS-IDFRIDATA               PIC X(25).                                
004420 77  WS-IDDISTR                 PIC X(4)   VALUE SPACE.                   
004503 77  ERROR-TEXT                 PIC X(80) VALUE SPACE.                    
004504 77  WS-INT-LEN1                 PIC 9 VALUE ZERO.                        
004505 77  WS-DEC-LEN1                 PIC 9 VALUE ZERO.                        
004506 77  WS-INT-LEN2                 PIC 9 VALUE ZERO.                        
004507 77  WS-DEC-LEN2                 PIC 9 VALUE ZERO.                        
004508 77  WS-INT-LEN3                 PIC 9 VALUE ZERO.                        
004509 77  WS-DEC-LEN3                 PIC 9 VALUE ZERO.                        
004510 77  WS-INT1                     PIC X(1) VALUE SPACE.                    
004511 77  WS-DEC1                     PIC X(5) VALUE SPACE.                    
004512 77  WS-INT2                     PIC X(1) VALUE SPACE.                    
004513 77  WS-DEC2                     PIC X(5) VALUE SPACE.                    
004514 77  WS-INT3                     PIC X(1) VALUE SPACE.                    
004515 77  WS-DEC3                     PIC X(5) VALUE SPACE.                    
004516 77  WS-PF7                      PIC X(1) VALUE '7'.                      
004517 77  WS-PF9                      PIC X(1) VALUE '9'.                      
004518 77  POS                         PIC S9(4)   VALUE ZERO COMP-3.           
004519 77  INTEGERS                    PIC S9(4)   VALUE ZERO COMP-3.           
004520 77  DECIMALS                    PIC S9(4)   VALUE ZERO COMP-3.           
004603                                                                          
004703 01 WS-REINSFAC-AIRA     PIC 9(1).9(5) VALUE ZERO.                        
004704 01 WS-REINSFAC-AIR1     PIC X(7).                                        
004803 01 WS-REINSFAC-AIR-N REDEFINES WS-REINSFAC-AIR1                          
004903                         PIC 9(1).9(5).                                   
004904 01 WS-REINSFAC-BOAT1    PIC X(7).                                        
004905 01 WS-REINSFAC-BOAT-N REDEFINES WS-REINSFAC-BOAT1                        
004906                         PIC 9(1).9(5).                                   
004907 01 WS-REINSFAC-ROAD1    PIC X(7).                                        
004908 01 WS-REINSFAC-ROAD-N REDEFINES WS-REINSFAC-ROAD1                        
004909                         PIC 9(1).9(5).                                   
004910 77  YES                         PIC X       VALUE 'J'.                   
004911 77  NOO                         PIC X       VALUE 'N'.                   
004920                                                                          
005003*    --- INDEX FOR SCROLL LINES                                           
005103 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005203 77  MAX-INDX                    PIC S9(4)  VALUE +3    COMP SYNC.        
005303*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005803 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005903     88  INDATA-OK                           VALUE 'J'.                   
006003     88  INDATA-WRONG                        VALUE 'N'.                   
006103                                                                          
006203 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006303     88  KEYS-OK                             VALUE 'J'.                   
006403     88  KEYS-WRONG                          VALUE 'N'.                   
006503                                                                          
006903 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007003     88  OWN-MID                             VALUE '4457'.                
007103     88  HELP-MID                            VALUE '0551'.                
007106                                                                          
007203     EJECT                                                                
007303*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007403 01  GENERAL-SUBPROGRAMS.                                                 
007503     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007603     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007703     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007803     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007903     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
008003     EJECT                                                                
008103 01  WS-CHANGE-INDX              PIC S9      VALUE ZERO.                  
008203*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
008303*01 -COPY WMEDAREA                                                        
008403     SKIP3                                                                
008503 01  MESSAGE-CODES.                                                       
008603     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008703     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008803     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008903     03  KEYS-ARE-MISSING        PIC X(3)    VALUE '005'.                 
009003     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009103     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009203     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
009303     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009403     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
009405     03  ERR-MORE-THAN-ONE-RULE  PIC X(3)    VALUE '238'.                 
009406     03  ERR-DC-MISSING          PIC X(3)    VALUE '026'.                 
009503     EJECT                                                                
009603*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
009703*                                                                         
009803 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009903     SKIP3                                                                
010003*01 -COPY WMSGINIT                                                        
010103     EJECT                                                                
010203*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
010303*                                                                         
010403 01  WS-WDR201KY-UPD.                                                     
010503     03 WS-IDDC-UPD              PIC X(2)    VALUE SPACE.                 
010603     03 WS-IDDISTR-UPD           PIC S9(5)   VALUE ZERO  COMP-3.          
010604                                                                          
010803 01  SAVE-AREA.                                                           
010903     03 SAVE-IDTRANS             PIC X(4)    VALUE '4457'.                
011003     03 SAVE-IDDC-ENTER          PIC X(2).                                
011004     03 SAVE-IDDC-NEXT           PIC X(2).                                
011005     03 SAVE-IDDISTR-ENTER       PIC S9(5)        COMP-3.                 
011103     03 SAVE-IDDISTR-NEXT        PIC S9(5)        COMP-3.                 
011304     03 SAVE-ROWS.                                                        
011305        05 SAVE-ROW OCCURS 3.                                             
011306           07 SAVE-IDDC          PIC X(2).                                
011307           07 SAVE-IDDISTR       PIC S9(5)        COMP-3.                 
011403     EJECT                                                                
011503*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
011603*                                                                         
011703 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011803     SKIP3                                                                
011903*01  MID -COPY W4I45701                                                   
012003     EJECT                                                                
012103 01  FILLER                      PIC X(16)   VALUE 'MOD-AREA'.            
012203     SKIP3                                                                
012303     EJECT                                                                
012403 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012503     SKIP3                                                                
012504* - - - - - - - - - - - - - - - - - - -  MESSAGES                         
012505 01  FILLER                      PIC X(16)   VALUE 'MESSAGES   '.         
012506 01  MISSING-DC-MSG.                                                      
012507      03  FILLER                 PIC X(40)                                
012508          VALUE '   DC MISSING. PRESS PF9 TO ADD      '.                  
012509 01  INVALID-DC-ISRT-MSG.                                                 
012510      03  FILLER                 PIC X(40)                                
012511          VALUE '   PLEASE ADD A VALID DC             '.                  
012512 01  DC-PRESENT-MSG.                                                      
012513      03  FILLER                 PIC X(40)                                
012514          VALUE '   DC ALREADY EXISTS!!               '.                  
012515 01  DC-INSERTED-MSG.                                                     
012516      03  FILLER                 PIC X(40)                                
012517          VALUE '   DC INSERTED SUCCESSFULLY!         '.                  
012520     SKIP2                                                                
012604*    --- PARAMETERS FOR SUB PROGRAM WDECEDIT                              
012704*01  -COPY WDECAREA                                                       
013004*01 -COPY WWDCKONS                                                        
013104*01  -COPY WDATAREA                                                       
013304     EJECT                                                                
013404*01  -COPY WMSGAREA                                                       
013504     EJECT                                                                
013604     03  MOD REDEFINES MSG-AREA.                                          
013704*      05  -COPY W4O45701                                                 
013804     EJECT                                                                
013904 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014004     SKIP3                                                                
014104*01  -COPY WMFSAREA                                                       
014204     EJECT                                                                
014304*    --- WORK-AREAS FOR IMS-SECTIONS                                      
014404*                                                                         
014504 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014604     SKIP3                                                                
014704 01  KEYS-FOR-DLI.                                                        
014804*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
014904                                                                          
015004     03  W-WDGXKEY-X.                                                     
015104         05  W-IDHTYP            PIC X(4)    VALUE '4591'.                
015204         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
015304         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
015404                                                                          
015504     03  W-IDDISTR-MIN-X.                                                 
015604         05  W-IDDISTR-MIN       PIC S9(5)     COMP-3.                    
015704                                                                          
015804     03  W-IDDISTR-MAX-X.                                                 
015904         05  W-IDDISTR-MAX       PIC S9(5)    COMP-3.                     
016004                                                                          
016104     03  W-IDDC-X.                                                        
016204         05  W-IDDC-B6-X         PIC X(2)    VALUE SPACE.                 
016304     03  W-IDDISTR-X.                                                     
016404         05  W-IDDISTR           PIC S9(5) COMP-3 VALUE ZERO.             
016504     SKIP2                                                                
016604*    --- STATUS CODES FROM IMS                                            
016704 01  STATUS-WS                   PIC XX.                                  
016804     88  SEGMENT-FOUND                       VALUE '  '.                  
016904     88  SEGMENT-EXISTS                      VALUE 'II'.                  
017004     88  SEGMENT-MISSING                     VALUE 'GE'.                  
017104     88  END-OF-DATA                         VALUE 'GB'.                  
017204     SKIP2                                                                
017304 01  GOOD-STATUSCODES.                                                    
017404     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017504     SKIP3                                                                
017604 01  SSA1                        PIC X(64).                               
017704 01  SSA2                        PIC X(64).                               
017804 01  SSA-WDB6                    PIC X(64).                               
017904     EJECT                                                                
018004*    --- IMS FUNCTION CODES                                               
018104*01  -COPY W0003                                                          
018204     EJECT                                                                
018304*    ---  DLI INPUT-OUTPUT AREA                                           
018404                                                                          
018504 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
018604 01  DLI-IO-WDGX01.                                                       
018704*    03  -COPY WDGX01DC -PRE WDR2-                                        
018804     EJECT                                                                
018904 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4592'.                    
019004 01  DLI-IO-WDGX4592.                                                     
019104*    03  -COPY WDGX4592                                                   
019204 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
019304 01  DLI-IO-WDB601.                                                       
019404*    03  -COPY WDB601                                                     
019504     EJECT                                                                
019604 LINKAGE SECTION.                                                         
019704*01  -COPY W0009  -PRE MSG-                                               
019804                                                                          
019904*01  -COPY W0008  -PRE WDP7-                                              
020004     05  FILLER                  PIC X.                                   
020104*01  -COPY W0008  -PRE 4591-                                              
020204     05  FILLER                  PIC X.                                   
020304*01  -COPY W0008  -PRE WDB6-                                              
020404     05  FILLER                  PIC X.                                   
020504     EJECT                                                                
020604 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB 4591-PCB WDB6-PCB.            
020704 MAIN SECTION.                                                            
020804                                                                          
020904     PERFORM IMS-GET-MSG                                                  
021004     IF SEGMENT-FOUND                                                     
021005                                                                          
021104       PERFORM A-INIT                                                     
021204       PERFORM B-CHECK-KEYS                                               
021304       IF KEYS-OK                                                         
021404         IF MFS-UPDATE                                                    
021504           PERFORM G-CHECK-INPUT                                          
021604           IF INDATA-OK                                                   
021705             PERFORM H-UPDATE                                             
021707           END-IF                                                         
021708         ELSE                                                             
021709           IF MFS-IDPFK = WS-PF9                                          
021710             PERFORM I-INSERT-DC                                          
021720           ELSE                                                           
022004             IF MFS-FIRST                                                 
022104               PERFORM C-FIRST-PAGE                                       
022204             ELSE                                                         
022304               IF MFS-NEXT                                                
022404                 PERFORM D-NEXT-PAGE                                      
022504               ELSE                                                       
022606                   PERFORM E-SAME-PAGE                                    
022705               END-IF                                                     
022804             END-IF                                                       
022805           END-IF                                                         
022904         END-IF                                                           
023004         PERFORM F-READ-SHOW-INFO                                         
023104       END-IF                                                             
023204       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O45701 + 4                      
023304       PERFORM IMS-INSERT-MSG                                             
023404     END-IF                                                               
023504                                                                          
023604     MOVE ZERO TO RETURN-CODE                                             
023704     GOBACK                                                               
023804     .                                                                    
023904     EJECT                                                                
024004 A-INIT SECTION.                                                          
024105     MOVE 'A-INIT          '     TO CURRENT-SECTION                       
024106                                                                          
024204     IF MSG-DOUBLE-TRANSACTIONS                                           
024304       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I45701                 
024404       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
024504       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
024604     ELSE                                                                 
024704       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I45701                  
024804       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
024904       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
025004     END-IF                                                               
025104                                                                          
025204     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
025304     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
025404     MOVE MFS-IDTRANS TO W-IDTRANS                                        
025504                                                                          
025604     MOVE LOW-VALUE TO MSG-AREA                                           
025704     MOVE 'W4O457N1' TO MFS-IDMOD                                         
025804     MOVE '4457' TO MOD-IDTRANS                                           
025904     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
026004                                                                          
026104     IF OWN-MID OR HELP-MID                                               
026204       CONTINUE                                                           
026304     ELSE                                                                 
026404       MOVE SPACE TO MFS-KDTRTYP                                          
026504       MOVE WS-PF7    TO MFS-IDPFK                                        
026604     END-IF                                                               
026704     .                                                                    
026804     EJECT                                                                
026904 B-CHECK-KEYS SECTION.                                                    
027005     MOVE 'B-CHECK-KEYS    '     TO CURRENT-SECTION                       
027006                                                                          
027104     MOVE ALL '+'           TO MSGI-WMSGINIT                              
027204     MOVE '001'             TO MSGI-KDCALL                                
027304     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
027404     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
027504     MOVE '4457'            TO MSGI-IDTRANS                               
027604     IF OWN-MID                                                           
027704         MOVE MID-IDDC-IN        TO MSGI-IDDC-KEY                         
027804         MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                          
027904     END-IF                                                               
028004     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
028104     MOVE MSGI-SPAR-AREA TO SAVE-AREA                                     
028204                                                                          
028304*    - LANGUAGE TO BE USED BY MEDKONV                                     
028404     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
028504                                                                          
028604     MOVE YES TO KEYS-SW                                                  
028709                                                                          
028804*    -- CHECK OF IDDC                                                     
028904     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
029004                                                                          
029104     IF MID-IDDC-IN NOT = ALL '+'                                         
029204       MOVE WS-PF7      TO MFS-IDPFK                                      
029304       MOVE SPACE       TO MFS-KDTRTYP                                    
029305     ELSE                                                                 
029306     CONTINUE                                                             
029703     END-IF                                                               
029704     MOVE MSGI-IDDC-KEY TO MOD-IDDC-UT                                    
029705                           W-IDDC-B6-X                                    
029706                           W-IDDC                                         
029707     PERFORM IMS-GU-WDB601                                                
029708     IF SEGMENT-FOUND                                                     
029710         PERFORM BA-CHECK-DC-ROOT                                         
029720     ELSE                                                                 
029730        MOVE NOO     TO KEYS-SW                                           
029740        MOVE ERR-WRONG-KEY     TO MED-IDMFSINF                            
029750        CALL WMEDKONV     USING   MED-WMEDAREA                            
029760        MOVE MED-MFSINF     TO    MOD-TEMFSFEL                            
029770        PERFORM MFS-ERASE-FIELD-IN                                        
030404     END-IF                                                               
030504                                                                          
030604*    -- CHECK OF IDDISTR                                                  
030605     IF KEYS-OK                                                           
030704        MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-IN                            
030804                                                                          
030805        IF MID-IDDISTR-IN = ALL '+'                                       
030806           MOVE YES                  TO KEYS-SW                           
030807           MOVE SPACES               TO MOD-IDDISTR-UT                    
030808           MOVE LOW-VALUE            TO W-IDDISTR-MIN-X                   
030809           MOVE HIGH-VALUE           TO W-IDDISTR-MAX-X                   
030810        ELSE                                                              
030820          MOVE MID-IDDISTR-IN   TO WS-IDDISTR                             
030830                                   MOD-IDDISTR-UT                         
030840          MOVE WS-PF7      TO MFS-IDPFK                                   
030850          MOVE SPACE       TO MFS-KDTRTYP                                 
030860          INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO              
030870          IF WS-IDDISTR IS NUMERIC                                        
030880              MOVE YES                  TO KEYS-SW                        
030890              MOVE WS-IDDISTR           TO W-IDDISTR-MIN                  
030900                                           W-IDDISTR-MAX                  
031000          ELSE                                                            
031100              MOVE NOO    TO KEYS-SW                                      
031110              MOVE ERR-WRONG-KEY   TO   MED-IDMFSINF                      
031120              CALL WMEDKONV     USING   MED-WMEDAREA                      
031130              MOVE MED-MFSINF     TO    MOD-TEMFSFEL                      
031140              PERFORM MFS-ERASE-FIELD-IN                                  
031200          END-IF                                                          
031300        END-IF                                                            
031310     END-IF                                                               
031400                                                                          
033700*    IF KEYS-WRONG                                                        
033800*      MOVE ERR-WRONG-KEY   TO   MED-IDMFSINF                             
033900*      CALL WMEDKONV     USING   MED-WMEDAREA                             
034000*      MOVE MED-MFSINF     TO    MOD-TEMFSFEL                             
034100*      PERFORM MFS-ERASE-FIELD-IN                                         
034200*    END-IF                                                               
034300     .                                                                    
034400     EJECT                                                                
034410*WHEN USER GIVES DC INPUT AND ENTER, CHECK IN WDR2                        
034420*IF DC MISSING, USER PRESS PF9 TO INSERT DC.SO THE SEGMENT MISSING        
034430*IS SKIPPED FOR PF9                                                       
034500 BA-CHECK-DC-ROOT SECTION.                                                
034610     MOVE 'BA-CHECK-DC-ROOT'     TO CURRENT-SECTION                       
034620                                                                          
034621     IF DCS-NDC                                                           
034622        PERFORM IMS-GU-WDR201                                             
034623        IF SEGMENT-MISSING                                                
034624          IF MFS-IDPFK = WS-PF9                                           
034625             CONTINUE                                                     
034626          ELSE                                                            
034627             MOVE NOO     TO KEYS-SW                                      
034628             MOVE MISSING-DC-MSG       TO MOD-TEMFSFEL                    
034629             PERFORM MFS-DONT-TOUCH-FIELD-IN                              
034630          END-IF                                                          
034631        ELSE                                                              
034632          IF SEGMENT-FOUND                                                
034633             MOVE YES     TO KEYS-SW                                      
034634          END-IF                                                          
034635        END-IF                                                            
034636     ELSE                                                                 
034637        MOVE NOO     TO KEYS-SW                                           
034638        MOVE ERR-WRONG-KEY     TO MED-IDMFSINF                            
034639        CALL WMEDKONV     USING   MED-WMEDAREA                            
034640        MOVE MED-MFSINF     TO    MOD-TEMFSFEL                            
034641        PERFORM MFS-ERASE-FIELD-IN                                        
034642     END-IF                                                               
034643     .                                                                    
034644     EJECT                                                                
034650 C-FIRST-PAGE SECTION.                                                    
034660     MOVE 'C-FIRST-PAGE    '     TO CURRENT-SECTION                       
034670                                                                          
034700     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
034800     CALL WMEDKONV USING MED-WMEDAREA                                     
034900     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
035000                                                                          
035110     PERFORM MFS-ERASE-FIELD-IN                                           
035200     .                                                                    
035300     EJECT                                                                
035400 D-NEXT-PAGE SECTION.                                                     
035500     MOVE 'D-NEXT-PAGE     '     TO CURRENT-SECTION                       
035600                                                                          
035800     IF SAVE-IDTRANS = '4457'                                             
035900       MOVE SAVE-IDDISTR-NEXT TO W-IDDISTR-MIN                            
036200     ELSE                                                                 
036300       PERFORM MFS-ERASE-FIELD-IN                                         
036400     END-IF                                                               
036500     .                                                                    
036600     EJECT                                                                
036700 E-SAME-PAGE SECTION.                                                     
036710     MOVE 'E-SAME-PAGE     '     TO CURRENT-SECTION                       
036720                                                                          
036800     MOVE +1 TO INDX                                                      
036810                                                                          
036900     PERFORM VARYING INDX FROM +1 BY +1 UNTIL INDX > MAX-INDX             
037000      IF MID-KDCMD (INDX) = 'N'                                           
037101        MOVE MOD-IDDISTR(INDX) TO MOD-IDDISTR-UPD                         
037120        MOVE MOD-RETRPFAC-AIR(INDX) TO MOD-RETRPFAC-AIR-UPD               
037130        MOVE MOD-PRWEIGHT-AIR(INDX) TO MOD-PRWEIGHT-AIR-UPD               
037131        MOVE MOD-PRHAZMAT-AIR(INDX)TO MOD-PRHAZMAT-AIR-UPD                
037140        MOVE MOD-RETRPFAC-BOAT(INDX)TO MOD-RETRPFAC-BOAT-UPD              
037150        MOVE MOD-PRWEIGHT-BOAT(INDX)TO MOD-PRWEIGHT-BOAT-UPD              
037151        MOVE MOD-PRHAZMAT-BOAT(INDX)TO MOD-PRHAZMAT-BOAT-UPD              
037160        MOVE MOD-RETRPFAC-ROAD(INDX)TO MOD-RETRPFAC-ROAD-UPD              
037170        MOVE MOD-PRWEIGHT-ROAD(INDX)TO MOD-PRWEIGHT-ROAD-UPD              
037191        MOVE MOD-PRHAZMAT-ROAD(INDX)TO MOD-PRHAZMAT-ROAD-UPD              
037192        MOVE MOD-REINSFAC-AIR(INDX) TO MOD-REINSFAC-AIR-UPD2              
037193        MOVE MOD-REINSFAC-BOAT(INDX)TO MOD-REINSFAC-BOAT-UPD2             
037194        MOVE MOD-REINSFAC-ROAD(INDX)TO MOD-REINSFAC-ROAD-UPD2             
037195      END-IF                                                              
037196     END-PERFORM                                                          
037197                                                                          
037200     IF SAVE-IDTRANS = '4457'                                             
047063       MOVE SAVE-IDDISTR-ENTER TO W-IDDISTR-MIN                           
047065       IF MID-INPUT = ALL '+' AND                                         
047066          MID-UPD  = ALL '+'                                              
047067          PERFORM MFS-ERASE-FIELD-IN                                      
047069       ELSE                                                               
047070          MOVE INF-PRESS-PF11 TO MED-IDMFSINF                             
047071          CALL WMEDKONV USING MED-WMEDAREA                                
047072          MOVE MED-MFSINF TO MOD-TEMFSFEL                                 
047073       END-IF                                                             
047074     ELSE                                                                 
047075       PERFORM MFS-ERASE-FIELD-IN                                         
047076     END-IF                                                               
047077     .                                                                    
047078     EJECT                                                                
047079 F-READ-SHOW-INFO SECTION.                                                
047080     MOVE 'F-READ-SHOW-INFO'     TO CURRENT-SECTION                       
047081                                                                          
047082     PERFORM IMS-GU-WDR201                                                
047083      IF SEGMENT-FOUND                                                    
047090        MOVE +17                    TO MOD-KDFRAKT-AIR1                   
047091        MOVE +18                    TO MOD-KDFRAKT-AIR2                   
047092        MOVE +19                    TO MOD-KDFRAKT-AIR3                   
047102        MOVE +41                    TO MOD-KDFRAKT-BOAT1                  
047103        MOVE +43                    TO MOD-KDFRAKT-BOAT2                  
047124        PERFORM IMS-GNP-WDGX4592                                          
047125        MOVE +1                     TO INDX                               
047126        PERFORM VARYING INDX FROM +1 BY +1 UNTIL INDX > MAX-INDX          
047127         IF SEGMENT-FOUND                                                 
047128            IF INDX = 1                                                   
047129               MOVE 4592-IDDISTR        TO SAVE-IDDISTR-ENTER             
047130                                           SAVE-IDDISTR-NEXT              
047131            END-IF                                                        
047132            MOVE 4592-IDDISTR           TO MOD-IDDISTR(INDX)              
047133                                           SAVE-IDDISTR(INDX)             
047134            COMPUTE WS-RETRPFAC-AIR1  = (4592-RETRPFAC-AIR * 100)         
047135            COMPUTE WS-RETRPFAC-BOAT1 = (4592-RETRPFAC-BOAT *100)         
047136            COMPUTE WS-RETRPFAC-ROAD1 = (4592-RETRPFAC-ROAD * 100)        
047137            IF WS-RETRPFAC-AIR1  > 0                                      
047138              MOVE WS-RETRPFAC-AIR1     TO MOD-RETRPFAC-AIR(INDX)         
047139              MOVE '%'                  TO MOD-AIR-PERCENT(INDX)          
047140            ELSE                                                          
047141              IF  4592-PRWEIGHT-AIR  > ZERO                               
047142                MOVE 4592-PRWEIGHT-AIR  TO MOD-PRWEIGHT-AIR(INDX)         
047143              END-IF                                                      
047144            END-IF                                                        
047145            IF WS-RETRPFAC-BOAT1  > 0                                     
047146              MOVE WS-RETRPFAC-BOAT1    TO MOD-RETRPFAC-BOAT(INDX)        
047147              MOVE '%'                  TO MOD-BOAT-PERCENT(INDX)         
047148            ELSE                                                          
047149              IF  4592-PRWEIGHT-BOAT > ZERO                               
047150                MOVE 4592-PRWEIGHT-BOAT TO MOD-PRWEIGHT-BOAT(INDX)        
047151              END-IF                                                      
047152            END-IF                                                        
047160            IF  WS-RETRPFAC-ROAD1  > 0                                    
047233              MOVE WS-RETRPFAC-ROAD1    TO MOD-RETRPFAC-ROAD(INDX)        
047234              MOVE '%'                  TO MOD-ROAD-PERCENT(INDX)         
047235            ELSE                                                          
047236              IF  4592-PRWEIGHT-ROAD > ZERO                               
047237               MOVE 4592-PRWEIGHT-ROAD  TO MOD-PRWEIGHT-ROAD(INDX)        
047240              END-IF                                                      
047241            END-IF                                                        
047259            IF  4592-PRHAZMAT-AIR  > ZERO                                 
047260              MOVE 4592-PRHAZMAT-AIR    TO MOD-PRHAZMAT-AIR(INDX)         
047261            END-IF                                                        
047262            IF  4592-PRHAZMAT-BOAT > ZERO                                 
047263              MOVE 4592-PRHAZMAT-BOAT   TO MOD-PRHAZMAT-BOAT(INDX)        
047264            END-IF                                                        
047265            IF  4592-PRHAZMAT-ROAD  > ZERO                                
047266              MOVE 4592-PRHAZMAT-ROAD   TO MOD-PRHAZMAT-ROAD(INDX)        
047267            END-IF                                                        
047268            IF  4592-REINSFAC-AIR > ZERO                                  
047269              MOVE 4592-REINSFAC-AIR    TO MOD-REINSFAC-AIR(INDX)         
047270            END-IF                                                        
047271            IF  4592-REINSFAC-BOAT > ZERO                                 
047272              MOVE 4592-REINSFAC-BOAT   TO MOD-REINSFAC-BOAT(INDX)        
047273            END-IF                                                        
047274            IF  4592-REINSFAC-ROAD > ZERO                                 
047275              MOVE 4592-REINSFAC-ROAD   TO MOD-REINSFAC-ROAD(INDX)        
047276            END-IF                                                        
047277            PERFORM IMS-GNP-WDGX4592                                      
047286         END-IF                                                           
047288        END-PERFORM                                                       
047289      END-IF                                                              
047290                                                                          
047291     PERFORM FA-CHECK-CHANGE                                              
047292                                                                          
047293     IF SEGMENT-FOUND                                                     
047294       MOVE 4592-IDDISTR              TO SAVE-IDDISTR-NEXT                
047300       MOVE INF-MORE-INFO-EXISTS      TO MED-IDMFSINF                     
047400       CALL WMEDKONV                  USING MED-WMEDAREA                  
047500       MOVE MED-TEMFSINF              TO MOD-TEMFSINF                     
047600     ELSE                                                                 
047700       MOVE INF-LAST-PAGE             TO MED-IDMFSINF                     
047800       CALL WMEDKONV                  USING MED-WMEDAREA                  
047900       MOVE MED-TEMFSINF              TO MOD-TEMFSINF                     
048000     END-IF                                                               
056118                                                                          
056119     MOVE '002'                       TO MSGI-KDCALL                      
056120     MOVE '4457'                      TO SAVE-IDTRANS                     
056121     MOVE SAVE-AREA                   TO MSGI-SPAR-AREA                   
056122     CALL W005INIT                    USING MSGI-WMSGINIT                 
056123                                            WDP7-PCB                      
056124     .                                                                    
056125     EJECT                                                                
056126 FA-CHECK-CHANGE SECTION.                                                 
056127     MOVE 'FA-CHECK-CHANGE '     TO CURRENT-SECTION                       
056128                                                                          
056129     MOVE +1 TO INDX                                                      
056130     PERFORM VARYING INDX FROM +1 BY +1 UNTIL INDX > MAX-INDX             
056131      IF MID-KDCMD (INDX) = 'C'                                           
056132        MOVE MOD-IDDISTR(INDX)      TO MOD-IDDISTR-UPD                    
056133        MOVE MOD-IDDISTR-UPD        TO W-IDDISTR                          
056134        MOVE MOD-RETRPFAC-AIR(INDX) TO MOD-RETRPFAC-AIR-UPD               
056135        MOVE MOD-PRWEIGHT-AIR(INDX) TO MOD-PRWEIGHT-AIR-UPD               
056136        MOVE MOD-PRHAZMAT-AIR(INDX) TO MOD-PRHAZMAT-AIR-UPD               
056137        MOVE MOD-RETRPFAC-BOAT(INDX)TO MOD-RETRPFAC-BOAT-UPD              
056138        MOVE MOD-PRWEIGHT-BOAT(INDX)TO MOD-PRWEIGHT-BOAT-UPD              
056139        MOVE MOD-PRHAZMAT-BOAT(INDX)TO MOD-PRHAZMAT-BOAT-UPD              
056140        MOVE MOD-RETRPFAC-ROAD(INDX)TO MOD-RETRPFAC-ROAD-UPD              
056141        MOVE MOD-PRWEIGHT-ROAD(INDX)TO MOD-PRWEIGHT-ROAD-UPD              
056142        MOVE MOD-PRHAZMAT-ROAD(INDX)TO MOD-PRHAZMAT-ROAD-UPD              
056143        MOVE MOD-REINSFAC-AIR(INDX) TO MOD-REINSFAC-AIR-UPD2              
056144        MOVE MOD-REINSFAC-BOAT(INDX)TO MOD-REINSFAC-BOAT-UPD2             
056145        MOVE MOD-REINSFAC-ROAD(INDX)TO MOD-REINSFAC-ROAD-UPD2             
056146      END-IF                                                              
056147     END-PERFORM                                                          
056148     .                                                                    
056149     EJECT                                                                
056150 G-CHECK-INPUT SECTION.                                                   
056151     MOVE 'G-CHECK-INPUT   '     TO CURRENT-SECTION                       
056152                                                                          
056153     MOVE YES                      TO INDATA-SW                           
056154                                                                          
056155     IF (MID-INPUT = ALL '+' OR SPACES) AND                               
056156        (MID-UPD   = ALL '+' OR SPACES)                                   
056157       MOVE ERR-PF11-AND-NO-DATA   TO MED-IDMFSFEL                        
056158       CALL WMEDKONV            USING MED-WMEDAREA                        
056159       MOVE MED-MFSFEL             TO MOD-TEMFSFEL                        
056160       MOVE NOO                    TO INDATA-SW                           
056161     ELSE                                                                 
056162       MOVE SPACE                     TO MED-IDMFSFEL                     
056163       PERFORM VARYING INDX FROM +1 BY +1 UNTIL INDX > MAX-INDX           
056164          IF MID-KDCMD (INDX) = ALL '+' OR                                
056165             MID-KDCMD (INDX) = SPACE   OR                                
056166             MID-KDCMD (INDX) < SPACE                                     
056167            MOVE MFS-RENSA-FAELT      TO MOD-KDCMD (INDX)                 
056170          ELSE                                                            
056175            IF  MID-KDCMD (INDX) = 'C'                                    
056177             AND NOT MFS-UPDATE                                           
056179                MOVE MFS-RENSA-FAELT  TO MOD-KDCMD (INDX)                 
056180            ELSE                                                          
056181              IF  MID-KDCMD (INDX) = 'D'                                  
056182                MOVE MFS-ADD-READ-FIELD                                   
056183                                      TO MOD-KDCMD-ATTR (INDX)            
056184                MOVE MFS-DO-NOT-TOUCH-FIELD                               
056185                                      TO MOD-KDCMD      (INDX)            
056186              ELSE                                                        
056187                MOVE MFS-ALPHA-FIELD-WRONG                                
056188                                      TO MOD-KDCMD-ATTR (INDX)            
056189                MOVE NOO              TO INDATA-SW                        
056190              END-IF                                                      
056192            END-IF                                                        
056193          END-IF                                                          
056194       END-PERFORM                                                        
056195     END-IF                                                               
056196                                                                          
056197     IF  MID-UPD NOT = ALL '+'                                            
056198       PERFORM GA-CHECK-UPD-ROW                                           
056203       IF INDATA-OK                                                       
056204        PERFORM S07-VALIDATE-RULE                                         
056221        IF                                                                
056222        (WS-RULE-CNT1 = 1 OR WS-RULE-CNT2 = 1 OR WS-RULE-CNT3 = 1)        
056223         OR (WS-RULE-CNT1 = 1 AND                                         
056224             WS-RULE-CNT2 = 1 AND                                         
056225             WS-RULE-CNT3 = 1)                                            
056227           MOVE NOO          TO INDATA-SW                                 
056228        END-IF                                                            
056229       END-IF                                                             
056230     END-IF                                                               
056231                                                                          
056232     IF INDATA-WRONG                                                      
056240        MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                       
056243        IF WS-RULE-CNT1= 1                                                
056244          MOVE ERR-MORE-THAN-ONE-RULE TO MED-IDMFSFEL                     
056245          MOVE MFS-ADD-READ-HILIGHT-FIELD                                 
056246                                    TO MOD-RETRPFAC-AIR-UPD-ATTR          
056247                                       MOD-PRWEIGHT-AIR-UPD-ATTR          
056248        END-IF                                                            
056249        IF WS-RULE-CNT2= 1                                                
056250          MOVE ERR-MORE-THAN-ONE-RULE TO MED-IDMFSFEL                     
056251          MOVE MFS-ADD-READ-HILIGHT-FIELD                                 
056252                                    TO  MOD-RETRPFAC-BOAT-UPD-ATTR        
056253                                        MOD-PRWEIGHT-BOAT-UPD-ATTR        
056260        END-IF                                                            
056262        IF WS-RULE-CNT3= 1                                                
056264          MOVE ERR-MORE-THAN-ONE-RULE TO MED-IDMFSFEL                     
056266          MOVE MFS-ADD-READ-HILIGHT-FIELD                                 
056267                                    TO  MOD-RETRPFAC-ROAD-UPD-ATTR        
056268                                        MOD-PRWEIGHT-ROAD-UPD-ATTR        
056269        END-IF                                                            
056274        CALL WMEDKONV          USING MED-WMEDAREA                         
056275        MOVE MED-MFSFEL           TO MOD-TEMFSFEL                         
056277        PERFORM MFS-DONT-TOUCH-FIELD-IN                                   
056280     END-IF                                                               
056283     .                                                                    
056290     EJECT                                                                
057608 GA-CHECK-UPD-ROW  SECTION.                                               
057609     MOVE 'GA-CHECK-UPD-ROW'     TO CURRENT-SECTION                       
057610                                                                          
057620     IF MID-IDDISTR-UPD NOT NUMERIC                                       
057630       MOVE MFS-NUM-FIELD-WRONG    TO MOD-IDDISTR-UPD-ATTR                
057640       MOVE NOO                    TO INDATA-SW                           
057728     ELSE                                                                 
057729       MOVE MFS-NUM-FIELD-OK       TO MOD-IDDISTR-UPD-ATTR                
057730       MOVE MID-IDDISTR-UPD        TO W-IDDISTR                           
057731                                      WS-IDDISTR                          
057735     END-IF                                                               
057736                                                                          
057740*    ---AIR  FREIGHT VALUE VALIDATION                                     
057741                                                                          
057742     IF MID-RETRPFAC-AIR-UPD NOT = ALL '+'                                
057750       MOVE MID-RETRPFAC-AIR-UPD   TO WS-IDFRIDATA                        
057760       MOVE WS-IDFRIDATA           TO DEC-IDFRIDATA                       
057770       MOVE 3                      TO DEC-KVHELTAL                        
057780       MOVE 2                      TO DEC-KVDECIMAL                       
057790       CALL WDECEDIT            USING DEC-WDECAREA                        
057900       IF DEC-KDSVAR-OK                                                   
058000         MOVE DEC-IDEDITDATA      TO WS-RETRPFAC-AIR2                     
058100         MOVE MFS-NUM-FIELD-OK    TO MOD-RETRPFAC-AIR-UPD-ATTR            
058200       ELSE                                                               
058300         MOVE MFS-NUM-FIELD-WRONG TO                                      
058400                                     MOD-RETRPFAC-AIR-UPD-ATTR            
058500         MOVE NOO                 TO INDATA-SW                            
058600       END-IF                                                             
058700     END-IF                                                               
058800                                                                          
058801*    ---AIR  FREIGHT WEIGHT    VALIDATION                                 
058810     IF MID-PRWEIGHT-AIR-UPD NOT = ALL '+'                                
058900       INSPECT MID-PRWEIGHT-AIR-UPD  REPLACING LEADING SPACE              
059000                               BY ZERO                                    
060417       IF MID-PRWEIGHT-AIR-UPD   NUMERIC                                  
060418         MOVE MFS-NUM-FIELD-OK       TO MOD-PRWEIGHT-AIR-UPD-ATTR         
060419         MOVE MID-PRWEIGHT-AIR-UPD   TO WS-PRWEIGHT-AIR                   
060420       ELSE                                                               
060430         MOVE MFS-NUM-FIELD-WRONG    TO MOD-PRWEIGHT-AIR-UPD-ATTR         
060440         MOVE NOO                    TO INDATA-SW                         
060453       END-IF                                                             
060454     END-IF                                                               
060455*    ---AIR  FREIGHT HAZMAT  VALIDATION                                   
060460                                                                          
060470     IF MID-PRHAZMAT-AIR-UPD NOT = ALL '+'                                
060480       INSPECT MID-PRHAZMAT-AIR-UPD  REPLACING LEADING SPACE              
060481                               BY ZERO                                    
060483       IF MID-PRHAZMAT-AIR-UPD NUMERIC                                    
060484         MOVE MFS-NUM-FIELD-OK       TO MOD-PRHAZMAT-AIR-UPD-ATTR         
060485       ELSE                                                               
060486         MOVE MFS-NUM-FIELD-WRONG    TO MOD-PRHAZMAT-AIR-UPD-ATTR         
060487         MOVE NOO                    TO INDATA-SW                         
060488       END-IF                                                             
060489     END-IF                                                               
060505                                                                          
060506*    ---BOAT FREIGHT VALUE VALIDATION                                     
060507                                                                          
060508     IF MID-RETRPFAC-BOAT-UPD   NOT = ALL '+'                             
060509        MOVE MID-RETRPFAC-BOAT-UPD   TO WS-IDFRIDATA                      
060510        MOVE WS-IDFRIDATA           TO DEC-IDFRIDATA                      
060511        MOVE 3                      TO DEC-KVHELTAL                       
060520        MOVE 2                      TO DEC-KVDECIMAL                      
060530        CALL WDECEDIT            USING DEC-WDECAREA                       
060540        IF DEC-KDSVAR-OK                                                  
060550          MOVE DEC-IDEDITDATA      TO WS-RETRPFAC-BOAT2                   
060560          MOVE MFS-NUM-FIELD-OK    TO MOD-RETRPFAC-BOAT-UPD-ATTR          
060570        ELSE                                                              
060580          MOVE MFS-NUM-FIELD-WRONG TO                                     
060590                                      MOD-RETRPFAC-BOAT-UPD-ATTR          
060600          MOVE NOO                 TO INDATA-SW                           
060601        END-IF                                                            
060602     END-IF                                                               
060603*    ---BOAT WEIGHT/VOLUME VALIDATION                                     
060604                                                                          
060605     IF MID-PRWEIGHT-BOAT-UPD  NOT = ALL '+'                              
060606       INSPECT MID-PRWEIGHT-BOAT-UPD  REPLACING LEADING SPACE             
060607                               BY ZERO                                    
060608       IF MID-PRWEIGHT-BOAT-UPD   NUMERIC                                 
060609         MOVE MFS-NUM-FIELD-OK       TO MOD-PRWEIGHT-BOAT-UPD-ATTR        
060610         MOVE MID-PRWEIGHT-BOAT-UPD   TO WS-PRWEIGHT-BOAT                 
060611       ELSE                                                               
060612         MOVE MFS-NUM-FIELD-WRONG    TO MOD-PRWEIGHT-BOAT-UPD-ATTR        
060613         MOVE NOO                    TO INDATA-SW                         
060614       END-IF                                                             
060615     END-IF                                                               
065180                                                                          
065190*    ---BOAT HAZMAT VALIDATION                                            
067603     IF MID-PRHAZMAT-BOAT-UPD  NOT = ALL '+'                              
067604       INSPECT MID-PRHAZMAT-BOAT-UPD  REPLACING LEADING SPACE             
067605                               BY ZERO                                    
067606       IF MID-PRHAZMAT-BOAT-UPD  NUMERIC                                  
067607         MOVE MFS-NUM-FIELD-OK       TO MOD-PRHAZMAT-BOAT-UPD-ATTR        
067608       ELSE                                                               
067609         MOVE MFS-NUM-FIELD-WRONG    TO MOD-PRHAZMAT-BOAT-UPD-ATTR        
067610         MOVE NOO                    TO INDATA-SW                         
067611       END-IF                                                             
067612     END-IF                                                               
067613                                                                          
067614*    ---ROAD FREIGHT VALUE VALIDATION                                     
067615                                                                          
067616     IF MID-RETRPFAC-ROAD-UPD   NOT = ALL '+'                             
067617        MOVE MID-RETRPFAC-ROAD-UPD   TO WS-IDFRIDATA                      
067618        MOVE WS-IDFRIDATA           TO DEC-IDFRIDATA                      
067619        MOVE 3                      TO DEC-KVHELTAL                       
067620        MOVE 2                      TO DEC-KVDECIMAL                      
067621        CALL WDECEDIT            USING DEC-WDECAREA                       
067622         IF DEC-KDSVAR-OK                                                 
067623           MOVE DEC-IDEDITDATA      TO WS-RETRPFAC-ROAD2                  
067624           MOVE MFS-NUM-FIELD-OK    TO MOD-RETRPFAC-ROAD-UPD-ATTR         
067625         ELSE                                                             
067626           MOVE MFS-NUM-FIELD-WRONG TO                                    
067627                                    MOD-RETRPFAC-ROAD-UPD-ATTR            
067628           MOVE NOO                 TO INDATA-SW                          
067629         END-IF                                                           
067630     END-IF                                                               
067631                                                                          
067632*    ---ROAD WEIGHT/VOLUME VALIDATION                                     
067633     IF MID-PRWEIGHT-ROAD-UPD  NOT = ALL '+'                              
067634       INSPECT MID-PRWEIGHT-ROAD-UPD  REPLACING LEADING SPACE             
067640                               BY ZERO                                    
070705       IF MID-PRWEIGHT-ROAD-UPD  NUMERIC                                  
070707         MOVE MFS-NUM-FIELD-OK      TO MOD-PRWEIGHT-ROAD-UPD-ATTR         
070708         MOVE MID-PRWEIGHT-ROAD-UPD TO WS-PRWEIGHT-ROAD                   
070709       ELSE                                                               
070710         MOVE MFS-NUM-FIELD-WRONG   TO MOD-PRWEIGHT-ROAD-UPD-ATTR         
070711         MOVE NOO                   TO INDATA-SW                          
070712       END-IF                                                             
070713     END-IF                                                               
070714                                                                          
070715*    ---ROAD HAZMAT VALIDATION                                            
070741     IF MID-PRHAZMAT-ROAD-UPD   NOT = ALL '+'                             
070742       INSPECT MID-PRHAZMAT-ROAD-UPD  REPLACING LEADING SPACE             
070743                               BY ZERO                                    
070744       IF MID-PRHAZMAT-ROAD-UPD  NUMERIC                                  
070745         MOVE MFS-NUM-FIELD-OK      TO MOD-PRHAZMAT-ROAD-UPD-ATTR         
070746       ELSE                                                               
070747         MOVE MFS-NUM-FIELD-WRONG   TO MOD-PRHAZMAT-ROAD-UPD-ATTR         
070748         MOVE NOO                   TO INDATA-SW                          
070749       END-IF                                                             
070750     END-IF                                                               
070753                                                                          
070754*    --- INSURANCE FACTOR VALIDATION --AIR                                
070755                                                                          
070784     IF MID-REINSFAC-AIR-UPD2 NOT = ALL '+'                               
070785        INSPECT MID-REINSFAC-AIR-UPD2 REPLACING LEADING SPACE             
070786                             BY ZERO                                      
070787        MOVE MID-REINSFAC-AIR-UPD2 TO WS-REINSFAC-AIR1                    
070788        UNSTRING WS-REINSFAC-AIR1 DELIMITED BY '.' INTO                   
070789                                       WS-INT1                            
070790                                       WS-DEC1                            
070791        COMPUTE WS-INT-LEN1 = FUNCTION LENGTH                             
070792                          (FUNCTION TRIM (WS-INT1))                       
070793        COMPUTE WS-DEC-LEN1 = FUNCTION LENGTH                             
070794                          (FUNCTION TRIM (WS-DEC1))                       
070795       IF (WS-INT-LEN1 = 1 AND WS-DEC-LEN1 <= 5) AND                      
070796             (WS-INT1 NUMERIC )                                           
070800          COMPUTE WS-REINSFAC-AIR-N =                                     
070801                     FUNCTION NUMVAL(WS-REINSFAC-AIR1)                    
070802          MOVE MFS-NUM-FIELD-OK    TO MOD-REINSFAC-AIR-UPD2-ATTR          
070807       ELSE                                                               
070808          MOVE MFS-NUM-FIELD-WRONG TO MOD-REINSFAC-AIR-UPD2-ATTR          
070810          MOVE NOO                 TO INDATA-SW                           
070811       END-IF                                                             
070812     END-IF                                                               
070830                                                                          
070831*    --- INSURANCE FACTOR VALIDATION --BOAT                               
070832     IF MID-REINSFAC-BOAT-UPD2 NOT = ALL '+'                              
070833       INSPECT MID-REINSFAC-BOAT-UPD2 REPLACING LEADING SPACE             
070834                            BY ZERO                                       
070835       MOVE MID-REINSFAC-BOAT-UPD2 TO WS-REINSFAC-BOAT1                   
070836       UNSTRING WS-REINSFAC-BOAT1 DELIMITED BY '.' INTO                   
070837                                          WS-INT2                         
070838                                          WS-DEC2                         
070839       COMPUTE WS-INT-LEN2 = FUNCTION LENGTH                              
070840                              (FUNCTION TRIM (WS-INT2))                   
070841       COMPUTE WS-DEC-LEN2 = FUNCTION LENGTH                              
070842                              (FUNCTION TRIM (WS-DEC2))                   
070843        IF (WS-INT-LEN2 = 1 AND WS-DEC-LEN2 <= 5) AND                     
070844              (WS-INT2 NUMERIC )                                          
070847          COMPUTE WS-REINSFAC-BOAT-N =                                    
070852                       FUNCTION NUMVAL(WS-REINSFAC-BOAT1)                 
070853          MOVE MFS-NUM-FIELD-OK TO MOD-REINSFAC-BOAT-UPD2-ATTR            
070854        ELSE                                                              
070855          MOVE MFS-NUM-FIELD-WRONG TO                                     
070856                                   MOD-REINSFAC-BOAT-UPD2-ATTR            
070857          MOVE NOO                TO INDATA-SW                            
070858        END-IF                                                            
070859     END-IF                                                               
070862                                                                          
070863*    --- INSURANCE FACTOR VALIDATION --ROAD                               
070864     IF MID-REINSFAC-ROAD-UPD2 NOT = ALL '+'                              
070865       INSPECT MID-REINSFAC-ROAD-UPD2 REPLACING LEADING SPACE             
070866                             BY ZERO                                      
070867       MOVE MID-REINSFAC-ROAD-UPD2 TO WS-REINSFAC-ROAD1                   
070868       UNSTRING WS-REINSFAC-ROAD1 DELIMITED BY '.' INTO                   
070869                                    WS-INT3                               
070870                                    WS-DEC3                               
070871       COMPUTE WS-INT-LEN3 = FUNCTION LENGTH                              
070880                               (FUNCTION TRIM (WS-INT3))                  
070881       COMPUTE WS-DEC-LEN3 = FUNCTION LENGTH                              
070882                               (FUNCTION TRIM (WS-DEC3))                  
070883       IF (WS-INT-LEN3 = 1 AND WS-DEC-LEN3 <= 5) AND                      
070884              (WS-INT3 NUMERIC )                                          
070889          COMPUTE WS-REINSFAC-ROAD-N =                                    
070890                    FUNCTION NUMVAL(WS-REINSFAC-ROAD1)                    
070891          MOVE MFS-NUM-FIELD-OK TO MOD-REINSFAC-ROAD-UPD2-ATTR            
070892       ELSE                                                               
070893          MOVE MFS-NUM-FIELD-WRONG TO                                     
070894                                MOD-REINSFAC-ROAD-UPD2-ATTR               
070895          MOVE NOO                TO INDATA-SW                            
070896       END-IF                                                             
070897     END-IF                                                               
079330     .                                                                    
079401     EJECT                                                                
093799 S07-VALIDATE-RULE SECTION.                                               
093800     MOVE 'S07-VALIDATE-RUL'     TO CURRENT-SECTION                       
093801                                                                          
093802     MOVE 0                      TO WS-RULE-CNT1                          
093803     IF WS-RETRPFAC-AIR2    > ZERO                                        
093804      AND WS-PRWEIGHT-AIR > ZERO                                          
093805         ADD 1                   TO WS-RULE-CNT1                          
093806     END-IF                                                               
093807                                                                          
093808     MOVE 0 TO WS-RULE-CNT2                                               
093809     IF WS-RETRPFAC-BOAT2    > ZERO                                       
093810      AND WS-PRWEIGHT-BOAT > ZERO                                         
093811         ADD 1                   TO WS-RULE-CNT2                          
093812     END-IF                                                               
093813                                                                          
093815     MOVE 0 TO WS-RULE-CNT3                                               
093816     IF WS-RETRPFAC-ROAD2    > ZERO                                       
093817      AND WS-PRWEIGHT-ROAD > ZERO                                         
093818         ADD 1                   TO WS-RULE-CNT3                          
093820     END-IF                                                               
093823     .                                                                    
093830     EJECT                                                                
093900 H-UPDATE SECTION.                                                        
094000     MOVE 'H-UPDATE        '     TO CURRENT-SECTION                       
094100                                                                          
094208     PERFORM VARYING INDX FROM +1 BY +1 UNTIL INDX > MAX-INDX             
094210        IF MID-KDCMD (INDX) = 'D'                                         
094211        AND MID-IDDISTR-UPD  = ALL '+'  OR SPACES                         
094212          MOVE SAVE-IDDISTR(INDX)  TO W-IDDISTR                           
094217          PERFORM IMS-GHU-WDGX4592                                        
094218          IF SEGMENT-FOUND                                                
094220            PERFORM IMS-DLET-WDGX4592                                     
094221          END-IF                                                          
094230        END-IF                                                            
094260                                                                          
094302        IF MID-IDDISTR-UPD NOT = ALL '+'                                  
094305          MOVE MID-IDDISTR-UPD TO W-IDDISTR                               
094311          PERFORM IMS-GHU-WDGX4592                                        
094313          IF SEGMENT-FOUND                                                
094314            PERFORM HB-MOVE-FIELDS-REPL                                   
094315            PERFORM IMS-REPL-WDGX4592                                     
094316          ELSE                                                            
094332            PERFORM HA-INIT-FIELDS                                        
094333            PERFORM HC-MOVE-FIELDS-ISRT                                   
094334            PERFORM IMS-ISRT-WDGX4592                                     
094335          END-IF                                                          
094336        END-IF                                                            
094337     END-PERFORM                                                          
094361                                                                          
094362     PERFORM MFS-FORM-ATTR                                                
094363     PERFORM MFS-ERASE-FIELD-IN                                           
094364     MOVE INF-UPDATE-DONE          TO MED-IDMFSINF                        
094365     CALL WMEDKONV              USING MED-WMEDAREA                        
094366     MOVE MED-MFSINF               TO MOD-TEMFSFEL                        
097501     .                                                                    
097601     EJECT                                                                
097602 HA-INIT-FIELDS SECTION.                                                  
097603     MOVE 'HA-INIT-FIELDS  '     TO CURRENT-SECTION                       
097604                                                                          
097605     INITIALIZE   4592-RETRPFAC-AIR                                       
097606                  4592-RETRPFAC-BOAT                                      
097607                  4592-RETRPFAC-ROAD                                      
097608                  4592-REINSFAC-AIR                                       
097609                  4592-REINSFAC-BOAT                                      
097610                  4592-REINSFAC-ROAD                                      
097611                  4592-PRWEIGHT-AIR                                       
097612                  4592-PRWEIGHT-BOAT                                      
097630                  4592-PRWEIGHT-ROAD                                      
097650                  4592-PRHAZMAT-AIR                                       
097660                  4592-PRHAZMAT-BOAT                                      
097670                  4592-PRHAZMAT-ROAD                                      
097690     .                                                                    
097700     EJECT                                                                
106601 HB-MOVE-FIELDS-REPL SECTION.                                             
106602     MOVE 'HB-MOVE-FIELDS-R'     TO CURRENT-SECTION                       
106801                                                                          
106903     IF MID-IDDISTR-UPD        NOT = ALL '+'                              
107004       MOVE W-IDDISTR              TO 4592-IDDISTR                        
107101     END-IF                                                               
107202     IF MID-RETRPFAC-AIR-UPD   NOT = ALL '+'                              
107301       COMPUTE WS-RETRPFAC-AIR  = WS-RETRPFAC-AIR2 / 100                  
107302       MOVE WS-RETRPFAC-AIR          TO 4592-RETRPFAC-AIR                 
107401     END-IF                                                               
107702     IF MID-PRWEIGHT-AIR-UPD  NOT = ALL '+'                               
107703       MOVE MID-PRWEIGHT-AIR-UPD     TO 4592-PRWEIGHT-AIR                 
107705     END-IF                                                               
107706     IF MID-PRHAZMAT-AIR-UPD  NOT = ALL '+'                               
107707       MOVE MID-PRHAZMAT-AIR-UPD     TO 4592-PRHAZMAT-AIR                 
107708     END-IF                                                               
108002     IF MID-RETRPFAC-BOAT-UPD  NOT = ALL '+'                              
108003       COMPUTE WS-RETRPFAC-BOAT  = WS-RETRPFAC-BOAT2 / 100                
108004       MOVE WS-RETRPFAC-BOAT         TO 4592-RETRPFAC-BOAT                
108005     END-IF                                                               
109302     IF MID-PRWEIGHT-BOAT-UPD  NOT = ALL '+'                              
109401       MOVE MID-PRWEIGHT-BOAT-UPD    TO 4592-PRWEIGHT-BOAT                
109502     END-IF                                                               
109503     IF MID-PRHAZMAT-BOAT-UPD  NOT = ALL '+'                              
109504       MOVE MID-PRHAZMAT-BOAT-UPD    TO 4592-PRHAZMAT-BOAT                
109505     END-IF                                                               
109506     IF MID-RETRPFAC-ROAD-UPD  NOT = ALL '+'                              
109507       COMPUTE WS-RETRPFAC-ROAD  = WS-RETRPFAC-ROAD2 / 100                
109508       MOVE WS-RETRPFAC-ROAD         TO 4592-RETRPFAC-ROAD                
109509     END-IF                                                               
109602     IF MID-PRWEIGHT-ROAD-UPD  NOT = ALL '+'                              
109701       MOVE MID-PRWEIGHT-ROAD-UPD    TO 4592-PRWEIGHT-ROAD                
109802     END-IF                                                               
110502     IF MID-PRHAZMAT-ROAD-UPD  NOT = ALL '+'                              
110601       MOVE MID-PRHAZMAT-ROAD-UPD    TO 4592-PRHAZMAT-ROAD                
110702     END-IF                                                               
110703     IF MID-REINSFAC-AIR-UPD2 NOT = ALL '+'                               
110707       MOVE WS-REINSFAC-AIR-N        TO 4592-REINSFAC-AIR                 
110708     END-IF                                                               
110710     IF MID-REINSFAC-BOAT-UPD2 NOT = ALL '+'                              
110712       MOVE WS-REINSFAC-BOAT-N       TO 4592-REINSFAC-BOAT                
110713     END-IF                                                               
110720     IF MID-REINSFAC-ROAD-UPD2 NOT = ALL '+'                              
110741       MOVE WS-REINSFAC-ROAD-N       TO 4592-REINSFAC-ROAD                
110750     END-IF                                                               
110801     .                                                                    
110901     EJECT                                                                
110902 HC-MOVE-FIELDS-ISRT SECTION.                                             
110903     MOVE 'HC-MOVE-FIELDS-I'     TO CURRENT-SECTION                       
110905                                                                          
110907     IF MID-IDDISTR-UPD        NOT = ALL '+'                              
110908       MOVE MID-IDDISTR-UPD        TO 4592-IDDISTR                        
110909     END-IF                                                               
110910     IF MID-RETRPFAC-AIR-UPD   NOT = ALL '+'                              
110911       COMPUTE WS-RETRPFAC-AIR   = WS-RETRPFAC-AIR2 / 100                 
110912       MOVE WS-RETRPFAC-AIR          TO 4592-RETRPFAC-AIR                 
110920     END-IF                                                               
110950     IF MID-PRWEIGHT-AIR-UPD   NOT = ALL '+'                              
110960       MOVE MOD-PRWEIGHT-AIR-UPD     TO 4592-PRWEIGHT-AIR                 
110970     END-IF                                                               
110980     IF MID-PRHAZMAT-AIR-UPD  NOT = ALL '+'                               
110990       MOVE MOD-PRHAZMAT-AIR-UPD     TO 4592-PRHAZMAT-AIR                 
111000     END-IF                                                               
111100     IF MID-RETRPFAC-BOAT-UPD  NOT = ALL '+'                              
111200       COMPUTE WS-RETRPFAC-BOAT  = WS-RETRPFAC-BOAT2 / 100                
111300       MOVE WS-RETRPFAC-BOAT         TO 4592-RETRPFAC-BOAT                
111400     END-IF                                                               
111500     IF MID-PRWEIGHT-BOAT-UPD  NOT = ALL '+'                              
111600       MOVE MOD-PRWEIGHT-BOAT-UPD    TO 4592-PRWEIGHT-BOAT                
111700     END-IF                                                               
111800     IF MID-PRHAZMAT-BOAT-UPD  NOT = ALL '+'                              
111900       MOVE MOD-PRHAZMAT-BOAT-UPD    TO 4592-PRHAZMAT-BOAT                
112000     END-IF                                                               
112100     IF MID-RETRPFAC-ROAD-UPD  NOT = ALL '+'                              
112200       COMPUTE WS-RETRPFAC-ROAD  = WS-RETRPFAC-ROAD2 / 100                
112300       MOVE WS-RETRPFAC-ROAD         TO 4592-RETRPFAC-ROAD                
112400     END-IF                                                               
112500     IF MID-PRWEIGHT-ROAD-UPD   NOT = ALL '+'                             
112600       MOVE MOD-PRWEIGHT-ROAD-UPD    TO 4592-PRWEIGHT-ROAD                
112700     END-IF                                                               
112800     IF MID-PRHAZMAT-ROAD-UPD  NOT  = ALL '+'                             
112900       MOVE MOD-PRHAZMAT-ROAD-UPD    TO 4592-PRHAZMAT-ROAD                
113000     END-IF                                                               
113100     IF  MID-REINSFAC-AIR-UPD2 NOT = ALL '+'                              
113200       MOVE  WS-REINSFAC-AIR-N       TO 4592-REINSFAC-AIR                 
113300     END-IF                                                               
113400     IF MID-REINSFAC-BOAT-UPD2 NOT = ALL '+'                              
113500       MOVE WS-REINSFAC-BOAT-N       TO 4592-REINSFAC-BOAT                
113600     END-IF                                                               
113700     IF MID-REINSFAC-ROAD-UPD2 NOT = ALL '+'                              
113710       MOVE WS-REINSFAC-ROAD-N       TO 4592-REINSFAC-ROAD                
113720     END-IF                                                               
113800     .                                                                    
113900     EJECT                                                                
114200 I-INSERT-DC SECTION.                                                     
114300     MOVE 'I-INSERT-DC     '     TO CURRENT-SECTION                       
114400                                                                          
114401     IF DCS-NDC                                                           
114402       MOVE '4591'               TO WDR2-IDHTYP                           
114403       MOVE W-IDDC               TO WDR2-IDDC                             
114404       MOVE LOW-VALUES           TO WDR2-NYCKEL-VALFRI                    
114405                                                                          
114406       PERFORM IMS-ISRT-WDR201                                            
114407                                                                          
114408       IF SEGMENT-EXISTS                                                  
114409         MOVE DC-PRESENT-MSG     TO MOD-TEMFSFEL                          
114410         MOVE NOO                TO KEYS-SW                               
114411                                      INDATA-SW                           
114412         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
114413       ELSE                                                               
114414         MOVE DC-INSERTED-MSG    TO MOD-TEMFSFEL                          
114415         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
114416       END-IF                                                             
114420     ELSE                                                                 
114431       MOVE INVALID-DC-ISRT-MSG  TO MOD-TEMFSFEL                          
114432       MOVE NOO                  TO KEYS-SW                               
114433                                    INDATA-SW                             
114434       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
114440     END-IF                                                               
114500     .                                                                    
114600     EJECT                                                                
123202 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
123203     MOVE 'MFS-ERASE-LINE-F'     TO CURRENT-SECTION                       
123204                                                                          
123206     MOVE MFS-ERASE-FIELD        TO MOD-KDCMD(INDX)                       
123207                                    MOD-IDDISTR(INDX)                     
123208                                    MOD-RETRPFAC-AIR(INDX)                
123209                                    MOD-PRWEIGHT-AIR(INDX)                
123210                                    MOD-PRHAZMAT-AIR(INDX)                
123220                                    MOD-RETRPFAC-BOAT(INDX)               
123230                                    MOD-PRWEIGHT-BOAT(INDX)               
123240                                    MOD-PRHAZMAT-BOAT(INDX)               
123250                                    MOD-RETRPFAC-ROAD(INDX)               
123260                                    MOD-PRWEIGHT-ROAD(INDX)               
123270                                    MOD-PRHAZMAT-ROAD(INDX)               
123280                                    MOD-REINSFAC-AIR(INDX)                
123290                                    MOD-REINSFAC-BOAT(INDX)               
123300                                    MOD-REINSFAC-ROAD(INDX)               
123301     .                                                                    
123302     SKIP3                                                                
123303                                                                          
123404 MFS-ERASE-FIELD-IN SECTION.                                              
123405     MOVE 'MFS-ERASE-FIELD-'     TO CURRENT-SECTION                       
123406                                                                          
123602     PERFORM VARYING INDX FROM 1 BY +1 UNTIL INDX > MAX-INDX              
123603       MOVE MFS-ERASE-FIELD      TO MOD-KDCMD (INDX)                      
123604     END-PERFORM                                                          
123701                                                                          
124201     MOVE MFS-ERASE-FIELD        TO MOD-IDDISTR-UPD                       
124301                                    MOD-RETRPFAC-AIR-UPD                  
124302                                    MOD-PRWEIGHT-AIR-UPD                  
124303                                    MOD-PRHAZMAT-AIR-UPD                  
124401                                    MOD-RETRPFAC-BOAT-UPD                 
124402                                    MOD-PRWEIGHT-BOAT-UPD                 
124403                                    MOD-PRHAZMAT-BOAT-UPD                 
124501                                    MOD-RETRPFAC-ROAD-UPD                 
124502                                    MOD-PRWEIGHT-ROAD-UPD                 
124503                                    MOD-PRHAZMAT-ROAD-UPD                 
124601                                    MOD-REINSFAC-AIR-UPD2                 
124701                                    MOD-REINSFAC-BOAT-UPD2                
124801                                    MOD-REINSFAC-ROAD-UPD2                
125701     .                                                                    
125801     EJECT                                                                
127801 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
127802     MOVE 'MFS-DONT-TOUCH-F'     TO CURRENT-SECTION                       
127803                                                                          
128001     PERFORM VARYING INDX FROM 1 BY +1 UNTIL INDX > MAX-INDX              
128002       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDCMD   (INDX)                  
128003     END-PERFORM                                                          
128401                                                                          
128501     MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-IDDISTR-UPD                     
128601                                      MOD-RETRPFAC-AIR-UPD                
128701                                      MOD-PRWEIGHT-AIR-UPD                
128801                                      MOD-PRHAZMAT-AIR-UPD                
128901                                      MOD-RETRPFAC-BOAT-UPD               
129001                                      MOD-PRWEIGHT-BOAT-UPD               
129101                                      MOD-PRHAZMAT-BOAT-UPD               
129201                                      MOD-RETRPFAC-ROAD-UPD               
129301                                      MOD-PRWEIGHT-ROAD-UPD               
129501                                      MOD-PRHAZMAT-ROAD-UPD               
129601                                      MOD-REINSFAC-AIR-UPD2               
129701                                      MOD-REINSFAC-BOAT-UPD2              
129801                                      MOD-REINSFAC-ROAD-UPD2              
131600     .                                                                    
131700     EJECT                                                                
132501 MFS-FORM-ATTR SECTION.                                                   
132502     MOVE 'MFS-FORM-ATTR   '     TO CURRENT-SECTION                       
132503                                                                          
132702     PERFORM VARYING INDX FROM 1 BY +1 UNTIL INDX > MAX-INDX              
132703       MOVE MFS-FORMAT-DEFAULT-ATTR TO  MOD-KDCMD  (INDX)                 
132704     END-PERFORM                                                          
133201                                                                          
133301     MOVE MFS-FORMAT-DEFAULT-ATTR  TO MOD-IDDISTR-UPD                     
133401                                      MOD-RETRPFAC-AIR-UPD                
133501                                      MOD-PRWEIGHT-AIR-UPD                
133601                                      MOD-PRHAZMAT-AIR-UPD                
133701                                      MOD-RETRPFAC-BOAT-UPD               
133801                                      MOD-PRWEIGHT-BOAT-UPD               
133901                                      MOD-PRHAZMAT-BOAT-UPD               
134001                                      MOD-RETRPFAC-ROAD-UPD               
134101                                      MOD-PRWEIGHT-ROAD-UPD               
134201                                      MOD-PRHAZMAT-ROAD-UPD               
134401                                      MOD-REINSFAC-AIR-UPD2               
134501                                      MOD-REINSFAC-BOAT-UPD2              
134601                                      MOD-REINSFAC-ROAD-UPD2              
134701     .                                                                    
134801     EJECT                                                                
134920* --- IMS SECTIONS ---                                                    
135001     SKIP3                                                                
135101 IMS-GET-MSG SECTION.                                                     
135102     MOVE 'IMS-GET-MSG  '        TO IMS-SECTION                           
135103                                                                          
135401     MOVE '  QC'                   TO GOOD-STATUSCODES                    
135501     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
135601     MOVE MSG-STATUS-CODE          TO STATUS-WS                           
135701     PERFORM IMS-STATUSCHECK                                              
135801     .                                                                    
135901     SKIP3                                                                
136001 IMS-INSERT-MSG SECTION.                                                  
136002     MOVE 'IMS-INSERT-MS'        TO IMS-SECTION                           
136003                                                                          
136301     MOVE LOW-VALUE                TO MSG-KDZ1 MSG-KDZ2                   
136401     MOVE SPACE                    TO GOOD-STATUSCODES                    
136501     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
136601     MOVE MSG-STATUS-CODE          TO STATUS-WS                           
136701     PERFORM IMS-STATUSCHECK                                              
136801     .                                                                    
136901     EJECT                                                                
137001 IMS-GU-WDR201 SECTION.                                                   
137002     MOVE 'IMS-GU-WDR201'        TO IMS-SECTION                           
137003                                                                          
137201     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
137301          DELIMITED BY SIZE INTO SSA1                                     
137401     MOVE '  GE' TO GOOD-STATUSCODES                                      
137503     CALL CBLTDLI USING GU 4591-PCB DLI-IO-WDGX01 SSA1                    
137601     MOVE 4591-STATUS-CODE TO STATUS-WS                                   
137701     PERFORM IMS-STATUSCHECK                                              
137801     .                                                                    
137901     EJECT                                                                
138001     SKIP3                                                                
138101 IMS-GNP-WDGX4592 SECTION.                                                
138102     MOVE 'IMS-GNP-WDGX4592'     TO IMS-SECTION                           
138103                                                                          
138301     STRING 'WDGX4592(IDDISTR >=' W-IDDISTR-MIN-X                         
138401                    '&IDDISTR <=' W-IDDISTR-MAX-X ')'                     
138501            DELIMITED BY SIZE INTO SSA1                                   
138601     MOVE '  GE'                 TO GOOD-STATUSCODES                      
138701     CALL CBLTDLI USING GNP 4591-PCB DLI-IO-WDGX4592 SSA1                 
138801     MOVE 4591-STATUS-CODE TO STATUS-WS                                   
138901     PERFORM IMS-STATUSCHECK                                              
139001     .                                                                    
139101     SKIP3                                                                
139102 IMS-GHU-WDGX4592 SECTION.                                                
139103     MOVE 'IMS-GHU-WDGX4592'     TO IMS-SECTION                           
139104                                                                          
139107     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
139108         DELIMITED BY SIZE INTO SSA1                                      
139110     STRING 'WDGX4592(IDDISTR  =' W-IDDISTR-X ')'                         
139111          DELIMITED BY SIZE INTO SSA2                                     
139115     MOVE '  GE'                 TO GOOD-STATUSCODES                      
139116     CALL CBLTDLI USING GHU 4591-PCB DLI-IO-WDGX4592 SSA1 SSA2            
139117     MOVE 4591-STATUS-CODE TO STATUS-WS                                   
139118     PERFORM IMS-STATUSCHECK                                              
139120     .                                                                    
139130     SKIP3                                                                
139201 IMS-DLET-WDGX4592 SECTION.                                               
139202     MOVE 'IMS-DLET-WDGX459'     TO IMS-SECTION                           
139203                                                                          
139401     MOVE 'WDGX4592 ' TO SSA1                                             
139502     MOVE '  ' TO GOOD-STATUSCODES                                        
139601     CALL CBLTDLI USING DLET 4591-PCB DLI-IO-WDGX4592 SSA1                
139701     MOVE 4591-STATUS-CODE         TO STATUS-WS                           
139801     PERFORM IMS-STATUSCHECK                                              
139901     .                                                                    
140001     EJECT                                                                
140003 IMS-ISRT-WDR201 SECTION.                                                 
140004     MOVE 'IMS-ISRT-WDR201'        TO IMS-SECTION                         
140005                                                                          
140007     MOVE 'WDR201'            TO SSA1                                     
140008     MOVE '  II' TO GOOD-STATUSCODES                                      
140009     CALL CBLTDLI USING ISRT 4591-PCB DLI-IO-WDGX01 SSA1                  
140010     MOVE 4591-STATUS-CODE TO STATUS-WS                                   
140011     PERFORM IMS-STATUSCHECK                                              
140012     .                                                                    
140013     EJECT                                                                
140014     SKIP3                                                                
140102 IMS-ISRT-WDGX4592 SECTION.                                               
140103     MOVE 'IMS-ISRT-WDGX459'     TO IMS-SECTION                           
140104                                                                          
140205     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
140206         DELIMITED BY SIZE INTO SSA1                                      
141002     MOVE 'WDGX4592'            TO SSA2                                   
141003     MOVE '    ' TO GOOD-STATUSCODES                                      
141103     CALL CBLTDLI USING ISRT 4591-PCB DLI-IO-WDGX4592 SSA1 SSA2           
141203     MOVE 4591-STATUS-CODE TO STATUS-WS                                   
141302     PERFORM IMS-STATUSCHECK                                              
141402     .                                                                    
141502     SKIP3                                                                
141601 IMS-REPL-WDGX4592 SECTION.                                               
141602     MOVE 'IMS-REPL-WDGX459'     TO IMS-SECTION                           
141603                                                                          
141801     MOVE '  ' TO GOOD-STATUSCODES                                        
141901     CALL CBLTDLI USING REPL 4591-PCB DLI-IO-WDGX4592                     
142001     MOVE 4591-STATUS-CODE TO STATUS-WS                                   
142101     PERFORM IMS-STATUSCHECK                                              
142201     .                                                                    
142301     SKIP3                                                                
142401 IMS-GU-WDB601 SECTION.                                                   
142402     MOVE 'IMS-GU-WDB601   '     TO IMS-SECTION                           
142403                                                                          
142701     STRING 'WDB601  (IDDC     =' W-IDDC-X    ')'                         
142801          DELIMITED BY SIZE INTO SSA-WDB6                                 
142901     MOVE '  GE' TO GOOD-STATUSCODES                                      
143001     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA-WDB6                
143101     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
143201     PERFORM IMS-STATUSCHECK                                              
143601     .                                                                    
143701     EJECT                                                                
143801 IMS-STATUSCHECK SECTION.                                                 
144001     SET STATUS-IX TO 1                                                   
144101     SEARCH GOOD-STATUS                                                   
144201       AT END                                                             
144301         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
144401         DELIMITED BY SIZE INTO ERROR-TEXT                                
144501         CALL FELLOG                                                      
144601       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
144701         CONTINUE                                                         
145001     END-SEARCH                                                           
150001     .                                                                    
