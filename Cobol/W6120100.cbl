000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6120100.                                                
000300 AUTHOR.         SANTHOSHKUMAR ANGAMUTHU.                                 
000400 DATE-WRITTEN.   08/10/20.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM CREATES THE REPORTS FOR PREPACKING.                 
000900*                                                                         
001000*                                                                         
001100*                                                                         
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- VALID DISTRICTS FROM WDB6                                  
002600     SELECT W61201                     ASSIGN TO W61201D1.                
002700                                                                          
002800 DATA DIVISION.                                                           
002900     SKIP2                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W61201                                                               
003300     RECORDING       V                                                    
003400     BLOCK CONTAINS  0.                                                   
003500     EJECT                                                                
003600 01  UT-POST                             PIC X(300).                      
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'W6120100'.            
004000 77  WS-NE-VA                    PIC 9(4)V9.                              
004100 77  W-SEC-PALL-TYP              PIC 9(3).                                
004200 77  WS-SEC-TIME                 PIC 9(4)V9.                              
004300 77  WS-WAIT-TIME                PIC 9(4)V9.                              
004400 77  WS-TIME-LINE                PIC 9(7)V9.                              
004500 77  WS-MINS-TOT                 PIC 9(7)V9.                              
004600 77  BAL-MINS                    PIC 9(7).                                
004700 77  WS-TOT-TIME-LINE            PIC 9(7)V9.                              
004800 77  WS-HRS-TOT                  PIC 9(7).                                
004900 01  NEW-TIME                    PIC 9(4)V9(4).                           
005000 01  FILLER REDEFINES NEW-TIME.                                           
005100     03  NEW-TIME-HH             PIC 9(4).                                
005200     03  NEW-TIME-MM             PIC 9(4).                                
005300 01  WS-NE-VA                    PIC 9(4)V9999.                           
005400 01  FILLER REDEFINES WS-NE-VA.                                           
005500     03  WS-NE-VA-01             PIC 9(4).                                
005600     03  WS-NE-VA-02             PIC 9(4).                                
005700 77  NEW-TIME-HH-ZS              PIC Z(3)9.                               
005800 01  NEW-MINS                    PIC 9(2)V9(2).                           
005900 01  FILLER REDEFINES NEW-MINS.                                           
006000     03  NEW-MINS-MM             PIC 9(2).                                
006100     03  NEW-MINS-SS             PIC 9(2).                                
006200 01  WS-TIME.                                                             
006300     03 WS-TIME-HH               PIC Z(2)9.                               
006400     03 FILLER                   PIC X(1)    VALUE ':'.                   
006500     03 WS-TIME-MM               PIC 9(2).                                
006600 01  TOT-NEW-TIME                PIC 9(4)V9(4).                           
006700 01  FILLER REDEFINES TOT-NEW-TIME.                                       
006800     03  TOT-NEW-TIME-HH         PIC 9(4).                                
006900     03  TOT-NEW-TIME-MM         PIC 9(4).                                
007000*77  TOT-NEW-TIME-HH-ZS          PIC Z(3)9.                               
007100 01  TOT-NEW-MINS                PIC 9(2)V9(2).                           
007200 01  FILLER REDEFINES TOT-NEW-MINS.                                       
007300     03  TOT-NEW-MINS-MM         PIC 9(2).                                
007400     03  TOT-NEW-MINS-SS         PIC 9(2).                                
007500 01  WS-TOT-TIME.                                                         
007600     03 WS-TOT-TIME-HH           PIC Z(7)9.                               
007700     03 FILLER                   PIC X(1)    VALUE ':'.                   
007800     03 WS-TOT-TIME-MM           PIC 9(2).                                
007900 77  W-HOURS                     PIC 9(2).                                
008000 77  W-MINUTES                   PIC 9(2).                                
008100 77  W-REM-MINUTES               PIC 9(4).                                
008200                                                                          
008300 77  YES                         PIC X       VALUE 'J'.                   
008400 77  NOO                         PIC X       VALUE 'N'.                   
008500 77  WS-REFILL                   PIC X(6)    VALUE 'REFILL'.              
008600 77  WS-RETUR                    PIC X(6)    VALUE 'RETURN'.              
008700 77  WS-QRETUR                   PIC X(14)   VALUE                        
008800                                             'QUALITY RETURN'.            
008900 77  WS-IDLANDX2                 PIC X(2)    VALUE SPACE.                 
009000 77  WS-CLAG-IDARTNR-EMBQ0       PIC 9(9).                                
009100 77  WS-CLAG-IDARTNR-EMBQ1       PIC 9(9).                                
009200 77  WS-CLAG-IDARTNR-EMBQ2       PIC 9(9).                                
009300 77  W-QTY-EMBQ0                 PIC 9(5).                                
009400 77  W-QTY-EMBQ1                 PIC 9(5).                                
009500 77  W-QTY-EMBQ2                 PIC 9(5).                                
009600     EJECT                                                                
009700 01  WS-KDFORP                   PIC X(3)    VALUE SPACE.                 
009800 01  FILLER REDEFINES WS-KDFORP.                                          
009900    03 WS-KDFORPGP               PIC 9(2).                                
010000    03 WS-KDFORPUF               PIC 9.                                   
010100                                                                          
010200 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
010300 01  FILLER REDEFINES TODAYS-DATE.                                        
010400     03  TODAYS-DATE-YEAR        PIC 9(2).                                
010500     03  TODAYS-DATE-MONTH       PIC 9(2).                                
010600     03  TODAYS-DATE-DAY         PIC 9(2).                                
010700     EJECT                                                                
010800 01  GENERAL-SUBPROGRAMS.                                                 
010900*                                                                         
011000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
011100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011400     SKIP2                                                                
011500*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
011600                                                                          
011700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
011900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012000     SKIP2                                                                
012100 01  ERROR-TEXT.                                                          
012200     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
012300     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
012400     EJECT                                                                
012500*    --- PARAMETRAR TILL POSTSUM                                          
012600*                                                                         
012700*01  -COPY W0005   -PRE  POSTSUM-                                         
012800     EJECT                                                                
012900 01  UT-AREA-START               PIC X(24)   VALUE                        
013000                                 'UT-AREA-START  '.                       
013100*01  AREA  -COPY W61201  -PRE UT-                                         
013200     SKIP2                                                                
013300 01  FILLER                       PIC X(16)  VALUE 'PREPACK'.             
013400*   -COPY W6120101                                                        
013500     EJECT                                                                
013600*    --- AREAS FOR IMS-SECTIONS                                           
013700*                                                                         
013800     EJECT                                                                
013900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014000     SKIP3                                                                
014100 01  KEYS-FOR-DLI.                                                        
014200     03  W-IDDC-X.                                                        
014300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
014400                                                                          
014500*--------W6D1                                                             
014600     03  W-W6D101KY-MIN-X.                                                
014700         05  W-INL-IDDC-MIN        PIC  X(2)  VALUE SPACE.                
014800         05  W-INL-IDLEVNR-MIN     PIC X(5)   VALUE SPACE.                
014900         05  W-INL-IDFS-MIN        PIC X(8)   VALUE SPACE.                
015000         05  W-INL-TIAVIDAT-MIN    PIC S9(7)  VALUE ZERO COMP-3.          
015100                                                                          
015200     03  W-W6D101KY-MAX-X.                                                
015300         05  W-INL-IDDC-MAX        PIC  X(2)  VALUE SPACE.                
015400         05  W-INL-IDLEVNR-MAX     PIC X(5)   VALUE SPACE.                
015500         05  W-INL-IDFS-MAX        PIC X(8)   VALUE SPACE.                
015600         05  W-INL-TIAVIDAT-MAX    PIC S9(7)  VALUE ZERO COMP-3.          
015700                                                                          
015800     03  W-IDRADNR-INL-X.                                                 
015900         05  W-IDRADNR-INL        PIC S9(5)   COMP-3.                     
016000                                                                          
016010     03  W-IDRADNR-W6D121-INL-X.                                          
016020         05  W-IDRADNR-W6D121-INL  PIC S9(5)   COMP-3.                    
016030                                                                          
016100*--------WDK6                                                             
016200     03  W-IDARTNR-X.                                                     
016300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016400 01  UT2-FOOTER.                                                          
016500     03  FILLER                  PIC X(01)   VALUE ';'.                   
016600     03  FILLER                  PIC X(01)   VALUE ';'.                   
016700     03  FILLER                  PIC X(01)   VALUE ';'.                   
016800     03  FILLER                  PIC X(01)   VALUE ';'.                   
016900     03  FILLER                  PIC X(01)   VALUE ';'.                   
017000     03  FILLER                  PIC X(01)   VALUE ';'.                   
017100     03  FILLER                  PIC X(01)   VALUE ';'.                   
017200     03  FILLER                  PIC X(01)   VALUE ';'.                   
017300     03  FILLER                  PIC X(01)   VALUE ';'.                   
017400     03  FILLER                  PIC X(01)   VALUE ';'.                   
017500     03  FILLER                  PIC X(01)   VALUE ';'.                   
017600     03  FILLER                  PIC X(01)   VALUE ';'.                   
017700     03  FILLER                  PIC X(01)   VALUE ';'.                   
017800     03  FILLER                  PIC X(01)   VALUE ';'.                   
017900     03  FILLER                  PIC X(01)   VALUE ';'.                   
018000     03  FILLER                  PIC X(01)   VALUE ';'.                   
018100     03  FILLER                  PIC X(21)   VALUE                        
018200                                        'TOTAL TID IN AK QUEUE'.          
018300     03  FILLER                  PIC X(01)   VALUE ';'.                   
018400     03  UT-TOT-TITIDPAK         PIC X(11).                               
018500     03  FILLER                  PIC X(01)   VALUE ';'.                   
018600     03  UT-TOT-TITIDPAK-MIN     PIC Z(6)9.9.                             
018700     03  FILLER                  PIC X(01)   VALUE ';'.                   
018800*                                                                         
018900 01  UT2-HEADER.                                                          
019000     03  FILLER                  PIC X(09)   VALUE 'PLACERING'.           
019100     03  FILLER                  PIC X(01)   VALUE ';'.                   
019200     03  FILLER                  PIC X(13)   VALUE                        
019300                                             'ARTIKELNUMMER'.             
019400     03  FILLER                  PIC X(01)   VALUE ';'.                   
019500     03  FILLER                  PIC X(10)   VALUE 'LEVERANTÖR'.          
019600     03  FILLER                  PIC X(01)   VALUE ';'.                   
019700     03  FILLER                  PIC X(05)   VALUE 'KOLLI'.               
019800     03  FILLER                  PIC X(01)   VALUE ';'.                   
019900     03  FILLER                  PIC X(04)   VALUE 'VAGN'.                
020000     03  FILLER                  PIC X(01)   VALUE ';'.                   
020100     03  FILLER                  PIC X(11)   VALUE 'PARTINUMMER'.         
020200     03  FILLER                  PIC X(01)   VALUE ';'.                   
020300     03  FILLER                  PIC X(09)   VALUE 'BENÄMNING'.           
020400     03  FILLER                  PIC X(01)   VALUE ';'.                   
020500     03  FILLER                  PIC X(05)   VALUE 'ANTAL'.               
020600     03  FILLER                  PIC X(01)   VALUE ';'.                   
020700     03  FILLER                  PIC X(15)   VALUE                        
020800                                             'FÖRPACKNINGSKOD'.           
020900     03  FILLER                  PIC X(01)   VALUE ';'.                   
021000     03  FILLER                  PIC X(11)   VALUE 'EMBALLAGE-0'.         
021100     03  FILLER                  PIC X(01)   VALUE ';'.                   
021200     03  FILLER                  PIC X(11)   VALUE 'EMBALLAGE-1'.         
021300     03  FILLER                  PIC X(01)   VALUE ';'.                   
021400     03  FILLER                  PIC X(11)   VALUE 'EMBALLAGE-2'.         
021500     03  FILLER                  PIC X(01)   VALUE ';'.                   
021600     03  FILLER                  PIC X(09)   VALUE 'EMB.BRIST'.           
021700     03  FILLER                  PIC X(01)   VALUE ';'.                   
021800     03  FILLER                  PIC X(14)   VALUE                        
021900                                             'RESTORDERANTAL'.            
022000     03  FILLER                  PIC X(01)   VALUE ';'.                   
022100     03  FILLER                  PIC X(07)   VALUE 'PALLTYP'.             
022200     03  FILLER                  PIC X(01)   VALUE ';'.                   
022300     03  FILLER                  PIC X(15)   VALUE                        
022400                                            'FÖRPACKNINGSTYP'.            
022500     03  FILLER                  PIC X(01)   VALUE ';'.                   
022600     03  FILLER                  PIC X(02)   VALUE 'Q3'.                  
022700     03  FILLER                  PIC X(01)   VALUE ';'.                   
022800     03  FILLER                  PIC X(17)   VALUE                        
022900                                      'TOTAL TID (HH:MM)'.                
023000     03  FILLER                  PIC X(01)   VALUE ';'.                   
023100     03  FILLER                  PIC X(15)   VALUE                        
023200                                      'TOTAL TID (MIN)'.                  
023300     03  FILLER                  PIC X(01)   VALUE ';'.                   
023400                                                                          
023500     SKIP2                                                                
023600*    --- STATUS-KOD FRÅN IMS                                              
023700 01  STATUS-WS                   PIC XX.                                  
023800     88  SEGMENT-FOUND                       VALUE '  '.                  
023900     88  SEGMENT-MISSING                     VALUE 'GB'.                  
024000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024100     SKIP2                                                                
024200 01  GOOD-STATUSCODES.                                                    
024300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024400     SKIP3                                                                
024500 01  SSA1                        PIC X(112).                              
024600 01  SSA2                        PIC X(112).                              
024700     EJECT                                                                
024800*    --- IMS FUNCTION CODES                                               
024900*01  -COPY W0003                                                          
025000     EJECT                                                                
025100*    ---  DLI INPUT-OUTPUT AREA                                           
025200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB6'.           
025300 01  DLI-IO-AREA                 PIC X(900).                              
025400 01  DLI-IO-AREA-601     REDEFINES DLI-IO-AREA.                           
025500*    03  -COPY WDB601                                                     
025600     EJECT                                                                
025700 01  DLI-IO-AREA-616     REDEFINES DLI-IO-AREA.                           
025800*    03  -COPY WDB616                                                     
025900     EJECT                                                                
026000 01  FILLER                      PIC X(16)   VALUE 'W6D101 AREA'.         
026100 01   DLI-IO-W6D101.                                                      
026200*     03  -COPY W6D101                                                    
026300                                                                          
026400 01  FILLER                      PIC X(16)   VALUE 'W6D111 AREA'.         
026500 01   DLI-IO-W6D111.                                                      
026600*     03  -COPY W6D111                                                    
026700                                                                          
026800                                                                          
026900 01  FILLER                      PIC X(16)   VALUE 'W6D121 AREA'.         
027000 01   DLI-IO-W6D121.                                                      
027100*     03  -COPY W6D121                                                    
027200                                                                          
027300 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK601'.        
027400     SKIP3                                                                
027500 01  DLI-IO-WDK601.                                                       
027600*        05  -COPY WDK601 -PRE WDK601                                     
027700                                                                          
027800 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK611'.        
027900     SKIP3                                                                
028000 01  DLI-IO-WDK611.                                                       
028100*        05  -COPY WDK611                                                 
028200     EJECT                                                                
028300 LINKAGE SECTION.                                                         
028400                                                                          
028500                                                                          
028600*01  -COPY W0008  -PRE W6D1-                                              
028700     05  FILLER                  PIC X.                                   
028800     EJECT                                                                
028900*01  -COPY W0008  -PRE WDK6-                                              
029000     05  FILLER                  PIC X.                                   
029100     EJECT                                                                
029200 PROCEDURE DIVISION  USING W6D1-PCB WDK6-PCB.                             
029300 MAIN SECTION.                                                            
029400     ENTRY 'DLITCBL' USING W6D1-PCB WDK6-PCB.                             
029500                                                                          
029600                                                                          
029700     PERFORM A-INIT                                                       
029800     PERFORM BC-SPARA-FRAN-INLA                                           
029900     PERFORM Z-FINIT                                                      
030000                                                                          
030100     MOVE ZERO TO RETURN-CODE                                             
030200     GOBACK                                                               
030300     .                                                                    
030400     EJECT                                                                
030500 A-INIT SECTION.                                                          
030600                                                                          
030700     OPEN OUTPUT W61201                                                   
030800     ACCEPT TODAYS-DATE  FROM DATE                                        
030900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
031000     .                                                                    
031100     EJECT                                                                
031200 BC-SPARA-FRAN-INLA SECTION.                                              
031300     MOVE LOW-VALUE            TO W-W6D101KY-MIN-X                        
031400     MOVE HIGH-VALUE           TO W-W6D101KY-MAX-X                        
031500                                                                          
031600     MOVE '11'          TO W-INL-IDDC-MIN                                 
031700                           W-INL-IDDC-MAX                                 
031800                           W-IDDC                                         
031900*                                                                         
032000     MOVE ZERO          TO WS-TOT-TIME-LINE                               
032100                                                                          
032200* WRITE HEADER                                                            
032300     MOVE UT2-HEADER TO UT-POST                                           
032400     PERFORM S10-WRITE-W61201                                             
032500* WRITE HEADER END                                                        
032600                                                                          
032700     PERFORM IMS-GN-W6D101                                                
032800     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-MISSING                      
032900      IF SEGMENT-FOUND                                                    
033000        IF (INL-KDINL = 'R31' OR '310')                                   
033005                                                                          
033010                                                                          
033100            MOVE 1    TO W-IDRADNR-INL                                    
033200            PERFORM IMS-GNP-W6D111-ARTNR                                  
033300            PERFORM UNTIL SEGMENT-SAKNAS                                  
033400              IF SEGMENT-FOUND                                            
033500                 IF ART-TIUPPDAT = 0                                      
033510                    MOVE ART-IDRADNR-INL TO W-IDRADNR-W6D121-INL          
033520                                            W-IDRADNR-INL                 
033600                    PERFORM IMS-GNP-W6D121                                
033700                    PERFORM UNTIL SEGMENT-SAKNAS                          
033800                       IF SEGMENT-FOUND                                   
033900                          IF RAD-ADINLOMR > ' '                           
034000                             MOVE ART-IDARTNR      TO W-IDARTNR           
034100                             PERFORM IMS-GU-WDK611                        
034200                             PERFORM B-MOVE-VALUES                        
034300                          END-IF                                          
034400                       END-IF                                             
034600                       PERFORM IMS-GNP-W6D121                             
034700                    END-PERFORM                                           
034800                 END-IF                                                   
034900              END-IF                                                      
035000              ADD 1 TO W-IDRADNR-INL                                      
035100              PERFORM IMS-GNP-W6D111-ARTNR-NEXT                           
035200            END-PERFORM                                                   
035300        END-IF                                                            
035400      END-IF                                                              
035500      PERFORM IMS-GN-W6D101                                               
035600     END-PERFORM                                                          
035700                                                                          
035800** TOTAL TIME                                                             
035900     COMPUTE WS-MINS-TOT = WS-TOT-TIME-LINE                               
036000     COMPUTE WS-HRS-TOT = (WS-TOT-TIME-LINE / 60 )                        
036100     COMPUTE BAL-MINS = WS-MINS-TOT - (60 * WS-HRS-TOT)                   
036200     MOVE WS-HRS-TOT      TO WS-TOT-TIME-HH                               
036300     MOVE BAL-MINS        TO WS-TOT-TIME-MM                               
036400                                                                          
036500     MOVE WS-TOT-TIME     TO UT-TOT-TITIDPAK                              
036600     MOVE WS-TOT-TIME-LINE TO UT-TOT-TITIDPAK-MIN                         
036700     MOVE UT2-FOOTER TO UT-POST                                           
036800     PERFORM S10-WRITE-W61201                                             
036900     .                                                                    
037000     EJECT                                                                
037100                                                                          
037200 B-MOVE-VALUES      SECTION.                                              
037300                                                                          
037400     MOVE RAD-ADINLOMR    TO UT-ADINLOMR                                  
037500     MOVE ART-IDARTNR     TO UT-IDARTNR                                   
037600     MOVE INL-IDLEVNR     TO UT-IDLEVNR-KOLLI                             
037700     MOVE RAD-IDOKOLLI    TO UT-IDOKOLLI                                  
037800     MOVE RAD-IDINLVGN    TO UT-IDINLVGN                                  
037900     MOVE ART-IDLOPNRM    TO UT-IDLOPNRM                                  
038000     MOVE ART-BEART       TO UT-BEART                                     
038100     MOVE RAD-KVINLART    TO UT-KVAVIS                                    
038200     MOVE CLAG-KDFORPGP   TO WS-KDFORPGP                                  
038300     MOVE CLAG-KDFORPUF   TO WS-KDFORPUF                                  
038400     MOVE WS-KDFORP       TO UT-KDFORP                                    
038500     MOVE CLAG-IDARTNR-EMBQ0                                              
038600                          TO UT-IDARTNR-EMBQ0                             
038700     MOVE CLAG-IDARTNR-EMBQ1                                              
038800                          TO UT-IDARTNR-EMBQ1                             
038900     MOVE CLAG-IDARTNR-EMBQ2                                              
039000                          TO UT-IDARTNR-EMBQ2                             
039100     MOVE CLAG-KVROS      TO UT-KVROS                                     
039200     MOVE CLAG-IDARTNR-EMBQ3                                              
039300                          TO UT-IDARTNR-EMBQ3                             
039400     MOVE CLAG-BEFT       TO UT-BEFT                                      
039500     MOVE CLAG-KVQPACK-3  TO UT-KVQPACK-3                                 
039600     MOVE ZERO            TO WS-CLAG-IDARTNR-EMBQ0                        
039700                             WS-CLAG-IDARTNR-EMBQ1                        
039800                             WS-CLAG-IDARTNR-EMBQ2                        
039900                                                                          
040000     IF CLAG-IDARTNR-EMBQ0 > 0 AND CLAG-KVQPACK-0 > 0                     
040100        COMPUTE W-QTY-EMBQ0 ROUNDED                                       
040200                          = RAD-KVINLART / CLAG-KVQPACK-0                 
040300        MOVE CLAG-IDARTNR-EMBQ0 TO WS-CLAG-IDARTNR-EMBQ0                  
040400     END-IF                                                               
040500                                                                          
040600     IF CLAG-IDARTNR-EMBQ1 > 0 AND CLAG-KVQPACK-1 > 0                     
040700        COMPUTE W-QTY-EMBQ1 ROUNDED                                       
040800                          = RAD-KVINLART / CLAG-KVQPACK-1                 
040900        MOVE CLAG-IDARTNR-EMBQ1 TO WS-CLAG-IDARTNR-EMBQ1                  
041000     END-IF                                                               
041100                                                                          
041200     IF CLAG-IDARTNR-EMBQ2 > 0 AND CLAG-KVQPACK-2 > 0                     
041300        COMPUTE W-QTY-EMBQ2 ROUNDED                                       
041400                          = RAD-KVINLART / CLAG-KVQPACK-2                 
041500        MOVE CLAG-IDARTNR-EMBQ2 TO WS-CLAG-IDARTNR-EMBQ2                  
041600     END-IF                                                               
041700                                                                          
041800     MOVE ZERO TO WS-SEC-TIME WS-TIME-LINE                                
041900                                                                          
042000     SEARCH ALL FPCODE-TIME                                               
042100        AT END                                                            
042200          CONTINUE                                                        
042300       WHEN PREPACK-SOK-FPCODE(FPCODE-IX1) = WS-KDFORP                    
042400         MOVE PREPACK-SOK-SECTIM(FPCODE-IX1)                              
042500                          TO WS-SEC-TIME                                  
042600         MOVE PREPACK-SOK-WAITIM(FPCODE-IX1)                              
042700                          TO WS-WAIT-TIME                                 
042800     END-SEARCH                                                           
042900     MOVE ZERO            TO WS-TIME-LINE                                 
043000     MOVE ZERO            TO UT-TIM-MIN                                   
043100     MOVE ZERO            TO WS-TIME-HH WS-TIME-MM                        
043200                                                                          
043300     IF WS-SEC-TIME > 0 OR WS-SEC-TIME > 0                                
043400     COMPUTE WS-TIME-LINE = (WS-SEC-TIME * RAD-KVINLART)                  
043500                                         + WS-WAIT-TIME                   
043600** NEW LOGIC TO ADD TIME! Q3 DETAILS                                      
043700     IF CLAG-IDARTNR-EMBQ3 > 0                                            
043800       EVALUATE CLAG-IDARTNR-EMBQ3                                        
043900         WHEN 211                                                         
044000           MOVE 240 TO W-SEC-PALL-TYP                                     
044100         WHEN 212                                                         
044200           MOVE 300 TO W-SEC-PALL-TYP                                     
044300         WHEN 213                                                         
044400           MOVE 360 TO W-SEC-PALL-TYP                                     
044500         WHEN 214                                                         
044600           MOVE 420 TO W-SEC-PALL-TYP                                     
044700         WHEN OTHER                                                       
044800           MOVE 180 TO W-SEC-PALL-TYP                                     
044900       END-EVALUATE                                                       
045000                                                                          
045100       IF CLAG-KVQPACK-3 > 0                                              
045200         MOVE ZERO TO WS-NE-VA                                            
045300            COMPUTE WS-NE-VA = RAD-KVINLART / CLAG-KVQPACK-3              
045400            IF WS-NE-VA > 0                                               
045500               IF WS-NE-VA-02 > 0                                         
045600                  COMPUTE WS-NE-VA-01 = WS-NE-VA-01 + 1                   
045700               END-IF                                                     
045800            END-IF                                                        
045900            COMPUTE WS-TIME-LINE = WS-TIME-LINE +                         
046000                        (WS-NE-VA-01 * W-SEC-PALL-TYP)                    
046100       ELSE                                                               
046200         COMPUTE WS-TIME-LINE = WS-TIME-LINE + W-SEC-PALL-TYP             
046300       END-IF                                                             
046400     ELSE                                                                 
046500****     IF PALLET TYPE = 0 THEN YOU SHOULD ALSO ADD 180 SEC              
046600         MOVE 180 TO W-SEC-PALL-TYP                                       
046700         COMPUTE WS-TIME-LINE = WS-TIME-LINE + W-SEC-PALL-TYP             
046800     END-IF                                                               
046900                                                                          
047000     COMPUTE WS-MINS-TOT = WS-TIME-LINE / 60                              
047100     MOVE WS-MINS-TOT TO UT-TIM-MIN                                       
047200** FOR TOTAL TIME IN FOOTER                                               
047300     COMPUTE WS-TOT-TIME-LINE = WS-TOT-TIME-LINE + WS-MINS-TOT            
047400** FOR TOTAL TIME IN FOOTER - END                                         
047500     COMPUTE WS-HRS-TOT = (WS-TIME-LINE / 60 ) / 60                       
047600     COMPUTE BAL-MINS = WS-MINS-TOT - (60 * WS-HRS-TOT)                   
047700     MOVE WS-HRS-TOT      TO WS-TIME-HH                                   
047800     MOVE BAL-MINS        TO WS-TIME-MM                                   
047900                                                                          
048000     END-IF                                                               
048100     MOVE WS-TIME         TO UT-TITIDPAK                                  
048200     MOVE SPACES          TO UT-EMBINFO                                   
048300     IF WS-CLAG-IDARTNR-EMBQ0 > 0                                         
048400         MOVE WS-CLAG-IDARTNR-EMBQ0 TO W-IDARTNR                          
048500         PERFORM IMS-GU-WDK611                                            
048600         IF SEGMENT-FOUND                                                 
048700            IF CLAG-KVLS < W-QTY-EMBQ0                                    
048800              MOVE 'EMB.BRIST' TO UT-EMBINFO                              
048900            END-IF                                                        
049000         END-IF                                                           
049100     END-IF                                                               
049200     IF WS-CLAG-IDARTNR-EMBQ1 > 0                                         
049300         MOVE WS-CLAG-IDARTNR-EMBQ1 TO W-IDARTNR                          
049400         PERFORM IMS-GU-WDK611                                            
049500         IF SEGMENT-FOUND                                                 
049600            IF CLAG-KVLS < W-QTY-EMBQ1                                    
049700              MOVE 'EMB.BRIST' TO UT-EMBINFO                              
049800            END-IF                                                        
049900         END-IF                                                           
050000     END-IF                                                               
050100     IF WS-CLAG-IDARTNR-EMBQ2 > 0                                         
050200         MOVE WS-CLAG-IDARTNR-EMBQ2 TO W-IDARTNR                          
050300         PERFORM IMS-GU-WDK611                                            
050400         IF SEGMENT-FOUND                                                 
050500            IF CLAG-KVLS < W-QTY-EMBQ2                                    
050600              MOVE 'EMB.BRIST' TO UT-EMBINFO                              
050700            END-IF                                                        
050800         END-IF                                                           
050900     END-IF                                                               
051000                                                                          
051100     MOVE ';'             TO UT-FILLER1                                   
051200                             UT-FILLER2                                   
051300                             UT-FILLER3                                   
051400                             UT-FILLER4                                   
051500                             UT-FILLER5                                   
051600                             UT-FILLER6                                   
051700                             UT-FILLER7                                   
051800                             UT-FILLER8                                   
051900                             UT-FILLER9                                   
052000                             UT-FILLER10                                  
052100                             UT-FILLER11                                  
052200                             UT-FILLER12                                  
052300                             UT-FILLER13                                  
052400                             UT-FILLER14                                  
052500                             UT-FILLER15                                  
052600                             UT-FILLER16                                  
052700                             UT-FILLER17                                  
052800                             UT-FILLER18                                  
052900                                                                          
053000     PERFORM S11-WRITE-W61201                                             
053100     .                                                                    
053200     EJECT                                                                
053300 S10-WRITE-W61201 SECTION.                                                
053400                                                                          
053500     WRITE UT-POST                                                        
053600                                                                          
053700     MOVE 'W61201'   TO POSTSUM-FDNAMN                                    
053800     MOVE 'W61201D1' TO POSTSUM-DDNAMN2                                   
053900     CALL POSTSUM USING POSTSUM-PARM                                      
054000     .                                                                    
054100     EJECT                                                                
054200                                                                          
054300                                                                          
054400 S11-WRITE-W61201 SECTION.                                                
054500                                                                          
054600     WRITE UT-POST  FROM UT-AREA                                          
054700                                                                          
054800     MOVE 'W61201'   TO POSTSUM-FDNAMN                                    
054900     MOVE 'W61201D1' TO POSTSUM-DDNAMN2                                   
055000     CALL POSTSUM USING POSTSUM-PARM                                      
055100     .                                                                    
055200     EJECT                                                                
055300                                                                          
055400 Z-FINIT SECTION.                                                         
055500     CLOSE W61201                                                         
055600     .                                                                    
055700     EJECT                                                                
055800 S99-ABEND SECTION.                                                       
055900                                                                          
056000     SKIP2                                                                
056100     MOVE 'S'        TO POSTSUM-OPKOD                                     
056200     CALL POSTSUM USING POSTSUM-PARM                                      
056300     CALL ABEND   USING RKOD-ABEND                                        
056400     .                                                                    
056500     EJECT                                                                
056600* --- IMS SECTIONS  ---                                                   
056700                                                                          
056800                                                                          
056900 IMS-GU-W6D101 SECTION.                                                   
057000                                                                          
057100     STRING 'W6D101  (W6D101KY>=' W-W6D101KY-MIN-X                        
057200                    '&W6D101KY<=' W-W6D101KY-MAX-X ')'                    
057300          DELIMITED BY SIZE INTO SSA1                                     
057400     MOVE '  GE'          TO GOOD-STATUSCODES                             
057500     CALL CBLTDLI USING GU  W6D1-PCB  DLI-IO-W6D101 SSA1                  
057600     MOVE W6D1-STATUS-CODE  TO STATUS-WS                                  
057700     PERFORM IMS-STATUSCHECK                                              
057800     .                                                                    
057900     EJECT                                                                
058000*----------------------------------------------------------------*        
058100 IMS-GN-W6D101 SECTION.                                                   
058200                                                                          
058300     STRING 'W6D101  (IDDC     =' W-IDDC ')'                              
058400          DELIMITED BY SIZE INTO SSA1                                     
058500     MOVE '  GEGB'          TO GOOD-STATUSCODES                           
058600     CALL CBLTDLI USING GN  W6D1-PCB  DLI-IO-W6D101 SSA1                  
058700     MOVE W6D1-STATUS-CODE  TO STATUS-WS                                  
058800     PERFORM IMS-STATUSCHECK                                              
058900     .                                                                    
059000     EJECT                                                                
059100*----------------------------------------------------------------*        
059200 IMS-GNP-W6D111-ARTNR SECTION.                                            
059300     STRING 'W6D111  *F(IDRADNRI>=' W-IDRADNR-INL-X ')'                   
059400          DELIMITED BY SIZE INTO SSA1                                     
059500     MOVE '  GE'          TO GOOD-STATUSCODES                             
059600     CALL CBLTDLI USING GNP W6D1-PCB DLI-IO-W6D111 SSA1                   
059700     MOVE W6D1-STATUS-CODE  TO STATUS-WS                                  
059800     PERFORM IMS-STATUSCHECK                                              
059900     .                                                                    
060000     EJECT                                                                
060010*----------------------------------------------------------------*        
060020 IMS-GNP-W6D111-ARTNR-NEXT SECTION.                                       
060030     STRING 'W6D111  *F(IDRADNRI>=' W-IDRADNR-INL-X ')'                   
060040          DELIMITED BY SIZE INTO SSA1                                     
060050     MOVE '  GE'          TO GOOD-STATUSCODES                             
060060     CALL CBLTDLI USING GNP W6D1-PCB DLI-IO-W6D111 SSA1                   
060070     MOVE W6D1-STATUS-CODE  TO STATUS-WS                                  
060080     PERFORM IMS-STATUSCHECK                                              
060090     .                                                                    
060100     EJECT                                                                
060200*----------------------------------------------------------------*        
060210 IMS-GNP-W6D121  SECTION.                                                 
060300     STRING 'W6D111  (IDRADNRI =' W-IDRADNR-W6D121-INL-X ')'              
060400          DELIMITED BY SIZE INTO SSA1                                     
060500     MOVE 'W6D121  '      TO SSA2                                         
060600     MOVE '  GE'          TO GOOD-STATUSCODES                             
060700     CALL CBLTDLI USING GNP W6D1-PCB DLI-IO-W6D121 SSA1 SSA2              
060800     MOVE W6D1-STATUS-CODE  TO STATUS-WS                                  
060900     PERFORM IMS-STATUSCHECK                                              
061000     .                                                                    
061100     EJECT                                                                
061200*----------------------------------------------------------------*        
061293 IMS-GU-WDK611   SECTION.                                                 
061294                                                                          
061295     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
061296          DELIMITED BY SIZE INTO SSA1                                     
061297     MOVE 'WDK611  ' TO SSA2                                              
061298     MOVE '  GE' TO GOOD-STATUSCODES                                      
061299     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
061300     MOVE WDK6-STATUS-CODE  TO STATUS-WS                                  
061400     PERFORM IMS-STATUSCHECK                                              
061500     .                                                                    
061600     SKIP3                                                                
061700*----------------------------------------------------------------*        
061800 IMS-STATUSCHECK SECTION.                                                 
061900                                                                          
062000     SET STATUS-IX TO 1                                                   
062100     SEARCH GOOD-STATUS                                                   
062200       AT END                                                             
062300         STRING ' INVALID STATUS CODE FROM IMS:' STATUS-WS                
062400           DELIMITED BY SIZE INTO ERROR-TEXT                              
062500         CALL FELLOG                                                      
062600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
062700         CONTINUE                                                         
062800     END-SEARCH                                                           
062900     .                                                                    
