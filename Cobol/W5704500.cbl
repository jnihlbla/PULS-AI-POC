000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5704500.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   11/05/12.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        SB FOR WDL6                                                      
000900*                                                                         
001200 ENVIRONMENT DIVISION.                                                    
001400 INPUT-OUTPUT SECTION.                                                    
001600 FILE-CONTROL.                                                            
001700     SKIP2                                                                
001800*          --- EXTRACT R30 FROM WDL6 LEVNR 1441                           
001900     SELECT UTFIL                      ASSIGN TO W57045D1.                
001910*          --- EXTRACT R30 310 FROM WDL6 AK LEVNR 1441                    
001920     SELECT UTFIL2                     ASSIGN TO W57045D2.                
001930*          --- EXTRACT R30 310 R31 FROM WDL6 AK KINA AND US               
001940     SELECT UTFIL3                     ASSIGN TO W57045D3.                
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200     SKIP2                                                                
002300 FILE SECTION.                                                            
002400     SKIP3                                                                
002500 FD  UTFIL                                                                
002600     RECORDING       F                                                    
002700     BLOCK CONTAINS  0.                                                   
002800                                                                          
002900*01  POST -COPY W57045 -PRE  UT-  -L.                                     
003000     EJECT                                                                
003010 FD  UTFIL2                                                               
003020     RECORDING       F                                                    
003030     BLOCK CONTAINS  0.                                                   
003040                                                                          
003050*01  POST -COPY W57045 -PRE  UT2-  -L.                                    
003060     EJECT                                                                
003070 FD  UTFIL3                                                               
003080     RECORDING       F                                                    
003090     BLOCK CONTAINS  0.                                                   
003091                                                                          
003092*01  POST -COPY W57045 -PRE  UT3-  -L.                                    
003093     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300 77  IDPGM                       PIC X(8)    VALUE 'W5704500'.            
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003510 77  WS-PRARTNTO                 PIC S9(7)V9(2) VALUE ZERO                
003511                                             COMP-3.                      
003520 77  WS-KDTRADP                  PIC X(4)       VALUE SPACE.              
003700                                                                          
003800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
003900 01  FILLER REDEFINES DAGENS-DATUM.                                       
004000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004300                                                                          
004400 01  DYNAMISKA-SUBPROGRAM.                                                
004500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004700     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
004800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005000                                                                          
005100 01  FELTEXT.                                                             
005200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005400     EJECT                                                                
005500*01  -COPY WWDC99                                                         
005600     EJECT                                                                
005610*01  -COPY WWDIST07                                                       
005620     EJECT                                                                
005700*    --- PARAMETRAR TILL POSTSUM                                          
005800*                                                                         
005900*01  -COPY W0005   -PRE  POSTSUM-                                         
006000     EJECT                                                                
006100 01  UT-AREA-START               PIC X(24)   VALUE                        
006200                                 'UT-AREA-START  '.                       
006400*01  AREA -COPY W57045     -PRE UT-                                       
006500     EJECT                                                                
006501                                                                          
006510 01  UT2-AREA-START               PIC X(24)   VALUE                       
006520                                 'UT2-AREA-START  '.                      
006540*01  AREA -COPY W57045     -PRE UT2-                                      
006541                                                                          
006542 01  UT3-AREA-START               PIC X(24)   VALUE                       
006543                                 'UT3-AREA-START  '.                      
006544*01  AREA -COPY W57045     -PRE UT3-                                      
006545                                                                          
006550     EJECT                                                                
006600*    --- STATUS-KOD FRÅN IMS                                              
006700 01  STATUS-WS                   PIC XX.                                  
006800     88  SEGMENT-FINNS                       VALUE '  '.                  
006900     88  BASEN-SLUT                          VALUE 'GB'.                  
007000                                                                          
007100 01  GODK-STATUSKODER.                                                    
007200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
007300     SKIP3                                                                
007400 01  SSA1                        PIC X(160).                              
007410 01  SSA2                        PIC X(160).                              
007500     EJECT                                                                
007600*    --- IMS FUNKTIONSKODER                                               
007700*01  -COPY W0003                                                          
007800     EJECT                                                                
007801 01  KEYS-FOR-DLI.                                                        
007802     03  WS-IDDC-X.                                                       
007803         05  WS-IDDC          PIC X(2)    VALUE SPACE.                    
007804                                                                          
007810 01  NYCKLAR-TILL-DLI.                                                    
007820     03  W-IDARTNR-X.                                                     
007830         05  W-IDARTNR       PIC S9(9)   VALUE +0  COMP-3.                
007840     03  W-IDDC-X.                                                        
007850         05  W-IDDC          PIC X(2)    VALUE SPACES.                    
007860     EJECT                                                                
007900*    ---  DLI INPUT-OUTPUT AREA                                           
008000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL6'.                        
008100 01  DLI-IO-WDL6.                                                         
008200     03 IO-AREA     PIC X(600) VALUE SPACE.                               
008300         03 DLI-IO-WDL601 REDEFINES IO-AREA.                              
008400*            05 -COPY WDL601                                              
008500     EJECT                                                                
008600         03 DLI-IO-WDL611 REDEFINES IO-AREA.                              
008700*            05 -COPY WDL611                                              
008701 01  FILLER                    PIC X(16)  VALUE 'WDK711'.                 
008702*01  WDK711   -COPY WDK711                                                
008703     EJECT                                                                
008704 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
008705 01  DLI-IO-WDB601.                                                       
008706*    03  -COPY WDB601                                                     
008707     EJECT                                                                
008800 LINKAGE SECTION.                                                         
008900                                                                          
009000*01  -COPY W0008  -PRE WDL6-                                              
009100     05  FILLER                  PIC X.                                   
009110*01  -COPY W0008  -PRE WDK7-                                              
009120     05  FILLER                  PIC X.                                   
009130*01  -COPY W0008  -PRE WDB6-                                              
009140     05  FILLER                  PIC X.                                   
009200     EJECT                                                                
009300 PROCEDURE DIVISION  USING WDL6-PCB WDK7-PCB WDB6-PCB.                    
009400 MAIN SECTION.                                                            
009500     ENTRY 'DLITCBL' USING WDL6-PCB WDK7-PCB WDB6-PCB.                    
009600                                                                          
009700     PERFORM A-INIT                                                       
009800                                                                          
009900     PERFORM IMS-GET-WDL6                                                 
010000     PERFORM UNTIL BASEN-SLUT                                             
010100       EVALUATE WDL6-SEG-NAME-FB                                          
010200         WHEN 'WDL601'                                                    
010300           MOVE ART-IDARTNR TO UT-IDARTNR                                 
010310                               UT2-IDARTNR                                
010320                               UT3-IDARTNR                                
010330                               W-IDARTNR                                  
010400         WHEN 'WDL611'                                                    
010410           PERFORM S12-GET-KDTRADP                                        
010500           PERFORM B-URVAL                                                
010600       END-EVALUATE                                                       
010700       PERFORM IMS-GET-WDL6                                               
010800     END-PERFORM                                                          
010900                                                                          
011000     PERFORM Z-FINIT                                                      
011100     MOVE ZERO TO RETURN-CODE                                             
011200     GOBACK                                                               
011300     .                                                                    
011400     EJECT                                                                
011410                                                                          
011500 A-INIT SECTION.                                                          
011600     OPEN OUTPUT UTFIL                                                    
011610                 UTFIL2                                                   
011620                 UTFIL3                                                   
011700     MOVE IDPGM    TO POSTSUM-PROGNAMN                                    
011800     .                                                                    
011900     EJECT                                                                
011910                                                                          
012000 B-URVAL SECTION.                                                         
012100     IF INL-IDPTYP = 'R30'                                                
012200       MOVE INL-IDDC                TO WS-IDDC                            
012300       MOVE INL-IDDISTR             TO DIST07-IDDISTR                     
012310       IF INL-IDLEVNR = '1441'                                            
012320          IF INL-TIINLMOT = ZERO                                          
012330             IF XDC-NON-VCC-OWNED OR LDC-CN                               
012400               MOVE INL-IDDC        TO UT-IDDC                            
012500               MOVE INL-DAINLEV     TO UT-DAINLEV                         
012600               MOVE INL-PRARTNTO    TO UT-PRARTNTO                        
012700               MOVE INL-KVAVIS      TO UT-KVAVIS                          
012710               MOVE INL-KDVALISO    TO UT-KDVALISO                        
012720               MOVE WS-KDTRADP      TO UT-KDTRADP                         
012800               PERFORM S11-SKRIV-UTFIL                                    
012801             END-IF                                                       
012802          ELSE                                                            
012803             IF XDC-NON-VCC-OWNED OR LDC-CN                               
012805               MOVE INL-IDDC        TO UT2-IDDC                           
012806               MOVE INL-DAINLEV     TO UT2-DAINLEV                        
012807               MOVE INL-PRARTNTO    TO UT2-PRARTNTO                       
012808               MOVE INL-KVAVIS      TO UT2-KVAVIS                         
012809               MOVE INL-KDVALISO    TO UT2-KDVALISO                       
012810               MOVE WS-KDTRADP      TO UT2-KDTRADP                        
012811               PERFORM S11-SKRIV-UTFIL-2                                  
012812             END-IF                                                       
012813          END-IF                                                          
012814       ELSE                                                               
012815          IF XDC-NON-VCC-OWNED OR LDC-CN OR NDC-US                        
012816**** IF RETURNS TO CHINA, TAKE AVERAGE COST                               
012817             IF DIST07-KINA                                               
012818               PERFORM S12-GET-AVG-COST                                   
012819               MOVE WS-PRARTNTO     TO UT3-PRARTNTO                       
012820             ELSE                                                         
012821               MOVE INL-PRARTNTO    TO UT3-PRARTNTO                       
012822             END-IF                                                       
012823             MOVE INL-IDDC          TO UT3-IDDC                           
012824             MOVE INL-DAINLEV       TO UT3-DAINLEV                        
012825             MOVE INL-KVAVIS        TO UT3-KVAVIS                         
012826             MOVE INL-KDVALISO      TO UT3-KDVALISO                       
012827             MOVE WS-KDTRADP        TO UT3-KDTRADP                        
012828             PERFORM S11-SKRIV-UTFIL-3                                    
012829          END-IF                                                          
012830       END-IF                                                             
012900     END-IF                                                               
013010     IF INL-IDPTYP = '310'                                                
013020         MOVE INL-IDDC              TO WS-IDDC                            
013030         MOVE INL-IDDISTR           TO DIST07-IDDISTR                     
013040         IF INL-IDLEVNR = '1441'                                          
013050            IF XDC-NON-VCC-OWNED OR LDC-CN                                
013094               MOVE INL-IDDC        TO UT2-IDDC                           
013095               MOVE INL-DAINLEV     TO UT2-DAINLEV                        
013096               MOVE INL-PRARTNTO    TO UT2-PRARTNTO                       
013097               MOVE INL-KVAVIS      TO UT2-KVAVIS                         
013098               MOVE INL-KDVALISO    TO UT2-KDVALISO                       
013099               MOVE WS-KDTRADP      TO UT2-KDTRADP                        
013100               PERFORM S11-SKRIV-UTFIL-2                                  
013101            END-IF                                                        
013102         ELSE                                                             
013103            IF XDC-NON-VCC-OWNED OR LDC-CN OR NDC-US                      
013104**** IF RETURNS TO CHINA, TAKE AVERAGE COST                               
013105              IF DIST07-KINA                                              
013106                PERFORM S12-GET-AVG-COST                                  
013107                MOVE WS-PRARTNTO    TO UT3-PRARTNTO                       
013112              ELSE                                                        
013113                MOVE INL-PRARTNTO   TO UT3-PRARTNTO                       
013114              END-IF                                                      
013116              MOVE INL-IDDC         TO UT3-IDDC                           
013117              MOVE INL-DAINLEV      TO UT3-DAINLEV                        
013118              MOVE INL-KVAVIS       TO UT3-KVAVIS                         
013119              MOVE INL-KDVALISO     TO UT3-KDVALISO                       
013120              MOVE WS-KDTRADP       TO UT3-KDTRADP                        
013121              PERFORM S11-SKRIV-UTFIL-3                                   
013122            END-IF                                                        
013123         END-IF                                                           
013124     END-IF                                                               
013125     IF INL-IDPTYP = 'R31'                                                
013126        MOVE INL-IDDC          TO WS-IDDC                                 
013127        MOVE INL-IDDISTR       TO DIST07-IDDISTR                          
013128        IF INL-IDLEVNR = '1441'                                           
013129           IF XDC-NON-VCC-OWNED OR LDC-CN                                 
013130              CONTINUE                                                    
013131           END-IF                                                         
013132         ELSE                                                             
013133           IF XDC-NON-VCC-OWNED OR LDC-CN OR NDC-US                       
013134**** EXCLUDE DISTRICTS GREATER THAN 9999                                  
013135**** THESE ARE NOT VALID DISTRICTS                                        
013136             IF INL-IDDISTR > 9999                                        
013137               CONTINUE                                                   
013138             ELSE                                                         
013139**** IF RETURNS TO CHINA, TAKE AVERAGE COST                               
013140               IF DIST07-KINA                                             
013141                 PERFORM S12-GET-AVG-COST                                 
013142                 MOVE WS-PRARTNTO   TO UT3-PRARTNTO                       
013143               ELSE                                                       
013144                 MOVE INL-PRARTNTO  TO UT3-PRARTNTO                       
013145               END-IF                                                     
013146              MOVE INL-IDDC         TO UT3-IDDC                           
013147              MOVE INL-DAINLEV      TO UT3-DAINLEV                        
013148              MOVE INL-KVAVIS       TO UT3-KVAVIS                         
013149              MOVE INL-KDVALISO     TO UT3-KDVALISO                       
013150              MOVE WS-KDTRADP       TO UT3-KDTRADP                        
013151              PERFORM S11-SKRIV-UTFIL-3                                   
013152            END-IF                                                        
013153         END-IF                                                           
013154     END-IF                                                               
013155     IF INL-IDPTYP = 'R31'                                                
013156        MOVE INL-IDDC          TO WS-IDDC                                 
013157        MOVE INL-IDDISTR       TO DIST07-IDDISTR                          
013158        IF INL-IDLEVNR = '1441'                                           
013159           IF XDC-NON-VCC-OWNED OR LDC-CN                                 
013160*              CONTINUE                                                   
013161               MOVE INL-IDDC        TO UT3-IDDC                           
013162               MOVE INL-DAINLEV     TO UT3-DAINLEV                        
013163               MOVE INL-KVAVIS      TO UT3-KVAVIS                         
013164               MOVE INL-KDVALISO    TO UT3-KDVALISO                       
013165               MOVE WS-KDTRADP      TO UT3-KDTRADP                        
013166               PERFORM S11-SKRIV-UTFIL-3                                  
013167             END-IF                                                       
013168           END-IF                                                         
013169         END-IF                                                           
013170     END-IF                                                               
013180     .                                                                    
013200     EJECT                                                                
013210                                                                          
013300 Z-FINIT SECTION.                                                         
013400     CLOSE UTFIL                                                          
013410     CLOSE UTFIL2                                                         
013420     CLOSE UTFIL3                                                         
013500     MOVE 'S' TO POSTSUM-OPKOD                                            
013600     CALL POSTSUM USING POSTSUM-PARM                                      
013700     .                                                                    
013800     SKIP3                                                                
013810                                                                          
013900 S11-SKRIV-UTFIL SECTION.                                                 
014000     WRITE UT-POST FROM UT-AREA                                           
014100     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
014200     MOVE 'UTFIL'    TO POSTSUM-FDNAMN                                    
014300     MOVE 'W57045D1' TO POSTSUM-DDNAMN2                                   
014400     CALL POSTSUM USING POSTSUM-PARM                                      
014500     .                                                                    
014600     EJECT                                                                
014601                                                                          
014610 S11-SKRIV-UTFIL-2 SECTION.                                               
014620     WRITE UT2-POST FROM UT2-AREA                                         
014630     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
014640     MOVE 'UTFIL2'   TO POSTSUM-FDNAMN                                    
014650     MOVE 'W57045D2' TO POSTSUM-DDNAMN2                                   
014660     CALL POSTSUM USING POSTSUM-PARM                                      
014670     .                                                                    
014680     EJECT                                                                
014681                                                                          
014690 S11-SKRIV-UTFIL-3 SECTION.                                               
014691     WRITE UT3-POST FROM UT3-AREA                                         
014692     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
014693     MOVE 'UTFIL3'   TO POSTSUM-FDNAMN                                    
014694     MOVE 'W57045D3' TO POSTSUM-DDNAMN2                                   
014695     CALL POSTSUM USING POSTSUM-PARM                                      
014696     .                                                                    
014697     EJECT                                                                
014698 S12-GET-AVG-COST SECTION.                                                
014800                                                                          
014802     MOVE INL-IDDC        TO W-IDDC                                       
014803     PERFORM IMS-GU-WDK711                                                
014804     IF SEGMENT-FINNS                                                     
014805       MOVE SLAG-PRAVCOST TO WS-PRARTNTO                                  
014806     ELSE                                                                 
014807       MOVE ZERO          TO WS-PRARTNTO                                  
014808     END-IF                                                               
014809     .                                                                    
014810     EJECT                                                                
014811 S12-GET-KDTRADP SECTION.                                                 
014812     MOVE INL-IDDC             TO WS-IDDC                                 
014813     PERFORM IMS-GU-WDB601                                                
014814     IF SEGMENT-FINNS                                                     
014815       MOVE DCS-KDTRADP         TO WS-KDTRADP                             
014823     ELSE                                                                 
014817       MOVE SPACES              TO WS-KDTRADP                             
014832     END-IF                                                               
014833     .                                                                    
014834     EJECT                                                                
014840* --- IMS SEKTIONER ---                                                   
014900 IMS-GET-WDL6 SECTION.                                                    
015000     CALL CBLTDLI USING GN WDL6-PCB DLI-IO-WDL6                           
015100     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
015200     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
015300     PERFORM IMS-STATUSKONTROLL                                           
015400     .                                                                    
015500     SKIP3                                                                
015510                                                                          
015520 IMS-GU-WDK711 SECTION.                                                   
015530     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
015540                        DELIMITED  BY SIZE INTO SSA1                      
015550     STRING 'WDK711  (IDDC     =' W-IDDC      ')'                         
015570            DELIMITED BY SIZE INTO SSA2                                   
015580     MOVE '  GEGB' TO GODK-STATUSKODER                                    
015590     CALL CBLTDLI USING GU WDK7-PCB WDK711 SSA1 SSA2                      
015591     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
015592     PERFORM IMS-STATUSKONTROLL                                           
015593     .                                                                    
015594     SKIP3                                                                
015595 IMS-GU-WDB601 SECTION.                                                   
015596                                                                          
015597     STRING 'WDB601  (IDDC     =' WS-IDDC      ')'                        
015598            DELIMITED BY SIZE INTO SSA1                                   
015599     MOVE '  GE'                 TO GODK-STATUSKODER                      
015600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
015601     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
015602     PERFORM IMS-STATUSKONTROLL                                           
015603     .                                                                    
015604     SKIP3                                                                
015605                                                                          
015610 IMS-STATUSKONTROLL SECTION.                                              
015700     SET STATUS-IX TO 1                                                   
015800     SEARCH GODK-STATUS                                                   
015900       AT END                                                             
016000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016100           DELIMITED BY SIZE INTO FELTEXT                                 
016200         DISPLAY FELTEXT                                                  
016300         CALL FELLOG                                                      
016400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
016500         CONTINUE                                                         
016600     END-SEARCH                                                           
016700     .                                                                    
