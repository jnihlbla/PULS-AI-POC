001100 ID DIVISION.                                                             
001300 PROGRAM-ID.     W1223000.                                                
001400 AUTHOR.         BODIL LINDAHL.                                           
001500 DATE-WRITTEN.   061025.                                                  
001510 DATE-COMPILED.                                                           
001600                                                                          
001900*    FUNKTION:                                                            
002000*        PROGRAMMET LÄSER W12207 SOM SKAPAS I PGM W12204.                 
002100*        BÅDE ARTIKLAR SOM SKA SKALAS/RENSAS WDK6 TAS BORT                
002200*        FRÅN WDF212.                                                     
002300*                                                                         
002410*        PROGRAMMET UPPDATERAR WDF2.                                      
002500*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP2                                                                
003400 INPUT-OUTPUT SECTION.                                                    
003500                                                                          
003600 FILE-CONTROL.                                                            
003701     SKIP2                                                                
003702*          --- ARTIKLAR SOM SKA RENSAS/SKALAS WDK6                        
003710     SELECT W12207                     ASSIGN TO W12230D1.                
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004200 FILE SECTION.                                                            
004301     SKIP3                                                                
004302 FD  W12207                                                               
004303     RECORDING       F                                                    
004304     BLOCK CONTAINS  0.                                                   
004305                                                                          
004310*01  -COPY W12207      -L.                                                
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600     SKIP2                                                                
004601                                                                          
004610*    -- CHECKED BY WY2000                                                 
004700 77  IDPGM                       PIC X(8)    VALUE 'W1223000'.            
004800 01  CHKP-VAR.                                                            
004900 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005000 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005100 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005200 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005300 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005400 03  CHKP-MAX                    PIC S9(3)   VALUE +200.                  
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005600 77  W-DLET-WDF212               PIC 9(7)    VALUE ZERO.                  
005700     SKIP2                                                                
005800 01  FELTEXT.                                                             
005900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006201                                                                          
006202 77  W12207-EOF-SW               PIC X       VALUE 'N'.                   
006210     88  END-OF-W12207                       VALUE 'J'.                   
006500     EJECT                                                                
007200 01  DYNAMISKA-SUBPROGRAM.                                                
007300*                                                                         
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007610     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007701     EJECT                                                                
007702*    --- PARAMETRAR TILL POSTSUM                                          
007703*                                                                         
007710*01  -COPY W0005   -PRE  POSTSUM-                                         
008001     EJECT                                                                
008002 01  IN-AREA-START               PIC X(24)   VALUE                        
008003                                             'IN-AREA-START'.             
008004     SKIP2                                                                
008005                                                                          
008010*01  AREA -COPY W12207     -PRE IN-                                       
008100*                                                                         
008200     EJECT                                                                
008300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008400     SKIP3                                                                
008500 01  NYCKLAR-TILL-DLI.                                                    
008601     03  W-IDARTNR-X.                                                     
008610         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008693     03  W-WDF2A1KY-MIN-X.                                                
008694         05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
008695         05  W-IDLEVNR-MIN       PIC  X(5)   VALUE SPACE.                 
008697     03  W-WDF2A1KY-MAX-X.                                                
008698         05  W-IDARTNR-MAX       PIC S9(9)   VALUE ZERO COMP-3.           
008699         05  W-IDLEVNR-MAX       PIC  X(5)   VALUE SPACE.                 
008705     03  W-WDF201KY-X.                                                    
008706         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
008707         05  W-IDDIRGRP          PIC X(10)   VALUE SPACE.                 
008710     EJECT                                                                
008800*    --- STATUS-KOD FRÅN IMS                                              
008900 01  STATUS-WS                   PIC XX.                                  
009000     88  SEGMENT-FINNS                       VALUE '  '.                  
009100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009400     88  IMS-EJ-OK                           VALUE 'XD'.                  
009500     SKIP2                                                                
009600 01  GODK-STATUSKODER.                                                    
009700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009800     SKIP3                                                                
009900 01  SSA1                        PIC X(64).                               
010000 01  SSA2                        PIC X(64).                               
010100     EJECT                                                                
010200*    --- IMS FUNKTIONSKODER                                               
010300*01  -COPY W0003                                                          
010400     EJECT                                                                
010600*    ---  DLI INPUT-OUTPUT AREA                                           
010610 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF201'.                      
010620 01  DLI-IO-WDF201.                                                       
010630*    03  -COPY WDF201.                                                    
010640     EJECT                                                                
010690 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF212'.                      
010700 01  DLI-IO-WDF212.                                                       
010800*    03  -COPY WDF212.                                                    
010900     EJECT                                                                
011000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF2A'.                       
011100 01  DLI-IO-WDF2A.                                                        
011200*    03  -COPY WDF2A1                                                     
011300     EJECT                                                                
012200 LINKAGE SECTION.                                                         
012400*01  -COPY W0009  -PRE MSG-                                               
012500     EJECT                                                                
012502*01  -COPY W0008  -PRE WDF2-                                              
012510     05  FILLER                  PIC X.                                   
012800     EJECT                                                                
012900*01  -COPY W0008  -PRE WDF2A-                                             
012901     05  FILLER                  PIC X.                                   
012902     EJECT                                                                
012903 PROCEDURE DIVISION  USING MSG-PCB WDF2-PCB WDF2A-PCB.                    
012904 MAIN SECTION.                                                            
012910     ENTRY 'DLITCBL' USING MSG-PCB WDF2-PCB WDF2A-PCB.                    
013000                                                                          
013300     PERFORM A-INIT                                                       
013400                                                                          
013410     PERFORM S01-LAES-W12207                                              
013500     PERFORM UNTIL END-OF-W12207                                          
013801       IF IN-UTFIL-TYP = 'S' OR 'B'                                       
013802          MOVE LOW-VALUE  TO W-WDF2A1KY-MIN-X                             
013803          MOVE HIGH-VALUE TO W-WDF2A1KY-MAX-X                             
013820          MOVE IN-IDARTNR TO W-IDARTNR-MIN                                
013830                             W-IDARTNR-MAX                                
013840                             W-IDARTNR                                    
013850          PERFORM IMS-GET-WDF2A-FIRST                                     
013860          PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                    
013870             MOVE SEQA-IDLEVNR TO W-IDLEVNR                               
013880             MOVE SEQA-IDDIRGRP TO W-IDDIRGRP                             
014011             PERFORM IMS-GHU-WDF212                                       
014100*****        DISPLAY 'DLET ' W-IDARTNR                                    
014110             PERFORM IMS-DLET-WDF2                                        
                   ADD 1 TO W-DLET-WDF212                                       
014200             ADD +1 TO CHKP-ANT                                           
014302             PERFORM IMS-GET-WDF2A-NEXT                                   
014306          END-PERFORM                                                     
014310          IF CHKP-ANT > CHKP-MAX                                          
014320             PERFORM X-TAG-CHECKPOINT                                     
014330          END-IF                                                          
014400       END-IF                                                             
014510       PERFORM S01-LAES-W12207                                            
014600     END-PERFORM                                                          
014800                                                                          
014900     PERFORM Z-FINIT                                                      
015100     MOVE ZERO TO RETURN-CODE                                             
015200     GOBACK                                                               
015300     .                                                                    
015400     EJECT                                                                
015500 A-INIT SECTION.                                                          
015700                                                                          
015800     PERFORM IMS-RESTART                                                  
016010     OPEN INPUT W12207                                                    
016710     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017000     .                                                                    
017200     EJECT                                                                
017300 Z-FINIT SECTION.                                                         
017801                                                                          
           DISPLAY 'ANTAL BORTTAGNA WDF212: ' W-DLET-WDF212                     
017810     CLOSE W12207                                                         
018002     MOVE 'S' TO POSTSUM-OPKOD                                            
018010     CALL POSTSUM USING POSTSUM-PARM                                      
018200     .                                                                    
018301     EJECT                                                                
018302 S01-LAES-W12207  SECTION.                                                
018303                                                                          
018304     READ W12207 INTO IN-AREA                                             
018305     AT END                                                               
018307        SET END-OF-W12207 TO TRUE                                         
018308                                                                          
018309     NOT AT END                                                           
018310        MOVE 'W12207'     TO POSTSUM-FDNAMN                               
018311        MOVE 'W12226D1'   TO POSTSUM-DDNAMN2                              
018312        MOVE IN-UTFIL-TYP TO POSTSUM-TRANSTYP                             
018313        CALL POSTSUM USING POSTSUM-PARM                                   
018316     END-READ                                                             
018320     .                                                                    
018600     EJECT                                                                
018700 X-TAG-CHECKPOINT SECTION.                                                
018800                                                                          
019400     PERFORM IMS-CHECKPOINT                                               
019500     MOVE ZERO TO CHKP-ANT                                                
019700     .                                                                    
019800     EJECT                                                                
019900* --- IMS SEKTIONER ---                                                   
020000     SKIP3                                                                
020020     EJECT                                                                
020021 IMS-GET-WDF2A-FIRST SECTION.                                             
020030     STRING 'WDF2A1  (WDF2A1KY>=' W-WDF2A1KY-MIN-X                        
020040                    '&WDF2A1KY<=' W-WDF2A1KY-MAX-X ')'                    
020050          DELIMITED BY SIZE INTO SSA1                                     
020060     MOVE '  GEGB' TO GODK-STATUSKODER                                    
020070     CALL CBLTDLI USING GU WDF2A-PCB DLI-IO-WDF2A SSA1                    
020080     MOVE WDF2A-STATUS-CODE TO STATUS-WS                                  
020090     PERFORM IMS-STATUSKONTROLL                                           
020091     .                                                                    
020092     SKIP3                                                                
020094 IMS-GET-WDF2A-NEXT SECTION.                                              
020095     STRING 'WDF2A1  (WDF2A1KY>=' W-WDF2A1KY-MIN-X                        
020096                    '&WDF2A1KY<=' W-WDF2A1KY-MAX-X ')'                    
020097          DELIMITED BY SIZE INTO SSA1                                     
020098     MOVE '  GEGB' TO GODK-STATUSKODER                                    
020099     CALL CBLTDLI USING GN WDF2A-PCB DLI-IO-WDF2A SSA1                    
020100     MOVE WDF2A-STATUS-CODE TO STATUS-WS                                  
020101     PERFORM IMS-STATUSKONTROLL                                           
020102     .                                                                    
020103     SKIP3                                                                
020104 IMS-GET-WDF201 SECTION.                                                  
020106     STRING 'WDF201  (WDF201KY =' W-WDF201KY-X ')'                        
020107          DELIMITED BY SIZE INTO SSA1                                     
020108     MOVE '  GE' TO GODK-STATUSKODER                                      
020109     CALL CBLTDLI USING GU WDF2-PCB DLI-IO-WDF201 SSA1                    
020110     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
020111     PERFORM IMS-STATUSKONTROLL                                           
020112     .                                                                    
020113     EJECT                                                                
020123 IMS-GHU-WDF212 SECTION.                                                  
020124     STRING 'WDF201  (WDF201KY =' W-WDF201KY-X ')'                        
020125          DELIMITED BY SIZE INTO SSA1                                     
020126     STRING 'WDF212  (IDARTNR  =' W-IDARTNR-X ')'                         
020127          DELIMITED BY SIZE INTO SSA2                                     
020128     MOVE '  ' TO GODK-STATUSKODER                                        
020129     CALL CBLTDLI USING GHU WDF2-PCB DLI-IO-WDF212 SSA1 SSA2              
020130     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
020131     PERFORM IMS-STATUSKONTROLL                                           
020132     .                                                                    
020133     SKIP3                                                                
020167 IMS-DLET-WDF2 SECTION.                                                   
020168     MOVE '  ' TO GODK-STATUSKODER                                        
020169     CALL CBLTDLI USING DLET WDF2-PCB DLI-IO-WDF212                       
020170     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
020171     PERFORM IMS-STATUSKONTROLL                                           
020180     .                                                                    
020200     EJECT                                                                
020300 IMS-RESTART SECTION.                                                     
020400     SKIP2                                                                
020500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020600     MOVE '  ' TO GODK-STATUSKODER                                        
020700     CALL CBLTDLI USING XRST MSG-PCB                                      
020800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020900                        CHKP-AREA-LENGTH CHKP-AREA                        
021000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021100     PERFORM IMS-STATUSKONTROLL                                           
021200     .                                                                    
021300     SKIP3                                                                
021400 IMS-CHECKPOINT SECTION.                                                  
021600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021700     MOVE '  XD' TO GODK-STATUSKODER                                      
021800     CALL CBLTDLI USING CHKP MSG-PCB                                      
021900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
022000                        CHKP-AREA-LENGTH CHKP-AREA                        
022100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022200     PERFORM IMS-STATUSKONTROLL                                           
022400     IF IMS-EJ-OK                                                         
022500       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
022600       DISPLAY FELTEXT                                                    
022700       CALL FELLOG                                                        
022800     END-IF                                                               
022900     .                                                                    
023000     SKIP3                                                                
023100 IMS-STATUSKONTROLL SECTION.                                              
023200     SKIP2                                                                
023300     SET STATUS-IX TO 1                                                   
023400     SEARCH GODK-STATUS                                                   
023500       AT END                                                             
023800         CALL FELLOG                                                      
023900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
024000         CONTINUE                                                         
024100     END-SEARCH                                                           
024200     .                                                                    
