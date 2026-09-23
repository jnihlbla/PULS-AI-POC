000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W5104600.                                                
000400 AUTHOR.         THOMAS LARSSON.                                          
000500 DATE-WRITTEN.   95/03/23.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET LÄSER INFIL MED INFORMATION OM VILKA                  
001100*        TRANSAKTIONER SOM SKALL RENSAS PÅ WDR8-BASEN.                    
001200*                                                                         
001300*        DET ÄR OK ATT SEGMENTET SAKNAS.                                  
001400*        DETTA FÖR ATT MAN LÄTT SKALL KUNNA ÅTERSTARTA MED                
001500*        MED SAMMA INFIL VID EN EVENTUELL ABEND.                          
001600*                                                                         
001700*        PROGRAMMET UPPATERAR WLFILB (WDR8)                               
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  . . . .                                                 
002100*        U1000 -  . . . .                                                 
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- WDR8 TRANSAR SOM SKALL RENSAS                              
003100     SELECT W51039                     ASSIGN TO W51046D1.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W51039                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000     SKIP2                                                                
004100*01  -COPY WDR801          -L.                                            
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004401*    -- CHECKED BY WY2000                                                 
004410     SKIP3                                                                
004500 77  IDPGM                       PIC X(8)    VALUE 'W5104600'.            
004600 77  MSG-IO-AREA-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
004700 77  MSG-IO-AREA                 PIC X(32)   VALUE SPACE.                 
004800 77  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004900 77  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005000 77  CHKP-ANT                    PIC S9(3)   VALUE +0     COMP-3.         
005100 77  CHKP-MAX                    PIC S9(3)   VALUE +100   COMP-3.         
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400                                                                          
005500 77  W51039-EOF-SW               PIC X       VALUE 'N'.                   
005600     88  END-OF-W51039                       VALUE 'J'.                   
005700     EJECT                                                                
005800 01  DYNAMISKA-SUBPROGRAM.                                                
005900*                                                                         
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006300     EJECT                                                                
006400*    --- PARAMETRAR TILL POSTSUM                                          
006500*                                                                         
006600*01  -COPY W0005      -PRE  POSTSUM-                                      
006700     EJECT                                                                
006800 01  FILLER                      PIC X(16)   VALUE 'W51039-AREA'.         
006900                                                                          
007000*01  AREA -COPY WDR801     -PRE INR8-                                     
007100*                                                                         
007200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007300     SKIP3                                                                
007400 01  NYCKLAR-TILL-DLI.                                                    
007500     03  W-WDR801KY-X.                                                    
007600         05  W-IDPGM             PIC X(8)    VALUE SPACE.                 
007700         05  W-TIREGDAT          PIC S9(7)   VALUE ZERO COMP-3.           
007800         05  W-TIKLOCK           PIC S9(9)   VALUE ZERO COMP-3.           
007900         05  W-IDSEKVNR          PIC S9(3)   VALUE ZERO COMP-3.           
008000         05  W-IDSYSTEM          PIC X(4)    VALUE SPACE.                 
008100         05  W-IDPTYP            PIC X(3)    VALUE SPACE.                 
008200         05  W-IDVTYP            PIC X(1)    VALUE SPACE.                 
008300     SKIP2                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FINNS                       VALUE '  '.                  
008700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009000     88  IMS-EJ-OK                           VALUE 'XD'.                  
009100     SKIP2                                                                
009200 01  GODK-STATUSKODER.                                                    
009300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009400     SKIP3                                                                
009500 01  SSA1                        PIC X(64).                               
009600     EJECT                                                                
009700*    --- IMS FUNKTIONSKODER                                               
009800*01  -COPY W0003                                                          
009900     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010200     SKIP3                                                                
010300 01  DLI-IO-AREA.                                                         
010400     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
010500     SKIP3                                                                
010600     03  WLFILB01 REDEFINES IO-AREA.                                      
010700*        05  -COPY WDR801     -PRE FILB-                                  
010800     EJECT                                                                
010900 LINKAGE SECTION.                                                         
011000                                                                          
011100*01  -COPY W0009      -PRE MSG-                                           
011200                                                                          
011300*01  -COPY W0008      -PRE FILB-                                          
011400     05  FILLER                  PIC X.                                   
011500     EJECT                                                                
011600 PROCEDURE DIVISION  USING MSG-PCB FILB-PCB.                              
011610 MAIN SECTION.                                                            
011700     ENTRY 'DLITCBL' USING MSG-PCB FILB-PCB.                              
011800                                                                          
011900     PERFORM A-INIT                                                       
012000                                                                          
012100     PERFORM S01-LAES-W51039                                              
012200     PERFORM UNTIL END-OF-W51039                                          
012300                                                                          
012400       IF CHKP-ANT             > CHKP-MAX                                 
012500         PERFORM X-TAG-CHECKPOINT                                         
012600       END-IF                                                             
012700                                                                          
012800       PERFORM B-BEHANDLA-INPOST                                          
012900                                                                          
013000       PERFORM S01-LAES-W51039                                            
013100     END-PERFORM                                                          
013200                                                                          
013300     PERFORM Z-FINIT                                                      
013400                                                                          
013500     MOVE ZERO                 TO RETURN-CODE                             
013600     GOBACK                                                               
013700     .                                                                    
013800     EJECT                                                                
013900 A-INIT SECTION.                                                          
014000                                                                          
014100     PERFORM IMS-RESTART                                                  
014200                                                                          
014300     OPEN INPUT W51039                                                    
014400                                                                          
014500     MOVE IDPGM                TO POSTSUM-PROGNAMN                        
014600     MOVE +0                   TO  CHKP-ANT                               
014700     .                                                                    
014800     EJECT                                                                
014900 B-BEHANDLA-INPOST      SECTION.                                          
015000                                                                          
015100     MOVE INR8-FIL-IDPGM         TO W-IDPGM                               
015200     MOVE INR8-FIL-TIREGDAT      TO W-TIREGDAT                            
015300     MOVE INR8-FIL-TIKLOCK       TO W-TIKLOCK                             
015400     MOVE INR8-FIL-IDSEKVNR      TO W-IDSEKVNR                            
015500     MOVE INR8-FIL-CT-IDSYSTEM   TO W-IDSYSTEM                            
015600     MOVE INR8-FIL-CT-IDPTYP     TO W-IDPTYP                              
015700     MOVE INR8-FIL-CT-IDVTYP     TO W-IDVTYP                              
015800                                                                          
015900     PERFORM IMS-GET-FILB-FIL                                             
016000     IF SEGMENT-FINNS                                                     
016100       ADD +1                    TO  CHKP-ANT                             
016200       PERFORM IMS-DLET-FILB                                              
016300     END-IF                                                               
016400     .                                                                    
016500     EJECT                                                                
016600 Z-FINIT SECTION.                                                         
016700                                                                          
016800     CLOSE W51039                                                         
016900                                                                          
017000     MOVE 'S'                  TO POSTSUM-OPKOD                           
017100     CALL POSTSUM              USING POSTSUM-PARM                         
017200     .                                                                    
017300     EJECT                                                                
017400 S01-LAES-W51039  SECTION.                                                
017500                                                                          
017600     READ W51039 INTO INR8-AREA                                           
017700     AT END                                                               
017800        SET END-OF-W51039      TO TRUE                                    
017900                                                                          
018000     NOT AT END                                                           
018100        MOVE 'W51039'          TO POSTSUM-FDNAMN                          
018200        MOVE 'W51046D1'        TO POSTSUM-DDNAMN2                         
018300        MOVE INR8-FIL-CT-IDPTYP                                           
018400                               TO POSTSUM-TRANSTYP                        
018500        CALL POSTSUM           USING POSTSUM-PARM                         
018600     END-READ                                                             
018700     .                                                                    
018800     EJECT                                                                
018900 X-TAG-CHECKPOINT   SECTION.                                              
019000                                                                          
019100     PERFORM IMS-CHECKPOINT                                               
019200     MOVE +0                   TO  CHKP-ANT                               
019300     .                                                                    
019400     EJECT                                                                
019500* --- IMS SEKTIONER ---                                                   
019600                                                                          
019700 IMS-GET-FILB-FIL SECTION.                                                
019800                                                                          
019900     STRING 'WLFILB01(WDR801KY =' W-WDR801KY-X ')'                        
020000          DELIMITED BY SIZE INTO SSA1                                     
020100     MOVE '  GE'               TO GODK-STATUSKODER                        
020200     CALL CBLTDLI USING GHU FILB-PCB DLI-IO-AREA SSA1                     
020300     MOVE FILB-STATUS-CODE     TO STATUS-WS                               
020400     PERFORM IMS-STATUS-KONTROLL                                          
020500     .                                                                    
020600     SKIP2                                                                
020700 IMS-DLET-FILB SECTION.                                                   
020800                                                                          
020900     MOVE '  '                 TO GODK-STATUSKODER                        
021000     CALL CBLTDLI USING DLET FILB-PCB DLI-IO-AREA                         
021100     MOVE FILB-STATUS-CODE     TO STATUS-WS                               
021200     PERFORM IMS-STATUS-KONTROLL                                          
021300     .                                                                    
021400     EJECT                                                                
021500 IMS-RESTART SECTION.                                                     
021600                                                                          
021700     MOVE SPACE                TO MSG-IO-AREA                             
021800     MOVE '  '                 TO GODK-STATUSKODER                        
021900     CALL CBLTDLI USING        XRST MSG-PCB                               
022000                               MSG-IO-AREA-LENGTH MSG-IO-AREA             
022100                               CHKP-AREA-LENGTH CHKP-AREA                 
022200     MOVE MSG-STATUS-CODE      TO STATUS-WS                               
022300     PERFORM IMS-STATUS-KONTROLL                                          
022400     .                                                                    
022500     EJECT                                                                
022600 IMS-CHECKPOINT SECTION.                                                  
022700                                                                          
022800     MOVE SPACE TO MSG-IO-AREA                                            
022900     MOVE '  XD' TO GODK-STATUSKODER                                      
023000     CALL CBLTDLI USING CHKP MSG-PCB                                      
023100                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
023200                        CHKP-AREA-LENGTH CHKP-AREA                        
023300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
023400     PERFORM IMS-STATUS-KONTROLL                                          
023500                                                                          
023600     IF IMS-EJ-OK                                                         
023700       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
023800       CALL FELLOG                                                        
023900     END-IF                                                               
024000     .                                                                    
024100     SKIP2                                                                
024200 IMS-STATUS-KONTROLL SECTION.                                             
024300                                                                          
024400     SET STATUS-IX TO 1                                                   
024500     SEARCH GODK-STATUS                                                   
024600       AT END CALL FELLOG                                                 
024700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
024800     END-SEARCH                                                           
024900     .                                                                    
