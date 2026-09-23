000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2247300.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   13/10/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*       INPUT:                                                            
000810*          PARAMETER FROM SCREEN 2111 - IDLEVNR + KVDAGAR-TT              
000901*          PARAMETER FROM SCREEN 2115 - IDLEVNR + KVVECKOR-AT +           
000902*                                       KVVECKOR-LT                       
000910*                                                                         
001000*       OUTPUT: ALL PARTS CONNECTED TO THE SUPPLIER                       
001100*               FROM THE INPUT FILE.CDC - FOR CN/US SEE W2247400.         
001200*                                                                         
001300*        THE PROGRAM READS     WDK6                                       
001400*                                                                         
001500*    ABENDCODES:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- PARAMETER FROM SCREEN 2111 OR 2115                         
002800     SELECT W22473-PARM                ASSIGN TO W22473D1.                
002900*          --- OUTPUT FILE TO UPDATE WDK6                                 
003000     SELECT W22473                     ASSIGN TO W22473D2.                
003100 DATA DIVISION.                                                           
003200                                                                          
003300 FILE SECTION.                                                            
003400                                                                          
003500 FD  W22473-PARM                                                          
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900 01  IN-PARM           PIC X(80).                                         
004000                                                                          
004100 FD  W22473                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  W2247301 -COPY W2247301 -PRE  OUT-  -L.                              
004600                                                                          
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77  IDPGM                       PIC X(8)    VALUE 'W2247300'.            
005000 77  YES                         PIC X       VALUE 'Y'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005300 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005400     EJECT                                                                
005500 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005600 01  FILLER REDEFINES TODAYS-DATE.                                        
005700     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005800     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005900     03  TODAYS-DATE-DAY         PIC 9(2).                                
006000     EJECT                                                                
006100 01  GENERAL-SUBPROGRAMS.                                                 
006200*                                                                         
006300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006700     SKIP2                                                                
006800*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006900                                                                          
007000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007300     SKIP2                                                                
007400 01  ERROR-TEXT.                                                          
007500     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
007600     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL POSTSUM                                          
007900*                                                                         
008000*01  -COPY W0005   -PRE  POSTSUM-                                         
008100     EJECT                                                                
008200 01  IN-PARM-START               PIC X(24)   VALUE                        
008300                                 'IN-PARM-START  '.                       
008600 01  PARM-AREA.                                                           
008700     03  PARM-IDLEVNR            PIC X(5).                                
008800     03  FILLER                  PIC X(1).                                
008900     03  PARM-IDDC               PIC X(2).                                
009000     03  FILLER                  PIC X(1).                                
009010     03  PARM-KVVECKOR-AT        PIC X(2).                                
009020     03  FILLER                  PIC X(1).                                
009100     03  PARM-KVVECKOR-LT        PIC X(2).                                
009110     03  FILLER                  PIC X(1).                                
009120     03  PARM-KVDAGAR-TT         PIC X(2).                                
009200     03  FILLER                  PIC X(66)   VALUE SPACE.                 
009300     EJECT                                                                
009400 01  OUT-AREA-START              PIC X(24)   VALUE                        
009500                                 'OUT-AREA-START  '.                      
009800*01  AREA -COPY W2247301     -PRE OUT-                                    
009900     EJECT                                                                
010000                                                                          
010100*    --- AREAS FOR IMS-SECTIONS                                           
010200*                                                                         
010300     EJECT                                                                
010400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010500     SKIP3                                                                
010600 01  KEYS-FOR-DLI.                                                        
010610     03 WDK6G1KY-MIN-X.                                                   
010620        05 W-IDLEVNR-SHIP-MIN   PIC X(5)         VALUE SPACE.             
010630        05 FILLER               PIC X(5)         VALUE LOW-VALUE.         
010640                                                                          
010650     03 WDK6G1KY-MAX-X.                                                   
010660        05  W-IDLEVNR-SHIP-MAX  PIC X(5)         VALUE SPACE.             
010670        05 FILLER               PIC X(5)         VALUE HIGH-VALUE.        
010900                                                                          
010910     03 WDK6A1KY-MIN-X.                                                   
010920        05 W-IDLEVNR-K6-MIN     PIC X(5)         VALUE SPACE.             
010930        05 FILLER               PIC X(5)         VALUE LOW-VALUE.         
010940                                                                          
010950     03 WDK6A1KY-MAX-X.                                                   
010960        05  W-IDLEVNR-K6-MAX    PIC X(5)         VALUE SPACE.             
010970        05 FILLER               PIC X(5)         VALUE HIGH-VALUE.        
010980                                                                          
011000*    --- STATUS-KOD FRÅN IMS                                              
011100 01  STATUS-WS                   PIC XX.                                  
011200     88  SEGMENT-FOUND                       VALUE '  '.                  
011300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
011400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
011500     88  SEGMENT-END                         VALUE 'GB'.                  
011600     SKIP2                                                                
011700 01  GOOD-STATUSCODES.                                                    
011800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011900     SKIP3                                                                
012000 01  SSA1                        PIC X(64).                               
012100 01  SSA2                        PIC X(64).                               
012200     EJECT                                                                
012300*    --- IMS FUNCTION CODES                                               
012400*01  -COPY W0003                                                          
012500     EJECT                                                                
012600*    ---  DLI INPUT-OUTPUT AREA                                           
012700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK6G1'.                      
012800 01  DLI-IO-WDK6G1.                                                       
012900*    03  -COPY WDK6G1                                                     
013000     EJECT                                                                
013010 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK6A1'.                      
013020 01  DLI-IO-WDK6A1.                                                       
013030*    03  -COPY WDK6A1                                                     
013040     EJECT                                                                
013100 LINKAGE SECTION.                                                         
013200                                                                          
013300*01  -COPY W0008  -PRE WDK6G-                                             
013400     05  FILLER                  PIC X.                                   
013500     EJECT                                                                
013510*01  -COPY W0008  -PRE WDK6A-                                             
013520     05  FILLER                  PIC X.                                   
013530     EJECT                                                                
013600 PROCEDURE DIVISION  USING WDK6G-PCB WDK6A-PCB.                           
013700 MAIN SECTION.                                                            
013800     ENTRY 'DLITCBL' USING WDK6G-PCB WDK6A-PCB.                           
013900                                                                          
014000     PERFORM A-INIT                                                       
014100                                                                          
014200     PERFORM S01-READ-PARM-FROM-SCREEN-2115                               
014360                                                                          
014370     IF PARM-KVDAGAR-TT = ALL '+'                                         
014371        MOVE PARM-IDLEVNR         TO W-IDLEVNR-K6-MIN                     
014372                                     W-IDLEVNR-K6-MAX                     
014373        PERFORM IMS-GN-WDK6A1                                             
014374        PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                      
014375           MOVE SEQA-IDLEVNR      TO OUT-IDLEVNR                          
014376           MOVE SEQA-IDARTNR      TO OUT-IDARTNR                          
014377           MOVE PARM-KVVECKOR-AT  TO OUT-KVVECKOR-AT                      
014378           MOVE PARM-KVVECKOR-LT  TO OUT-KVVECKOR-LT                      
014379           MOVE PARM-KVDAGAR-TT   TO OUT-KVDAGAR-TT                       
014380                                                                          
014381           PERFORM S11-WRITE-W22473                                       
014382                                                                          
014383           PERFORM IMS-GN-WDK6A1                                          
014384        END-PERFORM                                                       
014390     ELSE                                                                 
014400        MOVE PARM-IDLEVNR         TO W-IDLEVNR-SHIP-MIN                   
014410                                     W-IDLEVNR-SHIP-MAX                   
014500        PERFORM IMS-GN-WDK6G1                                             
014600        PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                      
014800           MOVE SEQG-IDLEVNR-SHIP TO OUT-IDLEVNR                          
014900           MOVE SEQG-IDARTNR      TO OUT-IDARTNR                          
015000           MOVE PARM-KVVECKOR-AT  TO OUT-KVVECKOR-AT                      
015100           MOVE PARM-KVVECKOR-LT  TO OUT-KVVECKOR-LT                      
015110           MOVE PARM-KVDAGAR-TT   TO OUT-KVDAGAR-TT                       
015200                                                                          
015300           PERFORM S11-WRITE-W22473                                       
015400                                                                          
015500           PERFORM IMS-GN-WDK6G1                                          
015600        END-PERFORM                                                       
015610     END-IF                                                               
015700                                                                          
015800     PERFORM Z-FINIT                                                      
015900                                                                          
016000     MOVE ZERO TO RETURN-CODE                                             
016100     GOBACK                                                               
016200     .                                                                    
016300     EJECT                                                                
016400 A-INIT SECTION.                                                          
016500                                                                          
016600     OPEN INPUT  W22473-PARM                                              
016700          OUTPUT W22473                                                   
016800                                                                          
016900     ACCEPT TODAYS-DATE  FROM DATE                                        
017000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017100     .                                                                    
017200     EJECT                                                                
017300 Z-FINIT SECTION.                                                         
017400     CLOSE W22473-PARM                                                    
017500           W22473                                                         
017600     SKIP2                                                                
017700     MOVE 'S' TO POSTSUM-OPKOD                                            
017800     CALL POSTSUM USING POSTSUM-PARM                                      
017900     .                                                                    
018000     EJECT                                                                
018100 S01-READ-PARM-FROM-SCREEN-2115 SECTION.                                  
018200     MOVE 'S01-READ-PARM-FROM-SCREEN-2115' TO CURRENT-SECTION             
018300                                                                          
018400     READ W22473-PARM INTO PARM-AREA                                      
018500                                                                          
018610     MOVE 'W224PP'   TO POSTSUM-FDNAMN                                    
018700     MOVE 'W22473D1' TO POSTSUM-DDNAMN2                                   
018800     MOVE 'PARM'     TO POSTSUM-TRANSTYP                                  
018900     CALL POSTSUM USING POSTSUM-PARM                                      
019000     .                                                                    
019100     EJECT                                                                
019200 S11-WRITE-W22473 SECTION.                                                
019300                                                                          
019400     WRITE OUT-W2247301  FROM OUT-AREA                                    
019500                                                                          
019700     MOVE 'W22473'   TO POSTSUM-FDNAMN                                    
019800     MOVE 'W22473D2' TO POSTSUM-DDNAMN2                                   
019810     MOVE 'OUT '     TO POSTSUM-TRANSTYP                                  
019900     CALL POSTSUM USING POSTSUM-PARM                                      
020000     .                                                                    
020100     EJECT                                                                
020200 S99-ABEND SECTION.                                                       
020300                                                                          
020400     SKIP2                                                                
020500     MOVE 'S' TO POSTSUM-OPKOD                                            
020600     CALL POSTSUM USING POSTSUM-PARM                                      
020700     CALL ABEND USING RKOD-ABEND                                          
020800     .                                                                    
020900     EJECT                                                                
021000* --- IMS SECTIONS  ---                                                   
021100                                                                          
021200     EJECT                                                                
022600 IMS-GN-WDK6G1        SECTION.                                            
022610     MOVE 'IMS-GN-WDK6G1'       TO CURRENT-IMS-SECTION                    
022620                                                                          
022630     STRING 'WDK6G1  (WDK6G1KY=>' WDK6G1KY-MIN-X                          
022640                    '&WDK6G1KY<=' WDK6G1KY-MAX-X ')'                      
022900              DELIMITED BY SIZE INTO SSA1                                 
023000     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
023100     CALL CBLTDLI USING GN  WDK6G-PCB DLI-IO-WDK6G1 SSA1                  
023300     MOVE WDK6G-STATUS-CODE TO STATUS-WS                                  
023400     PERFORM IMS-STATUSCHECK                                              
023500     .                                                                    
023600                                                                          
023610 IMS-GN-WDK6A1 SECTION.                                                   
023611     MOVE 'IMS-GN-WDK611'       TO CURRENT-IMS-SECTION                    
023612                                                                          
023620     STRING 'WDK6A1  (WDK6A1KY=>' WDK6A1KY-MIN-X                          
023630                    '&WDK6A1KY<=' WDK6A1KY-MAX-X ')'                      
023640          DELIMITED BY SIZE INTO SSA1                                     
023650     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
023660     CALL CBLTDLI USING GN WDK6A-PCB DLI-IO-WDK6A1 SSA1                   
023670     MOVE WDK6A-STATUS-CODE TO STATUS-WS                                  
023671     PERFORM IMS-STATUSCHECK                                              
023690     .                                                                    
023700     EJECT                                                                
023800 IMS-STATUSCHECK SECTION.                                                 
023900                                                                          
024000     SET STATUS-IX TO 1                                                   
024100     SEARCH GOOD-STATUS                                                   
024200       AT END                                                             
024300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
024400           DELIMITED BY SIZE INTO ERROR-TEXT                              
024500         DISPLAY ERROR-TEXT                                               
024600         CALL FELLOG                                                      
024700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
024800         CONTINUE                                                         
024900     END-SEARCH                                                           
025000     .                                                                    
