000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2710600.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   20/09/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        CREATE OUTPUT REFERRAL BLOCK REPORT IN EXCEL - DIST.PRINT        
000900*                                                                         
001000*        THE PROGRAM READS     WDR5                                       
001100*                              WDD3                                       
001110*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700 ENVIRONMENT DIVISION.                                                    
001800                                                                          
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200                                                                          
002300*          --- REFERRAL BLOCK REPORT IN EXCEL                             
002400     SELECT W27106                     ASSIGN TO W27106D1.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700                                                                          
002800 FILE SECTION.                                                            
002900                                                                          
003000 FD  W27106                                                               
003100     RECORDING       V                                                    
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400 01  DAP-RECORD                  PIC X(999).                              
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800 77  IDPGM                       PIC X(8)    VALUE 'W2710600'.            
003900 77  JA                          PIC X       VALUE 'J'.                   
003910 77  YES                         PIC X       VALUE 'Y'.                   
004000 77  NOO                         PIC X       VALUE 'N'.                   
004010 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
004020 77  DBS-SECTION                 PIC X(80)   VALUE SPACE.                 
004100                                                                          
004200 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004300 01  FILLER REDEFINES TODAYS-DATE.                                        
004400     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004500     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004600     03  TODAYS-DATE-DAY         PIC 9(2).                                
004700     EJECT                                                                
004800 01  GENERAL-SUBPROGRAMS.                                                 
004900*                                                                         
005000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005400     SKIP2                                                                
005500*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
005600                                                                          
005700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006000     SKIP2                                                                
006100 01  ERROR-TEXT.                                                          
006200     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
006300     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006400     EJECT                                                                
006500*    --- PARAMETRAR TILL POSTSUM                                          
006600*                                                                         
006700*01  -COPY W0005   -PRE  POSTSUM-                                         
006800     EJECT                                                                
006900 01  WS-DAP-AREA.                                                         
007000    03  FILLER                  PIC X(165) VALUE SPACE.                   
007100                                                                          
007200 01 WS-DAP-LINE.                                                          
007300    03 WS-DAP-NAME              PIC X(04).                                
007400    03 WS-DAP-IDLANDX2          PIC X(02).                                
007500    03 WS-DAP-FILLER            PIC X(330)  VALUE SPACE.                  
007600                                                                          
007700 01 W-HEAD.                                                               
007800    03 W-HEAD-IDARTNR           PIC X(09) VALUE 'PART'.                   
007900    03 FILLER                   PIC X(01) VALUE ';'.                      
007910    03 W-HEAD-IDBEART           PIC X(25) VALUE 'DESCRIPTION'.            
007920    03 FILLER                   PIC X(01) VALUE ';'.                      
008000    03 W-HEAD-FLREFERAL         PIC X(09) VALUE 'REF.BLOCK'.              
008100    03 FILLER                   PIC X(01) VALUE ';'.                      
008200    03 W-HEAD-IDUSER            PIC X(08) VALUE 'USER'.                   
008300    03 FILLER                   PIC X(01) VALUE ';'.                      
008400    03 W-HEAD-TIREGDAT          PIC X(08) VALUE 'REG.DATE'.               
008500                                                                          
008600 01 W-LINE.                                                               
008700    03 W-LINE-IDARTNR           PIC 9(09).                                
008800    03 FILLER                   PIC X(01) VALUE ';'.                      
008810    03 W-LINE-BEART             PIC X(25).                                
008820    03 FILLER                   PIC X(01) VALUE ';'.                      
008900    03 W-LINE-FLREFERAL         PIC X(09).                                
009000    03 FILLER                   PIC X(01) VALUE ';'.                      
009100    03 W-LINE-IDUSER            PIC X(08).                                
009200    03 FILLER                   PIC X(01) VALUE ';'.                      
009300    03 W-LINE-TIREGDAT          PIC 9(08).                                
009400     EJECT                                                                
009500*    --- AREAS FOR IMS-SECTIONS                                           
009600*                                                                         
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009900     SKIP3                                                                
010000 01  KEYS-FOR-DLI.                                                        
010001     03  W-IDARTNR-X.                                                     
010002         05  W-IDARTNR           PIC S9(9)               COMP-3.          
010010     03  W-IDSKYLT-X.                                                     
010020         05  W-IDSKYLT           PIC  X(3)   VALUE 'GB '.                 
010100     03  W-WDGX2507-X.                                                    
010200         05  W-IDHTYP-2507       PIC X(4)    VALUE '2507'.                
010300         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
010310     03  W-IDLANDX2-X.                                                    
010320         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
010400                                                                          
010500*    --- STATUS-KOD FRÅN IMS                                              
010600 01  STATUS-WS                   PIC XX.                                  
010700     88  SEGMENT-FOUND                       VALUE '  '.                  
010800     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
010900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
011000     SKIP2                                                                
011100 01  GOOD-STATUSCODES.                                                    
011200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011300     SKIP3                                                                
011400 01  SSA1                        PIC X(64).                               
011500 01  SSA2                        PIC X(64).                               
011510 01  SSA3                        PIC X(64).                               
011600     EJECT                                                                
011700*    --- IMS FUNCTION CODES                                               
011800*01  -COPY W0003                                                          
011900     EJECT                                                                
012000*    ---  DLI INPUT-OUTPUT AREA                                           
012100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
012200 01  DLI-IO-WDGX01.                                                       
012300*    03  -COPY WDGX01                                                     
012400     EJECT                                                                
012420 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2508'.                    
012430 01  DLI-IO-WDGX2508.                                                     
012440*    03  -COPY WDGX2508                                                   
012450     EJECT                                                                
012500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2510'.                    
012600 01  DLI-IO-WDGX2510.                                                     
012700*    03  -COPY WDGX2510                                                   
012800     EJECT                                                                
012810 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
012820 01  DLI-IO-WDD311.                                                       
012830*    03  -COPY WDD311                                                     
012840     EJECT                                                                
012900 LINKAGE SECTION.                                                         
013000                                                                          
013100                                                                          
013200*01  -COPY W0008  -PRE WDR5-                                              
013300     05  FILLER                  PIC X.                                   
013400     EJECT                                                                
013410*01  -COPY W0008  -PRE WDD3-                                              
013420     05  FILLER                  PIC X.                                   
013430     EJECT                                                                
013500 PROCEDURE DIVISION  USING WDR5-PCB WDD3-PCB.                             
013600 MAIN SECTION.                                                            
013700     ENTRY 'DLITCBL' USING WDR5-PCB WDD3-PCB.                             
013800                                                                          
013900     PERFORM A-INIT                                                       
014000                                                                          
014100     PERFORM IMS-GU-WDGX2507                                              
014130                                                                          
014200     PERFORM IMS-GNP-WDGX2508                                             
014300     PERFORM UNTIL SEGMENT-MISSING                                        
014400       PERFORM B-CREATE-DAP                                               
014500       PERFORM IMS-GNP-WDGX2510                                           
014900       PERFORM UNTIL SEGMENT-MISSING                                      
015000         PERFORM C-CREATE-REFERAL-REPORT                                  
015100         PERFORM IMS-GNP-WDGX2510                                         
015200       END-PERFORM                                                        
015300                                                                          
015400       PERFORM IMS-GNP-WDGX2508                                           
015500     END-PERFORM                                                          
015600                                                                          
015700                                                                          
015800     PERFORM Z-FINIT                                                      
015900                                                                          
016000     MOVE ZERO TO RETURN-CODE                                             
016100     GOBACK                                                               
016200     .                                                                    
016300     EJECT                                                                
016400 A-INIT SECTION.                                                          
016500                                                                          
016600     OPEN OUTPUT W27106                                                   
016700                                                                          
016800     ACCEPT TODAYS-DATE  FROM DATE                                        
016900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017000     .                                                                    
017100     EJECT                                                                
017200 B-CREATE-DAP SECTION.                                                    
017300                                                                          
017400     MOVE SPACE                 TO WS-DAP-AREA                            
017500     MOVE '¤DAPW27106-001'      TO WS-DAP-LINE                            
017600     MOVE WS-DAP-LINE           TO WS-DAP-AREA                            
017700     PERFORM S11-WRITE-W27106                                             
017800                                                                          
017900     MOVE SPACE                 TO WS-DAP-LINE                            
018000     MOVE '¤DAP'                TO WS-DAP-NAME                            
018100     MOVE 2508-IDLANDX2         TO WS-DAP-IDLANDX2                        
018101                                   W-IDLANDX2                             
018200     MOVE WS-DAP-LINE           TO WS-DAP-AREA                            
018300     PERFORM S11-WRITE-W27106                                             
018400                                                                          
018500     MOVE W-HEAD                TO WS-DAP-AREA                            
018600     PERFORM S11-WRITE-W27106                                             
018700     .                                                                    
018800     EJECT                                                                
018900 C-CREATE-REFERAL-REPORT SECTION.                                         
019000                                                                          
019100     MOVE 2510-IDARTNR          TO W-LINE-IDARTNR                         
019101     MOVE 2510-IDARTNR          TO W-IDARTNR                              
019103     PERFORM IMS-GU-WDD311                                                
019104     IF SEGMENT-FOUND                                                     
019105        MOVE TEXT-BEART         TO W-LINE-BEART                           
019106     ELSE                                                                 
019107        MOVE SPACE              TO W-LINE-BEART                           
019108     END-IF                                                               
019110     IF 2510-FLREFERAL = JA OR YES                                        
019120        MOVE YES                TO W-LINE-FLREFERAL                       
019130     ELSE                                                                 
019200        MOVE 2510-FLREFERAL     TO W-LINE-FLREFERAL                       
019210     END-IF                                                               
019300     MOVE 2510-IDUSER           TO W-LINE-IDUSER                          
019400     MOVE 2510-TIREGDAT         TO W-LINE-TIREGDAT                        
019500     MOVE W-LINE                TO WS-DAP-AREA                            
019600                                                                          
019700     PERFORM S11-WRITE-W27106                                             
019800     .                                                                    
019900     EJECT                                                                
020000 Z-FINIT SECTION.                                                         
020100     CLOSE W27106                                                         
020200     SKIP2                                                                
020300     MOVE 'S' TO POSTSUM-OPKOD                                            
020400     CALL POSTSUM USING POSTSUM-PARM                                      
020500     .                                                                    
020600     EJECT                                                                
020700 S11-WRITE-W27106 SECTION.                                                
020800                                                                          
020900     WRITE DAP-RECORD FROM WS-DAP-AREA                                    
021000                                                                          
021100     MOVE 'OUT'      TO POSTSUM-TRANSTYP                                  
021200     MOVE 'W27106'   TO POSTSUM-FDNAMN                                    
021300     MOVE 'W27106D1' TO POSTSUM-DDNAMN2                                   
021400     CALL POSTSUM USING POSTSUM-PARM                                      
021500     .                                                                    
021600     EJECT                                                                
021700 S99-ABEND SECTION.                                                       
021800                                                                          
021900     MOVE 'S' TO POSTSUM-OPKOD                                            
022000     CALL POSTSUM USING POSTSUM-PARM                                      
022100     CALL ABEND USING RKOD-ABEND                                          
022200     .                                                                    
022300     EJECT                                                                
022400* --- IMS SECTIONS  ---                                                   
022500                                                                          
022600     EJECT                                                                
022700 IMS-GU-WDGX2507 SECTION.                                                 
022800     MOVE 'IMS-GU-WDGX2507'   TO DBS-SECTION                              
022900                                                                          
023000     STRING 'WDR501  (WDGXKEY  =' W-WDGX2507-X ')'                        
023100          DELIMITED BY SIZE INTO SSA1                                     
023200     MOVE '  '                TO GOOD-STATUSCODES                         
023300     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDGX01 SSA1                    
023400     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
023500     PERFORM IMS-STATUSCHECK                                              
023600     .                                                                    
023800 IMS-GNP-WDGX2508 SECTION.                                                
023900     MOVE 'IMS-GNP-WDGX2508'   TO DBS-SECTION                             
024000                                                                          
024100     MOVE 'WDGX2508'          TO SSA1                                     
024200     MOVE '  GE'              TO GOOD-STATUSCODES                         
024300     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDGX2508 SSA1                 
024400     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
024500     PERFORM IMS-STATUSCHECK                                              
024700     .                                                                    
024800                                                                          
024900 IMS-GNP-WDGX2510 SECTION.                                                
025000     MOVE 'IMS-GNP-WDGX2510'   TO DBS-SECTION                             
025100                                                                          
025140     STRING 'WDGX2508(IDLANDX2 =' W-IDLANDX2-X ')'                        
025150          DELIMITED BY SIZE INTO SSA1                                     
025200     MOVE 'WDGX2510'          TO SSA2                                     
025300     MOVE '  GE'              TO GOOD-STATUSCODES                         
025400     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDGX2510 SSA1 SSA2            
025500     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
025510     PERFORM IMS-STATUSCHECK                                              
025800     .                                                                    
025900     EJECT                                                                
025910 IMS-GU-WDD311 SECTION.                                                   
025920     MOVE 'IMS-GU-WDD311 '  TO DBS-SECTION                                
025930                                                                          
025940     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
025950            DELIMITED BY SIZE INTO SSA1                                   
025960     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
025970            DELIMITED BY SIZE INTO SSA2                                   
025980     MOVE '  GE'                TO GOOD-STATUSCODES                       
025990     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
025991     MOVE WDD3-STATUS-CODE      TO STATUS-WS                              
025992     PERFORM IMS-STATUSCHECK                                              
025993     CONTINUE.                                                            
025994     EJECT                                                                
026000 IMS-STATUSCHECK SECTION.                                                 
026100                                                                          
026200     SET STATUS-IX TO 1                                                   
026300     SEARCH GOOD-STATUS                                                   
026400       AT END                                                             
026500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
026600           DELIMITED BY SIZE INTO ERROR-TEXT                              
026700         DISPLAY ERROR-TEXT                                               
026800         CALL FELLOG                                                      
026900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
027000         CONTINUE                                                         
027100     END-SEARCH                                                           
027200     .                                                                    
