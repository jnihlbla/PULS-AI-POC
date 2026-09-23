000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2241000.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   13/02/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        Matrialförsörjning Lokal Anskaffning (Kina och USA)              
000900*                                                                         
001000*        THE PROGRAM READS     WDG3 (2203/2204)                           
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- 2204-RECORDS                                               
002500     SELECT W22410                     ASSIGN TO W22410D1.                
002600     SKIP2                                                                
002700*          --- 2203-RECORDS                                               
002800     SELECT W22411                     ASSIGN TO W22410D2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP2                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W22410                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  RECORD -COPY W2242204 -PRE  OUT-2204- -L.                            
003900     SKIP3                                                                
004000 FD  W22411                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  RECORD -COPY W2242203 -PRE  OUT-2203- -L.                            
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800 77  IDPGM                       PIC X(8)    VALUE 'W2241000'.            
004900 77  CURRENT-SECTION             PIC X(30)  VALUE SPACE.                  
005000 77  DBS-SECTION                 PIC X(30)  VALUE SPACE.                  
005100 77  YES                         PIC X       VALUE 'J'.                   
005200 77  NOO                         PIC X       VALUE 'N'.                   
005300     EJECT                                                                
005400*01  -COPY WWDCKONS                                                       
005500     EJECT                                                                
005600 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005700 01  FILLER REDEFINES TODAYS-DATE.                                        
005800     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005900     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006000     03  TODAYS-DATE-DAY         PIC 9(2).                                
006100     EJECT                                                                
006200 01  GENERAL-SUBPROGRAMS.                                                 
006300*                                                                         
006400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006800     SKIP2                                                                
006900*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
007000                                                                          
007100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007400     SKIP2                                                                
007500 01  ERROR-TEXT.                                                          
007600     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
007700     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007800     EJECT                                                                
007900*    --- PARAMETRAR TILL POSTSUM                                          
008000*                                                                         
008100*01  -COPY W0005   -PRE  POSTSUM-                                         
008200     EJECT                                                                
008300 01  OUT-2203-AREA-START         PIC X(24)   VALUE                        
008400                                 'OUT-2203-AREA-START  '.                 
008500     SKIP2                                                                
008600                                                                          
008700*01  AREA -COPY W2242203     -PRE OUT-2203-                               
008800     EJECT                                                                
008900 01  OUT-2204-AREA-START         PIC X(24)   VALUE                        
009000                                 'OUT-2204-AREA-START  '.                 
009100     SKIP2                                                                
009200                                                                          
009300*01  AREA -COPY W2242204     -PRE OUT-2204-                               
009400     EJECT                                                                
009500*    --- AREAS FOR IMS-SECTIONS                                           
009600*                                                                         
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009900     SKIP3                                                                
010000 01  KEYS-FOR-DLI.                                                        
010100     03  W-WDGXKEY-2203-X.                                                
010200         05  W-IDHTYP            PIC X(04)   VALUE '2203'.                
010300         05  W-IDDC              PIC X(02)   VALUE SPACE.                 
010400         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
010500                                                                          
010600 01  W-IDHTYP-2204               PIC X(04)   VALUE '2204'.                
010700     SKIP2                                                                
010800*    --- STATUS-KOD FRÅN IMS                                              
010900 01  STATUS-WS                   PIC XX.                                  
011000     88  SEGMENT-FOUND                       VALUE '  '.                  
011100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
011200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
011300     SKIP2                                                                
011400 01  GOOD-STATUSCODES.                                                    
011500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011600     SKIP3                                                                
011700 01  SSA1                        PIC X(64).                               
011800 01  SSA2                        PIC X(64).                               
011900     EJECT                                                                
012000*    --- IMS FUNCTION CODES                                               
012100*01  -COPY W0003                                                          
012200     EJECT                                                                
012300*    ---  DLI INPUT-OUTPUT AREA                                           
012400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2203'.                    
012500 01  DLI-IO-WDGX2203.                                                     
012600*    03  -COPY WDG301   -PRE 2203-.                                       
012700     EJECT                                                                
012800                                                                          
012900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2204'.                    
013000 01  DLI-IO-WDGX2204.                                                     
013100*    03  -COPY WDGX2204.                                                  
013200         05  FILLER PIC X(3).                                             
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500                                                                          
013600                                                                          
013700*01  -COPY W0008  -PRE WDG3-                                              
013800     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014000 PROCEDURE DIVISION  USING WDG3-PCB.                                      
014100 MAIN SECTION.                                                            
014200     ENTRY 'DLITCBL' USING WDG3-PCB.                                      
014300                                                                          
014400     PERFORM A-INIT                                                       
014500                                                                          
014600     MOVE WC-NDC-CN-71 TO W-IDDC                                          
014700     PERFORM B-READ-IDHTYP-2203-2204                                      
014800                                                                          
014900     MOVE WC-NDC-CN-72 TO W-IDDC                                          
015000     PERFORM B-READ-IDHTYP-2203-2204                                      
015100                                                                          
015200     MOVE WC-NDC-CN-73 TO W-IDDC                                          
015300     PERFORM B-READ-IDHTYP-2203-2204                                      
015400                                                                          
015500     MOVE WC-NDC-CN-74 TO W-IDDC                                          
015600     PERFORM B-READ-IDHTYP-2203-2204                                      
015700                                                                          
015800*-- DC41                                                                  
015900     MOVE WC-NDC-US-RU TO W-IDDC                                          
016000     PERFORM B-READ-IDHTYP-2203-2204                                      
016100                                                                          
016200*-- DC43                                                                  
016300     MOVE WC-NDC-US-LA TO W-IDDC                                          
016400     PERFORM B-READ-IDHTYP-2203-2204                                      
016500                                                                          
016600*-- DC44                                                                  
016700     MOVE WC-NDC-US-SE TO W-IDDC                                          
016800     PERFORM B-READ-IDHTYP-2203-2204                                      
016900                                                                          
017000*-- DC45                                                                  
017100     MOVE WC-NDC-US-CH TO W-IDDC                                          
017200     PERFORM B-READ-IDHTYP-2203-2204                                      
017300                                                                          
017400*-- DC46                                                                  
017500     MOVE WC-NDC-US-JA TO W-IDDC                                          
017600     PERFORM B-READ-IDHTYP-2203-2204                                      
017700                                                                          
017800*-- DC47                                                                  
017900     MOVE WC-NDC-US-DA TO W-IDDC                                          
018000     PERFORM B-READ-IDHTYP-2203-2204                                      
018100                                                                          
018200     PERFORM Z-FINIT                                                      
018300                                                                          
018400     MOVE ZERO TO RETURN-CODE                                             
018500     GOBACK                                                               
018600     .                                                                    
018700     EJECT                                                                
018800 A-INIT SECTION.                                                          
018900     MOVE 'A-INIT'             TO CURRENT-SECTION                         
019000                                                                          
019100     OPEN OUTPUT W22410                                                   
019200                 W22411                                                   
019300                                                                          
019400     ACCEPT TODAYS-DATE  FROM DATE                                        
019500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019600     .                                                                    
019700     EJECT                                                                
019800 B-READ-IDHTYP-2203-2204 SECTION.                                         
019900     MOVE 'B-READ-IDHTYP-2203-2204' TO CURRENT-SECTION                    
020000                                                                          
020200     PERFORM IMS-GU-WDGX2203                                              
020400     IF SEGMENT-FOUND                                                     
020500        MOVE 2203-IDHTYP      TO OUT-2203-IDHTYP                          
020600        MOVE W-IDDC           TO OUT-2203-IDDC                            
020700        PERFORM S12-WRITE-W22411                                          
020800                                                                          
020900        PERFORM IMS-GNP-WDGX2204                                          
021000        PERFORM UNTIL SEGMENT-MISSING                                     
021100          MOVE W-IDHTYP-2204  TO OUT-2204-IDHTYP                          
021200          MOVE W-IDDC         TO OUT-2204-IDDC                            
021300          MOVE 2204-IDARTNR   TO OUT-2204-IDARTNR                         
021400          MOVE 2204-KDLPORS   TO OUT-2204-KDLPORS                         
021500          PERFORM S11-WRITE-W22410                                        
021600                                                                          
021700          PERFORM IMS-GNP-WDGX2204                                        
021800        END-PERFORM                                                       
021900     END-IF                                                               
022000     .                                                                    
022100     EJECT                                                                
022200 Z-FINIT SECTION.                                                         
022300     MOVE 'Z-FINIT                ' TO CURRENT-SECTION                    
022400     CLOSE W22410                                                         
022500           W22411                                                         
022600     SKIP2                                                                
022700     MOVE 'S' TO POSTSUM-OPKOD                                            
022800     CALL POSTSUM USING POSTSUM-PARM                                      
022900     .                                                                    
023000     EJECT                                                                
023100 S11-WRITE-W22410 SECTION.                                                
023200     MOVE 'S11-WRITE-W22410       ' TO CURRENT-SECTION                    
023300                                                                          
023400     WRITE OUT-2204-RECORD FROM OUT-2204-AREA                             
023500                                                                          
023600     MOVE '2204'     TO POSTSUM-TRANSTYP                                  
023700     MOVE 'W22410'   TO POSTSUM-FDNAMN                                    
023800     MOVE 'W22410D1' TO POSTSUM-DDNAMN2                                   
023900     CALL POSTSUM USING POSTSUM-PARM                                      
024000     .                                                                    
024100     EJECT                                                                
024200 S12-WRITE-W22411 SECTION.                                                
024300     MOVE 'S12-WRITE-W22411       ' TO CURRENT-SECTION                    
024400                                                                          
024500     WRITE OUT-2203-RECORD FROM DLI-IO-WDGX2203                           
024600                                                                          
024700     MOVE '2203'     TO POSTSUM-TRANSTYP                                  
024800     MOVE 'W22411'   TO POSTSUM-FDNAMN                                    
024900     MOVE 'W22410D2' TO POSTSUM-DDNAMN2                                   
025000     CALL POSTSUM USING POSTSUM-PARM                                      
025100     .                                                                    
025200     EJECT                                                                
025300 S99-ABEND SECTION.                                                       
025400     MOVE 'S99-ABEND              ' TO CURRENT-SECTION                    
025500                                                                          
025600     SKIP2                                                                
025700     MOVE 'S' TO POSTSUM-OPKOD                                            
025800     CALL POSTSUM USING POSTSUM-PARM                                      
025900     CALL ABEND USING RKOD-ABEND                                          
026000     .                                                                    
026100     EJECT                                                                
026200* --- IMS SECTIONS  ---                                                   
026300                                                                          
026400     EJECT                                                                
026500 IMS-GU-WDGX2203 SECTION.                                                 
026600     MOVE 'IMS-GU-WDGX2203      ' To DBS-SECTION                          
026700                                                                          
026800     STRING 'WLXXBJ01(WDG3KEY  =' W-WDGXKEY-2203-X ')'                    
026900            DELIMITED BY SIZE INTO SSA1                                   
027000     MOVE '  GE'           TO GOOD-STATUSCODES                            
027100     CALL CBLTDLI USING GU WDG3-PCB DLI-IO-WDGX2203 SSA1                  
027200     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
027300     PERFORM IMS-STATUSCHECK                                              
027400     .                                                                    
027500                                                                          
027600 IMS-GNP-WDGX2204 SECTION.                                                
027700     MOVE 'IMS-GNP-WDGX2204     ' To DBS-SECTION                          
027800                                                                          
027900     MOVE 'WLXXBJ11'       TO SSA1                                        
028000     MOVE '  GE'           TO GOOD-STATUSCODES                            
028100     CALL CBLTDLI USING GNP WDG3-PCB DLI-IO-WDGX2204 SSA1                 
028200     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
028300     PERFORM IMS-STATUSCHECK                                              
028400     .                                                                    
028500                                                                          
028600 IMS-STATUSCHECK SECTION.                                                 
028700                                                                          
028800     SET STATUS-IX TO 1                                                   
028900     SEARCH GOOD-STATUS                                                   
029000       AT END                                                             
029100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
029200           DELIMITED BY SIZE INTO ERROR-TEXT                              
029300         DISPLAY ERROR-TEXT                                               
029400         CALL FELLOG                                                      
029500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
029600         CONTINUE                                                         
029700     END-SEARCH                                                           
029800     .                                                                    
