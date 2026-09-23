000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4184800.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   21/04/22.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        DISCREPANCY INFO TO AZURE DATALAKE                               
001000*                                                                         
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
002400*          --- W41842 INFO                                                
002500     SELECT W41842                     ASSIGN TO W41848D1.                
002600     SKIP2                                                                
002700*          --- AZURE DATALAKE                                             
002800     SELECT W41842X1                   ASSIGN TO W41848D2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W41842                                                               
003500     RECORDING       V                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800 01  WDA201-AREA.                                                         
003900   03  FILLER                    PIC X(3).                                
004000*03  A01-AREA  -COPY WDA201  -L                                           
004100                                                                          
004200 01  WDA211-AREA.                                                         
004300   03  FILLER                    PIC X(3).                                
004400*03  A11-AREA  -COPY WDA211  -L                                           
004500                                                                          
004600 01  WDA221-AREA.                                                         
004700   03  FILLER                    PIC X(3).                                
004800*03  A21-AREA  -COPY WDA221  -L                                           
004900     EJECT                                                                
005000     SKIP3                                                                
005100 FD  W41842X1                                                             
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400                                                                          
005500*01  W41842X1 -COPY W41842X1 -PRE  OUT- -L.                               
005600     EJECT                                                                
005700 WORKING-STORAGE SECTION.                                                 
005800                                                                          
005900 77  IDPGM                       PIC X(8)    VALUE 'W4184800'.            
006000 77  YES                         PIC X       VALUE 'J'.                   
006100 77  NOO                         PIC X       VALUE 'N'.                   
006200                                                                          
006300 77  W41842-EOF-SW               PIC X       VALUE 'N'.                   
006400     88  END-OF-W41842                       VALUE 'J'.                   
006500     EJECT                                                                
006600 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006700 01  FILLER REDEFINES TODAYS-DATE.                                        
006800     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006900     03  TODAYS-DATE-MONTH       PIC 9(2).                                
007000     03  TODAYS-DATE-DAY         PIC 9(2).                                
007100     EJECT                                                                
007200 01  GENERAL-SUBPROGRAMS.                                                 
007300*                                                                         
007400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007600     SKIP2                                                                
007700*    --- PARAMETERS TO ABEND                                              
007800                                                                          
007900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008200     SKIP2                                                                
008300 01  ERROR-TEXT.                                                          
008400     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
008500     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
008600     EJECT                                                                
008700*    --- PARAMETRAR TILL POSTSUM                                          
008800*                                                                         
008900*01  -COPY W0005   -PRE  POSTSUM-                                         
009000     EJECT                                                                
009100 01  IN-AREA-WDA2               PIC X(24) VALUE 'IN-RDA2-AREA'.           
009200 01  IN-AREA.                                                             
009300     03 IN-IDPTYP                PIC X(3).                                
009400     03 IN-WDA2-AREA             PIC X(1500).                             
009500     03 ANM-AREA REDEFINES IN-WDA2-AREA.                                  
009600*      05 -COPY WDA201  -PRE IN-                                          
009700                                                                          
009800     03 LEV-AREA REDEFINES IN-WDA2-AREA.                                  
009900*      05 -COPY WDA211  -PRE IN-                                          
010000                                                                          
010100     03 TXT-AREA REDEFINES IN-WDA2-AREA.                                  
010200*      05 -COPY WDA221  -PRE IN-                                          
010300     EJECT                                                                
010400 01  OUT-AREA-START              PIC X(24)   VALUE                        
010500                                 'OUT-AREA-START  '.                      
010600     SKIP2                                                                
010700                                                                          
010800*01  AREA -COPY W41842X1     -PRE OUT-                                    
010900     EJECT                                                                
011000 PROCEDURE DIVISION.                                                      
011100 MAIN SECTION.                                                            
011200                                                                          
011300     PERFORM A-INIT                                                       
011400     PERFORM S01-READ-W41842                                              
011500     INITIALIZE OUT-AREA                                                  
011600     PERFORM UNTIL END-OF-W41842                                          
011700                                                                          
011800       IF IN-IDPTYP = '201'                                               
011900          IF OUT-IDARTNR  > +0 AND OUT-KDLEVANM NOT = ' '                 
012000             WRITE OUT-W41842X1 FROM OUT-AREA                             
012100          END-IF                                                          
012200                                                                          
012300          INITIALIZE OUT-AREA                                             
012400          PERFORM S12-MOVE-ANM-TO-W41842X                                 
012500       ELSE                                                               
012600          IF IN-IDPTYP = '211'                                            
012700            IF OUT-IDARTNR  > +0 AND OUT-KDLEVANM NOT = ' '               
012800               WRITE OUT-W41842X1 FROM OUT-AREA                           
012900            END-IF                                                        
013000                                                                          
013100            PERFORM S13-MOVE-LEV-TO-W41842X                               
013200          ELSE                                                            
013300            IF IN-IDPTYP = '221'                                          
013400               PERFORM S14-MOVE-TXT-TO-W41842X                            
013500            END-IF                                                        
013600          END-IF                                                          
013700       END-IF                                                             
013800       PERFORM S01-READ-W41842                                            
013900     END-PERFORM                                                          
014000                                                                          
014100     IF OUT-IDARTNR  > +0 AND OUT-KDLEVANM NOT = ' '                      
014200        WRITE OUT-W41842X1 FROM OUT-AREA                                  
014300     END-IF                                                               
014400                                                                          
014500     PERFORM Z-FINIT                                                      
014600                                                                          
014700     MOVE ZERO TO RETURN-CODE                                             
014800     GOBACK                                                               
014900     .                                                                    
015000     EJECT                                                                
015100 A-INIT SECTION.                                                          
015200                                                                          
015300     OPEN INPUT  W41842                                                   
015400                                                                          
015500     OPEN OUTPUT W41842X1                                                 
015600     SKIP2                                                                
015700     ACCEPT TODAYS-DATE  FROM DATE                                        
015800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015900     .                                                                    
016000     EJECT                                                                
016100 Z-FINIT SECTION.                                                         
016200     CLOSE W41842                                                         
016300           W41842X1                                                       
016400     SKIP2                                                                
016500     MOVE 'S' TO POSTSUM-OPKOD                                            
016600     CALL POSTSUM USING POSTSUM-PARM                                      
016700     .                                                                    
016800     EJECT                                                                
016900 S01-READ-W41842  SECTION.                                                
017000     READ W41842 INTO IN-AREA                                             
017100     AT END                                                               
017200        MOVE HIGH-VALUE TO IN-AREA                                        
017300        SET END-OF-W41842 TO TRUE                                         
017400                                                                          
017500     NOT AT END                                                           
017600        MOVE 'W41842' TO POSTSUM-FDNAMN                                   
017700        MOVE 'W41848D1' TO POSTSUM-DDNAMN2                                
017800*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
017900        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
018000        CALL POSTSUM USING POSTSUM-PARM                                   
018100     END-READ                                                             
018200     .                                                                    
018300     EJECT                                                                
018400 S11-WRITE-W41842 SECTION.                                                
018500                                                                          
018600     WRITE OUT-W41842X1 FROM OUT-AREA                                     
018700                                                                          
018800     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
018900     MOVE 'W41842X1' TO POSTSUM-FDNAMN                                    
019000     MOVE 'W41848D2' TO POSTSUM-DDNAMN2                                   
019100     CALL POSTSUM USING POSTSUM-PARM                                      
019200     .                                                                    
019300     EJECT                                                                
019400 S12-MOVE-ANM-TO-W41842X    SECTION.                                      
019500                                                                          
019600     MOVE IN-ANM-IDDISTR           TO OUT-IDDISTR                         
019700     MOVE IN-ANM-IDKUNDNR          TO OUT-IDKUNDNR                        
019800     MOVE IN-ANM-IDRAPPNR          TO OUT-IDRAPPNR                        
019900     MOVE IN-ANM-IDFTG             TO OUT-IDFTG-ANM                       
020000     MOVE IN-ANM-IDPERSON          TO OUT-IDPERSON-ANM                    
020100     MOVE IN-ANM-IDUSER            TO OUT-IDUSER                          
020200     MOVE IN-ANM-KDARBTYP          TO OUT-KDARBTYP-ANM                    
020300     MOVE IN-ANM-KDLEVANM          TO OUT-KDLEVANM                        
020400     MOVE IN-ANM-KVRADER-OBEH      TO OUT-KVRADER-OBEH                    
020500     MOVE IN-ANM-KVRADER-RT        TO OUT-KVRADER-RT                      
020600     MOVE IN-ANM-PRFOERS           TO OUT-PRFOERS                         
020700     MOVE IN-ANM-PRFRAKT           TO OUT-PRFRAKT-ANM                     
020800     MOVE IN-ANM-PRLEGKST          TO OUT-PRLEGKST                        
020900     MOVE IN-ANM-REEMBHNT          TO OUT-REEMBHNT                        
021000     MOVE IN-ANM-RELANDCO          TO OUT-RELANDCO                        
021100     MOVE IN-ANM-DALEVANM          TO OUT-DALEVANM-ANM                    
021200     MOVE IN-ANM-DARETANK          TO OUT-DARETANK                        
021300     MOVE IN-ANM-DARETILL          TO OUT-DARETILL                        
021400     MOVE IN-ANM-FLFARLIG          TO OUT-FLFARLIG                        
021500     MOVE IN-ANM-DARTPMN           TO OUT-DARTPMN                         
021600     MOVE IN-ANM-KDLEVANM-UPD      TO OUT-KDLEVANM-UPD                    
021700     MOVE IN-ANM-KDVALISO          TO OUT-KDVALISO                        
021800     MOVE IN-ANM-BEANST            TO OUT-BEANST                          
021900     MOVE IN-ANM-IDUSER-ADM        TO OUT-IDUSER-ADM                      
022000     MOVE IN-ANM-KDLEVATT          TO OUT-KDLEVATT                        
022100     MOVE IN-ANM-IDDC-RET          TO OUT-IDDC-RET-ANM                    
022200     MOVE IN-ANM-IXDCCLEAR         TO OUT-IXDCCLEAR                       
022300     MOVE IN-ANM-IDSYSTEM          TO OUT-IDSYSTEM                        
022400     .                                                                    
022500     EJECT                                                                
022600 S13-MOVE-LEV-TO-W41842X SECTION.                                         
022700                                                                          
022800     MOVE IN-LEV-IDARTNR           TO OUT-IDARTNR                         
022900     MOVE IN-LEV-IDRADNR           TO OUT-IDRADNR                         
023000     MOVE IN-LEV-ADGANG            TO OUT-ADGANG                          
023100     MOVE IN-LEV-ADLAGOMR          TO OUT-ADLAGOMR                        
023200     MOVE IN-LEV-ADPLATS           TO OUT-ADPLATS                         
023300     MOVE IN-LEV-FLANLYSF          TO OUT-FLANLYSF                        
023400     MOVE IN-LEV-FLANNULL          TO OUT-FLANNULL                        
023500     MOVE IN-LEV-FLAUTKRE          TO OUT-FLAUTKRE                        
023600     MOVE IN-LEV-FLDIRLEV          TO OUT-FLDIRLEV                        
023700     MOVE IN-LEV-FLSKROT           TO OUT-FLSKROT                         
023800     MOVE IN-LEV-FLSVAR            TO OUT-FLSVAR                          
023900     MOVE IN-LEV-FLTEXT            TO OUT-FLTEXT                          
024000     MOVE IN-LEV-IDANALYS          TO OUT-IDANALYS                        
024100     MOVE IN-LEV-IDANSTNR-RET      TO OUT-IDANSTNR-RET                    
024200     MOVE IN-LEV-IDDC              TO OUT-IDDC                            
024300     MOVE IN-LEV-IDDC-RET          TO OUT-IDDC-RET-LEV                    
024400     MOVE IN-LEV-IDFAKT            TO OUT-IDFAKT                          
024500     MOVE IN-LEV-IDFAKT-LOC        TO OUT-IDFAKT-LOC                      
024600     MOVE IN-LEV-IDFTG             TO OUT-IDFTG-LEV                       
024700     MOVE IN-LEV-IDILIST           TO OUT-IDILIST                         
024800     MOVE IN-LEV-IDKNOTNR          TO OUT-IDKNOTNR                        
024900     MOVE IN-LEV-IDKOLLI           TO OUT-IDKOLLI                         
025000     MOVE IN-LEV-IDKONTO           TO OUT-IDKONTO                         
025100     MOVE IN-LEV-IDKST             TO OUT-IDKST                           
025200     MOVE IN-LEV-IDKUNDRF          TO OUT-IDKUNDRF                        
025300     MOVE IN-LEV-IDLOPNRM          TO OUT-IDLOPNRM                        
025400     MOVE IN-LEV-IDPERSON          TO OUT-IDPERSON-LEV                    
025500     MOVE IN-LEV-IDPERSON-REM      TO OUT-IDPERSON-REM                    
025600     MOVE IN-LEV-IDUSER-PACK       TO OUT-IDUSER-PACK                     
025700     MOVE IN-LEV-KDANMORS          TO OUT-KDANMORS                        
025800     MOVE IN-LEV-KDARBTYP          TO OUT-KDARBTYP-LEV                    
025900     MOVE IN-LEV-KDARBTYP-REM      TO OUT-KDARBTYP-REM                    
026000     MOVE IN-LEV-KDEMBLEV          TO OUT-KDEMBLEV                        
026100     MOVE IN-LEV-KDFAKTYP          TO OUT-KDFAKTYP                        
026200     MOVE IN-LEV-KDFAKTYP-KNOT     TO OUT-KDFAKTYP-KNOT                   
026300     MOVE IN-LEV-KDFRAKT           TO OUT-KDFRAKT                         
026400     MOVE IN-LEV-KDKREBEH          TO OUT-KDKREBEH                        
026500     MOVE IN-LEV-KDORDKL           TO OUT-KDORDKL                         
026600     MOVE IN-LEV-KVANTAL-ILI       TO OUT-KVANTAL-ILI                     
026700     MOVE IN-LEV-KVAVV-KVAL        TO OUT-KVAVV-KVAL                      
026800     MOVE IN-LEV-KVAVV-KVANT       TO OUT-KVAVV-KVANT                     
026900     MOVE IN-LEV-KVLEVANM          TO OUT-KVLEVANM                        
027000     MOVE IN-LEV-KVLEVANM-BEKR     TO OUT-KVLEVANM-BEKR                   
027100     MOVE IN-LEV-KVRETINL          TO OUT-KVRETINL                        
027200     MOVE IN-LEV-KVRETINL-SKR      TO OUT-KVRETINL-SKR                    
027300     MOVE IN-LEV-PRARTBTO          TO OUT-PRARTBTO                        
027400     MOVE IN-LEV-PRARTBTO-LOC      TO OUT-PRARTBTO-LOC                    
027500     MOVE IN-LEV-PRFRAKT           TO OUT-PRFRAKT-LEV                     
027600     MOVE IN-LEV-TIFAKT            TO OUT-TIFAKT                          
027700     MOVE IN-LEV-TIFAKT-LOC        TO OUT-TIFAKT-LOC                      
027800     MOVE IN-LEV-TIINLINL          TO OUT-TIINLINL                        
027900     MOVE IN-LEV-TIKNOTA           TO OUT-TIKNOTA                         
028000     MOVE IN-LEV-DALEVANM          TO OUT-DALEVANM-LEV                    
028100     MOVE IN-LEV-TIREMISS-IN       TO OUT-TIREMISS-IN                     
028200     MOVE IN-LEV-TIREMISS-UT       TO OUT-TIREMISS-UT                     
028300     MOVE IN-LEV-TIUTSKR           TO OUT-TIUTSKR                         
028400     MOVE IN-LEV-IDARTNR-DEL       TO OUT-IDARTNR-DEL                     
028500     MOVE IN-LEV-TIUPPDAT-ILI      TO OUT-TIUPPDAT-ILI                    
028600     MOVE IN-LEV-FLLSBOK           TO OUT-FLLSBOK                         
028700     MOVE IN-LEV-KDAVVTYP          TO OUT-KDAVVTYP                        
028800     MOVE IN-LEV-FLINVUPD          TO OUT-FLINVUPD                        
028900     MOVE IN-LEV-FLPRQUES          TO OUT-FLPRQUES                        
029000     MOVE IN-LEV-IDPRQUES          TO OUT-IDPRQUES                        
029100     MOVE IN-LEV-KDVAT             TO OUT-KDVAT                           
029200     MOVE IN-LEV-BEART-VIPS        TO OUT-BEART-VIPS                      
029300     MOVE IN-LEV-PRARTSTD          TO OUT-PRARTSTD                        
029400     MOVE IN-LEV-PRARTSJK          TO OUT-PRARTSJK                        
029500     MOVE IN-LEV-PRARTBTO-LOCINV   TO OUT-PRARTBTO-LOCINV                 
029600     MOVE IN-LEV-FLRETUR           TO OUT-FLRETUR                         
029700     MOVE IN-LEV-IDANSTNR-ILIU     TO OUT-IDANSTNR-ILIU                   
029800                                                                          
029900     MOVE SPACE                    TO OUT-TEANMNOT-REG(01)                
030000                                      OUT-TEANMNOT-REG(02)                
030100                                      OUT-TEANMNOT-REG(03)                
030200                                                                          
030300                                      OUT-TEANMNOT-ADM(01)                
030400                                      OUT-TEANMNOT-ADM(02)                
030500                                      OUT-TEANMNOT-ADM(03)                
030600                                                                          
030700                                      OUT-TEANMNOT-REM(01)                
030800                                      OUT-TEANMNOT-REM(02)                
030900                                      OUT-TEANMNOT-REM(03)                
031000                                                                          
031100                                      OUT-TEANMNOT-RET(01)                
031200                                      OUT-TEANMNOT-RET(02)                
031300                                      OUT-TEANMNOT-RET(03)                
031400                                                                          
031500                                      OUT-TEANMNOT-DLR(01)                
031600                                      OUT-TEANMNOT-DLR(02)                
031700                                      OUT-TEANMNOT-DLR(03)                
031800     .                                                                    
031900     EJECT                                                                
032000 S14-MOVE-TXT-TO-W41842X SECTION.                                         
032100                                                                          
032200     MOVE IN-TXT-TEANMNOT-REG(01)  TO OUT-TEANMNOT-REG(01)                
032300     MOVE IN-TXT-TEANMNOT-REG(02)  TO OUT-TEANMNOT-REG(02)                
032400     MOVE IN-TXT-TEANMNOT-REG(03)  TO OUT-TEANMNOT-REG(03)                
032500                                                                          
032600     MOVE IN-TXT-TEANMNOT-ADM(01)  TO OUT-TEANMNOT-ADM(01)                
032700     MOVE IN-TXT-TEANMNOT-ADM(02)  TO OUT-TEANMNOT-ADM(02)                
032800     MOVE IN-TXT-TEANMNOT-ADM(03)  TO OUT-TEANMNOT-ADM(03)                
032900                                                                          
033000     MOVE IN-TXT-TEANMNOT-REM(01)  TO OUT-TEANMNOT-REM(01)                
033100     MOVE IN-TXT-TEANMNOT-REM(02)  TO OUT-TEANMNOT-REM(02)                
033200     MOVE IN-TXT-TEANMNOT-REM(03)  TO OUT-TEANMNOT-REM(03)                
033300                                                                          
033400     MOVE IN-TXT-TEANMNOT-RET(01)  TO OUT-TEANMNOT-RET(01)                
033500     MOVE IN-TXT-TEANMNOT-RET(02)  TO OUT-TEANMNOT-RET(02)                
033600     MOVE IN-TXT-TEANMNOT-RET(03)  TO OUT-TEANMNOT-RET(03)                
033700                                                                          
033800     MOVE IN-TXT-TEANMNOT-DLR(01)  TO OUT-TEANMNOT-DLR(01)                
033900     MOVE IN-TXT-TEANMNOT-DLR(02)  TO OUT-TEANMNOT-DLR(02)                
034000     MOVE IN-TXT-TEANMNOT-DLR(03)  TO OUT-TEANMNOT-DLR(03)                
034100     .                                                                    
034200     EJECT                                                                
034300 S99-ABEND SECTION.                                                       
034400                                                                          
034500     SKIP2                                                                
034600     MOVE 'S' TO POSTSUM-OPKOD                                            
034700     CALL POSTSUM USING POSTSUM-PARM                                      
034800     CALL ABEND USING RKOD-ABEND                                          
034900     .                                                                    
