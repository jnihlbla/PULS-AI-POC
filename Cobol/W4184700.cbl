000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4184700.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   21/04/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION: CREATE FILE TO AZURE DATALAKE IN DISPLAY FORMAT            
000900*                                                                         
001000*    ABENDCODES:                                                          
001100*                                                                         
001200                                                                          
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*          --- INPUT FILE FROM W41846                                     
002100     SELECT W4184C                     ASSIGN TO W41847D1.                
002200*          --- OUTPUT FILE TO AZURE DATALAKE                              
002300     SELECT W4184CX                    ASSIGN TO W41847D2.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W4184C                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  -COPY W41842X1        -PRE  IN-   -L.                                
003400     SKIP3                                                                
003500 FD  W4184CX                                                              
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  RECORD -COPY W41842X  -PRE  OUTX- -L.                                
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W4184700'.            
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600 77  IX1                         PIC S9(9)   VALUE +0 COMP SYNC.          
004700                                                                          
004800 77  W4184C-EOF-SW               PIC X       VALUE 'N'.                   
004900     88  END-OF-W4184C                       VALUE 'J'.                   
005000     EJECT                                                                
005100 01  GENERAL-SUBPROGRAMS.                                                 
005200*                                                                         
005300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
005400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005600     SKIP2                                                                
005700*    --- PARAMETERS TO ABEND                                              
005800                                                                          
005900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006200     SKIP2                                                                
006300 01  ERROR-TEXT.                                                          
006400     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
006500     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL POSTSUM                                          
006800*                                                                         
006900*01  -COPY W0005   -PRE  POSTSUM-                                         
007000     EJECT                                                                
007100 01  IN-AREA-START               PIC X(24)   VALUE                        
007200                                 'IN-AREA-START  '.                       
007300                                                                          
007400*01  AREA -COPY W41842X1    -PRE IN-                                      
007500     EJECT                                                                
007600 01  OUT1-AREA-START             PIC X(24)   VALUE                        
007700                                 'OUT1-AREA-START  '.                     
007800                                                                          
007900*01  AREA -COPY W41842X     -PRE OUTX-                                    
008000     EJECT                                                                
008100 PROCEDURE DIVISION.                                                      
008200 MAIN SECTION.                                                            
008300     SKIP2                                                                
008400                                                                          
008500     PERFORM A-INIT                                                       
008600     PERFORM S01-READ-W4184C                                              
008700     PERFORM UNTIL END-OF-W4184C                                          
008800       PERFORM S11-WRITE-W4184CX                                          
008900                                                                          
009000       PERFORM S01-READ-W4184C                                            
009100     END-PERFORM                                                          
009200                                                                          
009300     PERFORM Z-FINIT                                                      
009400                                                                          
009500     MOVE ZERO TO RETURN-CODE                                             
009600     GOBACK                                                               
009700     .                                                                    
009800     EJECT                                                                
009900 A-INIT SECTION.                                                          
010000                                                                          
010100     OPEN INPUT  W4184C                                                   
010200                                                                          
010300          OUTPUT W4184CX                                                  
010400                                                                          
010500     .                                                                    
010600     EJECT                                                                
010700 Z-FINIT SECTION.                                                         
010800     CLOSE W4184C                                                         
010900           W4184CX                                                        
011000     SKIP2                                                                
011100     MOVE 'S' TO POSTSUM-OPKOD                                            
011200     CALL POSTSUM USING POSTSUM-PARM                                      
011300     .                                                                    
011400     EJECT                                                                
011500 S01-READ-W4184C  SECTION.                                                
011600                                                                          
011700     READ W4184C INTO IN-AREA                                             
011800     AT END                                                               
011900        MOVE HIGH-VALUE TO IN-AREA                                        
012000        SET END-OF-W4184C TO TRUE                                         
012100                                                                          
012200     NOT AT END                                                           
012300        MOVE 'W4184C'   TO POSTSUM-FDNAMN                                 
012400        MOVE 'W41847D1' TO POSTSUM-DDNAMN2                                
012500        MOVE SPACE      TO POSTSUM-TRANSTYP                               
012600        CALL POSTSUM USING POSTSUM-PARM                                   
012700     END-READ                                                             
012800     .                                                                    
012900     EJECT                                                                
013000 S11-WRITE-W4184CX SECTION.                                               
013100                                                                          
013200*WDA201                                                                   
013600     MOVE IN-IDDISTR             TO OUTX-IDDISTR                          
013900     MOVE IN-IDKUNDNR            TO OUTX-IDKUNDNR                         
014200     MOVE IN-IDRAPPNR            TO OUTX-IDRAPPNR                         
014500     MOVE IN-IDFTG-ANM           TO OUTX-IDFTG-ANM                        
014800     MOVE IN-IDPERSON-ANM        TO OUTX-IDPERSON-ANM                     
015100     MOVE IN-IDUSER              TO OUTX-IDUSER                           
015400     MOVE IN-KDARBTYP-ANM        TO OUTX-KDARBTYP-ANM                     
015700     MOVE IN-KDLEVANM            TO OUTX-KDLEVANM                         
016000     MOVE IN-KVRADER-OBEH        TO OUTX-KVRADER-OBEH                     
016300     MOVE IN-KVRADER-RT          TO OUTX-KVRADER-RT                       
016600     MOVE IN-PRFOERS             TO OUTX-PRFOERS                          
016900     MOVE IN-PRFRAKT-ANM         TO OUTX-PRFRAKT-ANM                      
017200     MOVE IN-PRLEGKST            TO OUTX-PRLEGKST                         
017500     MOVE IN-REEMBHNT            TO OUTX-REEMBHNT                         
017800     MOVE IN-RELANDCO            TO OUTX-RELANDCO                         
018100     MOVE IN-DALEVANM-ANM        TO OUTX-DALEVANM-ANM                     
018400     MOVE IN-DARETANK            TO OUTX-DARETANK                         
018700     MOVE IN-DARETILL            TO OUTX-DARETILL                         
019000     MOVE IN-FLFARLIG            TO OUTX-FLFARLIG                         
019300     MOVE IN-DARTPMN             TO OUTX-DARTPMN                          
019700     MOVE IN-KDLEVANM-UPD        TO OUTX-KDLEVANM-UPD                     
020000     MOVE IN-KDVALISO            TO OUTX-KDVALISO                         
020300     MOVE IN-BEANST              TO OUTX-BEANST                           
020600     MOVE IN-IDUSER-ADM          TO OUTX-IDUSER-ADM                       
020900     MOVE IN-KDLEVATT            TO OUTX-KDLEVATT                         
021200     MOVE IN-IDDC-RET-ANM        TO OUTX-IDDC-RET-ANM                     
021500     MOVE IN-IXDCCLEAR           TO OUTX-IXDCCLEAR                        
021800     MOVE IN-IDSYSTEM            TO OUTX-IDSYSTEM                         
022100*WDA211                                                                   
022400     MOVE IN-IDARTNR             TO OUTX-IDARTNR                          
022500     MOVE IN-IDRADNR             TO OUTX-IDRADNR                          
022800     MOVE IN-ADGANG              TO OUTX-ADGANG                           
023100     MOVE IN-ADLAGOMR            TO OUTX-ADLAGOMR                         
023200     MOVE IN-ADPLATS             TO OUTX-ADPLATS                          
023300     MOVE IN-FLANLYSF            TO OUTX-FLANLYSF                         
023400     MOVE IN-FLANNULL            TO OUTX-FLANNULL                         
023500     MOVE IN-FLAUTKRE            TO OUTX-FLAUTKRE                         
023600     MOVE IN-FLDIRLEV            TO OUTX-FLDIRLEV                         
023700     MOVE IN-FLSKROT             TO OUTX-FLSKROT                          
028400     MOVE IN-FLSVAR              TO OUTX-FLSVAR                           
028700     MOVE IN-FLTEXT              TO OUTX-FLTEXT                           
029000     MOVE IN-IDANALYS            TO OUTX-IDANALYS                         
029600     MOVE IN-IDANSTNR-RET        TO OUTX-IDANSTNR-RET                     
030900     MOVE IN-IDDC                TO OUTX-IDDC                             
031400     MOVE IN-IDDC-RET-LEV        TO OUTX-IDDC-RET-LEV                     
031500     MOVE IN-IDFAKT              TO OUTX-IDFAKT                           
031510     MOVE IN-IDFAKT-LOC          TO OUTX-IDFAKT-LOC                       
031600     MOVE IN-IDFTG-LEV           TO OUTX-IDFTG-LEV                        
031610     MOVE IN-IDILIST             TO OUTX-IDILIST                          
031620     MOVE IN-IDKNOTNR            TO OUTX-IDKNOTNR                         
031630     MOVE IN-IDKOLLI             TO OUTX-IDKOLLI                          
031640     MOVE IN-IDKONTO             TO OUTX-IDKONTO                          
031650     MOVE IN-IDKST               TO OUTX-IDKST                            
031660     MOVE IN-IDKUNDRF            TO OUTX-IDKUNDRF                         
031670     MOVE IN-IDLOPNRM            TO OUTX-IDLOPNRM                         
031680     MOVE IN-IDPERSON-LEV        TO OUTX-IDPERSON-LEV                     
031700     MOVE IN-IDPERSON-REM        TO OUTX-IDPERSON-REM                     
032000     MOVE IN-IDUSER-PACK         TO OUTX-IDUSER-PACK                      
032300     MOVE IN-KDANMORS            TO OUTX-KDANMORS                         
032600     MOVE IN-KDARBTYP-LEV        TO OUTX-KDARBTYP-LEV                     
032900     MOVE IN-KDARBTYP-REM        TO OUTX-KDARBTYP-REM                     
033200     MOVE IN-KDEMBLEV            TO OUTX-KDEMBLEV                         
033500     MOVE IN-KDFAKTYP            TO OUTX-KDFAKTYP                         
033800     MOVE IN-KDFAKTYP-KNOT       TO OUTX-KDFAKTYP-KNOT                    
034000     MOVE IN-KDFRAKT             TO OUTX-KDFRAKT                          
034300     MOVE IN-KDKREBEH            TO OUTX-KDKREBEH                         
034600     MOVE IN-KDORDKL             TO OUTX-KDORDKL                          
034900     MOVE IN-KVANTAL-ILI         TO OUTX-KVANTAL-ILI                      
035200     MOVE IN-KVAVV-KVAL          TO OUTX-KVAVV-KVAL                       
035500     MOVE IN-KVAVV-KVANT         TO OUTX-KVAVV-KVANT                      
035800     MOVE IN-KVLEVANM            TO OUTX-KVLEVANM                         
036100     MOVE IN-KVLEVANM-BEKR       TO OUTX-KVLEVANM-BEKR                    
036300     MOVE IN-KVRETINL            TO OUTX-KVRETINL                         
036600     MOVE IN-KVRETINL-SKR        TO OUTX-KVRETINL-SKR                     
036900     MOVE IN-PRARTBTO            TO OUTX-PRARTBTO                         
037200     MOVE IN-PRARTBTO-LOC        TO OUTX-PRARTBTO-LOC                     
037500     MOVE IN-PRFRAKT-LEV         TO OUTX-PRFRAKT-LEV                      
037800     MOVE IN-TIFAKT              TO OUTX-TIFAKT                           
038100     MOVE IN-TIFAKT-LOC          TO OUTX-TIFAKT-LOC                       
038400     MOVE IN-TIINLINL            TO OUTX-TIINLINL                         
038700     MOVE IN-TIKNOTA             TO OUTX-TIKNOTA                          
039000     MOVE IN-DALEVANM-LEV        TO OUTX-DALEVANM-LEV                     
039300     MOVE IN-TIREMISS-IN         TO OUTX-TIREMISS-IN                      
039600     MOVE IN-TIREMISS-UT         TO OUTX-TIREMISS-UT                      
039900     MOVE IN-TIUTSKR             TO OUTX-TIUTSKR                          
040200     MOVE IN-IDARTNR-DEL         TO OUTX-IDARTNR-DEL                      
040500     MOVE IN-TIUPPDAT-ILI        TO OUTX-TIUPPDAT-ILI                     
040700     MOVE IN-FLLSBOK             TO OUTX-FLLSBOK                          
041000     MOVE IN-KDAVVTYP            TO OUTX-KDAVVTYP                         
041500     MOVE IN-FLINVUPD            TO OUTX-FLINVUPD                         
041800     MOVE IN-FLPRQUES            TO OUTX-FLPRQUES                         
042100     MOVE IN-IDPRQUES            TO OUTX-IDPRQUES                         
042400     MOVE IN-KDVAT               TO OUTX-KDVAT                            
042700     MOVE IN-BEART-VIPS          TO OUTX-BEART-VIPS                       
043000     MOVE IN-PRARTSTD            TO OUTX-PRARTSTD                         
043300     MOVE IN-PRARTSJK            TO OUTX-PRARTSJK                         
043600     MOVE IN-PRARTBTO-LOCINV     TO OUTX-PRARTBTO-LOCINV                  
043900     MOVE IN-FLRETUR             TO OUTX-FLRETUR                          
044200     MOVE IN-IDANSTNR-ILIU       TO OUTX-IDANSTNR-ILIU                    
044300*WDA221                                                                   
044400     MOVE +1                     TO IX1                                   
044500     PERFORM UNTIL IX1 > 3                                                
044600       MOVE IN-TEANMNOT-REG(IX1) TO OUTX-TEANMNOT-REG(IX1)                
044700       ADD +1                    TO IX1                                   
044800     END-PERFORM                                                          
044900                                                                          
045000     MOVE +1                     TO IX1                                   
045100     PERFORM UNTIL IX1 > 3                                                
045200       MOVE IN-TEANMNOT-ADM(IX1) TO OUTX-TEANMNOT-ADM(IX1)                
045300       ADD +1                    TO IX1                                   
045400     END-PERFORM                                                          
045500                                                                          
045600     MOVE +1                     TO IX1                                   
045700     PERFORM UNTIL IX1 > 3                                                
045800       MOVE IN-TEANMNOT-REM(IX1) TO OUTX-TEANMNOT-REM(IX1)                
045900       ADD +1                    TO IX1                                   
046000     END-PERFORM                                                          
046100                                                                          
046200     MOVE +1                     TO IX1                                   
046300     PERFORM UNTIL IX1 > 3                                                
046400       MOVE IN-TEANMNOT-RET(IX1) TO OUTX-TEANMNOT-RET(IX1)                
046500       ADD +1                    TO IX1                                   
046600     END-PERFORM                                                          
046700                                                                          
046800     MOVE +1                     TO IX1                                   
046900     PERFORM UNTIL IX1 > 3                                                
047000       MOVE IN-TEANMNOT-DLR(IX1) TO OUTX-TEANMNOT-DLR(IX1)                
047100       ADD +1                    TO IX1                                   
047200     END-PERFORM                                                          
048300                                                                          
048400     PERFORM S11A-WRITE-W4184CX                                           
048500     .                                                                    
048600     EJECT                                                                
048700 S11A-WRITE-W4184CX SECTION.                                              
048800                                                                          
048900     WRITE OUTX-RECORD FROM OUTX-AREA                                     
049000                                                                          
049100     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
049200     MOVE 'W4184CX'  TO POSTSUM-FDNAMN                                    
049300     MOVE 'W41847D2' TO POSTSUM-DDNAMN2                                   
049400     CALL POSTSUM USING POSTSUM-PARM                                      
049500     .                                                                    
049600     EJECT                                                                
049700 S99-ABEND SECTION.                                                       
049800                                                                          
049900     SKIP2                                                                
050000     MOVE 'S' TO POSTSUM-OPKOD                                            
050100     CALL POSTSUM USING POSTSUM-PARM                                      
051000     CALL ABEND USING RKOD-ABEND                                          
060000     .                                                                    
