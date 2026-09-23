000100*                  * CONVERTED BY VILMAII *                               
000200*                  * TO PURE COBOLCODE    *                               
000300                                                                          
000400 ID DIVISION.                                                             
000500                                                                          
000600 PROGRAM-ID.             W4739400.                                        
000700*              PROGRAM CONVERTED BY                                       
000800*              COBOL CONVERSION AID PO 5785-ABJ                           
000900*              CONVERSION DATE 05/25/91 16:12:51.                         
001000*AUTHOR.                 SVANTE BJÖRKBERG.                                
001100*DATE-WRITTEN.           JULI 1987.                                       
001200                                                                          
001300*REMARKS.                                                                 
001400                                                                          
001500*    FUNKTION:                                                            
001600*        PROGRAMMET FÅR IDENTITER PÅ FÄRDIGPACKADE ORDER                  
001700*        EXPORT.                                                          
001800*       KOLLIN TILL DESSA ORDER LÄSES UT FRÅN KOLLIREG.                   
001900                                                                          
002000*        EN FIL SKAPAS: - FIL TILL EMBALLAGEPROFORMA.                     
002100                                                                          
002200                                                                          
002300*    ABENDKODER:                                                          
002400*        U0016 - OM RETURKOD FRÅN SORT (EX.VIS FÖR LITE SORTWK)           
002500     EJECT                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200*    ---- INFILER:                                                        
003300*                            - FÄRDIGPACKADE ORDER                        
003400     SELECT  W47628        ASSIGN  W47394D1.                              
003500     SKIP2                                                                
003600*    ---- UTFILER:                                                        
003700*                            - POSTER TILL EMB-PROF                       
003800     SELECT  W47394        ASSIGN  W47394D2.                              
003900     SKIP2                                                                
004000*    ---- SORTFIL:                                                        
004100*                                                                         
004200     SELECT  SORTFIL       ASSIGN  W47394DS.                              
004300     EJECT                                                                
004400 DATA DIVISION.                                                           
004500                                                                          
004600 FILE SECTION.                                                            
004700     SKIP3                                                                
004800 FD  W47628                                                               
004900     RECORDING  F                                                         
005000     BLOCK CONTAINS 0.                                                    
005100                                                                          
005200*    -COPY W47394        -L.                                              
005300     EJECT                                                                
005400 FD  W47394                                                               
005500     RECORDING  F                                                         
005600     BLOCK CONTAINS 0.                                                    
005700                                                                          
005800*01  POST   -COPY W47390       -PRE IN-   -L.                             
005900     EJECT                                                                
006000 SD  SORTFIL                                                              
006100                .                                                         
006200                                                                          
006300*01  POST   -COPY W47394       -PRE SORT-.                                
006400     EJECT                                                                
006500 WORKING-STORAGE SECTION.                                                 
006600                                                                          
006700*    -- CHECKED BY WY2000                                                 
006800 77  INDENT-I PIC X(40) VALUE                                             
006900     'W4739400 91/05/25 TIME 13.09 VILMAII'.                              
007000***  STATEMENT ABOVE GENERATED BY VILMAII CONVERTER                       
007100*                                                                         
007200     SKIP2                                                                
007300*    ---- GENERERAT PROGRAM-NAMN                                          
007400 77  PROGRAM-NAMN REDEFINES INDENT-I                                      
007500                                 PIC X(8).                                
007600                                                                          
007700                                                                          
007800 77  JA                      PIC X       VALUE 'J'.                       
007900 77  NEJ                     PIC X       VALUE 'N'.                       
008000 77  WS-IDDISTR              PIC S9(5)   COMP-3.                          
008100 77  WS-IDKUNDNR             PIC S9(7)   COMP-3.                          
008200 77  WS-KDFAKTYP             PIC X.                                       
008300 77  WS-IDFAKT               PIC S9(7)   COMP-3.                          
008400                                                                          
008500 77  IX                      PIC S9(9)   VALUE +0   COMP SYNC.            
008600     SKIP2                                                                
008700                                                                          
008800 77  SORTFIL-EOF             PIC X       VALUE 'N'.                       
008900                                                                          
009000     EJECT                                                                
009100                                                                          
009200 01  ABEND                   PIC X(8)    VALUE 'ABEND   '.                
009300 01  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.                
009400 01  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.                
009500 01  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.                
009600     SKIP3                                                                
009700 01  WS-KDKOLLI.                                                          
009800     03  WS-KDKOLLI-1        PIC X.                                       
009900     03  WS-KDKOLLI-2        PIC X.                                       
010000     03  WS-KDKOLLI-3        PIC X.                                       
010100     03  FILLER              PIC X(5).                                    
010200 01  FILLER   REDEFINES  WS-KDKOLLI.                                      
010300     03  FILLER              PIC X.                                       
010400     03  WS-KDEMB            PIC 9(3).                                    
010500     03  WS-KDEMB2           PIC 9(1).                                    
010600     03  FILLER              PIC 9(3).                                    
010700     SKIP1                                                                
010800 01  WS-KVPALL               PIC 9(3).                                    
010900 01  WS-KVLOCK               PIC 9(3).                                    
011000 01  WS-KVRAM-X.                                                          
011100     03  WS-KVRAM            PIC 9.                                       
011200     SKIP3                                                                
011300*    ---- PARAMETRAR TILL ABEND                                           
011400                                                                          
011500 01  RKOD.                                                                
011600   03  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   VALUE   +16 COMP SYNC.         
011700   03  RKOD-ABEND-MED-DUMP     PIC S9(4)   VALUE +1000 COMP SYNC.         
011800     EJECT                                                                
011900*    ----  PARAMETRAR TILL POSTSUM                                        
012000                                                                          
012100*01  -COPY W0005       -PRE POSTSUM-.                                     
012200     EJECT                                                                
012300*    ----  AREA FÖR SORTERADE TRANSAKTIONER                               
012400 01  FILLER          PIC X(24)   VALUE 'WSORT-AREA-START'.                
012500     SKIP2                                                                
012600*01  AREA  -COPY W47394      -PRE  WSORT-.                                
012700     EJECT                                                                
012800*    ----  AREA FÖR EMB-PROF                                              
012900 01  FILLER          PIC X(24)   VALUE 'EMB-AREA-START'.                  
013000     SKIP2                                                                
013100*01  AREA   -COPY W47390      -PRE EMB-.                                  
013200     EJECT                                                                
013300 PROCEDURE DIVISION.                                                      
013400     ENTRY 'DLITCBL'.                                                     
013500                                                                          
013600     PERFORM A-INIT                                                       
013700                                                                          
013800     SORT SORTFIL                                                         
013900       ASCENDING SORT-EXP-EMB-IDDISTR                                     
014000                 SORT-EXP-EMB-IDKUNDNR                                    
014100                 SORT-EXP-EMB-KDFAKTYP                                    
014200                 SORT-EXP-EMB-IDFAKT                                      
014300       USING W47628                                                       
014400       OUTPUT PROCEDURE B-LAS-BAS-SKAPA-FIL                               
014500                                                                          
014600     IF  SORT-RETURN > ZERO                                               
014700       DISPLAY '*** W4739400 - FEL VID SORTERING'                         
014800       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
014900     ELSE                                                                 
015000       PERFORM Z-FINIT                                                    
015100       MOVE ZERO TO RETURN-CODE                                           
015200       GOBACK                                                             
015300     END-IF                                                               
015400     .                                                                    
015500     EJECT                                                                
015600 A-INIT       SECTION.                                                    
015700                                                                          
015800     OPEN OUTPUT W47394                                                   
015900                                                                          
016000     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
016100                                                                          
016200     MOVE +1                 TO IX                                        
016300     PERFORM UNTIL                                                        
016400      NOT ( IX < 25)                                                      
016500       MOVE ZERO          TO EMB-KVPALL (IX)                              
016600                             EMB-KVRAM  (IX)                              
016700                             EMB-KVLOCK (IX)                              
016800                             EMB-KVEMBSPA-02 (IX)                         
016900       ADD +1             TO IX                                           
017000     END-PERFORM                                                          
017100     .                                                                    
017200     EJECT                                                                
017300 B-LAS-BAS-SKAPA-FIL   SECTION.                                           
017400                                                                          
017500     PERFORM BA-LAS-SORTERAD-W47628                                       
017600                                                                          
017700     PERFORM UNTIL                                                        
017800      NOT ( SORTFIL-EOF = NEJ )                                           
017900       MOVE WSORT-EXP-EMB-IDDISTR  TO WS-IDDISTR                          
018000       MOVE WSORT-EXP-EMB-IDKUNDNR TO WS-IDKUNDNR                         
018100       MOVE WSORT-EXP-EMB-KDFAKTYP TO WS-KDFAKTYP                         
018200       MOVE WSORT-EXP-EMB-IDFAKT   TO WS-IDFAKT                           
018300                                                                          
018400       PERFORM BD-FLYTTA-TILL-EXP-EMB-PROF                                
018500                                                                          
018600       PERFORM UNTIL                                                      
018700        NOT ( WSORT-EXP-EMB-IDDISTR = WS-IDDISTR AND                      
018800          WSORT-EXP-EMB-IDKUNDNR = WS-IDKUNDNR AND                        
018900          WSORT-EXP-EMB-KDFAKTYP = WS-KDFAKTYP AND                        
019000          WSORT-EXP-EMB-IDFAKT = WS-IDFAKT AND SORTFIL-EOF =              
019100          NEJ )                                                           
019200         PERFORM BC-BEHANDLA-KOLLI                                        
019300         PERFORM BA-LAS-SORTERAD-W47628                                   
019400       END-PERFORM                                                        
019500       PERFORM BE-SKRIV-EXP-EMB-PROF                                      
019600     END-PERFORM                                                          
019700     .                                                                    
019800     EJECT                                                                
019900 BA-LAS-SORTERAD-W47628       SECTION.                                    
020000                                                                          
020100     RETURN SORTFIL INTO WSORT-AREA                                       
020200     AT END                                                               
020300        MOVE JA TO SORTFIL-EOF                                            
020400     END-RETURN                                                           
020500                                                                          
020600     IF SORTFIL-EOF = NEJ                                                 
020700       MOVE 'W47628'               TO POSTSUM-FDNAMN                      
020800       MOVE 'W47394D1'             TO POSTSUM-DDNAMN2                     
020900       MOVE SPACE                  TO POSTSUM-TRANSTYP                    
021000       CALL POSTSUM USING POSTSUM-PARM                                    
021100     END-IF                                                               
021200     .                                                                    
021300     EJECT                                                                
021400 BC-BEHANDLA-KOLLI SECTION.                                               
021500                                                                          
021600     MOVE WSORT-EXP-EMB-KDKOLLI  TO WS-KDKOLLI                            
021700                                                                          
021800     IF   WS-KDKOLLI = 'FLEN    '                                         
021900       ADD +1                    TO EMB-KVPALL (1)                        
022000       ADD +2                    TO EMB-KVPALL (2)                        
022100       ADD +8                    TO EMB-KVRAM  (2)                        
022200       ADD +1                    TO EMB-KVLOCK (1)                        
022300       ADD +1                    TO EMB-KVLOCK (2)                        
022400     ELSE                                                                 
022500       IF     WS-KDKOLLI-1 = 'L'                                          
023800*                                                                         
023900*        NYTT EMBALLAGE FÅR PLATS 22 I TABELLEN                           
024000*                                                                         
024100         IF WS-KDKOLLI-3 NUMERIC                                          
024200           ADD +1              TO EMB-KVPALL (22)                         
024300           ADD +1              TO EMB-KVLOCK (22)                         
024400           MOVE WS-KDKOLLI-3 TO WS-KVRAM-X                                
024500           ADD WS-KVRAM        TO EMB-KVRAM  (22)                         
024600           IF WS-KDKOLLI-3 NOT = '0'                                      
024700             ADD +1           TO EMB-KVEMBSPA-02(22)                      
024800           END-IF                                                         
025000         ELSE                                                             
025100                                                                          
025200           IF WS-KDKOLLI-2 NUMERIC                                        
025210             ADD +1            TO EMB-KVPALL (1)                          
025300             IF WS-KDKOLLI-3 = 'D'                                        
025400               ADD +2          TO EMB-KVLOCK (1)                          
025500             ELSE                                                         
025600               ADD +1          TO EMB-KVLOCK (1)                          
025700             END-IF                                                       
025800             MOVE WS-KDKOLLI-2 TO WS-KVRAM-X                              
025900             ADD WS-KVRAM    TO EMB-KVRAM  (1)                            
026000            IF WS-KDKOLLI-2 NOT = '0'                                     
026100             ADD +1          TO EMB-KVEMBSPA-02(1)                        
026200            END-IF                                                        
026300           END-IF                                                         
026400         END-IF                                                           
026500       ELSE                                                               
026600         EVALUATE TRUE                                                    
026700         WHEN WS-KDKOLLI-1 = 'K'                                          
026800           ADD +1                TO EMB-KVPALL (2)                        
026900           IF WS-KDKOLLI-2 NUMERIC                                        
027000             IF  WS-KDKOLLI-3 = 'D'                                       
027100               ADD +2            TO EMB-KVLOCK (2)                        
027200             ELSE                                                         
027300               ADD +1            TO EMB-KVLOCK (2)                        
027400             END-IF                                                       
027500             MOVE WS-KDKOLLI-2 TO WS-KVRAM-X                              
027600             ADD WS-KVRAM      TO EMB-KVRAM  (2)                          
027700            IF WS-KDKOLLI-2 NOT = '0'                                     
027800             ADD +1            TO EMB-KVEMBSPA-02(2)                      
027900            END-IF                                                        
028000           END-IF                                                         
028100         WHEN WS-KDKOLLI-1 = 'F'                                          
029000*                                                                         
029100*          NYTT EMBALLAGE FÅR PLATS 23 I TABELLEN                         
029200*                                                                         
029210           IF WS-KDKOLLI-3 NUMERIC                                        
029300             IF WS-KDKOLLI-3 NUMERIC                                      
029400               ADD +1            TO EMB-KVPALL (23)                       
029500               ADD +1            TO EMB-KVLOCK (23)                       
029600               MOVE WS-KDKOLLI-3 TO WS-KVRAM-X                            
029700               ADD WS-KVRAM      TO EMB-KVRAM  (23)                       
029800               IF WS-KDKOLLI-3 NOT = '0'                                  
029900                ADD +1           TO EMB-KVEMBSPA-02(23)                   
030000               END-IF                                                     
030100             END-IF                                                       
030200           ELSE                                                           
030300             IF WS-KDKOLLI-2 NUMERIC                                      
030310               ADD +1            TO EMB-KVPALL (3)                        
030400               IF WS-KDKOLLI-3 = 'D'                                      
030500                 ADD +2          TO EMB-KVLOCK (3)                        
030600               ELSE                                                       
030700                 ADD +1          TO EMB-KVLOCK (3)                        
030800               END-IF                                                     
030900               MOVE WS-KDKOLLI-2 TO WS-KVRAM-X                            
031000               ADD WS-KVRAM    TO EMB-KVRAM  (3)                          
031100              IF WS-KDKOLLI-2 NOT = '0'                                   
031200               ADD +1          TO EMB-KVEMBSPA-02(3)                      
031300              END-IF                                                      
031400             END-IF                                                       
031500           END-IF                                                         
031600         WHEN WS-KDKOLLI-1 = 'G'                                          
031700           ADD +1                TO EMB-KVPALL (4)                        
031800           IF WS-KDKOLLI-2 NUMERIC                                        
031900             IF  WS-KDKOLLI-3 = 'D'                                       
032000               ADD +2            TO EMB-KVLOCK (4)                        
032100             ELSE                                                         
032200               ADD +1            TO EMB-KVLOCK (4)                        
032300             END-IF                                                       
032400             MOVE WS-KDKOLLI-2 TO WS-KVRAM-X                              
032500             ADD WS-KVRAM      TO EMB-KVRAM  (4)                          
032600            IF WS-KDKOLLI-2 NOT = '0'                                     
032700             ADD +1            TO EMB-KVEMBSPA-02(4)                      
032800            END-IF                                                        
032900           END-IF                                                         
033000         WHEN WS-KDKOLLI-1 = 'H'                                          
033100           ADD +1                TO EMB-KVPALL (5)                        
033200           IF WS-KDKOLLI-2 NUMERIC                                        
033300             IF  WS-KDKOLLI-3 = 'D'                                       
033400               ADD +2            TO EMB-KVLOCK (5)                        
033500             ELSE                                                         
033600               ADD +1            TO EMB-KVLOCK (5)                        
033700             END-IF                                                       
033800             MOVE WS-KDKOLLI-2 TO WS-KVRAM-X                              
033900             ADD WS-KVRAM      TO EMB-KVRAM  (5)                          
034000            IF WS-KDKOLLI-2 NOT = '0'                                     
034100             ADD +1            TO EMB-KVEMBSPA-02(5)                      
034200            END-IF                                                        
034300           END-IF                                                         
034400         WHEN WS-KDKOLLI-1 = 'U'                                          
034500           ADD +1                TO EMB-KVPALL (6)                        
034600           IF WS-KDKOLLI-2 NUMERIC                                        
034700             IF  WS-KDKOLLI-3 = 'D'                                       
034800               ADD +2            TO EMB-KVLOCK (6)                        
034900             ELSE                                                         
035000               ADD +1            TO EMB-KVLOCK (6)                        
035100             END-IF                                                       
035200             MOVE WS-KDKOLLI-2 TO WS-KVRAM-X                              
035300             ADD WS-KVRAM      TO EMB-KVRAM  (6)                          
035400             ADD +1            TO EMB-KVEMBSPA-02(6)                      
035500           END-IF                                                         
035600         WHEN WS-KDKOLLI-1 = 'W'                                          
035700           ADD +1                TO EMB-KVPALL (7)                        
035800           IF WS-KDKOLLI-2 NUMERIC                                        
035900             IF  WS-KDKOLLI-3 = 'D'                                       
036000               ADD +2            TO EMB-KVLOCK (7)                        
036100             ELSE                                                         
036200               ADD +1            TO EMB-KVLOCK (7)                        
036300             END-IF                                                       
036400             MOVE WS-KDKOLLI-2 TO WS-KVRAM-X                              
036500             ADD WS-KVRAM      TO EMB-KVRAM  (7)                          
036600             ADD +1            TO EMB-KVEMBSPA-02(7)                      
036700           END-IF                                                         
036800         WHEN WS-KDKOLLI-1 = 'Y'                                          
036900          IF WS-KDEMB = 790                                               
037000            ADD +1             TO EMB-KVPALL (11)                         
037100            IF WS-KDKOLLI-2 NUMERIC                                       
037200             ADD +1            TO EMB-KVLOCK (11)                         
037300             ADD +1            TO EMB-KVRAM  (11)                         
037400             ADD +1            TO EMB-KVEMBSPA-02(11)                     
037500            END-IF                                                        
037600          ELSE                                                            
037700           IF WS-KDEMB = 750                                              
037800            ADD +1             TO EMB-KVPALL (12)                         
037900            IF WS-KDKOLLI-2 NUMERIC                                       
038000             ADD +1            TO EMB-KVLOCK (12)                         
038100             ADD +1            TO EMB-KVRAM  (12)                         
038200             ADD +1            TO EMB-KVEMBSPA-02(12)                     
038300            END-IF                                                        
038400           ELSE                                                           
038500*           KDEMB 780                                                     
038600            ADD +1                TO EMB-KVPALL (8)                       
038700            IF WS-KDKOLLI-2 NUMERIC                                       
038800             IF  WS-KDKOLLI-3 = 'D'                                       
038900               ADD +2            TO EMB-KVLOCK (8)                        
039000             ELSE                                                         
039100               ADD +1            TO EMB-KVLOCK (8)                        
039200             END-IF                                                       
039300             MOVE WS-KDKOLLI-2 TO WS-KVRAM-X                              
039400             ADD WS-KVRAM      TO EMB-KVRAM  (8)                          
039500             ADD +1            TO EMB-KVEMBSPA-02(8)                      
039600            END-IF                                                        
039700           END-IF                                                         
039800          END-IF                                                          
039900         WHEN WS-KDKOLLI-1 = 'C'                                          
040000          EVALUATE TRUE                                                   
040100           WHEN WS-KDEMB = 419                                            
040200            ADD +1             TO EMB-KVPALL (9)                          
040300            IF WS-KDKOLLI-2 NUMERIC                                       
040400             ADD +1            TO EMB-KVLOCK (9)                          
040500             ADD +1            TO EMB-KVRAM  (9)                          
040600             ADD +1            TO EMB-KVEMBSPA-02(9)                      
040700            END-IF                                                        
040800           WHEN WS-KDEMB = 422                                            
040900            ADD +1              TO EMB-KVPALL (10)                        
041000            IF WS-KDKOLLI-2 NUMERIC                                       
041100             ADD +1            TO EMB-KVLOCK (10)                         
041200             ADD +1            TO EMB-KVRAM  (10)                         
041300             ADD +1            TO EMB-KVEMBSPA-02(10)                     
041400            END-IF                                                        
041500           WHEN WS-KDEMB = 142 AND WS-KDEMB2 = 1                          
041600            ADD +1             TO EMB-KVPALL (13)                         
041700            IF WS-KDKOLLI-2 NUMERIC                                       
041800             ADD +1            TO EMB-KVLOCK (13)                         
041900             ADD +1            TO EMB-KVRAM  (13)                         
042000             ADD +1            TO EMB-KVEMBSPA-02(13)                     
042100            END-IF                                                        
042200           WHEN WS-KDEMB = 107                                            
042300            ADD +1             TO EMB-KVPALL (14)                         
042400            IF WS-KDKOLLI-2 NUMERIC                                       
042500             ADD +1            TO EMB-KVLOCK (14)                         
042600             ADD +1            TO EMB-KVRAM  (14)                         
042700             ADD +1            TO EMB-KVEMBSPA-02(14)                     
042800            END-IF                                                        
042900           WHEN WS-KDEMB = 460                                            
043000            ADD +1             TO EMB-KVPALL (15)                         
043100            IF WS-KDKOLLI-2 NUMERIC                                       
043200             ADD +1            TO EMB-KVLOCK (15)                         
043300             ADD +1            TO EMB-KVRAM  (15)                         
043400             ADD +1            TO EMB-KVEMBSPA-02(15)                     
043500            END-IF                                                        
043600           WHEN WS-KDEMB = 576                                            
043700            ADD +1             TO EMB-KVPALL (16)                         
043800            IF WS-KDKOLLI-2 NUMERIC                                       
043900             ADD +1            TO EMB-KVLOCK (16)                         
044000             ADD +1            TO EMB-KVRAM  (16)                         
044100             ADD +1            TO EMB-KVEMBSPA-02(16)                     
044200            END-IF                                                        
044300           WHEN WS-KDEMB = 595                                            
044400            ADD +1             TO EMB-KVPALL (17)                         
044500            IF WS-KDKOLLI-2 NUMERIC                                       
044600             ADD +1            TO EMB-KVLOCK (17)                         
044700             ADD +1            TO EMB-KVRAM  (17)                         
044800             ADD +1            TO EMB-KVEMBSPA-02(17)                     
044900            END-IF                                                        
045000           WHEN WS-KDEMB = 724                                            
045100            ADD +1             TO EMB-KVPALL (18)                         
045200            IF WS-KDKOLLI-2 NUMERIC                                       
045300             ADD +1            TO EMB-KVLOCK (18)                         
045400             ADD +1            TO EMB-KVRAM  (18)                         
045500             ADD +1            TO EMB-KVEMBSPA-02(18)                     
045600            END-IF                                                        
045700           WHEN WS-KDEMB = 742                                            
045800            ADD +1             TO EMB-KVPALL (19)                         
045900            IF WS-KDKOLLI-2 NUMERIC                                       
046000             ADD +1            TO EMB-KVLOCK (19)                         
046100             ADD +1            TO EMB-KVRAM  (19)                         
046200             ADD +1            TO EMB-KVEMBSPA-02(19)                     
046300            END-IF                                                        
046400           WHEN WS-KDEMB = 743                                            
046500            ADD +1             TO EMB-KVPALL (20)                         
046600            IF WS-KDKOLLI-2 NUMERIC                                       
046700             ADD +1            TO EMB-KVLOCK (20)                         
046800             ADD +1            TO EMB-KVRAM  (20)                         
046900             ADD +1            TO EMB-KVEMBSPA-02(20)                     
047000            END-IF                                                        
047100           WHEN WS-KDEMB = 840                                            
047200            ADD +1             TO EMB-KVPALL (21)                         
047300            IF WS-KDKOLLI-2 NUMERIC                                       
047400             ADD +1            TO EMB-KVLOCK (21)                         
047500             ADD +1            TO EMB-KVRAM  (21)                         
047600             ADD +1            TO EMB-KVEMBSPA-02(21)                     
047700            END-IF                                                        
047800          END-EVALUATE                                                    
047900         END-EVALUATE                                                     
048000       END-IF                                                             
048100     END-IF                                                               
048200     .                                                                    
048300     EJECT                                                                
048400 BD-FLYTTA-TILL-EXP-EMB-PROF  SECTION.                                    
048500                                                                          
048600     MOVE WSORT-EXP-EMB-IDDISTR    TO EMB-IDDISTR                         
048700     MOVE WSORT-EXP-EMB-IDKUNDNR   TO EMB-IDKUNDNR                        
048800     MOVE WSORT-EXP-EMB-IDDC-SEND  TO EMB-IDDC-SEND                       
048900     MOVE WSORT-EXP-EMB-IDDC-REC   TO EMB-IDDC-REC                        
049000     MOVE SPACE                    TO EMB-IDKUNDRF                        
049100     MOVE ZERO                     TO EMB-IDPRODNR                        
049200                                      EMB-KDORDKL                         
049300     MOVE WSORT-EXP-EMB-KDFAKTYP   TO EMB-KDFAKTYP                        
049400     MOVE WSORT-EXP-EMB-IDFAKT     TO EMB-IDFAKT                          
049500     .                                                                    
049600     EJECT                                                                
049700 BE-SKRIV-EXP-EMB-PROF        SECTION.                                    
049800                                                                          
049900     WRITE IN-POST FROM EMB-AREA                                          
050000                                                                          
050100     MOVE 'W47394'               TO POSTSUM-FDNAMN                        
050200     MOVE 'W47394D2'             TO POSTSUM-DDNAMN2                       
050300     MOVE SPACE                  TO POSTSUM-TRANSTYP                      
050400     CALL POSTSUM USING POSTSUM-PARM                                      
050500                                                                          
050600     MOVE +1                 TO IX                                        
050700     PERFORM UNTIL                                                        
050800      NOT ( IX < 25)                                                      
050900       MOVE ZERO          TO EMB-KVPALL (IX)                              
051000                             EMB-KVRAM  (IX)                              
051100                             EMB-KVLOCK (IX)                              
051200                             EMB-KVEMBSPA-02(IX)                          
051300       ADD +1             TO IX                                           
051400     END-PERFORM                                                          
051500     .                                                                    
051600     EJECT                                                                
051700 Z-FINIT   SECTION.                                                       
051800                                                                          
051900     CLOSE  W47394                                                        
052000                                                                          
052100     MOVE 'S' TO POSTSUM-OPKOD                                            
053000     CALL POSTSUM USING POSTSUM-PARM                                      
060000     .                                                                    
