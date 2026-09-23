000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BMP-DB2-PGM               
000300*                                                                         
000400 ID DIVISION.                                                             
000500 PROGRAM-ID.     WF200700.                                                
000600 AUTHOR.         BO HAMMARIN.                                             
000700 DATE-WRITTEN.   JUNE 2015.                                               
000800 DATE-COMPILED.                                                           
000900                                                                          
001000*   PGM                                                                   
001100*   1 UPDATES DOCUMENT HEADERS                                            
001200*     WITH MISC NON-KEY INFO FROM T01SLIN                                 
001400*                                                                         
001500*   PGM UPDATES                                                           
001600*   - ROWS IN TABLE T01DHEA                                               
001700*                                                                         
002100*   PGM READS                                                             
002300*   - ROWS IN TABLE T01PROC                                               
002400*   - ROWS IN TABLE T01SLIN                                               
002600*                                                                         
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200                                                                          
003300 DATA DIVISION.                                                           
003400                                                                          
003500 FILE SECTION.                                                            
003600                                                                          
003700 WORKING-STORAGE SECTION.                                                 
003800*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                        PIC X(8)  VALUE 'WF200700'.             
004000 77  WS-IDSYSTEM                  PIC X(4)  VALUE 'WF02'.                 
004100                                                                          
004700 01  WS-DIVERSE-MULTIFETCH.                                               
004800     03 WS-MX                    PIC S9(3)  COMP-3.                       
004900     03 WS-MULTIFETCH            PIC S9(3)  COMP-3.                       
005000                                                                          
005100     03 WS-SLIN-IDLEGSEL      OCCURS 100 PIC X(4).                        
005200     03 WS-SLIN-DAEXDAT       OCCURS 100 PIC X(8).                        
005300     03 WS-SLIN-TIEXTID       OCCURS 100 PIC S9(7) COMP-3.                
005400     03 WS-SLIN-KDVALISO      OCCURS 100 PIC X(3).                        
005500     03 WS-SLIN-IDLANDX3-SEND OCCURS 100 PIC X(3).                        
005600     03 WS-SLIN-IDLEVNR       OCCURS 100 PIC X(5).                        
005700     03 WS-SLIN-IDPARTNR      OCCURS 100 PIC X(9).                        
005800     03 WS-SLIN-KDFINDOC      OCCURS 100 PIC X(4).                        
005900     03 WS-SLIN-FLSOFT        OCCURS 100 PIC X(1).                        
006000     03 WS-SLIN-FLFREE        OCCURS 100 PIC X(1).                        
006100     03 WS-SLIN-FLPRIV        OCCURS 100 PIC X(1).                        
006200     03 WS-SLIN-IDBREAK-1     OCCURS 100 PIC X(8).                        
006300     03 WS-SLIN-IDBREAK-2     OCCURS 100 PIC X(8).                        
006400     03 WS-SLIN-IDSYSTEM-SEND OCCURS 100 PIC X(4).                        
006500     03 WS-SLIN-IDSYSTEM-REC  OCCURS 100 PIC X(4).                        
006600     03 WS-SLIN-BEANST        OCCURS 100 PIC X(25).                       
006700     03 WS-SLIN-IDUSER        OCCURS 100 PIC X(8).                        
006800     03 WS-SLIN-BETEXT        OCCURS 100 PIC X(125).                      
006900     03 WS-SLIN-BETEXT-CRE    OCCURS 100 PIC X(100).                      
007000     03 WS-DONS-IDFINDOC-NEXT OCCURS 100 PIC S9(9) COMP-3.                
007100     EJECT                                                                
007200                                                                          
007300 01  WS-ONE-LINE.                                                         
007400     03 WS-IDLEGSEL                 PIC X(4).                             
007500     03 WS-DAEXDAT                  PIC X(8).                             
007600     03 WS-TIEXTID                  PIC S9(7) COMP-3.                     
007700     03 WS-KDVALISO                 PIC X(3).                             
007800     03 WS-IDLANDX3-SEND            PIC X(3).                             
007900     03 WS-IDLEVNR                  PIC X(5).                             
008000     03 WS-IDPARTNR                 PIC X(9).                             
008100     03 WS-KDFINDOC                 PIC X(4).                             
008200     03 WS-FLSOFT                   PIC X(1).                             
008300     03 WS-FLFREE                   PIC X(1).                             
008400     03 WS-FLPRIV                   PIC X(1).                             
008500     03 WS-IDBREAK-1                PIC X(8).                             
008600     03 WS-IDBREAK-2                PIC X(8).                             
008700     03 WS-IDSYSTEM-SEND            PIC X(4).                             
008800     03 WS-IDSYSTEM-REC             PIC X(4).                             
008900     03 WS-BEANST                   PIC X(25).                            
009000     03 WS-IDUSER                   PIC X(8).                             
009100     03 WS-BETEXT                   PIC X(125).                           
009200     03 WS-BETEXT-CRE               PIC X(100).                           
009300     03 WS-IDFINDOC-NEXT            PIC S9(9) COMP-3.                     
009400     EJECT                                                                
009500                                                                          
009600 01  DYNAMISKA-SUBPROGRAM.                                                
009700*                                                                         
009800     03  ABEND                    PIC X(8)  VALUE 'ABEND   '.             
009900                                                                          
010000 01  FELTEXT                      PIC X(80) VALUE SPACE.                  
010100 01  RKOD-ABEND-DB2               PIC S9(4) VALUE +998  COMP SYNC.        
010200 01  RKOD-ABEND-MED-DUMP          PIC S9(4) VALUE +1000 COMP SYNC.        
010300     EJECT                                                                
010400*                                                                         
010500*        WORK-AREAS FOR DB2-SECTIONS                                      
010600*                                                                         
011000 01  FILLER                       PIC X(16)  VALUE 'PROC-TAB   '.         
011100*01  -COPY T01PROC    -PRE PROC-                                          
011101*                                                                         
011102 01  FILLER                       PIC X(16)  VALUE 'SLIN-TAB   '.         
011103*01  -COPY T01SLIN    -PRE SLIN-                                          
011200                                                                          
012200 01  FILLER                       PIC X(16)  VALUE 'DHEA-TAB   '.         
012300*01  -COPY T01DHEA    -PRE DHEA-                                          
012400     EJECT                                                                
012500                                                                          
014100 01  FILLER                       PIC X(16)  VALUE 'PROC-AREA'.           
014200       EXEC SQL INCLUDE T01PROC  END-EXEC.                                
014300     EJECT                                                                
014301                                                                          
014302 01  FILLER                       PIC X(16)  VALUE 'SLIN-AREA'.           
014303       EXEC SQL INCLUDE T01SLIN  END-EXEC.                                
014304     EJECT                                                                
014400                                                                          
014401 01  FILLER                       PIC X(16)  VALUE 'DHEA-AREA'.           
014402       EXEC SQL INCLUDE T01DHEA  END-EXEC.                                
014403     EJECT                                                                
014404                                                                          
014500 01  FILLER                       PIC X(16)  VALUE 'SQLCA-AREA'.          
014600       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
014700*                        **** STATUS-CODE FROM DB2                        
014800                                                                          
014900 01  FILLER                       PIC X(16)  VALUE 'SQLCODE-WS'.          
015000 01  DB2-WS.                                                              
015100   03  SQLCODE-WS                 PIC S9(3)  VALUE ZERO.                  
015200     88  ROW-FOUND                           VALUE +000.                  
015300     88  ROW-MISSING                         VALUE +100.                  
015400     88  ROW-OVERFLOW                        VALUE -413.                  
015500   03  GOOD-SQLCODES.                                                     
015600     05  GOOD-SQLCODE OCCURS 5                                            
015700         INDEXED BY SQLCODE-IX    PIC 999.                                
015800     EJECT                                                                
015900                                                                          
016000 PROCEDURE DIVISION.                                                      
016100 MAIN SECTION.                                                            
016200     PERFORM A-INIT                                                       
016300                                                                          
016400     PERFORM B-EXECUTE                                                    
016500                                                                          
016600     PERFORM Z-FINISH                                                     
016700     MOVE ZERO TO RETURN-CODE                                             
016800     GOBACK                                                               
016900     .                                                                    
017000     EJECT                                                                
017100                                                                          
017200 A-INIT SECTION.                                                          
017300     .                                                                    
017400     EJECT                                                                
017500                                                                          
017600 B-EXECUTE SECTION.                                                       
017700     PERFORM DB2-OPEN-CRS-SLIN                                            
017800     PERFORM DB2-FETCH-CRS-SLIN                                           
017900     IF SQLERRD(3) > 0                                                    
018000       MOVE 000     TO SQLCODE-WS                                         
018100     END-IF                                                               
018200     PERFORM UNTIL ROW-MISSING                                            
018300       MOVE SQLERRD(3) TO WS-MULTIFETCH                                   
018400       MOVE ZERO       TO WS-MX                                           
018500       PERFORM UNTIL WS-MX = WS-MULTIFETCH                                
018600         ADD +1        TO WS-MX                                           
018700         MOVE WS-SLIN-IDLEGSEL(WS-MX)      TO WS-IDLEGSEL                 
018800         MOVE WS-SLIN-DAEXDAT(WS-MX)       TO WS-DAEXDAT                  
018900         MOVE WS-SLIN-TIEXTID(WS-MX)       TO WS-TIEXTID                  
019000         MOVE WS-SLIN-KDVALISO(WS-MX)      TO WS-KDVALISO                 
019100         MOVE WS-SLIN-IDLANDX3-SEND(WS-MX) TO WS-IDLANDX3-SEND            
019200         MOVE WS-SLIN-IDLEVNR(WS-MX)       TO WS-IDLEVNR                  
019300         MOVE WS-SLIN-IDPARTNR(WS-MX)      TO WS-IDPARTNR                 
019400         MOVE WS-SLIN-KDFINDOC(WS-MX)      TO WS-KDFINDOC                 
019500         MOVE WS-SLIN-FLSOFT(WS-MX)        TO WS-FLSOFT                   
019600         MOVE WS-SLIN-FLFREE(WS-MX)        TO WS-FLFREE                   
019700         MOVE WS-SLIN-FLPRIV(WS-MX)        TO WS-FLPRIV                   
019800         MOVE WS-SLIN-IDBREAK-1(WS-MX)     TO WS-IDBREAK-1                
019900         MOVE WS-SLIN-IDBREAK-2(WS-MX)     TO WS-IDBREAK-2                
020000         MOVE WS-SLIN-IDSYSTEM-SEND(WS-MX) TO WS-IDSYSTEM-SEND            
020100         MOVE WS-SLIN-IDSYSTEM-REC(WS-MX)  TO WS-IDSYSTEM-REC             
020200         MOVE WS-SLIN-BEANST(WS-MX)        TO WS-BEANST                   
020300         MOVE WS-SLIN-IDUSER(WS-MX)        TO WS-IDUSER                   
020400         MOVE WS-SLIN-BETEXT(WS-MX)        TO WS-BETEXT                   
020500         MOVE WS-SLIN-BETEXT-CRE(WS-MX)    TO WS-BETEXT-CRE               
020600                                                                          
023500         PERFORM DB2-UPDATE-DHEA                                          
023501       END-PERFORM                                                        
023502                                                                          
023600       IF WS-MULTIFETCH = 100                                             
023700         PERFORM DB2-FETCH-CRS-SLIN                                       
023800         IF SQLERRD(3) > 0                                                
023900           MOVE 000     TO SQLCODE-WS                                     
024000         END-IF                                                           
024100       ELSE                                                               
024200         MOVE 100 TO SQLCODE-WS                                           
024300       END-IF                                                             
024400     END-PERFORM                                                          
024500     .                                                                    
024600     EJECT                                                                
024700                                                                          
024800 Z-FINISH SECTION.                                                        
024900     PERFORM DB2-CLOSE-CRS-SLIN                                           
025000     .                                                                    
025100     EJECT                                                                
025200                                                                          
025300* --- DB2 SECTIONS  ---                                                   
025400*                                                                         
025600 DB2-OPEN-CRS-SLIN SECTION.                                               
025700     EXEC SQL                                                             
025800              DECLARE SLIN-CRS CURSOR WITH ROWSET POSITIONING FOR         
025900              SELECT DISTINCT                                             
026000              T01SLIN.IDLEGSEL,                                           
026100              T01SLIN.DAEXDAT,                                            
026200              T01SLIN.TIEXTID,                                            
026300              T01SLIN.KDVALISO,                                           
026400              T01SLIN.IDLANDX3_SEND,                                      
026500              T01SLIN.IDLEVNR,                                            
026600              T01SLIN.IDPARTNR,                                           
026700              T01SLIN.KDFINDOC,                                           
026800              T01SLIN.FLSOFT,                                             
026900              T01SLIN.FLFREE,                                             
027000              T01SLIN.FLPRIV,                                             
027100              T01SLIN.IDBREAK_1,                                          
027200              T01SLIN.IDBREAK_2,                                          
027300              T01SLIN.IDSYSTEM_SEND,                                      
027400              T01SLIN.IDSYSTEM_REC,                                       
027500              T01SLIN.BEANST,                                             
027600              T01SLIN.IDUSER,                                             
027700              T01SLIN.BETEXT,                                             
027800              T01SLIN.BETEXT_CRE                                          
027900                                                                          
028000     FROM     T01PROC,                                                    
028100              T01SLIN                                                     
028200                                                                          
028300     WHERE    T01PROC.IDSYSTEM = 'WF02'                                   
028400          AND T01SLIN.IDLEGSEL = T01PROC.IDLEGSEL                         
028500          AND T01SLIN.DAEXDAT  = T01PROC.DAEXDAT                          
028600          AND T01SLIN.TIEXTID  = T01PROC.TIEXTID                          
028700     END-EXEC                                                             
028800                                                                          
028900     EXEC SQL OPEN SLIN-CRS                                               
029000     END-EXEC                                                             
029100                                                                          
029200     MOVE 000            TO GOOD-SQLCODES                                 
029300     MOVE SQLCODE        TO SQLCODE-WS                                    
029400     PERFORM DB2-STATUS-CHECK                                             
029500     .                                                                    
029600     EJECT                                                                
029700                                                                          
029800 DB2-FETCH-CRS-SLIN SECTION.                                              
029900     EXEC SQL                                                             
030000            FETCH NEXT ROWSET FROM SLIN-CRS FOR 100 ROWS                  
030100       INTO :WS-SLIN-IDLEGSEL,                                            
030200            :WS-SLIN-DAEXDAT,                                             
030300            :WS-SLIN-TIEXTID,                                             
030400            :WS-SLIN-KDVALISO,                                            
030500            :WS-SLIN-IDLANDX3-SEND,                                       
030600            :WS-SLIN-IDLEVNR,                                             
030700            :WS-SLIN-IDPARTNR,                                            
030800            :WS-SLIN-KDFINDOC,                                            
030900            :WS-SLIN-FLSOFT,                                              
031000            :WS-SLIN-FLFREE,                                              
031100            :WS-SLIN-FLPRIV,                                              
031200            :WS-SLIN-IDBREAK-1,                                           
031300            :WS-SLIN-IDBREAK-2,                                           
031400            :WS-SLIN-IDSYSTEM-SEND,                                       
031500            :WS-SLIN-IDSYSTEM-REC,                                        
031600            :WS-SLIN-BEANST,                                              
031700            :WS-SLIN-IDUSER,                                              
031800            :WS-SLIN-BETEXT,                                              
031900            :WS-SLIN-BETEXT-CRE                                           
032000     END-EXEC                                                             
032100                                                                          
032200     MOVE 000100         TO GOOD-SQLCODES                                 
032300     MOVE SQLCODE        TO SQLCODE-WS                                    
032400     PERFORM DB2-STATUS-CHECK                                             
032500     .                                                                    
032600     EJECT                                                                
046500                                                                          
046600 DB2-UPDATE-DHEA SECTION.                                                 
046700     EXEC SQL UPDATE T01DHEA                                              
046800     SET    IDSYSTEM_SEND     = :WS-IDSYSTEM-SEND,                        
046900            IDSYSTEM_REC      = :WS-IDSYSTEM-REC,                         
047000            BEANST            = :WS-BEANST,                               
047100            IDUSER            = :WS-IDUSER,                               
047200            BETEXT            = :WS-BETEXT,                               
047300            BETEXT_CRE        = :WS-BETEXT-CRE                            
047400                                                                          
047500     WHERE  T01DHEA.IDLEGSEL      = :WS-IDLEGSEL                          
047600     AND    T01DHEA.DAEXDAT       = :WS-DAEXDAT                           
047700     AND    T01DHEA.TIEXTID       = :WS-TIEXTID                           
047800     AND    T01DHEA.KDVALISO      = :WS-KDVALISO                          
047900     AND    T01DHEA.IDLANDX3_SEND = :WS-IDLANDX3-SEND                     
048000     AND    T01DHEA.IDLEVNR       = :WS-IDLEVNR                           
048100     AND    T01DHEA.IDPARTNR      = :WS-IDPARTNR                          
048200     AND    T01DHEA.KDFINDOC      = :WS-KDFINDOC                          
048300     AND    T01DHEA.FLSOFT        = :WS-FLSOFT                            
048400     AND    T01DHEA.FLFREE        = :WS-FLFREE                            
048500     AND    T01DHEA.FLPRIV        = :WS-FLPRIV                            
048600     AND    T01DHEA.IDBREAK_1     = :WS-IDBREAK-1                         
048700     AND    T01DHEA.IDBREAK_2     = :WS-IDBREAK-2                         
048800     END-EXEC                                                             
048900                                                                          
049000     MOVE 000            TO GOOD-SQLCODES                                 
049100     MOVE SQLCODE        TO SQLCODE-WS                                    
049200     PERFORM DB2-STATUS-CHECK                                             
049300     .                                                                    
049400     EJECT                                                                
049500                                                                          
052300 DB2-CLOSE-CRS-SLIN SECTION.                                              
052400     EXEC SQL                                                             
052500         CLOSE SLIN-CRS                                                   
052600     END-EXEC                                                             
052700     .                                                                    
052800     EJECT                                                                
052900                                                                          
053040 DB2-STATUS-CHECK SECTION.                                                
053100     SET SQLCODE-IX         TO 1                                          
053200     SEARCH GOOD-SQLCODE AT END                                           
053300           CALL ABEND USING RKOD-ABEND-DB2                                
053400        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
053500           CONTINUE                                                       
053600     END-SEARCH                                                           
053700     .                                                                    
