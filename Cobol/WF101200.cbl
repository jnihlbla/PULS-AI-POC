000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000300*                                                                         
000400 ID DIVISION.                                                             
000500 PROGRAM-ID.     WF101200.                                                
000600 AUTHOR.         BO HAMMARIN.                                             
000700 DATE-WRITTEN.   DEC 2001.                                                
000800 DATE-COMPILED.                                                           
000900                                                                          
001000*   PGM                                                                   
001100*   -SELECTS ROWS FROM                                                    
001200*    1 FINANCIAL CUSTOMER-TABLE                                           
001300*    2 APPROVED PAYMENT-TABLE                                             
001400*   -CREATES FILE CONTAINING                                              
001500*    1 NEW FINANCIAL CUSTOMERS                                            
001600*    2 CHANGED FINANCIAL CUSTOMERS                                        
001700*                                                                         
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000                                                                          
002100 INPUT-OUTPUT SECTION.                                                    
002200 FILE-CONTROL.                                                            
002300*          --- DISTRIBUTION-DATA                                          
002400     SELECT WF1018                     ASSIGN TO WF1012D1.                
002500                                                                          
002600 DATA DIVISION.                                                           
002700                                                                          
002800 FILE SECTION.                                                            
002900 FD  WF1018                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200                                                                          
003300*01  POST -COPY WF10CUS2  -PRE UT-   -L.                                  
003400     EJECT                                                                
003500                                                                          
003600 WORKING-STORAGE SECTION.                                                 
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(8)    VALUE 'WF101200'.            
003900                                                                          
004000*    ---KEYS FOR SELECTION OF ROWS IN TABLE T01FCUS                       
004100 01  WS-IDLEGSEL                 PIC X(4).                                
004200 01  WS-IDPARTNR                 PIC X(10).                               
004300 01  WS-KDSTATUS                 PIC S9(3)   COMP-3.                      
004400*    ---KEYS FOR SELECTION OF ROWS IN TABLE T01PATE                       
004500 01  WS-IDLEGSEL                 PIC X(4).                                
004600 01  WS-IDSPRAK                  PIC X(2).                                
004700 01  WS-KDBETALV                 PIC X(4).                                
004800     EJECT                                                                
004900                                                                          
005000 01  DYNAMISKA-SUBPROGRAM.                                                
005100*                                                                         
005200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
005300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005400     EJECT                                                                
005500                                                                          
005600 01  WS-DATUM.                                                            
005700     03  WS-DAGENS-DATUM         PIC X(8).                                
005800     EJECT                                                                
005900                                                                          
006000*    --- PARAMETRAR TILL ABEND                                            
006100*                                                                         
006200 01  RKOD-ABEND-DB2              PIC S9(4)   VALUE +998 COMP SYNC.        
006300     EJECT                                                                
006400                                                                          
006500*    --- PARAMETRAR TILL DATKORT                                          
006600*                                                                         
006700 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
006800                                                                          
006900*01  -COPY WDATKORT                                                       
007000     EJECT                                                                
007100                                                                          
007200 01  WF1018-AREA-START           PIC X(24)   VALUE                        
007300                                             'WF10-AREA-START'.           
007400*01  -COPY WF10CUS2       -PRE UT-                                        
007500     EJECT                                                                
007600*                                                                         
007700*        WORK-AREAS FOR DB2-SECTIONS                                      
007800*                                                                         
007900 01  FILLER                       PIC X(16)   VALUE 'DB2-FCUS   '.        
008000*01  -COPY T01FCUS        -PRE CUST-                                      
008100     EJECT                                                                
008200                                                                          
008300 01  FILLER                       PIC X(16)   VALUE 'DB2-PATE   '.        
008400*01  -COPY T01PATE        -PRE PATE-                                      
008500     EJECT                                                                
008600                                                                          
008700 01  FILLER                       PIC X(16)   VALUE 'FCUS-AREA'.          
008800       EXEC SQL INCLUDE T01FCUS  END-EXEC.                                
008900                                                                          
009000     EJECT                                                                
009100 01  FILLER                       PIC X(16)   VALUE 'PATE-AREA'.          
009200       EXEC SQL INCLUDE T01PATE  END-EXEC.                                
009300                                                                          
009400     EJECT                                                                
009500                                                                          
009600 01  FILLER                       PIC X(16)   VALUE 'SQLCA-AREA'.         
009700       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
009800*                        **** STATUS-CODE FROM DB2                        
009900                                                                          
010000 01  FILLER                       PIC X(16)   VALUE 'SQLCODE-WS'.         
010100 01  DB2-WS.                                                              
010200   03  SQLCODE-WS                 PIC S9(3)   VALUE ZERO.                 
010300     88  ROW-FOUND                            VALUE +000.                 
010400     88  ROW-MISSING                          VALUE +100.                 
010500   03  GOOD-SQLCODES.                                                     
010600     05  GOOD-SQLCODE OCCURS 5                                            
010700         INDEXED BY SQLCODE-IX    PIC 999.                                
010800     EJECT                                                                
010900                                                                          
011000 PROCEDURE DIVISION.                                                      
011100                                                                          
011200 MAIN SECTION.                                                            
011300     ENTRY 'DLITCBL'.                                                     
011400                                                                          
011500     PERFORM A-INIT                                                       
011600                                                                          
011700     PERFORM B-EXECUTE                                                    
011800                                                                          
011900     PERFORM Z-FINISH                                                     
012000                                                                          
012100     MOVE ZERO TO RETURN-CODE                                             
012200     GOBACK                                                               
012300     .                                                                    
012400     EJECT                                                                
012500                                                                          
012600 A-INIT SECTION.                                                          
012700     OPEN OUTPUT WF1018                                                   
012800                                                                          
012900     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
013000     MOVE 20           TO WS-DAGENS-DATUM(1:2)                            
013100     MOVE D-AAR        TO WS-DAGENS-DATUM(3:2)                            
013200     MOVE D-MAANAD     TO WS-DAGENS-DATUM(5:2)                            
013300     MOVE D-DAG        TO WS-DAGENS-DATUM(7:2)                            
013400     .                                                                    
013500     EJECT                                                                
013600                                                                          
013700 B-EXECUTE SECTION.                                                       
013800     PERFORM DB2-OPEN-CRS-CUST                                            
013900     PERFORM DB2-FETCH-CRS-CUST                                           
014000     PERFORM UNTIL ROW-MISSING                                            
014100       PERFORM BA-BUILD-OUTPUT-CUST                                       
014200       PERFORM S11-WRITE-WF1018                                           
014300       PERFORM DB2-FETCH-CRS-CUST                                         
014400     END-PERFORM                                                          
014500     PERFORM DB2-CLOSE-CRS-CUST                                           
014600     .                                                                    
014700     EJECT                                                                
014800                                                                          
014900 BA-BUILD-OUTPUT-CUST SECTION.                                            
015000     MOVE CUST-IDLEGSEL             TO UT-IDLEGSEL                        
015100     MOVE CUST-IDPARTNR             TO UT-IDPARTNR                        
015200     MOVE CUST-IDALPHA              TO UT-IDALPHA                         
015300     MOVE CUST-BEBET-NAME1          TO UT-BEBET-NAME1                     
015400     MOVE CUST-BEBET-NAME2          TO UT-BEBET-NAME2                     
015500     MOVE CUST-BEBET-NAME3          TO UT-BEBET-NAME3                     
015600     MOVE CUST-BEBET-NAME4          TO UT-BEBET-NAME4                     
015700     MOVE CUST-ADBET-STREET         TO UT-ADBET-STREET                    
015800     MOVE CUST-ADBET-BOX            TO UT-ADBET-BOX                       
015900     MOVE CUST-ADBET-CITY           TO UT-ADBET-CITY                      
016000     MOVE CUST-ADBET-PCODE          TO UT-ADBET-PCODE                     
016100     MOVE CUST-IDLANDX3             TO UT-IDLANDX3                        
016200     MOVE CUST-IDSPRAK              TO UT-IDSPRAK                         
016300     MOVE CUST-IDTFN                TO UT-IDTFN                           
016400     MOVE CUST-IDTFX                TO UT-IDTFX                           
016500     MOVE CUST-IDMAIL               TO UT-IDMAIL                          
016600     MOVE CUST-IDVAT                TO UT-IDVAT                           
016700     MOVE CUST-KDVALISO             TO UT-KDVALISO                        
016800     MOVE CUST-IDLEVNR-AP           TO UT-IDLEVNR-AP                      
016900     MOVE CUST-KDTRADP              TO UT-KDTRADP                         
017000     MOVE CUST-KDBETALV             TO UT-KDBETALV                        
017100     MOVE CUST-KDKREDSP             TO UT-KDKREDSP                        
017200     MOVE CUST-KDPARTTY             TO UT-KDPARTTY                        
017300     MOVE CUST-KDPARTGR             TO UT-KDPARTGR                        
017400     MOVE CUST-DAREGDAT             TO UT-DAREGDAT                        
017500     MOVE CUST-DAUPPDAT             TO UT-DAUPPDAT                        
017600     MOVE CUST-DADELDAT             TO UT-DADELDAT                        
017700     MOVE CUST-IDUSER               TO UT-IDUSER                          
017800     MOVE CUST-FLRATE               TO UT-FLRATE                          
017900     MOVE CUST-FLLOCCUR             TO UT-FLLOCCUR                        
018000     MOVE CUST-KDVALTYP             TO UT-KDVALTYP                        
018100     MOVE PATE-BEBETVIL             TO UT-BEBETVIL                        
018200     .                                                                    
018300     EJECT                                                                
018400                                                                          
018500 Z-FINISH SECTION.                                                        
018600     CLOSE WF1018                                                         
018700     .                                                                    
018800     EJECT                                                                
018900                                                                          
019000 S11-WRITE-WF1018 SECTION.                                                
019100     WRITE UT-POST   FROM UT-WF10CUST                                     
019200     .                                                                    
019300     EJECT                                                                
019400                                                                          
019500* --- DB2 SECTIONS  ---                                                   
019600*                                                                         
019700 DB2-OPEN-CRS-CUST SECTION.                                               
019800                                                                          
019900     EXEC SQL DECLARE CUST-CRS CURSOR FOR                                 
020000     SELECT   T01FCUS.IDLEGSEL,                                           
020100              T01FCUS.IDPARTNR,                                           
020200              T01FCUS.IDALPHA,                                            
020300              T01FCUS.BEBET_NAME1,                                        
020400              T01FCUS.BEBET_NAME2,                                        
020500              T01FCUS.BEBET_NAME3,                                        
020600              T01FCUS.BEBET_NAME4,                                        
020700              T01FCUS.ADBET_STREET,                                       
020800              T01FCUS.ADBET_BOX,                                          
020900              T01FCUS.ADBET_CITY,                                         
021000              T01FCUS.ADBET_PCODE,                                        
021100              T01FCUS.IDLANDX3,                                           
021200              T01FCUS.IDSPRAK,                                            
021300              T01FCUS.IDTFN,                                              
021400              T01FCUS.IDTFX,                                              
021500              T01FCUS.IDMAIL,                                             
021600              T01FCUS.IDLEVNR_AP,                                         
021700              T01FCUS.IDVAT,                                              
021800              T01FCUS.KDVALISO,                                           
021900              T01FCUS.KDTRADP,                                            
022000              T01FCUS.KDBETALV,                                           
022100              T01FCUS.KDKREDSP,                                           
022200              T01FCUS.KDPARTTY,                                           
022300              T01FCUS.KDPARTGR,                                           
022400              T01FCUS.DAREGDAT,                                           
022500              T01FCUS.DAUPPDAT,                                           
022600              T01FCUS.DADELDAT,                                           
022700              T01FCUS.IDUSER,                                             
022800              T01PATE.BEBETVIL,                                           
022900              T01FCUS.FLRATE,                                             
023000              T01FCUS.FLLOCCUR,                                           
023100              T01FCUS.KDVALTYP                                            
023200                                                                          
023300     FROM     T01FCUS,                                                    
023400              T01PATE                                                     
023500                                                                          
023600     WHERE    T01FCUS.IDLEGSEL = T01PATE.IDLEGSEL    AND                  
023700              T01FCUS.IDSPRAK  = T01PATE.IDSPRAK     AND                  
023800              T01FCUS.KDBETALV = T01PATE.KDBETALV    AND                  
023900             (T01FCUS.DAREGDAT = :WS-DAGENS-DATUM OR                      
024000              T01FCUS.DAUPPDAT = :WS-DAGENS-DATUM OR                      
024100              T01FCUS.DADELDAT = :WS-DAGENS-DATUM OR                      
024200              T01PATE.DAUPPDAT = :WS-DAGENS-DATUM)   AND                  
024300              T01FCUS.KDSTATUS = 1                                        
024400                                                                          
024500     ORDER BY T01FCUS.IDLEGSEL,                                           
024600              T01FCUS.IDPARTNR                                            
024700                                                                          
024800     FOR FETCH ONLY                                                       
024900     END-EXEC                                                             
025000                                                                          
025100     MOVE 000            TO GOOD-SQLCODES                                 
025200     EXEC SQL OPEN CUST-CRS                                               
025300     END-EXEC                                                             
025400     MOVE SQLCODE        TO SQLCODE-WS                                    
025500     PERFORM DB2-STATUS-CHECK                                             
025600     .                                                                    
025700     EJECT                                                                
025800                                                                          
025900 DB2-FETCH-CRS-CUST SECTION.                                              
026000                                                                          
026100     EXEC SQL FETCH CUST-CRS INTO                                         
026200            :CUST-IDLEGSEL,                                               
026300            :CUST-IDPARTNR,                                               
026400            :CUST-IDALPHA,                                                
026500            :CUST-BEBET-NAME1,                                            
026600            :CUST-BEBET-NAME2,                                            
026700            :CUST-BEBET-NAME3,                                            
026800            :CUST-BEBET-NAME4,                                            
026900            :CUST-ADBET-STREET,                                           
027000            :CUST-ADBET-BOX,                                              
027100            :CUST-ADBET-CITY,                                             
027200            :CUST-ADBET-PCODE,                                            
027300            :CUST-IDLANDX3,                                               
027400            :CUST-IDSPRAK,                                                
027500            :CUST-IDTFN,                                                  
027600            :CUST-IDTFX,                                                  
027700            :CUST-IDMAIL,                                                 
027800            :CUST-IDLEVNR-AP,                                             
027900            :CUST-IDVAT,                                                  
028000            :CUST-KDVALISO,                                               
028100            :CUST-KDTRADP,                                                
028200            :CUST-KDBETALV,                                               
028300            :CUST-KDKREDSP,                                               
028400            :CUST-KDPARTTY,                                               
028500            :CUST-KDPARTGR,                                               
028600            :CUST-DAREGDAT,                                               
028700            :CUST-DAUPPDAT,                                               
028800            :CUST-DADELDAT,                                               
028900            :CUST-IDUSER,                                                 
029000            :PATE-BEBETVIL,                                               
029100            :CUST-FLRATE,                                                 
029200            :CUST-FLLOCCUR,                                               
029300            :CUST-KDVALTYP                                                
029400     END-EXEC                                                             
029500                                                                          
029600     MOVE 000100         TO GOOD-SQLCODES                                 
029700     MOVE SQLCODE        TO SQLCODE-WS                                    
029800     PERFORM DB2-STATUS-CHECK                                             
029900     .                                                                    
030000     EJECT                                                                
030100                                                                          
030200 DB2-CLOSE-CRS-CUST SECTION.                                              
030300     EXEC SQL CLOSE CUST-CRS                                              
030400     END-EXEC                                                             
030500     .                                                                    
030600     EJECT                                                                
030700                                                                          
030800 DB2-STATUS-CHECK SECTION.                                                
030900     SET SQLCODE-IX         TO 1                                          
031000     SEARCH GOOD-SQLCODE AT END                                           
031100           CALL ABEND USING RKOD-ABEND-DB2                                
031200        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
031300           CONTINUE                                                       
031400     END-SEARCH                                                           
031500     .                                                                    
