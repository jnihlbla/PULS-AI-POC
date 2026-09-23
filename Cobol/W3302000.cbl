000100 ID  DIVISION.                                                            
000200                                                                          
000300 PROGRAM-ID.    W3302000.                                                 
000400 AUTHOR.        KARL JOHAN HANSSON.                                       
000500     DATE-WRITTEN.  OKTOBER 1989.                                         
000600     REMARKS.                                                             
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAM ÅT ARTIKELSTATISTIKEN.                                   
001000*        BERÄKNAR BRUTTOVINSTEN PER AF1 - AF5 SAMT SKAPAR TRANSAR         
001100*        PÅ TRE SUMMANIVÅER FÖR ATT LADDA DB2-BAS MED.                    
001200*                                                                         
001300*        DESSA TRE SUMMERINGSNIVÅER ÄR:                                   
001400*        - ARTIKEL TOTALT   (FSG2)                                        
001500*              POSTER SKRIVS PÅ UTFIL W33021.                             
001600*        - ARTIKEL/KONCERN  (FSG3)                                        
001700*              POSTER SKRIVS PÅ UTFIL W33023.                             
001800*        - ARTIKEL/DISTRIKT (FSG4)                                        
001900*              POSTER SKRIVS PÅ UTFIL W33025.                             
002000*                                                                         
002100*        PROGRAMMET LÄSER AF-FILENS DISTRIKTSPOST OCH                     
002200*        BERÄKAR BRUTTOVINSTEN SAMT SKRIVER EN TRANS PÅ FSG4-NIVÅ.        
002300*        DÄRPÅ ADDERAS FSG4-TRANSEN TILL FSG3- OCH FSG2-NIVÅER,           
002400*        OCH POSTER SKRIVS VID KONCERN- RESP ARTIKEL-BRYTNINGAR.          
002500     EJECT                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200*    --- INFIL:                                                           
003300*           --- SEKUNDÄWREGISTER:                                         
003400     SELECT W33015                       ASSIGN TO W33020D1.              
003500                                                                          
003600*    --- UTFILER:                                                         
003700*           --- FÖR LADNING AV DB2-BAS:                                   
003800     SELECT W33021                       ASSIGN TO W33020D2.              
003900     SELECT W33023                       ASSIGN TO W33020D3.              
004000     SELECT W33025                       ASSIGN TO W33020D4.              
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP2                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W33015                                                               
004700     LABEL RECORD   STANDARD                                              
004800     RECORDING      F                                                     
004900     BLOCK CONTAINS 0.                                                    
005000                                                                          
005100*    -COPY W33014   -L.                                                   
005300     EJECT                                                                
005400 FD  W33021                                                               
005500     LABEL RECORD   STANDARD                                              
005600     RECORDING      F                                                     
005700     BLOCK CONTAINS 0.                                                    
005800                                                                          
005900*01  W33021-POST -COPY FSG2   -L.                                         
006100     SKIP3                                                                
006200 FD  W33023                                                               
006300     LABEL RECORD   STANDARD                                              
006400     RECORDING      F                                                     
006500     BLOCK CONTAINS 0.                                                    
006600                                                                          
006700*01  W33023-POST -COPY FSG3   -L.                                         
006900     SKIP3                                                                
007000 FD  W33025                                                               
007100     LABEL RECORD   STANDARD                                              
007200     RECORDING      F                                                     
007300     BLOCK CONTAINS 0.                                                    
007400                                                                          
007500*01  W33025-POST -COPY FSG4   -L.                                         
007700     EJECT                                                                
007800 WORKING-STORAGE SECTION.                                                 
007900     SKIP2                                                                
007901                                                                          
007910*    -- CHECKED BY WY2000                                                 
008000 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W3302000'.            
008100 77  JA                          PIC X       VALUE 'J'.                   
008200 77  NEJ                         PIC X       VALUE 'N'.                   
008300                                                                          
008400 77  INFIL-POST-SW               PIC X(1)    VALUE 'J'.                   
008500     88  INFIL-POST-SAKNAS                   VALUE 'N'.                   
008600                                                                          
008700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008800                                                                          
008900 01  DYNAMISKA-SUBPROGRAM.                                                
009000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009200     SKIP3                                                                
009300*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
009400*                                                                         
009500*01  -COPY W0005 -PRE  POSTSUM-                                           
009700                                                                          
009800 01  W33015-TRANSID.                                                      
009900     03  FILLER                 PIC X(6)  VALUE 'W33015'.                 
010000     03  FILLER                 PIC X(8)  VALUE 'W33020D1'.               
010100     03  FILLER                 PIC X(4)  VALUE ' IN '.                   
010200 01  W33021-TRANSID.                                                      
010300     03  FILLER                 PIC X(6)  VALUE 'W33021'.                 
010400     03  FILLER                 PIC X(8)  VALUE 'W33020D2'.               
010500     03  FILLER                 PIC X(4)  VALUE 'FSG2'.                   
010600 01  W33023-TRANSID.                                                      
010700     03  FILLER                 PIC X(6)  VALUE 'W33023'.                 
010800     03  FILLER                 PIC X(8)  VALUE 'W33020D3'.               
010900     03  FILLER                 PIC X(4)  VALUE 'FSG3'.                   
011000 01  W33025-TRANSID.                                                      
011100     03  FILLER                 PIC X(6)  VALUE 'W33025'.                 
011200     03  FILLER                 PIC X(8)  VALUE 'W33020D4'.               
011300     03  FILLER                 PIC X(4)  VALUE 'FSG4'.                   
011400     EJECT                                                                
011500 01  FILLER                      PIC X(16)  VALUE 'IN-AREA-START'.        
011600*01  AREA    -PRE IN-  -COPY W33014                                       
011800     EJECT                                                                
011900*01  AREA    -PRE FSG2-    -COPY FSG2                                     
012100     EJECT                                                                
012200*01  AREA    -PRE FSG3-    -COPY FSG3                                     
012400     EJECT                                                                
012500*01  AREA    -PRE FSG4-    -COPY FSG4                                     
012700     EJECT                                                                
012800 PROCEDURE DIVISION.                                                      
012900                                                                          
013000     PERFORM A-INIT                                                       
013100                                                                          
013200     PERFORM S01-LAS-INFIL                                                
013300     PERFORM UNTIL INFIL-POST-SAKNAS                                      
013400         INITIALIZE FSG2-AREA                                             
013500         MOVE IN-IDARTNR    TO FSG2-IDARTNR                               
013600         PERFORM UNTIL INFIL-POST-SAKNAS OR                               
013700                   IN-IDARTNR NOT = FSG2-IDARTNR                          
013800                                                                          
013900             INITIALIZE FSG3-AREA                                         
014000             MOVE IN-IDKONCNR   TO FSG3-IDKONCNR                          
014100             MOVE IN-IDARTNR    TO FSG3-IDARTNR                           
014200                                                                          
014300             PERFORM UNTIL INFIL-POST-SAKNAS OR                           
014400                       IN-IDARTNR NOT = FSG3-IDARTNR OR                   
014500                       IN-IDKONCNR NOT = FSG3-IDKONCNR                    
014600                                                                          
014700                 PERFORM C-SKAPA-SKRIV-DISTRIKTPOST                       
014800                 PERFORM D-ADDERA-FSG2-FSG3-POSTER                        
014900                 PERFORM S01-LAS-INFIL                                    
015000                                                                          
015100             END-PERFORM                                                  
015200             PERFORM S03-SKRIV-FSG3-KONCERNPOST                           
015300                                                                          
015400         END-PERFORM                                                      
015500         PERFORM S02-SKRIV-FSG2-ARTIKELPOST                               
015600     END-PERFORM                                                          
015700                                                                          
015800     PERFORM Z-FINIT                                                      
015900     MOVE ZERO TO RETURN-CODE                                             
016000     GOBACK                                                               
016100     .                                                                    
016200     EJECT                                                                
016300 A-INIT SECTION.                                                          
016400                                                                          
016500     OPEN INPUT  W33015                                                   
016600          OUTPUT W33021                                                   
016700                 W33023                                                   
016800                 W33025                                                   
016900                                                                          
017000     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
017100     .                                                                    
017200     EJECT                                                                
017300 C-SKAPA-SKRIV-DISTRIKTPOST SECTION.                                      
017400                                                                          
017500     MOVE IN-IDARTNR             TO FSG4-IDARTNR                          
017600     MOVE IN-IDDISTR             TO FSG4-IDDISTR                          
017700     MOVE IN-SUARTFSG-PER        TO FSG4-SUARTFSG-PER                     
017800     MOVE IN-SUARTFSG-AAR        TO FSG4-SUARTFSG-AAR                     
017900     MOVE IN-SUARTFSG-RAAR-SPEC  TO FSG4-SUARTFSG-RAAR-SPEC               
018000     MOVE IN-SUARTFSG-RAAR-RAB   TO FSG4-SUARTFSG-RAAR-RAB                
018100     MOVE IN-SUARTFSG-RAAR-MAN   TO FSG4-SUARTFSG-RAAR-MAN                
018200     MOVE IN-SUARTFSG-RAAR-KRE   TO FSG4-SUARTFSG-RAAR-KRE                
018300     MOVE IN-SUARTFSG-FAAR       TO FSG4-SUARTFSG-FAAR                    
018400     MOVE IN-SUARTFSG-RAAR       TO FSG4-SUARTFSG-RAAR                    
018500     MOVE IN-SUARTFSG-FRAAR      TO FSG4-SUARTFSG-FRAAR                   
018600     MOVE IN-SULEVANT-PER        TO FSG4-SULEVANT-PER                     
018700     MOVE IN-SULEVANT-AAR        TO FSG4-SULEVANT-AAR                     
018800     MOVE IN-SULEVANT-RAAR-SPEC  TO FSG4-SULEVANT-RAAR-SPEC               
018900     MOVE IN-SULEVANT-RAAR-RAB   TO FSG4-SULEVANT-RAAR-RAB                
019000     MOVE IN-SULEVANT-RAAR-MAN   TO FSG4-SULEVANT-RAAR-MAN                
019100     MOVE IN-SULEVANT-RAAR-KRE   TO FSG4-SULEVANT-RAAR-KRE                
019200     MOVE IN-SULEVANT-FAAR       TO FSG4-SULEVANT-FAAR                    
019300     MOVE IN-SULEVANT-RAAR       TO FSG4-SULEVANT-RAAR                    
019400     MOVE IN-SULEVANT-FRAAR      TO FSG4-SULEVANT-FRAAR                   
019500     SUBTRACT IN-SUARTSJK-PER FROM IN-SUARTFSG-PER                        
019600                             GIVING FSG4-SUTOTBV-PER                      
019700     SUBTRACT IN-SUARTSJK-AAR FROM IN-SUARTFSG-AAR                        
019800                             GIVING FSG4-SUTOTBV-AAR                      
019900     SUBTRACT IN-SUARTSJK-FAAR FROM IN-SUARTFSG-FAAR                      
020000                             GIVING FSG4-SUTOTBV-FAAR                     
020100     SUBTRACT IN-SUARTSJK-RAAR FROM IN-SUARTFSG-RAAR                      
020200                             GIVING FSG4-SUTOTBV-RAAR                     
020300     SUBTRACT IN-SUARTSJK-FRAAR FROM IN-SUARTFSG-FRAAR                    
020400                             GIVING FSG4-SUTOTBV-FRAAR                    
020500                                                                          
020600     PERFORM S04-SKRIV-FSG4-DISTRIKTPOST                                  
020700     .                                                                    
020800     EJECT                                                                
020900 D-ADDERA-FSG2-FSG3-POSTER SECTION.                                       
021000                                                                          
021100     ADD FSG4-SUARTFSG-PER       TO FSG2-SUARTFSG-PER                     
021200                                    FSG3-SUARTFSG-PER                     
021300     ADD FSG4-SUARTFSG-AAR       TO FSG2-SUARTFSG-AAR                     
021400                                    FSG3-SUARTFSG-AAR                     
021500     ADD FSG4-SUARTFSG-RAAR-SPEC TO FSG2-SUARTFSG-RAAR-SPEC               
021600                                    FSG3-SUARTFSG-RAAR-SPEC               
021700     ADD FSG4-SUARTFSG-RAAR-RAB  TO FSG2-SUARTFSG-RAAR-RAB                
021800                                    FSG3-SUARTFSG-RAAR-RAB                
021900     ADD FSG4-SUARTFSG-RAAR-MAN  TO FSG2-SUARTFSG-RAAR-MAN                
022000                                    FSG3-SUARTFSG-RAAR-MAN                
022100     ADD FSG4-SUARTFSG-RAAR-KRE  TO FSG2-SUARTFSG-RAAR-KRE                
022200                                    FSG3-SUARTFSG-RAAR-KRE                
022300     ADD FSG4-SUARTFSG-FAAR      TO FSG2-SUARTFSG-FAAR                    
022400                                    FSG3-SUARTFSG-FAAR                    
022500     ADD FSG4-SUARTFSG-RAAR      TO FSG2-SUARTFSG-RAAR                    
022600                                    FSG3-SUARTFSG-RAAR                    
022700     ADD FSG4-SUARTFSG-FRAAR     TO FSG2-SUARTFSG-FRAAR                   
022800                                    FSG3-SUARTFSG-FRAAR                   
022900     ADD FSG4-SULEVANT-PER       TO FSG2-SULEVANT-PER                     
023000                                    FSG3-SULEVANT-PER                     
023100     ADD FSG4-SULEVANT-AAR       TO FSG2-SULEVANT-AAR                     
023200                                    FSG3-SULEVANT-AAR                     
023300     ADD FSG4-SULEVANT-RAAR-SPEC TO FSG2-SULEVANT-RAAR-SPEC               
023400                                    FSG3-SULEVANT-RAAR-SPEC               
023500     ADD FSG4-SULEVANT-RAAR-RAB  TO FSG2-SULEVANT-RAAR-RAB                
023600                                    FSG3-SULEVANT-RAAR-RAB                
023700     ADD FSG4-SULEVANT-RAAR-MAN  TO FSG2-SULEVANT-RAAR-MAN                
023800                                    FSG3-SULEVANT-RAAR-MAN                
023900     ADD FSG4-SULEVANT-RAAR-KRE  TO FSG2-SULEVANT-RAAR-KRE                
024000                                    FSG3-SULEVANT-RAAR-KRE                
024100     ADD FSG4-SULEVANT-FAAR      TO FSG2-SULEVANT-FAAR                    
024200                                    FSG3-SULEVANT-FAAR                    
024300     ADD FSG4-SULEVANT-RAAR      TO FSG2-SULEVANT-RAAR                    
024400                                    FSG3-SULEVANT-RAAR                    
024500     ADD FSG4-SULEVANT-FRAAR     TO FSG2-SULEVANT-FRAAR                   
024600                                    FSG3-SULEVANT-FRAAR                   
024700     ADD FSG4-SUTOTBV-PER        TO FSG2-SUTOTBV-PER                      
024800                                    FSG3-SUTOTBV-PER                      
024900     ADD FSG4-SUTOTBV-AAR        TO FSG2-SUTOTBV-AAR                      
025000                                    FSG3-SUTOTBV-AAR                      
025100     ADD FSG4-SUTOTBV-FAAR       TO FSG2-SUTOTBV-FAAR                     
025200                                    FSG3-SUTOTBV-FAAR                     
025300     ADD FSG4-SUTOTBV-RAAR       TO FSG2-SUTOTBV-RAAR                     
025400                                    FSG3-SUTOTBV-RAAR                     
025500     ADD FSG4-SUTOTBV-FRAAR      TO FSG2-SUTOTBV-FRAAR                    
025600                                    FSG3-SUTOTBV-FRAAR                    
025700     .                                                                    
025800                                                                          
025900     EJECT                                                                
026000 Z-FINIT SECTION.                                                         
026100                                                                          
026200     CLOSE W33015                                                         
026300           W33021                                                         
026400           W33023                                                         
026500           W33025                                                         
026600                                                                          
026700     MOVE 'S'      TO POSTSUM-OPKOD                                       
026800     CALL POSTSUM USING POSTSUM-PARM                                      
026900     .                                                                    
027000     SKIP3                                                                
027100 S01-LAS-INFIL        SECTION.                                            
027200                                                                          
027300     READ W33015       INTO IN-AREA                                       
027400       AT END                                                             
027500          MOVE NEJ TO INFIL-POST-SW                                       
027600     END-READ                                                             
027700                                                                          
027800     IF NOT INFIL-POST-SAKNAS                                             
027900         MOVE W33015-TRANSID TO POSTSUM-TRANSID                           
028000         CALL POSTSUM USING POSTSUM-PARM                                  
028100     END-IF                                                               
028200     .                                                                    
028300     EJECT                                                                
028400 S02-SKRIV-FSG2-ARTIKELPOST SECTION.                                      
028500                                                                          
028600     WRITE W33021-POST FROM FSG2-AREA                                     
028700                                                                          
028800     MOVE W33021-TRANSID TO POSTSUM-TRANSID                               
028900     CALL POSTSUM USING POSTSUM-PARM                                      
029000     .                                                                    
029100     SKIP3                                                                
029200 S03-SKRIV-FSG3-KONCERNPOST SECTION.                                      
029300                                                                          
029400     WRITE W33023-POST FROM FSG3-AREA                                     
029500                                                                          
029600     MOVE W33023-TRANSID TO POSTSUM-TRANSID                               
029700     CALL POSTSUM USING POSTSUM-PARM                                      
029800     .                                                                    
029900     SKIP3                                                                
030000 S04-SKRIV-FSG4-DISTRIKTPOST SECTION.                                     
030100                                                                          
030200     WRITE W33025-POST FROM FSG4-AREA                                     
030300                                                                          
030400     MOVE W33025-TRANSID TO POSTSUM-TRANSID                               
030500     CALL POSTSUM USING POSTSUM-PARM                                      
030600     .                                                                    
