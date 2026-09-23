000010 PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3015800.                                                
000300 AUTHOR.         GÖRAN KJELLSON.                                          
000400 DATE-WRITTEN.   12/11/01.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAMN:       CARPARTS.NDC.CORECROSSINDEX                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        CROSSREFERNS VOLVOARTIKEL / BYTESARTIKEL                         
001100*                                                                         
001200*        PROGRAMMET LÄSER      TABELL BYPRO                               
001300*        PROGRAMMET LÄSER      WDD3                                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W30158T                                             
001700*        REQUEST:     W30158I1                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        RESPONSE:    W30158O1                                            
002100                                                                          
002200                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700 DATA DIVISION.                                                           
002800                                                                          
002900 FILE SECTION.                                                            
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200 77  IDPGM                       PIC X(08)   VALUE 'W3015800'.            
003300 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
003400 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
003500 77  CURRENT-DB2-SECTION         PIC X(16)   VALUE SPACE.                 
003600                                                                          
003700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003900 77  KDRC-DISPLAY                PIC Z(5).                                
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004400 77  RAD-INDX                    PIC 9(4)    VALUE ZERO.                  
004500 77  RAD-INDX-MAX                PIC 9(4)    VALUE 500.                   
004600                                                                          
004700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004800     88  NYCKLAR-OK                          VALUE 'J'.                   
004900     88  NYCKLAR-FEL                         VALUE 'N'.                   
005000                                                                          
005100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005200 01  GENERELLA-SUBPROGRAM.                                                
005300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005600     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005700     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
005800                                                                          
005900                                                                          
006000 01  FILLER                      PIC X(16)  VALUE 'WTRAUTF8-AREA'.        
006100*01  -COPY WTRAUTF8                                                       
006200                                                                          
006300*    --- PARAMETRAR TILL ABEND                                            
006400                                                                          
006500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006800 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
006900                                                                          
007000 01  MESSAGE-CODES.                                                       
007100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
007200     03  ERR-ATKOMST-FEL         PIC X(3)    VALUE '025'.                 
007210     03  ERR-RADER-SAKNAS        PIC X(3)    VALUE '025'.                 
007300                                                                          
007400                                                                          
007500                                                                          
007600*                                                                         
007700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
007800*01  -COPY WZ01SUB                                                        
007900                                                                          
008000 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
008100 01  REQU-AREA.                                                           
008200*    03  -COPY WZ01REQU                                                   
008300*    03  -COPY W30158I1                                                   
008400                                                                          
008500                                                                          
008600                                                                          
008700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
008800 01  RESP-AREA.                                                           
008900*    03  -COPY WZ01RESP                                                   
009000*    03  -COPY W30158O1                                                   
009100                                                                          
009200                                                                          
009300                                                                          
009400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009500*                                                                         
009600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009700                                                                          
009800 01  NYCKLAR-TILL-DLI-DB2.                                                
009900     03 W-IDARTNR-BYPRO          PIC S9(9) COMP-3 VALUE ZERO.             
010000     03 W-IDARTNR-D311-X.                                                 
010100        05 W-IDARTNR-D311        PIC S9(9) COMP-3 VALUE ZERO.             
010200     03 W-IDSKYLT-X.                                                      
010300         05 W-IDSKYLT            PIC X(3)         VALUE SPACE.            
           03  W-IDDC-B6-X.                                                     
               05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
010400                                                                          
010500                                                                          
010600*    --- STATUS-KOD FRÅN IMS                                              
010700 01  STATUS-WS                   PIC XX.                                  
010800     88  SEGMENT-FINNS                       VALUE '  '.                  
010900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011000                                                                          
011100 01  GODK-STATUSKODER.                                                    
011200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011300     SKIP3                                                                
011400 01  SSA1                        PIC X(64).                               
011500 01  SSA2                        PIC X(64).                               
011600                                                                          
011700                                                                          
011800*    --- IMS FUNKTIONSKODER                                               
011900*01  -COPY W0003                                                          
012000                                                                          
012100                                                                          
012200 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
012300       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
012400                                                                          
012500 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
012600 01  DB2-WS.                                                              
012700     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
012800         88  CURSOR-OK                       VALUE 000.                   
012900         88  RADER-FINNS                     VALUE 000.                   
013000         88  RADER-SAKNAS                    VALUE 100.                   
013100         88  ATKOMST-FEL                     VALUE 904.                   
013200     03  GODK-SQLCODEKODER.                                               
013300         05  GODK-SQLCODE OCCURS 5                                        
013400             INDEXED BY SQLCODE-IX PIC 9(3).                              
013500                                                                          
013600                                                                          
013700                                                                          
013800                                                                          
013900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
014000 01  DLI-IO-WDD311.                                                       
014100*    03  -COPY WDD311                                                     
014200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
       01  DLI-IO-AREA-B601.                                                    
      *    03  -COPY WDB601                                                     
014300                                                                          
014400                                                                          
014500 01  FILLER                      PIC X(16)  VALUE 'BYPRO-AREA'.           
014600                                                                          
014700*01  -COPY BYPRO -PRE BYPRO-                                              
014800                                                                          
014900     EXEC SQL INCLUDE BYPRO END-EXEC.                                     
015000                                                                          
015100                                                                          
015200                                                                          
015300 LINKAGE SECTION.                                                         
015400*01  -COPY W0009  -PRE MSG-      PIC X.                                   
015500                                                                          
015600*01  -COPY W0008  -PRE WDD3-                                              
015700     05  FILLER                  PIC X.                                   
      *01  -COPY W0008  -PRE WDB6-                                              
           05  FILLER                  PIC X.                                   
           EJECT                                                                
015800                                                                          
015900                                                                          
016000                                                                          
016100 PROCEDURE DIVISION  USING MSG-PCB WDD3-PCB WDB6-PCB.                     
016200 MAIN SECTION.                                                            
016300     ENTRY 'DLITCBL' USING MSG-PCB WDD3-PCB WDB6-PCB.                     
016400                                                                          
016500     PERFORM S01-HAEMTA-ANROPSDATA                                        
016600     IF SUB-KDRC = 0                                                      
016700        PERFORM A-INIT                                                    
016800        PERFORM B-KOLLA-NYCKLAR                                           
016900        IF NYCKLAR-OK                                                     
017000           PERFORM F-LAES-VISA-INFO                                       
017100        END-IF                                                            
017200        PERFORM S02-RETURNERA-SVAR                                        
017300     END-IF                                                               
017400                                                                          
017500                                                                          
017600     PERFORM Z-FINIT                                                      
017700     MOVE ZERO TO RETURN-CODE                                             
017800     GOBACK                                                               
017900     .                                                                    
018000                                                                          
018100                                                                          
018200                                                                          
018300 A-INIT SECTION.                                                          
018400                                                                          
018500     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
018600                                                                          
018700     INITIALIZE GODK-SQLCODEKODER                                         
018800                                                                          
018900     MOVE ALL '+'   TO RESP-AREA                                          
019000     MOVE SPACE     TO RESP-IDMSG-ERROR                                   
019100                       RESP-IDMSG-INFO                                    
019200                       RESP-IDELMT-ERROR                                  
019300                       RESP-W30158O1                                      
019400     MOVE 001       TO RESP-IDMSGVER                                      
019500                                                                          
019600     .                                                                    
019700                                                                          
019800                                                                          
019900                                                                          
020000 B-KOLLA-NYCKLAR SECTION.                                                 
020100                                                                          
020200     MOVE 'B-KOLLA-NYCKLAR ' TO CURRENT-SECTION                           
020300                                                                          
020400     MOVE JA TO NYCKLAR-SW                                                
020500                                                                          
020600     IF REQU-IDARTNR-KEY = ALL '+'                                        
020700        MOVE ZERO         TO REQU-IDARTNR-KEY                             
020710     ELSE                                                                 
020720        MOVE REQU-IDARTNR-KEY                                             
020730                          TO RESP-IDARTNR-KEY                             
020800     END-IF                                                               
020810     IF REQU-IDARTNR-KEY NOT NUMERIC                                      
020820        MOVE NEJ          TO NYCKLAR-SW                                   
020830     END-IF                                                               
020900                                                                          
021000     IF REQU-IDARTNR-OBJ-KEY = ALL '+'                                    
021100        MOVE ZERO         TO REQU-IDARTNR-OBJ-KEY                         
021110     ELSE                                                                 
021120        MOVE REQU-IDARTNR-OBJ-KEY                                         
021130                          TO RESP-IDARTNR-OBJ-KEY                         
021200     END-IF                                                               
021210     IF REQU-IDARTNR-OBJ-KEY NOT NUMERIC                                  
021220        MOVE NEJ          TO NYCKLAR-SW                                   
021230     END-IF                                                               
021300                                                                          
021400     IF REQU-IDARTNR-KEY     = ZERO  AND                                  
021500        REQU-IDARTNR-OBJ-KEY = ZERO                                       
021600        MOVE NEJ          TO NYCKLAR-SW                                   
021700     END-IF                                                               
021800                                                                          
021900     IF REQU-IDARTNR-KEY     > ZERO  AND                                  
022000        REQU-IDARTNR-OBJ-KEY > ZERO                                       
022100        MOVE NEJ          TO NYCKLAR-SW                                   
022200     END-IF                                                               
022300                                                                          
           MOVE REQU-IDDC-KEY TO W-IDDC-B6                                      
                                                                                
022400     IF REQU-IDSPRAK-KEY = 'SV' OR 'EN' OR 'ZH'                           
022500        CONTINUE                                                          
022600     ELSE                                                                 
022700        MOVE NEJ          TO NYCKLAR-SW                                   
022800     END-IF                                                               
022900                                                                          
023000     IF NYCKLAR-FEL                                                       
023100       MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                             
           ELSE                                                                 
             PERFORM IMS-GU-WDB601                                              
023200     END-IF                                                               
023300     .                                                                    
023400                                                                          
023500                                                                          
023600                                                                          
023700 F-LAES-VISA-INFO SECTION.                                                
023800                                                                          
023900     MOVE 'F-LAES-VISA-INFO' TO CURRENT-SECTION                           
024000                                                                          
024100     IF REQU-IDSPRAK-KEY = 'SV'                                           
024200        MOVE 'S  '      TO W-IDSKYLT                                      
024300     ELSE                                                                 
024400        IF REQU-IDSPRAK-KEY = 'ZH'                                        
024500           MOVE 'RCN' TO W-IDSKYLT                                        
024600        ELSE                                                              
024700           MOVE 'GB ' TO W-IDSKYLT                                        
024800        END-IF                                                            
024900     END-IF                                                               
025000                                                                          
025100     IF REQU-IDARTNR-KEY > ZERO                                           
025200        MOVE REQU-IDARTNR-KEY     TO W-IDARTNR-BYPRO                      
025300        PERFORM DB2-DCL-OPN-CRS-BYPRO-ORIG                                
025400     ELSE                                                                 
025500        MOVE REQU-IDARTNR-OBJ-KEY TO W-IDARTNR-BYPRO                      
025600        PERFORM DB2-DCL-OPN-CRS-BYPRO-BYTE                                
025700     END-IF                                                               
025800                                                                          
025900     IF ATKOMST-FEL                                                       
026000*    NOT FOUND       ***                                                  
026100        MOVE ERR-ATKOMST-FEL TO RESP-IDMSG-ERROR                          
026200        MOVE 'IDARTNR'       TO RESP-IDELMT-ERROR                         
026300     ELSE                                                                 
026400        MOVE 1 TO RAD-INDX                                                
026500        IF REQU-IDARTNR-KEY > ZERO                                        
026600           PERFORM DB2-FETCH-BYPRO-ORIG                                   
026700        ELSE                                                              
026800           PERFORM DB2-FETCH-BYPRO-BYTE                                   
026900        END-IF                                                            
027000        PERFORM UNTIL RADER-SAKNAS                                        
027100                   OR RAD-INDX > RAD-INDX-MAX                             
027200                                                                          
027300           IF REQU-IDARTNR-KEY > ZERO                                     
027310              MOVE BYPRO-IDARTNR-BYT  TO RESP-IDARTNR (RAD-INDX)          
027320                                         W-IDARTNR-D311                   
027500           ELSE                                                           
027510             MOVE BYPRO-IDARTNR      TO RESP-IDARTNR (RAD-INDX)           
027520                                        W-IDARTNR-D311                    
027700           END-IF                                                         
027710           PERFORM FA-VISA-BEART                                          
027800           MOVE RAD-INDX TO RESP-KVRADER                                  
027900                                                                          
028000           IF REQU-IDARTNR-KEY > ZERO                                     
028100              PERFORM DB2-FETCH-BYPRO-ORIG                                
028200           ELSE                                                           
028300              PERFORM DB2-FETCH-BYPRO-BYTE                                
028400           END-IF                                                         
028500           ADD 1                 TO RAD-INDX                              
028600        END-PERFORM                                                       
028610        IF  RAD-INDX = 1                                                  
028620           MOVE ZERO             TO RESP-KVRADER                          
028621           MOVE ERR-RADER-SAKNAS TO RESP-IDMSG-ERROR                      
028622           MOVE 'IDARTNR'        TO RESP-IDELMT-ERROR                     
028623        END-IF                                                            
028700        IF REQU-IDARTNR-KEY > ZERO                                        
028800           PERFORM DB2-CLOSE-CRS-BYPRO-ORIG                               
028900        ELSE                                                              
029000           PERFORM DB2-CLOSE-CRS-BYPRO-BYTE                               
029100        END-IF                                                            
029200     END-IF                                                               
029300     .                                                                    
032500 FA-VISA-BEART            SECTION.                                        
032600                                                                          
032700     MOVE 'FB-VISA-ORIGINAL' TO CURRENT-SECTION                           
032800                                                                          
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
032860     PERFORM IMS-GU-WDD311                                                
032870     IF SEGMENT-FINNS                                                     
032880        MOVE TEXT-BEART      TO TRAUTF8-TECONV-FROM                       
032890     ELSE                                                                 
032891        MOVE SPACE           TO TRAUTF8-TECONV-FROM                       
032892     END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GU-WDD311                                               
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
032893     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
032894     MOVE TRAUTF8-TECONV-TO  TO RESP-BEART (RAD-INDX)                     
033700     .                                                                    
034100 Z-FINIT SECTION.                                                         
034200                                                                          
034300     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
034500     .                                                                    
034800*    --- DISPATCHER-SEKTIONER                                             
034900 S01-HAEMTA-ANROPSDATA SECTION.                                           
035000                                                                          
035100     MOVE 'S01-HAEMTA-ANROP' TO CURRENT-SECTION                           
035200                                                                          
035300     MOVE 'GETARG'                      TO SUB-KDFUNC                     
035400     MOVE 'CARPARTS.NDC.CORECROSSINDEX' TO SUB-ADDISPABS                  
035500     MOVE LENGTH OF REQU-AREA           TO SUB-KVDLEN                     
035600                                                                          
035700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
035800                                                                          
035900     IF SUB-KDRC > 0                                                      
036000        MOVE SUB-KDRC       TO KDRC-DISPLAY                               
036100        STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                    
036200        DELIMITED BY SIZE INTO FELTEXT                                    
036300        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
036400     END-IF                                                               
036500     .                                                                    
036600                                                                          
036700                                                                          
036800                                                                          
036900 S02-RETURNERA-SVAR SECTION.                                              
037000                                                                          
037100     MOVE 'S02-RETURNERA-SV' TO CURRENT-SECTION                           
037200                                                                          
037300     MOVE 'RETURN'             TO SUB-KDFUNC                              
037400     MOVE LENGTH OF RESP-AREA  TO SUB-KVDLEN                              
037500                                                                          
037600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
037700                                                                          
037800     IF SUB-KDRC > 0                                                      
037900        MOVE SUB-KDRC          TO KDRC-DISPLAY                            
038000        STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                    
038100        DELIMITED BY SIZE    INTO FELTEXT                                 
038200        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
038300     END-IF                                                               
038400     .                                                                    
038500                                                                          
038600                                                                          
038700                                                                          
038800 IMS-GU-WDD311 SECTION.                                                   
038900                                                                          
039000     MOVE 'IMS-GU-WDD311   ' TO CURRENT-IMS-SECTION                       
039100                                                                          
039200     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-D311-X ')'                    
039300            DELIMITED BY SIZE INTO SSA1                                   
039400     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
039500            DELIMITED BY SIZE INTO SSA2                                   
039600     MOVE '  GE'                TO GODK-STATUSKODER                       
039700     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
039800     MOVE WDD3-STATUS-CODE      TO STATUS-WS                              
039900     PERFORM IMS-STATUSKONTROLL                                           
040000     .                                                                    
       IMS-GU-WDB601    SECTION.                                                
           STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
           MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           EJECT                                                                
040500 IMS-STATUSKONTROLL SECTION.                                              
040600                                                                          
040700     SET STATUS-IX TO 1                                                   
040800     SEARCH GODK-STATUS                                                   
040900        AT END                                                            
041000           CALL FELLOG                                                    
041100        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
041200           CONTINUE                                                       
041300     END-SEARCH                                                           
041400     .                                                                    
041800 DB2-DCL-OPN-CRS-BYPRO-ORIG SECTION.                                      
041900                                                                          
042000     MOVE 'DB2-DCL-OPEN-ORI' TO CURRENT-DB2-SECTION                       
042100                                                                          
042200     EXEC SQL DECLARE BYPRO-ORIG-CRS CURSOR FOR                           
042300              SELECT IDARTNR_BYT,                                         
042400                     IDARTNR                                              
042500              FROM BYPRO                                                  
042600              WHERE IDARTNR = :W-IDARTNR-BYPRO                            
042700              ORDER BY IDARTNR_BYT                                        
042800     END-EXEC                                                             
042900                                                                          
043000     MOVE 000100904            TO GODK-SQLCODEKODER                       
043100     EXEC SQL OPEN BYPRO-ORIG-CRS END-EXEC                                
043200     MOVE SQLCODE              TO SQLCODE-WS                              
043300     PERFORM DB2-STATUSKONTROLL                                           
043400     .                                                                    
043800 DB2-FETCH-BYPRO-ORIG SECTION.                                            
043900                                                                          
044000     MOVE 'DB2-FETCH-ORIG  ' TO CURRENT-DB2-SECTION                       
044100                                                                          
044200     MOVE 000100               TO GODK-SQLCODEKODER                       
044300     EXEC SQL FETCH BYPRO-ORIG-CRS INTO                                   
044400            :BYPRO-IDARTNR-BYT,                                           
044500            :BYPRO-IDARTNR                                                
044600     END-EXEC                                                             
044700     MOVE SQLCODE              TO SQLCODE-WS                              
044800     PERFORM DB2-STATUSKONTROLL                                           
044900     .                                                                    
045200 DB2-CLOSE-CRS-BYPRO-ORIG SECTION.                                        
045300                                                                          
045400     MOVE 'DB2-CLOSE-ORIG  ' TO CURRENT-DB2-SECTION                       
045500                                                                          
045600     EXEC SQL CLOSE BYPRO-ORIG-CRS END-EXEC                               
045700     .                                                                    
046000 DB2-DCL-OPN-CRS-BYPRO-BYTE SECTION.                                      
046100                                                                          
046200     MOVE 'DB2-DCL-OPEN-BYT' TO CURRENT-DB2-SECTION                       
046300                                                                          
046400     EXEC SQL DECLARE BYPRO-BYT-CRS CURSOR FOR                            
046500              SELECT IDARTNR_BYT,                                         
046600                     IDARTNR                                              
046700              FROM BYPRO                                                  
046800              WHERE IDARTNR_BYT = :W-IDARTNR-BYPRO                        
046900              ORDER BY IDARTNR                                            
047000     END-EXEC                                                             
047100                                                                          
047200     MOVE 000100904            TO GODK-SQLCODEKODER                       
047300     EXEC SQL OPEN BYPRO-BYT-CRS END-EXEC                                 
047400     MOVE SQLCODE              TO SQLCODE-WS                              
047500     PERFORM DB2-STATUSKONTROLL                                           
047600     .                                                                    
048000 DB2-FETCH-BYPRO-BYTE SECTION.                                            
048100                                                                          
048200     MOVE 'DB2-FETCH-BYT   ' TO CURRENT-DB2-SECTION                       
048300                                                                          
048400     MOVE 000100               TO GODK-SQLCODEKODER                       
048500     EXEC SQL FETCH BYPRO-BYT-CRS INTO                                    
048600            :BYPRO-IDARTNR-BYT,                                           
048700            :BYPRO-IDARTNR                                                
048800     END-EXEC                                                             
048900     MOVE SQLCODE              TO SQLCODE-WS                              
049000     PERFORM DB2-STATUSKONTROLL                                           
049100     .                                                                    
049400 DB2-CLOSE-CRS-BYPRO-BYTE SECTION.                                        
049500                                                                          
049600     MOVE 'DB2-CLOSE-BYT   ' TO CURRENT-DB2-SECTION                       
049700                                                                          
049800     EXEC SQL CLOSE BYPRO-BYT-CRS END-EXEC                                
049900     .                                                                    
050200 DB2-STATUSKONTROLL  SECTION.                                             
050300     SET SQLCODE-IX                TO 1                                   
050400     SEARCH GODK-SQLCODE                                                  
050500       AT END                                                             
050600         CALL FELLOG                                                      
050700        WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
050800           CONTINUE                                                       
050900     END-SEARCH                                                           
051000     .                                                                    
