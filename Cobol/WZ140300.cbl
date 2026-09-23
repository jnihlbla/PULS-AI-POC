000100PROCESS DYNAM                                                             
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WZ140300.                                                
000400 AUTHOR.         RAHUL REDDY.                                             
000500 DATE-WRITTEN.   20/04/24.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        BATCH MODE FOR RESTARTING DISTRIBUTIONS.                         
001100*        INPUT - IDOUTTYPE, IDOUTREC, IDLIST,TIREGDAT, TIKLOCK,           
001200*                IDLOPNR                                                  
001300*                                                                         
001400*    ABENDCODES:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000                                                                          
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400*          --- INPUT WITH THE RESTART KEY                                 
002500     SELECT WZ1403                     ASSIGN TO WZ1403D1.                
002600                                                                          
002700 DATA DIVISION.                                                           
002800 FILE SECTION.                                                            
002900 FD  WZ1403                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200 01  FILLER                     PIC X(80).                                
003300                                                                          
003400 WORKING-STORAGE SECTION.                                                 
003500 77  IDPGM                       PIC X(8)    VALUE 'WZ140300'.            
003600 77  YES                         PIC X       VALUE 'J'.                   
003700 77  NOO                         PIC X       VALUE 'N'.                   
003800 77  GOBACK-RC                   PIC 9(4)    BINARY.                      
003900 77  WS-SECTION                  PIC X(20)   VALUE SPACE.                 
004000 77  WS-PRINT                    PIC X(4)    VALUE 'PRT '.                
004100 77  WS-FAX                      PIC X(4)    VALUE 'FAX '.                
004200 77  WS-VCOM                     PIC X(4)    VALUE 'VCOM'.                
004300 77  WS-EDI                      PIC X(4)    VALUE 'EDI '.                
004400 77  WS-MAIL                     PIC X(4)    VALUE 'MAIL'.                
004500 77  WS-ONDEMAND                 PIC X(4)    VALUE 'ONDE'.                
004600 77  WS-GET-IT                   PIC X(4)    VALUE 'GETI'.                
004700 77  WS-WEB                      PIC X(4)    VALUE 'WEB '.                
004800 77  WS-DATE-FORMAT              PIC X(6)    VALUE 'YYMMDD'.              
004900 77  WS-MAX-KVANTEX-PRINTAD      PIC S9(1)   VALUE +9 COMP-3.             
005000                                                                          
005100 77  WZ1403-EOF-SW               PIC X       VALUE 'N'.                   
005200     88  END-OF-WZ1403                       VALUE 'J'.                   
005300                                                                          
005400                                                                          
005500 01  WS-TIREGDAT-KEY             PIC S9(7)   VALUE ZERO COMP-3.           
005600 01  WS-TIKLOCK-KEY              PIC S9(9)   VALUE ZERO COMP-3.           
005700 01  WS-REKY-IDLOPNR-KEY         PIC S9(3)   VALUE ZERO COMP-3.           
005800 01  WS-REDA-IDLOPNR-KEY         PIC S9(3)   VALUE ZERO COMP-3.           
005900                                                                          
006000 01  WS-NOT-OK-COUNT             PIC 9(3)    VALUE ZERO.                  
006100 01  WS-IDCALL                   PIC S9(9)   COMP VALUE +0.               
006200 01  WS-KDFUNC                   PIC X(10)   VALUE SPACE.                 
006300 01  WS-KDRC                     PIC S9(9)   COMP VALUE +0.               
006400 01  WS-TEOUTDATA-L              PIC S9(9)   COMP VALUE +0.               
006500 01  WS-TEOUTDATA                PIC X(3000) VALUE SPACE.                 
006600                                                                          
006700 01  WS-RESULT-LINE              PIC X(80)   VALUE ALL '-'.               
006800 01  WS-RESULT-BLANK             PIC X(80)   VALUE SPACE.                 
006900 01  WS-RESULT-HEAD-1.                                                    
007000     03  FILLER                  PIC X(06)   VALUE 'RESULT'.              
007100     03  FILLER                  PIC X       VALUE SPACE.                 
007200     03  FILLER                  PIC X(15)   VALUE 'IDOUTTYPE'.           
007300     03  FILLER                  PIC X       VALUE SPACE.                 
007400     03  FILLER                  PIC X(26)   VALUE 'IDOUTREC'.            
007500     03  FILLER                  PIC X       VALUE SPACE.                 
007600     03  FILLER                  PIC X(10)   VALUE 'IDLIST'.              
007700     03  FILLER                  PIC X       VALUE SPACE.                 
007800     03  FILLER                  PIC X(06)   VALUE 'REGDAT'.              
007900     03  FILLER                  PIC X       VALUE SPACE.                 
008000     03  FILLER                  PIC X(08)   VALUE '   KLOCK'.            
008100     03  FILLER                  PIC X       VALUE SPACE.                 
008200     03  FILLER                  PIC X(03)   VALUE 'SEQ'.                 
008300 01  WS-RESULT-HEAD-2.                                                    
008400     03  FILLER                  PIC X(07)   VALUE SPACE.                 
008500     03  FILLER                  PIC X(04)   VALUE 'TYPE'.                
008600     03  FILLER                  PIC X       VALUE SPACE.                 
008700     03  FILLER                  PIC X(33)   VALUE 'DESTINATION'.         
008800     03  FILLER                  PIC X       VALUE SPACE.                 
008900     03  FILLER                  PIC X(34)   VALUE 'COMMENTS'.            
009000 01  WS-RESULT-DATA-1.                                                    
009100     03  WS-RESULT               PIC X(06).                               
009200         88  RESULT-NOT-OK                   VALUE 'NOT OK'.              
009300         88  RESULT-OK                       VALUE '    OK'.              
009400     03  FILLER                  PIC X       VALUE SPACE.                 
009500     03  WS-IDOUTTYPE            PIC X(15).                               
009600     03  FILLER                  PIC X       VALUE SPACE.                 
009700     03  WS-IDOUTREC             PIC X(26).                               
009800     03  FILLER                  PIC X       VALUE SPACE.                 
009900     03  WS-IDLIST               PIC X(10).                               
010000     03  FILLER                  PIC X       VALUE SPACE.                 
010100     03  WS-TIREGDAT             PIC Z(06).                               
010200     03  FILLER                  PIC X       VALUE SPACE.                 
010300     03  WS-TIKLOCK              PIC Z(08).                               
010400     03  FILLER                  PIC X       VALUE SPACE.                 
010500     03  WS-IDLOPNR              PIC 9(03).                               
010600 01  WS-RESULT-DATA-2.                                                    
010700     03  FILLER                  PIC X(07)   VALUE SPACE.                 
010800     03  WS-KDOUTMETH            PIC X(04)   VALUE SPACE.                 
010900     03  FILLER                  PIC X       VALUE SPACE.                 
011000     03  WS-IDOUTDEST            PIC X(33)   VALUE SPACE.                 
011100     03  FILLER                  PIC X       VALUE SPACE.                 
011200     03  WS-COMMENT              PIC X(34)   VALUE SPACE.                 
011300                                                                          
011400 01  GENERAL-SUBPROGRAMS.                                                 
011500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011600     03  WZ11OUTP                PIC X(8)    VALUE 'WZ11OUTP'.            
011700     03  WZ11OUTF                PIC X(8)    VALUE 'WZ11OUTF'.            
011800     03  WZ11OUTM                PIC X(8)    VALUE 'WZ11OUTM'.            
011900     03  WZ11OUTV                PIC X(8)    VALUE 'WZ11OUTV'.            
012000     03  WZ11OUTO                PIC X(8)    VALUE 'WZ11OUTO'.            
012100     03  WZ11OUTA                PIC X(8)    VALUE 'WZ11OUTA'.            
012200     03  WZ11OUTW                PIC X(8)    VALUE 'WZ11OUTW'.            
012300                                                                          
012400*    --- PARAMETRAR TILL SUBPROGRAMS                                      
012500*01  -COPY W0005   -PRE  POSTSUM-                                         
012600                                                                          
012700 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTF-AREA'.        
012800*01 -COPY WZ11OUTF                                                        
012900 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTM-AREA'.        
013000*01 -COPY WZ11OUTM                                                        
013100 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTO-AREA'.        
013200*01 -COPY WZ11OUTO                                                        
013300 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTP-AREA'.        
013400*01 -COPY WZ11OUTP                                                        
013500 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTV-AREA'.        
013600*01 -COPY WZ11OUTV                                                        
013700 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTA-AREA'.        
013800*01 -COPY WZ11OUTA                                                        
013900 01  FILLER                      PIC X(16)  VALUE 'WZ11OUTW-AREA'.        
014000*01 -COPY WZ11OUTW                                                        
014100                                                                          
014200 01  IN-AREA-START               PIC X(24)   VALUE                        
014300                                 'IN-AREA-START  '.                       
014400 01  IN-AREA.                                                             
014500     03  IN-IDOUTTYPE            PIC X(15).                               
014600     03  IN-IDOUTREC             PIC X(30).                               
014700     03  IN-IDLIST               PIC X(10).                               
014800     03  IN-TIREGDAT             PIC 9(06).                               
014900     03  IN-TIKLOCK              PIC 9(08).                               
015000     03  IN-IDLOPNR              PIC 9(03).                               
015100                                                                          
015200 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
015300       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
015400                                                                          
015500 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
015600 01  DB2-WS.                                                              
015700     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
015800         88  LINES-FOUND                     VALUE 000.                   
015900         88  LINES-MISSING                   VALUE 100.                   
016000     03  GOOD-SQLCODECODES.                                               
016100         05  GOOD-SQLCODE OCCURS 5                                        
016200             INDEXED BY SQLCODE-IX PIC 9(3).                              
016300                                                                          
016400 01  FILLER                      PIC X(16)   VALUE 'TZ4DIRU-AREA'.        
016500*01  -COPY TZ4DIRU -PRE DIRU-                                             
016600                                                                          
016700 01  FILLER                      PIC X(16)   VALUE 'TZ4REKY-AREA'.        
016800*01  -COPY TZ4REKY -PRE REKY-                                             
016900                                                                          
017000 01  FILLER                      PIC X(16)   VALUE 'TZ4REDA-AREA'.        
017100*01  -COPY TZ4REDA -PRE REDA-                                             
017200                                                                          
017300     EXEC SQL INCLUDE TZ4DIRU END-EXEC.                                   
017400     EXEC SQL INCLUDE TZ4REKY END-EXEC.                                   
017500     EXEC SQL INCLUDE TZ4REDA END-EXEC.                                   
017600                                                                          
017700 PROCEDURE DIVISION.                                                      
017800 MAIN SECTION.                                                            
017900                                                                          
018000     PERFORM A-INIT                                                       
018100     PERFORM S01-READ-WZ1403                                              
018200     PERFORM UNTIL END-OF-WZ1403                                          
018300       SET RESULT-OK             TO TRUE                                  
018400       MOVE SPACES               TO WS-COMMENT                            
018500                                    WS-KDOUTMETH                          
018600                                    WS-IDOUTDEST                          
018700       PERFORM B-SET-KEYS                                                 
018800       PERFORM DB2-SELECT-TZ4REKY-TAB                                     
018900       IF RESULT-OK                                                       
019000         MOVE REKY-KDOUTMETH     TO WS-KDOUTMETH                          
019100         MOVE REKY-IDOUTDEST     TO WS-IDOUTDEST                          
019200         PERFORM C-OPEN-CHANNEL                                           
019300         IF RESULT-OK                                                     
019400           PERFORM D-SEND-DATA                                            
019500           PERFORM E-CLOSE-CHANNEL                                        
019600         END-IF                                                           
019700         IF RESULT-OK                                                     
019800           PERFORM F-UDPATE-SENT-TIMES                                    
019900         END-IF                                                           
020000       ELSE                                                               
020100         IF LINES-MISSING                                                 
020200           MOVE 'RESTART KEY NOT FOUND'                                   
020300                                 TO WS-COMMENT                            
020400         END-IF                                                           
020500       END-IF                                                             
020600                                                                          
020700       IF RESULT-NOT-OK                                                   
020800         ADD 1                   TO WS-NOT-OK-COUNT                       
020900       END-IF                                                             
021000       DISPLAY WS-RESULT-DATA-1                                           
021100       DISPLAY WS-RESULT-DATA-2                                           
021200       DISPLAY WS-RESULT-BLANK                                            
021300       PERFORM S01-READ-WZ1403                                            
021400     END-PERFORM                                                          
021500                                                                          
021600     PERFORM Z-FINIT                                                      
021700                                                                          
021800     IF WS-NOT-OK-COUNT > 0                                               
021900       MOVE 4                    TO GOBACK-RC                             
022000     END-IF                                                               
022100     MOVE GOBACK-RC              TO RETURN-CODE                           
022200     GOBACK                                                               
022300     .                                                                    
022400                                                                          
022500 A-INIT SECTION.                                                          
022600                                                                          
022700     OPEN INPUT WZ1403                                                    
022800     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
022900     DISPLAY WS-RESULT-HEAD-1                                             
023000     DISPLAY WS-RESULT-HEAD-2                                             
023100     DISPLAY WS-RESULT-LINE                                               
023200     MOVE ZERO                   TO GOBACK-RC                             
023300     .                                                                    
023400 B-SET-KEYS SECTION.                                                      
023500                                                                          
023600     MOVE IN-IDOUTTYPE           TO WS-IDOUTTYPE                          
023700     MOVE IN-IDOUTREC            TO WS-IDOUTREC                           
023800     MOVE IN-IDLIST              TO WS-IDLIST                             
023900     MOVE IN-TIREGDAT            TO WS-TIREGDAT                           
024000                                    WS-TIREGDAT-KEY                       
024100     MOVE IN-TIKLOCK             TO WS-TIKLOCK                            
024200                                    WS-TIKLOCK-KEY                        
024300     MOVE IN-IDLOPNR             TO WS-IDLOPNR                            
024400                                                                          
024500*    -- REKY AND REDA USE DIFFERENT VALUES OF IDLOPNR                     
024600*    -- REKY USE THE SPECIFIELD VALUE, WHICH MAY BE > 100                 
024700*    -- BUT REDA ALWAYS USE A VALUE < 100.                                
024800*    -- IF REKY-IDLOPNR IS 000, 100, 200 ETC, REDA-IDLOPNR = 000          
024900*    -- IF REKY-IDLOPNR IS 001, 101, 201 ETC, REDA-IDLOPNR = 001          
025000*    -- AND SO ON.                                                        
025100     MOVE IN-IDLOPNR             TO WS-REKY-IDLOPNR-KEY                   
025200     IF WS-TIREGDAT-KEY < 151012                                          
025300       COMPUTE WS-REDA-IDLOPNR-KEY =                                      
025400             FUNCTION REM (WS-REKY-IDLOPNR-KEY, 100)                      
025500     ELSE                                                                 
025600       COMPUTE WS-REDA-IDLOPNR-KEY =                                      
025700             FUNCTION REM (WS-REKY-IDLOPNR-KEY, 50)                       
025800     END-IF                                                               
025900                                                                          
026000     .                                                                    
026100                                                                          
026200 C-OPEN-CHANNEL SECTION.                                                  
026300                                                                          
026400     MOVE 1                      TO WS-IDCALL                             
026500     MOVE 'OPEN'                 TO WS-KDFUNC                             
026600     PERFORM S04-CALL-WZ11OUTX                                            
026700                                                                          
026800     IF WS-KDRC > 0                                                       
026900       SET RESULT-NOT-OK         TO TRUE                                  
027000       MOVE 'OPEN CHANNEL FAILED'                                         
027100                                 TO WS-COMMENT                            
027200     END-IF                                                               
027300     .                                                                    
027400                                                                          
027500 D-SEND-DATA    SECTION.                                                  
027600                                                                          
027700     PERFORM DB2-DCL-OPN-TZ4REDA-CRS                                      
027800     IF RESULT-OK                                                         
027900       PERFORM DB2-FETCH-TZ4REDA-CRS                                      
028000       PERFORM UNTIL LINES-MISSING OR RESULT-NOT-OK                       
028100         MOVE 1                  TO WS-IDCALL                             
028200         MOVE 'PUT'              TO WS-KDFUNC                             
028300         MOVE REDA-TEOUTDATA-L   TO WS-TEOUTDATA-L                        
028400         MOVE REDA-TEOUTDATA-D(1:REDA-TEOUTDATA-L)                        
028500                                 TO WS-TEOUTDATA                          
028600         PERFORM S04-CALL-WZ11OUTX                                        
028700         IF WS-KDRC > 0                                                   
028800           SET RESULT-NOT-OK     TO TRUE                                  
028900           MOVE 'SEND TO CHANNEL FAILED'                                  
029000                                 TO WS-COMMENT                            
029100         END-IF                                                           
029200         PERFORM DB2-FETCH-TZ4REDA-CRS                                    
029300       END-PERFORM                                                        
029400       PERFORM DB2-CLOSE-TZ4REDA-CRS                                      
029500     END-IF                                                               
029600     .                                                                    
029700                                                                          
029800 E-CLOSE-CHANNEL SECTION.                                                 
029900     MOVE 1                      TO WS-IDCALL                             
030000     MOVE 'CLOSE'                TO WS-KDFUNC                             
030100     PERFORM S04-CALL-WZ11OUTX                                            
030200     IF WS-KDRC > 0                                                       
030300       SET RESULT-NOT-OK         TO TRUE                                  
030400       MOVE 'CLOSE CHANNEL FAILED'                                        
030500                                 TO WS-COMMENT                            
030600     END-IF                                                               
030700     .                                                                    
030800                                                                          
030900 F-UDPATE-SENT-TIMES SECTION.                                             
031000     IF REKY-KVANTEX-PRINTAD < WS-MAX-KVANTEX-PRINTAD                     
031100       ADD 1                     TO REKY-KVANTEX-PRINTAD                  
031200       PERFORM DB2-UPDATE-TZ4REKY-TAB                                     
031300     END-IF                                                               
031400     .                                                                    
031500                                                                          
031600 Z-FINIT SECTION.                                                         
031700     CLOSE WZ1403                                                         
031800                                                                          
031900     MOVE 'S'                    TO POSTSUM-OPKOD                         
032000     CALL POSTSUM             USING POSTSUM-PARM                          
032100     .                                                                    
032200                                                                          
032300 S01-READ-WZ1403  SECTION.                                                
032400     READ WZ1403               INTO IN-AREA                               
032500       AT END                                                             
032600         MOVE HIGH-VALUE         TO IN-AREA                               
032700         SET END-OF-WZ1403       TO TRUE                                  
032800                                                                          
032900       NOT AT END                                                         
033000         MOVE 'WZ1403'           TO POSTSUM-FDNAMN                        
033100         MOVE 'WZ1403D1'         TO POSTSUM-DDNAMN2                       
033200         MOVE SPACE              TO POSTSUM-TRANSTYP                      
033300         CALL POSTSUM         USING POSTSUM-PARM                          
033400     END-READ                                                             
033500     .                                                                    
033600                                                                          
033700 S04-CALL-WZ11OUTX SECTION.                                               
033800                                                                          
033900     MOVE 0                      TO WS-KDRC                               
034000     EVALUATE REKY-KDOUTMETH                                              
034100       WHEN WS-PRINT                                                      
034200         MOVE WS-IDCALL          TO OUTP-IDCALL                           
034300         MOVE WS-KDFUNC          TO OUTP-KDFUNC                           
034400         IF WS-KDFUNC = 'OPEN'                                            
034500*          -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE              
034600*          -- THERE IS A PROBLEM IN THE DISTRIBUTION                      
034700           MOVE IN-IDOUTTYPE                                              
034800                                 TO OUTP-IDOUTTYPE                        
034900           MOVE IN-IDOUTREC                                               
035000                                 TO OUTP-IDOUTREC                         
035100           MOVE IN-IDLIST        TO OUTP-IDLIST                           
035200           MOVE WS-TIREGDAT-KEY  TO OUTP-TIREGDAT                         
035300           MOVE WS-TIKLOCK-KEY   TO OUTP-TIKLOCK                          
035400                                                                          
035500           MOVE REKY-IDOUTDEST   TO OUTP-IDOUTDEST                        
035600           MOVE REKY-KVCOPIES    TO OUTP-KVCOPIES                         
035700           MOVE REKY-IDPFDEF     TO OUTP-IDPFDEF                          
035800           MOVE REKY-IDFORMSNM   TO OUTP-IDFORMSNM                        
035900           MOVE REKY-FLCARRCNTL  TO OUTP-FLCARRCNTL                       
036000           MOVE REKY-FLACIF      TO OUTP-FLACIF                           
036100*          -- OLD RULES MAY LACK FLACIF, AND PAGEDEF/FORMDEF              
036200*          -- PLUS NO CARRIAGE CONTROL CHARACTERS USED TO INDICATE        
036300*          -- ACIF SHOULD BE USED (EXAMPLE: BILL-IT INVOICES)             
036400           IF REKY-FLACIF = SPACE                                         
036500             MOVE NOO            TO OUTP-FLACIF                           
036600           END-IF                                                         
036700           IF REKY-FLCARRCNTL = NOO AND REKY-IDPFDEF NOT = SPACE          
036800             MOVE YES            TO OUTP-FLACIF                           
036900           END-IF                                                         
037000           MOVE SPACE            TO OUTP-IDPCB                            
037100         END-IF                                                           
037200         IF WS-KDFUNC = 'PUT'                                             
037300           MOVE WS-TEOUTDATA-L   TO OUTP-TEOUTDATA-L                      
037400           MOVE WS-TEOUTDATA     TO OUTP-TEOUTDATA                        
037500         END-IF                                                           
037600         CALL WZ11OUTP USING OUTP-WZ11OUT                                 
037700         MOVE OUTP-KDRC          TO WS-KDRC                               
037800         MOVE OUTP-TEOUTDATA     TO WS-TEOUTDATA                          
037900                                                                          
038000       WHEN WS-FAX                                                        
038100         MOVE WS-IDCALL          TO OUTF-IDCALL                           
038200         MOVE WS-KDFUNC          TO OUTF-KDFUNC                           
038300         IF WS-KDFUNC = 'OPEN'                                            
038400*          -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE              
038500*          -- THERE IS A PROBLEM IN THE DISTRIBUTION                      
038600           MOVE IN-IDOUTTYPE                                              
038700                                 TO OUTF-IDOUTTYPE                        
038800           MOVE IN-IDOUTREC                                               
038900                                 TO OUTF-IDOUTREC                         
039000           MOVE IN-IDLIST        TO OUTF-IDLIST                           
039100           MOVE WS-TIREGDAT-KEY  TO OUTF-TIREGDAT                         
039200           MOVE WS-TIKLOCK-KEY   TO OUTF-TIKLOCK                          
039300                                                                          
039400           MOVE REKY-IDOUTDEST   TO OUTF-IDOUTDEST                        
039500           MOVE REKY-IDPFDEF     TO OUTF-IDPFDEF                          
039600           MOVE REKY-FLCARRCNTL  TO OUTF-FLCARRCNTL                       
039700           MOVE REKY-IDMAIL-SENDER TO OUTF-IDMAIL-SENDER                  
039800           MOVE REKY-TEFAX-1     TO OUTF-TEFAX(1)                         
039900           MOVE REKY-TEFAX-2     TO OUTF-TEFAX(2)                         
040000           MOVE REKY-TEFAX-3     TO OUTF-TEFAX(3)                         
040100           MOVE REKY-TEFAX-4     TO OUTF-TEFAX(4)                         
040200           MOVE REKY-TEFAX-5     TO OUTF-TEFAX(5)                         
040300         END-IF                                                           
040400         IF WS-KDFUNC = 'PUT'                                             
040500           MOVE WS-TEOUTDATA-L   TO OUTF-TEOUTDATA-L                      
040600           MOVE WS-TEOUTDATA     TO OUTF-TEOUTDATA                        
040700         END-IF                                                           
040800         CALL WZ11OUTF        USING OUTF-WZ11OUT                          
040900         MOVE OUTF-KDRC          TO WS-KDRC                               
041000         MOVE OUTF-TEOUTDATA     TO WS-TEOUTDATA                          
041100                                                                          
041200       WHEN WS-MAIL                                                       
041300         MOVE WS-IDCALL          TO OUTM-IDCALL                           
041400         MOVE WS-KDFUNC          TO OUTM-KDFUNC                           
041500         IF WS-KDFUNC = 'OPEN'                                            
041600*          -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE              
041700*          -- THERE IS A PROBLEM IN THE DISTRIBUTION                      
041800           MOVE IN-IDOUTTYPE                                              
041900                                 TO OUTM-IDOUTTYPE                        
042000           MOVE IN-IDOUTREC                                               
042100                                 TO OUTM-IDOUTREC                         
042200           MOVE IN-IDLIST        TO OUTM-IDLIST                           
042300           MOVE WS-TIREGDAT-KEY  TO OUTM-TIREGDAT                         
042400           MOVE WS-TIKLOCK-KEY   TO OUTM-TIKLOCK                          
042500                                                                          
042600           MOVE REKY-IDOUTDEST   TO OUTM-IDOUTDEST                        
042700           MOVE REKY-IDPFDEF     TO OUTM-IDPFDEF                          
042800           MOVE REKY-FLCARRCNTL  TO OUTM-FLCARRCNTL                       
042900           MOVE REKY-IDMAIL-SENDER TO OUTM-IDMAIL-SENDER                  
043000*          -- USE FIRST TEFAX LINE AS TITLE, AND SHIFT                    
043100*          -- THE OTHERS UP ONE STEP                                      
043200           IF REKY-TEFAX-1  NOT = SPACE                                   
043300             MOVE REKY-TEFAX-1   TO OUTM-IDMAILTTL                        
043400           ELSE                                                           
043500             MOVE SPACE TO OUTM-IDMAILTTL                                 
043600             STRING 'D&P Mail ('                                          
043700                    IN-IDOUTTYPE                                          
043800                    ')'                                                   
043900                    DELIMITED BY SIZE INTO OUTM-IDMAILTTL                 
044000           END-IF                                                         
044100           MOVE REKY-TEFAX-2     TO OUTM-TEFAX(1)                         
044200           MOVE REKY-TEFAX-3     TO OUTM-TEFAX(2)                         
044300           MOVE REKY-TEFAX-4     TO OUTM-TEFAX(3)                         
044400           MOVE REKY-TEFAX-5     TO OUTM-TEFAX(4)                         
044500           MOVE SPACE            TO OUTM-TEFAX(5)                         
044600                                                                          
044700         END-IF                                                           
044800         IF WS-KDFUNC = 'PUT'                                             
044900           MOVE WS-TEOUTDATA-L   TO OUTM-TEOUTDATA-L                      
045000           MOVE WS-TEOUTDATA     TO OUTM-TEOUTDATA                        
045100         END-IF                                                           
045200                                                                          
045300         CALL WZ11OUTM        USING OUTM-WZ11OUT                          
045400                                                                          
045500         MOVE OUTM-KDRC          TO WS-KDRC                               
045600         MOVE OUTM-TEOUTDATA     TO WS-TEOUTDATA                          
045700                                                                          
045800       WHEN WS-EDI                                                        
045900*        -- NOTE: THE VCOM SUBROUTINE IS USED FOR EDI TOO                 
046000         MOVE WS-IDCALL          TO OUTV-IDCALL                           
046100         MOVE WS-KDFUNC          TO OUTV-KDFUNC                           
046200         IF WS-KDFUNC = 'OPEN'                                            
046300           MOVE REKY-IDOUTDEST   TO OUTV-IDOUTDEST                        
046400           MOVE REKY-TEVCOMST    TO OUTV-TEVCOMST                         
046500           MOVE REKY-IDVCINIT    TO OUTV-IDVCINIT                         
046600         END-IF                                                           
046700         IF WS-KDFUNC = 'PUT'                                             
046800           MOVE WS-TEOUTDATA-L   TO OUTV-TEOUTDATA-L                      
046900           MOVE WS-TEOUTDATA     TO OUTV-TEOUTDATA                        
047000         END-IF                                                           
047100         CALL WZ11OUTV        USING OUTV-WZ11OUT                          
047200         MOVE OUTV-KDRC          TO WS-KDRC                               
047300         MOVE OUTV-TEOUTDATA     TO WS-TEOUTDATA                          
047400                                                                          
047500       WHEN WS-VCOM                                                       
047600         MOVE WS-IDCALL          TO OUTV-IDCALL                           
047700         MOVE WS-KDFUNC          TO OUTV-KDFUNC                           
047800         IF WS-KDFUNC = 'OPEN'                                            
047900           MOVE REKY-IDOUTDEST   TO OUTV-IDOUTDEST                        
048000           MOVE REKY-TEVCOMST    TO OUTV-TEVCOMST                         
048100           MOVE REKY-IDVCINIT    TO OUTV-IDVCINIT                         
048200         END-IF                                                           
048300         IF WS-KDFUNC = 'PUT'                                             
048400           MOVE WS-TEOUTDATA-L   TO OUTV-TEOUTDATA-L                      
048500           MOVE WS-TEOUTDATA     TO OUTV-TEOUTDATA                        
048600         END-IF                                                           
048700         CALL WZ11OUTV        USING OUTV-WZ11OUT                          
048800         MOVE OUTV-KDRC          TO WS-KDRC                               
048900         MOVE OUTV-TEOUTDATA     TO WS-TEOUTDATA                          
049000                                                                          
049100       WHEN WS-ONDEMAND                                                   
049200         MOVE WS-IDCALL          TO OUTO-IDCALL                           
049300         MOVE WS-KDFUNC          TO OUTO-KDFUNC                           
049400         IF WS-KDFUNC = 'OPEN'                                            
049500*          -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE              
049600*          -- THERE IS A PROBLEM IN THE DISTRIBUTION                      
049700           MOVE IN-IDOUTTYPE                                              
049800                                 TO OUTO-IDOUTTYPE                        
049900           MOVE IN-IDOUTREC                                               
050000                                 TO OUTO-IDOUTREC                         
050100           MOVE IN-IDLIST        TO OUTO-IDLIST                           
050200           MOVE WS-TIREGDAT-KEY  TO OUTO-TIREGDAT                         
050300           MOVE WS-TIKLOCK-KEY   TO OUTO-TIKLOCK                          
050400                                                                          
050500           MOVE REKY-IDOUTDEST   TO OUTO-IDOUTDEST                        
050600           MOVE REKY-IDPFDEF     TO OUTO-IDPFDEF                          
050700           MOVE REKY-IDFORMSNM   TO OUTO-IDFORMSNM                        
050800           MOVE REKY-FLCARRCNTL  TO OUTO-FLCARRCNTL                       
050900           MOVE REKY-FLACIF      TO OUTO-FLACIF                           
051000*          -- OLD RULES MAY LACK FLACIF, AND PAGEDEF/FORMDEF              
051100*          -- PLUS NO CARRIAGE CONTROL CHARACTERS USED TO INDICATE        
051200*          -- ACIF SHOULD BE USED (EXAMPLE: BILL-IT INVOICES)             
051300           IF REKY-FLACIF = SPACE                                         
051400             MOVE NOO            TO OUTO-FLACIF                           
051500           END-IF                                                         
051600           IF REKY-FLCARRCNTL = NOO AND REKY-IDPFDEF NOT = SPACE          
051700             MOVE YES            TO OUTO-FLACIF                           
051800           END-IF                                                         
051900           MOVE SPACE            TO OUTO-IDPCB                            
052000         END-IF                                                           
052100         IF WS-KDFUNC = 'PUT'                                             
052200           MOVE WS-TEOUTDATA-L   TO OUTO-TEOUTDATA-L                      
052300           MOVE WS-TEOUTDATA     TO OUTO-TEOUTDATA                        
052400         END-IF                                                           
052500         CALL WZ11OUTO USING OUTO-WZ11OUT                                 
052600         MOVE OUTO-KDRC          TO WS-KDRC                               
052700         MOVE OUTO-TEOUTDATA     TO WS-TEOUTDATA                          
052800                                                                          
052900       WHEN WS-GET-IT                                                     
053000         MOVE WS-IDCALL          TO OUTA-IDCALL                           
053100         MOVE WS-KDFUNC          TO OUTA-KDFUNC                           
053200         IF WS-KDFUNC = 'OPEN'                                            
053300*          -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE              
053400*          -- THERE IS A PROBLEM IN THE DISTRIBUTION                      
053500           MOVE IN-IDOUTTYPE                                              
053600                                 TO OUTA-IDOUTTYPE                        
053700           MOVE IN-IDOUTREC                                               
053800                                 TO OUTA-IDOUTREC                         
053900           MOVE IN-IDLIST        TO OUTA-IDLIST                           
054000           MOVE WS-TIREGDAT-KEY  TO OUTA-TIREGDAT                         
054100           MOVE WS-TIKLOCK-KEY   TO OUTA-TIKLOCK                          
054200                                                                          
054300           MOVE REKY-IDOUTDEST   TO OUTA-IDOUTDEST                        
054400           MOVE REKY-IDPFDEF     TO OUTA-IDPFDEF                          
054500           MOVE REKY-FLCARRCNTL  TO OUTA-FLCARRCNTL                       
054600           MOVE ZERO             TO OUTA-TIREGDAT                         
054700           MOVE ZERO             TO OUTA-TIREGTID                         
054800         END-IF                                                           
054900         IF WS-KDFUNC = 'PUT'                                             
055000           MOVE WS-TEOUTDATA-L   TO OUTA-TEOUTDATA-L                      
055100           MOVE WS-TEOUTDATA     TO OUTA-TEOUTDATA                        
055200         END-IF                                                           
055300         CALL WZ11OUTA USING OUTA-WZ11OUT                                 
055400         MOVE OUTA-KDRC          TO WS-KDRC                               
055500         MOVE OUTA-TEOUTDATA     TO WS-TEOUTDATA                          
055600                                                                          
055700       WHEN WS-WEB                                                        
055800         MOVE WS-IDCALL          TO OUTW-IDCALL                           
055900         MOVE WS-KDFUNC          TO OUTW-KDFUNC                           
056000         IF WS-KDFUNC = 'OPEN'                                            
056100           MOVE IN-IDOUTTYPE TO OUTW-IDOUTTYPE                            
056200           MOVE IN-IDOUTREC      TO OUTW-IDOUTREC                         
056300           MOVE IN-IDLIST        TO OUTW-IDLIST                           
056400           MOVE WS-TIREGDAT-KEY  TO OUTW-TIREGDAT                         
056500           MOVE WS-TIKLOCK-KEY   TO OUTW-TIKLOCK                          
056600           MOVE WS-REKY-IDLOPNR-KEY   TO OUTW-IDLOPNR                     
056700         END-IF                                                           
056800         IF WS-KDFUNC = 'PUT'                                             
056900           MOVE WS-TEOUTDATA-L   TO OUTW-TEOUTDATA-L                      
057000           MOVE WS-TEOUTDATA     TO OUTW-TEOUTDATA                        
057100         END-IF                                                           
057200         CALL WZ11OUTW        USING OUTW-WZ11OUT                          
057300         MOVE OUTW-KDRC          TO WS-KDRC                               
057400         MOVE OUTW-TEOUTDATA     TO WS-TEOUTDATA                          
057500                                                                          
057600     END-EVALUATE                                                         
057700                                                                          
057800     .                                                                    
057900*    --- DB2 SECTIONS                                                     
058000 DB2-SELECT-TZ4REKY-TAB SECTION.                                          
058100                                                                          
058200     MOVE 'SELECT-TZ4REKY'       TO WS-SECTION                            
058300     MOVE 000                    TO GOOD-SQLCODECODES                     
058400     EXEC SQL                                                             
058500          SELECT KDOUTMETH                                                
058600               , IDOUTDEST                                                
058700               , KVCOPIES                                                 
058800               , FLCARRCNTL                                               
058900               , IDPFDEF                                                  
059000               , IDFORMSNM                                                
059100               , TEVCOMST                                                 
059200               , IDVCINIT                                                 
059300               , IDMAIL_SENDER                                            
059400               , TIAAMMDD_RENS                                            
059500               , TEFAX_1                                                  
059600               , TEFAX_2                                                  
059700               , TEFAX_3                                                  
059800               , TEFAX_4                                                  
059900               , TEFAX_5                                                  
060000               , KVANTEX_PRINTAD                                          
060100               , FLRULEMISS                                               
060200               , FLACIF                                                   
060300                                                                          
060400          INTO  :REKY-KDOUTMETH                                           
060500              , :REKY-IDOUTDEST                                           
060600              , :REKY-KVCOPIES                                            
060700              , :REKY-FLCARRCNTL                                          
060800              , :REKY-IDPFDEF                                             
060900              , :REKY-IDFORMSNM                                           
061000              , :REKY-TEVCOMST                                            
061100              , :REKY-IDVCINIT                                            
061200              , :REKY-IDMAIL-SENDER                                       
061300              , :REKY-TIAAMMDD-RENS                                       
061400              , :REKY-TEFAX-1                                             
061500              , :REKY-TEFAX-2                                             
061600              , :REKY-TEFAX-3                                             
061700              , :REKY-TEFAX-4                                             
061800              , :REKY-TEFAX-5                                             
061900              , :REKY-KVANTEX-PRINTAD                                     
062000              , :REKY-FLRULEMISS                                          
062100              , :REKY-FLACIF                                              
062200                                                                          
062300          FROM   TZ4REKY                                                  
062400                                                                          
062500          WHERE  IDOUTTYPE = :IN-IDOUTTYPE                                
062600           AND   IDOUTREC  = :IN-IDOUTREC                                 
062700           AND   IDLIST    = :IN-IDLIST                                   
062800           AND   TIREGDAT  = :WS-TIREGDAT-KEY                             
062900           AND   TIKLOCK   = :WS-TIKLOCK-KEY                              
063000           AND   IDLOPNR   = :WS-REKY-IDLOPNR-KEY                         
063100     END-EXEC                                                             
063200                                                                          
063300     MOVE SQLCODE TO SQLCODE-WS                                           
063400     PERFORM DB2-STATUS-CHECK                                             
063500     .                                                                    
063600 DB2-UPDATE-TZ4REKY-TAB  SECTION.                                         
063700                                                                          
063800     MOVE 'UPDATE-TZ4REKY'       TO WS-SECTION                            
063900     MOVE 000     TO GOOD-SQLCODECODES                                    
064000     EXEC SQL                                                             
064100         UPDATE TZ4REKY                                                   
064200           SET   KVANTEX_PRINTAD = :REKY-KVANTEX-PRINTAD                  
064300                                                                          
064400         WHERE   IDOUTTYPE = :IN-IDOUTTYPE                                
064500          AND    IDOUTREC  = :IN-IDOUTREC                                 
064600          AND    IDLIST    = :IN-IDLIST                                   
064700          AND    TIREGDAT  = :WS-TIREGDAT-KEY                             
064800          AND    TIKLOCK   = :WS-TIKLOCK-KEY                              
064900          AND    IDLOPNR   = :WS-REKY-IDLOPNR-KEY                         
065000     END-EXEC                                                             
065100                                                                          
065200     MOVE SQLCODE TO SQLCODE-WS                                           
065300     PERFORM DB2-STATUS-CHECK                                             
065400     .                                                                    
065500 DB2-DCL-OPN-TZ4REDA-CRS SECTION.                                         
065600                                                                          
065700     MOVE 'OPN-TZ4REDA-CRS'      TO WS-SECTION                            
065800     MOVE 000100 TO GOOD-SQLCODECODES                                     
065900                                                                          
066000     EXEC SQL                                                             
066100       DECLARE TZ4REDA-CRS CURSOR WITH HOLD FOR                           
066200                                                                          
066300       SELECT  TEOUTDATA                                                  
066400                                                                          
066500       FROM    TZ4REDA                                                    
066600                                                                          
066700       WHERE  IDOUTTYPE = :IN-IDOUTTYPE                                   
066800        AND   IDOUTREC  = :IN-IDOUTREC                                    
066900        AND   IDLIST    = :IN-IDLIST                                      
067000        AND   TIREGDAT  = :WS-TIREGDAT-KEY                                
067100        AND   TIKLOCK   = :WS-TIKLOCK-KEY                                 
067200        AND   IDLOPNR   = :WS-REDA-IDLOPNR-KEY                            
067300                                                                          
067400       ORDER BY IDOUTTYPE                                                 
067500              , IDOUTREC                                                  
067600              , IDLIST                                                    
067700              , TIREGDAT                                                  
067800              , TIKLOCK                                                   
067900              , IDLOPNR                                                   
068000              , KVPOST                                                    
068100     END-EXEC                                                             
068200                                                                          
068300     MOVE 000100  TO GOOD-SQLCODECODES                                    
068400                                                                          
068500     EXEC SQL                                                             
068600       OPEN TZ4REDA-CRS                                                   
068700     END-EXEC                                                             
068800                                                                          
068900     MOVE SQLCODE TO SQLCODE-WS                                           
069000     PERFORM DB2-STATUS-CHECK                                             
069100     .                                                                    
069200 DB2-FETCH-TZ4REDA-CRS SECTION.                                           
069300                                                                          
069400     MOVE 'FETCH-TZ4REDA-CRS'    TO WS-SECTION                            
069500     MOVE 000100  TO GOOD-SQLCODECODES                                    
069600                                                                          
069700     EXEC SQL                                                             
069800                                                                          
069900       FETCH TZ4REDA-CRS                                                  
070000                                                                          
070100       INTO :REDA-TEOUTDATA                                               
070200                                                                          
070300     END-EXEC                                                             
070400                                                                          
070500     MOVE SQLCODE TO SQLCODE-WS                                           
070600     PERFORM DB2-STATUS-CHECK                                             
070700     .                                                                    
070800 DB2-CLOSE-TZ4REDA-CRS SECTION.                                           
070900                                                                          
071000     MOVE 'CLOSE-TZ4REDA-CRS'    TO WS-SECTION                            
071100     EXEC SQL                                                             
071200        CLOSE TZ4REDA-CRS                                                 
071300     END-EXEC                                                             
071400     .                                                                    
071500 DB2-STATUS-CHECK  SECTION.                                               
071600                                                                          
071700     SET SQLCODE-IX TO 1                                                  
071800     SEARCH GOOD-SQLCODE                                                  
071900       AT END                                                             
072000         SET RESULT-NOT-OK       TO TRUE                                  
072100         STRING WS-SECTION     DELIMITED BY SPACE                         
072200                'DB2 STATUS:'  DELIMITED BY SIZE                          
072300                SQLCODE-WS     DELIMITED BY SIZE                          
072400                               INTO WS-COMMENT                            
072500       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
072600     END-SEARCH                                                           
072700     .                                                                    
