000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5226200.                                                
000300 AUTHOR.         SARASWATHY S.                                            
000400 DATE-WRITTEN.   17/06/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*                                                                         
000900*        THE PROGRAM                                                      
001000*        - READS FILE W52261 -  VAT-DATA FROM IVW-TABLE                   
001100*        - READS FILE W52262 -  GET COUNTRY CODE                          
001200*        - SENDS FILE W52263 VAT DATA TO DAP                              
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000                                                                          
002100*          --- SELECTED VAT-DATA FROM IVW-TABLE                           
002200     SELECT W52261                     ASSIGN TO W52262D1.                
002300                                                                          
002400*          --- COUNTRY CODE FILE                                          
002500     SELECT W52262                     ASSIGN TO W52262D2.                
002600                                                                          
002700*          --- SELECTED VAT-DATA TO D&P                                   
002800     SELECT W52263                     ASSIGN TO W52262D3.                
002900     EJECT                                                                
003000                                                                          
003100 DATA DIVISION.                                                           
003200                                                                          
003300 FILE SECTION.                                                            
003400 FD  W52261                                                               
003500     RECORDING  F                                                         
003600     BLOCK CONTAINS 0.                                                    
003700 01  W52261-POST.                                                         
003800*    03  -COPY W522VAT  -L.                                               
003900                                                                          
004000 FD  W52262                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300 01  W52262-POST                 PIC X(80).                               
004400                                                                          
004500 FD  W52263                                                               
004600     RECORDING       V                                                    
004700     BLOCK CONTAINS  0.                                                   
004800 01  W52263-POST.                                                         
004900     03 W52263-001-DAP-RAD       PIC X(112).                              
005000                                                                          
005100 WORKING-STORAGE SECTION.                                                 
005200 77  IDPGM                       PIC X(8)    VALUE 'W5226200'.            
005300 77  YES                         PIC X       VALUE 'Y'.                   
005400 77  NOO                         PIC X       VALUE 'N'.                   
005410 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
005420 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
005500     EJECT                                                                
005600 01  W52263-CNTL-REC1.                                                    
005700                                                                          
005800     03  FILLER                  PIC X(15)   VALUE                        
005900                                 ' ¤DAPW52262-001'.                       
006000                                                                          
006100     03  FILLER                  PIC X(97)  VALUE SPACE.                  
006200                                                                          
006300     EJECT                                                                
006400                                                                          
006500 01  W52263-CNTL-REC2.                                                    
006600                                                                          
006700     03  FILLER                  PIC X(9)    VALUE                        
006800                                 ' ¤DAPW522'.                             
006900                                                                          
007000     03  W52263-IDLAND           PIC X(2)    VALUE SPACE.                 
007100                                                                          
007200     03  FILLER                  PIC X(101)  VALUE SPACE.                 
007300     EJECT                                                                
007400                                                                          
007500                                                                          
007600 01  SAVE-IDLAND                 PIC X(2)    VALUE SPACE.                 
007700                                                                          
007800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007900 01  FILLER REDEFINES DAGENS-DATUM.                                       
008000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008300     EJECT                                                                
008400                                                                          
008500 01  WS-YDATE                    PIC X(8)    VALUE SPACE.                 
008600 01  WS-YDATE-CCAAMMDD  REDEFINES WS-YDATE.                               
008700     03 WS-YDATE-CC              PIC X(2).                                
008800     03 WS-YDATE-AAMMDD.                                                  
008900        05 WS-YDATEAA            PIC X(2).                                
009000        05 WS-YDATEMMDD          PIC X(4).                                
009100                                                                          
009200 01  CALCULATE-AREA.                                                      
009300     03  WS-SUNTO-TOT            PIC S9(10)V9(2) COMP-3.                  
009400     03  WS-SUVAT-BILLIT-TOT     PIC S9(10)V9(2) COMP-3.                  
009500     03  WS-SEK                  PIC 9(11)V9(5) VALUE ZERO.               
009600     03  WS-LOC                  PIC 9(11)V9(5) VALUE ZERO.               
009700                                                                          
009800 77  W52261-EOF-SW               PIC X       VALUE 'N'.                   
009900     88  END-OF-W52261                       VALUE 'Y'.                   
010000     EJECT                                                                
010100                                                                          
010200 77  W52262-EOF-SW               PIC X       VALUE 'N'.                   
010300     88  END-OF-W52262                       VALUE 'Y'.                   
010400     EJECT                                                                
010500                                                                          
010600 01  ERRTEXT.                                                             
010700     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
010800     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
010900                                                                          
011200 01  FELTEXT                     PIC X(80).                               
011300                                                                          
011400 01  GENERAL-SUBPROGRAMS.                                                 
011500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
011900     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
011910     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
012000       EJECT                                                              
012100                                                                          
012200*    --- PARAMETERS TO ABEND                                              
012300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012600     EJECT                                                                
012700*                                                                         
012710*01  -COPY W510CURR                                                       
012720     EJECT                                                                
012800 01  FILLER                      PIC X(16)   VALUE 'IDLAND'.              
012900*01  -COPY WWLAND09                                                       
013000     EJECT                                                                
013100*                                                                         
013200*    -- SUBPROGRAM WZ20DAYS                                               
013300 01  FILLER                      PIC X(16)   VALUE 'WZ20DAYS'.            
013400*01 -COPY WZ20DAYS                                                        
013500     EJECT                                                                
013600*                                                                         
013700*    --- 522VAT FILE - INPUT                                              
013800 01  W52261-AREA-START           PIC X(24)   VALUE                        
013900                                             'W52261-AREA-START'.         
014000 01  W52261-AREA.                                                         
014100*    03  -COPY W522VAT     -PRE IN-                                       
014200     EJECT                                                                
014300*                                                                         
014400*    --- COUNTRY CODE FILE W52262                                         
014500 01  W52262-AREA-START           PIC X(24)    VALUE                       
014600                                             'W52262-AREA-START'.         
014700 01  W52262-AREA.                                                         
014800     03  W52262-IDLAND           PIC X(2).                                
014900     03  FILLER                  PIC X(78)    VALUE SPACES.               
015000     EJECT                                                                
015100*                                                                         
015200*    --- W52263 FILE - OUTPUT - DAP                                       
015300 01  W52263-AREA-START           PIC X(24)    VALUE                       
015400                                             'W52263-AREA-START'.         
015500 01  W52263-AREA.                                                         
015600     03  W52263-001-DAP-RAD      PIC X(112).                              
015700                                                                          
015800     EJECT                                                                
015900*                                                                         
016000 01  W52263-DATA.                                                         
016100*                                 VAT DATA FOR DISTRIBUTION               
016200     03 UT-TIAAAA                PIC 9(4)  VALUE ZEROS.                   
016300*                           YEAR  (YYYY)                                  
016400*                                                                         
016500     03 UT-TIMMDD                PIC 9(4)  VALUE ZEROS.                   
016600*                           MONTH/DAY (MMDD)                              
016700*                                                                         
016800     03 UT-SEMICOLON             PIC X     VALUE ';'.                     
016900*                           SEMICOLON                                     
017000*                                                                         
017100     03 UT-IDLANDX3-SEND         PIC X(3)  VALUE SPACES.                  
017200*                           COUNTRY CODE SENDING COUNTRY                  
017300*                                                                         
017400     03 UT-SEMICOLON             PIC X     VALUE ';'.                     
017500*                           SEMICOLON                                     
017600*                                                                         
017700     03 UT-IDVAT-SEND            PIC X(17) VALUE SPACES.                  
017800*                           VAT REGISTRATION NUMBER                       
017900*                                                                         
018000     03 UT-SEMICOLON             PIC X     VALUE ';'.                     
018100*                                 SEMICOLON                               
018200*                                                                         
018300     03 UT-IDLANDX3-BET          PIC X(3)  VALUE SPACES.                  
018400*                           COUNTRY CODE PAYING CUSTOMER ETC              
018500*                                                                         
018600     03 UT-SEMICOLON             PIC X     VALUE ';'.                     
018700*                           SEMICOLON                                     
018800*                                                                         
018900     03 UT-IDVAT-REC             PIC X(17) VALUE SPACES.                  
019000*                           VAT REGISTRATION NUMBER                       
019100*                                                                         
019200     03 UT-SEMICOLON             PIC X     VALUE ';'.                     
019300*                           SEMICOLON                                     
019400*                                                                         
019500     03 UT-IDDISTR               PIC 9(4)  VALUE ZEROS.                   
019600*                           DISTRICT NUMBER                               
019700*                                                                         
019800     03 UT-SEMICOLON             PIC X     VALUE ';'.                     
019900*                           SEMICOLON                                     
020000*                                                                         
020100     03 UT-IDKUNDNR              PIC 9(6)  VALUE ZEROS.                   
020200*                           CUSTOMER NO                                   
020300*                                                                         
020400     03 UT-SEMICOLON             PIC X     VALUE ';'.                     
020500*                           SEMICOLON                                     
020600*                                                                         
020700     03 UT-TIFAKT                PIC 9(6)  VALUE ZEROS.                   
020800*                           INVOICING DATE   (YYMMDD)                     
020900*                                                                         
021000     03 UT-SEMICOLON             PIC X     VALUE ';'.                     
021100*                           SEMICOLON                                     
021200*                                                                         
021300     03 UT-IDFAKT                PIC 9(7)  VALUE ZEROS.                   
021400*                           INVOICE NO.                                   
021500*                                                                         
021600     03 UT-SEMICOLON             PIC X     VALUE ';'.                     
021700*                           SEMICOLON                                     
021800*                                                                         
021900     03 UT-SUFKTTOT-LOC          PIC +9(10)V9(2) VALUE ZEROS.             
022000*                           TOTAL INVOICED AMOUNT                         
022100*                                                                         
022200     03 UT-SEMICOLON             PIC X     VALUE ';'.                     
022300*                           SEMICOLON                                     
022400*                                                                         
022500     03 UT-SUVAT-FAKT-LOC        PIC +9(10)V9(2) VALUE ZEROS.             
022600*                           TOTAL VAT VALUE PER INVOICE/CREDIT            
022700*                                                                         
022800     03 UT-SEMICOLON             PIC X     VALUE ';'.                     
022900*                                 SEMICOLON                               
023000*                                                                         
023100     03 FILLER                   PIC X(4).                                
023200 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
023300     SKIP2                                                                
023400*01  -COPY WDATKORT                                                       
023500     EJECT                                                                
023600*                                                                         
027600 LINKAGE SECTION.                                                         
027700                                                                          
027800*01  -COPY W0008  -PRE WDG2-                                              
027900     05  FILLER                  PIC X.                                   
028000                                                                          
028100 PROCEDURE DIVISION  USING WDG2-PCB.                                      
028200     ENTRY 'DLITCBL' USING WDG2-PCB.                                      
028300                                                                          
028400     PERFORM A-INIT                                                       
028500                                                                          
028600     PERFORM S10-READ-W52262                                              
028700     PERFORM UNTIL END-OF-W52262                                          
028800       PERFORM S11-READ-W52261                                            
028900       PERFORM UNTIL END-OF-W52261                                        
029000          IF IN-IDLANDX3-SEND NOT = W52262-IDLAND AND                     
029100             IN-IDLANDX3-BET  NOT = W52262-IDLAND                         
029200             CONTINUE                                                     
029300          ELSE                                                            
029400             PERFORM B-EXECUTE                                            
029500          END-IF                                                          
029600          PERFORM S11-READ-W52261                                         
029700       END-PERFORM                                                        
029800       PERFORM S10-READ-W52262                                            
029900       IF W52262-EOF-SW = 'N'                                             
030000        CLOSE W52261                                                      
030100        OPEN INPUT W52261                                                 
030200        MOVE NOO TO W52261-EOF-SW                                         
030300       END-IF                                                             
030400     END-PERFORM                                                          
030500                                                                          
030600     PERFORM Z-FINISH                                                     
030700     MOVE ZERO TO RETURN-CODE                                             
030800     GOBACK                                                               
030900     .                                                                    
031000     EJECT                                                                
031100                                                                          
031200 A-INIT SECTION.                                                          
031300                                                                          
031400     OPEN INPUT  W52261                                                   
031500                 W52262                                                   
031600     OPEN OUTPUT W52263                                                   
031700     MOVE FUNCTION  CURRENT-DATE(3:6)  TO DAGENS-DATUM                    
031800     MOVE FUNCTION  CURRENT-DATE(1:2)  TO WS-YDATE-CC                     
031900                                                                          
032000     MOVE DAGENS-DATUM TO DAYS-TIDATE1                                    
032100     MOVE 'YYMMDD'     TO DAYS-KDDATFMT1                                  
032200     MOVE 'YYMMDD'     TO DAYS-KDDATFMT2                                  
032300     MOVE SPACE        TO DAYS-TIDATE2                                    
032400                          DAYS-IDCALEND                                   
032500     MOVE -1           TO DAYS-KVDAYS                                     
032600                                                                          
032700     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
032800                                                                          
032900     MOVE DAYS-TIDATE2(1:6) TO WS-YDATE-AAMMDD                            
033000     .                                                                    
033100     EJECT                                                                
033200                                                                          
033300 B-EXECUTE SECTION.                                                       
033400     IF IN-KDFINDOC = 'ECO'                                               
033500       IF IN-IDLANDX3-SEND > SPACE                                        
033600         PERFORM BA-CREATE-W52263                                         
033700         PERFORM S12-WRITE-W52263                                         
033800       ELSE                                                               
033900         IF IN-IDLANDX3-BET > SPACE                                       
034000           PERFORM BA-CREATE-W52263                                       
034100           PERFORM S12-WRITE-W52263                                       
034200         END-IF                                                           
034300       END-IF                                                             
034400     END-IF                                                               
034500                                                                          
034600     IF IN-KDFINDOC NOT = 'ECO'                                           
034700       PERFORM BA-CREATE-W52263                                           
034800       PERFORM S12-WRITE-W52263                                           
034900     END-IF                                                               
035000     .                                                                    
035100     EJECT                                                                
035200                                                                          
035300 BA-CREATE-W52263 SECTION.                                                
035400     IF SAVE-IDLAND = SPACE OR                                            
035500        SAVE-IDLAND NOT = W52262-IDLAND                                   
035600          MOVE W52262-IDLAND    TO W52263-IDLAND                          
035700                                   SAVE-IDLAND                            
035800          PERFORM BAA-WRITE-W52263-001-DAP-CTL                            
035900     END-IF                                                               
036000     MOVE IN-IDLANDX3-SEND     TO UT-IDLANDX3-SEND                        
036100     MOVE IN-IDLANDX3-BET      TO UT-IDLANDX3-BET                         
036200     MOVE WS-YDATE-CC          TO UT-TIAAAA(1:2)                          
036300     MOVE IN-TIAA              TO UT-TIAAAA(3:2)                          
036400     MOVE WS-YDATEMMDD         TO UT-TIMMDD                               
036500     MOVE IN-IDVAT-BET         TO UT-IDVAT-REC                            
036600     MOVE IN-IDVAT-RESP        TO UT-IDVAT-SEND                           
036700     MOVE IN-IDDISTR           TO UT-IDDISTR                              
036800     MOVE IN-IDKUNDNR          TO UT-IDKUNDNR                             
036900     MOVE IN-IDFINDOC          TO UT-IDFAKT                               
037000     MOVE IN-DAFINDOC          TO UT-TIFAKT                               
037100                                                                          
037200     IF IN-KDFINDOC = 'CR' OR 'CR2'                                       
037300       COMPUTE WS-SUNTO-TOT = IN-SUNTO-TOT *                              
037400                              -1                                          
037500       END-COMPUTE                                                        
037600       COMPUTE WS-SUVAT-BILLIT-TOT = IN-SUVAT-BILLIT-TOT *                
037700                                     -1                                   
037800       END-COMPUTE                                                        
037900       MOVE WS-SUNTO-TOT         TO UT-SUFKTTOT-LOC                       
038000       MOVE WS-SUVAT-BILLIT-TOT  TO UT-SUVAT-FAKT-LOC                     
038100     ELSE                                                                 
038200       MOVE IN-SUNTO-TOT         TO UT-SUFKTTOT-LOC                       
038300       MOVE IN-SUVAT-BILLIT-TOT  TO UT-SUVAT-FAKT-LOC                     
038400     END-IF                                                               
038500                                                                          
038600     CALL DATKORT USING IDPGM  DATUMKORT-ID DATUMKORT                     
038700     MOVE D-AAR                       TO W-DATE-AAMM(1:2)                 
038710     MOVE D-MAANAD                    TO W-DATE-AAMM(3:2)                 
038720     MOVE W-DATE-AAMM                 TO CURR-TIAAMM                      
038730     MOVE WS-KDVALISO-HUV             TO CURR-KDVALISO-HUV                
038740     MOVE 'M'                         TO CURR-KDVALTYP                    
038800     IF W52262-IDLAND = 'IE' OR 'NL' OR 'FR' OR 'BE' OR 'DE' OR           
038900                        'IT' OR 'GR' OR 'ES' OR 'FI' OR 'PT' OR           
039000                        'AT'                                              
039100       MOVE 'EUR'        TO CURR-KDVALISO-ROW                             
039200       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
039300     ELSE                                                                 
039400       IF W52262-IDLAND  = 'GB'                                           
039500         MOVE 'GBP'      TO CURR-KDVALISO-ROW                             
039600         CALL W510CURR USING CURR-W510CURR WDG2-PCB                       
039700       ELSE                                                               
039800         IF W52262-IDLAND = 'DK'                                          
039900           MOVE 'DKK'      TO CURR-KDVALISO-ROW                           
040000           CALL W510CURR USING CURR-W510CURR WDG2-PCB                     
040100         ELSE                                                             
040200           IF W52262-IDLAND = 'NO'                                        
040300             MOVE 'NOK'      TO CURR-KDVALISO-ROW                         
040400             CALL W510CURR USING CURR-W510CURR WDG2-PCB                   
040500           ELSE                                                           
040600             STRING 'INVALID COUNTRY CODE:' W52262-IDLAND                 
040700             DELIMITED BY SIZE INTO ERRTEXT-STR                           
040800             CALL ABEND USING RKOD-ABEND-WITH-DUMP                        
040900           END-IF                                                         
041000         END-IF                                                           
041100       END-IF                                                             
041200     END-IF                                                               
041300                                                                          
041400     IF IN-KDVALISO = CURR-KDVALISO-ROW                                   
041500*    --- NO COLLECTION (DEALER-NET / DDI)                                 
041600       CONTINUE                                                           
041700     ELSE                                                                 
041800       MOVE UT-SUFKTTOT-LOC   TO WS-SUNTO-TOT                             
041900       MOVE UT-SUVAT-FAKT-LOC TO WS-SUVAT-BILLIT-TOT                      
042000       IF IN-KDVALISO NOT = 'SEK'                                         
042100*    --- CALCULATED AMOUNT LOCAL CURRENCY -> SEK                          
042200         COMPUTE WS-SEK ROUNDED = WS-SUNTO-TOT *                          
042300                                  IN-PRKURS                               
042400*    --- CALCULATED AMOUNTS SEK -> LOCAL CURRENCY                         
042500         COMPUTE WS-LOC ROUNDED = (WS-SEK / CURR-PRKURS-NEW)              
042600         MOVE WS-LOC           TO UT-SUFKTTOT-LOC                         
042700                                                                          
042800*    --- CALCULATED AMOUNT LOCAL CURRENCY -> SEK                          
042900         COMPUTE WS-SEK ROUNDED = WS-SUVAT-BILLIT-TOT *                   
043000                                  IN-PRKURS                               
043100*    --- CALCULATED AMOUNTS SEK -> LOCAL CURRENCY                         
043200         COMPUTE WS-LOC ROUNDED = (WS-SEK / CURR-PRKURS-NEW)              
043300         MOVE WS-LOC           TO UT-SUVAT-FAKT-LOC                       
043400       ELSE                                                               
043500*    --- CALCULATED AMOUNTS IN LOCAL CURRENCY                             
043600         COMPUTE WS-LOC ROUNDED = WS-SUNTO-TOT /                          
043700                                  CURR-PRKURS-NEW                         
043800         MOVE WS-LOC           TO UT-SUFKTTOT-LOC                         
043900                                                                          
044000*    --- CALCULATED AMOUNTS IN LOCAL CURRENCY                             
044100         COMPUTE WS-LOC ROUNDED = WS-SUVAT-BILLIT-TOT /                   
044200                                  CURR-PRKURS-NEW                         
044300         MOVE WS-LOC           TO UT-SUVAT-FAKT-LOC                       
044400       END-IF                                                             
044500     END-IF                                                               
044600     .                                                                    
044700     EJECT                                                                
044800                                                                          
044900 BAA-WRITE-W52263-001-DAP-CTL SECTION.                                    
045000     MOVE W52263-CNTL-REC1 TO W52263-AREA                                 
045100     WRITE W52263-POST FROM W52263-AREA                                   
045200     MOVE W52263-CNTL-REC2 TO W52263-AREA                                 
045300     WRITE W52263-POST FROM W52263-AREA                                   
045400     .                                                                    
045500     EJECT                                                                
045600                                                                          
045700 Z-FINISH SECTION.                                                        
045800     CLOSE W52261                                                         
045900           W52262                                                         
046000           W52263                                                         
046100     .                                                                    
046200     EJECT                                                                
046300                                                                          
046400 S10-READ-W52262  SECTION.                                                
046500     READ W52262 INTO W52262-AREA                                         
046600     AT END                                                               
046700        MOVE YES TO W52262-EOF-SW                                         
046800     END-READ                                                             
046900     .                                                                    
047000                                                                          
047100 S11-READ-W52261  SECTION.                                                
047200     READ W52261 INTO W52261-AREA                                         
047300     AT END                                                               
047400        MOVE YES TO W52261-EOF-SW                                         
047500     END-READ                                                             
047600     .                                                                    
047700                                                                          
047800 S12-WRITE-W52263 SECTION.                                                
047900     MOVE W52263-DATA TO W52263-AREA                                      
048000     WRITE W52263-POST FROM W52263-AREA                                   
048100     .                                                                    
048200     EJECT                                                                
048300                                                                          
