000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3718000.                                                
000300 AUTHOR.         BO HAMMARIN.                                             
000400 DATE-WRITTEN.   MAY 2000.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*       -PGM SKRIVER                                                      
000900*       .BORTTAGS-TRANSAKTIONER FÖR ORDERRADER-RENOVÖR                    
001000*       .LARM-FIL FÖR ORDERRADER-RENOVÖR (TILL MEMO)                      
001100*                                                                         
001200*       -PROGRAMMET LÄSER      WDR4 (WDGX3161/62)                         
001300*       -PROGRAMMET LÄSER      WDR1 (WDGX3155/56)                         
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900*    CHANGE LOG:                                                          
002000*                                                                         
002100*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
002200*      ----------------------------------------------------------         
002300*      14/11/18 - REDDY RAHUL     - CHANGE IN RULES FOR CREATING          
002400*                                   MEMO AND RENS FILES.                  
002500*                                   CHANGE TO FILE LAYOUTS.               
002600*                                   E'TRACKER 10193018                    
002700*                                                                         
002800                                                                          
002900 ENVIRONMENT DIVISION.                                                    
003000                                                                          
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400*          --- BORTTAGS-TRANSAKTIONER                                     
003500     SELECT W37180                     ASSIGN TO W37180D1.                
003600                                                                          
003700*          --- RAPPORTPOSTER MEMO                                         
003800     SELECT W37173                     ASSIGN TO W37180D2.                
003900                                                                          
004000*          --- HEADERPOSTER MEMO                                          
004100     SELECT W37174                     ASSIGN TO W37180D3.                
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400                                                                          
004500 FILE SECTION.                                                            
004600                                                                          
004700 FD  W37180                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000*01  POST -COPY W37180 -PRE RENS-   -L.                                   
005100                                                                          
005200 FD  W37173                                                               
005300     RECORDING       F                                                    
005400     BLOCK CONTAINS  0.                                                   
005500*01  POST -COPY W37173 -PRE LARM- -L.                                     
005600                                                                          
005700 FD  W37174                                                               
005800     RECORDING       F                                                    
005900     BLOCK CONTAINS  0.                                                   
006000 01  HEAD-POST                   PIC X(80).                               
006100     EJECT                                                                
006200                                                                          
006300 WORKING-STORAGE SECTION.                                                 
006400*    -- CHECKED BY WY2000                                                 
006500 77  IDPGM                       PIC X(8)    VALUE 'W3718000'.            
006600 77  JA                          PIC X       VALUE 'Y'.                   
006700 77  NEJ                         PIC X       VALUE 'N'.                   
006800 77  WS-TIAAVV-GRP               PIC 9(4)    VALUE ZERO.                  
006800 77  WS-DAAAVV                   PIC 9(6)    VALUE 200000.                
006900 77  WS-DAAAVV-RENS-LIMIT        PIC 9(6)    VALUE 200000.                
007000 77  WS-DAAAVV-LARM-LIMIT        PIC 9(6)    VALUE 200000.                
007100 77  WS-SPAR-IDHTYP              PIC X(4)    VALUE SPACE.                 
007200 77  WS-SPAR-IDDISTR             PIC S9(5)   VALUE ZERO COMP-3.           
007300 77  WS-SPAR-DAORDREG            PIC 9(8)    VALUE ZERO.                  
007400 77  WS-SPAR-IDORDER             PIC S9(7)   VALUE ZERO COMP-3.           
007500 77  WS-COUNT                    PIC S9(3)   VALUE ZERO COMP-3.           
007600                                                                          
007700 01  SW-WRITE-RENS               PIC X       VALUE ' '.                   
007800     88  WRITE-RENS-YES                      VALUE 'J'.                   
007900     88  WRITE-RENS-NO                       VALUE 'N'.                   
008000 01  SW-WRITE-LARM               PIC X       VALUE ' '.                   
008100     88  WRITE-LARM-YES                      VALUE 'J'.                   
008200     88  WRITE-LARM-NO                       VALUE 'N'.                   
008300                                                                          
008400 01  W-TITLE.                                                             
008500     03  FILLER                  PIC X(10) VALUE 'DISTRICT'.              
008600     03  FILLER                  PIC X(10) VALUE 'ORDERNO'.               
008700     03  FILLER                  PIC X(60) VALUE SPACE.                   
008800                                                                          
008900 01  FELTEXT                     PIC X(80).                               
009000     EJECT                                                                
009100                                                                          
009200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009300 01  FILLER REDEFINES DAGENS-DATUM.                                       
009400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009700 01  WS-DAORDREG.                                                         
009800     03  FILLER                  PIC X(2).                                
009900     03  WS-TIORDREG             PIC 9(6).                                
010000 01  WS-DAREGDAT.                                                         
010100     03  FILLER                  PIC X(2).                                
010200     03  WS-TIREGDAT             PIC 9(6).                                
010300                                                                          
010400 01  DAGENS-KLOCKA               PIC 9(8)    VALUE ZERO.                  
010500 01  WS-KLOCKA                   PIC 9(6)    VALUE ZERO.                  
010600     EJECT                                                                
010700                                                                          
010800 01  DYNAMISKA-SUBPROGRAM.                                                
010900*                                                                         
011000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
011100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
011400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011600                                                                          
011700*    --- PARAMETRAR TILL ABEND                                            
011800                                                                          
011900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
012100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
012200     EJECT                                                                
012300                                                                          
012400 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
012500*01  FILLER  -COPY WWDIS134   -RED TEST-IDDISTR.                          
012600     EJECT                                                                
012700                                                                          
012800*    --- PARAMETRAR TILL DATKORT                                          
012900*                                                                         
013000 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W37180'.              
013100 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
013200                                                                          
013300*01  -COPY WDATKORT                                                       
013400     EJECT                                                                
013500                                                                          
013600 01  FILLER                       PIC X(16)   VALUE 'WDATAREA'.           
013700*01  -COPY WDATAREA                                                       
013800     EJECT                                                                
013900                                                                          
014000*    --- PARAMETRAR TILL POSTSUM                                          
014100*                                                                         
014200*01  -COPY W0005   -PRE  POSTSUM-                                         
014300     EJECT                                                                
014400                                                                          
014500 01  RENS-AREA-START              PIC X(24)   VALUE                       
014600                                 'RENS-AREA-START  '.                     
014700                                                                          
014800*01  -COPY W37180          -PRE RENS-                                     
014900     EJECT                                                                
015000                                                                          
015100 01  LARM-AREA-START              PIC X(24)   VALUE                       
015200                                 'LARM-AREA-START '.                      
015300*01  AREA -COPY W37173     -PRE LARM-                                     
015400     EJECT                                                                
015500                                                                          
015600 01  HEAD-AREA-START              PIC X(24)   VALUE                       
015700                                 'HEAD-AREA-START '.                      
015800 01  HEAD-AREA.                                                           
015900     03  FILLER                   PIC X(80).                              
016000     EJECT                                                                
016100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016200*                                                                         
016300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016400                                                                          
016500 01  NYCKLAR-TILL-DLI.                                                    
016600     03  W-WDGX3161-MIN-X.                                                
016700         05  W-IDHTYP-3161-MIN   PIC  X(4)   VALUE '3161'.                
016800         05  W-IDDISTR-3161-MIN  PIC S9(5)   VALUE ZERO COMP-3.           
016900         05  FILLER              PIC  X(23)  VALUE LOW-VALUE.             
017000     03  W-WDGX3161-MAX-X.                                                
017100         05  W-IDHTYP-3161-MAX   PIC  X(4)   VALUE '3161'.                
017200         05  W-IDDISTR-3161-MAX  PIC S9(5)   VALUE +99999 COMP-3.         
017300         05  FILLER              PIC  X(23)  VALUE HIGH-VALUE.            
017400     03  W-WDGX3162-X.                                                    
017500         05  W-DAORDREG-3162     PIC  9(8)   VALUE ZERO.                  
017600         05  W-IDORDER-3162      PIC S9(7)   VALUE ZERO COMP-3.           
017700         05  W-IDARTNR-3162      PIC S9(9)   VALUE ZERO COMP-3.           
017800         05  W-IDRADNR-3162      PIC S9(5)   VALUE ZERO COMP-3.           
017900     03  W-WDGX3155-X.                                                    
018000         05  W-IDHTYP-3155       PIC X(4)    VALUE '3155'.                
018100         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
018200     03  W-WDGX3156-X.                                                    
018300         05  W-KDSEGKEY-3156     PIC X(1)    VALUE '1'.                   
018400     EJECT                                                                
018500                                                                          
018600*    --- STATUS-KOD FRÅN IMS                                              
018700 01  STATUS-WS                   PIC XX.                                  
018800     88  SEGMENT-FINNS                       VALUE '  '.                  
018900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
019100                                                                          
019200 01  GODK-STATUSKODER.                                                    
019300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019400                                                                          
019500 01  SSA1                        PIC X(96).                               
019600 01  SSA2                        PIC X(64).                               
019700     EJECT                                                                
019800                                                                          
019900*    --- IMS FUNKTIONSKODER                                               
020000*01  -COPY W0003                                                          
020100     EJECT                                                                
020200                                                                          
020300*    ---  DLI INPUT-OUTPUT AREA                                           
020400 01  FILLER         PIC X(16) VALUE 'DLI-IO-3161'.                        
020500 01  DLI-IO-WDGX3161.                                                     
020600*    03  -COPY WDGX3161                                                   
020700     EJECT                                                                
020800                                                                          
020900 01  FILLER         PIC X(16) VALUE 'DLI-IO-3162'.                        
021000 01  DLI-IO-WDGX3162.                                                     
021100*    03  -COPY WDGX3162                                                   
021200     EJECT                                                                
021300                                                                          
021400 01  FILLER         PIC X(16) VALUE 'DLI-IO-3156'.                        
021500 01  DLI-IO-WDGX3156.                                                     
021600*    03  -COPY WDGX3156                                                   
021700     EJECT                                                                
021800                                                                          
021900 LINKAGE SECTION.                                                         
022000*01  -COPY W0008   -PRE 3161-                                             
022100     05  FILLER                  PIC X.                                   
022200     EJECT                                                                
022300                                                                          
022400*01  -COPY W0008   -PRE 3155-                                             
022500     05  FILLER                  PIC X.                                   
022600     EJECT                                                                
022700                                                                          
022800 PROCEDURE DIVISION  USING 3161-PCB 3155-PCB.                             
022900 MAIN SECTION.                                                            
023000     ENTRY 'DLITCBL' USING 3161-PCB 3155-PCB.                             
023100                                                                          
023200     PERFORM A-INIT                                                       
023300                                                                          
023400     PERFORM B-BEARBETA                                                   
023500                                                                          
023600     PERFORM C-BYGG-HEADER                                                
023700                                                                          
023800     PERFORM Z-FINIT                                                      
023900                                                                          
024000     MOVE ZERO TO RETURN-CODE                                             
024100     GOBACK                                                               
024200     .                                                                    
024300     EJECT                                                                
024400                                                                          
024500 A-INIT SECTION.                                                          
024600     OPEN OUTPUT W37180                                                   
024700                 W37173                                                   
024800                 W37174                                                   
024900                                                                          
025000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
025100     MOVE D-AAR        TO DAGENS-DATUM-AAR                                
025200     MOVE D-MAANAD     TO DAGENS-DATUM-MAANAD                             
025300     MOVE D-DAG        TO DAGENS-DATUM-DAG                                
025400                                                                          
025500     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
025600                                                                          
025700     ACCEPT DAGENS-KLOCKA FROM TIME                                       
025800     COMPUTE WS-KLOCKA = DAGENS-KLOCKA / 100                              
025900                                                                          
026000     MOVE 'AAMMDD'                      TO DAT-KDDATFORM                  
026100     MOVE DAGENS-DATUM                  TO DAT-I-TIDATUM                  
026200                                                                          
026300     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
026400                         DAT-O-TIDATUM DAT-KDSVAR                         
026500                                                                          
026600     IF DAT-KDSVAR-OK                                                     
             MOVE DAT-TIAAVV-GRP TO WS-TIAAVV-GRP                               
026700       ADD WS-TIAAVV-GRP                TO   WS-DAAAVV-RENS-LIMIT         
026800                                             WS-DAAAVV-LARM-LIMIT         
026900       DISPLAY 'LARM-LIMIT='                 WS-DAAAVV-LARM-LIMIT         
027000     ELSE                                                                 
027100       DISPLAY 'FELAKTIGT DATUM'                                          
027200       CALL FELLOG                                                        
027300     END-IF                                                               
027400                                                                          
027500     WRITE LARM-POST FROM W-TITLE                                         
027600                                                                          
027700     PERFORM IMS-GU-WDGX3156                                              
027800     .                                                                    
027900     EJECT                                                                
028000                                                                          
028100 B-BEARBETA SECTION.                                                      
028200     PERFORM IMS-GN-WDR401                                                
028300                                                                          
028400     PERFORM UNTIL SEGMENT-SLUT OR                                        
028500                   SEGMENT-SAKNAS                                         
028600       IF 3161-IDHTYP = '3161'                                            
028700         PERFORM IMS-GNP-WDGX3162                                         
028800                                                                          
028900         PERFORM UNTIL SEGMENT-SAKNAS                                     
029000           IF 3161-IDDISTR  NOT = WS-SPAR-IDDISTR  OR                     
029100              3162-DAORDREG NOT = WS-SPAR-DAORDREG OR                     
029200              3162-IDORDER  NOT = WS-SPAR-IDORDER                         
029300                                                                          
029400             IF WRITE-RENS-YES                                            
029500               PERFORM BA-BYGG-W37180                                     
029600             END-IF                                                       
029700             IF WRITE-LARM-YES                                            
029800               PERFORM BB-BYGG-W37173                                     
029900             END-IF                                                       
030000                                                                          
030100             MOVE 3161-IDHTYP               TO WS-SPAR-IDHTYP             
030200             MOVE 3161-IDDISTR              TO WS-SPAR-IDDISTR            
030300             MOVE 3162-DAORDREG             TO WS-SPAR-DAORDREG           
030400             MOVE 3162-IDORDER              TO WS-SPAR-IDORDER            
030500                                                                          
030600             MOVE SPACE                     TO SW-WRITE-RENS              
030700                                               SW-WRITE-LARM              
030800           END-IF                                                         
030900*                                                                         
031000           IF 3162-DAORDREG NOT = ZERO                                    
031100             MOVE 3162-DAORDREG             TO WS-DAORDREG                
031200                                                                          
031300             MOVE 'AAMMDD'                  TO DAT-KDDATFORM              
031400             MOVE WS-TIORDREG               TO DAT-I-TIDATUM              
031500                                                                          
031600             CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM              
031700                                 DAT-O-TIDATUM DAT-KDSVAR                 
031800                                                                          
031900             IF NOT DAT-KDSVAR-OK                                         
032000               DISPLAY 'FELAKTIGT DATUM'                                  
032100               CALL FELLOG                                                
032200             ELSE                                                         
032300               MOVE 200000                  TO WS-DAAAVV                  
032400               ADD 1                        TO DAT-TIAA-VECKA             
                     MOVE DAT-TIAAVV-GRP          TO WS-TIAAVV-GRP              
032500               ADD WS-TIAAVV-GRP            TO WS-DAAAVV                  
032600               IF WS-DAAAVV < WS-DAAAVV-RENS-LIMIT                        
032700                 IF 3162-KVAVBART = 0 AND                                 
032800                    (3162-KVAVIS > 0 AND                                  
032900                    3162-DAREGDAT = 0) OR                                 
033000                    (3162-KVAVIS = 0 AND                                  
033100                    3162-DAREGDAT = 0)                                    
033200                   SET WRITE-RENS-NO        TO TRUE                       
033300                 END-IF                                                   
033400                 IF SW-WRITE-RENS NOT = NEJ                               
033500                   SET WRITE-RENS-YES       TO TRUE                       
033600                 END-IF                                                   
033700               ELSE                                                       
033800                 SET WRITE-RENS-NO          TO TRUE                       
033900               END-IF                                                     
034000             END-IF                                                       
034100           ELSE                                                           
034200             SET WRITE-RENS-NO              TO TRUE                       
034300           END-IF                                                         
034400*                                                                         
034500           IF 3162-DAREGDAT NOT = ZERO                                    
034600             MOVE 3162-DAREGDAT             TO WS-DAREGDAT                
031300             MOVE 'AAMMDD'                  TO DAT-KDDATFORM              
031400             MOVE WS-TIREGDAT               TO DAT-I-TIDATUM              
031500                                                                          
031600             CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM              
031700                                 DAT-O-TIDATUM DAT-KDSVAR                 
031800                                                                          
031900             IF NOT DAT-KDSVAR-OK                                         
032000               DISPLAY 'FELAKTIGT DATUM'                                  
032100               CALL FELLOG                                                
032200             ELSE                                                         
032300               MOVE 200000                  TO WS-DAAAVV                  
                     MOVE DAT-TIAAVV-GRP          TO WS-TIAAVV-GRP              
032500               ADD WS-TIAAVV-GRP            TO WS-DAAAVV                  
036500               IF WS-DAAAVV = WS-DAAAVV-LARM-LIMIT                        
036600*   ENDAST ORDER MED MINST EN AVVIKELSE LARMAS                            
036700                  IF 3162-KVAVIS NOT = 3162-KVAVBART                      
036800*   BB-BYGG... SKALL UTFÖRAS FÖR SAMTLIGA RENOVÖRER SOM                   
036900*   BÖRJAR ANVÄNDA WEB-BILDERNA 3178/3179                                 
037000                    MOVE 3161-IDDISTR         TO TEST-IDDISTR             
037100                    IF DIS134-BYTESREN-WEB                                
037200                      SET WRITE-LARM-YES      TO TRUE                     
037300                    END-IF                                                
037400                  END-IF                                                  
037400               END-IF                                                     
037500             END-IF                                                       
037700           END-IF                                                         
037800                                                                          
038300           PERFORM IMS-GNP-WDGX3162                                       
038400         END-PERFORM                                                      
038500       END-IF                                                             
038600                                                                          
038700       PERFORM IMS-GN-WDR401                                              
038800     END-PERFORM                                                          
038900                                                                          
039000     IF WRITE-RENS-YES                                                    
039100       PERFORM BA-BYGG-W37180                                             
039200     END-IF                                                               
039300     IF WRITE-LARM-YES                                                    
039400       PERFORM BB-BYGG-W37173                                             
039500     END-IF                                                               
039600     .                                                                    
039700     EJECT                                                                
039800                                                                          
039900 BA-BYGG-W37180 SECTION.                                                  
040000     MOVE WS-SPAR-IDHTYP             TO RENS-IDHTYP                       
040100     MOVE WS-SPAR-IDDISTR            TO RENS-IDDISTR                      
040200     MOVE WS-SPAR-DAORDREG           TO RENS-DAORDREG                     
040300     MOVE WS-SPAR-IDORDER            TO RENS-IDORDER                      
040500     PERFORM S11-SKRIV-W37180                                             
040600     .                                                                    
040700     EJECT                                                                
040800                                                                          
040900 BB-BYGG-W37173 SECTION.                                                  
041000     MOVE WS-SPAR-IDDISTR            TO LARM-IDDISTR                      
041100     MOVE WS-SPAR-IDORDER            TO LARM-IDORDER                      
041300     PERFORM S12-SKRIV-W37173                                             
041400     .                                                                    
041500     EJECT                                                                
041600                                                                          
041700 C-BYGG-HEADER SECTION.                                                   
041800     MOVE ')SEND'                    TO HEAD-AREA                         
041900     PERFORM S13-SKRIV-W37174                                             
042000                                                                          
042100     MOVE 'TITLE REMAN CONFIRMATION' TO HEAD-AREA                         
042200     PERFORM S13-SKRIV-W37174                                             
042300                                                                          
042400     MOVE 'OPTION FORCE'             TO HEAD-AREA                         
042500     PERFORM S13-SKRIV-W37174                                             
042600                                                                          
042700     MOVE 'LINESIZE 80'              TO HEAD-AREA                         
042800     PERFORM S13-SKRIV-W37174                                             
042900                                                                          
043000     STRING 'DEST ' 3156-IDMAIL                                           
043100            DELIMITED BY SIZE  INTO HEAD-AREA                             
043200     PERFORM S13-SKRIV-W37174                                             
043300                                                                          
043400     MOVE 'MEMO'                     TO HEAD-AREA                         
043500     PERFORM S13-SKRIV-W37174                                             
043600     .                                                                    
043700     EJECT                                                                
043800                                                                          
043900 Z-FINIT SECTION.                                                         
044000     CLOSE W37180                                                         
044100           W37173                                                         
044200           W37174                                                         
044300                                                                          
044400     MOVE 'S' TO POSTSUM-OPKOD                                            
044500     CALL POSTSUM USING POSTSUM-PARM                                      
044600     .                                                                    
044700     EJECT                                                                
044800                                                                          
044900 S11-SKRIV-W37180 SECTION.                                                
045000     WRITE RENS-POST   FROM RENS-W37180                                   
045100                                                                          
045200     MOVE 'RENS'     TO   POSTSUM-TRANSTYP                                
045300     MOVE 'W37180'   TO   POSTSUM-FDNAMN                                  
045400     MOVE 'W37180D1' TO   POSTSUM-DDNAMN2                                 
045500     CALL POSTSUM USING POSTSUM-PARM                                      
045600     .                                                                    
045700     EJECT                                                                
045800                                                                          
045900 S12-SKRIV-W37173 SECTION.                                                
046000     WRITE LARM-POST FROM LARM-AREA                                       
046100                                                                          
046200     MOVE 'LARM'     TO POSTSUM-TRANSTYP                                  
046300     MOVE 'W37173'   TO POSTSUM-FDNAMN                                    
046400     MOVE 'W37180D2' TO POSTSUM-DDNAMN2                                   
046500     CALL POSTSUM USING POSTSUM-PARM                                      
046600     .                                                                    
046700     EJECT                                                                
046800                                                                          
046900 S13-SKRIV-W37174 SECTION.                                                
047000     WRITE HEAD-POST   FROM HEAD-AREA                                     
047100                                                                          
047200     MOVE 'HEAD'     TO   POSTSUM-TRANSTYP                                
047300     MOVE 'W37174'   TO   POSTSUM-FDNAMN                                  
047400     MOVE 'W37180D3' TO   POSTSUM-DDNAMN2                                 
047500     CALL POSTSUM USING POSTSUM-PARM                                      
047600     .                                                                    
047700     EJECT                                                                
047800                                                                          
047900* --- IMS SEKTIONER ---                                                   
048000 IMS-GN-WDR401   SECTION.                                                 
048100     MOVE 'WDR401  '       TO SSA1                                        
048200     MOVE '  GEGB'         TO GODK-STATUSKODER                            
048300     CALL CBLTDLI USING GN 3161-PCB DLI-IO-WDGX3161 SSA1                  
048400     MOVE 3161-STATUS-CODE TO STATUS-WS                                   
048500     PERFORM IMS-STATUSKONTROLL                                           
048600     .                                                                    
048700                                                                          
048800 IMS-GNP-WDGX3162 SECTION.                                                
048900     MOVE 'WDGX3162'       TO SSA1                                        
049000     MOVE '  GE'           TO GODK-STATUSKODER                            
049100     CALL CBLTDLI USING GNP 3161-PCB DLI-IO-WDGX3162 SSA1                 
049200     MOVE 3161-STATUS-CODE TO STATUS-WS                                   
049300     PERFORM IMS-STATUSKONTROLL                                           
049400     .                                                                    
049500     EJECT                                                                
049600                                                                          
049700 IMS-GU-WDGX3156 SECTION.                                                 
049800     STRING 'WDR101  (WDGXKEY  =' W-WDGX3155-X ')'                        
049900            DELIMITED BY SIZE INTO SSA1                                   
050000     STRING 'WDGX3156(KDSEGKEY =' W-WDGX3156-X ')'                        
050100            DELIMITED BY SIZE INTO SSA2                                   
050200     MOVE '  '             TO GODK-STATUSKODER                            
050300     CALL CBLTDLI USING GU  3155-PCB DLI-IO-WDGX3156 SSA1 SSA2            
050400     MOVE 3155-STATUS-CODE TO STATUS-WS                                   
050500     PERFORM IMS-STATUSKONTROLL                                           
050600     .                                                                    
050700     EJECT                                                                
050800                                                                          
050900 IMS-STATUSKONTROLL SECTION.                                              
051000     SET STATUS-IX TO 1                                                   
051100     SEARCH GODK-STATUS                                                   
051200       AT END                                                             
051300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
051400           DELIMITED BY SIZE INTO FELTEXT                                 
051500         DISPLAY FELTEXT                                                  
051600         CALL FELLOG                                                      
051700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
051800         CONTINUE                                                         
051900     END-SEARCH                                                           
052000     .                                                                    
