000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.             W1122400.                                        
000400 AUTHOR.                 P.DAHLÖF.                                        
000500     DATE-WRITTEN.       AUGUSTI  1990.                                   
000600*                                                                         
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*    SKAPAR FIL MED INFO OM SATSARTIKLAR ICH DESS INGÅENDE ARTIKLA        
001100*    LÄSER WLSATB-BASEN OCH KOMPLETTERAR MED UPPGIFTER FRÅN               
001200*    ARTIKELREGISTRET.                                                    
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700     SKIP2                                                                
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000     SELECT  W11225                   ASSIGN TO    W11224D1.              
002100     EJECT                                                                
002110     SELECT  W11225X                  ASSIGN TO    W11224D2.              
002120     EJECT                                                                
002200     SELECT  W11226                   ASSIGN TO    W11224D3.              
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP2                                                                
002600 FILE SECTION.                                                            
002700     SKIP2                                                                
002800 FD  W11225                                                               
002900     LABEL RECORD STANDARD                                                
003000     RECORDING      F                                                     
003100     BLOCK CONTAINS 0.                                                    
003200     SKIP2                                                                
003300*01  POST -COPY W11225 -PRE UT- -L                                        
003400     EJECT                                                                
003410 FD  W11225X                                                              
003420     LABEL RECORD STANDARD                                                
003430     RECORDING      F                                                     
003440     BLOCK CONTAINS 0.                                                    
003450     SKIP2                                                                
003460*01  POST -COPY W11225X -PRE UTX- -L                                      
003470     EJECT                                                                
003500 FD  W11226                                                               
003600     LABEL RECORD STANDARD                                                
003700     RECORDING      F                                                     
003800     BLOCK CONTAINS 0.                                                    
003900     SKIP2                                                                
004000*01  POST -COPY W11226 -PRE UT2- -L                                       
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004400*    -COPY WY2000W1                                                       
004500     SKIP3                                                                
004600 77  PROGRAM-NAMN                PIC X(8) VALUE 'W1122400'.               
004700     SKIP2                                                                
004800*    ---- GENERELLA KONSTANTER ---------------------------------          
004900 77  IX                          PIC S9(9) VALUE +0 COMP SYNC.            
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200 77  WS-DAGENS-AAMMDD            PIC 9(7)    VALUE ZERO.                  
005300 77  WS-IDARTNR-STR              PIC S9(9) VALUE +0 COMP-3.               
005400 01  NYCKLAR-TILL-DLI.                                                    
005500   03  FILLER                    PIC X(16)   VALUE                        
005600                                            'NYCKLAR-TILL-DLI'.           
005700   03  W-IDARTNR-X.                                                       
005800     05  W-IDARTNR               PIC S9(9)   COMP-3 VALUE ZERO.           
005900   03  W-IDLEVNR-X.                                                       
006000     05  W-IDLEVNR               PIC X(5)    VALUE SPACE.                 
006100   03  W-WDM301KY-MIN.                                                    
006200     05  FILLER                  PIC  X(16)  VALUE LOW-VALUE.             
006300   03  W-WDM301KY-MAX.                                                    
006400     05  FILLER                  PIC  X(16)  VALUE HIGH-VALUE.            
006500   03  W-KDSEGKEY-X.                                                      
006600     05  W-KDSEGKEY              PIC  X(1)   VALUE '1'.                   
006700   03  W-IDFKNGRP-X.                                                      
006800     05  FILLER                  PIC  X(6)   VALUE LOW-VALUE.             
006900*--------------------------------------------------------------*          
007000*    SUBPROGRAM OCH PARAMETERAREOR                                        
007100*--------------------------------------------------------------*          
007200     SKIP2                                                                
007300 01  DYNAMISKA-SUBPROGRAM.                                                
007400   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
007500   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
007600   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
007700     EJECT                                                                
007800*    ---- PARAMETRAR TILL POSTSUM                                         
007900 01  FILLER               PIC X(16)   VALUE  'POSTSUM-AREA'.              
008000*01  -COPY W0005 -PRE POSTSUM-.                                           
008100     EJECT                                                                
008200 01  FILLER             PIC X(16)       VALUE 'UT-W11225-AREA'.           
008300*01  AREA -COPY W11225 -PRE UT-                                           
008400     EJECT                                                                
008401 01  FILLER             PIC X(16)       VALUE 'UTX-W11225X-AREA'.         
008402*01  AREA -COPY W11225X -PRE UTX-                                         
008403     EJECT                                                                
008410 01  FILLER             PIC X(16)       VALUE 'UT2-W11626-AREA'.          
008420*01  AREA -COPY W11226 -PRE UT2-                                          
008430     EJECT                                                                
008500 01  IMS-WS.                                                              
008600   03  FILLER                    PIC X(8)   VALUE 'IMS-WS'.               
008700   03  STATUS-WS                 PIC X(2).                                
008800     88  SEGMENT-FINNS                      VALUE '  '.                   
008900     88  SEGMENT-SAKNAS                     VALUE 'GE' 'GB'.              
009000   03  SSA1                      PIC X(64).                               
009100   03  SSA2                      PIC X(64).                               
009200   03  GODK-STATUSKODER.                                                  
009300     05  GODK-STATUS     OCCURS 5 INDEXED BY STATUS-IX PIC XX.            
009400*01  -COPY W0003                                                          
009500     EJECT                                                                
009600 01  DLI-IO-AREA.                                                         
009700   03 IO-AREA                    PIC X(900)   VALUE SPACE.                
009800     SKIP2                                                                
009900*  03  WLSATB01 -COPY WDJ101 -RED IO-AREA -PRE SATB01-.                   
010000     EJECT                                                                
010100*  03  WLSATB11 -COPY WDJ111 -RED IO-AREA -PRE SATB11-.                   
010200     EJECT                                                                
010300*  03  WLBENA11 -COPY WDD311 -RED IO-AREA -PRE BENA11-.                   
010400     EJECT                                                                
010500*  03  WLARTC01 -COPY WDK601 -RED IO-AREA                                 
010600     EJECT                                                                
010700*  03  WLARTC11 -COPY WDK611 -RED IO-AREA                                 
010800     EJECT                                                                
010900*  03  WDF502   -COPY WDF502 -RED IO-AREA                                 
011000     EJECT                                                                
011100 LINKAGE SECTION.                                                         
011200*01      -COPY W0008     -PRE ARTC-                                       
011300      05 FILLER          PIC X(4).                                        
011400     SKIP2                                                                
011500*01      -COPY W0008     -PRE BENA-                                       
011600      05 FILLER          PIC X(4).                                        
011700     SKIP2                                                                
011800*01      -COPY W0008     -PRE SATB-                                       
011900      05 FILLER          PIC X(4).                                        
012000     SKIP2                                                                
012100*01      -COPY W0008     -PRE WDF5-                                       
012200      05 FILLER          PIC X(4).                                        
012300     EJECT                                                                
012400 PROCEDURE DIVISION  USING ARTC-PCB BENA-PCB SATB-PCB WDF5-PCB.           
012500     ENTRY 'DLITCBL' USING ARTC-PCB BENA-PCB SATB-PCB WDF5-PCB.           
012600     SKIP2                                                                
012700     PERFORM A-INIT                                                       
012800     PERFORM IMS-GN-SATB01                                                
012900     PERFORM UNTIL SEGMENT-SAKNAS                                         
013000        IF  SATB01-STR-TIBORT = ZERO                                      
013100        AND SATB01-STR-IDARTNR < +100000000                               
013200           PERFORM S02-NOLLSTAELL                                         
013300           MOVE SATB01-STR-IDARTNR       TO WS-IDARTNR-STR                
013400           PERFORM B-MOVE-ROTINFO-TO-UTFIL                                
013500           PERFORM C-MOVE-ARTC-BENA-TO-UTFIL                              
013600           PERFORM S01-SKRIV-W11225                                       
013601           PERFORM S03-SKRIV-W11225X                                      
013610           PERFORM S04-SKRIV-W11226                                       
013700           PERFORM IMS-GNP-SATB11                                         
013800           PERFORM UNTIL SEGMENT-SAKNAS                                   
013900              MOVE SATB11-RAD-TISTODAT   TO TMP1-YYMMDD                   
014000              MOVE WS-DAGENS-AAMMDD      TO TMP2-YYMMDD                   
014100              MOVE SATB11-RAD-TISTADAT   TO TMP3-YYMMDD                   
014200              PERFORM WY2000Q1                                            
014300              IF  TMP1-YYMMDD > TMP2-YYMMDD                               
014400              AND TMP3-YYMMDD NOT > TMP2-YYMMDD                           
014500                 PERFORM S02-NOLLSTAELL                                   
014600                 MOVE WS-IDARTNR-STR     TO UT-IDARTNR-STR                
014610                                            UT2-IDARTNR-STR               
014700                 PERFORM D-MOVE-11INFO-TO-UTFIL                           
014800                 PERFORM C-MOVE-ARTC-BENA-TO-UTFIL                        
014900                 PERFORM S01-SKRIV-W11225                                 
014901                 PERFORM S03-SKRIV-W11225X                                
014910                 PERFORM S04-SKRIV-W11226                                 
015000              END-IF                                                      
015100              PERFORM IMS-GNP-SATB11                                      
015200           END-PERFORM                                                    
015300        END-IF                                                            
015400        PERFORM IMS-GN-SATB01                                             
015500     END-PERFORM                                                          
015600     PERFORM Z-FINIT                                                      
015700     MOVE ZERO                           TO RETURN-CODE                   
015800     GOBACK                                                               
015900     .                                                                    
016000     EJECT                                                                
016100 A-INIT   SECTION.                                                        
016200     SKIP2                                                                
016300     OPEN OUTPUT W11225                                                   
016310                 W11225X                                                  
016320                 W11226                                                   
016400     MOVE PROGRAM-NAMN                TO POSTSUM-PROGNAMN                 
016500     ACCEPT WS-DAGENS-AAMMDD FROM DATE                                    
016600     .                                                                    
016700     EJECT                                                                
016800 B-MOVE-ROTINFO-TO-UTFIL  SECTION.                                        
016900     SKIP2                                                                
017000     MOVE '001'                       TO UT-IDPTYP                        
017010                                         UT2-IDPTYP                       
017100     MOVE SATB01-STR-IDARTNR          TO UT-IDARTNR-STR                   
017110                                         UT2-IDARTNR-STR                  
017200                                         W-IDARTNR                        
017300     MOVE 'S  '                       TO UT-IDSKYLT (8)                   
017400     MOVE SATB01-STR-BEART-SVE        TO UT-BEART   (8)                   
017500     MOVE SATB01-STR-IDFKNGRP         TO UT-IDFKNGRP                      
017600     MOVE SATB01-STR-IDSTRTYP         TO UT-IDSTRTYP                      
017700     MOVE SATB01-STR-IDTSPEC          TO UT-IDTSPEC                       
017800     MOVE SATB01-STR-KDPRODSL         TO UT-KDPRODSL                      
017900     MOVE SATB01-STR-TIREGDAT         TO UT-TIREGDAT                      
018000     .                                                                    
018100     EJECT                                                                
018200 C-MOVE-ARTC-BENA-TO-UTFIL  SECTION.                                      
018300     SKIP2                                                                
018400     PERFORM IMS-GU-ARTC01                                                
018500     IF SEGMENT-FINNS                                                     
018600       MOVE ART-KDERS-UTG               TO UT-KDERS                       
018700       MOVE ART-TIREGDAT                TO UT-TIREGDAT                    
018800       MOVE ART-IDAO (1)                TO UT-IDAO                        
018900       MOVE ART-IDLEVNR                 TO UT-IDLEVNR                     
019000       MOVE ART-IDLEVNR                 TO W-IDLEVNR                      
019100       MOVE ART-KDPRODSL                TO UT-KDPRODSL                    
019200       MOVE ART-IDFKNGRP                TO UT-IDFKNGRP                    
019300       MOVE ART-KDSORT                  TO UT-KDSORT                      
019400                                                                          
019500       PERFORM IMS-GNP-ARTC11                                             
019600       IF SEGMENT-FINNS                                                   
019700          MOVE CLAG-PRARTBTO-EXP        TO UT-PRARTBTO                    
019800          MOVE CLAG-PRARTSJK            TO UT-PRARTSJK                    
019900          MOVE CLAG-VKART               TO UT-VKART                       
020000          MOVE CLAG-KDERS               TO UT-KDERS                       
020010          MOVE CLAG-PRARTSTD            TO UT2-PRARTSTD                   
020100          IF UT-IDPTYP = '001'                                            
020200             MOVE CLAG-IDBERED          TO UT-IDBERED                     
020210             MOVE CLAG-KDARTURS         TO UT2-KDARTURS-STR               
020220          ELSE                                                            
020221             MOVE CLAG-KDARTURS         TO UT2-KDARTURS-ING               
020300          END-IF                                                          
020330                                                                          
020400       END-IF                                                             
020500       PERFORM CA-LAES-WLBENA-BSEQ                                        
020600       IF UT-IDPTYP = '001'                                               
020700          PERFORM IMS-GU-WDF502                                           
020800          IF SEGMENT-FINNS                                                
020900             MOVE XLEV-BELEVART         TO UT-BELEV                       
021000          ELSE                                                            
021100             MOVE SPACE                 TO UT-BELEV                       
021200          END-IF                                                          
021300       END-IF                                                             
021400     END-IF                                                               
021500     .                                                                    
021600     EJECT                                                                
021700 CA-LAES-WLBENA-BSEQ      SECTION.                                        
021800     SKIP2                                                                
021900     PERFORM IMS-GU-BENA01-BSEQ                                           
022000     IF SEGMENT-FINNS                                                     
022100        PERFORM IMS-GNP-BENA11                                            
022200        PERFORM UNTIL SEGMENT-SAKNAS                                      
022300           PERFORM CAA-TILLDELA-BENAEMNING                                
022400           PERFORM IMS-GNP-BENA11                                         
022500        END-PERFORM                                                       
022600     END-IF                                                               
022700     .                                                                    
022800     EJECT                                                                
022900 CAA-TILLDELA-BENAEMNING SECTION.                                         
023000     SKIP2                                                                
023100     IF BENA11-TEXT-IDSKYLT = 'D  '                                       
023200        MOVE +1                                            TO IX          
023300        MOVE BENA11-TEXT-IDSKYLT                           TO             
023400                                                   UT-IDSKYLT (IX)        
023500        MOVE BENA11-TEXT-BEART                             TO             
023600                                                   UT-BEART   (IX)        
023700     ELSE                                                                 
023800       IF BENA11-TEXT-IDSKYLT = 'E  '                                     
023900           MOVE +2                                         TO IX          
024000           MOVE BENA11-TEXT-IDSKYLT                        TO             
024100                                                   UT-IDSKYLT (IX)        
024200           MOVE BENA11-TEXT-BEART                          TO             
024300                                                   UT-BEART   (IX)        
024400       ELSE                                                               
024500          IF BENA11-TEXT-IDSKYLT = 'F  '                                  
024600              MOVE +3                                      TO IX          
024700              MOVE BENA11-TEXT-IDSKYLT                     TO             
024800                                                   UT-IDSKYLT (IX)        
024900              MOVE BENA11-TEXT-BEART                       TO             
025000                                                   UT-BEART   (IX)        
025100          ELSE                                                            
025200             IF BENA11-TEXT-IDSKYLT = 'GB '                               
025300                 MOVE +4                                   TO IX          
025400                 MOVE BENA11-TEXT-IDSKYLT                  TO             
025500                                                   UT-IDSKYLT (IX)        
025600                 MOVE BENA11-TEXT-BEART                    TO             
025700                                                   UT-BEART   (IX)        
025800             ELSE                                                         
025900                IF BENA11-TEXT-IDSKYLT = 'I  '                            
026000                    MOVE +5                                TO IX          
026100                    MOVE BENA11-TEXT-IDSKYLT               TO             
026200                                                   UT-IDSKYLT (IX)        
026300                    MOVE BENA11-TEXT-BEART                 TO             
026400                                                   UT-BEART   (IX)        
026500                ELSE                                                      
026600                   IF BENA11-TEXT-IDSKYLT = 'NL '                         
026700                       MOVE +6                             TO IX          
026800                       MOVE BENA11-TEXT-IDSKYLT            TO             
026900                                                   UT-IDSKYLT (IX)        
027000                       MOVE BENA11-TEXT-BEART              TO             
027100                                                   UT-BEART   (IX)        
027200                   ELSE                                                   
027300                     IF BENA11-TEXT-IDSKYLT = 'P  '                       
027400                         MOVE +7                           TO IX          
027500                         MOVE BENA11-TEXT-IDSKYLT          TO             
027600                                                   UT-IDSKYLT (IX)        
027700                         MOVE BENA11-TEXT-BEART            TO             
027800                                                   UT-BEART   (IX)        
027900                     ELSE                                                 
028000                       IF BENA11-TEXT-IDSKYLT = 'S  '                     
028100                           MOVE +8                         TO IX          
028200                           MOVE BENA11-TEXT-IDSKYLT        TO             
028300                                                   UT-IDSKYLT (IX)        
028400                           MOVE BENA11-TEXT-BEART          TO             
028500                                                   UT-BEART   (IX)        
028600                       ELSE                                               
028700                         IF BENA11-TEXT-IDSKYLT = 'SF '                   
028800                             MOVE +9                       TO IX          
028900                             MOVE BENA11-TEXT-IDSKYLT      TO             
029000                                                   UT-IDSKYLT (IX)        
029100                             MOVE BENA11-TEXT-BEART        TO             
029200                                                   UT-BEART   (IX)        
029300                         ELSE                                             
029400                           IF BENA11-TEXT-IDSKYLT = 'USA'                 
029500                               MOVE +10                    TO IX          
029600                               MOVE BENA11-TEXT-IDSKYLT    TO             
029700                                                   UT-IDSKYLT (IX)        
029800                               MOVE BENA11-TEXT-BEART      TO             
029900                                                   UT-BEART   (IX)        
030000                           END-IF                                         
030100                         END-IF                                           
030200                       END-IF                                             
030300                     END-IF                                               
030400                   END-IF                                                 
030500                END-IF                                                    
030600             END-IF                                                       
030700          END-IF                                                          
030800       END-IF                                                             
031060     END-IF                                                               
031070     .                                                                    
031100     EJECT                                                                
031200 D-MOVE-11INFO-TO-UTFIL  SECTION.                                         
031300     SKIP2                                                                
031400     MOVE '002'                       TO UT-IDPTYP                        
031410                                         UT2-IDPTYP                       
031500     MOVE SATB11-RAD-IDARTNR          TO UT-IDARTNR-ING                   
031510                                         UT2-IDARTNR-ING                  
031600                                         W-IDARTNR                        
031700     MOVE 'S  '                       TO UT-IDSKYLT (8)                   
031800     MOVE SATB11-RAD-BEART-SVE        TO UT-BEART   (8)                   
031900     MOVE SATB11-RAD-BELEVART         TO UT-BELEVART                      
032000     MOVE SATB11-RAD-IDLEVNR          TO UT-IDLEVNR                       
032100     MOVE SATB11-RAD-KDSORT           TO UT-KDSORT                        
032200     MOVE SATB11-RAD-REANTPSA         TO UT-REANTPSA                      
032210                                         UT2-REANTPSA                     
032300     .                                                                    
032400     EJECT                                                                
032500 S01-SKRIV-W11225        SECTION.                                         
032600     SKIP2                                                                
032700     WRITE UT-POST FROM UT-AREA                                           
032800     MOVE SPACE TO POSTSUM-TRANSTYP                                       
032900     MOVE 'W11225' TO POSTSUM-FDNAMN                                      
033000     MOVE 'W11224D1' TO POSTSUM-DDNAMN2                                   
033100     CALL POSTSUM USING POSTSUM-PARM                                      
033200     .                                                                    
033300     EJECT                                                                
033310 S03-SKRIV-W11225X       SECTION.                                         
033320     SKIP2                                                                
033321     MOVE UT-IDPTYP        TO UTX-IDPTYP                                  
033322     MOVE UT-IDARTNR-STR   TO UTX-IDARTNR-STR                             
033323     MOVE UT-IDARTNR-ING   TO UTX-IDARTNR-ING                             
033324     MOVE UT-IDTSPEC       TO UTX-IDTSPEC                                 
033325     MOVE UT-IDSTRTYP      TO UTX-IDSTRTYP                                
033326     MOVE UT-KDPRODSL      TO UTX-KDPRODSL                                
033327     MOVE UT-IDFKNGRP      TO UTX-IDFKNGRP                                
033328     MOVE UT-TIREGDAT      TO UTX-TIREGDAT                                
033329     MOVE UT-KDERS         TO UTX-KDERS                                   
033330     MOVE UT-IDAO          TO UTX-IDAO                                    
033331     MOVE UT-IDLEVNR       TO UTX-IDLEVNR                                 
033332     MOVE UT-IDBERED       TO UTX-IDBERED                                 
033333     MOVE UT-BELEV         TO UTX-BELEV                                   
033334     MOVE UT-PRARTBTO      TO UTX-PRARTBTO                                
033335     MOVE UT-PRARTSJK      TO UTX-PRARTSJK                                
033336     MOVE UT-VKART         TO UTX-VKART                                   
033337     MOVE UT-REANTPSA      TO UTX-REANTPSA                                
033338     MOVE UT-BELEVART      TO UTX-BELEVART                                
033339     MOVE UT-KDSORT        TO UTX-KDSORT                                  
033345                                                                          
033346     WRITE UTX-POST FROM UTX-AREA                                         
033347     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
033350     MOVE 'W11225X'  TO POSTSUM-FDNAMN                                    
033360     MOVE 'W11224D2' TO POSTSUM-DDNAMN2                                   
033370     CALL POSTSUM USING POSTSUM-PARM                                      
033380     .                                                                    
033390     EJECT                                                                
033400 S04-SKRIV-W11226        SECTION.                                         
033410     SKIP2                                                                
033420     WRITE UT2-POST FROM UT2-AREA                                         
033430     MOVE SPACE TO POSTSUM-TRANSTYP                                       
033440     MOVE 'W11226' TO POSTSUM-FDNAMN                                      
033450     MOVE 'W11224D3' TO POSTSUM-DDNAMN2                                   
033460     CALL POSTSUM USING POSTSUM-PARM                                      
033470     .                                                                    
033480     EJECT                                                                
033500 S02-NOLLSTAELL           SECTION.                                        
033600     SKIP2                                                                
033700     MOVE ZERO                        TO UT-IDARTNR-STR                   
033800                                         UT2-IDARTNR-STR                  
033810                                         UT-IDARTNR-ING                   
033820                                         UT2-IDARTNR-ING                  
033900                                         UT-KDPRODSL                      
034000                                         UT-IDFKNGRP                      
034100                                         UT-TIREGDAT                      
034200                                         UT-KDERS                         
034300                                         UT-IDBERED                       
034400                                         UT-PRARTBTO                      
034500                                         UT-PRARTSJK                      
034510                                         UT2-PRARTSTD                     
034600                                         UT-VKART                         
034700                                         UT-REANTPSA                      
034710                                         UT2-REANTPSA                     
034800     MOVE SPACE                       TO UT-IDPTYP                        
034810                                         UT2-IDPTYP                       
034900                                         UT-IDLEVNR                       
035000                                         UT-IDTSPEC                       
035100                                         UT-IDSTRTYP                      
035200                                         UT-IDAO                          
035300                                         UT-BELEVART                      
035400                                         UT-KDSORT                        
035500                                         UT-IDSKYLT (1)                   
035600                                         UT-IDSKYLT (2)                   
035700                                         UT-IDSKYLT (3)                   
035800                                         UT-IDSKYLT (4)                   
035900                                         UT-IDSKYLT (5)                   
036000                                         UT-IDSKYLT (6)                   
036100                                         UT-IDSKYLT (7)                   
036200                                         UT-IDSKYLT (8)                   
036300                                         UT-IDSKYLT (9)                   
036400                                         UT-IDSKYLT (10)                  
036500                                         UT-BEART (1)                     
036600                                         UT-BEART (2)                     
036700                                         UT-BEART (3)                     
036800                                         UT-BEART (4)                     
036900                                         UT-BEART (5)                     
037000                                         UT-BEART (6)                     
037100                                         UT-BEART (7)                     
037200                                         UT-BEART (8)                     
037300                                         UT-BEART (9)                     
037400                                         UT-BEART (10)                    
037410                                         UT2-KDARTURS-STR                 
037420                                         UT2-KDARTURS-ING                 
037500     .                                                                    
037600     EJECT                                                                
037700 Z-FINIT SECTION.                                                         
037800     SKIP2                                                                
037900     CLOSE   W11225                                                       
037910             W11225X                                                      
037920             W11226                                                       
038000     MOVE 'S' TO POSTSUM-OPKOD                                            
038100     CALL POSTSUM USING POSTSUM-PARM                                      
038200     .                                                                    
038300     EJECT                                                                
038400 IMS-GU-ARTC01 SECTION.                                                   
038500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
038600            DELIMITED BY SIZE INTO SSA1                                   
038700     MOVE '  GE'                 TO GODK-STATUSKODER                      
038800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA    SSA1                   
038900     MOVE ARTC-STATUS-CODE       TO STATUS-WS                             
039000     PERFORM IMS-STATUSKONTROLL                                           
039100     .                                                                    
039200     SKIP3                                                                
039300 IMS-GNP-ARTC11 SECTION.                                                  
039400     MOVE 'WLARTC11'             TO SSA1                                  
039500     MOVE '  GE'                 TO GODK-STATUSKODER                      
039600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
039700     MOVE ARTC-STATUS-CODE       TO STATUS-WS                             
039800     PERFORM IMS-STATUSKONTROLL                                           
039900     .                                                                    
040000     SKIP3                                                                
040100 IMS-GU-BENA01-BSEQ SECTION.                                              
040200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
040300            DELIMITED BY SIZE INTO SSA1                                   
040400     MOVE '  GE'                TO GODK-STATUSKODER                       
040500     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1                      
040600     MOVE BENA-STATUS-CODE      TO STATUS-WS                              
040700     PERFORM IMS-STATUSKONTROLL                                           
040800     .                                                                    
040900     SKIP3                                                                
041000 IMS-GNP-BENA11 SECTION.                                                  
041100     MOVE 'WLBENA11 '            TO SSA1                                  
041200     MOVE '  GE'                 TO GODK-STATUSKODER                      
041300     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
041400     MOVE BENA-STATUS-CODE       TO STATUS-WS                             
041500     PERFORM IMS-STATUSKONTROLL                                           
041600     .                                                                    
041700     SKIP3                                                                
041800 IMS-GU-WDF502 SECTION.                                                   
041900     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
042000            DELIMITED BY SIZE INTO SSA1                                   
042100     STRING 'WDF502  (IDLEVNR  =' W-IDLEVNR-X ')'                         
042200            DELIMITED BY SIZE INTO SSA2                                   
042300     MOVE '  GE'                 TO GODK-STATUSKODER                      
042400     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-AREA SSA1 SSA2                 
042500     MOVE WDF5-STATUS-CODE       TO STATUS-WS                             
042600     PERFORM IMS-STATUSKONTROLL                                           
042700     .                                                                    
042800     SKIP3                                                                
042900 IMS-GN-SATB01 SECTION.                                                   
043000     MOVE   'WLSATB01 '           TO SSA1                                 
043100     MOVE '  GB'                 TO GODK-STATUSKODER                      
043200     CALL CBLTDLI USING GN SATB-PCB DLI-IO-AREA    SSA1                   
043300     MOVE SATB-STATUS-CODE       TO STATUS-WS                             
043400     PERFORM IMS-STATUSKONTROLL                                           
043500     .                                                                    
043600     SKIP3                                                                
043700 IMS-GNP-SATB11 SECTION.                                                  
043800     MOVE 'WLSATB11'             TO SSA1                                  
043900     MOVE '  GE'                 TO GODK-STATUSKODER                      
044000     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1                     
044100     MOVE SATB-STATUS-CODE       TO STATUS-WS                             
044200     PERFORM IMS-STATUSKONTROLL                                           
044300     .                                                                    
044400     SKIP3                                                                
044500 IMS-STATUSKONTROLL SECTION.                                              
044600     SKIP1                                                                
044700     SET STATUS-IX TO 1                                                   
044800     SEARCH GODK-STATUS AT END CALL FELLOG                                
044900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS NEXT SENTENCE             
045000     END-SEARCH                                                           
045100     .                                                                    
045200     EJECT                                                                
045300*    -COPY WY2000Q1                                                       
