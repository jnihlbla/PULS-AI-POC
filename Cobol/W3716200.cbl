000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3716200.                                                
000300 AUTHOR.         BO HAMMARIN.                                             
000400 DATE-WRITTEN.   99/10/15.                                                
000500 DATE-COMPILED.                                                           
000600*                                                                         
000700*                                                                         
000800*    FUNKTION:                                                            
000900*      - PGM LÄSER BYTES POÄNGPARAMETRAR (WDGX3155/3156)                  
001000*      - PGM MATCHAR LAGERBAND MOT PRISFIL (W01160/W33504)                
001100*      - PGM SKAPAR FIL MED BYTESARTIKLAR SOM ANVÄNDS                     
001200*        . FÖR ATT UPPDATERA POÄNG-DATA I WDK6                            
001300*        . SOM HISTORIK                                                   
001400*        . FÖR RAPPORTERING TILL PRICE-MANAGER                            
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- UTDRAG UR WDK601/WDK611                                    
002900     SELECT W01160                     ASSIGN TO W37162D1.                
003000     SKIP2                                                                
003100*          --- UTDRAG FRÅN WDC1 ARTIKEL/PRIS PER MARKNAD                  
003200     SELECT W33505                     ASSIGN TO W37162D2.                
003300     SKIP2                                                                
003400*          --- UPPDATERINGSPOSTER BYTES                                   
003500     SELECT W37164                     ASSIGN TO W37162D3.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W01160                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  -COPY W01160      -L.                                                
004600     SKIP3                                                                
004700 FD  W33505                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
005100*01  -COPY W33504      -L.                                                
005200     SKIP3                                                                
005300 FD  W37164                                                               
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS  0.                                                   
005600                                                                          
005700*01  POST -COPY W37164 -PRE  BYT-  -L.                                    
005800     EJECT                                                                
005900 WORKING-STORAGE SECTION.                                                 
006000                                                                          
006100                                                                          
006200*    -- CHECKED BY WY2000                                                 
006300 77  IDPGM                       PIC X(8)    VALUE 'W3716200'.            
006400 77  JA                          PIC X       VALUE 'J'.                   
006500 77  NEJ                         PIC X       VALUE 'N'.                   
006600                                                                          
006700 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
006800     88  END-OF-W01160                       VALUE 'J'.                   
006900                                                                          
007000 77  W33505-EOF-SW               PIC X       VALUE 'N'.                   
007100     88  END-OF-W33505                       VALUE 'J'.                   
007200     EJECT                                                                
007300                                                                          
007400*01  -COPY WWPRODSL                                                       
007500                                                                          
007600 01  DYNAMISKA-SUBPROGRAM.                                                
007700*                                                                         
007800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.             
008000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
008100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008200                                                                          
008300*    --- PARAMETRAR TILL ABEND                                            
008400                                                                          
008500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008800                                                                          
008900 77  WS-NEW-POINT                PIC S9(7)V9(6) COMP VALUE +0.            
009000 77  WS-NEW-POINT-NODEC          PIC S9(7)      COMP VALUE +0.            
009100                                                                          
009200 01  FELTEXT.                                                             
009300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009500     EJECT                                                                
009600                                                                          
009700*    --- PARAMETRAR TILL POSTSUM                                          
009800*                                                                         
009900*01  -COPY W0005   -PRE  POSTSUM-                                         
010000     EJECT                                                                
010100                                                                          
010200 01  TEST-IDARTNR                PIC 9(9) COMP-3.                         
010300*01  FILLER -COPY WWBYT03  -RED TEST-IDARTNR                              
010400     EJECT                                                                
010500                                                                          
010600 01  LB-AREA-START               PIC X(24)   VALUE                        
010700                                 'LB-AREA-START  '.                       
010800     SKIP2                                                                
010900                                                                          
011000*01  AREA -COPY W01160     -PRE LB-                                       
011100     EJECT                                                                
011200 01  PR-AREA-START               PIC X(24)   VALUE                        
011300                                 'PR-AREA-START  '.                       
011400     SKIP2                                                                
011500                                                                          
011600*01  AREA -COPY W33504     -PRE PR-                                       
011700     EJECT                                                                
011800 01  BYT-AREA-START              PIC X(24)   VALUE                        
011900                                 'BYT-AREA-START  '.                      
012000     SKIP2                                                                
012100                                                                          
012200*01  AREA -COPY W37164     -PRE BYT-                                      
012300     EJECT                                                                
012400*01  -COPY WWBYT22                                                        
012500*01  -COPY WWBYT23                                                        
012600     EJECT                                                                
012700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012800*                                                                         
012900     EJECT                                                                
013000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013100     SKIP3                                                                
013200 01  NYCKLAR-TILL-DLI.                                                    
013300     03  W-WDGXKEY-3155-X.                                                
013400         05  W-IDHTYP-3155       PIC X(4)    VALUE '3155'.                
013500         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
013600     03  W-WDGXKEY-3156-X.                                                
013700         05  W-KDSEGKEY-3156     PIC X(1)    VALUE '1'.                   
013800                                                                          
013900*    --- STATUS-KOD FRÅN IMS                                              
014000 01  STATUS-WS                   PIC XX.                                  
014100     88  SEGMENT-FINNS                       VALUE '  '.                  
014200     SKIP2                                                                
014300 01  GODK-STATUSKODER.                                                    
014400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014500     SKIP3                                                                
014600 01  SSA1                        PIC X(64).                               
014700 01  SSA2                        PIC X(64).                               
014800     EJECT                                                                
014900*    --- IMS FUNKTIONSKODER                                               
015000*01  -COPY W0003                                                          
015100     EJECT                                                                
015200*    ---  DLI INPUT-OUTPUT AREA                                           
015300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3156'.                    
015400 01  DLI-IO-WDGX3156.                                                     
015500*    03  -COPY WDGX3156                                                   
015600     EJECT                                                                
015700 LINKAGE SECTION.                                                         
015800                                                                          
015900*01  -COPY W0008  -PRE 3156-                                              
016000     05  FILLER                  PIC X.                                   
016100                                                                          
016200     EJECT                                                                
016300 PROCEDURE DIVISION  USING 3156-PCB.                                      
016400 MAIN SECTION.                                                            
016500     ENTRY 'DLITCBL' USING 3156-PCB.                                      
016600                                                                          
016700     PERFORM A-INIT                                                       
016800                                                                          
016900     PERFORM S01-LAES-W01160                                              
017000     PERFORM S02-LAES-W33505                                              
017100                                                                          
017200     PERFORM UNTIL END-OF-W01160                                          
017300       MOVE LB-CLAG-IDARTNR          TO TEST-IDARTNR                      
017400       IF BYT03-OBJEKT                                                    
017500                                                                          
017600         PERFORM UNTIL END-OF-W33505 OR                                   
017700                       PR-IDARTNR > LB-CLAG-IDARTNR                       
018500           IF PR-IDARTNR = LB-CLAG-IDARTNR                                
018600             IF LB-CLAG-KDERS-UTG > +0                                    
018700               CONTINUE                                                   
018800             ELSE                                                         
018900               IF 3156-FLEXCBLK = 'N'                                     
019000                 PERFORM C-CALC-POINT                                     
019100                 IF (WS-NEW-POINT-NODEC NOT = LB-CLAG-KVPOINT)            
019200**ERROR CORRECTION 20000524  GS****                                       
019300                 OR ( LB-CLAG-KDPRODSL NOT = LB-CLAG-KDEXCHA)             
019400**END              20000524    ****                                       
019500                   MOVE 'C'          TO BYT-KDBEH                         
019600                   PERFORM D-BYGG-UPD-REC                                 
019700                   PERFORM S11-SKRIV-W37164                               
019800                 ELSE                                                     
019900                   MOVE LB-CLAG-KDPRODSL                                  
020000                                 TO TEST-KDPRODSL                         
020100                   IF KDPRODSL-BIMA-LOCAL                                 
020200                     IF PR-PRARTBTO-MARK-A > ZERO                         
020300                       MOVE ' '  TO BYT-KDBEH                             
020400                       PERFORM E-BYGG-NOCHANGE-REC                        
020500                       PERFORM S11-SKRIV-W37164                           
020600                     END-IF                                               
020700                   ELSE                                                   
020800                     IF PR-PRARTBTO-MARK-B > ZERO                         
020900                       MOVE ' '  TO BYT-KDBEH                             
021000                       PERFORM E-BYGG-NOCHANGE-REC                        
021100                       PERFORM S11-SKRIV-W37164                           
021200                     END-IF                                               
021300                   END-IF                                                 
021400                 END-IF                                                   
021500               ELSE                                                       
021600                 MOVE ' '            TO BYT-KDBEH                         
021700                 PERFORM E-BYGG-NOCHANGE-REC                              
021800                 PERFORM S11-SKRIV-W37164                                 
021900               END-IF                                                     
022000             END-IF                                                       
022100           END-IF                                                         
022200           PERFORM S02-LAES-W33505                                        
022300         END-PERFORM                                                      
022400                                                                          
022500       END-IF                                                             
022600       PERFORM S01-LAES-W01160                                            
022700     END-PERFORM                                                          
022800                                                                          
022900     PERFORM Z-FINIT                                                      
023000                                                                          
023100     MOVE ZERO TO RETURN-CODE                                             
023200     GOBACK                                                               
023300     .                                                                    
023400     EJECT                                                                
023500 A-INIT SECTION.                                                          
023600                                                                          
023700     OPEN INPUT  W01160                                                   
023800                 W33505                                                   
023900     OPEN OUTPUT W37164                                                   
024000                                                                          
024100     MOVE IDPGM     TO  POSTSUM-PROGNAMN                                  
024200                                                                          
024300     PERFORM IMS-GU-WDGX3156                                              
024400     .                                                                    
024500     EJECT                                                                
024600 C-CALC-POINT SECTION.                                                    
024700                                                                          
024800** ERSÄTTNING FÖR STOPPREGISTRET**************                            
024900     MOVE LB-CLAG-IDFKNGRP TO BYT22-IDFKNGRP                              
025000     MOVE LB-CLAG-IDARTNR  TO BYT23-IDARTNR                               
025100     MOVE LB-CLAG-KDPRODSL       TO TEST-KDPRODSL                         
025200*********************************************                             
025300****************FIX    RENÅ OCH LR******************                      
025400     IF KDPRODSL-BIMA-LOCAL                                               
025500       IF PR-PRARTBTO-MARK-A = ZERO                                       
025600         MOVE LB-CLAG-KVPOINT    TO WS-NEW-POINT-NODEC                    
025700       ELSE                                                               
025800         COMPUTE WS-NEW-POINT ROUNDED = PR-PRARTBTO-MARK-A /              
025900                                      3156-REPOINT                        
026000         END-COMPUTE                                                      
026100         COMPUTE WS-NEW-POINT-NODEC ROUNDED = WS-NEW-POINT * 1            
026200         END-COMPUTE                                                      
026300       END-IF                                                             
026400     END-IF                                                               
026500****************FIX EJ RENÅ OCH LR******************                      
026600     IF KDPRODSL-VOLVO-ALL                                                
026700       IF PR-PRARTBTO-MARK-B = ZERO                                       
026800         MOVE LB-CLAG-KVPOINT    TO WS-NEW-POINT-NODEC                    
026900       ELSE                                                               
027000         COMPUTE WS-NEW-POINT ROUNDED = PR-PRARTBTO-MARK-B /              
027100                                      3156-REPOINT                        
027200         END-COMPUTE                                                      
027300         COMPUTE WS-NEW-POINT-NODEC ROUNDED = WS-NEW-POINT * 1            
027400         END-COMPUTE                                                      
027500       END-IF                                                             
027600     END-IF                                                               
027700************SLUT FIX RENÅ OCH LR**************                            
027800*** ERSÄTTNING FÖR STOPPREGISTER**************                            
027900     IF BYT22-STOPP OR BYT23-STOPP                                        
028000     MOVE 0 TO WS-NEW-POINT-NODEC                                         
028100     END-IF                                                               
028200***********************************************                           
028300     .                                                                    
028400     EJECT                                                                
028500 D-BYGG-UPD-REC SECTION.                                                  
028600                                                                          
028700     MOVE LB-CLAG-IDARTNR            TO BYT-IDARTNR                       
028800     MOVE LB-CLAG-IDFKNGRP           TO BYT-IDFKNGRP                      
028900     MOVE FUNCTION CURRENT-DATE(1:8) TO BYT-DAXPOINT                      
029000     MOVE LB-CLAG-KDPRODSL           TO BYT-KDEXCHA                       
029100                                        TEST-KDPRODSL                     
029900     MOVE LB-CLAG-KVLS               TO BYT-KVLS                          
030000     MOVE WS-NEW-POINT-NODEC         TO BYT-KVPOINT                       
030100     MOVE LB-CLAG-PRARTBTO-EXP       TO BYT-PRARTBTO                      
030200     IF KDPRODSL-BIMA-LOCAL                                               
030300       MOVE PR-PRARTBTO-MARK-A       TO BYT-PRREF                         
030400     ELSE                                                                 
030500       MOVE PR-PRARTBTO-MARK-B       TO BYT-PRREF                         
030600     END-IF                                                               
030700     MOVE LB-CLAG-PRARTSJK           TO BYT-PRARTSJK                      
030800     .                                                                    
030900     EJECT                                                                
031000 E-BYGG-NOCHANGE-REC SECTION.                                             
031100                                                                          
031200     MOVE LB-CLAG-IDARTNR            TO BYT-IDARTNR                       
031300     MOVE LB-CLAG-IDFKNGRP           TO BYT-IDFKNGRP                      
031400     MOVE LB-CLAG-DAXPOINT           TO BYT-DAXPOINT                      
031500     MOVE LB-CLAG-KDPRODSL           TO BYT-KDEXCHA                       
032200     MOVE LB-CLAG-KVLS               TO BYT-KVLS                          
032300     MOVE LB-CLAG-KVPOINT            TO BYT-KVPOINT                       
032400     MOVE LB-CLAG-PRARTBTO-EXP       TO BYT-PRARTBTO                      
032500     MOVE LB-CLAG-PRREF              TO BYT-PRREF                         
032600     MOVE LB-CLAG-PRARTSJK           TO BYT-PRARTSJK                      
032700     .                                                                    
032800     EJECT                                                                
032900 Z-FINIT SECTION.                                                         
033000                                                                          
033100     CLOSE W01160                                                         
033200           W33505                                                         
033300           W37164                                                         
033400     SKIP2                                                                
033500     MOVE 'S' TO POSTSUM-OPKOD                                            
033600     CALL POSTSUM USING POSTSUM-PARM                                      
033700     .                                                                    
033800     EJECT                                                                
033900 S01-LAES-W01160  SECTION.                                                
034000                                                                          
034100     READ W01160 INTO LB-AREA                                             
034200     AT END                                                               
034300        MOVE HIGH-VALUE   TO LB-AREA                                      
034400        SET END-OF-W01160 TO TRUE                                         
034500                                                                          
034600     NOT AT END                                                           
034700        MOVE 'W01160'   TO POSTSUM-FDNAMN                                 
034800        MOVE 'W37162D1' TO POSTSUM-DDNAMN2                                
034900        MOVE 'LB'       TO POSTSUM-TRANSTYP                               
035000        CALL POSTSUM USING POSTSUM-PARM                                   
035100     END-READ                                                             
035200     .                                                                    
035300     EJECT                                                                
035400 S02-LAES-W33505  SECTION.                                                
035500                                                                          
035600     READ W33505 INTO PR-AREA                                             
035700     AT END                                                               
035800        MOVE HIGH-VALUE TO PR-AREA                                        
035900        SET END-OF-W33505 TO TRUE                                         
036000                                                                          
036100     NOT AT END                                                           
036200        MOVE 'W33505'   TO POSTSUM-FDNAMN                                 
036300        MOVE 'W37162D2' TO POSTSUM-DDNAMN2                                
036400        MOVE 'PR'       TO POSTSUM-TRANSTYP                               
036500        CALL POSTSUM USING POSTSUM-PARM                                   
036600     END-READ                                                             
036700     .                                                                    
036800     EJECT                                                                
036900 S11-SKRIV-W37164 SECTION.                                                
037000                                                                          
037100     WRITE BYT-POST FROM BYT-AREA                                         
037200                                                                          
037300     MOVE 'BYT'      TO POSTSUM-TRANSTYP                                  
037400     MOVE 'W37164'   TO POSTSUM-FDNAMN                                    
037500     MOVE 'W37162D3' TO POSTSUM-DDNAMN2                                   
037600     CALL POSTSUM USING POSTSUM-PARM                                      
037700     .                                                                    
037800* --- IMS SEKTIONER ---                                                   
037900                                                                          
038000 IMS-GU-WDGX3156 SECTION.                                                 
038100                                                                          
038200     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-3155-X ')'                    
038300            DELIMITED BY SIZE INTO SSA1                                   
038400     STRING 'WDGX3156(KDSEGKEY =' W-WDGXKEY-3156-X ')'                    
038500            DELIMITED BY SIZE INTO SSA2                                   
038600     MOVE '  '             TO GODK-STATUSKODER                            
038700     CALL CBLTDLI USING GU  3156-PCB DLI-IO-WDGX3156 SSA1 SSA2            
038800     MOVE 3156-STATUS-CODE TO STATUS-WS                                   
038900     PERFORM IMS-STATUSKONTROLL                                           
039000     .                                                                    
039100     EJECT                                                                
039200 IMS-STATUSKONTROLL SECTION.                                              
039300                                                                          
039400     SET STATUS-IX TO 1                                                   
039500     SEARCH GODK-STATUS                                                   
039600       AT END                                                             
039700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
039800           DELIMITED BY SIZE INTO FELTEXT                                 
039900         DISPLAY FELTEXT                                                  
040000         CALL FELLOG                                                      
040100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
040200         CONTINUE                                                         
040300     END-SEARCH                                                           
040400     .                                                                    
