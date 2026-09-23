000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W5123200.                                                
000400*AUTHOR.         ROYNA LUND.                                              
000500*DATE-WRITTEN.   93/04/20.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SB-PROGRAM SOM PLOCKAR UT INFORMATION FRÅN                       
001100*        WLARTC (WDK6) FÖR VARULAGERVÄRDERINGEN.                          
001200*                                                                         
001300*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- PRIS- OCH SALDO-INFO FÖR EJ UTGÅNGNA ARTIKLAR.             
002400     SELECT UTFIL                      ASSIGN TO W51232D1.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  UTFIL                                                                
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300     SKIP2                                                                
003400*01  POST -COPY W51233 -PRE  UT-  -L.                                     
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800*    -COPY WY2000W1                                                       
003900     SKIP3                                                                
004000 77  IDPGM                       PIC X(8)    VALUE 'W5123200'.            
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300     SKIP2                                                                
004400 77  SKRIV-SW                    PIC X       VALUE 'N'.                   
004500     88  SKRIV-POST                          VALUE 'J'.                   
004600     SKIP2                                                                
004700 77  IX                          PIC S9(9)   COMP SYNC.                   
004800     SKIP2                                                                
004900 01  W-DATUM                     PIC 9(6)    VALUE ZERO.                  
005000 01  W-KVAKS-T                   PIC S9(7)   VALUE ZERO   COMP-3.         
005100                                                                          
005200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005300*                                                                         
005400 01  DATUM-FAELT.                                                         
005500     03  WS-DAPRLIST-MAX         PIC 9(8)    VALUE 99999999.              
005600     03  WS-DAPRLIST             PIC 9(8).                                
005700     03  FILLER  REDEFINES  WS-DAPRLIST.                                  
005800      05 FILLER                  PIC 9(2).                                
005900      05 WS-LISTDATUM            PIC 9(6).                                
006000     SKIP2                                                                
006100 01  NOLL-AREA.                                                           
006200     03  NOLL-TIPRLIST     PIC S9(7)      COMP-3  VALUE +9999999.         
006300     03  NOLL-SUINLEV-PR   PIC S9(3)      COMP-3  VALUE ZERO.             
006400     03  NOLL-PRARTBEL-PR  PIC S9(8)V9(5) COMP-3  VALUE ZERO.             
006500     03  NOLL-KKDVALISO    PIC X(3)               VALUE SPACE.            
006600     EJECT                                                                
006700 01  DYNAMISKA-SUBPROGRAM.                                                
006800*                                                                         
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
007200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007300     SKIP2                                                                
007400*    --- VALID IDDC CODES                                                 
007500*                                                                         
007600*01  -COPY WWDCKONS                                                       
007700*    --- PARAMETRAR TILL ABEND                                            
007800                                                                          
007900 01  FELTEXT.                                                             
008000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008200     EJECT                                                                
008300*    --- PARAMETRAR TILL DATKORT                                          
008400*                                                                         
008500 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W51232'.              
008600     SKIP2                                                                
008700 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
008800     SKIP2                                                                
008900*01  -COPY WDATKORT                                                       
009000     EJECT                                                                
009100*    --- PARAMETRAR TILL POSTSUM                                          
009200*                                                                         
009300*01  -COPY W0005   -PRE  POSTSUM-                                         
009400     EJECT                                                                
009500 01  UT-AREA-START               PIC X(24)   VALUE                        
009600                                            'UT-AREA-START  '.            
009700     SKIP2                                                                
009800                                                                          
009900*01  AREA -COPY W51233     -PRE UT-                                       
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010200     SKIP3                                                                
010300*    --- STATUS-KOD FRÅN IMS                                              
010400 01  STATUS-WS                   PIC XX.                                  
010500     88  SEGMENT-FINNS                       VALUE '  '.                  
010600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010700     SKIP2                                                                
010800 01  GODK-STATUSKODER.                                                    
010900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011000     SKIP3                                                                
011100 01  SSA1                        PIC X(64).                               
011200 01  SSA2                        PIC X(64).                               
011300     EJECT                                                                
011400*    --- IMS FUNKTIONSKODER                                               
011500*01  -COPY W0003                                                          
011600     EJECT                                                                
011700*    ---  DLI INPUT-OUTPUT AREA                                           
011800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011900     SKIP3                                                                
012000 01  DLI-IO-AREA.                                                         
012100     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
012200     SKIP3                                                                
012300     03  WLARTC01 REDEFINES IO-AREA.                                      
012400*        05  -COPY WDK601                                                 
012500     EJECT                                                                
012600     03  WLARTC11 REDEFINES IO-AREA.                                      
012700*        05  -COPY WDK611                                                 
012800     EJECT                                                                
012900     03  WLARTC24 REDEFINES IO-AREA.                                      
013000*        05  -COPY WDK621                                                 
013100     EJECT                                                                
013200 LINKAGE SECTION.                                                         
013300                                                                          
013400     SKIP2                                                                
013500*01  -COPY W0008  -PRE ARTC-                                              
013600     05  FILLER                  PIC X(10).                               
013700     EJECT                                                                
013800 PROCEDURE DIVISION  USING ARTC-PCB.                                      
013900     ENTRY 'DLITCBL' USING ARTC-PCB.                                      
014000                                                                          
014100 STYR SECTION.                                                            
014200     PERFORM A-INIT                                                       
014300     PERFORM IMS-GN-SEGMENT                                               
014400                                                                          
014500     PERFORM UNTIL SEGMENT-SLUT                                           
014600       EVALUATE  ARTC-SEG-NAME-FB                                         
014700         WHEN 'WDK601  '                                                  
014800           IF SKRIV-POST                                                  
014900             PERFORM S11-SKRIV-UTFIL                                      
015000             MOVE NEJ          TO SKRIV-SW                                
015100           END-IF                                                         
015200           PERFORM B-NOLLA-FLYTTA-FALT                                    
015300         WHEN 'WDK611  '                                                  
015400           PERFORM C-KOLLA-SALDON-FLYTTA-FALT                             
015500         WHEN 'WDK621  '                                                  
015600           IF PRL-FLHUVLEV = JA                                           
015700             PERFORM D-KOLLA-STATUS-FLYTTA-FALT                           
015800           END-IF                                                         
015900       END-EVALUATE                                                       
016000                                                                          
016100       PERFORM IMS-GN-SEGMENT                                             
016200                                                                          
016300     END-PERFORM                                                          
016400                                                                          
016500     IF SKRIV-POST                                                        
016600       PERFORM S11-SKRIV-UTFIL                                            
016700     END-IF                                                               
016800                                                                          
016900     PERFORM Z-FINIT                                                      
017000     MOVE ZERO TO RETURN-CODE                                             
017100     GOBACK                                                               
017200                                                                          
017300     .                                                                    
017400     EJECT                                                                
017500 A-INIT SECTION.                                                          
017600                                                                          
017700     OPEN OUTPUT UTFIL                                                    
017800                                                                          
017900     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
018000     MOVE D-AAR       TO DAGENS-DATUM(1:2)                                
018100     MOVE D-MAANAD    TO DAGENS-DATUM(3:2)                                
018200     MOVE D-DAG       TO DAGENS-DATUM(5:2)                                
018300                                                                          
018400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018500     .                                                                    
018600     SKIP3                                                                
018700 B-NOLLA-FLYTTA-FALT SECTION.                                             
018800                                                                          
018900     MOVE ZERO TO IX                                                      
019000                                                                          
019100     MOVE NOLL-AREA TO UT-BEST-PRISER (1)                                 
019200                       UT-BEST-PRISER (2)                                 
019300                       UT-BEST-PRISER (3)                                 
019400                       UT-BEST-PRISER (4)                                 
019500                       UT-BEST-PRISER (5)                                 
019600                                                                          
019700     MOVE ART-IDARTNR    TO UT-IDARTNR                                    
019800     MOVE ART-IDLEVNR    TO UT-IDLEVNR                                    
019900     MOVE ART-KDPRODSL   TO UT-KDPRODSL                                   
020000     MOVE ART-IDFKNGRP   TO UT-IDFKNGRP                                   
020100                                                                          
020200     IF ART-KDERS-UTG = ZERO                                              
020300        MOVE JA          TO SKRIV-SW                                      
020400     END-IF                                                               
020500     .                                                                    
020600     EJECT                                                                
020700 C-KOLLA-SALDON-FLYTTA-FALT SECTION.                                      
020800                                                                          
020900     MOVE WC-CDC-SE      TO UT-IDDC                                       
021000     MOVE CLAG-PRARTSTD  TO UT-PRARTSTD                                   
021100     MOVE CLAG-PRINK     TO UT-PRINK                                      
021200     MOVE CLAG-RETULF    TO UT-RETULF                                     
021300     MOVE CLAG-KVAKS-CDC TO UT-KVAKS                                      
021400     ADD  CLAG-KVAKS-PAV TO UT-KVAKS                                      
021500     MOVE CLAG-KVEFRS    TO UT-KVEFRS                                     
021600     MOVE CLAG-KVLS      TO UT-KVLS                                       
021700     MOVE CLAG-KVAKS-T   TO W-KVAKS-T                                     
021800     .                                                                    
021900     SKIP3                                                                
022000 D-KOLLA-STATUS-FLYTTA-FALT SECTION.                                      
022100                                                                          
022200     IF PRL-KDSTATUS-PR = 1                                               
022300       ADD +1  TO IX                                                      
022400       IF IX > 5                                                          
022410         CONTINUE                                                         
022420       ELSE                                                               
022500         SUBTRACT PRL-DAPRLIST-9KOMPL FROM WS-DAPRLIST-MAX                
022600         GIVING WS-DAPRLIST                                               
022700         MOVE WS-LISTDATUM      TO W-DATUM                                
022800         MOVE W-DATUM           TO TMP1-YYMMDD                            
022900         MOVE DAGENS-DATUM      TO TMP2-YYMMDD                            
023000         PERFORM WY2000P1                                                 
023100                                                                          
023200         IF TMP1-YYMMDD > TMP2-YYMMDD                                     
023300           COMPUTE IX = IX - 1                                            
023400         ELSE                                                             
023500           MOVE W-DATUM         TO UT-TIPRLIST    (IX)                    
023600           MOVE PRL-SUINLEV-PR  TO UT-SUINLEV-PR  (IX)                    
023700           MOVE PRL-PRARTBEL-PR TO UT-PRARTBEL-PR (IX)                    
023800           MOVE PRL-KDVALISO    TO UT-KDVALISO    (IX)                    
023900         END-IF                                                           
023910       END-IF                                                             
024000     END-IF                                                               
024100     .                                                                    
024200     EJECT                                                                
024300 Z-FINIT SECTION.                                                         
024400                                                                          
024500     CLOSE UTFIL                                                          
024600                                                                          
024700     MOVE 'S'        TO POSTSUM-OPKOD                                     
024800     CALL POSTSUM USING POSTSUM-PARM                                      
024900     .                                                                    
025000     SKIP3                                                                
025100 S11-SKRIV-UTFIL SECTION.                                                 
025200                                                                          
025300     WRITE UT-POST FROM UT-AREA                                           
025400                                                                          
025500     MOVE '    '     TO POSTSUM-TRANSTYP                                  
025600     MOVE 'W51233'   TO POSTSUM-FDNAMN                                    
025700     MOVE 'W51232D1' TO POSTSUM-DDNAMN2                                   
025800     CALL POSTSUM USING POSTSUM-PARM                                      
025900     .                                                                    
026000     SKIP3                                                                
026100* --- IMS SEKTIONER ---                                                   
026200     SKIP3                                                                
026300 IMS-GN-SEGMENT SECTION.                                                  
026400                                                                          
026500     CALL CBLTDLI USING GN ARTC-PCB DLI-IO-AREA                           
026600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
026700     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
026800     PERFORM IMS-STATUSKONTROLL                                           
026900     .                                                                    
027000     SKIP3                                                                
027100 IMS-STATUSKONTROLL SECTION.                                              
027200                                                                          
027300     SET STATUS-IX TO 1                                                   
027400     SEARCH GODK-STATUS                                                   
027500       AT END                                                             
027600         MOVE 'FELAKTIG STATUSKOD FRÅN IMS' TO FELTEXT-STR                
027700         DISPLAY FELTEXT                                                  
027800         CALL FELLOG                                                      
027900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
028000         CONTINUE                                                         
028100     END-SEARCH                                                           
028200     .                                                                    
028300     EJECT                                                                
028400*    -COPY WY2000P1                                                       
