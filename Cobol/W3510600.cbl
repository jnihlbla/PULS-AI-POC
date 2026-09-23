000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3510600.                                                
000400 AUTHOR.         RICHARD.                                                 
000500 DATE-WRITTEN.   APRIL 1997.                                              
000510 DATE-COMPILED.                                                           
000600                                                                          
000900*    FUNKTION:                                                            
001000*        PROGRAMMET ÄR ETT LADDPROGRAM FÖR BASEN WDK8                     
001200*                                                                         
001300*       INFILER MED LADDTRANSAKTIONER:                                    
001400*                                                                         
001500*        -W35105 TOTALPOSTER FÖR ARTIKEL, SAMT                            
001600*             ALLA AIGRUPPER.                                             
001700*             DÄR KDAIKTYP = LOW-VALUE                                    
002000*        -W35106 KUNDPOSTER                                               
002100*             ALLA DETALJPOSTER                                           
002200*                                                                         
002300*       W35105 SORTERAS PÅ NYCKEL:                                        
002310*                       IDARTNR + KDSEGKEY + IDAIGRP + IDKUNDNR.          
002400*             BÄGGE FILERNA LÄSES OCH POSTER LÄGGS UPP PÅ                 
002500*             BASEN WDK8 I STIGANDE SEKVENS                               
002600*                                                                         
002700*    ABENDKODER:                                                          
002800*        U0016 - OM RETURKOD FRÅN SORT (EX.VIS FÖR LITE SORTWK)           
002900     EJECT                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100                                                                          
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003500                                                                          
003600*    ---- INFILER:                                                        
003700*                            - ARTIKEL- OCH MARKNADSGRP POSTER            
003800     SELECT  W35105        ASSIGN  W35106D1.                              
003900                                                                          
004000*                            - KUNDPOSTER                                 
004100     SELECT  W35106        ASSIGN  W35106D2.                              
004200                                                                          
004300*    ---- SORTFIL:                                                        
004400*                                                                         
004500     SELECT  SORTFIL       ASSIGN  W35106DS.                              
004600     EJECT                                                                
004700 DATA DIVISION.                                                           
004800                                                                          
004900 FILE SECTION.                                                            
005000                                                                          
005100 FD  W35105                                                               
005200     LABEL RECORD STANDARD                                                
005300     RECORDING  F                                                         
005400     BLOCK CONTAINS 0.                                                    
005500                                                                          
005600*    -COPY WDK801        -L.                                              
005800                                                                          
005810                                                                          
005820                                                                          
005900 FD  W35106                                                               
006000     LABEL RECORD STANDARD                                                
006100     RECORDING  F                                                         
006200     BLOCK CONTAINS 0.                                                    
006300                                                                          
006400*    -COPY WDK801        -L.                                              
006500                                                                          
006600                                                                          
006610     EJECT                                                                
006700 SD  SORTFIL.                                                             
006900                                                                          
007000*01  POST   -COPY WDK801       -PRE SORT-.                                
007100                                                                          
007200     EJECT                                                                
007300 WORKING-STORAGE SECTION.                                                 
007310                                                                          
007400*    -- CHECKED BY WY2000                                                 
007500 77  IDPGM                   PIC X(8)    VALUE 'W3510600'.                
007800 77  JA                      PIC X       VALUE 'J'.                       
007900 77  NEJ                     PIC X       VALUE 'N'.                       
007910 77  ANT-TOT                 PIC S9(9)   VALUE ZERO COMP-3.               
007911 77  ANT-KUND                PIC S9(9)   VALUE ZERO COMP-3.               
007912 77  ANT-SORT                PIC S9(9)   VALUE ZERO COMP-3.               
008000                                                                          
008100 77  SORTFIL-EOF             PIC X       VALUE 'N'.                       
008200 77  KUNDFIL-EOF             PIC X       VALUE 'N'.                       
008300                                                                          
008400 01  WS-SORT-NYCKEL.                                                      
008500   03  WS-SORT-IDARTNR       PIC S9(9)   VALUE ZERO  COMP-3.              
008501   03  WS-SORT-KDSEGKEY      PIC X       VALUE SPACE.                     
008510   03  WS-SORT-IDAIGRP       PIC X(03)   VALUE SPACE.                     
008520   03  WS-SORT-IDKUNDNR      PIC S9(7)   VALUE ZERO  COMP-3.              
008900                                                                          
009000                                                                          
009001 01  WS-IN-NYCKEL.                                                        
009002   03  WS-IN-IDARTNR         PIC S9(9)   VALUE ZERO  COMP-3.              
009003   03  WS-IN-KDSEGKEY        PIC X       VALUE SPACE.                     
009004   03  WS-IN-IDAIGRP         PIC X(03)   VALUE SPACE.                     
009005   03  WS-IN-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.              
009010                                                                          
009011                                                                          
009020 01  DYNAMISKA-SUBPROGRAM.                                                
009050   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
009060   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
009070                                                                          
009080                                                                          
009100 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   VALUE +16  COMP SYNC.            
009700     EJECT                                                                
010800 01  FILLER                  PIC X(24) VALUE 'WSORT-AREA-START'.          
010900                                                                          
011000*01  AREA  -COPY WDK801      -PRE  WSORT-.                                
011200     EJECT                                                                
011300 01  FILLER                  PIC X(24) VALUE 'IN-AREA-START'.             
011400                                                                          
011500*01  AREA  -COPY WDK801      -PRE  IN-.                                   
011700     EJECT                                                                
011800 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
011900                                                                          
012000*    ---- STATUSKOD FRÅN IMS                                              
012100                                                                          
012200 01  STATUS-WS               PIC XX.                                      
012300     88  SEGMENT-FINNS                    VALUE '  '.                     
012400     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
012500     88  SEGMENT-FINNS-REDAN              VALUE 'II'.                     
012600     SKIP3                                                                
012700 01  GODK-STATUSKODER.                                                    
012800   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
012900                                                                          
012910                                                                          
013000 01  SSA1                    PIC X(32).                                   
013100 01  SSA2                    PIC X(32).                                   
013200     EJECT                                                                
013300*01  -COPY W0003                                                          
013500     EJECT                                                                
013600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK801'.             
013700                                                                          
013800 01  DLI-IO-WDK801.                                                       
013900*  03  -COPY WDK801.                                                      
014000                                                                          
014100     EJECT                                                                
014200 LINKAGE SECTION.                                                         
014400*01  -COPY W0008      -PRE  WDK8-                                         
014600       05  FILLER                PIC X.                                   
014700     EJECT                                                                
014800 PROCEDURE DIVISION  USING  WDK8-PCB.                                     
014810 MAIN SECTION.                                                            
014900     ENTRY 'DLITCBL' USING  WDK8-PCB.                                     
015000                                                                          
015100     OPEN INPUT  W35106                                                   
015200                                                                          
015700     SORT SORTFIL ASCENDING SORT-ART-IDARTNR                              
015710                            SORT-ART-KDSEGKEY                             
015711                            SORT-ART-IDAIGRP                              
015720                            SORT-ART-IDKUNDNR                             
015800       USING W35105                                                       
015900       OUTPUT PROCEDURE B-UPPDATERA-BASEN                                 
016000                                                                          
016100     IF SORT-RETURN > ZERO                                                
016200       DISPLAY '*** W3510600 - FEL VID SORTERING'                         
016300       CALL FELLOG USING RKOD-ABEND-UTAN-DUMP                             
016400     ELSE                                                                 
016500       CLOSE  W35106                                                      
016601                                                                          
016604       DISPLAY 'W35106D2 W35106   ANT-KUND   = ' ANT-KUND                 
016605       DISPLAY 'W35106DS SORT     ANT-SORT   = ' ANT-SORT                 
016800                                                                          
016900       MOVE ZERO TO RETURN-CODE                                           
017000       GOBACK                                                             
017100     END-IF                                                               
017200     .                                                                    
017310                                                                          
017320     EJECT                                                                
017400 B-UPPDATERA-BASEN SECTION.                                               
017500                                                                          
017600     PERFORM S01-LAS-SORTPOST                                             
017700     PERFORM S02-LAS-KUNDPOST                                             
017800                                                                          
017900     PERFORM UNTIL SORTFIL-EOF = JA AND KUNDFIL-EOF = JA                  
018100       IF WS-SORT-NYCKEL < WS-IN-NYCKEL                                   
018200         MOVE WSORT-AREA TO DLI-IO-WDK801                                 
018300         PERFORM IMS-INSERT-WDK8                                          
018310         IF STATUS-WS = 'LB'                                              
018340           DISPLAY 'WSORT-ARTNR :' WSORT-ART-IDARTNR                      
018341           DISPLAY 'WSORT-AIGRP :' WSORT-ART-IDAIGRP                      
018342           DISPLAY 'WSORT-KUND  :' WSORT-ART-IDKUNDNR                     
018350         END-IF                                                           
018500         PERFORM S01-LAS-SORTPOST                                         
018600       ELSE                                                               
018700         MOVE IN-AREA TO DLI-IO-WDK801                                    
018800         PERFORM IMS-INSERT-WDK8                                          
018810         IF STATUS-WS = 'LB'                                              
018811           IF IN-ART-IDAIGRP NOT = 'PLC'                                  
018812             CONTINUE                                                     
018813***          DISPLAY 'IN-ARTNR :' IN-ART-IDARTNR                          
018814***          DISPLAY 'IN-AIGRP :' IN-ART-IDAIGRP                          
018815***          DISPLAY 'IN-KUND  :' IN-ART-IDKUNDNR                         
018816           END-IF                                                         
018850         END-IF                                                           
019000         PERFORM S02-LAS-KUNDPOST                                         
019100       END-IF                                                             
019200     END-PERFORM                                                          
019300     .                                                                    
019400     EJECT                                                                
019500 S01-LAS-SORTPOST     SECTION.                                            
019600                                                                          
019700     RETURN SORTFIL INTO WSORT-AREA                                       
019800     AT END                                                               
019900       MOVE JA TO SORTFIL-EOF                                             
020000       MOVE +999999999       TO WS-SORT-IDARTNR                           
020010       MOVE HIGH-VALUE       TO WS-SORT-KDSEGKEY                          
020011                                WS-SORT-IDAIGRP                           
020020       MOVE +9999999         TO WS-SORT-IDKUNDNR                          
020100     NOT AT END                                                           
020270       ADD +1 TO ANT-SORT                                                 
020280       MOVE WSORT-ART-IDARTNR   TO WS-SORT-IDARTNR                        
020290       MOVE WSORT-ART-KDSEGKEY  TO WS-SORT-KDSEGKEY                       
020300       MOVE WSORT-ART-IDAIGRP   TO WS-SORT-IDAIGRP                        
020400       MOVE WSORT-ART-IDKUNDNR  TO WS-SORT-IDKUNDNR                       
020910     END-RETURN                                                           
021000     .                                                                    
021100     SKIP3                                                                
021200 S02-LAS-KUNDPOST     SECTION.                                            
021300                                                                          
021400     READ W35106 INTO IN-AREA                                             
021500     AT END                                                               
021600       MOVE JA TO KUNDFIL-EOF                                             
021610       MOVE +999999999       TO WS-IN-IDARTNR                             
021620       MOVE HIGH-VALUE       TO WS-IN-KDSEGKEY                            
021630                                WS-IN-IDAIGRP                             
021640       MOVE +9999999         TO WS-IN-IDKUNDNR                            
021641     NOT AT END                                                           
021642       ADD +1 TO ANT-KUND                                                 
021643       MOVE IN-ART-IDARTNR   TO WS-IN-IDARTNR                             
021644       MOVE IN-ART-KDSEGKEY  TO WS-IN-KDSEGKEY                            
021645       MOVE IN-ART-IDAIGRP   TO WS-IN-IDAIGRP                             
021646       MOVE IN-ART-IDKUNDNR  TO WS-IN-IDKUNDNR                            
021700     END-READ                                                             
022700     .                                                                    
022710                                                                          
022800     EJECT                                                                
022900*    ---- IMS SEKTIONER                                                   
023000                                                                          
023100 IMS-INSERT-WDK8 SECTION.                                                 
023200                                                                          
023300     MOVE 'WDK801   '      TO SSA1                                        
023400     MOVE '  IILB'         TO GODK-STATUSKODER                            
023500     CALL CBLTDLI USING ISRT WDK8-PCB DLI-IO-WDK801 SSA1                  
023600     MOVE WDK8-STATUS-CODE TO STATUS-WS                                   
023700     PERFORM IMS-STATUSKONTROLL                                           
023800     .                                                                    
023900                                                                          
023910                                                                          
024000 IMS-STATUSKONTROLL SECTION.                                              
024100                                                                          
024200     SET STATUS-IX TO 1                                                   
024300     SEARCH GODK-STATUS                                                   
024400       AT END                                                             
024410         CALL FELLOG                                                      
024500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
024510         CONTINUE                                                         
024600     END-SEARCH                                                           
024700     .                                                                    
