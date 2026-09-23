000100 ID  DIVISION.                                                            
000200                                                                          
000300 PROGRAM-ID.    W5137100.                                                 
000400 AUTHOR.        KARL JOHAN HANSSON.                                       
000500 DATE-WRITTEN.  NOV  2006.                                                
000600                                                                          
000700* FUNKTION: LÄSER ARTIKELREGISTER WDK6 MED SB                             
000800*           FÖR VARJE ARTIKEL VARS LAGERSALDO EJ ÄR NOLL                  
000900*           SKAPAS EN POST PÅ UTFILEN W51316                              
001200*                                                                         
001210*           SKAPAR URVALSTRANSAR FÖR DC 11, PÅ UTFILEN W51371             
001220*                                                                         
001300*           OBS! VID ÅRETS SISTA KÖRNING SKAPAS URVAL FÖR FÖRSTA          
001400*           ARBETSDAG NYTT ÅR.                                            
001500*           DETTA STYRS AV ÅRTALSFILEN W5137D SOM SKAPAS NY VARJE         
001600*           ÅRSSKIFTE (I WYR001).                                         
001700*                                                                         
001703     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*--- URVALSFIL :                                                          
002500                                                                          
002600     SELECT W51316                       ASSIGN TO UT-S-W51371D1.         
002700                                                                          
002800     SELECT W5137D                       ASSIGN TO UT-S-W51371D2.         
002900                                                                          
002910     SELECT W51371                       ASSIGN TO UT-S-W51371D3.         
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP2                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W51316                                                               
003700     RECORDING  F                                                         
003800     BLOCK CONTAINS 0.                                                    
004000*01  POST -COPY W51371 -PRE  SORTWS- -L.                                  
004100     SKIP3                                                                
004110                                                                          
004200 FD  W5137D                                                               
004300     LABEL RECORD    STANDARD                                             
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS 0.                                                    
004510*01  INPOST -COPY W5137D             -L.                                  
004520     EJECT                                                                
004600                                                                          
004610 FD  W51371                                                               
004620     RECORDING  F                                                         
004630     BLOCK CONTAINS 0.                                                    
004640*01  UTPOST  -COPY W51371     -L.                                         
004650     EJECT                                                                
004660                                                                          
005400 WORKING-STORAGE SECTION.                                                 
005500                                                                          
005600 77  IDPGM                       PIC X(8)    VALUE 'W5137100'.            
005700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(3) VALUE +16.                     
005800 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
005900     88  END-OF-SORTFIL                      VALUE 'J'.                   
006200 77  CD-IX                       PIC S9(3) VALUE ZERO.                    
006300 77  JA                          PIC X       VALUE 'J'.                   
006400 77  NEJ                         PIC X       VALUE 'N'.                   
006500 77  EOF                         PIC X       VALUE 'N'.                   
006510 01  WS-DAINVDAT                 PIC S9(7)   COMP-3 VALUE ZERO.           
006600                                                                          
006700 01  FELTEXT.                                                             
006800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006900     03  FELTEXT-STR             PIC X(72)   VALUE 'SORT-FEL'.            
007000                                                                          
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
007510     03  WDATKONV                PIC X(8)      VALUE 'WDATKONV'.          
008100                                                                          
008200 01  W-URVALS-DATUM              PIC 9(7).                                
008300     EJECT                                                                
008400*01  -COPY W0005      -PRE POSTSUM-                                       
008500     EJECT                                                                
008510                                                                          
008520 01  FILLER                      PIC X(8)   VALUE 'WDATKONV'.             
008530*01  -COPY WDATAREA.                                                      
008540     EJECT                                                                
008550                                                                          
008600 01  FILLER                      PIC X(8)    VALUE 'IN-AREA'.             
008700 01  INAREA.                                                              
008800*    03  -COPY W5137D   -PRE IN-.                                         
008900     EJECT                                                                
009610 01  FILLER                      PIC X(8)    VALUE 'UT2-AREA'.            
009611 01  UT2-AREA.                                                            
009620*    03  -COPY W51371 -PRE UT2-                                           
009630     EJECT                                                                
009700 01  FILLER                      PIC X(8)    VALUE 'SOUTAREA'.            
009710 01  SOUT-AREA.                                                           
009800*    03  -COPY W51371 -PRE SOUT-                                          
009900                                                                          
010000 01  SORT-RETURN-X               PIC X(2)    VALUE SPACE.                 
010100     EJECT                                                                
010200 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
010300                                                                          
010400 01  IMS-WS.                                                              
010500                                                                          
010600     03  STATUS-WS               PIC X(2).                                
010700        88  SEGMENT-SLUT                     VALUE 'GB'.                  
010800        88  SEGMENT-FINNS                    VALUE '  ' 'GA' 'GK'.        
010900        88  SEGMENT-SAKNAS                   VALUE 'GE'.                  
011000                                                                          
011100     03  GODK-STATUSKODER.                                                
011200         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
011300                                                                          
011400*01  -COPY W0003                                                          
011500     EJECT                                                                
011600 01  DLI-IO-AREA.                                                         
011700     03  IO-AREA              PIC X(990).                                 
011800     SKIP3                                                                
011900*    03  FILLER -COPY WDK601 -RED IO-AREA                                 
012000     EJECT                                                                
012100*    03  FILLER -COPY WDK611 -RED IO-AREA                                 
012200     EJECT                                                                
012300 LINKAGE SECTION.                                                         
012400     SKIP3                                                                
012500*01  -COPY W0008   -PRE WDK6-                                             
012600         05  FILLER           PIC X(1).                                   
012700     EJECT                                                                
012800 PROCEDURE DIVISION USING  WDK6-PCB.                                      
012900     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
013000                                                                          
013100     PERFORM A-INIT                                                       
013200                                                                          
013500     PERFORM B-CREATE-FILE                                                
013700                                                                          
014600     PERFORM Z-FINIT                                                      
014800     GOBACK                                                               
015000     .                                                                    
015100     EJECT                                                                
015110                                                                          
015200 A-INIT SECTION.                                                          
015400     OPEN INPUT  W5137D                                                   
015500          OUTPUT W51316                                                   
015510          OUTPUT W51371                                                   
015540                                                                          
015600     PERFORM S01-LAS-IN-POST                                              
015601                                                                          
015700     IF EOF = NEJ                                                         
015800       COMPUTE W-URVALS-DATUM = IN-TIAAAA * 1000                          
015900       DISPLAY 'REDAN INV I ÅR, DATUM(AAAAVVD) > ' W-URVALS-DATUM         
016000     ELSE                                                                 
016100       DISPLAY 'URVALSDATUM-POST PÅ FIL W5137D SAKNAS'                    
016200       PERFORM S99-ABEND                                                  
016300     END-IF                                                               
016400                                                                          
016500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016600     .                                                                    
016700     EJECT                                                                
016710                                                                          
016800 B-CREATE-FILE SECTION.                                                   
017000     PERFORM IMS-GET-WDK6                                                 
017100                                                                          
017200     PERFORM UNTIL SEGMENT-SLUT                                           
017300        EVALUATE WDK6-SEG-NAME-FB                                         
017400           WHEN 'WDK601 ' MOVE ART-IDARTNR  TO SOUT-IDARTNR               
017500                          MOVE ART-KDPRODSL TO SOUT-KDPRODSL              
017600                          MOVE ART-IDFKNGRP TO SOUT-IDFKNGRP              
017800                                                                          
017900           WHEN 'WDK611 ' IF CLAG-KVLS > +0                               
018100                            PERFORM BA-SKRIV-POST                         
018110                            PERFORM S04-SKRIV-UTPOST                      
018120                            PERFORM S05-SKRIV-UTPOST2                     
018200                          END-IF                                          
018300        END-EVALUATE                                                      
018400        PERFORM IMS-GET-WDK6                                              
018500     END-PERFORM                                                          
018600     .                                                                    
018700     EJECT                                                                
018710                                                                          
018800 BA-SKRIV-POST SECTION.                                                   
019000*** TAG FRAM LAGEROMRÅDE GÅNG OCH PLATS                                   
019100     MOVE CLAG-ADLAGOMR                   TO SOUT-ADLAGOMR                
019200     MOVE CLAG-ADGANG                     TO SOUT-ADGANG                  
019300     MOVE CLAG-ADPLATS                    TO SOUT-ADPLATS                 
019400     IF   CLAG-ADLAGOMR     = ZERO AND                                    
019500          CLAG-ADGANG       = ZERO AND                                    
019600          CLAG-ADPLATS      = ZERO                                        
019901       IF CLAG-ADLAGOMR-SVS = ZERO AND                                    
019902          CLAG-ADGANG-SVS   = ZERO AND                                    
019903          CLAG-ADPLATS-SVS  = ZERO                                        
020000         MOVE +1 TO CD-IX                                                 
020100         PERFORM UNTIL CD-IX > 4                                          
020200           IF CLAG-ADLAGOMR-CD(CD-IX) NOT = ZERO                          
020300             MOVE CLAG-ADLAGOMR-CD(CD-IX) TO SOUT-ADLAGOMR                
020400             MOVE CLAG-ADGANG-CD  (CD-IX) TO SOUT-ADGANG                  
020500             MOVE CLAG-ADPLATS-CD (CD-IX) TO SOUT-ADPLATS                 
020600             MOVE +4 TO CD-IX                                             
020700           END-IF                                                         
020800           ADD +1 TO CD-IX                                                
020900         END-PERFORM                                                      
021000       ELSE                                                               
021100         MOVE CLAG-ADLAGOMR-SVS           TO SOUT-ADLAGOMR                
021200         MOVE CLAG-ADGANG-SVS             TO SOUT-ADGANG                  
021300         MOVE CLAG-ADPLATS-SVS            TO SOUT-ADPLATS                 
021400       END-IF                                                             
021500     END-IF                                                               
021600*** TAG FRAM ÖVRIGA VÄRDEN PÅ UTPOSTEN                                    
021700     MOVE CLAG-KDVVKL                     TO SOUT-KDVVKL                  
021800     MOVE '1'                             TO SOUT-IDURVAL-INV             
022200                                                                          
022300     IF CLAG-TIINVDAT = 0                                                 
022400       MOVE +0                            TO WS-DAINVDAT                  
022500     ELSE                                                                 
022600       IF CLAG-TIINVDAT > 50000                                           
022700         COMPUTE WS-DAINVDAT = 1900000 + CLAG-TIINVDAT                    
022800       ELSE                                                               
022900         COMPUTE WS-DAINVDAT = 2000000 + CLAG-TIINVDAT                    
023000       END-IF                                                             
023100     END-IF                                                               
023200                                                                          
023300     IF WS-DAINVDAT > W-URVALS-DATUM                                      
023400*** REDAN INVENTERAD I ÅR                                                 
023500       MOVE 9999999         TO WS-DAINVDAT                                
023600     END-IF                                                               
023900     .                                                                    
024000     EJECT                                                                
026310                                                                          
026400 Z-FINIT  SECTION.                                                        
026600     MOVE 'S' TO POSTSUM-OPKOD                                            
026700     CALL POSTSUM USING POSTSUM-PARM                                      
026800                                                                          
026900     CLOSE W5137D                                                         
027000           W51316                                                         
027010           W51371                                                         
027100     .                                                                    
027200     EJECT                                                                
027210                                                                          
027300 S01-LAS-IN-POST SECTION.                                                 
027500     READ W5137D INTO INAREA                                              
027600     AT END                                                               
027700         MOVE JA TO EOF                                                   
027800     END-READ                                                             
027900     .                                                                    
028000     EJECT                                                                
028010                                                                          
030500 S04-SKRIV-UTPOST SECTION.                                                
030600     MOVE '11'           TO SOUT-IDDC                                     
030700     WRITE SORTWS-POST FROM SOUT-AREA                                     
030800                                                                          
030900     MOVE SOUT-IDURVAL-INV TO POSTSUM-TRANSTYP                            
031000     MOVE 'W51316'   TO POSTSUM-FDNAMN                                    
031100     MOVE 'W51371D1' TO POSTSUM-DDNAMN2                                   
031200     CALL POSTSUM USING POSTSUM-PARM                                      
031300     .                                                                    
031400     EJECT                                                                
031410                                                                          
031420 S05-SKRIV-UTPOST2 SECTION.                                               
031421     MOVE SOUT-IDURVAL-INV       TO UT2-IDURVAL-INV                       
031422     MOVE SOUT-IDARTNR           TO UT2-IDARTNR                           
031423     MOVE '11'                   TO UT2-IDDC                              
031424     MOVE SOUT-ADLAGOMR          TO UT2-ADLAGOMR                          
031425     MOVE SOUT-ADGANG            TO UT2-ADGANG                            
031426     MOVE SOUT-ADPLATS           TO UT2-ADPLATS                           
031427     MOVE SOUT-KDVVKL            TO UT2-KDVVKL                            
031428     MOVE SOUT-KDPRODSL          TO UT2-KDPRODSL                          
031429     MOVE SOUT-IDFKNGRP          TO UT2-IDFKNGRP                          
031432     IF WS-DAINVDAT = 9999999                                             
031433       CONTINUE                                                           
031434     ELSE                                                                 
031435       WRITE UTPOST FROM UT2-AREA                                         
031440                                                                          
031450       MOVE SOUT-IDURVAL-INV TO POSTSUM-TRANSTYP                          
031460       MOVE 'W51371' TO POSTSUM-FDNAMN                                    
031470       MOVE 'W51371D3' TO POSTSUM-DDNAMN2                                 
031480       CALL POSTSUM USING POSTSUM-PARM                                    
031481     END-IF                                                               
031490     .                                                                    
031491     EJECT                                                                
031492                                                                          
031500 S99-ABEND SECTION.                                                       
031600     MOVE 'S' TO POSTSUM-OPKOD                                            
031700     CALL POSTSUM USING POSTSUM-PARM                                      
031800                                                                          
031900     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
032000     .                                                                    
032100     EJECT                                                                
032200*         * I M S  S E C T I O N                                          
032300                                                                          
032400 IMS-GET-WDK6         SECTION.                                            
032600     MOVE '  GAGKGBGAGK' TO GODK-STATUSKODER                              
032700     CALL CBLTDLI USING GN WDK6-PCB IO-AREA                               
032800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
032900     PERFORM IMS-STATUSKONTROLL                                           
033000     .                                                                    
033100     SKIP3                                                                
033110                                                                          
033200 IMS-STATUSKONTROLL   SECTION.                                            
033400     SET STATUS-IX TO 1                                                   
033500     SEARCH GODK-STATUS                                                   
033600       AT END CALL FELLOG                                                 
033700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
033800     END-SEARCH                                                           
033900     .                                                                    
