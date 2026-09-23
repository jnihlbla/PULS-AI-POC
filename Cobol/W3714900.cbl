000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W3714900.                                                
000400 AUTHOR.         RONNY STENHOLM.                                          
000500 DATE-WRITTEN.   96/09/16.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        HÄMTAR    LEVERANTÖRSNUMMER                                      
001000*                                                                         
001100*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
001200*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- REGISTRET                                                  
002700     SELECT W37148                     ASSIGN TO W37149D1.                
002800*          --- UTFIL                                                      
002900     SELECT W37149                     ASSIGN TO W37149D2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W37148                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY W37138      -L.                                                
004000     EJECT                                                                
004100 FD  W37149                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  POST -COPY W37149 -PRE  UT-  -L.                                     
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004801*    -- CHECKED BY WY2000                                                 
004810     SKIP3                                                                
004900 77  IDPGM                       PIC X(8)    VALUE 'W3714900'.            
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200                                                                          
005300 77  W37148-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W37148                       VALUE 'J'.                   
005500     EJECT                                                                
005600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005700 01  FILLER REDEFINES DAGENS-DATUM.                                       
005800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006100     EJECT                                                                
006200 01  DYNAMISKA-SUBPROGRAM.                                                
006300*                                                                         
006400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006800     SKIP2                                                                
006900*    --- PARAMETRAR TILL ABEND                                            
007000                                                                          
007100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007300     SKIP2                                                                
007400 01  FELTEXT.                                                             
007500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007700     EJECT                                                                
007800*    --- VALID IDDC CODES                                                 
007900*                                                                         
008000*01  -COPY WWDC99                                                         
008100     EJECT                                                                
008110*    --- PARAMETRAR TILL POSTSUM                                          
008120*                                                                         
008130*01  -COPY W0005   -PRE  POSTSUM-                                         
008140     EJECT                                                                
008200 01  IN-AREA-START               PIC X(24)   VALUE                        
008300                                 'IN-AREA-START  '.                       
008400     SKIP2                                                                
008500                                                                          
008600*01  AREA -COPY W37138     -PRE IN-                                       
008700     EJECT                                                                
008800 01  UT-AREA-START             PIC X(24)   VALUE                          
008900                                             'UT-AREA-START'.             
009000*01  AREA -COPY W37149     -PRE UT-                                       
009100     EJECT                                                                
009200     SKIP2                                                                
009300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009400*                                                                         
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009700     SKIP3                                                                
009800 01  NYCKLAR-TILL-DLI.                                                    
010100     03  W-KDSEGKEY-X.                                                    
010200         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
010300     03  W-IDARTNR-X.                                                     
010400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010500     03  W-IDDC-X.                                                        
010600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010700     SKIP2                                                                
010800*    --- STATUS-KOD FRÅN IMS                                              
010900 01  STATUS-WS                   PIC XX.                                  
011000     88  SEGMENT-FINNS                       VALUE '  '.                  
011100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011300     SKIP2                                                                
011400 01  GODK-STATUSKODER.                                                    
011500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011600     SKIP3                                                                
011700 01  SSA1                        PIC X(64).                               
011800 01  SSA2                        PIC X(64).                               
011900     EJECT                                                                
012000*    --- IMS FUNKTIONSKODER                                               
012100*01  -COPY W0003                                                          
012200     EJECT                                                                
012300*    ---  DLI INPUT-OUTPUT AREA                                           
012400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012500     SKIP3                                                                
012600 01  DLI-IO-AREA.                                                         
012700     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
012800     SKIP3                                                                
012900     03  WLARTC01 REDEFINES IO-AREA.                                      
013000*        05  -COPY WDK601  -PRE ARTC-                                     
013100     SKIP3                                                                
013200     03  WLARTC11 REDEFINES IO-AREA.                                      
013300*        05  -COPY WDK611  -PRE ARTC-                                     
013400     SKIP3                                                                
013500     03  WLARTS01 REDEFINES IO-AREA.                                      
013600*        05  -COPY WDK701  -PRE ARTS-                                     
013700     SKIP3                                                                
013800     03  WLARTS11 REDEFINES IO-AREA.                                      
013900*        05  -COPY WDK711  -PRE ARTS-                                     
014000     EJECT                                                                
014100 LINKAGE SECTION.                                                         
014200                                                                          
014300     EJECT                                                                
014400*01  -COPY W0008  -PRE ARTC-                                              
014500     05  FILLER                  PIC X.                                   
014600     EJECT                                                                
014700*01  -COPY W0008  -PRE ARTS-                                              
014800     05  FILLER                  PIC X.                                   
014900     EJECT                                                                
015000 PROCEDURE DIVISION  USING ARTC-PCB ARTS-PCB.                             
015100 MAIN SECTION.                                                            
015200     ENTRY 'DLITCBL' USING ARTC-PCB ARTS-PCB.                             
015300                                                                          
015400                                                                          
015500     PERFORM A-INIT                                                       
015600     PERFORM S01-LAES-W37148                                              
015700     PERFORM UNTIL END-OF-W37148                                          
015800                                                                          
015810*      MOVE IN-IDDC           TO WS-IDDC                                  
015900*      IF NDC                                                             
016000         IF IN-KVRETUR-GODK > 0                                           
016100           PERFORM B-HAEMTA-IDLEVNR                                       
016110           PERFORM C-FLYTTA-UT-RESTEN                                     
016200         END-IF                                                           
016300*      END-IF                                                             
016400       PERFORM S01-LAES-W37148                                            
016500     END-PERFORM                                                          
016600                                                                          
016700                                                                          
016800     PERFORM Z-FINIT                                                      
016900                                                                          
017000     MOVE ZERO TO RETURN-CODE                                             
017100     GOBACK                                                               
017200     .                                                                    
017300     EJECT                                                                
017400 A-INIT SECTION.                                                          
017500                                                                          
017600     OPEN INPUT  W37148                                                   
017610     OPEN OUTPUT W37149                                                   
017700                                                                          
017800     ACCEPT DAGENS-DATUM  FROM DATE                                       
017900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018000     .                                                                    
018100     EJECT                                                                
018200 B-HAEMTA-IDLEVNR SECTION.                                                
018210     SKIP2                                                                
018300     MOVE IN-IDARTNR-OBJ TO W-IDARTNR                                     
018400     MOVE IN-IDDC        TO W-IDDC                                        
018500     PERFORM IMS-GET-ARTS-SLAGER                                          
018600     IF SEGMENT-FINNS                                                     
018700       MOVE ARTS-SLAG-IDLEVNR TO UT-IDLEVNR                               
018800     ELSE                                                                 
018900       PERFORM IMS-GET-ARTC-CLAGER                                        
018910       IF SEGMENT-FINNS                                                   
018920         MOVE ARTC-CLAG-IDLEVNR-SEN TO UT-IDLEVNR                         
018930       ELSE                                                               
018940         MOVE SPACE                 TO UT-IDLEVNR                         
019000       END-IF                                                             
019010     END-IF                                                               
019100     .                                                                    
019200     EJECT                                                                
019201 C-FLYTTA-UT-RESTEN SECTION.                                              
019209     MOVE IN-IDGMTREF          TO UT-IDGMTREF                             
019233     MOVE IN-IDBYTRAP          TO UT-IDBYTRAP                             
019235     MOVE IN-TIREGDAT-DEALER   TO UT-TIREGDAT-DEALER                      
019237     MOVE IN-IDFAKT            TO UT-IDFAKT                               
019239     MOVE IN-IDBYTRAD          TO UT-IDBYTRAD                             
019241     MOVE IN-IDARTNR-OBJ       TO UT-IDARTNR-OBJ                          
019243     MOVE IN-IDTABNR           TO UT-IDTABNR                              
019245     MOVE IN-BEART-SVE         TO UT-BEART-SVE                            
019247     MOVE IN-BEART-ENG         TO UT-BEART-ENG                            
019249     MOVE IN-IDFKNGRP          TO UT-IDFKNGRP                             
019251     MOVE IN-KVRETUR-URSP      TO UT-KVRETUR-URSP                         
019253     MOVE IN-KVRETUR-GODK      TO UT-KVRETUR-GODK                         
019255     MOVE IN-KDBYTSTA-OBJ      TO UT-KDBYTSTA-OBJ                         
019257     MOVE IN-TIANKDAG          TO UT-TIANKDAG                             
019259     MOVE IN-TIREGDAT-GODK     TO UT-TIREGDAT-GODK                        
019263     MOVE IN-IDUSER            TO UT-IDUSER                               
019265     MOVE IN-BERADREF          TO UT-BERADREF                             
019267     MOVE IN-KDBYTREF          TO UT-KDBYTREF                             
019269     MOVE IN-IDDC              TO UT-IDDC                                 
019290                                                                          
019291     PERFORM S10-SKRIV-W37149-BYTES                                       
019292     .                                                                    
019293     EJECT                                                                
019300 Z-FINIT SECTION.                                                         
019400     CLOSE W37148                                                         
019410           W37149                                                         
019500     SKIP2                                                                
019600     MOVE 'S' TO POSTSUM-OPKOD                                            
019700     CALL POSTSUM USING POSTSUM-PARM                                      
019800     .                                                                    
019900     EJECT                                                                
020000 S01-LAES-W37148  SECTION.                                                
020100     READ W37148 INTO IN-AREA                                             
020200     AT END                                                               
020300*        MOVE HIGH-VALUE TO IN-ID                                         
020400        SET END-OF-W37148 TO TRUE                                         
020500                                                                          
020600     NOT AT END                                                           
020700        MOVE 'W37148'   TO POSTSUM-FDNAMN                                 
020800        MOVE 'W37149D1' TO POSTSUM-DDNAMN2                                
020900        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
021000        CALL POSTSUM USING POSTSUM-PARM                                   
021100     END-READ                                                             
021200     .                                                                    
021300     EJECT                                                                
021400 S10-SKRIV-W37149-BYTES SECTION.                                          
021500     SKIP2                                                                
021600     WRITE UT-POST   FROM UT-AREA                                         
021700                                                                          
021800     MOVE  'UT'      TO POSTSUM-TRANSTYP                                  
021900     MOVE 'W37149 '  TO POSTSUM-FDNAMN                                    
022000     MOVE 'W37149D2' TO POSTSUM-DDNAMN2                                   
022100     CALL POSTSUM USING POSTSUM-PARM                                      
022200     .                                                                    
022300     EJECT                                                                
022400 S99-ABEND SECTION.                                                       
022500                                                                          
022600     SKIP2                                                                
022700     MOVE 'S' TO POSTSUM-OPKOD                                            
022800     CALL POSTSUM USING POSTSUM-PARM                                      
022900     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
023000     .                                                                    
023100     EJECT                                                                
023200* --- IMS SEKTIONER ---                                                   
023300     SKIP3                                                                
023400     EJECT                                                                
023500 IMS-GET-ARTC-ROT SECTION.                                                
023600                                                                          
023700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
023800          DELIMITED BY SIZE INTO SSA1                                     
023900     MOVE '  GE' TO GODK-STATUSKODER                                      
024000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
024100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
024200     PERFORM IMS-STATUSKONTROLL                                           
024300     .                                                                    
024400     EJECT                                                                
024500 IMS-GET-ARTC-CLAGER SECTION.                                             
024600                                                                          
024700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
024800          DELIMITED BY SIZE INTO SSA1                                     
024900     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
025000          DELIMITED BY SIZE INTO SSA2                                     
025100     MOVE '  GE' TO GODK-STATUSKODER                                      
025200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1 SSA2                 
025300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
025400     PERFORM IMS-STATUSKONTROLL                                           
025500     .                                                                    
025600     EJECT                                                                
026700 IMS-GET-ARTS-SLAGER SECTION.                                             
026800                                                                          
026900     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
027000          DELIMITED BY SIZE INTO SSA1                                     
027100     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
027200          DELIMITED BY SIZE INTO SSA2                                     
027300     MOVE '  GE' TO GODK-STATUSKODER                                      
027400     CALL CBLTDLI USING GU  ARTS-PCB DLI-IO-AREA SSA1 SSA2                
027500     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
027600     PERFORM IMS-STATUSKONTROLL                                           
027700     .                                                                    
027800     EJECT                                                                
027900 IMS-STATUSKONTROLL SECTION.                                              
028000                                                                          
028100     SET STATUS-IX TO 1                                                   
028200     SEARCH GODK-STATUS                                                   
028300       AT END                                                             
028400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
028500           DELIMITED BY SIZE INTO FELTEXT                                 
028600         DISPLAY FELTEXT                                                  
028700         CALL FELLOG                                                      
028800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
028900         CONTINUE                                                         
029000     END-SEARCH                                                           
029100     .                                                                    
