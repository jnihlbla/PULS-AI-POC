000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6128000.                                                
000300 AUTHOR.         KENT JEBSEN.                                             
000400 DATE-WRITTEN.   97/07/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        ANROPAR WDL6 OCH WDK6 OCH SUMMERAR IHOP AK-PÅVÄG SAMT            
000900*        AK-SDC.                                                          
001000*                                                                         
001100*        PROGRAMMET LÄSER      WLINLC (WDL6)                              
001200*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800* ÄNDRING:         97-12-01    JOHAN LINDKVIST                            
001900*                  FELAKTIGT PLACERAD USERABEND I SECTION B...            
002000*                                                                         
002100*                  99-03-17    JOHAN LINDKVIST                            
002200*                  LÄSER ÄVEN AV INLC-INL-PRARTNTO (STDPRIS)              
002300*                  FRÅN WDL6 EFTERSOM JAPAN OCH AUSTRALIEN                
002400*                  VILL HA PRISER OCH VÄRDEN PÅ W61282-00X                
002500*                  I SEK OCH INTE YEN/DOLLAR                              
002600*                                                                         
002700*                2007-07-22    CONNY EGHOLT   ETRACKER=5348482            
002800*                  ABENDAR PÅ 'GE' I B-HAMTA-PRIS-K6                      
002900*             -->  TILLFÄLLIG "SEMESTERTID-LÖSNING":                      
003000*                  BEROR PÅ MISMATCH PÅ WDK621 P.G.A. LEV-BYTE            
003100*                  LÄGGER IN DISPLAY OCH GÅR FÖRBI ABENDEN.               
003200*                2011-09-23    RAHUL REDDY                                
003201*                  ADDED CALL TO WDB6 TO GET ADCITY.                      
003202                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     SKIP2                                                                
003600 INPUT-OUTPUT SECTION.                                                    
003700                                                                          
003800 FILE-CONTROL.                                                            
003900     SKIP2                                                                
004000*       --- INFO FRÅN WDK6 OCH WDK7                                       
004100     SELECT W61278                     ASSIGN TO W61280D1.                
004200     SKIP2                                                                
004300*       --- SAMMA SOM IN, KOMPLETTERAD MED SUMMERAT AK-PAV O -SDC         
004400     SELECT W61280                     ASSIGN TO W61280D2.                
004500     EJECT                                                                
004600 DATA DIVISION.                                                           
004700     SKIP2                                                                
004800 FILE SECTION.                                                            
004900     SKIP3                                                                
005000 FD  W61278                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  -COPY W6127801      -L.                                              
005500     SKIP3                                                                
005600 FD  W61280                                                               
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900                                                                          
006000*01  POST -COPY W6128001 -PRE  UT-  -L.                                   
006100     EJECT                                                                
006200 WORKING-STORAGE SECTION.                                                 
006300                                                                          
006400*    -CHECKED BY WY2000                                                   
006500     SKIP3                                                                
006600 77  IDPGM                       PIC X(8)    VALUE 'W6128000'.            
006700 77  JA                          PIC X       VALUE 'J'.                   
006800 77  NEJ                         PIC X       VALUE 'N'.                   
006900 01  WS-STYCKPRIS                PIC S9(7)V9(2)      COMP-3.              
007000 01  WS-SUAKSV-SDC               PIC S9(9)V9(2)      COMP-3.              
007100 01  WS-SUAKSV-PAV               PIC S9(9)V9(2)      COMP-3.              
007110 01  WS-ADCITY                   PIC X(25).                               
007200                                                                          
007300 77  W61278-EOF-SW               PIC X       VALUE 'N'.                   
007400     88  END-OF-W61278                       VALUE 'J'.                   
007500                                                                          
007600 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
007700     EJECT                                                                
007800*      --- VALID IDDC CODES                                               
007900*                                                                         
008000*01    -COPY WWDC99                                                       
008100       EJECT                                                              
008200 01  DYNAMISKA-SUBPROGRAM.                                                
008300*                                                                         
008400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008700     SKIP2                                                                
008800 01  FELTEXT.                                                             
008900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009100     EJECT                                                                
009200*    --- PARAMETRAR TILL POSTSUM                                          
009300*                                                                         
009400*01  -COPY W0005   -PRE  POSTSUM-                                         
009500     EJECT                                                                
009600 01  IN-AREA-START               PIC X(24)   VALUE                        
009700                                 'IN-AREA-START  '.                       
009800     SKIP2                                                                
009900                                                                          
010000*01  AREA -COPY W6127801     -PRE IN-                                     
010100     EJECT                                                                
010200 01  UT-AREA-START               PIC X(24)   VALUE                        
010300                                 'UT-AREA-START  '.                       
010400     SKIP2                                                                
010500                                                                          
010600*01  AREA -COPY W6128001     -PRE UT-                                     
010700     EJECT                                                                
010800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010900*                                                                         
011000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011100     SKIP3                                                                
011200 01  NYCKLAR-TILL-DLI.                                                    
011300     03  W-IDARTNR-X.                                                     
011400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011500     03  W-IDLEVNR-21-X.                                                  
011600         05  W-IDLEVNR-21        PIC X(5)    VALUE LOW-VALUE.             
011700     03  W-DAPRLIST-21-N.                                                 
011800         05  W-DAPRLIST-21       PIC 9(8)    VALUE ZERO.                  
011900     03  W-IDDC-X.                                                        
012000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
040610     03  W-IDDC-K7-X.                                                     
040620         05  W-IDDC-K7           PIC X(2)    VALUE '71'.                  
041521     03  W-IDLEVNR-PR-X.                                                  
041522         05  W-IDLEVNR-PR        PIC X(5)    VALUE ZERO.                  
041530     03  W-DAPRLIST-K7-N.                                                 
041540         05  W-DAPRLIST-K7       PIC 9(8)    VALUE ZERO.                  
041541     SKIP2                                                                
041542*    --- STATUS-KOD FRÅN IMS                                              
041543 01  STATUS-WS                   PIC XX.                                  
041544     88  SEGMENT-FINNS                       VALUE '  '.                  
041545     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
041546     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
041547     88  SEGMENT-SLUT                        VALUE 'GB'.                  
041548     SKIP2                                                                
041549*    --- STATUS-KOD FRÅN IMS                                              
041550 01  STATUS-WS-ARTC              PIC XX.                                  
041551     88  SEGMENT-FINNS-ARTC                  VALUE '  '.                  
041552     88  SEGMENT-FINNS-REDAN-ARTC            VALUE 'II'.                  
041553     88  SEGMENT-SAKNAS-ARTC                 VALUE 'GE'.                  
041554     88  SEGMENT-SLUT-ARTC                   VALUE 'GB'.                  
041555     SKIP2                                                                
041556 01  STATUS-WS-INLC              PIC XX.                                  
041557     88  SEGMENT-FINNS-INLC                  VALUE '  '.                  
041559     88  SEGMENT-SAKNAS-INLC                 VALUE 'GE'.                  
041561     SKIP2                                                                
041562 01  GODK-STATUSKODER.                                                    
041563     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
041564     SKIP3                                                                
041565 01  SSA1                        PIC X(64).                               
041566 01  SSA2                        PIC X(64).                               
041567     EJECT                                                                
041568*    --- IMS FUNKTIONSKODER                                               
041569*01  -COPY W0003                                                          
041570     EJECT                                                                
041571*    ---  DLI INPUT-OUTPUT AREA                                           
041572 01  DLI-IO-AREA.                                                         
041573     03  IO-AREA                 PIC X(600)  VALUE SPACE.                 
041574     SKIP3                                                                
041575     03  WLINLC01 REDEFINES IO-AREA.                                      
041576*        05  -COPY WDL601  -PRE INLC-                                     
041577     EJECT                                                                
041578     03  WLINLC11 REDEFINES IO-AREA.                                      
041579*        05  -COPY WDL611  -PRE INLC-                                     
041580     EJECT                                                                
041581 01  DLI-IO-AREA2.                                                        
041582     03  IO-AREA2                PIC X(900)  VALUE SPACE.                 
041583     SKIP3                                                                
041584     03  WLARTC01 REDEFINES IO-AREA2.                                     
041585*        05  -COPY WDK601  -PRE ARTC-                                     
041586     EJECT                                                                
041587     03  WLARTC11 REDEFINES IO-AREA2.                                     
041588*        05  -COPY WDK611  -PRE ARTC-                                     
041589     EJECT                                                                
041590     03  WLARTC21 REDEFINES IO-AREA2.                                     
041591*        05  -COPY WDK621  -PRE ARTC-                                     
041592     EJECT                                                                
041593 01  DLI-IO-AREA3.                                                        
041594     03  IO-AREA3                PIC X(657)  VALUE SPACE.                 
041595     SKIP3                                                                
041596     03  WDB601   REDEFINES IO-AREA3.                                     
041600*        05  -COPY WDB601  -PRE WDB6-                                     
058310 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
058320     SKIP3                                                                
058330 01  DLI-IO-WDK711.                                                       
058340*        05  -COPY WDK711                                                 
058371     SKIP3                                                                
058372 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK724'.             
058373 01  DLI-IO-WDK724.                                                       
058374*        05  -COPY WDK724                                                 
058375     EJECT                                                                
058380                                                                          
058381 LINKAGE SECTION.                                                         
058382                                                                          
058383     EJECT                                                                
058384*01  -COPY W0008  -PRE INLC-                                              
058385     05  FILLER                  PIC X.                                   
058386     EJECT                                                                
058387*01  -COPY W0008  -PRE ARTC-                                              
058388     05  FILLER                  PIC X.                                   
058389     EJECT                                                                
058390*01  -COPY W0008  -PRE WDB6-                                              
058400     05  FILLER                  PIC X.                                   
058500     EJECT                                                                
058600*01  -COPY W0008  -PRE WDK7-                                              
058700     05  FILLER                  PIC X.                                   
058701     EJECT                                                                
058702 PROCEDURE DIVISION  USING INLC-PCB ARTC-PCB WDB6-PCB                     
058703                           WDK7-PCB.                                      
058704 MAIN SECTION.                                                            
058705     ENTRY 'DLITCBL' USING INLC-PCB ARTC-PCB WDB6-PCB                     
058706                           WDK7-PCB.                                      
058707                                                                          
058708     PERFORM A-INIT                                                       
058709                                                                          
058710     PERFORM S01-LAES-W61278                                              
058711     PERFORM UNTIL END-OF-W61278                                          
058712       MOVE ZERO TO WS-SUAKSV-SDC                                         
058713                    WS-SUAKSV-PAV                                         
058714       MOVE SPACES    TO WS-ADCITY                                        
058715       MOVE IN-IDDC   TO WS-IDDC                                          
058716                         W-IDDC                                           
058717       PERFORM IMS-GU-WDB601                                              
058718       IF SEGMENT-FINNS                                                   
058719         MOVE WDB6-DCS-ADCITY IN WDB6-DCS-ADPOST-PNRORT                   
058720                              TO WS-ADCITY                                
058721       END-IF                                                             
058722       IF IN-KVAKS-SDC > 0 OR IN-KVAKS-PAV > 0                            
058723         MOVE IN-IDARTNR TO W-IDARTNR                                     
058724         PERFORM IMS-GET-INLC01                                           
058725         PERFORM UNTIL SEGMENT-SAKNAS-INLC                                
058726           PERFORM IMS-GNP-INLC11                                         
058727           IF IN-IDDC = INLC-INL-IDDC                                     
058728             IF IN-KVAKS-SDC > 0                                          
058729               IF INLC-INL-IDPTYP = '310'                                 
058730                 MOVE INLC-INL-PRARTNTO TO WS-STYCKPRIS                   
058731                 PERFORM S02-SUMMERA-AKSV-SDC                             
058732               ELSE                                                       
058733                 IF INLC-INL-IDPTYP = 'R31'                               
058734                   PERFORM B-HAMTA-PRIS                                   
058735                   PERFORM S02-SUMMERA-AKSV-SDC                           
058736                 END-IF                                                   
058737               END-IF                                                     
058738             END-IF                                                       
058739             IF IN-KVAKS-PAV > 0                                          
058740               IF INLC-INL-IDPTYP = 'R30'                                 
058741                 PERFORM S03-SUMMERA-AKSV-PAV                             
058742               END-IF                                                     
058743             END-IF                                                       
058744           END-IF                                                         
058745         END-PERFORM                                                      
058746       END-IF                                                             
058747       PERFORM S04-FLYTTA-IN-TILL-UT                                      
058748       PERFORM S11-SKRIV-W61280                                           
058749       PERFORM S01-LAES-W61278                                            
058750     END-PERFORM                                                          
058751                                                                          
058752                                                                          
058753     PERFORM Z-FINIT                                                      
058754                                                                          
058755     MOVE ZERO TO RETURN-CODE                                             
058756     GOBACK                                                               
058757     .                                                                    
058758     EJECT                                                                
058759 A-INIT SECTION.                                                          
058760                                                                          
058761     OPEN INPUT  W61278                                                   
058762                                                                          
058763     OPEN OUTPUT W61280                                                   
058764                                                                          
058765     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
058766     .                                                                    
058767     EJECT                                                                
058768 B-HAMTA-PRIS SECTION.                                                    
058769                                                                          
058770     IF WDB6-DCS-CHINA                                                    
058780       PERFORM BA-HAMTA-PRIS-K7                                           
058781     ELSE                                                                 
058782       PERFORM BB-HAMTA-PRIS-K6                                           
058783     END-IF                                                               
058784     .                                                                    
058785     EJECT                                                                
058786                                                                          
058787 BA-HAMTA-PRIS-K7 SECTION.                                                
184308*    -- WDK711                                                            
184311     PERFORM IMS-GU-WDK711                                                
184312     IF SEGMENT-SAKNAS                                                    
184313       MOVE 'ARTIKELDATA STÄMMER EJ MELLAN L6 OCH K7'                     
184314                                            TO FELTEXT-STR                
184315       DISPLAY '***** FELTEXT ***** DATUM ' DAGENS-DATUM                  
184316       DISPLAY FELTEXT                                                    
184317       DISPLAY 'IDDC-WDL6 =' INLC-INL-IDDC                                
184318       DISPLAY 'LEVNR-WDL6=' W-IDLEVNR-PR                                 
184319       DISPLAY 'ARTNR-WDK7=' W-IDARTNR                                    
184320       DISPLAY '*************************************'                    
184321     ELSE                                                                 
184322                                                                          
184323*    -- WDK724                                                            
184324       MOVE INLC-INL-IDLEVNR   TO W-IDLEVNR-PR                            
184325       COMPUTE W-DAPRLIST-K7 = 99999999 - DAGENS-DATUM                    
184326       PERFORM IMS-GNP-WDK724                                             
184329       IF SEGMENT-FINNS                                                   
184332         MOVE SPRL-PRARTBES-PR  TO WS-STYCKPRIS                           
184334       ELSE                                                               
184335         MOVE 'ARTIKELDATA STÄMMER EJ MELLAN L6 OCH K7'                   
184336                                              TO FELTEXT-STR              
184337         DISPLAY '***** FELTEXT ***** DATUM ' DAGENS-DATUM                
184338         DISPLAY FELTEXT                                                  
184339         DISPLAY 'IDDC-WDL6 =' INLC-INL-IDDC                              
184340         DISPLAY 'LEVNR-WDL6=' W-IDLEVNR-PR                               
184341         DISPLAY 'ARTNR-WDK7=' W-IDARTNR                                  
184342         DISPLAY '*************************************'                  
184343       END-IF                                                             
184344     END-IF                                                               
184345     .                                                                    
184346     EJECT                                                                
184347                                                                          
184348 BB-HAMTA-PRIS-K6 SECTION.                                                
184349     PERFORM IMS-GU-ARTC11                                                
184350     IF SEGMENT-FINNS-ARTC                                                
184351       MOVE INLC-INL-IDLEVNR    TO W-IDLEVNR-21                           
184352       COMPUTE W-DAPRLIST-21 = 99999999 - DAGENS-DATUM                    
184353       PERFORM IMS-GET-ARTC-PRL                                           
184354       PERFORM UNTIL SEGMENT-SAKNAS-ARTC OR SEGMENT-SLUT-ARTC OR          
184355           ARTC-PRL-KDSTATUS-PR = 1                                       
184356         PERFORM IMS-GET-ARTC-PRL                                         
184357       END-PERFORM                                                        
184358       IF SEGMENT-SAKNAS-ARTC OR SEGMENT-SLUT-ARTC                        
184359          MOVE 'ARTIKELDATA STÄMMER EJ MELLAN L6 OCH K6'                  
184360                                               TO FELTEXT-STR             
184361          DISPLAY '***** FELTEXT ***** DATUM ' DAGENS-DATUM               
184362          DISPLAY FELTEXT                                                 
184363          DISPLAY 'IDDC-WDL6 =' INLC-INL-IDDC                             
184364          DISPLAY 'LEVNR-WDL6=' W-IDLEVNR-21                              
184365          DISPLAY 'ARTNR-WDK6=' W-IDARTNR                                 
184366          DISPLAY '*************************************'                 
184367*         CALL FELLOG                                                     
184368       ELSE                                                               
184369          MOVE ARTC-PRL-PRARTBES-PR TO WS-STYCKPRIS                       
184370       END-IF                                                             
184371     END-IF                                                               
184372     .                                                                    
184373     EJECT                                                                
184374 Z-FINIT SECTION.                                                         
184375     CLOSE W61278                                                         
184376           W61280                                                         
184377     SKIP2                                                                
184378     MOVE 'S'        TO POSTSUM-OPKOD                                     
184379     CALL POSTSUM USING POSTSUM-PARM                                      
184380     .                                                                    
184381     EJECT                                                                
184382 S01-LAES-W61278  SECTION.                                                
184383     READ W61278 INTO IN-AREA                                             
184384     AT END                                                               
184385        MOVE HIGH-VALUE   TO IN-AREA                                      
184386        SET END-OF-W61278 TO TRUE                                         
184387                                                                          
184388     NOT AT END                                                           
184389        MOVE 'W61278'   TO POSTSUM-FDNAMN                                 
184390        MOVE 'W61280D1' TO POSTSUM-DDNAMN2                                
184391        MOVE SPACE      TO POSTSUM-TRANSTYP                               
184392        CALL POSTSUM USING POSTSUM-PARM                                   
184393     END-READ                                                             
184394     .                                                                    
184395     EJECT                                                                
184396 S02-SUMMERA-AKSV-SDC  SECTION.                                           
184397                                                                          
184398     IF INLC-INL-PRKURS > 0                                               
184399        IF NDC-PACIFIC                                                    
184400          COMPUTE WS-SUAKSV-SDC = WS-SUAKSV-SDC + (INLC-INL-KVAVIS        
184401                        * IN-PRARTSTD )                                   
184402          END-COMPUTE                                                     
184403        ELSE                                                              
184404          COMPUTE WS-SUAKSV-SDC = WS-SUAKSV-SDC + (INLC-INL-KVAVIS        
184405                        * (WS-STYCKPRIS / INLC-INL-PRKURS))               
184406          END-COMPUTE                                                     
184407        END-IF                                                            
184408     END-IF                                                               
184409     .                                                                    
184410     EJECT                                                                
184411 S03-SUMMERA-AKSV-PAV  SECTION.                                           
184412                                                                          
184413     IF INLC-INL-PRKURS > 0                                               
184414        IF NDC-PACIFIC                                                    
184415          COMPUTE WS-SUAKSV-PAV = WS-SUAKSV-PAV + (INLC-INL-KVAVIS        
184416                        * IN-PRARTSTD )                                   
184417          END-COMPUTE                                                     
184418        ELSE                                                              
184419          COMPUTE WS-SUAKSV-PAV = WS-SUAKSV-PAV + (INLC-INL-KVAVIS        
184420                        * (INLC-INL-PRARTNTO / INLC-INL-PRKURS))          
184421        END-IF                                                            
184422     END-IF                                                               
184423     .                                                                    
184424     EJECT                                                                
184425 S04-FLYTTA-IN-TILL-UT SECTION.                                           
184426                                                                          
184427     MOVE IN-IDARTNR         TO UT-IDARTNR                                
184428     MOVE IN-IDDC            TO UT-IDDC                                   
184429     MOVE IN-KVLS            TO UT-KVLS                                   
184430     MOVE IN-KVRESS          TO UT-KVRESS                                 
184431     MOVE IN-KVOKS           TO UT-KVOKS                                  
184432     MOVE IN-KVUTRS          TO UT-KVUTRS                                 
184433     MOVE IN-KVROS           TO UT-KVROS                                  
184434     MOVE IN-KVEFRS          TO UT-KVEFRS                                 
184435     MOVE IN-KVBEART         TO UT-KVBEART                                
184436     MOVE IN-KVSPARR-KVAL    TO UT-KVSPARR-KVAL                           
184437     MOVE IN-PRAVCOST        TO UT-PRAVCOST                               
184438     MOVE IN-PRARTSTD        TO UT-PRARTSTD                               
184439     MOVE IN-IDPERSON-BUY    TO UT-IDPERSON-BUY                           
184440     MOVE WS-SUAKSV-PAV      TO UT-SUAKSV-PAV                             
184450     MOVE WS-SUAKSV-SDC      TO UT-SUAKSV-SDC                             
184451     MOVE WS-ADCITY          TO UT-ADCITY                                 
184452     .                                                                    
184453     EJECT                                                                
184454 S11-SKRIV-W61280 SECTION.                                                
184455                                                                          
184456     WRITE UT-POST FROM UT-AREA                                           
184457                                                                          
184458     MOVE 'W61280'   TO POSTSUM-FDNAMN                                    
184459     MOVE 'W61280D2' TO POSTSUM-DDNAMN2                                   
184460     CALL POSTSUM USING POSTSUM-PARM                                      
184461     .                                                                    
184462     EJECT                                                                
184463* --- IMS SEKTIONER ---                                                   
184464     SKIP3                                                                
184465 IMS-GET-INLC01  SECTION.                                                 
184466                                                                          
184467     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
184468          DELIMITED BY SIZE INTO SSA1                                     
184469     MOVE '  GE' TO GODK-STATUSKODER                                      
184470     CALL CBLTDLI USING GU INLC-PCB DLI-IO-AREA SSA1                      
184471     MOVE INLC-STATUS-CODE TO STATUS-WS-INLC                              
184472     PERFORM IMS-STATUSKONTROLL                                           
184473     .                                                                    
184474     EJECT                                                                
184475 IMS-GNP-INLC11 SECTION.                                                  
184476                                                                          
184477     MOVE 'WLINLC11 ' TO SSA1                                             
184478     MOVE '  GE' TO GODK-STATUSKODER                                      
184479     CALL CBLTDLI USING GNP INLC-PCB DLI-IO-AREA SSA1                     
184480     MOVE INLC-STATUS-CODE TO STATUS-WS-INLC                              
184481     PERFORM IMS-STATUSKONTROLL                                           
184482     .                                                                    
184483     EJECT                                                                
184484 IMS-GU-ARTC11 SECTION.                                                   
184485     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
184486          DELIMITED BY SIZE INTO SSA1                                     
184487     MOVE 'WLARTC11'          TO SSA2                                     
184488     MOVE '  GE' TO GODK-STATUSKODER                                      
184489     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA2 SSA1 SSA2                
184490     MOVE ARTC-STATUS-CODE TO STATUS-WS-ARTC                              
184491     PERFORM IMS-STATUSKONTROLL-ARTC                                      
184492     .                                                                    
184493     EJECT                                                                
184494 IMS-GET-ARTC-PRL SECTION.                                                
184495                                                                          
184496     STRING 'WLARTC21(DAPRLIST>=' W-DAPRLIST-21-N                         
184497                    '&IDLEVNR  =' W-IDLEVNR-21-X ')'                      
184498          DELIMITED BY SIZE INTO SSA1                                     
184499     MOVE '  GEGB' TO GODK-STATUSKODER                                    
184500     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA2 SSA1                    
184501     MOVE ARTC-STATUS-CODE TO STATUS-WS-ARTC                              
184502     PERFORM IMS-STATUSKONTROLL-ARTC                                      
184503     .                                                                    
184504     EJECT                                                                
184505 IMS-GU-WDB601    SECTION.                                                
184506     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
184507          DELIMITED BY SIZE INTO SSA1                                     
184508     MOVE '  GE' TO GODK-STATUSKODER                                      
184509     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA3 SSA1                     
184510     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
184520     PERFORM IMS-STATUSKONTROLL                                           
184530     .                                                                    
184540     EJECT                                                                
268836 IMS-GU-WDK711 SECTION.                                                   
268837     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
268838          DELIMITED BY SIZE INTO SSA1                                     
268839     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
268840          DELIMITED BY SIZE INTO SSA2                                     
268841     MOVE '  GE' TO GODK-STATUSKODER                                      
268842     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711                         
268843          SSA1 SSA2                                                       
268844     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
268845     PERFORM IMS-STATUSKONTROLL                                           
268846     .                                                                    
268865                                                                          
268866 IMS-GNP-WDK724 SECTION.                                                  
268867     STRING 'WDK724  (DAPRLIST>=' W-DAPRLIST-K7-N                         
268868                    '&IDLEVNRP =' W-IDLEVNR-PR-X ')'                      
268870          DELIMITED BY SIZE INTO SSA1                                     
268871     MOVE '  GE' TO GODK-STATUSKODER                                      
268872     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK724 SSA1                  
268873     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
268874     PERFORM IMS-STATUSKONTROLL                                           
268875     .                                                                    
268876     EJECT                                                                
268877                                                                          
268878 IMS-STATUSKONTROLL SECTION.                                              
268879                                                                          
268880     SET STATUS-IX TO 1                                                   
268881     SEARCH GODK-STATUS                                                   
268882       AT END                                                             
268883         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
268884           DELIMITED BY SIZE INTO FELTEXT                                 
268885         DISPLAY FELTEXT                                                  
268886         CALL FELLOG                                                      
268887       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
268888         CONTINUE                                                         
268889     END-SEARCH                                                           
268890     .                                                                    
268891     EJECT                                                                
268892 IMS-STATUSKONTROLL-ARTC SECTION.                                         
268893                                                                          
268894     SET STATUS-IX TO 1                                                   
268895     SEARCH GODK-STATUS                                                   
268896       AT END                                                             
268897         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS-ARTC           
268898           DELIMITED BY SIZE INTO FELTEXT                                 
268899         DISPLAY FELTEXT                                                  
268900         CALL FELLOG                                                      
268901       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS-ARTC                      
268902         CONTINUE                                                         
268903     END-SEARCH                                                           
268904     .                                                                    
268905     EJECT                                                                
