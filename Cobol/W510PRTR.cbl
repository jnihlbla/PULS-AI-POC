000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.     W510PRTR                                                 
000500 AUTHOR.         ANDERS HENRIKSSON                                        
000600 DATE-WRITTEN.   2016-02-12                                               
000700 DATE-COMPILED.                                                           
000800*    FUNKTION:                                                            
000900*        UNDERSÖKER OM NYA PRISRADER                                      
001000*        SKALL ÖVERFÖRAS FRÅN WDK724 TILL WDK621                          
001100*                                                                         
001110*        UNDERSÖKER OM BORTTAG AV PRISRADER FRÅN WDK724                   
001200*        ÄVEN SKALL TA BORT MOTSVARANDE RAD PÅ WDK621                     
001210*                                                                         
001220*        UNDERSÖK OM PRISET PÅ WDK611 ÄR TIPPAT                           
001221*        DÅ SKALL PRISRAD ÖVERFÖRA PRISER WDK611                          
001230*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002100 INPUT-OUTPUT SECTION.                                                    
002300 FILE-CONTROL.                                                            
002500 DATA DIVISION.                                                           
002700 FILE SECTION.                                                            
002800                                                                          
002900 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(8)    VALUE 'W510PRTR'.            
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003610 77  PRICEROW-EXIST              PIC X       VALUE 'N'.                   
003611 77  FLSLUTA-LAS                 PIC X       VALUE 'N'.                   
003612 77  INDX                        PIC S9(9)   VALUE +0 COMP SYNC.          
003620                                                                          
003700 01  W-ARBETS-AREOR.                                                      
003800                                                                          
004700     03  DAGENS-DATUM            PIC 9(6)    VALUE ZERO.                  
004710                                                                          
004800 01  DATUM-FAELT.                                                         
004900     03  W-TIPRLIST          PIC 9(6).                                    
005000     03  W-DAPRLIST-MAX      PIC 9(8)    VALUE 99999999.                  
005100     03  W-DAPRLIST          PIC 9(8).                                    
005110     03  W-DAPRLIST-X  REDEFINES W-DAPRLIST.                              
005120      05 FILLER              PIC 9(2).                                    
005130      05 W-LISTDATUM         PIC 9(6).                                    
005200                                                                          
005300     EJECT                                                                
005400*    -- VALID IDDC CODES                                                  
005500*                                                                         
005600*01  -COPY WWDC99                                                         
005601                                                                          
005610*    -COPY WY2000W1                                                       
005700                                                                          
006000     EJECT                                                                
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006700     SKIP2                                                                
006800                                                                          
007000*    --- PARAMETRAR TILL ABEND                                            
007200 01  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007300 01  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007400     SKIP2                                                                
007500 01  FELTEXT.                                                             
007600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007800     EJECT                                                                
007900                                                                          
008000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008400                                                                          
008500 01  NYCKLAR-TILL-DLI.                                                    
008600     03  W-IDARTNR-X.                                                     
008700         05  W-IDARTNR       PIC S9(9)   VALUE +0  COMP-3.                
008800     03  W-KDSEGKEY-X.                                                    
008900         05  W-KDSEGKEY      PIC X(1)    VALUE '1'.                       
009000     03   W-WDK621KY-X.                                                   
009100       05  W-DAPRLIST-9KOMPL PIC 9(8)    VALUE ZERO.                      
009200       05  W-IDLEVNR-21      PIC X(5)    VALUE LOW-VALUE.                 
009400     SKIP2                                                                
009500*    --- STATUS-KOD FRÅN IMS                                              
009600 01  STATUS-WS                   PIC XX.                                  
009700     88  SEGMENT-FINNS                       VALUE '  '.                  
009800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010000     SKIP2                                                                
010100 01  GODK-STATUSKODER.                                                    
010200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010300     SKIP3                                                                
010400 01  SSA1                        PIC X(64).                               
010500 01  SSA2                        PIC X(64).                               
010600     EJECT                                                                
010700*    --- IMS FUNKTIONSKODER                                               
010800*01  -COPY W0003                                                          
010900     EJECT                                                                
011000*    ---  DLI INPUT-OUTPUT AREA                                           
011100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011200     SKIP2                                                                
011300                                                                          
011410 01  FILLER                    PIC X(16)  VALUE 'WDK601'.                 
011430*01  WDK601 -COPY WDK601                                                  
011440     EJECT                                                                
011441                                                                          
011450 01  FILLER                    PIC X(16)  VALUE 'WDK611'.                 
011470*01  WDK611 -COPY WDK611                                                  
011480     EJECT                                                                
011481                                                                          
011490 01  FILLER                    PIC X(16)  VALUE 'WDK621'.                 
011492*01  WDK621 -COPY WDK621                                                  
011493     EJECT                                                                
011500                                                                          
011510 01  FILLER                    PIC X(16)  VALUE 'WDK629'.                 
011520*01  WDK629 -COPY WDK629                                                  
011530     EJECT                                                                
011540                                                                          
011600 LINKAGE SECTION.                                                         
011700     SKIP2                                                                
011800 01  W510PRTR-AREA.                                                       
011900*    03  -COPY W510PRTR                                                   
012000     EJECT                                                                
012100*01  -COPY W0008  -PRE WDK6-                                              
012200     05  FILLER                  PIC X.                                   
012300     EJECT                                                                
012400                                                                          
012500 PROCEDURE DIVISION  USING W510PRTR-AREA  WDK6-PCB.                       
012600 MAIN SECTION.                                                            
012700                                                                          
012800     ACCEPT DAGENS-DATUM  FROM DATE                                       
012900     MOVE SPACE               TO PRTR-KDSVAR                              
013600                                                                          
013700     EVALUATE PRTR-KDCALL                                                 
013800                                                                          
013900       WHEN 010                                                           
014000          PERFORM A-CHECK-SEND-PR-TO-5111-CDC                             
014100       WHEN 015                                                           
014200          PERFORM A-CHECK-SEND-PR-TO-5111-BOUNCE                          
014300       WHEN 020                                                           
014400          PERFORM B-CHECK-SEND-DEL-TO-5112                                
014500       WHEN 030                                                           
014600          PERFORM C-CHECK-TIPPAT-TO-5111                                  
015900       WHEN OTHER                                                         
016000*** FEL KOD PÅ KDCALL (KDCALL) FINNS EJ                                   
016100          MOVE '2'        TO PRTR-KDSVAR                                  
016200     END-EVALUATE                                                         
016300                                                                          
016400     MOVE ZERO TO RETURN-CODE                                             
016500     GOBACK                                                               
016600     .                                                                    
016700     EJECT                                                                
016710                                                                          
016800 A-CHECK-SEND-PR-TO-5111-CDC SECTION.                                     
017000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
017100* VALIDERA OM MAN SKALL SKICKA PRISRADEN TILL 5111 PROGRAMMET   *         
017200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
017800     MOVE PRTR-IDARTNR        TO W-IDARTNR                                
018000     PERFORM IMS-GU-WDK601                                                
018100     IF SEGMENT-FINNS                                                     
018110       PERFORM IMS-GNP-WDK611                                             
018120       IF SEGMENT-FINNS                                                   
018121         MOVE PRTR-IDDC       TO WS-IDDC                                  
018122         IF NDC-CN                                                        
018123         OR NDC-US                                                        
018124           MOVE CLAG-IDDC-REF TO WS-IDDC                                  
018125           IF NDC-CN                                                      
018126           OR NDC-US                                                      
018127             MOVE '1'         TO PRTR-KDSVAR                              
018130           ELSE                                                           
018140             MOVE '2'         TO PRTR-KDSVAR                              
018150           END-IF                                                         
018151         ELSE                                                             
018152           MOVE '2'           TO PRTR-KDSVAR                              
018153         END-IF                                                           
018160       ELSE                                                               
018170         MOVE '2'             TO PRTR-KDSVAR                              
018180       END-IF                                                             
018200     ELSE                                                                 
018210       MOVE '2'               TO PRTR-KDSVAR                              
018300     END-IF                                                               
021700     .                                                                    
021800     EJECT                                                                
021900                                                                          
022000 A-CHECK-SEND-PR-TO-5111-BOUNCE SECTION.                                  
022100* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
022200* VALIDERA OM MAN SKALL SKICKA PRISRADEN TILL 5111 PROGRAMMET   *         
022300* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
022400     MOVE PRTR-IDARTNR        TO W-IDARTNR                                
022500     PERFORM IMS-GU-WDK601                                                
022600     IF SEGMENT-FINNS                                                     
022700       PERFORM IMS-GNP-WDK611                                             
022800       IF SEGMENT-FINNS                                                   
022900         MOVE PRTR-IDDC       TO WS-IDDC                                  
023000         IF NDC-CN                                                        
023100         OR NDC-US                                                        
023200           MOVE CLAG-IDDC-REF TO WS-IDDC                                  
023300           IF NDC-CN                                                      
023400           OR NDC-US                                                      
023500             MOVE '1'         TO PRTR-KDSVAR                              
023600           ELSE                                                           
023700             MOVE '1'         TO PRTR-KDSVAR                              
023800           END-IF                                                         
023900         ELSE                                                             
024000           MOVE '2'           TO PRTR-KDSVAR                              
024100         END-IF                                                           
024200       ELSE                                                               
024300         MOVE '2'             TO PRTR-KDSVAR                              
024400       END-IF                                                             
024500     ELSE                                                                 
024600       MOVE '2'               TO PRTR-KDSVAR                              
024700     END-IF                                                               
024800     .                                                                    
024900     EJECT                                                                
025000                                                                          
026800 B-CHECK-SEND-DEL-TO-5112 SECTION.                                        
027000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
027100* VALIDERA OM MAN SKALL DELETA PRISRADEN FRÅN 5112 PROGRAMMET   *         
027200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
027311     MOVE PRTR-IDARTNR        TO W-IDARTNR                                
027312     PERFORM IMS-GU-WDK601                                                
027313     IF SEGMENT-FINNS                                                     
027314       PERFORM IMS-GNP-WDK611                                             
027315       IF SEGMENT-FINNS                                                   
027316         MOVE PRTR-IDDC       TO WS-IDDC                                  
027317         IF NDC-CN                                                        
027318         OR NDC-US                                                        
027319           MOVE CLAG-IDDC-REF TO WS-IDDC                                  
027320           IF NDC-CN                                                      
027321           OR NDC-US                                                      
027322             MOVE NEJ TO PRICEROW-EXIST                                   
027323             MOVE NEJ TO FLSLUTA-LAS                                      
027324             PERFORM BA-CHECK-IF-PRICEROW-EXIST                           
027325             IF PRICEROW-EXIST = JA                                       
027326               MOVE '1'       TO PRTR-KDSVAR                              
027327             ELSE                                                         
027328               MOVE '2'       TO PRTR-KDSVAR                              
027329             END-IF                                                       
027330           ELSE                                                           
027331             MOVE '3'         TO PRTR-KDSVAR                              
027332           END-IF                                                         
027333         ELSE                                                             
027334           MOVE '4'           TO PRTR-KDSVAR                              
027335         END-IF                                                           
027336       ELSE                                                               
027337         MOVE '5'             TO PRTR-KDSVAR                              
027338       END-IF                                                             
027339     ELSE                                                                 
027340       MOVE '6'               TO PRTR-KDSVAR                              
027341     END-IF                                                               
027342     .                                                                    
027350     EJECT                                                                
071800                                                                          
071810 BA-CHECK-IF-PRICEROW-EXIST SECTION.                                      
071820* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
071830* VALIDERA OM MAN SKALL DELETA PRISRADEN FRÅN 5112 PROGRAMMET   *         
071840* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
071900* --- IMS SEKTIONER ---                                                   
071902* **       HÄR LÄSES BEST.PRIS SEGMENT FÖR ATT               **           
071903* **       KONTROLLERA OM PRIS FÖR ANGIVET                   **           
071904* **       DATUM FINNS LAGRAT OCH KAN TAS BORT               **           
071905                                                                          
071906     MOVE 1 TO INDX                                                       
071907     PERFORM UNTIL SEGMENT-SAKNAS OR INDX > 5 OR                          
071908                   FLSLUTA-LAS = JA                                       
071909       PERFORM IMS-GNP-WDK621                                             
071910                                                                          
071911       IF SEGMENT-FINNS                                                   
071912         IF PRL-IDLEVNR = PRTR-IDLEVNR                                    
071913           SUBTRACT PRL-DAPRLIST-9KOMPL FROM W-DAPRLIST-MAX               
071914           GIVING W-DAPRLIST                                              
071915           MOVE PRTR-TIPRLIST    TO TMP1-YYMMDD                           
071916           MOVE W-LISTDATUM      TO TMP2-YYMMDD                           
071918           IF TMP1-YYMMDD = TMP2-YYMMDD                                   
071920             IF PRL-SUINLEV-PR > 0                                        
071921               MOVE NEJ TO PRICEROW-EXIST                                 
071922             ELSE                                                         
071923               MOVE JA  TO PRICEROW-EXIST                                 
071924             END-IF                                                       
071925             MOVE JA TO FLSLUTA-LAS                                       
071926           ELSE                                                           
071934             ADD  1 TO INDX                                               
071937           END-IF                                                         
071938         ELSE                                                             
071939           ADD  1 TO INDX                                                 
071940         END-IF                                                           
071945       END-IF                                                             
071947     END-PERFORM                                                          
071948     .                                                                    
071949     EJECT                                                                
071950                                                                          
071951 C-CHECK-TIPPAT-TO-5111  SECTION.                                         
071952* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
071953* VALIDERA OM MAN SKALL SKICKA TIPPAT  TILL 5111 PROGRAMMET   *           
071954* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
071955     MOVE PRTR-IDARTNR        TO W-IDARTNR                                
071956     PERFORM IMS-GU-WDK601                                                
071957     IF SEGMENT-FINNS                                                     
071958       PERFORM IMS-GNP-WDK611                                             
071959       IF SEGMENT-FINNS                                                   
071960         IF CLAG-KDTIPPR = 1                                              
071966           MOVE '2'         TO PRTR-KDSVAR                                
071967         ELSE                                                             
071968           MOVE '1'         TO PRTR-KDSVAR                                
071969         END-IF                                                           
071970       ELSE                                                               
071971         MOVE '2'           TO PRTR-KDSVAR                                
071972       END-IF                                                             
071976     ELSE                                                                 
071977       MOVE '2'             TO PRTR-KDSVAR                                
071978     END-IF                                                               
071979     .                                                                    
071980     EJECT                                                                
071990                                                                          
072100 IMS-GU-WDK601 SECTION.                                                   
072300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
072400     DELIMITED  BY SIZE INTO SSA1                                         
072500     MOVE '  GE' TO GODK-STATUSKODER                                      
072600     CALL CBLTDLI USING GU WDK6-PCB WDK601 SSA1                           
072700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
072800     PERFORM IMS-STATUSKONTROLL                                           
072900     .                                                                    
073000     SKIP3                                                                
073010                                                                          
073100 IMS-GNP-WDK611 SECTION.                                                  
073210     MOVE 'WDK611   ' TO SSA1                                             
073220     MOVE '    ' TO GODK-STATUSKODER                                      
073230     CALL CBLTDLI USING GNP WDK6-PCB WDK611 SSA1                          
073240     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
073250     PERFORM IMS-STATUSKONTROLL                                           
073260     .                                                                    
073270     EJECT                                                                
073271                                                                          
073280 IMS-GNP-WDK621 SECTION.                                                  
073292     MOVE 'WDK621  ' TO SSA1                                              
073293     MOVE '  GE' TO GODK-STATUSKODER                                      
073294     CALL CBLTDLI USING GNP WDK6-PCB WDK621 SSA1                          
073295     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
073296     PERFORM IMS-STATUSKONTROLL                                           
073297     .                                                                    
073298     SKIP3                                                                
073299                                                                          
073320 IMS-STATUSKONTROLL SECTION.                                              
073500     SET STATUS-IX TO 1                                                   
073600     SEARCH GODK-STATUS                                                   
073700       AT END                                                             
073800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
073900           DELIMITED BY SIZE INTO FELTEXT                                 
074000         DISPLAY FELTEXT                                                  
074100         CALL FELLOG                                                      
074200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
074300         CONTINUE                                                         
074400     END-SEARCH                                                           
074500     .                                                                    
