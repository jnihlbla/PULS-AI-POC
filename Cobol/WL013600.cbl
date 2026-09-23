000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL013600.                                                
000300 AUTHOR.         GÖRAN KJELLSON   GUIDE                                   
000400 DATE-WRITTEN.   JULI  2005                                               
000500 DATE-COMPILED.                                                           
000600*    NAME:       'CARPARTS.LDC.QUALITYBLOCKING'                           
000700*                                                                         
000800*        WL013600 PROGRAM IS A REPLICA OF W6021600 PROGRAM                
000900*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
001000*                                                                         
001100*                                                                         
001200*    FUNKTION:                                                            
001300*        LÄSER KVALITETS SPÄRRADE ARTIKLAR.                               
001400*        BESTÄLLNING AV LISTA W614S1.                                     
001500*                                                                         
001600*        PROGRAMMET LÄSER      WDR5                                       
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: WL0136T                                             
002000*        REQUEST:     WL0136I1                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        RESPONS:     WL0136O1                                            
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 DATA DIVISION.                                                           
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200 77  IDPGM                       PIC X(08)   VALUE 'WL013600'.            
003300 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600 77  CURR-SECTION                PIC X(16)   VALUE 'MAIN'.                
003700 77  CURR-IMS-SECTION            PIC X(16)   VALUE SPACE.                 
003800 77  KDRC-DISPLAY                PIC Z(5).                                
003900 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
004000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
004100 77  WS-IDUSER                   PIC X(8)    VALUE SPACE.                 
004200 77  WS-KDLEVSP                  PIC 9(2)    VALUE ZERO.                  
004300 77  WS-FLKVANT                  PIC X       VALUE SPACE.                 
004400 77  WS-KVSPARR-KVAL             PIC 9(7)    VALUE ZERO.                  
004500 77  WS-KDSORT1                  PIC 9       VALUE ZERO.                  
004600 77  WS-IDFKNGRP                 PIC X(4)    VALUE ZERO.                  
004700 77  WS-KVRADER                  PIC 9(3)    VALUE 500.                   
004800 77  IDFKNGRP-SOEK               PIC X       VALUE 'N'.                   
004900 77  KDLEVSP-SOEK                PIC X       VALUE 'N'.                   
005000 77  KVANT-SOEK                  PIC X       VALUE 'N'.                   
005100 77  IDUSER-SOEK                 PIC X       VALUE 'N'.                   
005200 77  IDDC-SW                     PIC X       VALUE 'J'.                   
005300 77  INDX                        PIC S9(3)   VALUE ZERO.                  
005400 77  MAX-INDX                    PIC S9(3)   VALUE +500.                  
005500                                                                          
005600 77  WS-CP-UTF8                  PIC X(4)  VALUE 'UTF8'.                  
005700 77  WS-CP-EBCDIC                PIC X(3)  VALUE '278'.                   
005800 77  WS-BEART                    PIC X(25).                               
005900                                                                          
006000 01  WS-FKNURVAL                 PIC 9(4)    VALUE ZERO.                  
006100 01  FILLER REDEFINES WS-FKNURVAL.                                        
006200     03 WS-GRP-00                PIC 9(2).                                
006300     03 WS-RESTEN-00             PIC 9(2).                                
006400 01  FILLER REDEFINES WS-FKNURVAL.                                        
006500     03 WS-GRP-000               PIC 9(1).                                
006600     03 WS-RESTEN-000            PIC 9(3).                                
006700                                                                          
006800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006900     88  NYCKLAR-OK                          VALUE 'J'.                   
007000     88  NYCKLAR-FEL                         VALUE 'N'.                   
007100                                                                          
007200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007300     88  INDATA-OK                           VALUE 'J'.                   
007400     88  INDATA-FEL                          VALUE 'N'.                   
007500                                                                          
007600     EJECT                                                                
007700                                                                          
007800*    --- VALID DC CODES                                                   
007900*01  -COPY WWDC99                                                         
008000     EJECT                                                                
008100                                                                          
008200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008300 01  GENERELLA-SUBPROGRAM.                                                
008400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008500     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
008600     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
008700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009000     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
009100     EJECT                                                                
009200                                                                          
009300 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
009400*01  -COPY WTRAUTF8                                                       
009500     EJECT                                                                
009600                                                                          
009700*    --- AREOR FÖR WEBKOMMUNIKATION                                       
009800*                                                                         
009900*                                                                         
010000 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
010100*01  -COPY WZ01SUB                                                        
010200                                                                          
010300 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
010400 01  REQU-AREA.                                                           
010500*    03  -COPY WZ01REQU                                                   
010600*    03  -COPY WL0136I1                                                   
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
010900     SKIP3                                                                
011000 01  RESP-AREA.                                                           
011100*    03  -COPY WZ01RESP                                                   
011200*    03  -COPY WL0136O1                                                   
011300                                                                          
011400*01 -COPY W006PRT                                                         
011500     EJECT                                                                
011600 01  PROG-TO-PROG-SW.                                                     
011700*    03  -COPY WMSGSOP                                                    
011800     EJECT                                                                
011900 01  WS-PARAMETRAR.                                                       
012000     03  WS-URVAL.                                                        
012100         05 URV-KDSORT1     PIC 9.                                        
012200         05 URV-IDDC-BEST   PIC X(2).                                     
012300         05 URV-KDLEVSP     PIC 9(2).                                     
012400         05 URV-IDFKNGRP    PIC 9(4).                                     
012500         05 URV-FLKVANT     PIC X.                                        
012600         05 URV-IDDC        PIC X(2).                                     
012700         05 URV-IDUSER      PIC X(8).                                     
012800         05 URV-SIGNON-IDUSER PIC X(8).                                   
012900     03  WS-PRINTER.                                                      
013000         05 URV-IDPRINTER   PIC X(8) VALUE SPACE.                         
013100     EJECT                                                                
013200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013400     SKIP3                                                                
013500 01  NYCKLAR-TILL-DLI.                                                    
013600                                                                          
013700     03  W-IDDC-MIN-NYCKEL-X.                                             
013800         05  W-IDDC-MIN-NYCKEL   PIC X(2)    VALUE SPACE.                 
013900     03  W-IDDC-MAX-NYCKEL-X.                                             
014000         05  W-IDDC-MAX-NYCKEL   PIC X(2)    VALUE SPACE.                 
014100     03  W-IDDC-MIN-X.                                                    
014200         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
014300     03  W-IDDC-MAX-X.                                                    
014400         05  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                 
014500     03  W-IDFKNGRP-MIN-X.                                                
014600         05  W-IDFKNGRP-MIN      PIC S9(5)   VALUE ZERO COMP-3.           
014700     03  W-IDFKNGRP-MAX-X.                                                
014800         05  W-IDFKNGRP-MAX      PIC S9(5)   VALUE +99999 COMP-3.         
014900     03  W-KDLEVSP-MIN-X.                                                 
015000         05  W-KDLEVSP-MIN       PIC S9(3)   VALUE ZERO COMP-3.           
015100     03  W-KDLEVSP-MAX-X.                                                 
015200         05  W-KDLEVSP-MAX       PIC S9(3)   VALUE +999 COMP-3.           
015300     03  W-KVSPARR-MIN-X.                                                 
015400         05  W-KVSPARR-MIN       PIC S9(7)   VALUE ZERO COMP-3.           
015500     03  W-KVSPARR-MAX-X.                                                 
015600         05  W-KVSPARR-MAX       PIC S9(7)  VALUE +9999999 COMP-3.        
015700     03  W-IDUSER-MIN-X.                                                  
015800         05  W-IDUSER-MIN        PIC X(8)    VALUE LOW-VALUE.             
015900     03  W-IDUSER-MAX-X.                                                  
016000         05  W-IDUSER-MAX        PIC X(8)    VALUE HIGH-VALUE.            
016100                                                                          
016200     03  W-2403KEY-X.                                                     
016300         05  W-2403-IDHTYP      PIC X(4)     VALUE '2403'.                
016400         05  FILLER             PIC X(26)    VALUE LOW-VALUE.             
016500     03  W-2404KEY-MIN-X.                                                 
016600         05  W-IDARTNR-MIN      PIC S9(9)    VALUE ZERO COMP-3.           
016700         05  W-IDDC-KY-MIN      PIC X(2)     VALUE LOW-VALUE.             
016800         05  W-IDFKNGRP-KY-MIN  PIC S9(5)    VALUE ZERO COMP-3.           
016900     03  W-2404KEY-MAX-X.                                                 
017000         05  W-IDARTNR-MAX      PIC S9(9)  VALUE 999999999 COMP-3.        
017100         05  W-IDDC-KY-MAX      PIC X(2)     VALUE HIGH-VALUE.            
017200         05  W-IDFKNGRP-KY-MAX  PIC S9(5)    VALUE +99999 COMP-3.         
017300                                                                          
017400     03  W-IDARTNR-X.                                                     
017500         05  W-IDARTNR           PIC S9(9)              COMP-3.           
017510     03  W-IDSKYLT-X.                                                     
017520         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
017600                                                                          
017610     03  W-IDDC-B6-X.                                                     
017620         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
017630                                                                          
017700     EJECT                                                                
017800*    --- STATUS-KOD FRÅN IMS                                              
017900 01  STATUS-WS                   PIC XX.                                  
018000     88  SEGMENT-FINNS                       VALUE '  '.                  
018100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018300     88  BASEN-SLUT                          VALUE 'GB'.                  
018400     SKIP2                                                                
018500 01  GODK-STATUSKODER.                                                    
018600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018700     SKIP3                                                                
018800 01  SSA1                        PIC X(224).                              
018900 01  SSA2                        PIC X(32).                               
019000     EJECT                                                                
019100*    --- IMS FUNKTIONSKODER                                               
019200*01  -COPY W0003                                                          
019300     EJECT                                                                
019400*    ---  DLI INPUT-OUTPUT AREA                                           
019500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2404'.                    
019600 01  DLI-IO-WDGX2404.                                                     
019700*    03  -COPY WDGX2404                                                   
019710 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
019720 01  DLI-IO-AREA-B601.                                                    
019730*    03  -COPY WDB601                                                     
       01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD311'.                      
       01  DLI-IO-WDD311.                                                       
      *    03  -COPY WDD311                                                     
           EJECT                                                                
019900 LINKAGE SECTION.                                                         
020000*01  -COPY W0009   -PRE MSG-                                              
020100     EJECT                                                                
020200*01  -COPY W0009   -PRE ALT-                                              
020300     EJECT                                                                
020400*01  -COPY W0008   -PRE 2404-                                             
020401     05  FILLER                  PIC X.                                   
020402     EJECT                                                                
020410*01  -COPY W0008  -PRE WDB6-                                              
020500     05  FILLER                  PIC X.                                   
020700     EJECT                                                                
      *01  -COPY W0008  -PRE WDD3-                                              
           05  FILLER                  PIC X.                                   
           EJECT                                                                
020800 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB 2404-PCB WDB6-PCB              
020810                           WDD3-PCB.                                      
020900 MAIN SECTION.                                                            
021000     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB 2404-PCB WDB6-PCB              
021100                           WDD3-PCB.                                      
M21200     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
021300     IF SUB-KDRC = 0                                                      
021400        PERFORM A-INIT                                                    
021500        PERFORM B-KOLLA-NYCKLAR                                           
021600        IF NYCKLAR-OK                                                     
021700           IF REQU-KDPGMACT = 'E'                                         
021800              PERFORM C-KOLLA-INDATA                                      
021900              IF INDATA-OK                                                
022000                 PERFORM D-STARTA-SOP                                     
022100              END-IF                                                      
022200           END-IF                                                         
022300           IF INDATA-OK                                                   
022400              PERFORM E-LAES-VISA-INFO                                    
022500           END-IF                                                         
022600        END-IF                                                            
022700        PERFORM S02-RETURN-RESPONSE                                       
022800     END-IF                                                               
022900     MOVE ZERO TO RETURN-CODE                                             
023000     GOBACK                                                               
023100     .                                                                    
023200     EJECT                                                                
023300 A-INIT SECTION.                                                          
023400     MOVE 'A-INIT' TO CURR-SECTION                                        
023500                                                                          
023600     MOVE ALL '+'   TO RESP-AREA                                          
023700     MOVE SPACE     TO RESP-IDMSG-ERROR                                   
023800                       RESP-IDMSG-INFO                                    
023900                       RESP-IDELMT-ERROR                                  
024000     MOVE 001       TO RESP-IDMSGVER                                      
024010     MOVE REQU-IDDC-KEY TO W-IDDC-B6                                      
024030     PERFORM IMS-GU-WDB601                                                
024100                                                                          
024200     SET INDATA-OK TO TRUE                                                
024300     .                                                                    
024400                                                                          
024500     EJECT                                                                
024600 B-KOLLA-NYCKLAR SECTION.                                                 
024700     MOVE 'B-KOLLA-NYCKLAR' TO CURR-SECTION                               
024800                                                                          
024900     IF REQU-IDFKNGRP-KEY NOT = ALL '+'                                   
025000        INSPECT REQU-IDFKNGRP-KEY REPLACING                               
025100                         LEADING SPACE BY ZERO                            
025200     END-IF                                                               
025300                                                                          
025400     IF REQU-KDLEVSP-KEY NOT = ALL '+'                                    
025500        INSPECT REQU-KDLEVSP-KEY REPLACING LEADING SPACE BY ZERO          
025600        MOVE REQU-KDLEVSP-KEY TO WS-KDLEVSP                               
025700     END-IF                                                               
025800                                                                          
025900     MOVE REQU-FL-KVSPARR-KVAL-KEY TO WS-FLKVANT                          
026000     IF REQU-IDUSER-SPKVAL-KEY = ALL '+'                                  
026100       MOVE SPACE                  TO WS-IDUSER                           
026200     ELSE                                                                 
026300       MOVE REQU-IDUSER-SPKVAL-KEY   TO WS-IDUSER                         
026400     END-IF                                                               
026500                                                                          
026600     MOVE JA TO NYCKLAR-SW                                                
026700     MOVE NEJ TO IDFKNGRP-SOEK                                            
026800                 KDLEVSP-SOEK                                             
026900                 KVANT-SOEK                                               
027000                 IDUSER-SOEK                                              
027100                                                                          
027200     MOVE REQU-IDDC-KEY TO W-IDDC-MIN                                     
027300                           W-IDDC-MIN-NYCKEL                              
027400                           W-IDDC-MAX                                     
027500                           W-IDDC-MAX-NYCKEL                              
027600                                                                          
027700     IF REQU-IDFKNGRP-KEY > ZERO AND                                      
              REQU-IDFKNGRP-KEY NOT = ALL '+'                                   
027800        MOVE REQU-IDFKNGRP-KEY TO WS-FKNURVAL                             
027900        IF WS-RESTEN-00 > ZERO                                            
028000           MOVE WS-FKNURVAL TO W-IDFKNGRP-MIN                             
028100                               W-IDFKNGRP-MAX                             
028200        ELSE                                                              
028300           IF WS-RESTEN-000 = ZERO                                        
028400              MOVE WS-FKNURVAL TO W-IDFKNGRP-MIN                          
028500              MOVE 999 TO WS-RESTEN-000                                   
028600              MOVE WS-FKNURVAL TO W-IDFKNGRP-MAX                          
028700           ELSE                                                           
028800              IF WS-RESTEN-00 = ZERO                                      
028900                 MOVE WS-FKNURVAL TO W-IDFKNGRP-MIN                       
029000                 MOVE 99 TO WS-RESTEN-00                                  
029100                 MOVE WS-FKNURVAL TO W-IDFKNGRP-MAX                       
029200              END-IF                                                      
029300           END-IF                                                         
029400        END-IF                                                            
029500        MOVE JA TO IDFKNGRP-SOEK                                          
029600     END-IF                                                               
029700                                                                          
029800     IF WS-KDLEVSP NUMERIC                                                
029900        IF WS-KDLEVSP = ZERO                                              
030000           CONTINUE                                                       
030100        ELSE                                                              
030200           IF WS-KDLEVSP = 20 OR 21 OR 22                                 
030300              MOVE WS-KDLEVSP TO W-KDLEVSP-MIN                            
030400                                 W-KDLEVSP-MAX                            
030500              MOVE JA TO KDLEVSP-SOEK                                     
030600           ELSE                                                           
030700              MOVE NEJ TO NYCKLAR-SW                                      
030800           END-IF                                                         
030900        END-IF                                                            
031000     ELSE                                                                 
031100        MOVE NEJ TO NYCKLAR-SW                                            
031200     END-IF                                                               
031300                                                                          
031400     IF WS-FLKVANT = JA OR NEJ OR SPACE OR 'Y'                            
031500        IF WS-FLKVANT = JA OR 'Y'                                         
031600           MOVE +0000001 TO W-KVSPARR-MIN                                 
031700        ELSE                                                              
031800           IF WS-FLKVANT = NEJ                                            
031900              MOVE +0000000 TO W-KVSPARR-MIN                              
032000           END-IF                                                         
032100        END-IF                                                            
032200        IF WS-FLKVANT = JA OR NEJ OR 'Y'                                  
032300           MOVE JA TO KVANT-SOEK                                          
032400        END-IF                                                            
032500     ELSE                                                                 
032600        MOVE NEJ TO NYCKLAR-SW                                            
032700     END-IF                                                               
032800                                                                          
032900     IF WS-IDUSER NOT = SPACE AND                                         
033000        WS-IDUSER NOT = ALL '+'                                           
033100        MOVE WS-IDUSER TO W-IDUSER-MIN                                    
033200                          W-IDUSER-MAX                                    
033300        MOVE JA TO IDUSER-SOEK                                            
033400     END-IF                                                               
033500                                                                          
033600     IF NYCKLAR-FEL                                                       
033700        MOVE '023'       TO RESP-IDMSG-ERROR                              
033800        MOVE 'KEYS'      TO RESP-IDELMT-ERROR                             
033900        SET INDATA-FEL TO TRUE                                            
034000     END-IF                                                               
034100     .                                                                    
034200     EJECT                                                                
034300 C-KOLLA-INDATA SECTION.                                                  
034400                                                                          
034500     MOVE JA TO INDATA-SW                                                 
034600                IDDC-SW                                                   
034700                                                                          
034800     IF REQU-KDSORT1 NOT = ALL '+'                                        
034900        IF REQU-KDSORT1 = '0' OR '1' OR '2'                               
035000           MOVE REQU-KDSORT1 TO WS-KDSORT1                                
035100        ELSE                                                              
035200           MOVE NEJ TO INDATA-SW                                          
035300        END-IF                                                            
035400     ELSE                                                                 
035500        MOVE ZERO TO WS-KDSORT1                                           
035600     END-IF                                                               
035700                                                                          
035800     IF REQU-FLSKRIV = ALL '+'                                            
035900        MOVE NEJ TO INDATA-SW                                             
036000     ELSE                                                                 
036100        MOVE  004              TO PRT-KDCALL                              
036200        IF REQU-FLSKRIV = 'J'                                             
036300          MOVE 'UK8     '      TO PRT-IDPRTLST                            
036400          CALL W006PRT USING PRT-W006PRT                                  
036500          IF PRT-KDSVAR = 'R'                                             
036600             MOVE REQU-FLSKRIV TO URV-IDPRINTER                           
036700          ELSE                                                            
036800             MOVE NEJ TO INDATA-SW                                        
036900             MOVE 'KEYS'   TO RESP-IDELMT-ERROR                           
037000          END-IF                                                          
037100        END-IF                                                            
037200        IF REQU-FLSKRIV = 'N'                                             
037300          MOVE NEJ TO INDATA-SW                                           
037400          MOVE 'KEYS'   TO RESP-IDELMT-ERROR                              
037500        END-IF                                                            
037600     END-IF                                                               
037700                                                                          
037800     IF  INDATA-FEL                                                       
037900        MOVE '023'       TO RESP-IDMSG-ERROR                              
038000        SET INDATA-FEL TO TRUE                                            
038100     ELSE                                                                 
038200        IF REQU-IDDC-KEY NOT = ALL '+'                                    
038300           MOVE REQU-IDDC-KEY            TO RESP-IDDC-KEY                 
038400        END-IF                                                            
038500        IF REQU-IDFKNGRP-KEY NOT = ALL '+'                                
038600           MOVE REQU-IDFKNGRP-KEY        TO RESP-IDFKNGRP-KEY             
038700        END-IF                                                            
038800        IF REQU-KDLEVSP-KEY NOT = ALL '+'                                 
038900           MOVE REQU-KDLEVSP-KEY         TO RESP-KDLEVSP-KEY              
039000        END-IF                                                            
039100        IF REQU-FL-KVSPARR-KVAL-KEY NOT = ALL '+'                         
039200           MOVE REQU-FL-KVSPARR-KVAL-KEY TO                               
039300                                         RESP-FL-KVSPARR-KVAL-KEY         
039400        END-IF                                                            
039500        IF REQU-IDUSER-SPKVAL-KEY NOT = ALL '+'                           
039600           MOVE REQU-IDUSER-SPKVAL-KEY   TO RESP-IDUSER-SPKVAL-KEY        
039700        END-IF                                                            
039800     END-IF                                                               
039900     .                                                                    
040000     EJECT                                                                
040100 D-STARTA-SOP SECTION.                                                    
040200     MOVE 'D-STARTA-SOP    ' TO CURR-SECTION                              
040300                                                                          
040400     MOVE REQU-IDDC-KEY     TO URV-IDDC-BEST                              
040500                               URV-IDDC                                   
040600     MOVE WS-KDSORT1        TO URV-KDSORT1                                
040700     IF REQU-IDFKNGRP-KEY = SPACE OR ALL '+'                              
040800        MOVE ZERO           TO URV-IDFKNGRP                               
040900     ELSE                                                                 
041000        MOVE REQU-IDFKNGRP-KEY  TO URV-IDFKNGRP                           
041100     END-IF                                                               
041200     MOVE WS-IDUSER         TO URV-IDUSER                                 
041300     MOVE WS-KDLEVSP        TO URV-KDLEVSP                                
041400     IF WS-FLKVANT = 'Y' OR 'J'                                           
041500        MOVE 'J'            TO URV-FLKVANT                                
041600     ELSE                                                                 
041700        MOVE SPACE          TO URV-FLKVANT                                
041800     END-IF                                                               
041900     MOVE REQU-IDUSER       TO URV-SIGNON-IDUSER                          
042000                                                                          
042100     MOVE 'L136'   TO MSGSOP-IDTRANS                                      
042200     MOVE '1'      TO MSGSOP-KDMFSFOR                                     
042300     MOVE 'WL10S3' TO MSGSOP-IDPROCESS                                    
042400     MOVE 'O'      TO MSGSOP-KDSOPFUNK                                    
042500                                                                          
042600     STRING 'URVAL(' WS-URVAL ')PRT('                                     
042700            WS-PRINTER ')'                                                
042800            DELIMITED BY SIZE INTO MSGSOP-TESYMBV                         
042900                                                                          
043000     PERFORM IMS-INSERT-ALTMSG                                            
043100     MOVE '015' TO RESP-IDMSG-INFO                                        
043200     .                                                                    
043300     EJECT                                                                
043400 E-LAES-VISA-INFO SECTION.                                                
043500     MOVE 'E-LAES-VISA-INFO' TO CURR-SECTION                              
043600                                                                          
043700     PERFORM IMS-GET-WDR501                                               
043800                                                                          
043900     IF SEGMENT-SAKNAS                                                    
044000        MOVE '025'       TO RESP-IDMSG-ERROR                              
044100        SET INDATA-FEL TO TRUE                                            
044200        MOVE 'INFO'      TO RESP-IDELMT-ERROR                             
044300     ELSE                                                                 
044400        MOVE +1 TO INDX                                                   
044500        PERFORM IMS-GU-WDGX2404                                           
044600        IF SEGMENT-SAKNAS OR BASEN-SLUT                                   
044700           MOVE '025'       TO RESP-IDMSG-ERROR                           
044800           SET INDATA-FEL TO TRUE                                         
044900           MOVE 'INFO '   TO RESP-IDELMT-ERROR                            
045000        END-IF                                                            
045100                                                                          
045200        MOVE SPACE     TO RESP-WL0136O1                                   
045300        IF REQU-IDDC-KEY NOT = ALL '+'                                    
045400         MOVE REQU-IDDC-KEY            TO RESP-IDDC-KEY                   
045500        END-IF                                                            
045600        IF REQU-IDFKNGRP-KEY NOT = ALL '+'                                
045700         MOVE REQU-IDFKNGRP-KEY        TO RESP-IDFKNGRP-KEY               
045800        END-IF                                                            
045900        IF REQU-KDLEVSP-KEY NOT = ALL '+'                                 
046000         MOVE REQU-KDLEVSP-KEY         TO RESP-KDLEVSP-KEY                
046100        END-IF                                                            
046200        IF REQU-FL-KVSPARR-KVAL-KEY NOT = ALL '+'                         
046300         MOVE REQU-FL-KVSPARR-KVAL-KEY TO RESP-FL-KVSPARR-KVAL-KEY        
046400        END-IF                                                            
046500        IF REQU-IDUSER-SPKVAL-KEY NOT = ALL '+'                           
046600         MOVE REQU-IDUSER-SPKVAL-KEY   TO RESP-IDUSER-SPKVAL-KEY          
046700        END-IF                                                            
046800        MOVE ZERO TO WS-KVRADER                                           
046900        MOVE ZERO TO RESP-KVRADER                                         
047000        PERFORM UNTIL INDX > MAX-INDX                                     
047100          IF SEGMENT-FINNS                                                
047200            MOVE 2404-IDARTNR       TO RESP-IDARTNR       (INDX)          
047300                                       W-IDARTNR                          
047400            MOVE 2404-IDDC          TO RESP-IDDC          (INDX)          
047500                                       WS-IDDC                            
047600            MOVE 2404-IDFKNGRP      TO RESP-IDFKNGRP      (INDX)          
047700            MOVE 2404-KDLEVSP       TO RESP-KDLEVSP       (INDX)          
047800            MOVE 2404-KVSPARR-KVAL  TO RESP-KVSPARR-KVAL  (INDX)          
047900            MOVE 2404-KVLS          TO RESP-KVLS          (INDX)          
048000            MOVE 2404-KVROS         TO RESP-KVROS         (INDX)          
048100            MOVE 2404-TISPARR-KVAL  TO RESP-TISPARR-KVAL  (INDX)          
048200            MOVE 2404-IDUSER-SPKVAL TO RESP-IDUSER-SPKVAL(INDX)           
048300                                                                          
049301            MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                       
049302            IF DCS-UNICODE-IDSKYLT                                        
049303              MOVE 'UTF8'             TO TRAUTF8-KDCP                     
049304            ELSE                                                          
049305              MOVE '278 '             TO TRAUTF8-KDCP                     
049306            END-IF                                                        
049310            PERFORM IMS-GU-WDD311                                         
049320            IF SEGMENT-FINNS                                              
049330              MOVE TEXT-BEART TO TRAUTF8-TECONV-FROM                      
049340            ELSE                                                          
049350              MOVE SPACE           TO TRAUTF8-TECONV-FROM                 
049360            END-IF                                                        
                  IF TRAUTF8-TECONV-FROM = SPACES                               
                   MOVE 'GB'  TO W-IDSKYLT                                      
                   MOVE '278' TO TRAUTF8-KDCP                                   
                   PERFORM IMS-GU-WDD311                                        
                   MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                    
                  END-IF                                                        
049500*           -- STRIP SPACE OR CONVERT TO UNICODE                          
049600            CALL WTRAUTF8 USING TRAUTF8-AREA                              
049700*           -- MOVE CONVERTED DESCRIPTION TO THE RESPONSE                 
049800            MOVE TRAUTF8-TECONV-TO  TO RESP-BEART  (INDX)                 
049900                                                                          
050000            ADD 1 TO WS-KVRADER                                           
050100            PERFORM IMS-GN-WDGX2404                                       
050200          END-IF                                                          
050300          ADD 1 TO INDX                                                   
050400       END-PERFORM                                                        
050500       MOVE WS-KVRADER  TO RESP-KVRADER                                   
050600                                                                          
050700     END-IF                                                               
050800     .                                                                    
050900     EJECT                                                                
051000*    --- DISPATCHER SECTIONS                                              
051100 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
051200                                                                          
051300     MOVE 'GETARG'               TO SUB-KDFUNC                            
051400     MOVE 'CARPARTS.LDC.QUALITYBLOCKING' TO SUB-ADDISPABS                 
051500     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
051600                                                                          
051700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
051800                                                                          
051900     IF SUB-KDRC > 0                                                      
052000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
052100       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
052200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
052300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
052400     END-IF                                                               
052500     .                                                                    
052600                                                                          
052700 S02-RETURN-RESPONSE SECTION.                                             
052800                                                                          
052900     MOVE 'RETURN'                   TO SUB-KDFUNC                        
053000     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
053100                                                                          
053200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
053300                                                                          
053400     IF SUB-KDRC > 0                                                      
053500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
053600       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
053700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
053800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
053900     END-IF                                                               
054000     .                                                                    
054100                                                                          
054200* --- IMS SEKTIONER ---                                                   
054300     SKIP3                                                                
054400 IMS-INSERT-ALTMSG SECTION.                                               
054500     MOVE SPACE TO GODK-STATUSKODER                                       
054600     CALL CBLTDLI USING PURG ALT-PCB PROG-TO-PROG-SW                      
054700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
054800     PERFORM IMS-STATUSKONTROLL                                           
054900     .                                                                    
055000     EJECT                                                                
055100 IMS-GET-WDR501 SECTION.                                                  
055200     STRING 'WDR501  (WDGXKEY  =' W-2403KEY-X ')'                         
055300          DELIMITED BY SIZE INTO SSA1                                     
055400     MOVE 'GE' TO GODK-STATUSKODER                                        
055500     CALL CBLTDLI USING GU 2404-PCB DLI-IO-WDGX2404 SSA1                  
055600     MOVE 2404-STATUS-CODE TO STATUS-WS                                   
055700     PERFORM IMS-STATUSKONTROLL                                           
055800     .                                                                    
055900     SKIP3                                                                
056000 IMS-GU-WDGX2404 SECTION.                                                 
056100     STRING 'WDGX2404(KY2404  =>' W-2404KEY-MIN-X                         
056200                    '&KY2404  =<' W-2404KEY-MAX-X                         
056300                    '&IDDC    =>' W-IDDC-MIN-NYCKEL-X                     
056400                    '&IDDC    =<' W-IDDC-MAX-X                            
056500                    '&IDFKNGRP=>' W-IDFKNGRP-MIN-X                        
056600                    '&IDFKNGRP=<' W-IDFKNGRP-MAX-X                        
056700                    '&KDLEVSP =>' W-KDLEVSP-MIN-X                         
056800                    '&KDLEVSP =<' W-KDLEVSP-MAX-X                         
056900                    '&KVSPARR =>' W-KVSPARR-MIN-X                         
057000                    '&KVSPARR =<' W-KVSPARR-MAX-X                         
057100                    '&IDUSER  =>' W-IDUSER-MIN-X                          
057200                    '&IDUSER  =<' W-IDUSER-MAX-X ')'                      
057300           DELIMITED BY SIZE INTO SSA1                                    
057400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
057500     CALL CBLTDLI USING GN 2404-PCB DLI-IO-WDGX2404 SSA1                  
057600     MOVE 2404-STATUS-CODE TO STATUS-WS                                   
057700     PERFORM IMS-STATUSKONTROLL                                           
057800     .                                                                    
057900     EJECT                                                                
058000 IMS-GN-WDGX2404 SECTION.                                                 
058100     STRING 'WDGX2404(KY2404  =>' W-2404KEY-MIN-X                         
058200                    '&KY2404  =<' W-2404KEY-MAX-X                         
058300                    '&IDDC    =>' W-IDDC-MIN-X                            
058400                    '&IDDC    =<' W-IDDC-MAX-X                            
058500                    '&IDFKNGRP=>' W-IDFKNGRP-MIN-X                        
058600                    '&IDFKNGRP=<' W-IDFKNGRP-MAX-X                        
058700                    '&KDLEVSP =>' W-KDLEVSP-MIN-X                         
058800                    '&KDLEVSP =<' W-KDLEVSP-MAX-X                         
058900                    '&KVSPARR =>' W-KVSPARR-MIN-X                         
059000                    '&KVSPARR =<' W-KVSPARR-MAX-X                         
059100                    '&IDUSER  =>' W-IDUSER-MIN-X                          
059200                    '&IDUSER  =<' W-IDUSER-MAX-X ')'                      
059300          DELIMITED BY SIZE INTO SSA1                                     
059400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
059500     CALL CBLTDLI USING GN 2404-PCB DLI-IO-WDGX2404 SSA1                  
059600     MOVE 2404-STATUS-CODE TO STATUS-WS                                   
059700     PERFORM IMS-STATUSKONTROLL                                           
059800     .                                                                    
059900     EJECT                                                                
059910 IMS-GU-WDB601 SECTION.                                                   
059930                                                                          
059940     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
059950          DELIMITED BY SIZE INTO SSA1                                     
059960     MOVE '  ' TO GODK-STATUSKODER                                        
059970     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
059980     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
059990     PERFORM IMS-STATUSKONTROLL                                           
059991     .                                                                    
059992     SKIP3                                                                
       IMS-GU-WDD311 SECTION.                                                   
                                                                                
           STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
                   DELIMITED BY SIZE INTO SSA1                                  
           STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
                    DELIMITED BY SIZE INTO SSA2                                 
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
           MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           EJECT                                                                
060010 IMS-STATUSKONTROLL SECTION.                                              
060100     SET STATUS-IX TO 1                                                   
060200     SEARCH GODK-STATUS                                                   
060300       AT END                                                             
060400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
060500         DELIMITED BY SIZE INTO FELTEXT                                   
060600         CALL FELLOG                                                      
060700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
060800         CONTINUE                                                         
060900     END-SEARCH                                                           
061000     .                                                                    
