000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WL011300.                                                
000400 AUTHOR.         GÖRAN KJELLSON  GUIDE                                    
000500 DATE-WRITTEN.   JULI 2005                                                
000600 DATE-COMPILED.                                                           
000700*    NAME:       'CARPARTS.LDC.DELIVERYBLOCKING'                          
000800*                                                                         
000900*        WL011300 PROGRAM IS A REPLICA OF W6033800 PROGRAM                
001000*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
001100*                                                                         
001200*    FUNKTION:                                                            
001300*      - VISNING OCH UPPDATERING AV SPÄRRKOD OCH SPÄRRNOT.                
001400*        EN ARTIKEL KAN SPÄRRAS ANTINGEN GLOBALT ELLER LOKALT.            
001500*        OCH GÖRS PÅ GRUND AV KVALITETS-PROBLEM.                          
001600*        ARTIKEL STOPPAD FÖR UTLEVERANS                                   
001700*        00 = INGEN SPÄRR                                                 
001800*        20 = GLOBALSPÄRR PÅ CDC/SDC/NDC/LDC UPPDAT AV CDC.               
001900*        21 = LEVSPÄRR    PÅ CDC/SDC/NDC/LDC UPPDAT AV CDC.               
002000*        22 = LEVSPÄRR    PÅ SDC/NDC/LDC     UPPDAT AV CDC                
002100*                         SAMT SDC/NDC/LDC PÅ EGET DC.                    
002200*                                                                         
002300*    LOKALT ALLA SDC NDC LDC                                              
002400*      - LOKAL SPÄRRKOD = 00 ELLER 21 ELLER 22.                           
002500*      - SPÄRRKOD FÅR BARA UPPDATERAS OM DET INNAN INTE VAR 20.           
002600*      - KOD 21 FÅR BARA UPPDATERAS AV CDC-FOLK.                          
002700*      - KOD 22 FÅR UPPDATERAS AV SDC/NDC/LDC-FOLK PÅ EGET DC,            
002800*        SAMT AV CDC.                                                     
002900*      - KVANT FÅR BARA UPPDATERAS AV EGET DC VID KOD 00.                 
003000*      - KVALNOT FÅR UPPDATERAS AV CDC UTOM VID KOD 22                    
003100*        SAMT AV EGET DC VID SAMTLIGA SPÄRRKODER.                         
003200*                                                                         
003300*        PROGRAMMET LÄSER              WLARTC (WDK6)                      
003400*                                      WLBENA (WDD3)                      
003500*        PROGRAMMET LÄSER+UPPDATERAR   WLARTS (WDK7)                      
003600*                                                                         
003700*    TRANSACTION.     WL0113U                                             
003800*    INDATA.          WL0113I1                                            
003900*                                                                         
004000*    UTDATA.          WL0113O1                                            
004100                                                                          
004200     SKIP3                                                                
004300 ENVIRONMENT DIVISION.                                                    
004400     EJECT                                                                
004500 DATA DIVISION.                                                           
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800 77  IDPGM                       PIC X(08)   VALUE 'WL011300'.            
004900                                                                          
005000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005200                                                                          
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500 77  MAX-INDX                    PIC S9(9)   VALUE +500 COMP SYNC.        
005600 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
005700 77  IX                          PIC S9(9)   VALUE +0   COMP SYNC.        
005800 77  CURR-SECTION                PIC X(16) VALUE 'MAIN'.                  
005900 77  CURR-IMS-SECTION            PIC X(16) VALUE SPACE.                   
006000 77  KDRC-DISPLAY                PIC Z(5).                                
006100 77  SW-UPPDAT                   PIC X       VALUE 'N'.                   
006200 77  SW-UPPDAT1                  PIC X       VALUE 'N'.                   
006300 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006400 77  DAGENS-DATUM-TID            PIC S9(18)  VALUE ZERO.                  
006500 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
006600 77  INFO-TEXT                   PIC X(80) VALUE SPACE.                   
006700 77  INFO-TEXT1                  PIC X(80) VALUE SPACE.                   
006800 77  INFO-TEXT2                  PIC X(80) VALUE SPACE.                   
006900 77  INFO-TEXT3                  PIC X(80) VALUE SPACE.                   
007000 77  INFO-TEXT4                  PIC X(80) VALUE SPACE.                   
007100 77  WS-KDLEVSP-UPD              PIC 9(2)  VALUE ZERO.                    
007200 77  WS-SPAR-IDDC-6332           PIC X(2)  VALUE SPACE.                   
007300 77  WS-IDSKYLT-CN               PIC X(3)  VALUE 'RCN'.                   
007400 77  WS-IDSKYLT-GB               PIC X(3)  VALUE 'GB '.                   
007500 77  WS-CP-UTF8                  PIC X(4)  VALUE 'UTF8'.                  
007600 77  WS-CP-278                   PIC X(3)  VALUE '278'.                   
007701 77  WS-NEW-KVSPARR-KVAL         PIC S9(7)   COMP-3 VALUE +0.             
007801 77  WS-NEW-KDLEVSP              PIC 9(2)    VALUE ZERO.                  
007901 77  WS-NEW-IDDC                 PIC X(2)    VALUE SPACE.                 
008001                                                                          
008101*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
008201                                                                          
008301 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008401     88  NYCKLAR-OK                          VALUE 'J'.                   
008501     88  NYCKLAR-FEL                         VALUE 'N'.                   
008601                                                                          
008701 77  INDATA-SW                   PIC X       VALUE 'N'.                   
008801     88  INDATA-OK                           VALUE 'J'.                   
008901     88  INDATA-FEL                          VALUE 'N'.                   
009001                                                                          
009101 77  POST-SW                     PIC X       VALUE 'N'.                   
009201     88  POST-FINNS                          VALUE 'J'.                   
009301     88  POST-SAKNAS                         VALUE 'N'.                   
009401                                                                          
009501 77  LEVEL-SW                    PIC XX      VALUE '00'.                  
009601     88  LEVEL-2                             VALUE '20'.                  
009701     88  LEVEL3-UTAN-USER                    VALUE '30'.                  
009801     88  LEVEL3-MED-USER                     VALUE '31'.                  
009901                                                                          
010001 77  LEVEL2-SW                    PIC X      VALUE 'N'.                   
010101     88  LEVEL-2-IDDC                        VALUE 'J'.                   
010201     88  LEVEL-3-IDDC                        VALUE 'N'.                   
010301                                                                          
010401 77  DC-UPPDAT-SW                PIC X       VALUE 'N'.                   
010501     88  UPPDAT-OK                           VALUE 'J'.                   
010601     88  UPPDAT-INTE-GODKAND                 VALUE 'N'.                   
010701                                                                          
010801 77  STOCK-BALANCE-SW            PIC X       VALUE 'N'.                   
010901     88  STOCK-BAL-OK                        VALUE 'J'.                   
011001     88  STOCK-BAL-NO                        VALUE 'N'.                   
011101                                                                          
011201 77  STOPP-LAES-SW               PIC X       VALUE 'N'.                   
011301     88  STOPP-LAES                          VALUE 'J'.                   
011401                                                                          
011501*    --- VALID DC CODES                                                   
011601*01  -COPY WWDC99                                                         
011701     EJECT                                                                
011801                                                                          
011901*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012001 01  GENERELLA-SUBPROGRAM.                                                
012101     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012201     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012301     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
012401     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012501     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
012601     EJECT                                                                
012701*    --- PARAMETERS TO ABEND                                              
012801                                                                          
012901 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013001 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
013101 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
013201     SKIP3                                                                
013301     EJECT                                                                
013401                                                                          
013501 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
013601*01  -COPY WTRAUTF8                                                       
013701     EJECT                                                                
013801                                                                          
013901*    --- AREOR FÖR WEBKOMMUNIKATION                                       
014001*                                                                         
014101*                                                                         
014201 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
014301*01  -COPY WZ01SUB                                                        
014401                                                                          
014501 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
014601 01  REQU-AREA.                                                           
014701*    03  -COPY WZ01REQU                                                   
014801*    03  -COPY WL0113I1                                                   
014901     EJECT                                                                
015001 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
015101     SKIP3                                                                
015201 01  RESP-AREA.                                                           
015301*    03  -COPY WZ01RESP                                                   
015401*    03  -COPY WL0113O1                                                   
015501                                                                          
015601                                                                          
015701                                                                          
015801*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015901 01  FILLER                      PIC X(16)  VALUE 'TEST-WLARTS11'.        
016001 01  TEST-WLARTS11-AREA.                                                  
016101*    03 -COPY WDK711  -PRE TEST-                                          
016201     EJECT                                                                
016301 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016401     SKIP3                                                                
016501 01  NYCKLAR-TILL-DLI.                                                    
016601     03  W-IDARTNR-X.                                                     
016701         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016801     03  W-KDSEGKEY-X.                                                    
016901         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
017001     03  W-IDDC-X.                                                        
017101         05  W-IDDC              PIC XX      VALUE SPACE.                 
017201     03  W-IDSKYLT-X.                                                     
017301         05  W-IDSKYLT           PIC XXX     VALUE SPACE.                 
017401                                                                          
017501     03  W-IDDC-B6-X.                                                     
017601         05 W-IDDC-B6            PIC X(2).                                
017701                                                                          
017801     03  W-WDGXKEY-6331-X.                                                
017901         05  W-IDHTYP            PIC X(4)    VALUE '6331'.                
018001         05  W-6331-IDDC         PIC X(2)    VALUE '11'.                  
018101         05  W-6331-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
018201                                                                          
018301     03  W-WDGXKEY-6332-X.                                                
018401         05  W-IDDC-6332         PIC X(2)    VALUE SPACE.                 
018501                                                                          
018601     03  W-WDGXKEY-6334-X.                                                
018701         05  W-IDDC-6334         PIC X(2)    VALUE SPACE.                 
018801                                                                          
018901     03  W-2403KEY-X.                                                     
019001         05  W-2403-IDHTYP      PIC X(4)     VALUE '2403'.                
019101         05  FILLER             PIC X(26)    VALUE LOW-VALUE.             
019201                                                                          
019301     03  W-2404KEY-MIN-X.                                                 
019401         05  W-IDARTNR-MIN      PIC S9(9)    VALUE ZERO COMP-3.           
019501         05  W-IDDC-KY-MIN      PIC X(2)     VALUE LOW-VALUE.             
019601         05  W-IDFKNGRP-KY-MIN  PIC S9(5)    VALUE ZERO COMP-3.           
019701                                                                          
019801     03  W-2404KEY-MAX-X.                                                 
019901         05  W-IDARTNR-MAX      PIC S9(9)  VALUE 999999999 COMP-3.        
020001         05  W-IDDC-KY-MAX      PIC X(2)     VALUE HIGH-VALUE.            
020101         05  W-IDFKNGRP-KY-MAX  PIC S9(5)    VALUE +99999 COMP-3.         
020201                                                                          
020301     SKIP2                                                                
020401*    --- STATUS-KOD FRÅN IMS                                              
020501 01  STATUS-WS                   PIC XX.                                  
020601     88  SEGMENT-FINNS                       VALUE '  '.                  
020701     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020801     SKIP2                                                                
020901 01  GODK-STATUSKODER.                                                    
021001     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021101     SKIP3                                                                
021201 01  SSA1                        PIC X(64).                               
021301 01  SSA2                        PIC X(64).                               
021401 01  SSA3                        PIC X(64).                               
021501     EJECT                                                                
021601*    --- IMS FUNKTIONSKODER                                               
021701*01  -COPY W0003                                                          
021801     EJECT                                                                
021901*    ---  DLI INPUT-OUTPUT AREA                                           
022001                                                                          
022101 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC01'.                    
022201 01  DLI-IO-WLARTC01.                                                     
022301*    03  -COPY WDK601                                                     
022401     EJECT                                                                
022501 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC11'.                    
022601 01  DLI-IO-WLARTC11.                                                     
022701*    03  -COPY WDK611                                                     
022801     EJECT                                                                
022901 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTS01'.                    
023001 01  DLI-IO-WLARTS01.                                                     
023101*    03  -COPY WDK701                                                     
023201     EJECT                                                                
023301 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTS11'.                    
023401 01  DLI-IO-WLARTS11.                                                     
023501*    03  -COPY WDK711                                                     
023601     EJECT                                                                
023701 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLBENA01'.                    
023801 01  DLI-IO-WLBENA01.                                                     
023901*    03  -COPY WDD301  -PRE BENA-                                         
024001     EJECT                                                                
024101 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLBENA11'.                    
024201 01  DLI-IO-WLBENA11.                                                     
024301*    03  -COPY WDD311  -PRE BENA-                                         
024401     EJECT                                                                
024501 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6331'.                    
024601     SKIP3                                                                
024701 01  DLI-IO-WDGX6331.                                                     
024801*        05  -COPY WDGX6331                                               
024901     EJECT                                                                
025001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6332'.                    
025101     SKIP3                                                                
025201 01  DLI-IO-WDGX6332.                                                     
025301*        05  -COPY WDGX6332                                               
025401     EJECT                                                                
025501 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6334'.                    
025601     SKIP3                                                                
025701 01  DLI-IO-WDGX6334.                                                     
025801*        05  -COPY WDGX6334                                               
025901     EJECT                                                                
026001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
026101     SKIP3                                                                
026201 01  DLI-IO-AREA-WDB6.                                                    
026301*        05  -COPY WDB601                                                 
026401     EJECT                                                                
026501 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2404'.                    
026601 01  DLI-IO-WDGX2404.                                                     
026701*    03  -COPY WDGX2404                                                   
026801                                                                          
026901 LINKAGE SECTION.                                                         
027001*01  -COPY W0009   -PRE MSG-                                              
027101     EJECT                                                                
027201     EJECT                                                                
027301*01  -COPY W0008   -PRE ARTC-                                             
027401     05  FILLER                  PIC X.                                   
027501     EJECT                                                                
027601*01  -COPY W0008   -PRE ARTS-                                             
027701     05  FILLER                  PIC X.                                   
027801     EJECT                                                                
027901*01  -COPY W0008   -PRE BENA-                                             
028001     05  FILLER                  PIC X.                                   
028101     EJECT                                                                
028201*01  -COPY W0008   -PRE WDR2-                                             
028301     05  FILLER                  PIC X.                                   
028401*01  -COPY W0008   -PRE WDR2ALT-                                          
028501     05  FILLER                  PIC X.                                   
028601*01  -COPY W0008   -PRE WDB6-                                             
028701     05  FILLER                  PIC X.                                   
028801     EJECT                                                                
028901*01  -COPY W0008  -PRE 2404-                                              
029001     05  FILLER                  PIC X.                                   
029101     EJECT                                                                
029201                                                                          
029301 PROCEDURE DIVISION  USING MSG-PCB  ARTC-PCB ARTS-PCB                     
029401                           BENA-PCB WDR2-PCB WDR2ALT-PCB                  
029501                           WDB6-PCB 2404-PCB.                             
029601 MAIN SECTION.                                                            
029701     ENTRY 'DLITCBL' USING MSG-PCB  ARTC-PCB ARTS-PCB                     
029801                           BENA-PCB WDR2-PCB WDR2ALT-PCB                  
029901                           WDB6-PCB 2404-PCB.                             
030001                                                                          
030101     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
030201     IF SUB-KDRC = 0                                                      
030301        PERFORM A-INIT                                                    
030401        PERFORM B-CHECK-KEYS                                              
030501        PERFORM BA-CHECK-LEVEL                                            
030601        IF NYCKLAR-OK                                                     
030701           IF REQU-KDPGMACT = 'E'                                         
030801              PERFORM C-KOLLA-INPUT                                       
030901              IF INDATA-OK                                                
031001                 PERFORM D-UPPDATERA                                      
031101                                                                          
031201              END-IF                                                      
031301           END-IF                                                         
031401           IF INDATA-OK                                                   
031501              PERFORM E-LAES-VISA-INFO                                    
031601           END-IF                                                         
031701        END-IF                                                            
031801        IF INDATA-FEL AND REQU-KDPGMACT = 'E'                             
031901          MOVE ALL '+'      TO RESP-WL0113O1                              
032001          MOVE REQU-KVRADER TO RESP-KVRADER                               
032101        END-IF                                                            
032201        PERFORM S02-RETURN-RESPONSE                                       
032301     END-IF                                                               
032401                                                                          
032501     MOVE ZERO TO RETURN-CODE                                             
032601     GOBACK                                                               
032701     .                                                                    
032801     EJECT                                                                
032901 A-INIT SECTION.                                                          
033001     MOVE 'A-INIT' TO CURR-SECTION                                        
033101     ACCEPT DAGENS-DATUM FROM DATE                                        
033201     ACCEPT DAGENS-DATUM-TID FROM TIME                                    
033301                                                                          
033401     IF REQU-KDPGMACT = 'S'                                               
033501       MOVE SPACE     TO RESP-WL0113O1                                    
033601     ELSE                                                                 
033701       MOVE ALL '+'   TO RESP-AREA                                        
033801     END-IF                                                               
033901     MOVE SPACE     TO RESP-IDMSG-ERROR                                   
034001                       RESP-IDMSG-INFO                                    
034101                       RESP-IDELMT-ERROR                                  
034201     MOVE 001       TO RESP-IDMSGVER                                      
034301                                                                          
034401     SET INDATA-OK TO TRUE                                                
034501     .                                                                    
034601     EJECT                                                                
034701 B-CHECK-KEYS SECTION.                                                    
034801     MOVE 'B-CHECK-KEYS' TO CURR-SECTION                                  
034901                                                                          
035001     IF REQU-KDPGMACT = 'S' OR 'E'                                        
035101        CONTINUE                                                          
035201     ELSE                                                                 
035301        MOVE '023'              TO RESP-IDMSG-ERROR                       
035401*       WRONG ACTION KEY ***                                              
035501        MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                      
035601        MOVE NEJ                TO NYCKLAR-SW                             
035701     END-IF                                                               
035801                                                                          
035901     IF REQU-IDDC2-KEY NOT = ALL '+'                                      
036001        MOVE REQU-IDDC2-KEY  TO W-IDDC-B6                                 
036101                                RESP-IDDC2-KEY                            
036201                                W-IDDC                                    
036301        PERFORM IMS-GU-WDB601                                             
036401        IF SEGMENT-SAKNAS                                                 
036501          MOVE NEJ TO NYCKLAR-SW                                          
036601          MOVE 'IDDC'          TO RESP-IDELMT-ERROR                       
036701          MOVE '023'           TO RESP-IDMSG-ERROR                        
036801        END-IF                                                            
036901     ELSE                                                                 
037001        MOVE REQU-IDDC-KEY   TO W-IDDC-B6                                 
037101                                RESP-IDDC-KEY                             
037201                                W-IDDC                                    
037301        PERFORM IMS-GU-WDB601                                             
037401        IF SEGMENT-SAKNAS                                                 
037501          MOVE NEJ TO NYCKLAR-SW                                          
037601          MOVE 'IDDC'          TO RESP-IDELMT-ERROR                       
037701          MOVE '023'           TO RESP-IDMSG-ERROR                        
037801        END-IF                                                            
037901                                                                          
038001     END-IF                                                               
038101                                                                          
038701     IF REQU-IDARTNR-KEY NOT = ALL '+'                                    
             IF REQU-IDARTNR-KEY NOT NUMERIC                                    
               MOVE ZERO TO REQU-IDARTNR-KEY                                    
             END-IF                                                             
038901       IF REQU-IDARTNR-KEY NUMERIC                                        
039001         MOVE REQU-IDARTNR-KEY  TO W-IDARTNR                              
039101                                   RESP-IDARTNR-KEY                       
039201       ELSE                                                               
039301         MOVE NEJ TO NYCKLAR-SW                                           
039401         MOVE 'IDARTNR'         TO RESP-IDELMT-ERROR                      
039501         MOVE '023'             TO RESP-IDMSG-ERROR                       
039601       END-IF                                                             
039701     ELSE                                                                 
039801       MOVE NEJ TO NYCKLAR-SW                                             
039901       MOVE 'IDARTNR'           TO RESP-IDELMT-ERROR                      
040001       MOVE '023'               TO RESP-IDMSG-ERROR                       
040101     END-IF                                                               
040201                                                                          
040301     IF REQU-SHOW-KEY NOT = ALL '+'                                       
040401       IF REQU-SHOW-KEY = 'Y' OR 'J' OR 'N'                               
040501        IF REQU-SHOW-KEY = 'J'                                            
040601          MOVE 'Y'             TO RESP-SHOW-KEY                           
040701          MOVE 'Y'             TO REQU-SHOW-KEY                           
040801        ELSE                                                              
040901          MOVE REQU-SHOW-KEY   TO RESP-SHOW-KEY                           
041001        END-IF                                                            
041101       ELSE                                                               
041201         MOVE NEJ TO NYCKLAR-SW                                           
041301         MOVE 'SHOW' TO RESP-IDELMT-ERROR                                 
041401         MOVE '023'  TO RESP-IDMSG-ERROR                                  
041501       END-IF                                                             
041601     ELSE                                                                 
041701       MOVE 'N' TO RESP-SHOW-KEY                                          
041801     END-IF                                                               
041901                                                                          
042001     .                                                                    
042101     EJECT                                                                
042201 BA-CHECK-LEVEL   SECTION.                                                
042301     MOVE NEJ TO LEVEL2-SW                                                
042401     MOVE NEJ TO DC-UPPDAT-SW                                             
042501**** KOLLA OM PROF-IDDC = EN LEVEL2, FÅR UPPDATERA ALLA LEVEL3            
042601     MOVE REQU-IDDC-KEY TO W-IDDC-6332                                    
042701     PERFORM IMS-GHU-WDGX6332-UNIK                                        
042801     IF SEGMENT-FINNS                                                     
042901       MOVE JA TO LEVEL2-SW                                               
043001       MOVE W-IDDC-6332 TO WS-SPAR-IDDC-6332                              
043101** KOLLA OM INMATA DC FINNS PÅ SAMMA BEN SOM PROF IDDC                    
043201       IF REQU-IDDC2-KEY NOT = ALL '+'                                    
043301         MOVE W-IDDC TO W-IDDC-6334                                       
043401         PERFORM IMS-GET-WDGX6334-UNIK                                    
043501         IF SEGMENT-FINNS                                                 
043601           MOVE JA TO DC-UPPDAT-SW                                        
043701         END-IF                                                           
043801       END-IF                                                             
043901     END-IF                                                               
044001*** KOLLA W-IDDC SOM KAN VARA PROF ELLER INMATAT DC                       
044101     IF UPPDAT-INTE-GODKAND OR LEVEL-3-IDDC                               
044201      PERFORM IMS-GET-WDGX6331                                            
044301      IF SEGMENT-FINNS                                                    
044401                                                                          
044501       MOVE W-IDDC        TO W-IDDC-6332                                  
044601       PERFORM IMS-GHU-WDGX6332-UNIK                                      
044701       IF SEGMENT-FINNS                                                   
044801         MOVE W-IDDC-6332 TO WS-SPAR-IDDC-6332                            
044901         MOVE '20' TO LEVEL-SW                                            
045001*** IDDC INMAMTAT                                                         
045101         IF REQU-IDDC-KEY NOT = W-IDDC                                    
045201           IF 6332-IDUSER(1) NOT = SPACE                                  
045301             IF REQU-IDUSER NOT = 6332-IDUSER(1)                          
045401               MOVE '30' TO LEVEL-SW                                      
045501             ELSE                                                         
045601               MOVE '20' TO LEVEL-SW                                      
045701             END-IF                                                       
045801           ELSE                                                           
045901             MOVE '30' TO LEVEL-SW                                        
046001           END-IF                                                         
046101           IF LEVEL3-UTAN-USER                                            
046201             IF 6332-IDUSER(2) NOT = SPACE                                
046301               IF REQU-IDUSER = 6332-IDUSER(2)                            
046401                 MOVE '20' TO LEVEL-SW                                    
046501               END-IF                                                     
046601             END-IF                                                       
046701           END-IF                                                         
046801           IF LEVEL3-UTAN-USER                                            
046901             IF 6332-IDUSER(3) NOT = SPACE                                
047001               IF REQU-IDUSER = 6332-IDUSER(3)                            
047101                 MOVE '20' TO LEVEL-SW                                    
047201               END-IF                                                     
047301             END-IF                                                       
047401           END-IF                                                         
047501           IF LEVEL3-UTAN-USER                                            
047601             IF 6332-IDUSER(4) NOT = SPACE                                
047701               IF REQU-IDUSER = 6332-IDUSER(4)                            
047801                 MOVE '20' TO LEVEL-SW                                    
047901               END-IF                                                     
048001             END-IF                                                       
048101           END-IF                                                         
048201           IF LEVEL3-UTAN-USER                                            
048301             MOVE NEJ TO NYCKLAR-SW                                       
048401             MOVE 'IDDC'          TO RESP-IDELMT-ERROR                    
048501             MOVE '023'           TO RESP-IDMSG-ERROR                     
048601           END-IF                                                         
048701         END-IF                                                           
048801       ELSE                                                               
048901                                                                          
049001**  REQU-IDDC ÄR EN LEVEL 3 , HITTA VILKET DC SOM ÄR LEVEL2               
049101        PERFORM IMS-GET-WDGX6331                                          
049201        PERFORM IMS-GNP-WDGX6332                                          
049301****    MOVE REQU-IDDC-KEY TO W-IDDC-6334                                 
049401        MOVE W-IDDC        TO W-IDDC-6334                                 
049501        PERFORM UNTIL SEGMENT-SAKNAS OR POST-FINNS                        
049601          IF SEGMENT-FINNS                                                
049701            MOVE 6332-IDDC   TO W-IDDC-6332                               
049801            MOVE W-IDDC-6332 TO WS-SPAR-IDDC-6332                         
049901            PERFORM IMS-GET-WDGX6334-UNIK                                 
050001            IF SEGMENT-FINNS                                              
050101               MOVE JA TO POST-SW                                         
050201               IF 6332-IDUSER(1) NOT = SPACE                              
050301                 IF REQU-IDUSER NOT = 6332-IDUSER(1)                      
050401                   MOVE '30' TO LEVEL-SW                                  
050501                 ELSE                                                     
050601                   MOVE '31' TO LEVEL-SW                                  
050701                 END-IF                                                   
050801               ELSE                                                       
050901                 MOVE '30' TO LEVEL-SW                                    
051001               END-IF                                                     
051101               IF LEVEL3-UTAN-USER                                        
051201                 IF 6332-IDUSER(2) NOT = SPACE                            
051301                   IF REQU-IDUSER = 6332-IDUSER(2)                        
051401                     MOVE '31' TO LEVEL-SW                                
051501                   END-IF                                                 
051601                 END-IF                                                   
051701               END-IF                                                     
051801               IF LEVEL3-UTAN-USER                                        
051901                 IF 6332-IDUSER(3) NOT = SPACE                            
052001                   IF REQU-IDUSER  = 6332-IDUSER(3)                       
052101                     MOVE '31' TO LEVEL-SW                                
052201                   END-IF                                                 
052301                 END-IF                                                   
052401               END-IF                                                     
052501               IF LEVEL3-UTAN-USER                                        
052601                 IF 6332-IDUSER(4) NOT = SPACE                            
052701                   IF REQU-IDUSER  = 6332-IDUSER(4)                       
052801                     MOVE '31' TO LEVEL-SW                                
052901                   END-IF                                                 
053001                 END-IF                                                   
053101               END-IF                                                     
053201                                                                          
053301               IF W-IDDC-6334 NOT = REQU-IDDC-KEY                         
053401                 IF REQU-IDDC-KEY NOT = W-IDDC-6332                       
053501*****              LEVEL2 DC INTE LIKA MED PROFIL DC                      
053601*****              LEVEL3 HAR INTE RÄTT LEVEL2 IDDC                       
053701                     IF LEVEL3-UTAN-USER                                  
053801                       MOVE NEJ TO NYCKLAR-SW                             
053901                       MOVE 'IDDC'          TO RESP-IDELMT-ERROR          
054001                       MOVE '023'           TO RESP-IDMSG-ERROR           
054101                     END-IF                                               
054201                 END-IF                                                   
054301                 IF REQU-IDDC-KEY NOT = W-IDDC-6334                       
054401****             PROF-IDDC INTE LIKA MED LEVEL3 IDDC                      
054501                   IF LEVEL3-UTAN-USER                                    
054601                     MOVE NEJ TO NYCKLAR-SW                               
054701                     MOVE 'IDDC'          TO RESP-IDELMT-ERROR            
054801                     MOVE '023'           TO RESP-IDMSG-ERROR             
054901                   END-IF                                                 
055001                 END-IF                                                   
055101               END-IF                                                     
055201            ELSE                                                          
055301              PERFORM IMS-GNP-WDGX6332                                    
055401            END-IF                                                        
055501          END-IF                                                          
055601        END-PERFORM                                                       
055701        IF POST-SAKNAS                                                    
055801* FEJKAR ATT DC ÄR EN LEVEL 2 NÄR DEN SAKNAS PÅ DC TRÄDET                 
055901          MOVE '20' TO LEVEL-SW                                           
056001          MOVE JA   TO LEVEL2-SW                                          
056101          MOVE REQU-IDDC-KEY TO W-IDDC-6332                               
056201                                W-IDDC                                    
056301                                WS-SPAR-IDDC-6332                         
056401*         MOVE NEJ  TO NYCKLAR-SW                                         
056501*         MOVE 'IDDC'          TO RESP-IDELMT-ERROR                       
056601*         MOVE '023'           TO RESP-IDMSG-ERROR                        
056701        END-IF                                                            
056801       END-IF                                                             
056901      END-IF                                                              
057001     END-IF                                                               
057101     .                                                                    
057201     EJECT                                                                
057301 C-KOLLA-INPUT    SECTION.                                                
057401     MOVE 'C-KOLLA-INPUT' TO CURR-SECTION                                 
057501                                                                          
057601                                                                          
057701     IF REQU-INPUT = ALL '+' OR LOW-VALUE                                 
057801     OR REQU-KVRADER = ALL '+'                                            
057901        MOVE '014'           TO RESP-IDMSG-ERROR                          
058001*       EXECUTE BUT NO DATA ***                                           
058101        SET INDATA-FEL TO TRUE                                            
058201        MOVE ZERO TO REQU-KVRADER                                         
058301     ELSE                                                                 
058401        PERFORM CA-KOLLA-INDATAFAELT-FORMELLT                             
058501        IF INDATA-OK                                                      
058601           PERFORM IMS-GET-UNIK-WLARTC01                                  
058701           IF SEGMENT-SAKNAS                                              
058801              MOVE '025'       TO RESP-IDMSG-ERROR                        
058901              MOVE 'IDARTNR'   TO RESP-IDELMT-ERROR                       
059001              SET INDATA-FEL TO TRUE                                      
059101           ELSE                                                           
059201              PERFORM IMS-GET-UNIK-WLARTS01                               
059301              IF SEGMENT-FINNS                                            
059401                 PERFORM CB-KOLLA-LDC                                     
059501              ELSE                                                        
059601                 SET INDATA-FEL TO TRUE                                   
059701                 MOVE '025'     TO RESP-IDMSG-ERROR                       
059801                 MOVE 'IDARTNR' TO RESP-IDELMT-ERROR                      
059901              END-IF                                                      
060001           END-IF                                                         
060101        END-IF                                                            
060201     END-IF                                                               
060301     .                                                                    
060401     EJECT                                                                
060501 CA-KOLLA-INDATAFAELT-FORMELLT SECTION.                                   
060601     MOVE 'CA-KOLLA-INDATAFAELT-FORMELLT' TO CURR-SECTION                 
060701     MOVE +1 TO INDX                                                      
060801     PERFORM UNTIL INDX > REQU-KVRADER                                    
060901       IF REQU-KDLEVSP-UPD(INDX) NOT = ALL '+'                            
061001          IF REQU-KDLEVSP-UPD(INDX) NOT NUMERIC                           
061101             MOVE '023'        TO RESP-IDMSG-ERROR                        
061201***          IS INVALID ***                                               
061301             MOVE 'KDLEVSP'    TO RESP-IDELMT-ERROR                       
061401             MOVE NEJ          TO INDATA-SW                               
061501          END-IF                                                          
061601       END-IF                                                             
061701                                                                          
061801       IF REQU-KVSPARR-KVAL-UPD(INDX) NOT = ALL '+'                       
061901         IF REQU-KVSPARR-KVAL-UPD(INDX) NOT NUMERIC                       
062001            MOVE '023'         TO RESP-IDMSG-ERROR                        
062101***         IS INVALID ***                                                
062201            MOVE 'KVSPARR'     TO RESP-IDELMT-ERROR                       
062301            MOVE NEJ TO INDATA-SW                                         
062401         END-IF                                                           
062501       END-IF                                                             
062601       ADD +1 TO INDX                                                     
062701     END-PERFORM                                                          
062801     .                                                                    
062901     EJECT                                                                
063001 CB-KOLLA-LDC SECTION.                                                    
063101     MOVE 'CB-KOLLA-LDC' TO CURR-SECTION                                  
063201     MOVE +1 TO INDX                                                      
063301     PERFORM UNTIL INDX > REQU-KVRADER                                    
063401       MOVE REQU-IDDC(INDX) TO W-IDDC                                     
063501                               W-IDDC-B6                                  
063601       PERFORM IMS-GET-UNIK-WLARTS11                                      
063701       IF SEGMENT-FINNS                                                   
063801***  KOLLA OM WEB LDC                                                     
063901         PERFORM IMS-GU-WDB601                                            
064001         IF DCS-FLWEBDC = 'N' OR ' '                                      
064101           MOVE NEJ TO NYCKLAR-SW                                         
064201           MOVE 'IDDC'          TO RESP-IDELMT-ERROR                      
064301           MOVE '023'           TO RESP-IDMSG-ERROR                       
064401           MOVE NEJ TO INDATA-SW                                          
064501         END-IF                                                           
064601         IF REQU-KDLEVSP-UPD(INDX) NOT = ALL '+'                          
064701             EVALUATE REQU-KDLEVSP-UPD(INDX)                              
064801                                                                          
064901             WHEN ZERO                                                    
065001                IF SLAG-KDLEVSP = 20 OR 21                                
065101                   MOVE '023'     TO RESP-IDMSG-ERROR                     
065201***                IS INVALID   ***                                       
065301                   MOVE 'KDLEVSP' TO RESP-IDELMT-ERROR                    
065401                   SET INDATA-FEL TO TRUE                                 
065501                END-IF                                                    
065601                                                                          
065701             WHEN 21                                                      
065801                MOVE '023'        TO RESP-IDMSG-ERROR                     
065901***             IS INVALID      ***                                       
066001                MOVE 'KDLEVSP' TO RESP-IDELMT-ERROR                       
066101                SET INDATA-FEL TO TRUE                                    
066201                                                                          
066301             WHEN 22                                                      
066401                IF SLAG-KDLEVSP = 20 OR 21                                
066501                   MOVE '023'     TO RESP-IDMSG-ERROR                     
066601***                IS INVALID   ***                                       
066701                   MOVE 'KDLEVSP' TO RESP-IDELMT-ERROR                    
066801                   SET INDATA-FEL TO TRUE                                 
066901                ELSE                                                      
067001                   IF SLAG-KVSPARR-KVAL > ZERO                            
067101                      IF REQU-KVSPARR-KVAL-UPD(INDX) = ZERO               
067201                         CONTINUE                                         
067301                      ELSE                                                
067401                         MOVE '023'     TO RESP-IDMSG-ERROR               
067501***                      IS INVALID ***                                   
067601                         MOVE 'KVSPARR' TO RESP-IDELMT-ERROR              
067701                         SET INDATA-FEL TO TRUE                           
067801                      END-IF                                              
067901                   END-IF                                                 
068001                END-IF                                                    
068101                                                                          
068201             WHEN OTHER                                                   
068301                MOVE '023'     TO RESP-IDMSG-ERROR                        
068401***             IS INVALID   ***                                          
068501                MOVE 'KDLEVSP' TO RESP-IDELMT-ERROR                       
068601                SET INDATA-FEL TO TRUE                                    
068701             END-EVALUATE                                                 
068801         END-IF                                                           
068901                                                                          
069001         IF REQU-KVSPARR-KVAL-UPD(INDX) NOT = ALL '+'                     
069101             IF REQU-KVSPARR-KVAL-UPD(INDX) = ZERO                        
069201                CONTINUE                                                  
069301             ELSE                                                         
069401                IF REQU-KDLEVSP-UPD(INDX) NOT = ALL '+'                   
069501                   IF REQU-KDLEVSP-UPD(INDX) = ZERO                       
069601                      CONTINUE                                            
069701                   ELSE                                                   
069801                      MOVE '023'     TO RESP-IDMSG-ERROR                  
069901***                   IS INVALID ***                                      
070001                      MOVE 'KDLEVSP' TO RESP-IDELMT-ERROR                 
070101                      SET INDATA-FEL TO TRUE                              
070201                   END-IF                                                 
070301                ELSE                                                      
070401                   IF SLAG-KDLEVSP NOT = ZERO                             
070501                      MOVE '023'     TO RESP-IDMSG-ERROR                  
070601***                   IS INVALID ***                                      
070701                      MOVE 'KDLEVSP' TO RESP-IDELMT-ERROR                 
070801                      SET INDATA-FEL TO TRUE                              
070901                   END-IF                                                 
071001                END-IF                                                    
071101             END-IF                                                       
071201         END-IF                                                           
071301         IF INDATA-OK                                                     
071401           IF REQU-KDLEVSP-UPD(INDX)  = ALL '+' AND                       
071501              REQU-KVSPARR-KVAL-UPD(INDX) = ALL '+'                       
071601             CONTINUE                                                     
071701           ELSE                                                           
071801            IF POST-FINNS                                                 
071901              PERFORM CBA-KOLLA-DC-TRAD                                   
072001            ELSE                                                          
072101              MOVE JA TO DC-UPPDAT-SW                                     
072201            END-IF                                                        
072301            IF UPPDAT-INTE-GODKAND                                        
072401               MOVE NEJ TO INDATA-SW                                      
072501               MOVE '007'     TO RESP-IDMSG-ERROR                         
072601***            IS INVALID ***                                             
072701***            MOVE 'IDDC' TO RESP-IDELMT-ERROR                           
072801            END-IF                                                        
072901           END-IF                                                         
073001         END-IF                                                           
073101       ELSE                                                               
073201          IF REQU-TEKVAL(INDX) = ALL '+' OR SPACE                         
073301             CONTINUE                                                     
073401          ELSE                                                            
073501             MOVE '023'     TO RESP-IDMSG-ERROR                           
073601***          IS INVALID ***                                               
073701             MOVE 'TEKVAL'  TO RESP-IDELMT-ERROR                          
073801             MOVE NEJ TO INDATA-SW                                        
073901          END-IF                                                          
074001       END-IF                                                             
074101       ADD +1 TO INDX                                                     
074201     END-PERFORM                                                          
074301     .                                                                    
074401     EJECT                                                                
074501 CBA-KOLLA-DC-TRAD SECTION.                                               
074601     MOVE NEJ TO DC-UPPDAT-SW                                             
074701     MOVE NEJ TO LEVEL2-SW                                                
074801                                                                          
074901**** KOLLA OM PROF-IDDC = EN LEVEL2, FÅR UPPDATERA ALLA LEVEL3            
075001**** OCH INMATAT DC INTE ÄR I FYLLD                                       
075101     IF REQU-IDDC2-KEY = ALL '+'                                          
075201       MOVE REQU-IDDC-KEY TO W-IDDC-6332                                  
075301       PERFORM IMS-GHU-WDGX6332-UNIK                                      
075401       IF SEGMENT-FINNS                                                   
075501         MOVE JA TO DC-UPPDAT-SW                                          
075601         MOVE JA TO LEVEL2-SW                                             
075701         MOVE W-IDDC-6332 TO WS-SPAR-IDDC-6332                            
075801       END-IF                                                             
075901     END-IF                                                               
076001* KOLLA OM PROF IDDC OCH INMATA IDDC FINNS PÅ SAMMA GREN                  
076101     IF REQU-IDDC2-KEY NOT = ALL '+'                                      
076201       MOVE REQU-IDDC-KEY TO W-IDDC-6332                                  
076301       MOVE REQU-IDDC2-KEY TO W-IDDC-6334                                 
076401       PERFORM IMS-GET-WDGX6334-UNIK                                      
076501       IF SEGMENT-FINNS                                                   
076601         MOVE JA TO DC-UPPDAT-SW                                          
076701         MOVE JA TO LEVEL2-SW                                             
076801         MOVE W-IDDC-6332 TO WS-SPAR-IDDC-6332                            
076901       END-IF                                                             
077001     END-IF                                                               
077101                                                                          
077201**** KOLLA OM W-IDDC = EN LEVEL2, OM REQU-IDDC2-KEY INMATAT               
077301     IF REQU-IDDC2-KEY NOT = ALL '+' AND UPPDAT-INTE-GODKAND              
077401       MOVE REQU-IDDC2-KEY  TO W-IDDC-6332                                
077501       PERFORM IMS-GHU-WDGX6332-UNIK                                      
077601       IF SEGMENT-FINNS                                                   
077701         IF 6332-IDUSER(1) NOT = SPACE                                    
077801           IF REQU-IDUSER NOT = 6332-IDUSER(1)                            
077901             MOVE NEJ  TO DC-UPPDAT-SW                                    
078001           ELSE                                                           
078101             MOVE JA   TO DC-UPPDAT-SW                                    
078201           END-IF                                                         
078301         END-IF                                                           
078401                                                                          
078501         IF UPPDAT-INTE-GODKAND                                           
078601           IF 6332-IDUSER(2) NOT = SPACE                                  
078701             IF REQU-IDUSER = 6332-IDUSER(2)                              
078801               MOVE JA   TO DC-UPPDAT-SW                                  
078901             END-IF                                                       
079001           END-IF                                                         
079101         END-IF                                                           
079201                                                                          
079301         IF UPPDAT-INTE-GODKAND                                           
079401           IF 6332-IDUSER(3) NOT = SPACE                                  
079501             IF REQU-IDUSER = 6332-IDUSER(3)                              
079601               MOVE JA   TO DC-UPPDAT-SW                                  
079701             END-IF                                                       
079801           END-IF                                                         
079901         END-IF                                                           
080001                                                                          
080101         IF UPPDAT-INTE-GODKAND                                           
080201           IF 6332-IDUSER(4) NOT = SPACE                                  
080301             IF REQU-IDUSER = 6332-IDUSER(4)                              
080401               MOVE JA   TO DC-UPPDAT-SW                                  
080501             END-IF                                                       
080601           END-IF                                                         
080701         END-IF                                                           
080801         IF UPPDAT-OK                                                     
080901           MOVE JA TO DC-UPPDAT-SW                                        
081001           MOVE JA TO LEVEL2-SW                                           
081101           MOVE W-IDDC-6332 TO WS-SPAR-IDDC-6332                          
081201         END-IF                                                           
081301       END-IF                                                             
081401     END-IF                                                               
081501                                                                          
081601     IF UPPDAT-INTE-GODKAND                                               
081701***** KOLLA OM RAD-IDDC = PROFIL IDDC, UPPDAT OK                          
081801       IF REQU-IDDC (INDX) = REQU-IDDC-KEY                                
081901         MOVE JA TO DC-UPPDAT-SW                                          
082001       ELSE                                                               
082101****** KOLLA OM RAD-IDDC = LEVEL2 OCH OM USERID FINNS PÅ LEVEL2           
082201         IF UPPDAT-INTE-GODKAND                                           
082301           MOVE REQU-IDDC(INDX) TO W-IDDC-6332                            
082401           PERFORM IMS-GHU-WDGX6332-UNIK                                  
082501           IF SEGMENT-FINNS                                               
082601             IF 6332-IDUSER(1) NOT = SPACE                                
082701               IF REQU-IDUSER NOT = 6332-IDUSER(1)                        
082801                 MOVE NEJ  TO DC-UPPDAT-SW                                
082901               ELSE                                                       
083001                 MOVE JA   TO DC-UPPDAT-SW                                
083101               END-IF                                                     
083201             END-IF                                                       
083301                                                                          
083401             IF UPPDAT-INTE-GODKAND                                       
083501               IF 6332-IDUSER(2) NOT = SPACE                              
083601                 IF REQU-IDUSER = 6332-IDUSER(2)                          
083701                   MOVE JA   TO DC-UPPDAT-SW                              
083801                 END-IF                                                   
083901               END-IF                                                     
084001             END-IF                                                       
084101                                                                          
084201             IF UPPDAT-INTE-GODKAND                                       
084301               IF 6332-IDUSER(3) NOT = SPACE                              
084401                 IF REQU-IDUSER = 6332-IDUSER(3)                          
084501                   MOVE JA   TO DC-UPPDAT-SW                              
084601                 END-IF                                                   
084701               END-IF                                                     
084801             END-IF                                                       
084901                                                                          
085001             IF UPPDAT-INTE-GODKAND                                       
085101               IF 6332-IDUSER(4) NOT = SPACE                              
085201                 IF REQU-IDUSER = 6332-IDUSER(4)                          
085301                   MOVE JA   TO DC-UPPDAT-SW                              
085401                 END-IF                                                   
085501               END-IF                                                     
085601             END-IF                                                       
085701           END-IF                                                         
085801         END-IF                                                           
085901       END-IF                                                             
086001     END-IF                                                               
086101     .                                                                    
086201     EJECT                                                                
086301 D-UPPDATERA SECTION.                                                     
086401     MOVE 'D-UPPDATERA' TO CURR-SECTION                                   
086501                                                                          
086601     MOVE NEJ TO SW-UPPDAT                                                
086701     MOVE NEJ TO SW-UPPDAT1                                               
086801     MOVE +1 TO INDX                                                      
086901                                                                          
087001     IF LEVEL-2-IDDC                                                      
087101       PERFORM DB-KOLLA-KDLEVSP                                           
087201     END-IF                                                               
087301     PERFORM UNTIL INDX > REQU-KVRADER                                    
087401       MOVE REQU-IDDC(INDX) TO W-IDDC                                     
087501       MOVE 'LÄSA WDK7 ' TO INFO-TEXT1                                    
087601       PERFORM IMS-GET-UNIK-WLARTS11                                      
087701       IF SEGMENT-FINNS                                                   
087801         MOVE 'WDK7 FANNS ' TO INFO-TEXT2                                 
087901          MOVE SLAG-WDK711 TO TEST-SLAG-WDK711                            
088001                                                                          
088101          MOVE NEJ          TO STOCK-BALANCE-SW                           
088201          IF SLAG-KVLS      >  ZERO                                       
088301          OR SLAG-KVEFRS    >  ZERO                                       
088401          OR SLAG-KVAKS-PAV >  ZERO                                       
088501          OR SLAG-KVAKS-SDC >  ZERO                                       
088601             MOVE JA        TO STOCK-BALANCE-SW                           
088701          END-IF                                                          
088801                                                                          
088901          IF REQU-KDLEVSP-UPD(INDX) NOT = ALL '+'                         
089001             MOVE REQU-KDLEVSP-UPD(INDX) TO TEST-SLAG-KDLEVSP             
089101             IF  TEST-SLAG-KDLEVSP     = 22                               
089201             AND STOCK-BAL-NO                                             
089301                 CONTINUE                                                 
089401             ELSE                                                         
089501              IF TEST-SLAG-KDLEVSP NOT = SLAG-KDLEVSP                     
089601                MOVE TEST-SLAG-KDLEVSP TO RESP-KDLEVSP(INDX)              
089701                                          SLAG-KDLEVSP                    
089801                MOVE REQU-IDUSER       TO SLAG-IDUSER-SPKVAL              
089901                MOVE DAGENS-DATUM      TO SLAG-TISPARR-KVAL               
090001                IF SLAG-FLREFNYO = JA                                     
090101                   MOVE NEJ            TO SLAG-FLREFNYO                   
090201                END-IF                                                    
090301                MOVE JA TO SW-UPPDAT                                      
090401              ELSE                                                        
090501               MOVE NEJ TO SW-UPPDAT                                      
090601              END-IF                                                      
090701             END-IF                                                       
090801          END-IF                                                          
090901                                                                          
091001          IF REQU-KVSPARR-KVAL-UPD(INDX) NOT = ALL '+'                    
091101             MOVE 'I IF-SATS KVSPARR-KVAL(INDX) ' TO INFO-TEXT3           
091201             MOVE REQU-KVSPARR-KVAL-UPD(INDX) TO                          
091301                                 TEST-SLAG-KVSPARR-KVAL                   
091401             IF TEST-SLAG-KVSPARR-KVAL NOT =                              
091501                                          SLAG-KVSPARR-KVAL               
091601                MOVE TEST-SLAG-KVSPARR-KVAL TO                            
091701                                       SLAG-KVSPARR-KVAL                  
091801                                       RESP-KVSPARR-KVAL(INDX)            
091901                MOVE REQU-IDUSER    TO SLAG-IDUSER-SPKVAL                 
092001                MOVE DAGENS-DATUM   TO SLAG-TISPARR-KVAL                  
092101                IF SLAG-FLREFNYO = JA                                     
092201                   MOVE NEJ         TO SLAG-FLREFNYO                      
092301                END-IF                                                    
092401                MOVE JA TO SW-UPPDAT                                      
092501                MOVE ' SKA UPPDAT WDK7' TO INFO-TEXT3                     
092601                                                                          
092701             END-IF                                                       
092801          END-IF                                                          
092901                                                                          
093001          IF SLAG-KDLEVSP       = ZERO                                    
093101          AND SLAG-KVSPARR-KVAL = ZERO                                    
093201              MOVE SPACE TO TEST-SLAG-TEKVAL                              
093301          ELSE                                                            
093401             IF REQU-TEKVAL(INDX) NOT = ALL '+'                           
093501                MOVE REQU-TEKVAL(INDX) TO TEST-SLAG-TEKVAL                
093601             END-IF                                                       
093701          END-IF                                                          
093801                                                                          
093901          IF TEST-SLAG-TEKVAL NOT = SLAG-TEKVAL                           
094001             MOVE TEST-SLAG-TEKVAL TO SLAG-TEKVAL                         
094101                                      RESP-TEKVAL(INDX)                   
094201             MOVE REQU-IDUSER      TO SLAG-IDUSER-SPKVAL                  
094301             MOVE DAGENS-DATUM     TO SLAG-TISPARR-KVAL                   
094401             MOVE JA               TO SW-UPPDAT                           
094501          END-IF                                                          
094601          MOVE SLAG-IDUSER-SPKVAL TO                                      
094701                                    RESP-IDUSER-SPKVAL(INDX)              
094801          MOVE SLAG-TISPARR-KVAL TO RESP-TISPARR-KVAL(INDX)               
094901       END-IF                                                             
095001                                                                          
095101       IF SW-UPPDAT = JA                                                  
095201          PERFORM IMS-REPL-WLARTS11                                       
095301          PERFORM HFB-CHECK-REPL-WGX2404                                  
095401          MOVE '001' TO RESP-IDMSG-INFO                                   
095501          MOVE JA    TO SW-UPPDAT1                                        
095601          MOVE NEJ   TO SW-UPPDAT                                         
095701       END-IF                                                             
095801                                                                          
095901     ADD +1 TO INDX                                                       
096001     END-PERFORM                                                          
096101     IF SW-UPPDAT1 = JA                                                   
096201        MOVE '001' TO RESP-IDMSG-INFO                                     
096301     ELSE                                                                 
096401        MOVE '004' TO RESP-IDMSG-INFO                                     
096501     END-IF                                                               
096601     .                                                                    
096701     EJECT                                                                
096801 DB-KOLLA-KDLEVSP SECTION.                                                
096901     MOVE +1 TO IX                                                        
097001     IF REQU-KDLEVSP-UPD(IX) NOT = ALL '+' AND                            
097101        REQU-IDDC(IX)  = REQU-IDDC-KEY                                    
097201       MOVE REQU-KDLEVSP-UPD(IX) TO WS-KDLEVSP-UPD                        
097301       ADD +1 TO IX                                                       
097401       PERFORM UNTIL IX > REQU-KVRADER                                    
097501         MOVE WS-KDLEVSP-UPD TO REQU-KDLEVSP-UPD(IX)                      
097601       ADD +1 TO IX                                                       
097701       END-PERFORM                                                        
097801     END-IF                                                               
097901     .                                                                    
098001     EJECT                                                                
098101 E-LAES-VISA-INFO SECTION.                                                
098201     MOVE 'E-LAES-VISA-INFO' TO CURR-SECTION                              
098301                                                                          
098401     PERFORM IMS-GET-BENA01-BSEQ                                          
098501     IF SEGMENT-FINNS                                                     
098601*       MOVE SPACE            TO RESP-WL0113O1                            
098701        MOVE REQU-IDDC-KEY    TO RESP-IDDC-KEY                            
098801                                 WS-IDDC                                  
              MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                           
              IF DCS-UNICODE-IDSKYLT                                            
                 MOVE 'UTF8'             TO TRAUTF8-KDCP                        
              ELSE                                                              
                 MOVE '278 '             TO TRAUTF8-KDCP                        
                 IF REQU-BELANG-KEY = 'EN'                                      
                    MOVE 'GB ' TO W-IDSKYLT                                     
                 ELSE                                                           
                    MOVE 'S  ' TO W-IDSKYLT                                     
                 END-IF                                                         
              END-IF                                                            
100001        MOVE REQU-IDARTNR-KEY TO RESP-IDARTNR-KEY                         
100101        MOVE REQU-BELANG-KEY  TO RESP-BELANG-KEY                          
100201        PERFORM IMS-GET-BENA11                                            
              IF SEGMENT-FINNS                                                  
100301         MOVE BENA-TEXT-BEART   TO TRAUTF8-TECONV-FROM                    
              ELSE                                                              
               MOVE SPACES TO TRAUTF8-TECONV-FROM                               
              END-IF                                                            
              IF TRAUTF8-TECONV-FROM = SPACES                                   
               MOVE 'GB'  TO W-IDSKYLT                                          
               MOVE '278' TO TRAUTF8-KDCP                                       
               PERFORM IMS-GET-BENA01-BSEQ                                      
               PERFORM IMS-GET-BENA11                                           
               IF SEGMENT-FINNS                                                 
               MOVE BENA-TEXT-BEART    TO TRAUTF8-TECONV-FROM                   
               END-IF                                                           
              END-IF                                                            
100401*       -- STRIP SPACE OR CONVERT TO UNICODE                              
100501        CALL WTRAUTF8 USING TRAUTF8-AREA                                  
100601                                                                          
100701*       -- MOVE CONVERTED DESCRIPTION TO THE RESPONSE                     
100801        MOVE TRAUTF8-TECONV-TO TO RESP-BEART                              
100901        PERFORM IMS-GET-UNIK-WLARTC01                                     
101001     END-IF                                                               
101101                                                                          
101201     IF SEGMENT-SAKNAS                                                    
101301        MOVE '025'       TO RESP-IDMSG-ERROR                              
101401        MOVE 'IDARTNR'   TO RESP-IDELMT-ERROR                             
101501     ELSE                                                                 
101601        IF ART-KDERS-UTG = ZERO                                           
101701           PERFORM IMS-GET-NEXT-WLARTC11                                  
101801           IF SEGMENT-FINNS                                               
101901*** HÄMTA DC                                                              
102001             IF LEVEL3-UTAN-USER OR LEVEL3-MED-USER                       
102101***           LÄS NU MED LEVEL 3 IDDC                                     
102201              MOVE +1 TO INDX                                             
102301              MOVE W-IDDC-6334    TO W-IDDC                               
102401              PERFORM IMS-GET-UNIK-WLARTS01                               
102501              IF SEGMENT-FINNS                                            
102601                 PERFORM IMS-GET-UNIK-WLARTS11                            
102701                 IF SEGMENT-SAKNAS                                        
102801                    CONTINUE                                              
102901                 ELSE                                                     
103001                    MOVE W-IDDC             TO RESP-IDDC(INDX)            
103101                    MOVE SLAG-TEKVAL        TO RESP-TEKVAL(INDX)          
103201                    MOVE SLAG-KDLEVSP       TO RESP-KDLEVSP(INDX)         
103301                    MOVE SLAG-KVSPARR-KVAL  TO                            
103401                                          RESP-KVSPARR-KVAL(INDX)         
103501                    MOVE SLAG-IDUSER-SPKVAL TO                            
103601                                          RESP-IDUSER-SPKVAL(INDX)        
103701                    MOVE SLAG-TISPARR-KVAL  TO                            
103801                                          RESP-TISPARR-KVAL(INDX)         
103901                    MOVE SLAG-KVLS          TO RESP-KVLS(INDX)            
104001                    MOVE INDX TO RESP-KVRADER                             
104101                                                                          
104201                 END-IF                                                   
104301              END-IF                                                      
104401             ELSE                                                         
104501****           LEVEL2                                                     
104601               PERFORM IMS-GET-WDGX6331                                   
104701               MOVE WS-SPAR-IDDC-6332  TO W-IDDC                          
104801                                          W-IDDC-6332                     
104901               IF POST-FINNS                                              
105001                 PERFORM IMS-GNP-WDGX6332-UNIK                            
105101                 MOVE WS-SPAR-IDDC-6332  TO W-IDDC                        
105201               END-IF                                                     
105301               MOVE +0 TO INDX                                            
105401               PERFORM UNTIL SEGMENT-SAKNAS OR INDX > MAX-INDX            
105501                 PERFORM IMS-GET-UNIK-WLARTS01                            
105601                 IF SEGMENT-FINNS                                         
105701                   PERFORM IMS-GET-UNIK-WLARTS11                          
105801                   IF SEGMENT-FINNS                                       
105901                     ADD +1 TO INDX                                       
106001                     MOVE W-IDDC             TO RESP-IDDC  (INDX)         
106101                     MOVE SLAG-TEKVAL        TO RESP-TEKVAL(INDX)         
106201                     MOVE SLAG-KDLEVSP       TO RESP-KDLEVSP(INDX)        
106301                     MOVE SLAG-KVSPARR-KVAL  TO                           
106401                                          RESP-KVSPARR-KVAL(INDX)         
106501                     MOVE SLAG-IDUSER-SPKVAL TO                           
106601                                          RESP-IDUSER-SPKVAL(INDX)        
106701                     MOVE SLAG-TISPARR-KVAL  TO                           
106801                                          RESP-TISPARR-KVAL(INDX)         
106901                     MOVE SLAG-KVLS          TO RESP-KVLS(INDX)           
107001                     IF STOPP-LAES                                        
107101                       MOVE 'GE' TO STATUS-WS                             
107201                     END-IF                                               
107301                   END-IF                                                 
107401**   LÄGG UT BARA PROF IDDC OCH INMATA DC, STOPPA SEDAN LÄSNING           
107501**   OM REQU-IDDC2-KEY INMATAT                                            
107601                   IF LEVEL-2                                             
107701                     IF REQU-SHOW-KEY = 'Y'                               
107801                       PERFORM IMS-GNP-WDGX6334-UNIK                      
107901                       IF SEGMENT-FINNS                                   
108001                         MOVE 6334-IDDC TO W-IDDC                         
108101                       END-IF                                             
108201                     ELSE                                                 
108301                       MOVE 'GE' TO STATUS-WS                             
108401                     END-IF                                               
108501                   ELSE                                                   
108601                     IF REQU-IDDC2-KEY NOT = ALL '+'                      
108701                        AND SEGMENT-FINNS                                 
108801                       MOVE REQU-IDDC2-KEY TO W-IDDC-6334                 
108901                       PERFORM IMS-GET-WDGX6334-UNIK                      
109001                       IF SEGMENT-FINNS                                   
109101                         MOVE REQU-IDDC2-KEY TO W-IDDC                    
109201                           MOVE JA TO STOPP-LAES-SW                       
109301                       END-IF                                             
109401                                                                          
109501                     END-IF                                               
109601*                    I F LEVEL-2 AND REQU-SHOW-KEY = 'N'                  
109701*                    E ND-IF                                              
109801                   END-IF                                                 
109901                 END-IF                                                   
110001               END-PERFORM                                                
110101               MOVE INDX TO RESP-KVRADER                                  
110201             END-IF                                                       
110301           END-IF                                                         
110401        END-IF                                                            
110501     END-IF                                                               
110601     .                                                                    
110701     EJECT                                                                
110801 HFB-CHECK-REPL-WGX2404 SECTION.                                          
110901                                                                          
111002         MOVE SLAG-KDLEVSP TO WS-NEW-KDLEVSP                              
111102         MOVE SLAG-KVSPARR-KVAL TO                                        
111201                                   WS-NEW-KVSPARR-KVAL                    
111301                                                                          
111401         IF WS-NEW-KDLEVSP = 0 AND WS-NEW-KVSPARR-KVAL = 0                
111501            MOVE W-IDDC      TO W-IDDC-KY-MIN                             
111601                                W-IDDC-KY-MAX                             
111701            MOVE W-IDARTNR-X TO W-IDARTNR-MIN                             
111801                                W-IDARTNR-MAX                             
111901                                                                          
112001            PERFORM IMS-GHU-WDGX2404                                      
112101            IF SEGMENT-FINNS                                              
112201               PERFORM IMS-DLET-2404                                      
112301            END-IF                                                        
112401                                                                          
112501         ELSE                                                             
112601                                                                          
112701            IF WS-NEW-KDLEVSP = 0 OR WS-NEW-KVSPARR-KVAL = 0              
113401                                                                          
113501               MOVE W-IDDC      TO W-IDDC-KY-MIN                          
113601                                   W-IDDC-KY-MAX                          
113701               MOVE W-IDARTNR-X TO W-IDARTNR-MIN                          
113801                                   W-IDARTNR-MAX                          
113901                                                                          
114001               PERFORM IMS-GHU-WDGX2404                                   
114002                                                                          
114101               IF SEGMENT-FINNS                                           
114202                  IF WS-NEW-KDLEVSP = 0                                   
114203                     MOVE WS-NEW-KDLEVSP TO 2404-KDLEVSP                  
114204                  END-IF                                                  
114205                  IF WS-NEW-KVSPARR-KVAL = 0                              
114206                     MOVE WS-NEW-KVSPARR-KVAL TO                          
114207                                2404-KVSPARR-KVAL                         
114208                  END-IF                                                  
114209                  PERFORM IMS-REPL-2404                                   
114301               END-IF                                                     
114401                                                                          
114501            END-IF                                                        
114601         END-IF                                                           
114801                                                                          
114901     .                                                                    
115001     EJECT                                                                
115101                                                                          
115201*    --- DISPATCHER SECTIONS                                              
115301 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
115401                                                                          
115501     MOVE 'GETARG'               TO SUB-KDFUNC                            
115601     MOVE 'CARPARTS.LDC.DELIVERYBLOCKING' TO SUB-ADDISPABS                
115701     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
115801                                                                          
115901     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
116001                                                                          
116101     IF SUB-KDRC > 0                                                      
116201       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
116301       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
116401       DELIMITED BY SIZE INTO ERROR-TEXT                                  
116501       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
116601     END-IF                                                               
116701     .                                                                    
116801                                                                          
116901 S02-RETURN-RESPONSE SECTION.                                             
117001                                                                          
117101     MOVE 'RETURN'                   TO SUB-KDFUNC                        
117201     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
117301                                                                          
117401     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
117501                                                                          
117601     IF SUB-KDRC > 0                                                      
117701       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
117801       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
117901       DELIMITED BY SIZE INTO ERROR-TEXT                                  
118001       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
118101     END-IF                                                               
118201     .                                                                    
118301                                                                          
118401* --- IMS SEKTIONER ---                                                   
118501     SKIP3                                                                
118601 IMS-GET-UNIK-WLARTC01 SECTION.                                           
118701     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
118801          DELIMITED BY SIZE INTO SSA1                                     
118901     MOVE '  GE' TO GODK-STATUSKODER                                      
119001     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
119101     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
119201     PERFORM IMS-STATUSKONTROLL                                           
119301     .                                                                    
119401     SKIP3                                                                
119501 IMS-GET-NEXT-WLARTC11 SECTION.                                           
119601     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
119701          DELIMITED BY SIZE INTO SSA1                                     
119801     MOVE '  ' TO GODK-STATUSKODER                                        
119901     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC11 SSA1                 
120001     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
120101     PERFORM IMS-STATUSKONTROLL                                           
120201     .                                                                    
120301     SKIP3                                                                
120401 IMS-GET-UNIK-WLARTS01 SECTION.                                           
120501     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
120601          DELIMITED BY SIZE INTO SSA1                                     
120701     MOVE '  GE' TO GODK-STATUSKODER                                      
120801     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-WLARTS01 SSA1                  
120901     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
121001     PERFORM IMS-STATUSKONTROLL                                           
121101     .                                                                    
121201     SKIP3                                                                
121301 IMS-GET-UNIK-WLARTS11 SECTION.                                           
121401     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
121501          DELIMITED BY SIZE INTO SSA1                                     
121601     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
121701          DELIMITED BY SIZE INTO SSA2                                     
121801     MOVE '  GE' TO GODK-STATUSKODER                                      
121901     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-WLARTS11 SSA1 SSA2            
122001     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
122101     PERFORM IMS-STATUSKONTROLL                                           
122201     .                                                                    
122301     EJECT                                                                
122401 IMS-REPL-WLARTS11 SECTION.                                               
122501     MOVE '  ' TO GODK-STATUSKODER                                        
122601     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-WLARTS11                     
122701     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
122801     PERFORM IMS-STATUSKONTROLL                                           
122901     .                                                                    
122902 IMS-GHU-WDGX2404 SECTION.                                                
122903                                                                          
122904     STRING 'WDR501  (WDGXKEY  =' W-2403KEY-X ')'                         
122905          DELIMITED BY SIZE INTO SSA1                                     
122906     STRING 'WDGX2404(KY2404  =>' W-2404KEY-MIN-X                         
122907                    '&KY2404  =<' W-2404KEY-MAX-X')'                      
122908           DELIMITED BY SIZE INTO SSA2                                    
122909     MOVE '  GE' TO GODK-STATUSKODER                                      
122910     CALL CBLTDLI USING GHU 2404-PCB DLI-IO-WDGX2404 SSA1 SSA2            
122920     MOVE 2404-STATUS-CODE TO STATUS-WS                                   
122930     PERFORM IMS-STATUSKONTROLL                                           
122940     .                                                                    
122950     EJECT                                                                
122960                                                                          
123001     SKIP3                                                                
123101 IMS-REPL-2404 SECTION.                                                   
123201     MOVE '  ' TO GODK-STATUSKODER                                        
123301     CALL CBLTDLI USING REPL 2404-PCB DLI-IO-WDGX2404                     
123401     MOVE 2404-STATUS-CODE TO STATUS-WS                                   
123501     PERFORM IMS-STATUSKONTROLL                                           
123601     .                                                                    
123701     EJECT                                                                
123801 IMS-DLET-2404 SECTION.                                                   
123901     MOVE '  ' TO GODK-STATUSKODER                                        
124001     CALL CBLTDLI USING DLET 2404-PCB DLI-IO-WDGX2404                     
124101     MOVE 2404-STATUS-CODE TO STATUS-WS                                   
124201     PERFORM IMS-STATUSKONTROLL                                           
124301     .                                                                    
124401     EJECT                                                                
124501                                                                          
124601 IMS-GET-BENA01-BSEQ SECTION.                                             
124701     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
124801            DELIMITED BY SIZE INTO SSA1                                   
124901     MOVE '  GE' TO GODK-STATUSKODER                                      
125001     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA01 SSA1                  
125101     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
125201     PERFORM IMS-STATUSKONTROLL                                           
125301     .                                                                    
125401     SKIP3                                                                
125501 IMS-GET-BENA11 SECTION.                                                  
125601     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
125701            DELIMITED BY SIZE INTO SSA1                                   
125801     MOVE '  GE' TO GODK-STATUSKODER                                      
125901     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-WLBENA11 SSA1                 
126001     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
126101     PERFORM IMS-STATUSKONTROLL                                           
126201     .                                                                    
126301     EJECT                                                                
126401 IMS-GU-WDB601    SECTION.                                                
126501     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
126601          DELIMITED BY SIZE INTO SSA1                                     
126701     MOVE '  GE' TO GODK-STATUSKODER                                      
126801     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-WDB6 SSA1                 
126901     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
127001     PERFORM IMS-STATUSKONTROLL                                           
127101     IF SEGMENT-SAKNAS                                                    
127201         MOVE SPACE TO DCS-KDDC                                           
127301     END-IF                                                               
127401     .                                                                    
127501 IMS-GET-WDGX6331 SECTION.                                                
127601     MOVE 'IMS-GET-WDGX6331        ' TO CURR-IMS-SECTION                  
127701                                                                          
127801     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
127901          DELIMITED BY SIZE INTO SSA1                                     
128001     MOVE '  GE' TO GODK-STATUSKODER                                      
128101     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX6331 SSA1                  
128201     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
128301     PERFORM IMS-STATUSKONTROLL                                           
128401     .                                                                    
128501     EJECT                                                                
128601 IMS-GNP-WDGX6332 SECTION.                                                
128701     MOVE 'IMS-GNP-WDGX6332        ' TO CURR-IMS-SECTION                  
128801                                                                          
128901     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
129001          DELIMITED BY SIZE INTO SSA1                                     
129101                                                                          
129201     MOVE 'WDGX6332 ' TO SSA2                                             
129301                                                                          
129401     MOVE '  GE' TO GODK-STATUSKODER                                      
129501     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-WDGX6332 SSA1 SSA2            
129601     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
129701     PERFORM IMS-STATUSKONTROLL                                           
129801     .                                                                    
129901     EJECT                                                                
130001 IMS-GHU-WDGX6332-UNIK SECTION.                                           
130101     MOVE 'IMS-GHU-WDGX6332-UNIK    ' TO  CURR-IMS-SECTION                
130201                                                                          
130301     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
130401          DELIMITED BY SIZE INTO SSA1                                     
130501     STRING 'WDGX6332(IDDC     =' W-WDGXKEY-6332-X ')'                    
130601          DELIMITED BY SIZE INTO SSA2                                     
130701     MOVE '  GE' TO GODK-STATUSKODER                                      
130801     CALL CBLTDLI USING GHU WDR2-PCB DLI-IO-WDGX6332 SSA1 SSA2            
130901     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
131001     PERFORM IMS-STATUSKONTROLL                                           
131101     .                                                                    
131201     EJECT                                                                
131301 IMS-GNP-WDGX6332-UNIK SECTION.                                           
131401     MOVE 'IMS-GNP-WDGX6332-UNIK    ' TO  CURR-IMS-SECTION                
131501                                                                          
131601     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
131701          DELIMITED BY SIZE INTO SSA1                                     
131801     STRING 'WDGX6332(IDDC     =' W-WDGXKEY-6332-X ')'                    
131901          DELIMITED BY SIZE INTO SSA2                                     
132001     MOVE '  GE' TO GODK-STATUSKODER                                      
132101     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-WDGX6332 SSA1 SSA2            
132201     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
132301     PERFORM IMS-STATUSKONTROLL                                           
132401     .                                                                    
132501     EJECT                                                                
132601 IMS-GNP-WDGX6334-UNIK SECTION.                                           
132701     MOVE 'IMS-GNP-WDGX6334-UNIK   ' TO CURR-IMS-SECTION                  
132801                                                                          
132901     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
133001          DELIMITED BY SIZE INTO SSA1                                     
133101     STRING 'WDGX6332(IDDC     =' W-WDGXKEY-6332-X ')'                    
133201          DELIMITED BY SIZE INTO SSA2                                     
133301                                                                          
133401     MOVE 'WDGX6334 ' TO SSA3                                             
133501     MOVE '  GE' TO GODK-STATUSKODER                                      
133601     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-WDGX6334                      
133701                              SSA1 SSA2 SSA3                              
133801     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
133901     PERFORM IMS-STATUSKONTROLL                                           
134001     .                                                                    
134101     EJECT                                                                
134201 IMS-GET-WDGX6334-UNIK SECTION.                                           
134301     MOVE 'IMS-GET-WDGX6334-UNIK   ' TO CURR-IMS-SECTION                  
134401                                                                          
134501     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
134601          DELIMITED BY SIZE INTO SSA1                                     
134701                                                                          
134801     STRING 'WDGX6332(IDDC     =' W-WDGXKEY-6332-X ')'                    
134901          DELIMITED BY SIZE INTO SSA2                                     
135001                                                                          
135101     STRING 'WDGX6334(IDDC     =' W-WDGXKEY-6334-X ')'                    
135201          DELIMITED BY SIZE INTO SSA3                                     
135301                                                                          
135401     MOVE '  GE' TO GODK-STATUSKODER                                      
135501     CALL CBLTDLI USING GU WDR2ALT-PCB DLI-IO-WDGX6334                    
135601                       SSA1 SSA2 SSA3                                     
135701                                                                          
135801     MOVE WDR2ALT-STATUS-CODE TO STATUS-WS                                
135901     PERFORM IMS-STATUSKONTROLL                                           
136001     .                                                                    
136101     EJECT                                                                
136201 IMS-STATUSKONTROLL SECTION.                                              
136301     SET STATUS-IX TO 1                                                   
136401     SEARCH GODK-STATUS                                                   
136501       AT END                                                             
136601         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
136701         DELIMITED BY SIZE INTO FELTEXT                                   
136801         CALL FELLOG                                                      
136901       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
137001         CONTINUE                                                         
138001     END-SEARCH                                                           
140001     .                                                                    
