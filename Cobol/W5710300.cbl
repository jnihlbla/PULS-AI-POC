001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W5710300.                                                
001400 AUTHOR.         ARCHANA BHAT.                                            
001500 DATE-WRITTEN.   11/11/04.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNCTION:                                                            
002000*        CREATES THE INVENTORY MASTER DEVIATION LIST                      
002100*                                                                         
002201*        THE PROGRAM READS     WDB6                                       
002210*        THE PROGRAM READS     WDJ7                                       
002220*        THE PROGRAM READS     WDD3                                       
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100*          --- SYSIN FROM JCL                                             
003200     SELECT INDATA                     ASSIGN TO SYSIN.                   
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003810 FD INDATA                                                                
003820     LABEL RECORD STANDARD                                                
003830     RECORDING  F                                                         
003840     BLOCK CONTAINS 0.                                                    
003850 01  INPOST                  PIC X(80).                                   
003860*                                                                         
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100 77  IDPGM                       PIC X(8)    VALUE 'W5710300'.            
004110 77  KDRC-DISPLAY                PIC Z(5).                                
004200 77  YES                         PIC X       VALUE 'J'.                   
004300 77  NOO                         PIC X       VALUE 'N'.                   
004310 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004320 77  WS-CURRENT-DATE             PIC X(8)    VALUE SPACES.                
004330 77  WS-CURRENT-TIME             PIC 9(6)    VALUE ZERO.                  
004340 77  WS-COST-DEV                 PIC S9(9)V9(2) VALUE 0.                  
004341 77  WS-COST-DEV1                PIC S9(9)V9(2) VALUE 0.                  
004342 77  WS-QTY-DEV                  PIC S9(7)V9(2) VALUE 0.                  
004343 77  WS-QTY-DEV1                 PIC S9(7)V9(2) VALUE 0.                  
004344 77  WS-NO-DEVTN                 PIC X(15)   VALUE                        
004345                       'NO DEVIATION'.                                    
004346 77  WS-LINE-CNT                 PIC 9(7)   VALUE 0.                      
004347                                                                          
004348 77  WS-IDSKYLT-CHINESE          PIC X(3)    VALUE 'RCN'.                 
004349 77  WS-IDSKYLT-ENGLISH          PIC X(3)    VALUE 'GB '.                 
004350                                                                          
004351 77  INDATA-EOF-SW               PIC X       VALUE 'N'.                   
004360     88  END-OF-INDATA                       VALUE 'Y'.                   
004400     SKIP2                                                                
004410 01  FILLER                      PIC X(10)   VALUE 'INAREA'.              
004420 01  INAREA.                                                              
004430     03 WS-RECV-IDDC             PIC X(2)    VALUE SPACE.                 
004440     03 FILLER                   PIC X(78)   VALUE SPACE.                 
004500 01  ERROR-TEXT.                                                          
004600     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
004700     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005200     EJECT                                                                
005900 01  GENERAL-SUBPROGRAMS.                                                 
006000*                                                                         
006100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006320     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006340     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
006350     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
006360     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
006601     EJECT                                                                
006602 01  MESSAGE-CODES.                                                       
006603     03  ERROR-CODES.                                                     
006604         05  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.             
006605         05  ERR-INSERT-NOT-ALLOWED  PIC X(3)    VALUE '008'.             
006606         05  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.             
006607         05  ERR-INVALID-FIELD       PIC X(3)    VALUE '023'.             
006608         05  ERR-MUST-BE-NUMERIC     PIC X(3)    VALUE '024'.             
006609         05  NOT-FOUND               PIC X(3)    VALUE '025'.             
006610         05  ERR-MUST-BE-ENTERED     PIC X(3)    VALUE '026'.             
006611         05  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.             
006612         05  ERR-ALREADY-EXIST       PIC X(3)    VALUE '030'.             
006613         05  SYSTEM-ERROR            PIC X(3)    VALUE '099'.             
006614         05  ERR-INRE-ALREADY-EXISTS PIC X(3)    VALUE '104'.             
006615         05  ERR-ACS-NOT-ALLOWED     PIC X(3)    VALUE '331'.             
006616     03  INFO-CODES.                                                      
006617         05  INF-UPDATE-OK           PIC X(3)    VALUE '001'.             
006618         05  INF-INSERT-OK           PIC X(3)    VALUE '002'.             
006619         05  INF-DELETE-OK           PIC X(3)    VALUE '003'.             
006620         05  INF-PROCESS-STARTED     PIC X(3)    VALUE '015'.             
006621         05  INF-OTHER-VERSION-EXIST PIC X(3)    VALUE '101'.             
006622                                                                          
006630*01  -COPY WL01TIDZ                                                       
006631*                                                                         
006640*    --- PARAMETRAR TILL POSTSUM                                          
006660*01  -COPY W0005   -PRE  POSTSUM-                                         
006670                                                                          
006680 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
006690*01  -COPY WTRAUTF8                                                       
006800*                                                                         
006801*    --- AREAS FOR COMMUNICATION                                          
006802 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
006803*01  -COPY WZ01SEND                                                       
006804*                                                                         
006810 01  HEADER-AREA                 PIC X(24)    VALUE 'HEADER-AREA'.        
006820 01  DP-HDR-AREA.                                                         
006830*   03  -COPY WZ01REQU                                                    
006840*   03  -COPY WZ04HDR                                                     
006850                                                                          
006860 01  HDR-AREA.                                                            
006870*   03  -COPY W5710301                                                    
006880*                                                                         
006890 01  LINE-AREA                   PIC X(24)    VALUE 'LINE-AREA'.          
006891 01  DOC-LINE-AREA.                                                       
006892*    03 -COPY W5710302                                                    
006900     EJECT                                                                
007000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007100     SKIP3                                                                
007200 01  KEYS-TILL-DLI.                                                       
007300     03  W-IDDC-B6-X.                                                     
007310         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
007320     03  W-WDGXKEY-5104-X.                                                
007330         05 W-IDHTYP             PIC X(4)    VALUE '5103'.                
007340         05 W-LOWVALUE           PIC X(26)   VALUE LOW-VALUE.             
007350     03  W-IDDC-5104-X.                                                   
007360         05  W-IDDC-5104         PIC X(2)    VALUE SPACE.                 
007391     03  W-WDJ701KY-MIN-X.                                                
007392         05  W-WDJ701-IDDC-MIN    PIC X(2)   VALUE SPACE.                 
007393         05  W-WDJ701-IDARTNR-MIN PIC S9(9)  VALUE 0 COMP-3.              
007394     03  W-WDJ701KY-MAX-X.                                                
007395         05  W-WDJ701-IDDC-MAX    PIC X(2)   VALUE SPACE.                 
007396         05  W-WDJ701-IDARTNR-MAX PIC S9(9) COMP-3                        
007397                                              VALUE 999999999.            
007398     03  W-IDARTNR-X.                                                     
007399         05  W-IDARTNR            PIC S9(9)   VALUE ZERO   COMP-3.        
007400     03  W-IDSKYLT-X.                                                     
007401         05  W-IDSKYLT            PIC X(3)    VALUE SPACE.                
007410     SKIP2                                                                
007500*    --- STATUS-KOD FRÅN IMS                                              
007600 01  STATUS-WS                   PIC XX.                                  
007700     88  SEGMENT-FOUND                       VALUE '  '.                  
007800     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
007900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008000     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
008100     88  IMS-NOT-OK                          VALUE 'XD'.                  
008200     SKIP2                                                                
008300 01  GOOD-STATUSCODES.                                                    
008400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008500     SKIP3                                                                
008600 01  SSA1                        PIC X(64).                               
008700 01  SSA2                        PIC X(64).                               
008800     EJECT                                                                
008900*    --- IMS FUNCTION CODES                                               
009000*01  -COPY W0003                                                          
009100     EJECT                                                                
009300*    ---  DLI INPUT-OUTPUT AREA                                           
009400                                                                          
009501 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5104'.                    
009502 01  DLI-IO-WDGX5104.                                                     
009503*    03  -COPY WDGX5104                                                   
009504 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ701'.                      
009505 01  DLI-IO-WDJ701.                                                       
009510*    03  -COPY WDJ701                                                     
009520 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
009530 01  DLI-IO-WDB601.                                                       
009540*    03  -COPY WDB601                                                     
009550 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
009560 01  DLI-IO-WDD311.                                                       
009570*    03  -COPY WDD311                                                     
009800     EJECT                                                                
009900 LINKAGE SECTION.                                                         
010000                                                                          
010220*01  -COPY W0009   -PRE MSG-                                              
010230*01  -COPY W0009   -PRE DAP-                                              
010240                                                                          
010250*01  -COPY W0008   -PRE WDB6-                                             
010260     05  FILLER                  PIC X.                                   
010280*01  -COPY W0008   -PRE 5104-                                             
010290     05  FILLER                  PIC X.                                   
010300                                                                          
010400*01  -COPY W0008   -PRE WDJ7-                                             
010410     05  FILLER                  PIC X.                                   
010420*01  -COPY W0008   -PRE WDD3-                                             
010430     05  FILLER                  PIC X.                                   
010500     EJECT                                                                
010601 PROCEDURE DIVISION  USING MSG-PCB DAP-PCB 5104-PCB WDJ7-PCB              
010602                                           WDB6-PCB WDD3-PCB.             
010603                                                                          
010604 MAIN SECTION.                                                            
010610     ENTRY 'DLITCBL' USING MSG-PCB DAP-PCB 5104-PCB WDJ7-PCB              
010700                                           WDB6-PCB WDD3-PCB.             
011000     PERFORM A-INIT                                                       
011100     PERFORM B-GET-DATE-TIME-FROM-WDB6                                    
011200     PERFORM C-GET-DATA-FROM-WDR2                                         
012400                                                                          
012500     MOVE ZERO TO RETURN-CODE                                             
012600     GOBACK                                                               
012700     .                                                                    
012800     EJECT                                                                
012900 A-INIT SECTION.                                                          
013100     OPEN INPUT INDATA                                                    
013200     READ INDATA NEXT RECORD INTO INAREA                                  
013300       AT END                                                             
013310          SET END-OF-INDATA            TO TRUE                            
013320     END-READ                                                             
013330     CLOSE INDATA                                                         
013400                                                                          
013500     UNSTRING INAREA DELIMITED BY SPACE INTO WS-RECV-IDDC                 
013510                                                                          
013530     MOVE WS-RECV-IDDC                 TO W-IDDC-5104                     
013540                                          W-IDDC-B6                       
013550                                          W-WDJ701-IDDC-MIN               
013551                                          W-WDJ701-IDDC-MAX               
013560     MOVE FUNCTION CURRENT-DATE (1:8)  TO WS-CURRENT-DATE                 
013570     MOVE FUNCTION CURRENT-DATE (9:6)  TO WS-CURRENT-TIME                 
013580     MOVE IDPGM                        TO POSTSUM-PROGNAMN                
013600                                                                          
014000     .                                                                    
014100     EJECT                                                                
014110*-----------------------------------------------------------------        
014120*  GET THE LOCAL DATE AND TIME USING THE TIMEZONE VALUE PRESENT IN        
014130*  WDB6 DATABASE.                                                         
014140*-----------------------------------------------------------------        
014150 B-GET-DATE-TIME-FROM-WDB6 SECTION.                                       
014160                                                                          
014170     PERFORM IMS-GU-WDB601                                                
014180     IF SEGMENT-FOUND                                                     
014190        MOVE '011'                 TO MSGI-KDCALL                         
014191        MOVE DCS-IDTIDZON          TO MSGI-IDTIDZON                       
014191        MOVE DCS-IDDC              TO MSGI-IDDC                           
014192        MOVE WS-CURRENT-DATE(3:6)  TO MSGI-TILOKDAT                       
014193        MOVE WS-CURRENT-TIME(1:4)  TO MSGI-TILOKTID                       
014194        CALL WL01TIDZ   USING      MSGI-WL01TIDZ                          
014195                                                                          
014196        STRING WS-CURRENT-DATE(1:2) MSGI-TILOKDAT                         
014197               DELIMITED BY SIZE  INTO HEAD-TIDATETIME(1:8)               
014198                                                                          
014202        STRING MSGI-TILOKTID WS-CURRENT-TIME(5:2)                         
014203               DELIMITED BY SIZE  INTO HEAD-TIDATETIME(9:6)               
014206     END-IF                                                               
014207     .                                                                    
014208 C-GET-DATA-FROM-WDR2 SECTION.                                            
014209     PERFORM IMS-GU-WDGX5104                                              
014211     IF SEGMENT-FOUND                                                     
014220        IF 5104-KDACS = 'M'                                               
014240           PERFORM CA-CREATE-MASTER-DEV-LIST                              
014300        END-IF                                                            
014600     END-IF                                                               
014700     .                                                                    
014800 CA-CREATE-MASTER-DEV-LIST SECTION.                                       
014801                                                                          
014802     PERFORM CAA-CREATE-HDR                                               
014810     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                      
014820       MOVE 0 TO WS-COST-DEV                                              
014821       MOVE 0 TO WS-COST-DEV1                                             
014830                 WS-QTY-DEV                                               
014840                 WS-QTY-DEV1                                              
014900       PERFORM IMS-GN-WDJ701                                              
015000       IF SEGMENT-FOUND                                                   
015001         IF ACS-IDACSNR-T > 0                                             
015010            COMPUTE WS-COST-DEV = (ACS-KVTCOUNT * ACS-PRAVCOST)           
015020                                 -(ACS-KVLS * ACS-PRAVCOST)               
015021            COMPUTE WS-COST-DEV1 = ((ACS-KVTCOUNT * ACS-PRAVCOST)         
015022                                  -(ACS-KVLS * ACS-PRAVCOST)) * -1        
015023            IF ACS-KVLS = ACS-KVTCOUNT                                    
015024               MOVE 0 TO WS-QTY-DEV                                       
015025            ELSE                                                          
015026             IF ACS-KVLS > ACS-KVTCOUNT                                   
015030               IF ACS-KVTCOUNT NOT = 0                                    
015040                  COMPUTE WS-QTY-DEV ROUNDED                              
015041                             = 1 - (ACS-KVLS / ACS-KVTCOUNT)              
015042               ELSE                                                       
015043                  MOVE 0 TO WS-QTY-DEV                                    
015050               END-IF                                                     
015051             ELSE                                                         
015060               IF ACS-KVLS NOT = 0                                        
015070                  COMPUTE WS-QTY-DEV ROUNDED                              
015071                             = 1 - (ACS-KVTCOUNT / ACS-KVLS)              
015080               ELSE                                                       
015090                  MOVE 0 TO WS-QTY-DEV                                    
015091               END-IF                                                     
015092             END-IF                                                       
015093            END-IF                                                        
015100            IF WS-QTY-DEV  > 5104-REQTYDEV-2                              
015210            OR WS-COST-DEV > 5104-PRAVCOST-DEV2                           
015211            OR (WS-COST-DEV < 0                                           
015213            AND WS-COST-DEV1 > 5104-PRAVCOST-DEV2 )                       
015214               PERFORM CAB-CREATE-LINE                                    
015215            END-IF                                                        
015232         END-IF                                                           
015240       END-IF                                                             
015250     END-PERFORM                                                          
015251     IF WS-LINE-CNT = 0                                                   
015252       MOVE '2         '        TO LINE-IDAFPRCD                          
015253       MOVE 0                   TO LINE-IDARTNR                           
015255                                   LINE-ADLAGOMR                          
015256                                   LINE-ADGANG                            
015257                                   LINE-ADPLATS                           
015258                                   LINE-KVLS                              
015259                                   LINE-KVCOUNT                           
015260                                   LINE-SUAVCOST-DEV                      
015261                                   LINE-IDACSNR                           
015262       MOVE SPACES              TO LINE-IDCOUNTER-REG                     
015264       MOVE WS-NO-DEVTN         TO LINE-BEART                             
015265*    WE NEED SOME ADJUSTMENT TO UTF8                                      
015266*    BEFORE DISPLAY OF WS-NO-DEVTN                                        
015267                                                                          
015280       MOVE WS-NO-DEVTN         TO TRAUTF8-TECONV-FROM                    
015281       MOVE '278 '              TO TRAUTF8-KDCP                           
015282       MOVE 25                  TO TRAUTF8-KVMAXTL                        
015283       CALL WTRAUTF8 USING TRAUTF8-AREA                                   
015284       MOVE TRAUTF8-TECONV-TO   TO LINE-BEART                             
015285                                                                          
015286       PERFORM S03-PUT-REPORT-LINE                                        
015287     END-IF                                                               
015288                                                                          
015289     PERFORM S04-SEND-CLOSE                                               
015290     .                                                                    
015291 CAA-CREATE-HDR SECTION.                                                  
015292     MOVE 1                          TO REQU-IDMSGVER                     
015293     MOVE 'E'                        TO REQU-KDPGMACT                     
015294     MOVE IDPGM                      TO REQU-IDUSER                       
015295                                                                          
015296     MOVE SPACE                      TO HDR-IDOUTREC                      
015297     MOVE WS-RECV-IDDC               TO HDR-IDOUTREC                      
015298                                        HEAD-IDDC                         
015299     MOVE 'W57103-001'               TO HDR-IDOUTTYPE                     
015300     MOVE WS-CURRENT-DATE            TO HDR-IDLIST                        
015301     MOVE '1         '               TO HEAD-IDAFPRCD                     
015302     PERFORM S01-SEND-OPEN                                                
015303     MOVE SEND-IDCOM                 TO WZ04-SEND-IDCOM                   
015304     PERFORM S02-PUT-DP-HEADER                                            
015305     PERFORM S02-PUT-HEADER                                               
015306     .                                                                    
015307 CAB-CREATE-LINE SECTION.                                                 
015308     MOVE '2         '        TO LINE-IDAFPRCD                            
015309     MOVE ACS-IDARTNR         TO LINE-IDARTNR                             
015310     MOVE ACS-BEART           TO LINE-BEART                               
015311                                                                          
015312*    WE NEED SOME ADJUSTMENT FOR CHINESE NAME                             
015313*    BEFORE DISPLAY OF BEART                                              
015314                                                                          
015315     MOVE ACS-IDARTNR           TO W-IDARTNR                              
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
015323     PERFORM IMS-GU-WDD311                                                
015324     IF SEGMENT-FOUND                                                     
015325        MOVE TEXT-BEART         TO TRAUTF8-TECONV-FROM                    
015326     ELSE                                                                 
015327        MOVE SPACE              TO TRAUTF8-TECONV-FROM                    
015328        MOVE '278 '             TO TRAUTF8-KDCP                           
015329     END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GU-WDD311                                               
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
015330     MOVE 25                    TO TRAUTF8-KVMAXTL                        
015331     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
015332     MOVE TRAUTF8-TECONV-TO     TO LINE-BEART                             
015333                                                                          
015334     MOVE ACS-ADLAGOMR        TO LINE-ADLAGOMR                            
015335     MOVE ACS-ADGANG          TO LINE-ADGANG                              
015336     MOVE ACS-ADPLATS         TO LINE-ADPLATS                             
015337     MOVE ACS-KVLS            TO LINE-KVLS                                
015338     MOVE ACS-KVTCOUNT        TO LINE-KVCOUNT                             
015339     MOVE WS-COST-DEV         TO LINE-SUAVCOST-DEV                        
015340     MOVE ACS-IDCOUNTER-T-REG TO LINE-IDCOUNTER-REG                       
015341     MOVE ACS-IDACSNR-T       TO LINE-IDACSNR                             
015342                                                                          
015343     PERFORM S03-PUT-REPORT-LINE                                          
015344     ADD +1                   TO WS-LINE-CNT                              
015345     .                                                                    
015346*    --- DISPATCHER-SECTIONS                                              
015347 S01-SEND-OPEN SECTION.                                                   
015348     MOVE 'CARPARTS.DAP.DISTRDOC'         TO SEND-ADDISPABS               
015349     MOVE 'OPEN'                          TO SEND-KDFUNC                  
015350     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015351                         SEND-OPEN-AREA                                   
015352     IF SEND-KDRC > ZERO                                                  
015353       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
015354       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
015355       DELIMITED BY SIZE INTO ERROR-TEXT                                  
015356       DISPLAY ERROR-TEXT                                                 
015357       CALL FELLOG                                                        
015358     END-IF                                                               
015359     .                                                                    
015360     SKIP3                                                                
015361 S02-PUT-DP-HEADER SECTION.                                               
015362                                                                          
015363     MOVE 'PUT'                           TO SEND-KDFUNC                  
015364     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
015365     MOVE LENGTH OF DP-HDR-AREA           TO SEND-KVDLEN                  
015366     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015367                         SEND-KVDLEN                                      
015368                         DP-HDR-AREA                                      
015369     IF SEND-KDRC > ZERO                                                  
015370       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
015371       STRING 'WZ01SEND PUT-DP-HDR  ERROR RC=' KDRC-DISPLAY               
015372       DELIMITED BY SIZE INTO ERROR-TEXT                                  
015373       DISPLAY ERROR-TEXT                                                 
015374       CALL FELLOG                                                        
015375     END-IF                                                               
015376     .                                                                    
015377 S02-PUT-HEADER SECTION.                                                  
015378                                                                          
015379     MOVE 'PUT'                           TO SEND-KDFUNC                  
015380     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
015381     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
015382     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015383                         SEND-KVDLEN                                      
015384                         HDR-AREA                                         
015385     IF SEND-KDRC > ZERO                                                  
015386       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
015387       STRING 'WZ01SEND PUT-HDR ERROR RC=' KDRC-DISPLAY                   
015388       DELIMITED BY SIZE INTO ERROR-TEXT                                  
015389       DISPLAY ERROR-TEXT                                                 
015390       CALL FELLOG                                                        
015391     END-IF                                                               
015392     .                                                                    
015393     EJECT                                                                
015394 S03-PUT-REPORT-LINE    SECTION.                                          
015395                                                                          
015396     MOVE 'PUT'                           TO SEND-KDFUNC                  
015397     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
015398     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
015399     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015400                         SEND-KVDLEN                                      
015401                         DOC-LINE-AREA                                    
015402     IF SEND-KDRC > ZERO                                                  
015403       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
015404       STRING 'WZ01SEND PUT-LINE ERROR RC=' KDRC-DISPLAY                  
015405       DELIMITED BY SIZE INTO ERROR-TEXT                                  
015406       DISPLAY ERROR-TEXT                                                 
015407       CALL FELLOG                                                        
015408     END-IF                                                               
015409        MOVE 'W57103' TO POSTSUM-FDNAMN                                   
015410        MOVE 'DAP1' TO POSTSUM-DDNAMN2                                    
015411        MOVE 'LINE1'     TO POSTSUM-TRANSTYP                              
015412        CALL POSTSUM USING POSTSUM-PARM                                   
015413     .                                                                    
015414 S04-SEND-CLOSE SECTION.                                                  
015415                                                                          
015416     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
015417     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015418                                                                          
015419                                                                          
015420     IF SEND-KDRC > ZERO                                                  
015421       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
015422       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
015423       DELIMITED BY SIZE INTO ERROR-TEXT                                  
015424       DISPLAY ERROR-TEXT                                                 
015425       CALL FELLOG                                                        
015426     END-IF                                                               
015427     .                                                                    
015430* --- IMS SECTIONS  ---                                                   
015500                                                                          
015501     EJECT                                                                
015502 IMS-GU-WDB601 SECTION.                                                   
015503                                                                          
015504     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
015505          DELIMITED BY SIZE INTO SSA1                                     
015506     MOVE '  GE' TO GOOD-STATUSCODES                                      
015507     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
015508     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
015509     PERFORM IMS-STATUSCHECK                                              
015510     .                                                                    
015511     EJECT                                                                
015512 IMS-GU-WDGX5104 SECTION.                                                 
015513                                                                          
015514     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-5104-X ')'                    
015515          DELIMITED BY SIZE INTO SSA1                                     
015516     STRING 'WDGX5104(IDDC     =' W-IDDC-5104-X ')'                       
015517          DELIMITED BY SIZE INTO SSA2                                     
015518     MOVE '  GE' TO GOOD-STATUSCODES                                      
015519     CALL CBLTDLI USING GU 5104-PCB DLI-IO-WDGX5104 SSA1 SSA2             
015520     MOVE 5104-STATUS-CODE TO STATUS-WS                                   
015521     PERFORM IMS-STATUSCHECK                                              
015522     .                                                                    
015523 IMS-GN-WDJ701 SECTION.                                                   
015524                                                                          
015527     STRING 'WDJ701  (WDJ701KY>=' W-WDJ701KY-MIN-X                        
015528                    '&WDJ701KY<=' W-WDJ701KY-MAX-X  ')'                   
015529          DELIMITED BY SIZE INTO SSA1                                     
015530     MOVE '  GE' TO GOOD-STATUSCODES                                      
015531     CALL CBLTDLI USING GN WDJ7-PCB DLI-IO-WDJ701 SSA1                    
015532     MOVE WDJ7-STATUS-CODE TO STATUS-WS                                   
015533     PERFORM IMS-STATUSCHECK                                              
015540     .                                                                    
015600     EJECT                                                                
015610 IMS-GU-WDD311 SECTION.                                                   
015620                                                                          
015630     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
015640             DELIMITED BY SIZE INTO SSA1                                  
015650     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
015660             DELIMITED BY SIZE INTO SSA2                                  
015670     MOVE '  GE'                 TO GOOD-STATUSCODES                      
015680     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
015690     MOVE WDD3-STATUS-CODE       TO STATUS-WS                             
015691     PERFORM IMS-STATUSCHECK                                              
015692     .                                                                    
015693     EJECT                                                                
015700 IMS-STATUSCHECK SECTION.                                                 
015800     SKIP2                                                                
015900     SET STATUS-IX TO 1                                                   
016000     SEARCH GOOD-STATUS                                                   
016100       AT END                                                             
016200         STRING ' INVALID STATUS CODE FROM IMS:' STATUS-WS                
016300           DELIMITED BY SIZE INTO ERROR-TEXT                              
016400         DISPLAY ERROR-TEXT                                               
016500         CALL FELLOG                                                      
016600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
016700         CONTINUE                                                         
016800     END-SEARCH                                                           
016900     .                                                                    
