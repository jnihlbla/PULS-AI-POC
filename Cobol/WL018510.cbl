000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL018510.                                                
000300 AUTHOR.         SUBBARAO PARUCHURI V.                                    
000400 DATE-WRITTEN.   04/11/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        SHOWS SUBTOTALS PER SENDING WITH FOLLOWING INPUT KEYS:           
001000*        IDSHIPM AND IDDC                                                 
001100*        ---------------------------------------------------------        
001200*        PROGRAM SHOWS BOTH PACKAGE INFORMATION BEFORE SHIPPING           
001300*        AND AFTER SHIPPING DEPENDING ON THE INPUT KEYS AS BELOW          
001400*        1. IDSHIPM  GIVEN - INFORMATION AFTER SHIPPING                   
001500*        2. IDTRPTNR GIVEN - INFORMATION BEFORE SHIPPING                  
001600*        BOTH IDSHIPM AND IDTRPTNR TOGETHER ARE NOT ALLOWED               
001700*        INFORMATION SHOWN ON SCREEN -                                    
001800*        SUMMERY FIELDS: TOT NO CASE,TOT WEIGHT,TOT VOLUME,TOT            
001900*        VALUE,TOT NETWEIGHT                                              
002000*        LINE FIELDS:IDKUNDNR,IDORDER,CASE,CUSTOMS-ID,LENGTH,             
002100*        HIGHT,WEIGHT,VOLUME,VALUE,NET WEIGHT AND DANGEROUS GOODS         
002200*                                                                         
002300*          THE PROGRAM READS     WDE1                                     
002400*          THE PROGRAM READS     WDE6                                     
002500*          THE PROGRAM READS     WDB2                                     
002600*          THE PROGRAM READS     WDB1                                     
002700*          THE PROGRAM READS     WDG2                                     
002800*          THE PROGRAM READS     WDE4                                     
002900*                                                                         
003000*        WL018510 PROGRAM IS A REPLICA OF W4065400 PROGRAM                
003100*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
003200*                                                                         
003300*    INDATA.                                                              
003400*        REQUEST:     WL0185I1                                            
003500*                                                                         
003600*    OUTDATA.                                                             
003700*        RESPONSE:    WL0185O1                                            
003800                                                                          
003900                                                                          
004000 ENVIRONMENT DIVISION.                                                    
004100                                                                          
004200 DATA DIVISION.                                                           
004300                                                                          
004400 WORKING-STORAGE SECTION.                                                 
004500 77  IDPGM                       PIC X(08)   VALUE 'WL018510'.            
004600                                                                          
004700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004900 77  KDRC-DISPLAY                PIC Z(5).                                
005000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005100                                                                          
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400                                                                          
005500 77  MOD-INDX-KLI                PIC S9(4)   VALUE +0   COMP SYNC.        
005600 77  MOD-INDX                    PIC S9(4)   VALUE +0   COMP SYNC.        
005700 77  MAX-MOD-INDX                PIC S9(4)   VALUE +500 COMP SYNC.        
005800 77  MSG-IX                      PIC S9(9)   VALUE +0  COMP SYNC.         
005900                                                                          
006000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006100                                                                          
006200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006300     88  NYCKLAR-OK                          VALUE 'J'.                   
006400     88  NYCKLAR-FEL                         VALUE 'N'.                   
006500                                                                          
006600 01  ALL-LOW-VALUES.                                                      
006700     03  FILLER                  PIC X(80)  VALUE LOW-VALUES.             
006800                                                                          
006900 77  WS-DARFSDAT                 PIC 9(8).                                
007000 77  WS-JFR-IDPRC                PIC X(4).                                
007100                                                                          
007200 01  WS-IDPRC-FR                 PIC X(4).                                
007300 01  FILLER REDEFINES WS-IDPRC-FR.                                        
007400     03  WS-IDPRCBAS-FR          PIC 9(3).                                
007500     03  WS-IDPRCVAR-FR          PIC X.                                   
007600                                                                          
007700 01  WS-IDPRC-TO                 PIC X(4).                                
007800 01  FILLER REDEFINES WS-IDPRC-TO.                                        
007900     03  WS-IDPRCBAS-TO          PIC 9(3).                                
008000     03  WS-IDPRCVAR-TO          PIC X.                                   
008100                                                                          
008200 77  WS-REPROCENT                PIC 9(3)V9(2).                           
008300 77  WS-IDELMT-ERROR             PIC X(16).                               
008400 77  WS-IDMSG-ERROR              PIC X(03).                               
008500 77  WS-IDMSG-INFO               PIC X(03).                               
008600                                                                          
008700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008800 01  FILLER REDEFINES DAGENS-DATUM.                                       
008900     03  DAGENS-AA               PIC 9(2).                                
009000     03  DAGENS-MM               PIC 9(2).                                
009100     03  DAGENS-DD               PIC 9(2).                                
009200                                                                          
009300 01  WS-IDDC-LOCAL.                                                       
009400     03  FILLER                  PIC X(5)   VALUE 'WIDDC'.                
009500     03  WS-IDDC-LOCAL-DATE      PIC X(2).                                
009600     03  FILLER                  PIC X(1)   VALUE SPACE.                  
009700                                                                          
009800 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
009900                                                                          
010000*    --- AREA FÖR WORK-TABELL TILL MOD                                    
010100 01  FILLER                      PIC X(16)   VALUE 'TABELL  '.            
010200 01  WS-MOD-TABELL.                                                       
010300     03  MOD-TABELL OCCURS 500.                                           
010400         05  TAB-IDPRC               PIC X(4).                            
010500         05  TAB-KVRADER             PIC 9(5) COMP-3.                     
010600         05  TAB-KVRADER-UTSKR       PIC 9(5) COMP-3.                     
010700         05  TAB-REPROCENT-UTSKR     PIC 9(3) COMP-3.                     
010800         05  TAB-KVRADER-PACK        PIC 9(5) COMP-3.                     
010900         05  TAB-REPROCENT-PACK      PIC 9(3) COMP-3.                     
011000 01  FILLER                      PIC X(16)   VALUE 'TOTAL   '.            
011100 01  WS-TOTALER.                                                          
011200     03  TOT-KVRADER             PIC 9(5) COMP-3 VALUE ZERO.              
011300     03  TOT-KVRADER-UTSKR       PIC 9(5) COMP-3 VALUE ZERO.              
011400     03  TOT-REPROCENT-UTSKR     PIC 9(3) COMP-3 VALUE ZERO.              
011500     03  TOT-KVRADER-PACK        PIC 9(5) COMP-3 VALUE ZERO.              
011600     03  TOT-REPROCENT-PACK      PIC 9(3) COMP-3 VALUE ZERO.              
011700     EJECT                                                                
011800*    --- WORK TABLE FOR RESP-TABELLKLI.                                   
011900 01  FILLER                      PIC X(16)   VALUE 'TABELL 2'.            
012000 01  WS-TABLLKLI.                                                         
012100     03  TABLLKLI OCCURS 500.                                             
012200         05  TAB-IDPRC-KLI           PIC X(4).                            
012300         05  TAB-IDDISTR             PIC 9(4).                            
012400         05  TAB-IDPRODNR            PIC 9(7).                            
012500         05  TAB-IDKOLLI             PIC 9(5).                            
012600     EJECT                                                                
012700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
012800 01  GENERAL-SUBPROGRAMS.                                                 
012900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.             
013300     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
013400     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
013500     SKIP3                                                                
013600 01  MESSAGE-CODES.                                                       
013700     03  ERR-INVALID-KEY         PIC X(3)    VALUE '022'.                 
013800     03  ERR-RECORD-MISSING      PIC X(3)    VALUE '027'.                 
013900     03  TOO-MANY-LINES          PIC X(3)    VALUE '028'.                 
014000     03  WRONG-IDDC              PIC X(08)   VALUE 'IDDC'.                
014100     EJECT                                                                
014200*01  -COPY WDATAREA                                                       
014300     EJECT                                                                
014400*                                                                         
014500*    --- PARAMETERS TO ABEND                                              
014600                                                                          
014700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
014800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
014900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
015000     EJECT                                                                
015100*                                                                         
015200*01  -COPY WL01TIDZ                                                       
015300                                                                          
015400 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
015500*01  -COPY WMSGCONV                                                       
015600                                                                          
015700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015800*                                                                         
015900     EJECT                                                                
016000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016100     SKIP3                                                                
016200 01  NYCKLAR-TILL-DLI.                                                    
016300     03  W-WDQ3F1KY-MIN-X.                                                
016400         05  W-IDDC-MIN          PIC X(2).                                
016500         05  W-DARFSDAT-MIN      PIC 9(8).                                
016600         05  W-IDPRC-MIN         PIC X(4).                                
016700         05  W-IDORDER-MIN       PIC S9(7)  COMP-3 VALUE ZERO.            
016800         05  W-IDPRODNR-MIN      PIC S9(7)  COMP-3 VALUE ZERO.            
016900         05  W-IDPLKLST-MIN      PIC S9(3)  COMP-3 VALUE ZERO.            
017000     03  W-WDQ3F1KY-MAX-X.                                                
017100         05  W-IDDC-MAX          PIC X(2).                                
017200         05  W-DARFSDAT-MAX      PIC 9(8).                                
017300         05  W-IDPRC-MAX         PIC X(4).                                
017400         05  W-IDORDER-MAX       PIC S9(7)  COMP-3 VALUE +9999999.        
017500         05  W-IDPRODNR-MAX      PIC S9(7)  COMP-3 VALUE +9999999.        
017600         05  W-IDPLKLST-MAX      PIC S9(3)  COMP-3 VALUE +999.            
017700     03  W-IDDC-B6-X.                                                     
017800         05 W-IDDC-B6            PIC X(2).                                
017900     03  W-IDPRODNR-X.                                                    
018000         05 W-IDPRODNR           PIC S9(7)  COMP-3 VALUE ZERO.            
018100     03  W-KDKOLSTA-0-X.                                                  
018200         05 W-KDKOLSTA           PIC S9     COMP-3 VALUE ZERO.            
018300     SKIP2                                                                
018400*    --- STATUS-KOD FRÅN IMS                                              
018500 01  STATUS-WS                   PIC XX.                                  
018600     88  SEGMENT-FINNS                       VALUE '  '.                  
018700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018800     88  BASEN-SLUT                          VALUE 'GB'.                  
018900     SKIP2                                                                
019000 01  GODK-STATUSKODER.                                                    
019100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019200     SKIP3                                                                
019300 01  SSA1                        PIC X(128).                              
019400 01  SSA2                        PIC X(64).                               
019500     EJECT                                                                
019600*    --- IMS FUNKTIONSKODER                                               
019700*01  -COPY W0003                                                          
019800     EJECT                                                                
019900*    ---  DLI INPUT-OUTPUT AREA                                           
020000                                                                          
020100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ3F1'.                      
020200 01  DLI-IO-WDQ3F1.                                                       
020300*    03  -COPY WDQ3F1                                                     
020400     EJECT                                                                
020500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
020600 01   DLI-IO-AREA-B601.                                                   
020700*     03  -COPY WDB601                                                    
020800     EJECT                                                                
020900 01  FILLER               PIC X(16)   VALUE 'WDE601 AREA'.                
021000 01   DLI-IO-AREA-E601.                                                   
021100*     03  -COPY WDE601                                                    
021200     EJECT                                                                
021300 01  FILLER               PIC X(16)   VALUE 'WDE611 AREA'.                
021400 01   DLI-IO-AREA-E611.                                                   
021500*     03  -COPY WDE611                                                    
021600     EJECT                                                                
021700 LINKAGE SECTION.                                                         
021800 01  REQU-AREA.                                                           
021900*    03  -COPY WZ01REQ2                                                   
022000*    03  -COPY WL0185I1                                                   
022100                                                                          
022200 01  RESP-AREA.                                                           
022300*    03  -COPY WZ01RES2                                                   
022400*    03  -COPY WL0185O1                                                   
022500                                                                          
022600*01  -COPY W0008  -PRE WDQ3F-                                             
022700     05  FILLER                  PIC X.                                   
022800     EJECT                                                                
022900*01  -COPY W0008  -PRE WDB6-                                              
023000     05  FILLER                  PIC X.                                   
023100     EJECT                                                                
023200*01  -COPY W0008  -PRE WDE6-                                              
023300     05  FILLER                  PIC X.                                   
023400     EJECT                                                                
023500 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA                            
023600                           WDQ3F-PCB WDB6-PCB WDE6-PCB.                   
023700                                                                          
023800 MAIN SECTION.                                                            
023900     ENTRY 'DLITCBL' USING REQU-AREA RESP-AREA                            
024000                           WDQ3F-PCB WDB6-PCB WDE6-PCB.                   
024100                                                                          
024200                                                                          
024300     IF REQU-KDPGMACT = 'S'                                               
024400       PERFORM A-INIT                                                     
024500       PERFORM B-KOLLA-NYCKLAR                                            
024600       IF NYCKLAR-OK                                                      
024700         PERFORM F-LAES-VISA-INFO                                         
024800       END-IF                                                             
024900                                                                          
025000       MOVE RESP-IDMSG-INFO      TO WS-IDMSG-INFO                         
025100       MOVE RESP-IDMSG-ERROR     TO WS-IDMSG-ERROR                        
025200       MOVE RESP-IDELMT-ERROR    TO WS-IDELMT-ERROR                       
025300       IF WS-IDMSG-ERROR NOT = SPACE                                      
025400         MOVE ALL-LOW-VALUES     TO RESP-IDDC-KEY                         
025500                                    RESP-TIRFSDAT-KEY                     
025600                                    RESP-IDPRC-FR-KEY                     
025700                                    RESP-IDPRC-TO-KEY                     
025800         MOVE WS-IDMSG-ERROR     TO RESP-IDMSG-ERROR                      
025900         MOVE WS-IDELMT-ERROR    TO RESP-IDELMT-ERROR                     
026000         MOVE WS-IDMSG-INFO      TO RESP-IDMSG-INFO                       
026100         MOVE 002                TO RESP-IDRESVER                         
026200         MOVE ZERO               TO RESP-KVRADER-MAX1                     
026300                                    RESP-KVRADER-MAX2                     
026400       END-IF                                                             
026500                                                                          
026600     ELSE                                                                 
026700       MOVE '099'                TO RESP-IDMSG-ERROR                      
026800     END-IF                                                               
026900                                                                          
027000     PERFORM S01-MSG-CONV                                                 
027100                                                                          
027200     MOVE ZERO                   TO RETURN-CODE                           
027300     GOBACK                                                               
027400     .                                                                    
027500     EJECT                                                                
027600 A-INIT SECTION.                                                          
027700                                                                          
027800     MOVE LOW-VALUES             TO RESP-AREA                             
027900     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
028000                                    RESP-IDMSG-INFO                       
028100                                    RESP-IDELMT-ERROR                     
028200     MOVE 002                    TO RESP-IDRESVER                         
028300     MOVE ZERO                   TO RESP-KVRADER-MAX1                     
028400                                    RESP-KVRADER-MAX2                     
028500                                                                          
028600     ACCEPT DAGENS-DATUM       FROM DATE                                  
028700     ACCEPT DAGENS-TID         FROM TIME                                  
028800     .                                                                    
028900                                                                          
029000 B-KOLLA-NYCKLAR SECTION.                                                 
029100                                                                          
029200     MOVE JA                     TO NYCKLAR-SW                            
029300                                                                          
029400     MOVE SPACE                  TO W-IDDC-MIN                            
029500                                    W-IDPRC-MIN                           
029600     MOVE ZERO                   TO W-DARFSDAT-MIN                        
029700                                                                          
029800     MOVE REQU-IDDC-KEY          TO W-IDDC-MIN                            
029900                                    W-IDDC-MAX                            
030000                                    RESP-IDDC-KEY                         
030100                                    W-IDDC-B6                             
030200     PERFORM IMS-GU-WDB601                                                
030300     IF SEGMENT-SAKNAS                                                    
030400       MOVE NEJ                  TO NYCKLAR-SW                            
030500       MOVE WRONG-IDDC           TO RESP-IDELMT-ERROR                     
030600     END-IF                                                               
030700                                                                          
030800     MOVE '011'                  TO MSGI-KDCALL                           
030900     MOVE DCS-IDTIDZON           TO MSGI-IDTIDZON                         
030900     MOVE DCS-IDDC               TO MSGI-IDDC                             
031000     MOVE DAGENS-DATUM           TO MSGI-TILOKDAT                         
031100     MOVE DAGENS-TID             TO MSGI-TILOKTID                         
031200                                                                          
031300     CALL WL01TIDZ            USING MSGI-WL01TIDZ                         
031400     MOVE MSGI-TILOKDAT(1:6)     TO DAGENS-DATUM                          
031500     MOVE MSGI-TILOKTID(1:4)     TO DAGENS-TID(1:4)                       
031600********                                                                  
031700     PERFORM BB-KOLLA-TIRFSDAT                                            
031800     PERFORM BC-KOLLA-IDPRC-FR                                            
031900     PERFORM BD-KOLLA-IDPRC-TO                                            
032000     PERFORM BE-KOLLA-SAMBAND-IDPRC                                       
032100                                                                          
032200     MOVE WS-DARFSDAT (3:6)      TO RESP-TIRFSDAT-KEY                     
032300     MOVE WS-IDPRC-FR            TO RESP-IDPRC-FR-KEY                     
032400     MOVE WS-IDPRC-TO            TO RESP-IDPRC-TO-KEY                     
032500                                                                          
032600     IF NYCKLAR-FEL                                                       
032700       MOVE ERR-INVALID-KEY      TO RESP-IDMSG-ERROR                      
032800     END-IF                                                               
032900     .                                                                    
033000     EJECT                                                                
033100 BB-KOLLA-TIRFSDAT SECTION.                                               
033200                                                                          
033300     IF REQU-TIRFSDAT-KEY = LOW-VALUES OR                                 
033400        REQU-TIRFSDAT-KEY NUMERIC                                         
033500       CONTINUE                                                           
033600     ELSE                                                                 
033700       MOVE NEJ                  TO NYCKLAR-SW                            
033800       MOVE 'TIRFSDAT'           TO RESP-IDELMT-ERROR                     
033900     END-IF                                                               
034000                                                                          
034100     IF REQU-TIRFSDAT-KEY = LOW-VALUES                                    
034200       MOVE DAGENS-DATUM         TO WS-DARFSDAT                           
034300     ELSE                                                                 
034400       MOVE REQU-TIRFSDAT-KEY    TO WS-DARFSDAT                           
034500     END-IF                                                               
034600                                                                          
034700     IF REQU-TIRFSDAT-KEY = LOW-VALUES                                    
034800       MOVE DAGENS-DATUM         TO WS-DARFSDAT                           
034900     ELSE                                                                 
035000       MOVE REQU-TIRFSDAT-KEY    TO WS-DARFSDAT                           
035100     END-IF                                                               
035200                                                                          
035300     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
035400     MOVE WS-DARFSDAT (3:6)      TO DAT-I-TIDATUM                         
035500                                                                          
035600     CALL WDATKONV            USING DAT-KDDATFORM DAT-I-TIDATUM           
035700                                    DAT-O-TIDATUM DAT-KDSVAR              
035800                                                                          
035900     IF DAT-KDSVAR-OK                                                     
036000       MOVE DAT-TISEKEL          TO WS-DARFSDAT (1:2)                     
036100       MOVE WS-DARFSDAT          TO W-DARFSDAT-MIN                        
036200                                    W-DARFSDAT-MAX                        
036300     ELSE                                                                 
036400       MOVE NEJ                  TO NYCKLAR-SW                            
036500       MOVE 'TIRFSDAT'           TO RESP-IDELMT-ERROR                     
036600     END-IF                                                               
036700     .                                                                    
036800     EJECT                                                                
036900 BC-KOLLA-IDPRC-FR SECTION.                                               
037000                                                                          
037100     IF REQU-IDPRC-FR-IN-KEY = LOW-VALUES OR SPACES                       
037200       MOVE '000 '               TO WS-IDPRC-FR                           
037300     ELSE                                                                 
037400       IF FUNCTION TRIM(REQU-IDPRCBAS OF REQU-IDPRC-FR-IN-KEY)            
037500            IS NUMERIC                                                    
037600         MOVE FUNCTION TRIM(REQU-IDPRCBAS OF REQU-IDPRC-FR-IN-KEY)        
037700                                 TO WS-IDPRCBAS-FR                        
037800         MOVE REQU-IDPRCVAR OF REQU-IDPRC-FR-IN-KEY                       
037900                                 TO WS-IDPRCVAR-FR                        
038000       ELSE                                                               
038100         MOVE NEJ                TO NYCKLAR-SW                            
038200         MOVE 'IDPRC'            TO RESP-IDELMT-ERROR                     
038300       END-IF                                                             
038400     END-IF                                                               
038500                                                                          
038600     MOVE WS-IDPRC-FR            TO W-IDPRC-MIN                           
038700     .                                                                    
038800                                                                          
038900 BD-KOLLA-IDPRC-TO SECTION.                                               
039000                                                                          
039100     IF REQU-IDPRC-TO-IN-KEY = LOW-VALUES OR SPACES                       
039200       MOVE '9999'               TO WS-IDPRC-TO                           
039300     ELSE                                                                 
039400       IF FUNCTION TRIM(REQU-IDPRCBAS OF REQU-IDPRC-TO-IN-KEY)            
039500            IS NUMERIC                                                    
039600         MOVE FUNCTION TRIM(REQU-IDPRCBAS OF REQU-IDPRC-TO-IN-KEY)        
039700                                 TO WS-IDPRCBAS-TO                        
039800         MOVE REQU-IDPRCVAR OF REQU-IDPRC-TO-IN-KEY                       
039900                                 TO WS-IDPRCVAR-TO                        
040000       ELSE                                                               
040100         MOVE NEJ                TO NYCKLAR-SW                            
040200         MOVE 'IDPRC'            TO RESP-IDELMT-ERROR                     
040300       END-IF                                                             
040400     END-IF                                                               
040500                                                                          
040600     MOVE WS-IDPRC-TO            TO W-IDPRC-MAX                           
040700     .                                                                    
040800                                                                          
040900 BE-KOLLA-SAMBAND-IDPRC SECTION.                                          
041000                                                                          
041100     IF W-IDPRC-MIN > W-IDPRC-MAX AND                                     
041200        W-IDPRC-MAX NOT = '0000'                                          
041300       MOVE NEJ                  TO NYCKLAR-SW                            
041400       MOVE 'IDPRC'              TO RESP-IDELMT-ERROR                     
041500     ELSE                                                                 
041600       IF W-IDPRC-MAX = '0000'                                            
041700         MOVE WS-IDPRC-FR        TO WS-IDPRC-TO                           
041800         MOVE W-IDPRC-MIN        TO W-IDPRC-MAX                           
041900       END-IF                                                             
042000     END-IF                                                               
042100     .                                                                    
042200                                                                          
042300 F-LAES-VISA-INFO SECTION.                                                
042400                                                                          
042500     MOVE +0                     TO MOD-INDX                              
042600     PERFORM IMS-GN-WDQ3F1                                                
042700     IF SEGMENT-FINNS                                                     
042800       MOVE +1                   TO MOD-INDX                              
042900                                    MOD-INDX-KLI                          
043000       MOVE +0                   TO TAB-KVRADER       (MOD-INDX)          
043100                                    TAB-KVRADER-UTSKR (MOD-INDX)          
043200                                    TAB-KVRADER-PACK  (MOD-INDX)          
043300     END-IF                                                               
043400                                                                          
043500     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
043600                   BASEN-SLUT     OR                                      
043700                   MOD-INDX > MAX-MOD-INDX                                
043800       MOVE SEQF-IDPRC           TO TAB-IDPRC (MOD-INDX)                  
043900                                    WS-JFR-IDPRC                          
044000       ADD SEQF-KVRADER          TO TAB-KVRADER (MOD-INDX)                
044100                                    TOT-KVRADER                           
044200       IF SEQF-KDODELSTA = 'U'                                            
044300         ADD SEQF-KVRADER        TO TAB-KVRADER-UTSKR (MOD-INDX)          
044400                                    TOT-KVRADER-UTSKR                     
044500                                                                          
044600         PERFORM FA-SHOW-NON-PACKED-CASE                                  
044700       ELSE                                                               
044800         IF SEQF-KDODELSTA = 'P'                                          
044900           ADD SEQF-KVRADER      TO TAB-KVRADER-PACK (MOD-INDX)           
045000                                    TOT-KVRADER-PACK                      
045100                                    TAB-KVRADER-UTSKR (MOD-INDX)          
045200                                    TOT-KVRADER-UTSKR                     
045300         END-IF                                                           
045400       END-IF                                                             
045500                                                                          
045600       PERFORM IMS-GN-WDQ3F1                                              
045700       IF (SEGMENT-FINNS                       AND                        
045800           WS-JFR-IDPRC NOT = SEQF-IDPRC) OR                              
045900          (NOT SEGMENT-FINNS)                                             
046000         IF TAB-KVRADER (MOD-INDX) > 0                                    
046100           COMPUTE WS-REPROCENT ROUNDED           =                       
046200                   TAB-KVRADER-UTSKR (MOD-INDX) /                         
046300                  (TAB-KVRADER (MOD-INDX) / 100)                          
046400           MOVE WS-REPROCENT     TO TAB-REPROCENT-UTSKR (MOD-INDX)        
046500           COMPUTE WS-REPROCENT                   =                       
046600                   TAB-KVRADER-PACK  (MOD-INDX) /                         
046700                  (TAB-KVRADER (MOD-INDX) / 100)                          
046800           MOVE WS-REPROCENT     TO TAB-REPROCENT-PACK  (MOD-INDX)        
046900         END-IF                                                           
047000         ADD  +1                 TO MOD-INDX                              
047100         IF MOD-INDX < +501                                               
047200           MOVE +0               TO TAB-KVRADER       (MOD-INDX)          
047300                                    TAB-KVRADER-UTSKR (MOD-INDX)          
047400                                    TAB-KVRADER-PACK  (MOD-INDX)          
047500         END-IF                                                           
047600       END-IF                                                             
047700     END-PERFORM                                                          
047800                                                                          
047900     PERFORM FB-BYGG-DETRADER-TOTRAD                                      
048000     .                                                                    
048100     EJECT                                                                
048200                                                                          
048300 FA-SHOW-NON-PACKED-CASE SECTION.                                         
048400                                                                          
048500*** BELOW IS TO LIST IN A SEPARATE TABEL THE CASES NOT YET PACKED         
048600     IF SEQF-IDPRODNR NOT = W-IDPRODNR                                    
048700       MOVE SEQF-IDPRODNR        TO W-IDPRODNR                            
048800       PERFORM IMS-GU-WDE601                                              
048900       IF SEGMENT-SAKNAS                                                  
049000         DISPLAY '0185 EJ PRODNR > ' W-IDPRODNR                           
049100       ELSE                                                               
049200         PERFORM IMS-GNP-WDE611-STA0                                      
049300         PERFORM UNTIL SEGMENT-SAKNAS OR                                  
049400                       MOD-INDX-KLI > MAX-MOD-INDX                        
049500           MOVE SEQF-IDPRC       TO TAB-IDPRC-KLI(MOD-INDX-KLI)           
049600           MOVE VORD-IDDISTR     TO TAB-IDDISTR(MOD-INDX-KLI)             
049700           MOVE VORD-IDPRODNR    TO TAB-IDPRODNR(MOD-INDX-KLI)            
049800           MOVE KOLLI-IDKOLLI    TO TAB-IDKOLLI(MOD-INDX-KLI)             
049900           DISPLAY '0185 NOT PACKED > PRC PACKER D/C/O/ CASE '            
050000            SEQF-IDPRC ' ' VORD-IDDISTR ' ' VORD-IDPRODNR ' '             
050100            TAB-IDKOLLI(MOD-INDX-KLI)                                     
050200           PERFORM IMS-GNP-WDE611-STA0                                    
050300           ADD +1                TO MOD-INDX-KLI                          
050400         END-PERFORM                                                      
050500       END-IF                                                             
050600     END-IF                                                               
050700     .                                                                    
050800     EJECT                                                                
050900 FB-BYGG-DETRADER-TOTRAD SECTION.                                         
051000                                                                          
051100     IF MOD-INDX > 0                                                      
051200       IF MOD-INDX > 500                                                  
051300         MOVE TOO-MANY-LINES     TO RESP-IDMSG-INFO                       
051400       END-IF                                                             
051500       COMPUTE MAX-MOD-INDX = MOD-INDX - 1                                
051600       MOVE +1                   TO MOD-INDX                              
051700       PERFORM UNTIL MOD-INDX > MAX-MOD-INDX                              
051800         ADD +1                  TO RESP-KVRADER-MAX1                     
051900         MOVE TAB-IDPRC   (MOD-INDX)                                      
052000                                 TO RESP-IDPRC   (MOD-INDX)               
052100         MOVE TAB-KVRADER (MOD-INDX)                                      
052200                                 TO RESP-KVORDRAD (MOD-INDX)              
052300         MOVE TAB-KVRADER-UTSKR (MOD-INDX)                                
052400                                 TO RESP-KVORDRAD-UTSKR(MOD-INDX)         
052500         MOVE TAB-REPROCENT-UTSKR (MOD-INDX)                              
052600                                 TO RESP-REPROCENT-UTSKR(MOD-INDX)        
052700         MOVE TAB-KVRADER-PACK (MOD-INDX)                                 
052800                                 TO RESP-KVORDRAD-PACK (MOD-INDX)         
052900         MOVE TAB-REPROCENT-PACK (MOD-INDX)                               
053000                                 TO RESP-REPROCENT-PACK (MOD-INDX)        
053100         ADD +1                  TO MOD-INDX                              
053200                                                                          
053300       END-PERFORM                                                        
053400                                                                          
053500       IF MOD-INDX-KLI > 0                                                
053600         IF MOD-INDX-KLI > 500                                            
053700           MOVE TOO-MANY-LINES   TO RESP-IDMSG-INFO                       
053800         END-IF                                                           
053900                                                                          
054000         COMPUTE MAX-MOD-INDX = MOD-INDX-KLI - 1                          
054100         MOVE +1                 TO MOD-INDX-KLI                          
054200         PERFORM UNTIL MOD-INDX-KLI > MAX-MOD-INDX                        
054300           ADD +1                TO RESP-KVRADER-MAX2                     
054400           MOVE TAB-IDPRC-KLI (MOD-INDX-KLI)                              
054500                                 TO RESP-IDPRC-KLI  (MOD-INDX-KLI)        
054600           MOVE TAB-IDDISTR (MOD-INDX-KLI)                                
054700                                 TO RESP-IDDISTR    (MOD-INDX-KLI)        
054800           MOVE TAB-IDPRODNR (MOD-INDX-KLI)                               
054900                                 TO RESP-IDPRODNR   (MOD-INDX-KLI)        
055000           MOVE TAB-IDKOLLI (MOD-INDX-KLI)                                
055100                                 TO RESP-IDKOLLI    (MOD-INDX-KLI)        
055200           ADD +1                TO MOD-INDX-KLI                          
055300         END-PERFORM                                                      
055400       END-IF                                                             
055500     END-IF                                                               
055600                                                                          
055700     IF MOD-INDX = 0                                                      
055800       MOVE ERR-RECORD-MISSING   TO RESP-IDMSG-ERROR                      
055900     ELSE                                                                 
056000       IF TOT-KVRADER > 0                                                 
056100         COMPUTE WS-REPROCENT ROUNDED           =                         
056200                 TOT-KVRADER-UTSKR /                                      
056300                (TOT-KVRADER / 100)                                       
056400         MOVE WS-REPROCENT       TO TOT-REPROCENT-UTSKR                   
056500         COMPUTE WS-REPROCENT                   =                         
056600                 TOT-KVRADER-PACK /                                       
056700                (TOT-KVRADER / 100)                                       
056800         MOVE WS-REPROCENT       TO TOT-REPROCENT-PACK                    
056900       END-IF                                                             
057000                                                                          
057100       MOVE TOT-KVRADER          TO RESP-KVORDRAD-TOT                     
057200       MOVE TOT-KVRADER-UTSKR    TO RESP-KVORDRAD-UTSKR-TOT               
057300       MOVE TOT-REPROCENT-UTSKR  TO RESP-REPROCENT-UTSKR-TOT              
057400       MOVE TOT-KVRADER-PACK     TO RESP-KVORDRAD-PACK-TOT                
057500       MOVE TOT-REPROCENT-PACK   TO RESP-REPROCENT-PACK-TOT               
057600     END-IF                                                               
057700     .                                                                    
057800                                                                          
057900 S01-MSG-CONV SECTION.                                                    
058000                                                                          
058100     MOVE LOW-VALUES             TO RESP-MESSAGES (1)                     
058200                                    RESP-MESSAGES (2)                     
058300     MOVE 1                      TO MSG-IX                                
058400*    REQUEST OK                                                           
058500     MOVE 200                    TO RESP-KDSTATUS-API                     
058600     IF RESP-IDMSG-INFO > SPACE                                           
058700       MOVE SPACES               TO MSG-CONV-AREA                         
058800       MOVE RESP-IDMSG-INFO      TO MSG-CONV-IDMSG-IN                     
058900       CALL WMSGCONV          USING MSG-CONV-AREA                         
059000       MOVE MSG-CONV-IDMSG-OUT   TO RESP-IDMSG   (MSG-IX)                 
059100       MOVE MSG-CONV-MESSAGE     TO RESP-MESSAGE (MSG-IX)                 
059200       ADD 1                     TO MSG-IX                                
059300     END-IF                                                               
059400     IF RESP-IDMSG-ERROR > SPACE                                          
059500       IF RESP-IDMSG-ERROR = ERR-RECORD-MISSING                           
059600*        NOT FOUND                                                        
059700         MOVE 404                TO RESP-KDSTATUS-API                     
059800       ELSE                                                               
059900*        BAD REQUEST                                                      
060000         MOVE 400                TO RESP-KDSTATUS-API                     
060100       END-IF                                                             
060200       MOVE SPACES               TO MSG-CONV-AREA                         
060300       MOVE RESP-IDMSG-ERROR     TO MSG-CONV-IDMSG-IN                     
060400       MOVE RESP-IDELMT-ERROR    TO MSG-CONV-IDELMT                       
060500       CALL WMSGCONV          USING MSG-CONV-AREA                         
060600       MOVE MSG-CONV-IDMSG-OUT   TO RESP-IDMSG   (MSG-IX)                 
060700       MOVE MSG-CONV-MESSAGE     TO RESP-MESSAGE (MSG-IX)                 
060800     END-IF                                                               
060900     .                                                                    
061000                                                                          
061100*---- IMS SEKTIONER ---                                                   
061200 IMS-GN-WDQ3F1   SECTION.                                                 
061300                                                                          
061400     STRING 'WDQ3F1  (WDQ3F1KY>=' W-WDQ3F1KY-MIN-X                        
061500                    '&WDQ3F1KY<=' W-WDQ3F1KY-MAX-X ')'                    
061600          DELIMITED BY SIZE INTO SSA1                                     
061700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
061800     CALL CBLTDLI USING GN WDQ3F-PCB DLI-IO-WDQ3F1 SSA1                   
061900     MOVE WDQ3F-STATUS-CODE TO STATUS-WS                                  
062000     PERFORM IMS-STATUSKONTROLL                                           
062100     .                                                                    
062200 IMS-GU-WDB601    SECTION.                                                
062300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
062400             DELIMITED BY SIZE INTO SSA1                                  
062500     MOVE '  GE'                 TO GODK-STATUSKODER                      
062600     CALL CBLTDLI             USING GU                                    
062700                                    WDB6-PCB                              
062800                                    DLI-IO-AREA-B601                      
062900                                    SSA1                                  
063000     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
063100     PERFORM IMS-STATUSKONTROLL                                           
063200     .                                                                    
063300                                                                          
063400 IMS-GU-WDE601    SECTION.                                                
063500     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
063600             DELIMITED BY SIZE INTO SSA1                                  
063700     MOVE '  GE'                 TO GODK-STATUSKODER                      
063800     CALL CBLTDLI             USING GU                                    
063900                                    WDE6-PCB                              
064000                                    DLI-IO-AREA-E601                      
064100                                    SSA1                                  
064200     MOVE WDE6-STATUS-CODE       TO STATUS-WS                             
064300     PERFORM IMS-STATUSKONTROLL                                           
064400     .                                                                    
064500                                                                          
064600 IMS-GNP-WDE611-STA0 SECTION.                                             
064700     STRING 'WDE611  (KDKOLSTA =' W-KDKOLSTA-0-X ')'                      
064800             DELIMITED BY SIZE INTO SSA1                                  
064900     MOVE '  GE'                 TO GODK-STATUSKODER                      
065000     CALL CBLTDLI             USING GNP                                   
065100                                    WDE6-PCB                              
065200                                    DLI-IO-AREA-E611                      
065300                                    SSA1                                  
065400     MOVE WDE6-STATUS-CODE       TO STATUS-WS                             
065500     PERFORM IMS-STATUSKONTROLL                                           
065600     .                                                                    
065700                                                                          
065800 IMS-STATUSKONTROLL SECTION.                                              
065900                                                                          
066000     SET STATUS-IX TO 1                                                   
066100     SEARCH GODK-STATUS                                                   
066200       AT END                                                             
066300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
066400         DELIMITED BY SIZE INTO FELTEXT                                   
066500         CALL FELLOG                                                      
066600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
066700         CONTINUE                                                         
066800     END-SEARCH                                                           
066900     .                                                                    
