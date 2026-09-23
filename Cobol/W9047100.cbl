000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9047100.                                                
000300 AUTHOR.         CONNY EGHOLT.                                            
000400 DATE-WRITTEN.   2005/04/22.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       'CARPARTS.SPIE.DCNPARTINFO'                              
000800*                                                                         
000900*    ETRACKER: 1626694 (New Program)                                      
001000*                                                                         
001100*    FUNCTION:                                                            
001200*        THE PROGRAM IS A MPP TO SERVE THE SPIE-SYSTEM WITH               
001300*        INFO ABOUT PARTS BELONGING TO DCN (ƒO) STATED IN REQU            
001400*                                                                         
001500*        PROGRAM READS         WDK6 (PARTNOL)                             
001600*        PROGRAM READS         WDK6F         SEQ-INDEX ON IDAO            
001700*        PROGRAM READS         WDN6 (CAT-MASTER)                          
001800*        PROGRAM READS         WDP3 (PERSONAL CODE)                       
001900*        PROGRAM READS         WDP3A         SEQ-INDEX ON PARTNO          
002000*        PROGRAM READS         WDP3B         SEQ-INDEX ON SUPPLIER        
002100*        PROGRAM READS         WDP3C         SEQ-INDEX ON FKNGRP          
002200*        PROGRAM READS         WDD2 (NYPON)                               
002300*        PROGRAM READS         WDD9 (DELIVERY SCHEDULES)                  
002400*        PROGRAM READS         WDL2 (GOODS RECEIVING)                     
002500*                                                                         
002600*                                                                         
002700*        CODE FOR WDP3-READINGS PARTLY COPIED FROM W6010800               
002800*        CODE FOR WDD2-READINGS PARTLY COPIED FROM W2014200               
002900*        CODE FOR WDD9-READINGS PARTLY COPIED FROM W2010600               
003000*                                                                         
003100*    INPUTS DATA.                                                         
003200*        TRANSACTION: W90471T                                             
003300*        REQUEST:     W90471I1                                            
003400*                                                                         
003500*    OUTPUTS DATA.                                                        
003600*        RESPONSE:    W90471O1                                            
003700*                                                                         
003800*    CHANGES:                                                             
003900*         SCR eTracker 2398409 inst. 29/8-05                              
004000*         § The list aborts when all RESP-UTDATARAD are filled            
004100*           and at the same time the current DCN was exactly at           
004200*           last partnumber. (This didn't occur during TEST phase)        
004300*                                                                         
004400*         Emerg. eTracker 2407635  of 31/8-05                             
004500*         § The list aborts when all RESP-UTDATARAD are filled            
004600*           and at the same time the next and maybe more DCN:no           
004700*           in the REQU-list have no partnumbers in WDK6F.                
004800*           (This gives actually 'GE' at read with GU on WDK6F.)          
004900*           (This did not appear during TEST phases)                      
005000*                                                                         
005100*         SCR eTracker 2411589  of  1/9-05                                
005200*         § Add the fields for PROJ and IDPROJK.                          
005300                                                                          
005400     SKIP3                                                                
005500 ENVIRONMENT DIVISION.                                                    
005600     SKIP2                                                                
005700 INPUT-OUTPUT SECTION.                                                    
005800                                                                          
005900 FILE-CONTROL.                                                            
006000     EJECT                                                                
006100 DATA DIVISION.                                                           
006200     SKIP3                                                                
006300 FILE SECTION.                                                            
006400     EJECT                                                                
006500 WORKING-STORAGE SECTION.                                                 
006600 77  IDPGM                       PIC X(08)   VALUE 'W9047100'.            
006700                                                                          
006800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
006900 77  KDRC-DISPLAY                PIC Z(5).                                
007000                                                                          
007100 77  DLI-CALL-SECT               PIC X(40) VALUE SPACE.                   
007200 77  ERRTEXT                     PIC X(80) VALUE SPACE.                   
007300                                                                          
007400 77  JA                          PIC X     VALUE 'J'.                     
007500 77  NEJ                         PIC X     VALUE 'N'.                     
007600                                                                          
007700 01  FILLER                      PIC X(16) VALUE 'INDEXFƒLT'.             
007800 77  REQU-IX                     PIC S9(9) VALUE +0 COMP SYNC.            
007900 77  RESP-IX                     PIC S9(9) VALUE +0 COMP SYNC.            
008000 77  IND                         PIC S9(9) VALUE +0 COMP SYNC.            
008100                                                                          
008200*    -COPY WWDCKONS                                                       
008300                                                                          
008400*    ---  ARBETSFƒLT                                                      
008500 01  FILLER                      PIC X(16) VALUE 'SPAR-FƒLT'.             
008600 77  SAVE-IDANSK                 PIC S9(3) VALUE +0 COMP-3.               
008700 77  SAVE-IDBERED                PIC S9(3) VALUE +0 COMP-3.               
008800 77  SAVE-IDINK                  PIC S9(3) VALUE +0 COMP-3.               
008900 77  SAVE-IDFKNGRP               PIC 9(4)  VALUE ZERO.                    
009000 77  SAVE-IDLEVNR                PIC X(5)  VALUE SPACE.                   
009100 77  SAVE-TIAVIDAT-EARLIEST      PIC S9(7) VALUE +0 COMP-3.               
009200 77  WS-IDARTNR                  PIC 9(8)  VALUE ZERO.                    
009300 77  WS-COUNT                    PIC 9(3)  VALUE ZERO.                    
009400 77  R-IX                        PIC 9(3)  VALUE ZERO.                    
009500 01  WS-CURRENT-DATE.                                                     
009600     03  WS-CURRENT-DATUM        PIC X(8) VALUE SPACE.                    
009700     03  WS-CURRENT-TIME         PIC X(8) VALUE SPACE.                    
009800                                                                          
009900 77  PERSON-SW                   PIC X     VALUE 'J'.                     
010000     88  PERSON-EXIST                      VALUE 'J'.                     
010100     88  PERSON-MISSING                    VALUE 'N'.                     
010200                                                                          
010300 77  INDATA-SW                   PIC X     VALUE 'J'.                     
010400     88  INDATA-OK                         VALUE 'J'.                     
010500     88  INDATA-ERR                        VALUE 'N'.                     
010600 77  UTDATA-SW                   PIC X     VALUE 'J'.                     
010700     88  UTDATA-OK                         VALUE 'J'.                     
010800     88  UTDATA-ERR                        VALUE 'N'.                     
010900                                                                          
011000 77  IDAO-SW                     PIC X     VALUE 'N'.                     
011100     88  IDAO-EXIST                        VALUE 'J'.                     
011200     88  IDAO-MISSING                      VALUE 'N'.                     
011300                                                                          
011400 77  ALL-SW                      PIC X     VALUE 'J'.                     
011500     88  ALL-OK                            VALUE 'J'.                     
011600                                                                          
011700                                                                          
011800                                                                          
011900 01  FILLER                      PIC X(16) VALUE 'WS-FIELDS'.             
012000 01  GENERAL-WS-FIELDS.                                                   
012100     SKIP2                                                                
012200     03 WS-AAVV.                                                          
012300         05 WS-AA                PIC 9(2).                                
012400         05 WS-VV                PIC 9(2).                                
012500     03 WS-KDERS                 PIC S9(3) VALUE +0 COMP-3.               
012600     03 WS-IDPERSON              PIC S9(3) VALUE +0 COMP-3.               
012700     03 WS-DALEVBSK-AVS          PIC 9(8).                                
012800     03 FILLER REDEFINES WS-DALEVBSK-AVS.                                 
012900         05 WS-DALEVBSK-SS       PIC 9(2).                                
013000         05 WS-DALEVBSK-AAMMDD   PIC 9(6).                                
013100     EJECT                                                                
013200*                 -----  WORK AREA FOR Y2K                                
013300*    -COPY WY2000W1                                                       
013400                                                                          
013500     EJECT                                                                
013600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
013700 01  GENERAL-SUBPROGRAMS.                                                 
013800     03  FELLOG                  PIC X(8)  VALUE 'FELLOG  '.              
013900     03  ABEND                   PIC X(8)  VALUE 'ABEND   '.              
014000     03  WZ01SUB                 PIC X(8)  VALUE 'WZ01SUB '.              
014100     03  WDATKONV                PIC X(8)  VALUE 'WDATKONV'.              
014200     03  CBLTDLI                 PIC X(8)  VALUE 'CBLTDLI '.              
014300     SKIP3                                                                
014400*    --- PARAMETERS TO ABEND                                              
014500                                                                          
014600 77  RKOD-ABEND                  PIC S9(4) COMP VALUE +0.                 
014700 77  RKOD-ABEND-NO-DUMP          PIC S9(4) COMP VALUE +16.                
014800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4) COMP VALUE +1000.              
014900     EJECT                                                                
015000*                                                                         
015100 01  FILLER                      PIC X(16) VALUE 'SUB-CONTROL'.           
015200     SKIP3                                                                
015300*01  -COPY WZ01SUB                                                        
015400     EJECT                                                                
015500 01  FILLER                      PIC X(16) VALUE 'REQU-AREA'.             
015600     SKIP3                                                                
015700 01  REQU-AREA.                                                           
015800*    03  -COPY W90471I1                                                   
015900     EJECT                                                                
016000 01  FILLER                      PIC X(16) VALUE 'RESP-AREA'.             
016100     SKIP3                                                                
016200 01  RESP-AREA.                                                           
016300*    03  -COPY W90471O1                                                   
016400     EJECT                                                                
016500*    --- SUBPROGRAM WDATKONV                                              
016600*01 -COPY WDATAREA                                                        
016700     EJECT                                                                
016800*    --- WORK AREAS TO IMS-SECTIONS                                       
016900*                                                                         
017000     SKIP3                                                                
017100 01  FILLER                      PIC X(16) VALUE 'IMS-WS'.                
017200     SKIP3                                                                
017300 01  KEYS-TILL-DLI.                                                       
017400                                                                          
017500*    ---- WDK6-KEYS                                                       
017600     03  W-IDARTNR-X.                                                     
017700         05  W-IDARTNR       PIC S9(9)     VALUE ZERO COMP-3.             
017800     03  W-KDEMBAL-X.                                                     
017900         05  W-KDEMBAL       PIC X(3)      VALUE SPACE.                   
018000                                                                          
018100*    ---- WDD9-KEYS                                                       
018400     03  W-WDD901KY-X.                                                    
018410         05  W-IDARTNR-D9    PIC S9(9)     VALUE ZERO COMP-3.             
018411         05  W-IDDC-D9       PIC X(02)     VALUE SPACE.                   
018420     03  W-IDLEVNR-X.                                                     
018430         05  W-IDLEVNR       PIC X(5)      VALUE SPACE.                   
018500                                                                          
018600*    ---- IDAO IS SET TO SEARCH THE 7 FIRST BYTES (SUFFIX EXCLUDED        
018700*    ---- WDK6F-KEYS                                                      
018800     03 W-WDK6F1KY-LO-X.                                                  
018900        05 W-IDAO-LO-X.                                                   
019000           07 W-IDAO-LO      PIC X(07)     VALUE LOW-VALUE.               
019100           07 FILLER         PIC X(03)     VALUE LOW-VALUE.               
019200           07 FILLER         PIC X(05)     VALUE LOW-VALUE.               
019300                                                                          
019400     03 W-WDK6F1KY-HI-X.                                                  
019500        05 W-IDAO-HI-X.                                                   
019600           07 W-IDAO-HI      PIC X(07)     VALUE HIGH-VALUE.              
019700           07 FILLER         PIC X(03)     VALUE HIGH-VALUE.              
019800           07 FILLER         PIC X(05)     VALUE HIGH-VALUE.              
019900                                                                          
020000*    ---- WDP3-KEYS                                                       
020100     03  W-KDARBTYP-X.                                                    
020200         05  W-KDARBTYP          PIC X(8)  VALUE SPACE.                   
020300     03  W-IDPERSON-X.                                                    
020400         05  W-IDPERSON          PIC S9(3) VALUE +0 COMP-3.               
020500                                                                          
020600     03  W-WDP3A1-MIN.                                                    
020700         05 W-IDLANDA1-MIN       PIC X(2)  VALUE SPACE.                   
020710         05 W-IDARTNRF-MIN       PIC S9(9) VALUE ZERO COMP-3.             
020800         05 W-IDARTNRT-MIN       PIC S9(9) VALUE ZERO COMP-3.             
020900         05 W-KDARBTYP-A-MIN     PIC X(8)  VALUE LOW-VALUE.               
021000     03  W-WDP3A1-MAX.                                                    
021010         05 W-IDLANDA1-MAX       PIC x(2)  VALUE SPACE.                   
021100         05 W-IDARTNRF-MAX       PIC S9(9) VALUE 999999999 COMP-3.        
021200         05 W-IDARTNRT-MAX       PIC S9(9) VALUE 999999999 COMP-3.        
021300         05 W-KDARBTYP-A-MAX     PIC X(8)  VALUE HIGH-VALUE.              
021400                                                                          
021500     03  W-WDP3B1-X.                                                      
021510         05  W-IDLAND-B          PIC x(2)  VALUE SPACE.                   
021600         05  W-IDLEVNR-B         PIC X(5)  VALUE LOW-VALUE.               
021700         05  W-KDARBTYP-B        PIC X(8)  VALUE LOW-VALUE.               
021800                                                                          
021900     03  W-WDP3C1-MIN.                                                    
021910         05 W-IDLANDC1-MIN       PIC x(2)  VALUE SPACE.                   
022000         05 W-IDFKNGRPF-C-MIN    PIC S9(5) VALUE +00000 COMP-3.           
022100         05 W-IDFKNGRPT-C-MIN    PIC S9(5) VALUE +00000 COMP-3.           
022200         05 W-KDARBTYP-C-MIN     PIC X(8)  VALUE LOW-VALUE.               
022300     03  W-WDP3C1-MAX.                                                    
022310         05 W-IDLANDC1-MAX       PIC x(2)  VALUE SPACE.                   
022400         05 W-IDFKNGRPF-C-MAX    PIC S9(5) VALUE +99999 COMP-3.           
022500         05 W-IDFKNGRPT-C-MAX    PIC S9(5) VALUE +99999 COMP-3.           
022600         05 W-KDARBTYP-C-MAX     PIC X(8)  VALUE HIGH-VALUE.              
022700                                                                          
022800*    ---- WDL2-KEYS                                                       
022900     03  W-DAINLEV-X.                                                     
023000         05  W-DAINLEV         PIC 9(16)   VALUE ZERO.                    
023100     SKIP2                                                                
023200*    ---- WDF7-KEYS                                                       
023300     03    W-WDF701KY-X.                                                  
023400        05    W-IDARTNR-F7       PIC S9(9)  VALUE ZERO  COMP-3.           
023500        05    W-IDPRTNER         PIC S9(5)  VALUE +1    COMP-3.           
023600     SKIP2                                                                
023700*    --- STATUS-CODE FROM IMS                                             
023800 01  STATUS-WS                   PIC XX.                                  
023900     88  SEGMENT-EXIST           VALUE  IS  '  '.                         
024000     88  SEGMENT-MISSING         VALUES ARE 'GE', 'GB'.                   
024100     SKIP2                                                                
024200 01  GOOD-STATUSCODES.                                                    
024300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024400     SKIP3                                                                
024500 01  SSA1                        PIC X(96).                               
024600 01  SSA2                        PIC X(96).                               
024700 01  SSA3                        PIC X(64).                               
024800     EJECT                                                                
024900*    --- IMS FUNCTION CODES                                               
025000*01  -COPY W0003                                                          
025100     EJECT                                                                
025200*    ---  DLI INPUT-OUTPUT AREOR                                          
025300 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA'.           
025400     SKIP3                                                                
025500 01  DLI-IO-AREA.                                                         
025600     SKIP3                                                                
025700     03  DLI-WDK6F-AREA.                                                  
025800*        05  -COPY WDK6F1                                                 
025900     EJECT                                                                
026000     03  DLI-WDK601-AREA.                                                 
026100*        05  -COPY WDK601 -PRE WDK6-                                      
026200     EJECT                                                                
026300     03  DLI-WDK611-AREA.                                                 
026400*        05  -COPY WDK611                                                 
026500     EJECT                                                                
026600     03  DLI-WDK613-AREA.                                                 
026700*        05  -COPY WDK613                                                 
026800     EJECT                                                                
026900     03  DLI-WDK623-AREA.                                                 
027000*        05  -COPY WDK623                                                 
027100     EJECT                                                                
027200     03  DLI-WDN601-AREA.                                                 
027300*        05  -COPY WDN601                                                 
027400     EJECT                                                                
027500     03  DLI-WDP3A-AREA.                                                  
027600*        05  -COPY WDP3A1                                                 
027700     EJECT                                                                
027800     03  DLI-WDP3B-AREA.                                                  
027900*        05  -COPY WDP3B1                                                 
028000     EJECT                                                                
028100     03  DLI-WDP3C-AREA.                                                  
028200*        05  -COPY WDP3C1                                                 
028300     EJECT                                                                
028400     03  DLI-WDP311-AREA.                                                 
028500*        05  -COPY WDP311                                                 
028600     EJECT                                                                
028700     03  DLI-WDD201-AREA.                                                 
028800*        05  -COPY WDD201   -PRE WDD2-                                    
028900     EJECT                                                                
029000     03  DLI-WDD924-AREA.                                                 
029100*        05  -COPY WDD924                                                 
029200     EJECT                                                                
029300     03  DLI-WDL201-AREA.                                                 
029400*        05  -COPY WDL201   -PRE WDL2-                                    
029500     EJECT                                                                
029600     03  DLI-WDL211-AREA.                                                 
029700*        05  -COPY WDL211                                                 
029800     EJECT                                                                
029900     03  DLI-WDL221-AREA.                                                 
030000*        05  -COPY WDL221                                                 
030100     EJECT                                                                
030200 01  DLI-IO-AREA-7.                                                       
030300     03  IO-AREA-7       PIC X(300)  VALUE SPACE.                         
030400*    03  WDF701   -COPY WDF701               -RED IO-AREA-7.              
030500     EJECT                                                                
030600     EJECT                                                                
030700                                                                          
030800                                                                          
030900 LINKAGE SECTION.                                                         
031000 01  MSG-PCB                     PIC X.                                   
031100     EJECT                                                                
031200*01  -COPY W0008  -PRE WDK6-                                              
031300     05  FILLER                  PIC X.                                   
031400     EJECT                                                                
031500*01  -COPY W0008  -PRE WDK6F-                                             
031600     05  FILLER                  PIC X.                                   
031700     EJECT                                                                
031800*01  -COPY W0008  -PRE WDN6-                                              
031900     05  FILLER                  PIC X.                                   
032000     EJECT                                                                
032100*01  -COPY W0008  -PRE WDP3-                                              
032200     05  FILLER                  PIC X.                                   
032300     EJECT                                                                
032400*01  -COPY W0008  -PRE WDP3A-                                             
032500     05  FILLER                  PIC X.                                   
032600     EJECT                                                                
032700*01  -COPY W0008  -PRE WDP3B-                                             
032800     05  FILLER                  PIC X.                                   
032900     EJECT                                                                
033000*01  -COPY W0008  -PRE WDP3C-                                             
033100     05  FILLER                  PIC X.                                   
033200     EJECT                                                                
033300*01  -COPY W0008  -PRE WDD2-                                              
033400     05  FILLER                  PIC X.                                   
033500     EJECT                                                                
033600*01  -COPY W0008  -PRE WDD9-                                              
033700     05  FILLER                  PIC X.                                   
033800     EJECT                                                                
033900*01  -COPY W0008  -PRE WDL2-                                              
034000     05  FILLER                  PIC X.                                   
034100     EJECT                                                                
034200*01  -COPY W0008  -PRE WDF7-                                              
034300     05  FILLER                  PIC X.                                   
034400     EJECT                                                                
034500                                                                          
034600 PROCEDURE DIVISION  USING MSG-PCB  WDK6-PCB  WDK6F-PCB  WDN6-PCB         
034700                           WDP3-PCB WDP3A-PCB WDP3B-PCB WDP3C-PCB         
034800                           WDD2-PCB WDD9-PCB  WDL2-PCB WDF7-PCB.          
034900 MAIN SECTION.                                                            
035000     ENTRY 'DLITCBL' USING MSG-PCB  WDK6-PCB  WDK6F-PCB  WDN6-PCB         
035100                           WDP3-PCB WDP3A-PCB WDP3B-PCB WDP3C-PCB         
035200                           WDD2-PCB WDD9-PCB  WDL2-PCB WDF7-PCB.          
035300                                                                          
035400     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
035500     IF SUB-KDRC = 0                                                      
035600       PERFORM A-INIT                                                     
035700       PERFORM B-CHECK-KEYS-AND-REQU-IX                                   
035800       IF INDATA-OK                                                       
035900           PERFORM UNTIL ( REQU-IX > REQU-KVRADER-MAX )                   
036000*                     --- Loop until end of Requested DCN-list            
036100                      OR ( RESP-IX = 100 AND                              
036200                           REQU-IX <= REQU-KVRADER-MAX                    
036300*                     --- or RESP-MAX reached before end-of-REQU          
036400                            )                                             
036500               PERFORM IMS-GU-WDK6F1-IDAO                                 
036600*              --- Read a new DCN-no (=IDAO)                              
036700               IF SEGMENT-EXIST                                           
036800                   IF W-IDARTNR  > ZERO                                   
036900                       PERFORM IMS-GU-WDK6F1-IDAO-ARTIKEL                 
037000                   END-IF                                                 
037100                                                                          
037200                   PERFORM UNTIL SEGMENT-MISSING                          
037300                              OR (RESP-IX = 100 )                         
037400                       PERFORM C-BEHANDLA-ARTIKEL-I-IDAO                  
037500                       PERFORM IMS-GN-WDK6F1-IDAO                         
037600*                      -- Last PartNo gives SEGMENT-MISSING               
037700                   END-PERFORM                                            
037800               ELSE                                                       
037900                   ADD +1         TO RESP-IX                              
038000                   INITIALIZE        RESP-UTDATARAD(RESP-IX)              
038100                   MOVE W-IDAO-LO TO RESP-IDAO(RESP-IX)                   
038200*TEST visa "ej funna" rader                                               
038300*                  DISPLAY RESP-UTDATARAD(RESP-IX)                        
038400*TEST                                                                     
038500               END-IF                                                     
038600                                                                          
038700               ADD +1 TO REQU-IX                                          
038800               IF REQU-IX <= REQU-KVRADER-MAX                             
038900*                  --- Set read-keys for next IDAO                        
039000                   MOVE REQU-IDAO(REQU-IX) TO W-IDAO-LO                   
039100                                              W-IDAO-HI                   
039200                   MOVE ZERO               TO W-IDARTNR                   
039300                   MOVE ZERO               TO W-IDARTNR-F7                
039400               END-IF                                                     
039500           END-PERFORM                                                    
039600*                                                                         
039700*          --- Process finished   ----                                    
039800*          --- is resp-area full ?                                        
039900           IF RESP-IX = 100                                               
040000               IF SEGMENT-EXIST                                           
040100*                  --- More partnos found on WDK6F                        
040200*                  --- Report keys for the next transaction               
040300                   MOVE SEQF-IDAO    TO RESP-IDAO-NEXT                    
040400                   MOVE SEQF-IDARTNR TO RESP-IDARTNR-NEXT                 
040500               ELSE                                                       
040600*                  -- Check if there is more DCN in REQU                  
040700*                  -- Last read could have reached last PartNo            
040800*                  -- and thus got SEGMENT-MISSING                        
040900                   IF REQU-IX <= REQU-KVRADER-MAX                         
041000*                      -- New Read keys already moved above               
041100                       PERFORM IMS-GU-WDK6F1-IDAO                         
041200                                                                          
041300                       PERFORM UNTIL SEGMENT-EXIST                        
041400                               OR REQU-IX >= REQU-KVRADER-MAX             
041500*                         --- Set read-keys for next IDAO                 
041600                          ADD +1 TO REQU-IX                               
041700                          MOVE REQU-IDAO(REQU-IX) TO W-IDAO-LO            
041800                                                     W-IDAO-HI            
041900                          MOVE ZERO        TO W-IDARTNR                   
042000                          MOVE ZERO        TO W-IDARTNR-F7                
042100                          PERFORM IMS-GU-WDK6F1-IDAO                      
042200                       END-PERFORM                                        
042300                                                                          
042400                       IF SEGMENT-EXIST                                   
042500*                        --- Next valid hit on WDK6F                      
042600*                        --- Report keys for next transaction             
042700                         MOVE SEQF-IDAO    TO RESP-IDAO-NEXT              
042800                         MOVE SEQF-IDARTNR TO RESP-IDARTNR-NEXT           
042900                       END-IF                                             
043000                   ELSE                                                   
043100                       CONTINUE                                           
043200*                  -- It was exactly 100 output lines                     
043300*                  -- And no more REQU lines exist                        
043400                   END-IF                                                 
043500               END-IF                                                     
043600           ELSE                                                           
043700               CONTINUE                                                   
043800           END-IF                                                         
043900*TEST                                                                     
044000*          DISPLAY 'RESP-MAX   = 100'                                     
044100*          DISPLAY 'RESP-IX    =' RESP-IX                                 
044200*          DISPLAY 'IDAO-NEXT  =' RESP-IDAO-NEXT                          
044300*          DISPLAY 'IDART-NEXT =' RESP-IDARTNR-NEXT                       
044400*TEST                                                                     
044500       ELSE                                                               
044600           CONTINUE                                                       
044700       END-IF                                                             
044800     ELSE                                                                 
044900*      --- RC > 0                                                         
045000       MOVE 'TRANSACTION RECEPTION FAILED ' TO RESP-MEDDELANDE            
045100     END-IF                                                               
045200                                                                          
045300     MOVE RESP-IX TO RESP-KVRADER-MAX                                     
045400*    CALL FELLOG                                                          
045500     PERFORM S02-RETURN-RESPONSE                                          
045600                                                                          
045700     MOVE ZERO TO RETURN-CODE                                             
045800     GOBACK                                                               
045900     .                                                                    
046000     EJECT                                                                
046100 A-INIT SECTION.                                                          
046200     SKIP2                                                                
046300                                                                          
046400     MOVE SPACE TO  RESP-MEDDELANDE                                       
046500                    RESP-IDAO-NEXT                                        
046600     MOVE ZERO  TO  RESP-IDARTNR-NEXT                                     
046700                                                                          
046800     MOVE 100   TO  RESP-KVRADER-MAX                                      
046900     MOVE +0    TO  RESP-IX                                               
047000                                                                          
047100     INITIALIZE     RESP-UTDATARAD(1)                                     
047200                                                                          
047300     MOVE LOW-VALUE  TO W-WDP3A1-MIN                                      
047400                        W-WDP3C1-MIN                                      
047500     MOVE HIGH-VALUE TO W-WDP3A1-MAX                                      
047600                        W-WDP3C1-MAX                                      
047610     MOVE 'SE'       TO W-IDLANDA1-MIN                                    
047620                        W-IDLANDA1-MAX                                    
047630                        W-IDLAND-B                                        
047640                        W-IDLANDC1-MIN                                    
047650                        W-IDLANDC1-MAX                                    
047700                                                                          
047800     MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE                        
047900                                                                          
048000*TEST                                                                     
048100*    DISPLAY 'W90471T DEN: ' WS-CURRENT-DATUM                             
048200*                    ' KL: ' WS-CURRENT-TIME                              
048300*    DISPLAY 'REQU-80-BYTE: ' REQU-W90471I1(1:80)                         
048400*TEST                                                                     
048500     .                                                                    
048600     EJECT                                                                
048700 B-CHECK-KEYS-AND-REQU-IX     SECTION.                                    
048800     SKIP2                                                                
048900     MOVE ZERO TO REQU-IX                                                 
049000                                                                          
049100     IF REQU-IDAO-NEXT = SPACE OR ALL '+'                                 
049200*      --- I.e. New query from start of REQU-IDAO table                   
049300       IF REQU-KVRADER-MAX > ZERO                                         
049400*        --- Table has obviously at least 1 DCN-no, set the key           
049500         MOVE +1 TO REQU-IX                                               
049600         MOVE REQU-IDAO(REQU-IX) TO W-IDAO-LO                             
049700                                    W-IDAO-HI                             
049800         MOVE ZERO               TO W-IDARTNR                             
049900         MOVE ZERO               TO W-IDARTNR-F7                          
050000       ELSE                                                               
050100         MOVE NEJ TO INDATA-SW                                            
050200         MOVE 'ZERO VALUE IN ''KVRADER-MAX''' TO RESP-MEDDELANDE          
050300         MOVE REQU-IDAO-NEXT    TO RESP-IDAO-NEXT                         
050400         MOVE REQU-IDARTNR-NEXT TO RESP-IDARTNR-NEXT                      
050500         MOVE REQU-KVRADER-MAX  TO RESP-KVRADER-MAX                       
050600       END-IF                                                             
050700     ELSE                                                                 
050800*      --- Previous Request was not quite ready processed !               
050900*      --- Start from IDAO-NEXT and IDARTNR-NEXT                          
051000       IF REQU-KVRADER-MAX > ZERO                                         
051100*        --- Everything OK                                                
051200         PERFORM BA-BESTAEM-AATERSTART-IX                                 
051300                                                                          
051400       ELSE                                                               
051500         MOVE NEJ TO INDATA-SW                                            
051600         MOVE 'ZERO VALUE IN ''KVRADER-MAX''' TO RESP-MEDDELANDE          
051700         MOVE REQU-IDAO-NEXT    TO RESP-IDAO-NEXT                         
051800         MOVE REQU-IDARTNR-NEXT TO RESP-IDARTNR-NEXT                      
051900         MOVE REQU-KVRADER-MAX  TO RESP-KVRADER-MAX                       
052000       END-IF                                                             
052100     END-IF                                                               
052200     .                                                                    
052300     EJECT                                                                
052400                                                                          
052500 BA-BESTAEM-AATERSTART-IX SECTION.                                        
052600     SKIP2                                                                
052700*    --- Position the DCN that was interupted in last transaction         
052800     MOVE +1 TO REQU-IX                                                   
052900     PERFORM UNTIL REQU-IX > REQU-KVRADER-MAX                             
052910                OR REQU-IDAO-NEXT = REQU-IDAO(REQU-IX)                    
053100         ADD +1 TO REQU-IX                                                
053200     END-PERFORM                                                          
053300*    --- Now is REQU-IX on right IDAO, or beyond REQU-KVRADER-MAX         
053400                                                                          
053500     IF REQU-IX > REQU-KVRADER-MAX                                        
053600         MOVE NEJ TO INDATA-SW                                            
053700         STRING 'IDAO-NEXT ' REQU-IDAO-NEXT ' NOT FOUND IN REQU-.'        
053800         DELIMITED BY SIZE INTO RESP-MEDDELANDE                           
053900     ELSE                                                                 
054000*      --- Proper start-DCN found in the REQU table.                      
054100*      --- Read partno in WDK6F that was next in former transactio        
054200*      --- Qualified search keys for Unique hit                           
054300       MOVE REQU-IDAO(REQU-IX)    TO W-IDAO-LO                            
054400                                     W-IDAO-HI                            
054500       MOVE REQU-IDARTNR-NEXT     TO W-IDARTNR                            
054600       MOVE REQU-IDARTNR-NEXT     TO W-IDARTNR-F7                         
054700     END-IF                                                               
054800     .                                                                    
054900     EJECT                                                                
055000                                                                          
055100 C-BEHANDLA-ARTIKEL-I-IDAO  SECTION.                                      
055200     SKIP2                                                                
055300     ADD +1 TO RESP-IX                                                    
055400                                                                          
055500     MOVE SEQF-IDAO    TO RESP-IDAO (RESP-IX)                             
055600     MOVE SEQF-IDARTNR TO RESP-IDARTNR (RESP-IX)                          
055700                          W-IDARTNR                                       
055800                          W-IDARTNR-F7                                    
055900                                                                          
056000     PERFORM CA-READ-WDK6DATA-IDARTNR                                     
056100                                                                          
056200     IF UTDATA-OK                                                         
056300         PERFORM CB-READ-WDN6DATA-IDARTNR                                 
056400         PERFORM CC-READ-WDP3-NAMN                                        
056500         PERFORM CD-READ-WDD2-INKOP                                       
056600         PERFORM CE-READ-WDD9-LEVBSK                                      
056700         PERFORM CF-READ-EARLIEST-WDL2-TIAVIDAT                           
056800         PERFORM CG-READ-WDF7-FLGEMFMC                                    
056900*TEST visa funna rader                                                    
057000*        DISPLAY RESP-UTDATARAD(RESP-IX)                                  
057100*TEST                                                                     
057200     ELSE                                                                 
057300         SUBTRACT +1 FROM RESP-IX                                         
057400     END-IF                                                               
057500     .                                                                    
057600     EJECT                                                                
057700                                                                          
057800 CA-READ-WDK6DATA-IDARTNR SECTION.                                        
057900     SKIP2                                                                
058000*    --- Here is   RESP-TIREGDAT,   RESP-KDUART,                          
058100*                  RESP-IDBERED,    RESP-KDERS,                           
058200*                  RESP-KDSORT,     RESP-KDEMBKOD-EMQ2,                   
058300*                  RESP-KDPRODSL    RESP-TIAVTAL-FIRST (≈≈MMDD)           
058400*             --- set                                                     
058500*                                                                         
058600*    --- Prepare with                                                     
058700*                  SAVE-IDLEVNR,                                          
058800*                  SAVE-IDFKNGRP,                                         
058900*                  SAVE-IDANSK,                                           
059000*                  SAVE-IDINK,                                            
059100*                  SAVE-IDBERED    for all IDNAMN                         
059200     PERFORM IMS-GU-WDK601                                                
059300                                                                          
059400     IF SEGMENT-EXIST                                                     
059500       MOVE WDK6-ART-KDERS-UTG TO WS-KDERS                                
059600       MOVE WDK6-ART-IDLEVNR   TO SAVE-IDLEVNR                            
059700       MOVE WDK6-ART-IDFKNGRP  TO SAVE-IDFKNGRP                           
059800       MOVE WDK6-ART-TIREGDAT  TO RESP-TIREGDAT (RESP-IX)                 
059900       MOVE WDK6-ART-KDSORT    TO RESP-KDSORT  (RESP-IX)                  
060000       MOVE WDK6-ART-KDPRODSL  TO RESP-KDPRODSL (RESP-IX)                 
060100                                                                          
060200       PERFORM IMS-GNP-WDK611                                             
060300                                                                          
060400       IF SEGMENT-EXIST                                                   
060500         MOVE CLAG-KDUART  TO RESP-KDUART  (RESP-IX)                      
060600         MOVE CLAG-KDERS   TO WS-KDERS                                    
060700         MOVE CLAG-IDANSK  TO SAVE-IDANSK                                 
060800         MOVE CLAG-IDBERED TO RESP-IDBERED (RESP-IX)                      
060900                              SAVE-IDBERED                                
061000         IF CLAG-IDINK (1:3) NUMERIC                                      
061100            MOVE CLAG-IDINK (1:3) TO SAVE-IDINK                           
061200         ELSE                                                             
061300            IF CLAG-IDINK (2:3) NUMERIC                                   
061400               MOVE CLAG-IDINK (2:3) TO SAVE-IDINK                        
061500            ELSE                                                          
061600               IF CLAG-IDINK (1:2) NUMERIC                                
061700                  MOVE CLAG-IDINK (1:2) TO SAVE-IDINK                     
061800               ELSE                                                       
061900                  IF CLAG-IDINK (1:1) NUMERIC                             
062000                     MOVE CLAG-IDINK (1:1) TO SAVE-IDINK                  
062100                  ELSE                                                    
062200                     MOVE ZERO TO SAVE-IDINK                              
062300                  END-IF                                                  
062400               END-IF                                                     
062500            END-IF                                                        
062600         END-IF                                                           
062700                                                                          
062800*        --- H‰mta ‰ldsta avtalsdatum                                     
062900*        --- Get oldest (first inserted) date                             
063000         PERFORM IMS-GNP-WDK623-SISTA                                     
063100         IF SEGMENT-EXIST                                                 
063200           MOVE AVT-TIAVTAL     TO RESP-TIAVTAL-FIRST (RESP-IX)           
063300**     Prepared  Move AVT-IDLEVNR-AVT TO RESP-IDLEVNR-AVT(RESP-IX)        
063400**     Prepared  Move AVT-KVAVTANT   TO RESP-ARSANTAL-AVT(RESP-IX)        
063500         ELSE                                                             
063600           MOVE ZEROES          TO RESP-TIAVTAL-FIRST (RESP-IX)           
063700         END-IF                                                           
063800                                                                          
063900       ELSE                                                               
064000           MOVE SPACE TO SAVE-IDLEVNR                                     
064100           MOVE ZERO  TO SAVE-IDANSK                                      
064200           MOVE ZERO  TO SAVE-IDINK                                       
064300           MOVE ZERO  TO SAVE-IDBERED                                     
064400           MOVE ZERO  TO SAVE-IDFKNGRP                                    
064500       END-IF                                                             
064600       MOVE WS-KDERS TO RESP-KDERS (RESP-IX)                              
064700                                                                          
064800       MOVE 'Q2' TO W-KDEMBAL                                             
064900       PERFORM IMS-GNP-WDK6-EMB-KVAL                                      
065000       IF SEGMENT-EXIST                                                   
065100           MOVE EMB-KDEMBKOD TO RESP-KDEMBKOD-EMQ2 (RESP-IX)              
065200       ELSE                                                               
065300           MOVE SPACE        TO RESP-KDEMBKOD-EMQ2 (RESP-IX)              
065400       END-IF                                                             
065500     ELSE                                                                 
065600         MOVE NEJ TO UTDATA-SW                                            
065700     END-IF                                                               
065800     .                                                                    
065900     EJECT                                                                
066000                                                                          
066100 CB-READ-WDN6DATA-IDARTNR SECTION.                                        
066200     SKIP2                                                                
066300*    --- Here is RESP-IDARTNR-KATALOG-MASTER set.                         
066400*                                                                         
066500     PERFORM IMS-GU-WDN601                                                
066600     IF SEGMENT-EXIST                                                     
066700         MOVE JA TO RESP-IDARTNR-KATALOG-MASTER (RESP-IX)                 
066800     ELSE                                                                 
066900         MOVE NEJ TO RESP-IDARTNR-KATALOG-MASTER (RESP-IX)                
067000     END-IF                                                               
067100     .                                                                    
067200     EJECT                                                                
067300                                                                          
067400 CC-READ-WDP3-NAMN   SECTION.                                             
067500     SKIP2                                                                
067600*    --- Here is   RESP-IDNAMN-ANSK, RESP-IDNAMN-INK,                     
067700*                  RESP-IDNAMN-FORP, RESP-IDNAMN-BER  set.                
067800*                                                                         
067900     IF SAVE-IDANSK > +0                                                  
068000       MOVE 'ANSK    '  TO W-KDARBTYP                                     
068100       MOVE SAVE-IDANSK TO W-IDPERSON                                     
068200       PERFORM IMS-GU-WDP311                                              
068300       IF SEGMENT-EXIST                                                   
068400         MOVE PERS-IDNAMN  TO RESP-IDNAMN-ANSK (RESP-IX)                  
068500       ELSE                                                               
068600         MOVE SPACE        TO RESP-IDNAMN-ANSK (RESP-IX)                  
068700       END-IF                                                             
068800     ELSE                                                                 
068900       MOVE SPACE          TO RESP-IDNAMN-ANSK (RESP-IX)                  
069000     END-IF                                                               
069100                                                                          
069200     IF SAVE-IDINK  > +0                                                  
069300       MOVE 'INK     '  TO W-KDARBTYP                                     
069400       MOVE SAVE-IDINK  TO W-IDPERSON                                     
069500       PERFORM IMS-GU-WDP311                                              
069600       IF SEGMENT-EXIST                                                   
069700         MOVE PERS-IDNAMN  TO RESP-IDNAMN-INK (RESP-IX)                   
069800       ELSE                                                               
069900         MOVE SPACE        TO RESP-IDNAMN-INK (RESP-IX)                   
070000       END-IF                                                             
070100     ELSE                                                                 
070200       MOVE SPACE          TO RESP-IDNAMN-INK (RESP-IX)                   
070300     END-IF                                                               
070400                                                                          
070500     IF SAVE-IDBERED > +0                                                 
070600       MOVE 'BER     '   TO W-KDARBTYP                                    
070700       MOVE SAVE-IDBERED TO W-IDPERSON                                    
070800       PERFORM IMS-GU-WDP311                                              
070900       IF SEGMENT-EXIST                                                   
071000         MOVE PERS-IDNAMN  TO RESP-IDNAMN-BER (RESP-IX)                   
071100       ELSE                                                               
071200         MOVE SPACE        TO RESP-IDNAMN-BER (RESP-IX)                   
071300       END-IF                                                             
071400     ELSE                                                                 
071500       MOVE SPACE          TO RESP-IDNAMN-BER (RESP-IX)                   
071600     END-IF                                                               
071700                                                                          
071800                                                                          
071900     MOVE 'CDC   ' TO W-KDARBTYP                                          
072000                      W-KDARBTYP-B                                        
072100     PERFORM CCA-SOEK-IDPERSON                                            
072200                                                                          
072300     IF PERSON-EXIST                                                      
072400       MOVE WS-IDPERSON  TO W-IDPERSON                                    
072500       PERFORM IMS-GU-WDP311                                              
072600       IF SEGMENT-EXIST                                                   
072700         MOVE PERS-IDNAMN  TO RESP-IDNAMN-FORP(RESP-IX)                   
072800       ELSE                                                               
072900         MOVE SPACE        TO RESP-IDNAMN-FORP(RESP-IX)                   
073000       END-IF                                                             
073100     ELSE                                                                 
073200       MOVE SPACE          TO RESP-IDNAMN-FORP(RESP-IX)                   
073300     END-IF                                                               
073400                                                                          
073500     .                                                                    
073600     EJECT                                                                
073700                                                                          
073800 CCA-SOEK-IDPERSON SECTION.                                               
073900     SKIP2                                                                
074000     SET PERSON-MISSING TO TRUE                                           
074100                                                                          
074200     PERFORM IMS-GU-WDP3A                                                 
074300     PERFORM UNTIL SEGMENT-MISSING OR PERSON-EXIST                        
074400                                                                          
074500         IF SEQA-IDARTNR-TOM < W-IDARTNR                                  
074600             PERFORM IMS-GN-WDP3A                                         
074700         ELSE                                                             
074800             IF    W-IDARTNR >= SEQA-IDARTNR-FOM                          
074900             AND W-IDARTNR <= SEQA-IDARTNR-TOM                            
075000                 SET PERSON-EXIST TO TRUE                                 
075100             ELSE                                                         
075200                 MOVE 'GE' TO STATUS-WS                                   
075300             END-IF                                                       
075400         END-IF                                                           
075500                                                                          
075600     END-PERFORM                                                          
075700                                                                          
075800                                                                          
075900     IF PERSON-EXIST                                                      
076000        MOVE SEQA-IDPERSON TO WS-IDPERSON                                 
076100     ELSE                                                                 
076200        MOVE SAVE-IDLEVNR  TO W-IDLEVNR-B                                 
076300                                                                          
076400        PERFORM IMS-GU-WDP3B                                              
076500        IF SEGMENT-EXIST                                                  
076600           SET PERSON-EXIST   TO TRUE                                     
076700           MOVE SEQB-IDPERSON TO WS-IDPERSON                              
076800                                                                          
076900        ELSE                                                              
077000           PERFORM IMS-GU-WDP3C                                           
077100           PERFORM UNTIL SEGMENT-MISSING OR PERSON-EXIST                  
077200              or save-idfkngrp = zero                                     
077300              IF SEQC-IDFKNGRP-TOM < SAVE-IDFKNGRP                        
077400                 PERFORM IMS-GN-WDP3C                                     
077500              ELSE                                                        
077600                 IF SEQC-IDFKNGRP-FOM <= SAVE-IDFKNGRP                    
077700                 AND SEQC-IDFKNGRP-TOM >= SAVE-IDFKNGRP                   
077800                    SET PERSON-EXIST TO TRUE                              
077810                 ELSE                                                     
077820                     MOVE 'GE' TO STATUS-WS                               
077900                 END-IF                                                   
078000              END-IF                                                      
078100           END-PERFORM                                                    
078200           IF PERSON-EXIST                                                
078300              MOVE SEQC-IDPERSON TO WS-IDPERSON                           
078400                                                                          
078500           END-IF                                                         
078600        END-IF                                                            
078700     END-IF                                                               
078800     .                                                                    
078900     EJECT                                                                
079000                                                                          
079100 CD-READ-WDD2-INKOP         SECTION.                                      
079200     SKIP2                                                                
079300*    --- Here is RESP-TIINKOP (≈≈VV) or (≈≈MMDD) set.                     
079400*                                                                         
079500     PERFORM IMS-GU-WDD201                                                
079600     IF SEGMENT-EXIST                                                     
079700       MOVE WDD2-ART-IDPROJ      TO RESP-IDPROJ(RESP-IX)                  
079800       MOVE WDD2-ART-IDPROJK     TO RESP-IDPROJK(RESP-IX)                 
079900                                                                          
080000       IF WDD2-ART-TIINKOP = +111111                                      
080100          MOVE 11 TO WS-AA                                                
080200          MOVE 11 TO WS-VV                                                
080300          MOVE WS-AAVV           TO RESP-TIINKOP(RESP-IX)                 
080400       ELSE                                                               
080500          MOVE 'AAMMDD'          TO DAT-KDDATFORM                         
080600          MOVE WDD2-ART-TIINKOP  TO DAT-I-TIDATUM                         
080700                                                                          
080800          CALL WDATKONV       USING DAT-KDDATFORM  DAT-I-TIDATUM          
080900                                    DAT-O-TIDATUM DAT-KDSVAR              
081000          IF DAT-KDSVAR-OK                                                
081100             MOVE DAT-TIAA-VECKA TO WS-AA                                 
081200             MOVE DAT-TIVV       TO WS-VV                                 
081300             MOVE WS-AAVV        TO RESP-TIINKOP(RESP-IX)                 
081400          ELSE                                                            
081500             MOVE WDD2-ART-TIINKOP TO RESP-TIINKOP(RESP-IX)               
081600          END-IF                                                          
081700       END-IF                                                             
081800     ELSE                                                                 
081900       MOVE ZERO TO RESP-TIINKOP(RESP-IX)                                 
082000       MOVE SPACE TO RESP-IDPROJ(RESP-IX)                                 
082100                     RESP-IDPROJK(RESP-IX)                                
082200     END-IF                                                               
082300     .                                                                    
082400     EJECT                                                                
082500                                                                          
082600 CE-READ-WDD9-LEVBSK        SECTION.                                      
082700     SKIP2                                                                
082800*    --- Here is  RESP-TILEVBSK-AVS    (≈≈VVD)                            
082900*                 RESP-KVAVIS-BSKKVAR   9(6)    set.                      
083000*                                                                         
083100     IF SAVE-IDLEVNR = SPACE                                              
083200       MOVE ZEROES             TO RESP-TILEVBSK-AVS (RESP-IX)             
083300       MOVE ZEROES             TO RESP-KVAVIS-BSKKVAR(RESP-IX)            
083400     ELSE                                                                 
083410       MOVE W-IDARTNR          TO W-IDARTNR-D9                            
083420       MOVE WC-CDC-SE          TO W-IDDC-D9                               
083500       MOVE SAVE-IDLEVNR       TO W-IDLEVNR                               
083600       PERFORM IMS-GU-WDD924                                              
083700       IF SEGMENT-EXIST                                                   
083800          MOVE LEV-DALEVBSK-AVS   TO WS-DALEVBSK-AVS                      
083900          MOVE WS-DALEVBSK-AAMMDD TO DAT-I-TIDATUM                        
084000                                                                          
084100          MOVE 'AAMMDD'           TO DAT-KDDATFORM                        
084200          CALL WDATKONV USING DAT-KDDATFORM  DAT-I-TIDATUM                
084300                              DAT-O-TIDATUM  DAT-KDSVAR                   
084400          IF DAT-KDSVAR-OK                                                
084500             MOVE DAT-TIAAVVD     TO RESP-TILEVBSK-AVS (RESP-IX)          
084600          ELSE                                                            
084700             MOVE ZEROES          TO RESP-TILEVBSK-AVS (RESP-IX)          
084800          END-IF                                                          
084900          MOVE LEV-KVAVIS-BSKKVAR TO RESP-KVAVIS-BSKKVAR(RESP-IX)         
085000       ELSE                                                               
085100          MOVE ZEROES             TO RESP-TILEVBSK-AVS (RESP-IX)          
085200       END-IF                                                             
085300     END-IF                                                               
085400     .                                                                    
085500     EJECT                                                                
085600                                                                          
085700 CF-READ-EARLIEST-WDL2-TIAVIDAT  SECTION.                                 
085800     SKIP2                                                                
085900*    --- Here is earliest RESP-TIAVIDAT  (≈≈MMDD)                         
086000*    --- for KDRT=0  set.                                                 
086100*                                                                         
086200     MOVE ZERO   TO W-DAINLEV                                             
086300                    SAVE-TIAVIDAT-EARLIEST                                
086400                    RESP-TIAVIDAT (RESP-IX)                               
086500     PERFORM IMS-GU-WDL201                                                
086600                                                                          
086700     IF SEGMENT-EXIST                                                     
086800         PERFORM IMS-GNP-WDL211                                           
086900                                                                          
087000         PERFORM UNTIL SEGMENT-MISSING                                    
087100             MOVE INL-DAINLEV TO W-DAINLEV                                
087200             PERFORM IMS-GNP-WDL221                                       
087300                                                                          
087400             PERFORM UNTIL SEGMENT-MISSING                                
087500                 IF MOT-KDRT = ZERO                                       
087600                     IF SAVE-TIAVIDAT-EARLIEST = ZERO                     
087700                       MOVE MOT-TIAVIDAT TO SAVE-TIAVIDAT-EARLIEST        
087800                     END-IF                                               
087900*                    --- Check if TIAVIDAT is the earliest.               
088000                     MOVE MOT-TIAVIDAT           TO TMP1-YYMMDD           
088100                     MOVE SAVE-TIAVIDAT-EARLIEST TO TMP2-YYMMDD           
088200                     PERFORM WY2000P1                                     
088300                     IF TMP1-YYMMDD < TMP2-YYMMDD                         
088400                       MOVE MOT-TIAVIDAT TO SAVE-TIAVIDAT-EARLIEST        
088500                     ELSE                                                 
088600                       CONTINUE                                           
088700                     END-IF                                               
088800                 ELSE                                                     
088900                     CONTINUE                                             
089000                 END-IF                                                   
089100*                --- Read next reception report                           
089200                 PERFORM IMS-GNP-WDL221                                   
089300             END-PERFORM                                                  
089400*            --- Read next DAINLEV                                        
089500             PERFORM IMS-GNP-WDL211                                       
089600         END-PERFORM                                                      
089700         MOVE SAVE-TIAVIDAT-EARLIEST TO RESP-TIAVIDAT (RESP-IX)           
089800     ELSE                                                                 
089900         CONTINUE                                                         
090000     END-IF                                                               
090100     .                                                                    
090200     EJECT                                                                
090300                                                                          
090400 CG-READ-WDF7-FLGEMFMC      SECTION.                                      
090500*    --- LƒS WDF7  "CROSS REF FORD-VOLVO"                                 
090600     PERFORM IMS-GU-WDF701                                                
090700     IF SEGMENT-EXIST                                                     
090800       MOVE MPNR-FLGEMFMC   TO RESP-FLGEMFMC(RESP-IX)                     
090900     ELSE                                                                 
091000       MOVE '-'             TO RESP-FLGEMFMC(RESP-IX)                     
091100     END-IF                                                               
091200     .                                                                    
091300     EJECT                                                                
091400                                                                          
091500*    --- DISPATCHER SECTIONS                                              
091600 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
091700     SKIP2                                                                
091800     MOVE 'GETARG'                    TO SUB-KDFUNC                       
091900     MOVE 'CARPARTS.SPIE.DCNPARTINFO' TO SUB-ADDISPABS                    
092000     MOVE SPACE TO REQU-AREA                                              
092100     MOVE 200                         TO REQU-KVRADER-MAX                 
092200     MOVE LENGTH OF REQU-AREA         TO SUB-KVDLEN                       
092300                                                                          
092400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
092500                                                                          
092600     IF SUB-KDRC > 0                                                      
092700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
092800       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
092900       DELIMITED BY SIZE INTO ERRTEXT                                     
093000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
093100     END-IF                                                               
093200     MOVE SUB-KVDLEN TO R-IX                                              
093300     .                                                                    
093400     SKIP2                                                                
093500                                                                          
093600 S02-RETURN-RESPONSE SECTION.                                             
093700                                                                          
093800     MOVE 'RETURN'                   TO SUB-KDFUNC                        
093900     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
094000                                                                          
094100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
094200                                                                          
094300     IF SUB-KDRC > 0                                                      
094400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
094500       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
094600       DELIMITED BY SIZE INTO ERRTEXT                                     
094700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
094800     END-IF                                                               
094900     .                                                                    
095000     EJECT                                                                
095100     SKIP2                                                                
095200                                                                          
095300                                                                          
095400*    ----------------- IMS SECTIONS  -------------------------            
095500                                                                          
095600                                                                          
095700 IMS-GU-WDK6F1-IDAO-ARTIKEL SECTION.                                      
095800     MOVE 'IMS-GU-WDK6F1-IDAO-ARTIKEL' TO DLI-CALL-SECT                   
095900                                                                          
096000     STRING 'WDK6F1  (WDK6F1KY>=' W-WDK6F1KY-LO-X                         
096100                    '&WDK6F1KY<=' W-WDK6F1KY-HI-X                         
096200                    '&IDARTNR  =' W-IDARTNR-X  ')'                        
096300          DELIMITED BY SIZE INTO SSA1                                     
096400     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
096500     CALL CBLTDLI USING GU WDK6F-PCB DLI-WDK6F-AREA SSA1                  
096600     MOVE WDK6F-STATUS-CODE TO STATUS-WS                                  
096700     PERFORM IMS-STATUSKONTROLL                                           
096800     .                                                                    
096900     SKIP2                                                                
097000                                                                          
097100 IMS-GU-WDK6F1-IDAO         SECTION.                                      
097200     MOVE 'IMS-GU-WDK6F1-IDAO           ' TO DLI-CALL-SECT                
097300                                                                          
097400     STRING 'WDK6F1  (WDK6F1KY>=' W-WDK6F1KY-LO-X                         
097500                    '&WDK6F1KY<=' W-WDK6F1KY-HI-X ')'                     
097600          DELIMITED BY SIZE INTO SSA1                                     
097700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
097800     CALL CBLTDLI USING GU WDK6F-PCB DLI-WDK6F-AREA SSA1                  
097900     MOVE WDK6F-STATUS-CODE TO STATUS-WS                                  
098000     PERFORM IMS-STATUSKONTROLL                                           
098100     .                                                                    
098200     EJECT                                                                
098300 IMS-GN-WDK6F1-IDAO         SECTION.                                      
098400     MOVE 'IMS-GN-WDK6F1-IDAO           ' TO DLI-CALL-SECT                
098500                                                                          
098600     STRING 'WDK6F1  (WDK6F1KY>=' W-WDK6F1KY-LO-X                         
098700                    '&WDK6F1KY<=' W-WDK6F1KY-HI-X ')'                     
098800          DELIMITED BY SIZE INTO SSA1                                     
098900     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
099000     CALL CBLTDLI USING GN WDK6F-PCB DLI-WDK6F-AREA SSA1                  
099100     MOVE WDK6F-STATUS-CODE TO STATUS-WS                                  
099200     PERFORM IMS-STATUSKONTROLL                                           
099300     .                                                                    
099400     EJECT                                                                
099500                                                                          
099600 IMS-GU-WDK601 SECTION.                                                   
099700     MOVE 'IMS-GU-WDK601' TO DLI-CALL-SECT                                
099800                                                                          
099900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
100000          DELIMITED BY SIZE INTO SSA1                                     
100100     MOVE '  GE' TO GOOD-STATUSCODES                                      
100200     CALL CBLTDLI USING GU WDK6-PCB DLI-WDK601-AREA SSA1                  
100300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
100400     PERFORM IMS-STATUSKONTROLL                                           
100500     .                                                                    
100600     SKIP2                                                                
100700                                                                          
100800 IMS-GNP-WDK611 SECTION.                                                  
100900     MOVE 'IMS-GNP-WDK611' TO DLI-CALL-SECT                               
101000                                                                          
101100     MOVE 'WDK611  ' TO SSA1                                              
101200     MOVE '  GE' TO GOOD-STATUSCODES                                      
101300     CALL CBLTDLI USING GNP WDK6-PCB DLI-WDK611-AREA SSA1                 
101400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
101500     PERFORM IMS-STATUSKONTROLL                                           
101600     .                                                                    
101700     SKIP2                                                                
101800                                                                          
101900 IMS-GNP-WDK6-EMB-KVAL SECTION.                                           
102000     MOVE 'IMS-GNP-WDK6-EMB-KVAL' TO DLI-CALL-SECT                        
102100                                                                          
102200     STRING 'WDK613  (KDEMBAL  =' W-KDEMBAL-X ')'                         
102300          DELIMITED BY SIZE INTO SSA1                                     
102400     MOVE '  GE' TO GOOD-STATUSCODES                                      
102500     CALL CBLTDLI USING GNP WDK6-PCB DLI-WDK613-AREA SSA1                 
102600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
102700     PERFORM IMS-STATUSKONTROLL                                           
102800     .                                                                    
102900     EJECT                                                                
103000                                                                          
103100 IMS-GNP-WDK623-SISTA  SECTION.                                           
103200     MOVE 'IMS-GNP-WDK623-SISTA' TO DLI-CALL-SECT                         
103300*                                                                         
103400     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA1                                 
103500     STRING 'WDK623  *L '                                                 
103600             DELIMITED BY SIZE INTO SSA2                                  
103700     MOVE '  GE' TO GOOD-STATUSCODES                                      
103800     CALL CBLTDLI USING GNP WDK6-PCB DLI-WDK623-AREA SSA1 SSA2            
103900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
104000     PERFORM IMS-STATUSKONTROLL                                           
104100     .                                                                    
104200     SKIP3                                                                
104300                                                                          
104400 IMS-GU-WDN601 SECTION.                                                   
104500     MOVE 'IMS-GU-WDN601' TO DLI-CALL-SECT                                
104600                                                                          
104700     STRING 'WDN601  (IDARTNR  =' W-IDARTNR-X ')'                         
104800          DELIMITED BY SIZE INTO SSA1                                     
104900     MOVE '  GE' TO GOOD-STATUSCODES                                      
105000     CALL CBLTDLI USING GU WDN6-PCB DLI-WDN601-AREA SSA1                  
105100     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
105200     PERFORM IMS-STATUSKONTROLL                                           
105300     .                                                                    
105400     EJECT                                                                
105500                                                                          
105600 IMS-GU-WDP3A SECTION.                                                    
105700     MOVE 'IMS-GU-WDP3A' TO DLI-CALL-SECT                                 
105800     STRING 'WDP3A1  (WDP3A1KY=>' W-WDP3A1-MIN                            
105900                    '&WDP3A1KY=<' W-WDP3A1-MAX                            
106000                    '&KDARBTYP =' W-KDARBTYP ')'                          
106100            DELIMITED BY SIZE INTO SSA1                                   
106200     MOVE '  GE' TO GOOD-STATUSCODES                                      
106300     CALL CBLTDLI USING GU WDP3A-PCB DLI-WDP3A-AREA SSA1                  
106400     MOVE WDP3A-STATUS-CODE TO STATUS-WS                                  
106500     PERFORM IMS-STATUSKONTROLL                                           
106600     .                                                                    
106700     SKIP2                                                                
106800                                                                          
106900 IMS-GN-WDP3A SECTION.                                                    
107000     MOVE 'IMS-GN-WDP3A' TO DLI-CALL-SECT                                 
107100                                                                          
107200     STRING 'WDP3A1  (WDP3A1KY=>' W-WDP3A1-MIN                            
107300                    '&WDP3A1KY=<' W-WDP3A1-MAX                            
107400                    '&KDARBTYP =' W-KDARBTYP ')'                          
107500            DELIMITED BY SIZE INTO SSA1                                   
107600     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
107700     CALL CBLTDLI USING GN WDP3A-PCB DLI-WDP3A-AREA SSA1                  
107800     MOVE WDP3A-STATUS-CODE TO STATUS-WS                                  
107900     PERFORM IMS-STATUSKONTROLL                                           
108000     .                                                                    
108100     SKIP2                                                                
108200                                                                          
108300 IMS-GU-WDP3B SECTION.                                                    
108400     MOVE 'IMS-GU-WDP3B' TO DLI-CALL-SECT                                 
108500                                                                          
108600     STRING 'WDP3B1  (WDP3B1KY =' W-WDP3B1-X                              
108700                    '&KDARBTYP =' W-KDARBTYP ')'                          
108800            DELIMITED BY SIZE INTO SSA1                                   
108900     MOVE '  GE' TO GOOD-STATUSCODES                                      
109000     CALL CBLTDLI USING GU WDP3B-PCB DLI-WDP3B-AREA SSA1                  
109100     MOVE WDP3B-STATUS-CODE TO STATUS-WS                                  
109200     PERFORM IMS-STATUSKONTROLL                                           
109300     .                                                                    
109400     SKIP2                                                                
109500                                                                          
109600 IMS-GU-WDP3C SECTION.                                                    
109700     MOVE 'IMS-GU-WDP3C' TO DLI-CALL-SECT                                 
109800                                                                          
109900     STRING 'WDP3C1  (WDP3C1KY=>' W-WDP3C1-MIN                            
110000                    '&WDP3C1KY=<' W-WDP3C1-MAX ')'                        
110100            DELIMITED BY SIZE INTO SSA1                                   
110200     MOVE '  GE' TO GOOD-STATUSCODES                                      
110300     CALL CBLTDLI USING GU WDP3C-PCB DLI-WDP3C-AREA SSA1                  
110400     MOVE WDP3C-STATUS-CODE TO STATUS-WS                                  
110500     PERFORM IMS-STATUSKONTROLL                                           
110600     .                                                                    
110700     SKIP2                                                                
110800                                                                          
110900 IMS-GN-WDP3C SECTION.                                                    
111000     MOVE  'IMS-GN-WDP3C' TO DLI-CALL-SECT                                
111100                                                                          
111200     STRING 'WDP3C1  (WDP3C1KY=>' W-WDP3C1-MIN                            
111300                    '&WDP3C1KY=<' W-WDP3C1-MAX ')'                        
111400            DELIMITED BY SIZE INTO SSA1                                   
111500     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
111600     CALL CBLTDLI USING GN WDP3C-PCB DLI-WDP3C-AREA SSA1                  
111700     MOVE WDP3C-STATUS-CODE TO STATUS-WS                                  
111800     PERFORM IMS-STATUSKONTROLL                                           
111900     .                                                                    
112000     SKIP2                                                                
112100                                                                          
112200 IMS-GU-WDP311      SECTION.                                              
112300     MOVE 'IMS-GU-WDP311' TO DLI-CALL-SECT                                
112400                                                                          
112500     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
112600          DELIMITED BY SIZE INTO SSA1                                     
112700     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
112800          DELIMITED BY SIZE INTO SSA2                                     
112900     MOVE '  GE' TO GOOD-STATUSCODES                                      
113000     CALL CBLTDLI USING GU  WDP3-PCB                                      
113100                            DLI-WDP311-AREA                               
113200                            SSA1 SSA2                                     
113300     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
113400     PERFORM IMS-STATUSKONTROLL                                           
113500     .                                                                    
113600     SKIP2                                                                
113700                                                                          
113800 IMS-GU-WDD201      SECTION.                                              
113900     MOVE 'IMS-GU-WDD201' TO DLI-CALL-SECT                                
114000                                                                          
114100     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
114200            DELIMITED BY SIZE INTO SSA1                                   
114300     MOVE '  GE'   TO GOOD-STATUSCODES                                    
114400     CALL CBLTDLI USING GU WDD2-PCB DLI-WDD201-AREA SSA1                  
114500     MOVE   WDD2-STATUS-CODE TO STATUS-WS                                 
114600     PERFORM IMS-STATUSKONTROLL                                           
114700     .                                                                    
114800     EJECT                                                                
114900                                                                          
115000 IMS-GU-WDD924   SECTION.                                                 
115100     MOVE 'IMS-GU-WDD924' TO DLI-CALL-SECT                                
115200                                                                          
115300     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
115400            DELIMITED BY SIZE INTO SSA1                                   
115500     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
115600            DELIMITED BY SIZE INTO SSA2                                   
115700     MOVE   'WDD924    '        TO SSA3                                   
115800     MOVE '  GE' TO GOOD-STATUSCODES                                      
115900     CALL CBLTDLI USING GU WDD9-PCB DLI-WDD924-AREA SSA1 SSA2 SSA3        
116000     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
116100     PERFORM IMS-STATUSKONTROLL                                           
116200     .                                                                    
116300*                                                                         
116400     EJECT                                                                
116500 IMS-GU-WDL201    SECTION.                                                
116600     MOVE 'IMS-GU-WDL201' TO DLI-CALL-SECT                                
116700                                                                          
116800     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
116900             DELIMITED BY SIZE INTO SSA1                                  
117000     MOVE '  GE' TO GOOD-STATUSCODES                                      
117100     CALL CBLTDLI USING GU WDL2-PCB DLI-WDL201-AREA SSA1                  
117200     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
117300     PERFORM IMS-STATUSKONTROLL                                           
117400     .                                                                    
117500     SKIP3                                                                
117600 IMS-GNP-WDL211    SECTION.                                               
117700     MOVE 'IMS-GNP-WDL211' TO DLI-CALL-SECT                               
117800                                                                          
117900     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
118000             DELIMITED BY SIZE INTO SSA1                                  
118100     STRING 'WDL211  (DAINLEV =>' W-DAINLEV-X ')'                         
118200             DELIMITED BY SIZE INTO SSA2                                  
118300     MOVE '  GE' TO GOOD-STATUSCODES                                      
118400     CALL CBLTDLI USING GNP WDL2-PCB DLI-WDL211-AREA SSA1 SSA2            
118500     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
118600     PERFORM IMS-STATUSKONTROLL                                           
118700     .                                                                    
118800     EJECT                                                                
118900 IMS-GNP-WDL221    SECTION.                                               
119000     MOVE 'IMS-GNP-WDL221' TO DLI-CALL-SECT                               
119100                                                                          
119200     STRING 'WDL211  (DAINLEV  =' W-DAINLEV-X ')'                         
119300             DELIMITED BY SIZE INTO SSA1                                  
119400     MOVE   'WDL221  ' TO SSA2                                            
119500     MOVE '  GE' TO GOOD-STATUSCODES                                      
119600     CALL CBLTDLI USING GNP WDL2-PCB DLI-WDL221-AREA SSA1 SSA2            
119700     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
119800     PERFORM IMS-STATUSKONTROLL                                           
119900     .                                                                    
120000     SKIP3                                                                
120100                                                                          
120200 IMS-GU-WDF701 SECTION.                                                   
120300     STRING 'WDF701  (WDF701KY =' W-WDF701KY-X ')'                        
120400            DELIMITED BY SIZE INTO SSA1                                   
120500     MOVE '  GE' TO GOOD-STATUSCODES                                      
120600     CALL CBLTDLI USING GU WDF7-PCB DLI-IO-AREA-7 SSA1                    
120700     MOVE WDF7-STATUS-CODE TO STATUS-WS                                   
120800     PERFORM IMS-STATUSKONTROLL                                           
120900     .                                                                    
121000     SKIP3                                                                
121100 IMS-STATUSKONTROLL SECTION.                                              
121200                                                                          
121300     SET STATUS-IX TO 1                                                   
121400     SEARCH GOOD-STATUS                                                   
121500       AT END                                                             
121600         STRING 'WRONG STATUS CODE FROM IMS: ' STATUS-WS                  
121700         ' IN SECT. ' DLI-CALL-SECT                                       
121800         DELIMITED BY SIZE INTO ERRTEXT                                   
121900         CALL FELLOG                                                      
122000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
122100         CONTINUE                                                         
122200     END-SEARCH                                                           
122300     .                                                                    
122400*    ----------------- Y2K SECTIONS  -------------------------            
122500     EJECT                                                                
122600*    -COPY WY2000P1                                                       
