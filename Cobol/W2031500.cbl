000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2031500.                                                
000400 AUTHOR.         ARVIDSSON LENA.                                          
000500 DATE-WRITTEN.   03/03/27.                                                
000600 DATE-COMPILED.                                                           
000700*                                                                         
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        MPP PROGRAM, VIEW CAMPAIGN INFORMATION PER PARTNUMBER.           
001100*        POSSIBLE TO SELECT CAMPAIGNNO OR CAMPAIGNGROUP                   
001200*        AND THROUGH PF-KEYS JUMP TO PAGE 2313, 2314 AND 2316.            
001300*                                                                         
001400*        THE PROGRAM READS   TABLE TP1KAMP                                
001500*        THE PROGRAM READS   TABLE TP1ARTK                                
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSACTION: W2T315                                              
001900*        MID:         W2I31501                                            
002000*                                                                         
002100*    OUTDATA.                                                             
002200*        MOD:         W2O31501                                            
002300*                                                                         
002400**-------------------------------------------------------------           
002500* CORRECTIONS.                                                            
002600* 2015-04-21   E'TRACKER 10254063 RÄTTA BILD 2315 SOM ABENDAR.            
002700*                                                                         
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 DATA DIVISION.                                                           
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300     SKIP2                                                                
003400*    -COPY WY2000W1                                                       
003500     SKIP3                                                                
003600 77  IDPGM                       PIC X(08)   VALUE 'W2031500'.            
003700                                                                          
003800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300                                                                          
004400 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004500*    --- INDEX FOR SCROLL LINES                                           
004600 77  INDX                        PIC S9(4)   VALUE +0    COMP-3.          
004700 77  INDX-SPARA                  PIC S9(4)   VALUE +0    COMP-3.          
004800 77  MAX-INDX                    PIC S9(4)   VALUE +13   COMP-3.          
005000 77  IX                          PIC 9(9)    VALUE ZERO.                  
005100 77  IX2                         PIC 9(9)    VALUE ZERO.                  
005200 77  RAD-IX                      PIC 9(9)    VALUE ZERO.                  
005300 77  RAD-MAX                     PIC 9(9)    VALUE 13.                    
005400 77  IX-SISTA-POST               PIC 9(9)    VALUE ZERO.                  
005500*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005600                                                                          
005700 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005800     88  ALLT-OK                             VALUE 'J'.                   
005900     88  ALLT-EJ                             VALUE 'N'.                   
006000                                                                          
006100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006200     88  INDATA-OK                           VALUE 'J'.                   
006300     88  INDATA-FEL                          VALUE 'N'.                   
006400                                                                          
006410 77  IDTRANS-HOPP-SW             PIC X       VALUE 'N'.                   
006420     88  IDTRANS-HOPP-IFYLLT                 VALUE 'J'.                   
006430     88  IDTRANS-HOPP-SAKNAS                 VALUE 'N'.                   
006440                                                                          
006500 77  JUMP-SW                     PIC X       VALUE 'J'.                   
006600     88  JUMP-OK                             VALUE 'J'.                   
006700     88  NO-JUMP                             VALUE 'N'.                   
006800                                                                          
006900 77  KEYS-SW                     PIC X       VALUE 'J'.                   
007000     88  KEYS-OK                             VALUE 'J'.                   
007100     88  KEYS-WRONG                          VALUE 'N'.                   
007200                                                                          
007300 77  BILD-SW                     PIC X       VALUE 'J'.                   
007400     88  BILD-OK                             VALUE 'J'.                   
007500                                                                          
007600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007700     88  OWN-MID                             VALUE '2315'.                
007710     88  2317-MID                            VALUE '2317'.                
007800     88  GOOD-MID                            VALUE '2313' '2314'          
007900                                                   '2315' '2316'.         
008000     88  HELP-MID                            VALUE '0551'.                
008100                                                                          
008200 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
008300                                                                          
008400 01  WS-DATUM                    PIC 9(8)    VALUE ZERO.                  
008500 01  FILLER   REDEFINES WS-DATUM.                                         
008600     03 WS-DATUM-SEKEL           PIC 9(2).                                
008700     03 WS-DATUM-AAR             PIC 9(2).                                
008800     03 WS-DATUM-MAN             PIC 9(2).                                
008900     03 WS-DATUM-DAG             PIC 9(2).                                
009000     EJECT                                                                
009100                                                                          
009200 01  WS-SEKEL-KONTROLL           PIC 9(6).                                
009300 01  FILLER REDEFINES WS-SEKEL-KONTROLL.                                  
009400     03 WS-SEKEL-KONTR           PIC 9(2).                                
009500     03 FILLER                   PIC 9(4).                                
009600                                                                          
009700 01  WS.                                                                  
009800     03 WS-SECTION               PIC X(24)   VALUE SPACE.                 
009900     03 FILLER                   PIC X(16)   VALUE                        
010000                                             'WS-DB2-SEKTION'.            
010100     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
010200****************************************************************          
010300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
010400 01  GENERAL-SUBPROGRAMS.                                                 
010500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011000     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
011100     EJECT                                                                
011200*    --- PARAMETERS FOR SUBPROGRAM WDATKONV                               
011300*01 -COPY WDATAREA                                                        
011400*                                                                         
011500*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
011600*01 -COPY WMEDAREA                                                        
011700     SKIP3                                                                
011800 01  MESSAGE-CODES.                                                       
011900     03  INF-FIRST-PAGE               PIC X(3)    VALUE '006'.            
012000     03  INF-MORE-INFO-EXISTS         PIC X(3)    VALUE '105'.            
012100     03  ERR-WRONG-KEY                PIC X(3)    VALUE '401'.            
012200     03  ERR-CORR-HILITE-FLDS         PIC X(3)    VALUE '001'.            
012300     03  ERR-PF11-AND-NO-DATA         PIC X(3)    VALUE '011'.            
012400     03  PART-MISSING                 PIC X(3)    VALUE '017'.            
012500     03  FIELDS-ARE-NOT-NUMERIC       PIC X(3)    VALUE '020'.            
012600     03  AREA-MISSING                 PIC X(3)    VALUE '705'.            
012700     03  ZERO-NOT-ALLOWED             PIC X(3)    VALUE '724'.            
012800     03  INFORMATION-MISSING          PIC X(3)    VALUE '760'.            
012900     03  CHANGE-SCREENS               PIC X(3)    VALUE '127'.            
013000     EJECT                                                                
013100 01  FELTEXTER.                                                           
013200     03  MED-1                        PIC X(40)                           
013300         VALUE 'SELECT ROW, SCREEN (2313,2314 OR 2316)  '.                
013400                                                                          
013500*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
013600*01 -COPY WMSGINIT                                                        
013700     EJECT                                                                
013800*                                                                         
013900*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
014000*                                                                         
014100 01  SAVE-AREA.                                                           
014200     03  SAVE-IDTRANS                 PIC X(4)    VALUE '2315'.           
014300     03  WS-MSGI-SSA-KEY-ENTER.                                           
014400       05 SAVE-RADNR-ENTER            PIC 9(3)    VALUE ZERO.             
014500     03  WS-MSGI-SSA-KEY-NEXT.                                            
014600       05 SAVE-RADNR-NEXT             PIC 9(3)    VALUE ZERO.             
014700     03  SAVE-TABELL.                                                     
014800       05  TABELL OCCURS 13.                                              
014900         07 SAVE-IDKAMP               PIC X(7)    VALUE SPACE.            
015000         07 SAVE-IDKAMP-GRP           PIC X(7)    VALUE SPACE.            
015100         07 SAVE-TISTADAT-KAMP        PIC 9(6)    VALUE ZERO.             
015200         07 SAVE-TISTODAT-KAMP        PIC 9(6)    VALUE ZERO.             
015300         07 SAVE-KDKAMP               PIC X       VALUE SPACE.            
015400     EJECT                                                                
015500*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
015600******************************************************************        
015700* EFTERSOM WINSTOR INTE KLARAR ATT SORTERA I FALLANDE ORDNING             
015800* KRÄVS ATT VI I PROGRAMMET SORTERAR OM TABELLEN EFTER                    
015900* ANROPET AV WINTSOR                                                      
016000* OBS WS-TAB-POST-SORT MÅSTE HA SAMMA LÄNGD SOM WS-TAB-POST-RAD           
016100*     JMF STEGLANGD                                                       
016200******************************************************************        
016300 01  TABENTRY-PARM.                                                       
016400     03  STEGLANGD                    PIC S9(9) COMP  VALUE 49.           
016500     03  ANTAL                        PIC S9(9) COMP.                     
016600     03  NYCKELLANGD                  PIC S9(9) COMP  VALUE 22.           
016700                                                                          
016800*    --- WS AREA FOR TABLE AND SORTING                                    
016900                                                                          
017000 01  WS-TAB.                                                              
017100     03  WS-TABELL-MAX                PIC 9(3)    VALUE 200.              
017200     03  WS-TABELL.                                                       
017300      04 WS-TAB-POST    OCCURS 200.                                       
017400       05  WS-TAB-POST-RAD.                                               
017500        06 WS-TAB-RAD.                                                    
017600         07 WS-IDKAMP-TAB             PIC X(7).                           
017700         07 WS-IDKAMP-GRP-TAB         PIC 9(7).                           
017800         07 WS-TISTADAT-KAMP-TAB      PIC 9(6).                           
017900         07 WS-TISTODAT-KAMP-TAB      PIC 9(6).                           
018000         07 WS-KDKAMP-TAB             PIC X.                              
018100        06  WS-TAB-SORT.                                                  
018200         07 WS-TISTADAT-SORT          PIC 9(8).                           
018300         07 WS-IDKAMP-SORT            PIC X(7).                           
018400         07 WS-IDKAMP-GRP-SORT        PIC 9(7).                           
018500******************************************************************        
018600* EFTERSOM WINSTOR INTE KLARAR ATT SORTERA I FALLANDE ORDNING             
018700* KRÄVS ATT VI I PROGRAMMET SORTERAR OM TABELLEN EFTER                    
018800* ANROPET AV WINTSOR                                                      
018900* OBS WS-TAB-POST-SORT MÅSTE HA SAMMA LÄNGD SOM WS-TAB-POST-RAD           
019000*     JMF STEGLANGD                                                       
019100******************************************************************        
019200 01  WS-TAB-RED.                                                          
019300     03  WS-TAB-POST-SORT OCCURS 200                                      
019400                                   PIC X(49).                             
019500******************************************************************        
019600 01  W-IDARTNR                   PIC S9(9)  VALUE ZERO COMP-3.            
019700 01  W-BILD                      PIC X(4)   VALUE SPACE.                  
019800 01  W-IDKAMP                    PIC X(7)   VALUE SPACE.                  
019900 01  W-IDKAMP-GRP                PIC X(7)   VALUE SPACE.                  
020000 01  W-TISTADAT-KAMP             PIC 9(6)   VALUE ZERO.                   
020100 01  W-TISTODAT-KAMP             PIC 9(6)   VALUE ZERO.                   
020200 01  W-KDKAMP                    PIC X      VALUE SPACE.                  
020300                                                                          
020400 01  WS-TISTODAT-KAMP            PIC 9(6)   VALUE ZERO.                   
020500                                                                          
020600*    --- FIELD FOR JUMP TO AN OTHER SCREEN                                
020700   77  SW-STARTA-ANNAN-BILD      PIC X      VALUE 'N'.                    
020800     88  STARTA-ANNAN-BILD                  VALUE 'J'.                    
020900                                                                          
021000 01  BILD-HOPP-AREOR.                                                     
021100                                                                          
021200   03    W-BILD-HOPP             PIC X(4)   VALUE SPACE.                  
021300   03    W-HOPP-IDTRANS.                                                  
021400     05  FILLER                  PIC X(1)   VALUE 'W'.                    
021500     05  W-HOPP-IDTRANS-2        PIC X(1).                                
021600     05  FILLER                  PIC X(1)   VALUE 'T'.                    
021700     05  W-HOPP-IDTRANS-4-6      PIC X(3).                                
021800     05  FILLER                  PIC X(2)   VALUE SPACE.                  
021900                                                                          
022000                                                                          
022100   03 FILLER                     PIC X(16)  VALUE 'P-TO-P-AREA'.          
022200   03    P-TO-P-SW.                                                       
022300                                                                          
022400     05  P-TO-P-KVLL             PIC S9(4)  VALUE +117 COMP SYNC.         
022500     05  P-TO-P-KDZ1             PIC X(1)   VALUE LOW-VALUE.              
022600     05  P-TO-P-KDZ2             PIC X(1)   VALUE LOW-VALUE.              
022700     05  P-TO-P-KDTRANS          PIC X(8).                                
022800     05  P-TO-P-IDTRANS          PIC X(4).                                
022900     05  P-TO-P-KDMFSFOR         PIC X(1).                                
023000     05  P-TO-P-DATA             PIC X(100) VALUE ALL '+'.                
023100                                                                          
023200*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
023300*                                                                         
023400 01  FILLER                      PIC X(16)  VALUE 'MID-AREA'.             
023500     SKIP3                                                                
023600*01  MID -COPY W2I31501                                                   
023700     EJECT                                                                
023800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
023900     SKIP3                                                                
024000*01  -COPY WMSGAREA                                                       
024100     EJECT                                                                
024200     03  MOD REDEFINES MSG-AREA.                                          
024300*      05  -COPY W2O31501                                                 
024400     EJECT                                                                
024500 01  FILLER                      PIC X(16)  VALUE 'MFS-AREA'.             
024600     SKIP3                                                                
024700*01  -COPY WMFSAREA                                                       
024800     EJECT                                                                
024900*    --- WORK-AREAS FOR IMS-SECTIONS                                      
025000*                                                                         
025100 01  FILLER                      PIC X(16)  VALUE 'IMS-WS'.               
025200     SKIP3                                                                
025300 01  KEYS-TO-DLI.                                                         
025400*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
025500     03  W-IDKAMP-MIN-X.                                                  
025600         05  W-IDKAMP-MIN        PIC X(7).                                
025700                                                                          
025800     SKIP2                                                                
025900*    --- STATUS-CODE FROM IMS                                             
026000 01  STATUS-WS                   PIC XX.                                  
026100     88  SEGMENT-FOUND                      VALUE '  '.                   
026200     88  SEGMENT-FOUND-EXISTS               VALUE 'II'.                   
026300     88  SEGMENT-MISSING                    VALUE 'GE'.                   
026400     88  TRANSKOD-FEL                       VALUE 'A1'.                   
026500     88  SECURITY-FEL                       VALUE 'A4'.                   
026600     SKIP2                                                                
026700 01  GOOD-STATUSCODES.                                                    
026800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026900     SKIP3                                                                
027000 01  SSA1                        PIC X(64).                               
027100 01  SSA2                        PIC X(64).                               
027200     EJECT                                                                
027300*    --- IMS FUNCTION CODES                                               
027400*                                                                         
027500*01  -COPY W0003                                                          
027600     EJECT                                                                
027700 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
027800       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
027900                                                                          
028000 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
028100 01  DB2-WS.                                                              
028200     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
028300         88  CURSOR-OK                      VALUE 000.                    
028400         88  LINES-FOUND                    VALUE 000.                    
028500         88  LINES-MISSING                  VALUE 100.                    
028600         88  RESOURCE-WRONG                 VALUE 904.                    
028700     03  GOOD-SQLCODECODES.                                               
028800         05  GOOD-SQLCODE OCCURS 5                                        
028900             INDEXED BY SQLCODE-IX PIC 9(3).                              
029000     EJECT                                                                
029100*    ---  DLI INPUT-OUTPUT AREA                                           
029200                                                                          
029300     EJECT                                                                
029400 01  FILLER                      PIC X(16)  VALUE 'TP1KAMP-AREA'.         
029500                                                                          
029600*01  -COPY TP1KAMP -PRE TP1KAMP-                                          
029700     EJECT                                                                
029800 01  FILLER                      PIC X(16)  VALUE 'TP1ARTK-AREA'.         
029900                                                                          
030000*01  -COPY TP1ARTK -PRE TP1ARTK-                                          
030100     EJECT                                                                
030200     EXEC SQL INCLUDE TP1KAMP END-EXEC.                                   
030300     EJECT                                                                
030400     EXEC SQL INCLUDE TP1ARTK END-EXEC.                                   
030500     EJECT                                                                
030600 LINKAGE SECTION.                                                         
030700*01  -COPY W0009   -PRE MSG-                                              
030800     EJECT                                                                
030900*01  -COPY W0009   -PRE ALT-                                              
031000     EJECT                                                                
031100*01  -COPY W0008   -PRE USEA-                                             
031200     05  FILLER                  PIC X.                                   
031300     EJECT                                                                
031400                                                                          
031500 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB.                      
031600 MAIN SECTION.                                                            
031700     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB.                      
031800                                                                          
031900     PERFORM IMS-GET-MSG                                                  
032000     IF SEGMENT-FOUND                                                     
032100       PERFORM A-INIT                                                     
032200       PERFORM B-CHECK-KEYS                                               
032300       IF KEYS-OK                                                         
032400         IF MFS-SPLIT                                                     
032500           PERFORM J-JUMP-PAGE                                            
032600         ELSE                                                             
032700           IF MFS-FIRST                                                   
032800             PERFORM C-FIRST-PAGE                                         
032900           ELSE                                                           
033000             IF MFS-NEXT                                                  
033100               PERFORM D-NEXT-PAGE                                        
033200             ELSE                                                         
033300               PERFORM E-SAME-PAGE                                        
033400             END-IF                                                       
033500           END-IF                                                         
033600         END-IF                                                           
033700         IF STARTA-ANNAN-BILD                                             
033800           CONTINUE                                                       
033900         ELSE                                                             
034000           IF INDATA-OK                                                   
034100             PERFORM F-READ-SHOW-INFO                                     
034200           END-IF                                                         
034300         END-IF                                                           
034400       END-IF                                                             
034500                                                                          
034600       IF STARTA-ANNAN-BILD                                               
034700         MOVE ALL '+'      TO MSGI-WMSGINIT                               
034800         MOVE '001'        TO MSGI-KDCALL                                 
034900         MOVE MSG-SIGNON-USERID                                           
035000                           TO MSGI-IDUSER                                 
035100         MOVE MSG-LTERM-NAME                                              
035200                           TO MSGI-IDLTERM-USER                           
035300         MOVE '2315'       TO MSGI-IDTRANS                                
035400         MOVE MID-IDKAMP  (INDX)                                          
035500                           TO MSGI-IDKAMP                                 
035600         MOVE MID-IDKAMP-GRP  (INDX)                                      
035700                           TO MSGI-IDKAMP-GRP                             
035800         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
035900                                                                          
036000       ELSE                                                               
036100         MOVE ALL '+'      TO MSGI-WMSGINIT                               
036200         MOVE '001'        TO MSGI-KDCALL                                 
036300         MOVE MSG-SIGNON-USERID                                           
036400                           TO MSGI-IDUSER                                 
036500         MOVE MSG-LTERM-NAME                                              
036600                           TO MSGI-IDLTERM-USER                           
036700         MOVE '2314'       TO MSGI-IDTRANS                                
036800         MOVE W-IDARTNR    TO MSGI-IDARTNR                                
036900         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
037000       END-IF                                                             
037100                                                                          
037200       IF STARTA-ANNAN-BILD                                               
037300         CONTINUE                                                         
037400       ELSE                                                               
037500         COMPUTE MSG-KVLL = LENGTH OF MOD-W2O31501-CTX + 4                
037700         PERFORM IMS-INSERT-MSG                                           
037800       END-IF                                                             
037900     END-IF                                                               
038000                                                                          
038100     MOVE ZERO TO RETURN-CODE                                             
038200     GOBACK                                                               
038300     .                                                                    
038400     EJECT                                                                
038500***************************************************                       
038600*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
038700*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
038800*      COMPUTE XXXX       LENGTH OF MID-W2I31501 + 17                     
038900*     + LÄGG TILL IF JUMP = YES                                           
039000 A-INIT SECTION.                                                          
039010     MOVE 'A-INIT    '   TO WS-SECTION                                    
039100                                                                          
039200     IF MSG-DOUBLE-TRANSACTIONS                                           
039300       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W2I31501-CTX             
039400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
039500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
039600     ELSE                                                                 
039700       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W2I31501-CTX              
039800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
039900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
040000     END-IF                                                               
040100                                                                          
040200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
040300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
040400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
040500                                                                          
040600     MOVE LOW-VALUE TO MSG-AREA                                           
040700     MOVE 'W2O315N1' TO MFS-IDMOD                                         
040800     MOVE '2315' TO MOD-IDTRANS                                           
040900     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
041000                                                                          
041100     IF OWN-MID OR HELP-MID                                               
041200       CONTINUE                                                           
041300     ELSE                                                                 
041400       MOVE SPACE TO MFS-KDTRTYP                                          
041500       MOVE '7' TO MFS-IDPFK                                              
041600     END-IF                                                               
041700                                                                          
041800     MOVE 'GB'  TO MED-IDSKYLT                                            
041900                                                                          
042000     INITIALIZE GOOD-SQLCODECODES                                         
042100                                                                          
042200     MOVE FUNCTION  CURRENT-DATE(1:8)  TO DAGENS-DATUM                    
042300     MOVE 1                  TO IX                                        
042400     PERFORM UNTIL IX > WS-TABELL-MAX                                     
042500                                                                          
042600        MOVE SPACE           TO WS-IDKAMP-TAB        (IX)                 
042700                                WS-IDKAMP-SORT       (IX)                 
042800                                WS-KDKAMP-TAB        (IX)                 
042900        MOVE ZERO            TO WS-IDKAMP-GRP-TAB    (IX)                 
043000                                WS-IDKAMP-GRP-SORT   (IX)                 
043100                                WS-TISTADAT-KAMP-TAB (IX)                 
043200                                WS-TISTODAT-KAMP-TAB (IX)                 
043300                                WS-TISTADAT-SORT     (IX)                 
043400                                                                          
043500        ADD 1                TO IX                                        
043600     END-PERFORM                                                          
043700     .                                                                    
043800     EJECT                                                                
043900 B-CHECK-KEYS SECTION.                                                    
043910     MOVE 'B-CHECK-KEYS '  TO WS-SECTION                                  
044000                                                                          
044100     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
044200                                WS-MSGI-SSA-KEY-NEXT                      
044300     MOVE ALL '+'            TO MSGI-WMSGINIT                             
044400     MOVE '001'              TO MSGI-KDCALL                               
044500     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
044600     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
044700     MOVE '2315'             TO MSGI-IDTRANS                              
044800     IF GOOD-MID OR 2317-MID                                              
044900         MOVE MID-IDARTNR    TO MSGI-IDARTNR                              
045000     END-IF                                                               
045100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
045200     MOVE MSGI-SPAR-AREA     TO SAVE-AREA                                 
045300                                                                          
045400*    - LANGUAGE TO BE USED BY MEDKONV                                     
045500     MOVE MSGI-IDLAND-SPR    TO MED-IDSKYLT                               
045600     MOVE +2                 TO SPRAK-IX                                  
045700     MOVE 'GB '              TO MED-IDSKYLT                               
045800                                                                          
045900     MOVE YES TO KEYS-SW                                                  
046000                                                                          
046100*    -- CONTROL OF PARTNO (IDARTNR)                                       
046200     MOVE MFS-ERASE-FIELD    TO MOD-IDARTNR-IN                            
046300                                                                          
046400     IF MID-IDARTNR NOT = ALL '+'                                         
046500       MOVE '7'              TO MFS-IDPFK                                 
046600       MOVE SPACE            TO MFS-KDTRTYP                               
046700     END-IF                                                               
046800     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
046900     IF MSGI-IDARTNR NUMERIC                                              
047000       MOVE MSGI-IDARTNR     TO W-IDARTNR                                 
047100                                MOD-IDARTNR-UT                            
047200     ELSE                                                                 
047300       MOVE NOO TO KEYS-SW                                                
047400     END-IF                                                               
047500                                                                          
047600     IF KEYS-WRONG                                                        
047700       MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                              
047800       CALL WMEDKONV USING MED-WMEDAREA                                   
047900       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
048000       PERFORM MFS-ERASE-FIELD-OUT                                        
048100     END-IF                                                               
048200     .                                                                    
048300     EJECT                                                                
048400 C-FIRST-PAGE SECTION.                                                    
048410     MOVE 'C-FIRST-PAGE '  TO WS-SECTION                                  
048500                                                                          
048600     MOVE INF-FIRST-PAGE     TO MED-IDMFSINF                              
048700     CALL WMEDKONV USING MED-WMEDAREA                                     
048800     MOVE MED-MFSINF         TO MOD-TEMFSFEL                              
048900                                                                          
049000*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
049100     MOVE SPACE              TO W-IDKAMP                                  
049200                                WS-MSGI-SSA-KEY-ENTER                     
049300                                WS-MSGI-SSA-KEY-NEXT                      
049400                                                                          
049500     PERFORM MFS-ERASE-FIELD-IN                                           
049600     .                                                                    
049700     EJECT                                                                
049800 D-NEXT-PAGE SECTION.                                                     
049810     MOVE 'D-NEXT-PAGE '  TO WS-SECTION                                   
049900                                                                          
050000     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
050100     .                                                                    
050200     EJECT                                                                
050300 E-SAME-PAGE SECTION.                                                     
050310     MOVE 'E-SAME-PAGE   '  TO WS-SECTION                                 
050400                                                                          
050600                                                                          
050700     MOVE NOO TO IDTRANS-HOPP-SW                                          
050710                                                                          
050720     MOVE +1 TO INDX                                                      
050730     PERFORM UNTIL INDX > MAX-INDX                                        
050740       IF MID-BILD (INDX) NOT = ALL '+'                                   
050750         MOVE YES  TO IDTRANS-HOPP-SW                                     
050760       END-IF                                                             
050770       ADD +1  TO INDX                                                    
050780     END-PERFORM                                                          
050790                                                                          
050800     IF IDTRANS-HOPP-SAKNAS                                               
050900       PERFORM MFS-ERASE-FIELD-IN                                         
051000     ELSE                                                                 
051100       IF OWN-MID OR HELP-MID                                             
051200         MOVE CHANGE-SCREENS       TO MED-IDMFSINF                        
051300         CALL WMEDKONV USING MED-WMEDAREA                                 
051400         MOVE MED-TEMFSINF                                                
051500                             TO MOD-TEMFSINF                              
051600         PERFORM MFS-READ-IN-AGAIN                                        
051900         PERFORM EA-MID-INDATA-TO-MOD                                     
052000       ELSE                                                               
052100         PERFORM MFS-ERASE-FIELD-IN                                       
052200       END-IF                                                             
052300     END-IF                                                               
052400     .                                                                    
052500     EJECT                                                                
052600 EA-MID-INDATA-TO-MOD SECTION.                                            
052610     MOVE 'EA-MID-INDATA-TO-MOD '  TO WS-SECTION                          
052700* * * * * FÖR VARJE MID-FÄLT                                              
052800* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
052900* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
053000                                                                          
053100     MOVE 1                  TO INDX                                      
053200     PERFORM UNTIL INDX > MAX-INDX                                        
053300                                                                          
053400       IF MID-BILD (INDX) = ALL '+'                                       
053500         MOVE MFS-ERASE-FIELD                                             
053600                             TO MOD-BILD (INDX)                           
053700       ELSE                                                               
053800         MOVE MID-BILD (INDX)                                             
053900                             TO MOD-BILD (INDX)                           
054000       END-IF                                                             
054100                                                                          
054200       ADD 1                 TO INDX                                      
054300     END-PERFORM                                                          
054400     .                                                                    
054500     EJECT                                                                
054600 F-READ-SHOW-INFO SECTION.                                                
054610     MOVE 'F-READ-SHOW-INFO '  TO WS-SECTION                              
054700                                                                          
054800     PERFORM DB2-DCL-OPN-TP1ARTK-CRS                                      
054900                                                                          
055000     MOVE 1                TO IX                                          
055100     MOVE ZERO             TO IX-SISTA-POST                               
055200                                                                          
055300     IF SQLCODE-WS = ZERO                                                 
055400       PERFORM FA-READ-SHOW-TP1ARTK                                       
055500     END-IF                                                               
055600                                                                          
055700     IF LINES-MISSING                                                     
055800        MOVE '017'           TO MED-IDMFSFEL                              
055900        CALL WMEDKONV USING MED-WMEDAREA                                  
056000        MOVE MED-MFSFEL      TO MOD-TEMFSFEL                              
056100        PERFORM MFS-ERASE-FIELD-OUT                                       
056110        PERFORM MFS-CLOSE-FIELD-IN                                        
056200     ELSE                                                                 
056300       MOVE W-IDKAMP-MIN     TO W-IDKAMP                                  
056400                                                                          
056500       PERFORM UNTIL SQLCODE > ZERO                                       
056600       OR IX > WS-TABELL-MAX                                              
056700         MOVE 1              TO IX                                        
056800         PERFORM UNTIL IX > WS-TABELL-MAX                                 
056900         OR IX > IX-SISTA-POST                                            
057000         OR TP1KAMP-IDKAMP      = WS-IDKAMP-TAB (IX)                      
057100         OR (TP1KAMP-IDKAMP-GRP = WS-IDKAMP-GRP-TAB (IX)                  
057200         AND TP1KAMP-IDKAMP-GRP NOT = ZERO)                               
057300          ADD 1              TO IX                                        
057400         END-PERFORM                                                      
057500         IF IX > WS-TABELL-MAX                                            
057600            CONTINUE                                                      
057700         ELSE                                                             
057800           IF IX > IX-SISTA-POST                                          
057900             IF TP1KAMP-IDKAMP-GRP NOT = ZERO                             
058000               IF TP1KAMP-TISTODAT-KAMP = ZERO                            
058100                 COMPUTE WS-TISTODAT-KAMP =                               
058200                            TP1KAMP-TISTADAT-KAMP + 50000                 
058300               ELSE                                                       
058400                 MOVE TP1KAMP-TISTODAT-KAMP                               
058500                                        TO WS-TISTODAT-KAMP               
058600               END-IF                                                     
058700                                                                          
058800               MOVE DAGENS-DATUM (3:6) TO WS-DATUM                        
058900                                                                          
059000               IF WS-DATUM-MAN > 06                                       
059100                 SUBTRACT 600 FROM WS-DATUM                               
059200               ELSE                                                       
059300                 ADD 600 TO WS-DATUM                                      
059400                 SUBTRACT 10000 FROM WS-DATUM                             
059500               END-IF                                                     
059600                                                                          
059700               MOVE WS-TISTODAT-KAMP TO TMP1-YYMMDD                       
059800               MOVE WS-DATUM TO TMP2-YYMMDD                               
059900               PERFORM WY2000P1                                           
060000               IF TMP1-YYMMDD >= TMP2-YYMMDD                              
060100                 MOVE IX     TO IX-SISTA-POST                             
060200                 MOVE TP1KAMP-IDKAMP-GRP                                  
060300                             TO WS-IDKAMP-GRP-TAB (IX)                    
060400                                WS-IDKAMP-GRP-SORT (IX)                   
060500                                                                          
060600                 MOVE TP1KAMP-KDKAMP                                      
060700                             TO WS-KDKAMP-TAB (IX)                        
060800                 MOVE SPACE  TO WS-IDKAMP-TAB (IX)                        
060900                                WS-IDKAMP-SORT (IX)                       
061000                 MOVE TP1KAMP-TISTADAT-KAMP                               
061100                             TO WS-TISTADAT-KAMP-TAB (IX)                 
061200                                WS-TISTADAT-SORT     (IX)                 
061300                 MOVE TP1KAMP-TISTADAT-KAMP                               
061400                           TO WS-SEKEL-KONTROLL                           
061500                 IF WS-SEKEL-KONTR <   50                                 
061600                   MOVE 20   TO WS-TISTADAT-SORT (IX) (1:2)               
061700                 ELSE                                                     
061800                   MOVE 19   TO WS-TISTADAT-SORT (IX) (1:2)               
061900                 END-IF                                                   
062000                 MOVE TP1KAMP-TISTODAT-KAMP                               
062100                             TO WS-TISTODAT-KAMP-TAB (IX)                 
062200               END-IF                                                     
062300             ELSE                                                         
062400                                                                          
062500               IF TP1KAMP-TISTODAT-KAMP = ZERO                            
062600                 COMPUTE WS-TISTODAT-KAMP =                               
062700                            TP1KAMP-TISTADAT-KAMP + 50000                 
062800               ELSE                                                       
062900                 MOVE TP1KAMP-TISTODAT-KAMP                               
063000                                        TO WS-TISTODAT-KAMP               
063100               END-IF                                                     
063200                                                                          
063300               MOVE DAGENS-DATUM (3:6) TO WS-DATUM                        
063400                                                                          
063500               IF WS-DATUM-MAN > 06                                       
063600                 SUBTRACT 600 FROM WS-DATUM                               
063700               ELSE                                                       
063800                 ADD 600 TO WS-DATUM                                      
063900                 SUBTRACT 10000 FROM WS-DATUM                             
064000               END-IF                                                     
064100                                                                          
064200               MOVE WS-TISTODAT-KAMP TO TMP1-YYMMDD                       
064300               MOVE WS-DATUM TO TMP2-YYMMDD                               
064400               PERFORM WY2000P1                                           
064500               IF TMP1-YYMMDD >= TMP2-YYMMDD                              
064600                  MOVE IX    TO IX-SISTA-POST                             
064700                  MOVE TP1KAMP-IDKAMP                                     
064800                             TO WS-IDKAMP-TAB (IX)                        
064900                                WS-IDKAMP-SORT (IX)                       
065000                  MOVE TP1KAMP-KDKAMP                                     
065100                             TO WS-KDKAMP-TAB (IX)                        
065200                  MOVE ZERO  TO WS-IDKAMP-GRP-TAB (IX)                    
065300                                WS-IDKAMP-GRP-SORT (IX)                   
065400                  MOVE TP1KAMP-TISTADAT-KAMP                              
065500                             TO WS-TISTADAT-KAMP-TAB   (IX)               
065600                                WS-TISTADAT-SORT (IX)                     
065700                  MOVE TP1KAMP-TISTADAT-KAMP                              
065800                             TO WS-SEKEL-KONTROLL                         
065900                  IF WS-SEKEL-KONTR <  50                                 
066000                    MOVE 20  TO WS-TISTADAT-SORT (IX) (1:2)               
066100                  ELSE                                                    
066200                    MOVE 19  TO WS-TISTADAT-SORT (IX) (1:2)               
066300                  END-IF                                                  
066400                  MOVE TP1KAMP-TISTODAT-KAMP                              
066500                             TO WS-TISTODAT-KAMP-TAB (IX)                 
066600               END-IF                                                     
066700             END-IF                                                       
066800           ELSE                                                           
066900             IF  TP1KAMP-TISTADAT-KAMP = WS-TISTADAT-KAMP-TAB (IX)        
067000             AND TP1KAMP-TISTODAT-KAMP = WS-TISTODAT-KAMP-TAB (IX)        
067100               CONTINUE                                                   
067200             ELSE                                                         
067300               MOVE ZERO                                                  
067400                             TO WS-TISTADAT-KAMP-TAB (IX)                 
067500                                WS-TISTODAT-KAMP-TAB (IX)                 
067600                                WS-TISTADAT-SORT     (IX)                 
067700             END-IF                                                       
067800           END-IF                                                         
067900         END-IF                                                           
068000                                                                          
068100         PERFORM FA-READ-SHOW-TP1ARTK                                     
068300       END-PERFORM                                                        
068400                                                                          
068500                                                                          
068600       PERFORM FB-SORTERA-PLATSER                                         
068700                                                                          
068800***  HÄR LÄSES ETT STARTVÄRDE FÖR IX-RAD FRÅN USER-BASEN                  
068900***  SÅ ATT VID BLÄDDRING, START SKER MED RÄTT RAD                        
069000       MOVE 1                TO IX                                        
069100       IF WS-MSGI-SSA-KEY-ENTER NOT = SPACE                               
069200          MOVE SAVE-RADNR-ENTER                                           
069300                             TO IX                                        
069400       ELSE                                                               
069500         IF WS-MSGI-SSA-KEY-NEXT NOT = SPACE                              
069600            MOVE SAVE-RADNR-NEXT                                          
069700                             TO IX                                        
069800         END-IF                                                           
069900       END-IF                                                             
070000                                                                          
070100       MOVE 1                TO RAD-IX                                    
070200       PERFORM UNTIL RAD-IX > RAD-MAX                                     
070300       OR IX > IX-SISTA-POST                                              
070400         IF RAD-IX = 1                                                    
070500           MOVE IX           TO SAVE-RADNR-ENTER                          
070600         END-IF                                                           
070700         MOVE WS-IDKAMP-TAB (IX)                                          
070800                             TO MOD-IDKAMP (RAD-IX)                       
070900         MOVE WS-IDKAMP-GRP-TAB (IX)                                      
071000                             TO MOD-IDKAMP-GRP (RAD-IX)                   
071100                 INSPECT MOD-IDKAMP-GRP (RAD-IX)                          
071200                               REPLACING LEADING ZERO BY SPACE            
071300         MOVE WS-TISTADAT-KAMP-TAB (IX)                                   
071400                             TO MOD-TISTADAT-KAMP (RAD-IX)                
071500         MOVE WS-TISTODAT-KAMP-TAB (IX)                                   
071600                             TO MOD-TISTODAT-KAMP (RAD-IX)                
071700         MOVE WS-KDKAMP-TAB (IX)                                          
071800                             TO MOD-KDKAMP (RAD-IX)                       
071900         ADD 1               TO RAD-IX                                    
072000         ADD 1               TO IX                                        
072100       END-PERFORM                                                        
072200                                                                          
072300***  OM FLER RADER FINNS, SÅ SPARAS I USERBASEN NÄSTA RADNR               
072400***  FRÅN TABELLEN , ANNARS BLANKAS                                       
072500       IF IX             <= ANTAL                                         
072600          MOVE IX            TO SAVE-RADNR-NEXT                           
072700          MOVE INF-MORE-INFO-EXISTS                                       
072800                             TO MED-IDMFSFEL                              
072900          CALL WMEDKONV USING MED-WMEDAREA                                
073000          MOVE MED-TEMFSFEL  TO MOD-TEMFSFEL                              
073100       ELSE                                                               
073200          MOVE SPACE         TO WS-MSGI-SSA-KEY-NEXT                      
073300       END-IF                                                             
073400                                                                          
073500       PERFORM UNTIL RAD-IX > MAX-INDX                                    
073600         MOVE MFS-ERASE-FIELD TO MOD-IDKAMP (RAD-IX)                      
073700                                 MOD-IDKAMP-GRP (RAD-IX)                  
073800                                 MOD-TISTADAT-KAMP (RAD-IX)               
073900                                 MOD-TISTODAT-KAMP (RAD-IX)               
074000                                 MOD-KDKAMP (RAD-IX)                      
074010                                 MOD-BILD (RAD-IX)                        
074020         MOVE MFS-CLOSE-FIELD TO MOD-BILD-ATTR (RAD-IX)                   
074100         ADD 1 TO RAD-IX                                                  
074200       END-PERFORM                                                        
074300                                                                          
074400                                                                          
074500* ---    UPPDATERA MSGI-SPAR-AREA                                         
074600       MOVE '002'               TO MSGI-KDCALL                            
074700       MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                      
074800       MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                            
075000       MOVE '2315'     TO SAVE-IDTRANS                                    
075100       MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                  
075200       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
075300                                                                          
075400                                                                          
075500     END-IF                                                               
075600     PERFORM DB2-CLOSE-TP1ARTK-CRS                                        
075700     .                                                                    
075800     EJECT                                                                
075900 FA-READ-SHOW-TP1ARTK SECTION.                                            
075910     MOVE 'FA-READ-SHOW-TP1ARTK '  TO WS-SECTION                          
076000***  READ AND SHOW TP1ARTK AND TP1KAMP                                    
076100     PERFORM DB2-FETCH-TP1ARTK-CRS                                        
076200     .                                                                    
076300     EJECT                                                                
076400 FB-SORTERA-PLATSER SECTION.                                              
076410     MOVE 'FB-SORTERA-PLATSER '  TO WS-SECTION                            
076500                                                                          
076600     MOVE IX-SISTA-POST       TO ANTAL                                    
076700                                                                          
076800     CALL WINTSOR USING WS-TABELL STEGLANGD ANTAL                         
076900                  WS-TAB-SORT (1) NYCKELLANGD                             
077000                                                                          
077100     MOVE 1                   TO IX                                       
077200     MOVE ANTAL               TO IX2                                      
077300     PERFORM UNTIL IX > ANTAL                                             
077400       MOVE WS-TAB-POST-RAD (IX2)                                         
077500                              TO WS-TAB-POST-SORT (IX)                    
077600       ADD 1                  TO   IX                                     
077700       SUBTRACT 1             FROM IX2                                    
077800     END-PERFORM                                                          
077900                                                                          
078000     MOVE 1                   TO IX                                       
078100     PERFORM UNTIL IX > ANTAL                                             
078200       MOVE WS-TAB-POST-SORT (IX)                                         
078300                              TO WS-TAB-POST-RAD (IX)                     
078400       ADD 1                  TO   IX                                     
078500     END-PERFORM                                                          
078600     .                                                                    
078700     EJECT                                                                
078800***-----------------------------------------------------------            
078900***  JUMP TO SCREEN 2313, 2314 OR 2316                                    
078910***-----------------------------------------------------------            
079000                                                                          
079100 J-JUMP-PAGE SECTION.                                                     
079210     MOVE 'J-JUMP-PAGE        ' TO WS-SECTION                             
079300                                                                          
079400     MOVE 'J' TO INDATA-SW                                                
079401                                                                          
079410     MOVE NOO TO IDTRANS-HOPP-SW                                          
079430     MOVE +1 TO INDX                                                      
079440     PERFORM UNTIL INDX > MAX-INDX                                        
079450       IF MID-BILD (INDX) NOT = ALL '+'                                   
079460         MOVE YES  TO IDTRANS-HOPP-SW                                     
079470       END-IF                                                             
079480       ADD +1  TO INDX                                                    
079490     END-PERFORM                                                          
079491                                                                          
079492     IF IDTRANS-HOPP-SAKNAS                                               
079700        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
079800        MOVE 'N' TO INDATA-SW                                             
079900     ELSE                                                                 
080000                                                                          
080100        MOVE +1              TO INDX                                      
080200        PERFORM UNTIL INDX > MAX-INDX                                     
080300        OR STARTA-ANNAN-BILD OR INDATA-FEL                                
080400                                                                          
080500          IF MID-BILD (INDX) = ALL '+'                                    
080501             MOVE MFS-ALFA-FAELT-RAETT TO MOD-BILD-ATTR(INDX)             
080510          ELSE                                                            
080600                                                                          
080610             IF MID-BILD (INDX) NUMERIC                                   
080700               IF MID-BILD (INDX) = '2313' OR '2314' OR '2316'            
080800                 MOVE 'J'      TO SW-STARTA-ANNAN-BILD                    
080900                 MOVE MFS-ALFA-FAELT-RAETT                                
081000                               TO MOD-BILD-ATTR(INDX)                     
081100               ELSE                                                       
081200                 MOVE INDX     TO INDX-SPARA                              
081400                 MOVE MFS-ALFA-FAELT-FEL                                  
081500                               TO MOD-BILD-ATTR (INDX-SPARA)              
081600                 MOVE ERR-CORR-HILITE-FLDS                                
081700                               TO MED-IDMFSFEL                            
081800                 MOVE 'N'  TO INDATA-SW                                   
081801                                                                          
081810                 MOVE MED-1          TO MOD-TEMFSINF                      
082100               END-IF                                                     
082200             ELSE                                                         
082201               MOVE INDX     TO INDX-SPARA                                
082202               MOVE MFS-ALFA-FAELT-FEL                                    
082203                             TO MOD-BILD-ATTR (INDX-SPARA)                
082204               MOVE ERR-CORR-HILITE-FLDS                                  
082205                             TO MED-IDMFSFEL                              
082206               MOVE 'N'  TO INDATA-SW                                     
082207             END-IF                                                       
082210                                                                          
082300          END-IF                                                          
082400                                                                          
082500          ADD 1 TO INDX                                                   
082600        END-PERFORM                                                       
082700                                                                          
082800     END-IF                                                               
082900                                                                          
083000     IF INDATA-FEL                                                        
083100        CALL WMEDKONV USING MED-WMEDAREA                                  
083200        MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                              
083300        PERFORM MFS-DONT-TOUCH-FIELD-OUT                                  
083400        PERFORM MFS-DONT-TOUCH-FIELD-IN                                   
083500     ELSE                                                                 
083600                                                                          
083700       IF STARTA-ANNAN-BILD                                               
083800         MOVE LOW-VALUE      TO P-TO-P-KDZ1                               
083900         MOVE LOW-VALUE      TO P-TO-P-KDZ2                               
084000                                                                          
084100         COMPUTE INDX = INDX - 1                                          
084200         MOVE MID-BILD (INDX) (1:1) TO W-HOPP-IDTRANS-2                   
084300         MOVE MID-BILD (INDX) (2:3) TO W-HOPP-IDTRANS-4-6                 
084400                                                                          
084500         MOVE W-HOPP-IDTRANS TO P-TO-P-KDTRANS                            
084600         MOVE '2315'         TO P-TO-P-IDTRANS                            
084700         MOVE MFS-KDMFSFOR   TO P-TO-P-KDMFSFOR                           
084800         MOVE P-TO-P-SW      TO MSG-IO-AREA                               
084900                                                                          
085000         PERFORM IMS-CHANGE-ALTMSG                                        
085100         PERFORM IMS-INSERT-ALTMSG                                        
085200       ELSE                                                               
085300         MOVE MED-1          TO MOD-TEMFSFEL                              
085400         MOVE 'N'            TO INDATA-SW                                 
085500         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
085600         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
085700                                                                          
085800       END-IF                                                             
085900     END-IF                                                               
086000     .                                                                    
086100     EJECT                                                                
086200                                                                          
086300 MFS-ERASE-FIELD-OUT SECTION.                                             
086400                                                                          
086500*    --- ALLA UTDATA-FÄLT                                                 
086600*    --- INCL. SCROLL KEYS                                                
086700                                                                          
086800     MOVE +1 TO INDX                                                      
086900     PERFORM UNTIL INDX > MAX-INDX                                        
087000     MOVE MFS-ERASE-FIELD TO MOD-BILD (INDX)                              
087100                             MOD-IDKAMP (INDX)                            
087200                             MOD-IDKAMP-GRP (INDX)                        
087300                             MOD-TISTADAT-KAMP (INDX)                     
087400                             MOD-TISTODAT-KAMP (INDX)                     
087500                             MOD-KDKAMP (INDX)                            
087600       ADD +1 TO  INDX                                                    
087700     END-PERFORM                                                          
087800     .                                                                    
087900     SKIP3                                                                
088000 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
088100                                                                          
088200*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
088300     MOVE MFS-ERASE-FIELD TO MOD-IDKAMP (INDX)                            
088400     .                                                                    
088500     SKIP3                                                                
088600 MFS-ERASE-FIELD-IN SECTION.                                              
088700                                                                          
088800*    --- ALLA INDATA-FÄLT                                                 
089000     MOVE +1 TO INDX                                                      
089100     PERFORM UNTIL INDX > MAX-INDX                                        
089200       MOVE MFS-ERASE-FIELD TO MOD-BILD (INDX)                            
089300       ADD +1 TO INDX                                                     
089400     END-PERFORM                                                          
089500     .                                                                    
089600     EJECT                                                                
089700 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
089800                                                                          
089900*    --- ALLA UTDATA-FÄLT                                                 
090000*    --- INCL SCROLL KEYS AND LINEDATA                                    
090100     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDARTNR-UT                        
090200                                                                          
090300     MOVE +1 TO INDX                                                      
090400     PERFORM UNTIL INDX > MAX-INDX                                        
090500       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
090600       ADD +1 TO INDX                                                     
090700     END-PERFORM                                                          
090800     .                                                                    
090900     SKIP2                                                                
091000 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
091100                                                                          
091200*    --- OUTDATA FIELD ON SCROLL KEYS                                     
091300     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BILD (INDX)                       
091400                                    MOD-IDKAMP (INDX)                     
091500                                    MOD-IDKAMP-GRP (INDX)                 
091600                                    MOD-TISTADAT-KAMP (INDX)              
091700                                    MOD-TISTODAT-KAMP (INDX)              
091800                                    MOD-KDKAMP (INDX)                     
091900     .                                                                    
092000     SKIP3                                                                
092100 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
092200                                                                          
092300*    --- ALLA INDATA-FÄLT                                                 
092500     MOVE +1 TO INDX                                                      
092600     PERFORM UNTIL INDX > MAX-INDX                                        
092700       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BILD (INDX)                     
092800       ADD +1 TO INDX                                                     
092900     END-PERFORM                                                          
093000     .                                                                    
093100     EJECT                                                                
093200 MFS-FORM-ATTR SECTION.                                                   
093300                                                                          
093400*    --- ALL INDATA-FIELDS                                                
093500     MOVE +1 TO INDX                                                      
093600     PERFORM UNTIL INDX > MAX-INDX                                        
093700     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-BILD-ATTR (INDX)                 
093800     ADD +1 TO INDX                                                       
093900     END-PERFORM                                                          
094000     .                                                                    
094100     SKIP2                                                                
094200 MFS-READ-IN-AGAIN SECTION.                                               
094300                                                                          
094400*    --- ALL INDATA-FIELDS                                                
094500*    MOVE MFS-ADD-READ-FIELD TO MOD-XXXXXXXX-ATTR                         
094510                                                                          
094600     MOVE +1 TO INDX                                                      
094700     PERFORM UNTIL INDX > MAX-INDX                                        
094800       MOVE MFS-ADD-READ-FIELD    TO MOD-BILD-ATTR (INDX)                 
095400       ADD +1 TO INDX                                                     
095500     END-PERFORM                                                          
095600     .                                                                    
095700     EJECT                                                                
095800 MFS-CLOSE-FIELD-IN SECTION.                                              
095900                                                                          
095910*    --- ALL INDATA-FIELDS                                                
095930                                                                          
095940     MOVE +1 TO INDX                                                      
095950     PERFORM UNTIL INDX > MAX-INDX                                        
095960       MOVE MFS-CLOSE-FIELD    TO MOD-BILD-ATTR (INDX)                    
095970       ADD +1 TO INDX                                                     
095980     END-PERFORM                                                          
095990     .                                                                    
095991     EJECT                                                                
096000* --- IMS SECTIONS ---                                                    
096100     SKIP3                                                                
096200 IMS-GET-MSG SECTION.                                                     
096300                                                                          
096400     MOVE '  QC' TO GOOD-STATUSCODES                                      
096500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
096600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
096700     PERFORM IMS-STATUSCHECK                                              
096800     .                                                                    
096900     SKIP3                                                                
097000 IMS-INSERT-MSG SECTION.                                                  
097100                                                                          
097200*    IF MSGI-IDLAND-SPR = 'GB'                                            
097300*      MOVE '0' TO MFS-KDHUVOMR                                           
097400*    END-IF                                                               
097500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
097600     MOVE SPACE TO GOOD-STATUSCODES                                       
097700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
097800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
097900     PERFORM IMS-STATUSCHECK                                              
098000     .                                                                    
098100     EJECT                                                                
098200 IMS-CHANGE-ALTMSG SECTION.                                               
098300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
098400     MOVE '  A1A4' TO GOOD-STATUSCODES                                    
098500     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS-1                        
098600     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
098700     PERFORM IMS-STATUSCHECK                                              
098800     .                                                                    
098900     SKIP3                                                                
099000 IMS-INSERT-ALTMSG SECTION.                                               
099100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
099200     MOVE SPACE TO GOOD-STATUSCODES                                       
099300     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
099400     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
099500     PERFORM IMS-STATUSCHECK                                              
099600     .                                                                    
099700     EJECT                                                                
099800 IMS-STATUSCHECK SECTION.                                                 
099900                                                                          
100000     SET STATUS-IX TO 1                                                   
100100     SEARCH GOOD-STATUS                                                   
100200       AT END                                                             
100300         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
100400         DELIMITED BY SIZE INTO ERROR-TEXT                                
100500         CALL FELLOG                                                      
100600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
100700         CONTINUE                                                         
100800     END-SEARCH                                                           
100900     .                                                                    
101000     EJECT                                                                
101100 DB2-DCL-OPN-TP1KAMP-CRS  SECTION.                                        
101200     MOVE 'DB2-DCL-OPN-TP1KAMP   ' TO  WS-DB2-SEKTION                     
101300                                                                          
101400     MOVE 000100  TO GOOD-SQLCODECODES                                    
101500                                                                          
101600     EXEC SQL                                                             
101700         DECLARE TP1KAMP-CRS CURSOR FOR                                   
101800         SELECT  IDKAMP                                                   
101900                ,TISTADAT_KAMP                                            
102000                ,TISTODAT_KAMP                                            
102100                ,KDKAMP                                                   
102200                ,IDKAMP_GRP                                               
102300                ,TISTADAT_KAMP + 20000000 AS STADAT                       
102400                                                                          
102500         FROM TP1KAMP                                                     
102600                                                                          
102700         WHERE   TISTADAT_KAMP < 500000                                   
102800             AND IDKAMP = :W-IDKAMP                                       
102900                                                                          
103000         UNION                                                            
103100                                                                          
103200         SELECT  IDKAMP                                                   
103300                ,TISTADAT_KAMP                                            
103400                ,TISTODAT_KAMP                                            
103500                ,KDKAMP                                                   
103600                ,IDKAMP_GRP                                               
103700                ,TISTADAT_KAMP + 19000000 AS STADAT                       
103800                                                                          
103900         FROM TP1KAMP                                                     
104000                                                                          
104100         WHERE TISTADAT_KAMP > 500000                                     
104200               AND IDKAMP = :W-IDKAMP                                     
104300                                                                          
104400         GROUP BY  IDKAMP                                                 
104500                  ,TISTADAT_KAMP                                          
104600                  ,TISTODAT_KAMP                                          
104700                  ,KDKAMP                                                 
104800                  ,IDKAMP_GRP                                             
104900                                                                          
105000         ORDER BY  STADAT DESC                                            
105100     END-EXEC                                                             
105200                                                                          
105300     MOVE 000100  TO GOOD-SQLCODECODES                                    
105400     EXEC SQL OPEN TP1KAMP-CRS END-EXEC                                   
105500     .                                                                    
105600     SKIP3                                                                
105700 DB2-FETCH-TP1KAMP-CRS  SECTION.                                          
105800     MOVE 'DB2-FETCH-TP1KAMP   ' TO  WS-DB2-SEKTION                       
105900     SKIP2                                                                
106000     MOVE 000100  TO GOOD-SQLCODECODES                                    
106100     EXEC SQL FETCH TP1KAMP-CRS INTO                                      
106200                :TP1KAMP-IDKAMP                                           
106300               ,:TP1KAMP-TISTADAT-KAMP                                    
106400               ,:TP1KAMP-TISTODAT-KAMP                                    
106500               ,:TP1KAMP-KDKAMP                                           
106600               ,:TP1KAMP-IDKAMP-GRP                                       
106700     END-EXEC                                                             
106800                                                                          
106900     MOVE SQLCODE TO SQLCODE-WS                                           
107000     PERFORM DB2-STATUS-CHECK                                             
107100     .                                                                    
107200     SKIP3                                                                
107300 DB2-CLOSE-TP1KAMP-CRS  SECTION.                                          
107400     MOVE 'DB2-CLOSE-TP1KAMP   '  TO  WS-DB2-SEKTION                      
107500                                                                          
107600     EXEC SQL CLOSE TP1KAMP-CRS END-EXEC                                  
107700     .                                                                    
107800     EJECT                                                                
107900 DB2-SELECT-TP1KAMP    SECTION.                                           
108000     MOVE 'DB2-SELECT-TP1KAMP   ' TO  WS-DB2-SEKTION                      
108100                                                                          
108200     MOVE 000100                  TO GOOD-SQLCODECODES                    
108300                                                                          
108400     EXEC SQL                                                             
108500         SELECT  IDKAMP                                                   
108600                ,TISTADAT_KAMP                                            
108700                ,TISTODAT_KAMP                                            
108800                ,KDKAMP                                                   
108900                ,IDKAMP_GRP                                               
109000                                                                          
109100           INTO   :TP1KAMP-IDKAMP                                         
109200                 ,:TP1KAMP-TISTADAT-KAMP                                  
109300                 ,:TP1KAMP-TISTODAT-KAMP                                  
109400                 ,:TP1KAMP-KDKAMP                                         
109500                 ,:TP1KAMP-IDKAMP-GRP                                     
109600                                                                          
109700           FROM    TP1KAMP                                                
109800                                                                          
109900           WHERE   IDKAMP    = :W-IDKAMP                                  
110000     END-EXEC                                                             
110100                                                                          
110200     MOVE SQLCODE TO SQLCODE-WS                                           
110300     PERFORM DB2-STATUS-CHECK                                             
110400     .                                                                    
110500     EJECT                                                                
110600 DB2-DCL-OPN-TP1ARTK-CRS  SECTION.                                        
110700     MOVE 'DB2-DCL-OPN-TP1ARTK   ' TO  WS-DB2-SEKTION                     
110800                                                                          
110900     MOVE 000100  TO GOOD-SQLCODECODES                                    
111000                                                                          
111100     EXEC SQL                                                             
111200         DECLARE TP1ARTK-CRS CURSOR FOR                                   
111300           SELECT  A.IDKAMP                                               
111400                  ,A.IDARTNR                                              
111500                  ,B.TISTADAT_KAMP                                        
111600                  ,B.TISTODAT_KAMP                                        
111700                  ,B.KDKAMP                                               
111800                  ,B.IDKAMP_GRP                                           
111900                                                                          
112000           FROM    TP1ARTK A                                              
112100                  ,TP1KAMP B                                              
112200                                                                          
112300           WHERE   A.IDARTNR = :W-IDARTNR                                 
112400               AND A.IDKAMP  =  B.IDKAMP                                  
112500                                                                          
112600           ORDER BY B.IDKAMP                                              
112700     END-EXEC                                                             
112800                                                                          
112900     MOVE 000100  TO GOOD-SQLCODECODES                                    
113000     EXEC SQL OPEN TP1ARTK-CRS END-EXEC                                   
113100     .                                                                    
113200     SKIP3                                                                
113300 DB2-FETCH-TP1ARTK-CRS  SECTION.                                          
113400     MOVE 'DB2-FETCH-TP1ARTK   ' TO  WS-DB2-SEKTION                       
113500     SKIP2                                                                
113600     MOVE 000100  TO GOOD-SQLCODECODES                                    
113700     EXEC SQL                                                             
113800         FETCH TP1ARTK-CRS INTO                                           
113900                    :TP1KAMP-IDKAMP                                       
114000                   ,:TP1ARTK-IDARTNR                                      
114100                   ,:TP1KAMP-TISTADAT-KAMP                                
114200                   ,:TP1KAMP-TISTODAT-KAMP                                
114300                   ,:TP1KAMP-KDKAMP                                       
114400                   ,:TP1KAMP-IDKAMP-GRP                                   
114500     END-EXEC                                                             
114600                                                                          
114700     MOVE SQLCODE TO SQLCODE-WS                                           
114800     PERFORM DB2-STATUS-CHECK                                             
114900     .                                                                    
115000     SKIP3                                                                
115100 DB2-CLOSE-TP1ARTK-CRS  SECTION.                                          
115200     MOVE 'DB2-CLOSE-TP1ARTK   ' TO  WS-DB2-SEKTION                       
115300                                                                          
115400     EXEC SQL CLOSE TP1ARTK-CRS END-EXEC                                  
115500     .                                                                    
115600     EJECT                                                                
115700 DB2-STATUS-CHECK  SECTION.                                               
115800                                                                          
115900     SET SQLCODE-IX TO 1                                                  
116000     SEARCH GOOD-SQLCODE                                                  
116100       AT END                                                             
116200*         STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
116300*         DELIMITED BY SIZE INTO ERROR-TEXT                               
116400          CALL FELLOG                                                     
116500       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
116600     END-SEARCH                                                           
116700     .                                                                    
116800     EJECT                                                                
116900     -COPY WY2000P1                                                       
