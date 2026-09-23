000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3015400.                                                
000400 AUTHOR.         ÖSTRÖM ELEONOR.                                          
000500 DATE-WRITTEN.   00/03/29.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800**   FUNKTION:                                                            
000900**       FRÅGEPGM SOM LÄSER DB2-TABELLER:                                 
001000*        URVAL GÖRS GENOM VAL AV INTERVALL VECKOR (MAX 26).               
001100*        HOPP KAN GÖRAS TILL BILD 3155.                                   
001200*                                                                         
001300*        PROGRAMMET LÄSER      DB2-TABELL BYLRAD                          
001400*                              DB2-TABELL BYLACK                          
001500*                              DB2-TABELL BYLART                          
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W3T154                                              
001900*        MID:         W3I15401                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W3O15401                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600                                                                          
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM                       PIC X(08)   VALUE 'W3015400'.            
003300                                                                          
003400*    ---INDEX FÖR BLÄDDRINGSRADER                                         
003500 77  INDX                        PIC S9(4)   VALUE +0  COMP SYNC.         
003600 77  MAX-INDX                    PIC S9(4)   VALUE +15 COMP SYNC.         
003700 77  W-INDX                      PIC S9(4)   VALUE +0  COMP SYNC.         
003800                                                                          
003900 77  W-IN                        PIC S9(7)   COMP-3   VALUE ZERO.         
004000 77  W-OUT                       PIC S9(7)   COMP-3   VALUE ZERO.         
004100 77  WS-INOUT                    PIC S9(7)   COMP-3   VALUE ZERO.         
004200 77  WS-DIFF                     PIC S9(6)V9(3) VALUE ZERO COMP-3.        
004300 77  WS-PROCENT                  PIC -(3)9   VALUE ZERO.                  
004400 77  WS-JAMFOR-PROC              PIC 9(4)    VALUE ZERO.                  
004500 77  WS-BYT-RELARM-PER           PIC 9(3)    VALUE ZERO.                  
004600 77  WS-INDX                     PIC S9(4)   VALUE +0 COMP SYNC.          
004700 77  WS-VECKA-ANTAL              PIC S9(4)   VALUE +0 COMP SYNC.          
004800 77  WS-IDARTNR-ALFA             PIC X(8)    VALUE SPACE.                 
004900 77  WS-NUM-IDARTNR              PIC 9(9).                                
005000 77  WS-KVANTAL                  PIC S9(7)   COMP-3   VALUE ZERO.         
005100                                                                          
005200 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005300     88  ALLT-OK                             VALUE 'J'.                   
005400     88  ALLT-EJ                             VALUE 'N'.                   
005500                                                                          
005600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005700     88  INDATA-OK                           VALUE 'J'.                   
005800                                                                          
005900 77  HOPP-SW                     PIC X       VALUE 'J'.                   
006000     88  HOPP-OK                             VALUE 'N'.                   
006100     88  NO-HOPP                             VALUE 'J'.                   
006200                                                                          
006300 77  TIAAVV-SW                   PIC X       VALUE 'J'.                   
006400     88  TIAAVV-EJ                           VALUE 'N'.                   
006500     88  TIAAVV-OK                           VALUE 'J'.                   
006600                                                                          
006700 77  DATUM-SW                    PIC X       VALUE 'J'.                   
006800     88  DATUM-OK                            VALUE 'J'.                   
006900                                                                          
007000 77  RAD-SW                      PIC X       VALUE 'N'.                   
007100     88  RAD-OK                              VALUE 'J'.                   
007200                                                                          
007300 01  EXCL-IDDISTR                PIC 9(5).                                
007400     88  EXCL-IDDISTR-OK                     VALUE 07512                  
007500                                                   07625                  
007600                                                   06121                  
007700                                                   05619                  
007700                                                   06251                  
007700                                                   06200                  
007700                                                   06270.                 
750000                                                                          
760000 01  WS-RET                      PIC X(3)    VALUE 'RET'.                 
770000 01  WS-FAK                      PIC X(3)    VALUE 'FAK'.                 
780000 01  WS-KRE                      PIC X(3)    VALUE 'KRE'.                 
790000                                                                          
800000 01  WS-DATUM-X.                                                          
810000     03  WS-DAR                  PIC X(2)    VALUE '20'.                  
820000     03  WS-AAVV                 PIC X(4).                                
830000 01  DATUM-WS.                                                            
840000     03 AA-WS                    PIC 9(2).                                
850000     03 VV-WS                    PIC 9(2).                                
860000                                                                          
870000 01  W-FROM-IN.                                                           
880000     03  WS-DAR-FROM             PIC X(2)    VALUE '20'.                  
890000     03  WS-AAVV-FROM            PIC X(4).                                
900000 01  WS-FROM-IN                  PIC X(6).                                
910000 01  WS-FROM-NUM.                                                         
920000     03  WS-DAR-FROM             PIC 9(2).                                
930000     03  WS-AAR-FROM             PIC 9(2).                                
940000     03  WS-VECKA-FROM           PIC 9(2).                                
950000                                                                          
960000 01  W-TOM-IN.                                                            
970000     03  WS-DAR-TOM              PIC X(2)    VALUE '20'.                  
980000     03  WS-AAVV-TOM             PIC X(4).                                
990000 01  WS-TOM-IN                   PIC X(6).                                
000000 01  WS-TOM-NUM.                                                          
010000     03  WS-DAR-TOM              PIC 9(2).                                
020000     03  WS-AAR-TOM              PIC 9(2).                                
030000     03  WS-VECKA-TOM            PIC 9(2).                                
040000                                                                          
050000 01  JAMFOR-IDFKNGRP             PIC X(4).                                
060000 01  JAMFOR-IDDISTR              PIC X(4).                                
070000 01  JAMFOR-IDARTNR              PIC X(8).                                
080000                                                                          
090000 01  WS-IDDISTR                  PIC S9(5)    COMP-3 VALUE ZERO.          
100000 01  WS-IDARTNR                  PIC S9(9)    COMP-3 VALUE ZERO.          
110000 01  WS-IDFKNGRP                 PIC S9(5)    COMP-3 VALUE ZERO.          
120000                                                                          
130000 77  W-IDARTNR-MIN               PIC S9(9)   COMP-3 VALUE ZERO.           
140000                                                                          
150000 77  IDARTNR-WS                  PIC S9(9)   COMP-3 VALUE ZERO.           
160000 77  WS-S01-IDARTNR              PIC S9(9)   COMP-3 VALUE ZERO.           
170000                                                                          
180000 01  FILLER                      PIC  X(16)  VALUE 'BYTES-ART '.          
190000 01  TEST-IDARTNR                PIC  9(9)   COMP-3.                      
200000*01  FILLER  -COPY WWBYT02     -RED TEST-IDARTNR.                         
210000                                                                          
220000*01  FILLER  -COPY WWBYT03     -RED TEST-IDARTNR.                         
230000                                                                          
240000*01  FILLER  -COPY WWBYT16     -RED TEST-IDARTNR.                         
250000                                                                          
260000     SKIP2                                                                
270000*    --- ALTERNATIV TRANSAKTIONER                                         
280000 01  ALT-IO-3155.                                                         
290000     03 M-SW-LL                PIC   S9(4)  VALUE +051 COMP SYNC.         
300000     03 M-SW-Z1-Z2             PIC    X(2)  VALUE LOW-VALUE.              
310000     03 M-SW-KDTRANS           PIC    X(8)  VALUE 'W3T155 7'.             
320000     03 M-SW-IDTRANS           PIC    X(4)  VALUE '3154'.                 
330000     03 M-SW-KDMFSFOR          PIC    X(1)  VALUE '2'.                    
340000*03  -COPY W3I15501   -PRE  M-                                            
350000     SKIP2                                                                
360000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
370000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
380000                                                                          
390000 77  JA                          PIC X       VALUE 'J'.                   
400000 77  NEJ                         PIC X       VALUE 'N'.                   
410000                                                                          
420000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
430000                                                                          
440000                                                                          
450000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
460000     88  NYCKLAR-OK                          VALUE 'J'.                   
470000     88  NYCKLAR-FEL                         VALUE 'N'.                   
480000                                                                          
490000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
500000     88  EGEN-MID                            VALUE '3154'.                
510000     88  GODK-MID                            VALUE '3154'                 
520000                                                   '3155'.                
530000     88  HELP-MID                            VALUE '0551'.                
540000                                                                          
550000 77  IDDISTR-WS                  PIC X       VALUE 'J'.                   
560000     88  IDDISTR-OK                          VALUE 'J'.                   
570000     88  IDDISTR-NEJ                         VALUE 'N'.                   
580000                                                                          
590000 77  IDFKNGRP-WS                 PIC X       VALUE 'J'.                   
600000     88  IDFKNGRP-OK                         VALUE 'J'.                   
610000     88  IDFKNGRP-NEJ                        VALUE 'N'.                   
620000                                                                          
630000 77  IDARTNR-SW                  PIC X       VALUE 'J'.                   
640000     88  IDARTNR-OK                          VALUE 'J'.                   
650000     88  IDARTNR-NEJ                         VALUE 'N'.                   
660000                                                                          
670000     EJECT                                                                
680000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
690000 01  GENERELLA-SUBPROGRAM.                                                
700000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
710000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
720000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
730000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
740000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
750000     EJECT                                                                
760000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
770000*01 -COPY WMEDAREA                                                        
780000*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
790000*01 -COPY WDATAREA                                                        
800000     SKIP3                                                                
810000 01  MESSAGE-CODES.                                                       
820000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
830000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '003'.                 
840000     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
850000     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
860000     03  ENTER-CMD               PIC X(3)    VALUE '048'.                 
870000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
880000     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
890000     03  INFORMATION-MISSING     PIC X(3)    VALUE '413'.                 
900000     EJECT                                                                
910000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
920000*                                                                         
930000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
940000     SKIP3                                                                
950000*01 -COPY WMSGINIT                                                        
960000     EJECT                                                                
970000*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
980000*                                                                         
990000 01  FILLER                      PIC X(16)   VALUE 'SPAR-AREA'.           
000000 01  SPAR-AREA.                                                           
010000     03  SPAR-IDTRANS            PIC X(4)    VALUE '3154'.                
020000     03  SPAR-IDARTNR-ENTER  PIC S9(9) COMP-3  VALUE ZERO.                
030000     03  SPAR-IDARTNR-NEXT   PIC S9(9) COMP-3  VALUE ZERO.                
040000     03  SPAR-TIAAVV-FOM         PIC 9(6).                                
050000     03  SPAR-TIAAVV-TOM         PIC 9(6).                                
060000     03  SPAR-IDDISTR            PIC 9(5)  COMP-3.                        
070000     03  SPAR-TABELL.                                                     
080000       05 TABELL OCCURS 15.                                               
090000         07 SPAR-IDARTNR-TAB          PIC Z(7)9.                          
100000         07 SPAR-BEART-ENG-TAB        PIC X(25).                          
110000         07 SPAR-OUTLEV-TAB           PIC Z(7)9.                          
120000         07 SPAR-INLEV-TAB            PIC Z(7)9.                          
130000         07 SPAR-DIFF-TAB             PIC Z(7)9.                          
140000         07 SPAR-REPROCENT-TAB        PIC -(3)9.                          
150000     03  FILLER                   PIC X(6).                               
160000     03  SPAR-3154-IDDISTR        PIC X(4).                               
170000     03  SPAR-3155-IDDISTR        PIC X(4).                               
180000     03  SPAR-3156-IDDEALR        PIC X(6).                               
190000     03  SPAR-IDFKNGRP            PIC X(4).                               
200000     03  SPAR-IDARTNR             PIC 9(9) COMP-3 VALUE ZERO.             
210000     03  SPAR-3154-IDARTNR        PIC X(8).                               
220000                                                                          
230000                                                                          
240000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
250000*                                                                         
260000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
270000     SKIP3                                                                
280000*01  MID -COPY W3I15401                                                   
290000     EJECT                                                                
300000 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
310000     SKIP3                                                                
320000*01  -COPY WMSGAREA                                                       
330000     EJECT                                                                
340000     03  MOD REDEFINES MSG-AREA.                                          
350000*      05  -COPY W3O15401                                                 
360000     EJECT                                                                
370000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
380000     SKIP3                                                                
390000*01  -COPY WMFSAREA                                                       
400000     EJECT                                                                
410000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
420000*                                                                         
430000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
440000     SKIP3                                                                
450000 01  NYCKLAR-TILL-DLI.                                                    
460000     03  W-601-IDARTNR-X.                                                 
470000         05  W-601-IDARTNR       PIC S9(9)   VALUE ZERO COMP-3.           
480000     03  W-KDSEGKEY-X.                                                    
490000         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
500000     03  W-628-IDARTNR-X.                                                 
510000         05  W-628-IDARTNR       PIC S9(9)   VALUE ZERO COMP-3.           
520000     SKIP2                                                                
530000*    --- STATUS-KOD FRÅN IMS                                              
540000 01  STATUS-WS                   PIC XX.                                  
550000     88  SEGMENT-FINNS                       VALUE '  '.                  
560000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
570000     SKIP2                                                                
580000 01  GODK-STATUSKODER.                                                    
590000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
600000     SKIP3                                                                
610000 01  SSA1                        PIC X(64).                               
620000 01  SSA2                        PIC X(64).                               
630000 01  SSA3                        PIC X(64).                               
640000     EJECT                                                                
650000*    --- IMS FUNKTIONSKODER                                               
660000*01  -COPY W0003                                                          
670000     EJECT                                                                
680000 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
690000       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
700000                                                                          
710000 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
720000 01  DB2-WS.                                                              
730000     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
740000         88  CURSOR-OK                       VALUE 000.                   
750000         88  RADER-FINNS                     VALUE 000.                   
760000         88  RADER-SAKNAS                    VALUE 100.                   
770000         88  ATKOMST-FEL                     VALUE 904.                   
780000     03  BYLRAD-WS               PIC 9(3)    VALUE ZERO.                  
790000         88  BYLRAD-OK                       VALUE 000.                   
800000         88  BYLRAD-SAKNAS                   VALUE 100.                   
810000         88  BYLRAD-FEL                      VALUE 904.                   
820000     03  BYLACK-WS               PIC 9(3)    VALUE ZERO.                  
830000         88  BYLACK-OK                       VALUE 000.                   
840000         88  BYLACK-SAKNAS                   VALUE 100.                   
850000         88  BYLACK-FEL                      VALUE 904.                   
860000     03  BYLART-WS               PIC 9(3)    VALUE ZERO.                  
870000         88  BYLART-OK                       VALUE 000.                   
880000         88  BYLART-SAKNAS                   VALUE 100.                   
890000         88  BYLART-FEL                      VALUE 904.                   
900000     03  GODK-SQLCODEKODER.                                               
910000         05  GODK-SQLCODE OCCURS 5                                        
920000             INDEXED BY SQLCODE-IX PIC 9(3).                              
930000     EJECT                                                                
940000 01  FILLER         PIC X(16) VALUE 'BYLART-AREA     '.                   
950000*01  -COPY BYLART -PRE BYLART-                                            
960000     EJECT                                                                
970000 01  FILLER         PIC X(16) VALUE 'BYLRAD-AREA     '.                   
980000*01  -COPY BYLRAD -PRE BYLRAD-                                            
990000     EJECT                                                                
000000 01  FILLER         PIC X(16) VALUE 'BYLACK-AREA     '.                   
010000*01  -COPY BYLACK -PRE BYLACK-                                            
020000     EJECT                                                                
030000 01  FILLER                      PIC X(16)   VALUE 'BYLART-AREA'.         
040000       EXEC SQL INCLUDE BYLART  END-EXEC.                                 
050000     EJECT                                                                
060000 01  FILLER                      PIC X(16)   VALUE 'BYLRAD-AREA'.         
070000       EXEC SQL INCLUDE BYLRAD  END-EXEC.                                 
080000     EJECT                                                                
090000 01  FILLER                      PIC X(16)   VALUE 'BYLACK-AREA'.         
100000       EXEC SQL INCLUDE BYLACK  END-EXEC.                                 
110000     EJECT                                                                
120000 LINKAGE SECTION.                                                         
130000*01  -COPY W0009   -PRE MSG-                                              
140000*01  -COPY W0009   -PRE ALT-                                              
150000*01  -COPY W0008   -PRE USEA-                                             
160000     05  FILLER                  PIC X.                                   
170000                                                                          
180000     EJECT                                                                
190000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB.                      
200000 MAIN SECTION.                                                            
210000     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB.                      
220000                                                                          
230000     PERFORM IMS-GET-MSG                                                  
240000     IF SEGMENT-FINNS                                                     
250000       PERFORM A-INIT                                                     
260000       PERFORM B-KOLLA-NYCKLAR                                            
270000       IF NYCKLAR-OK                                                      
280000         IF MFS-SPLIT                                                     
290000            PERFORM G-KOLLA-SELECT                                        
300000            IF INDATA-OK                                                  
310000              PERFORM H-SKICKA-TRANS                                      
320000            END-IF                                                        
330000         ELSE                                                             
340000           IF MFS-FIRST                                                   
350000             PERFORM C-FOERSTA-SIDA                                       
360000           ELSE                                                           
370000             IF MFS-NEXT                                                  
380000               PERFORM D-NAESTA-SIDA                                      
390000             ELSE                                                         
400000               PERFORM E-SAMMA-SIDA                                       
410000             END-IF                                                       
420000           END-IF                                                         
430000         END-IF                                                           
440000         IF NO-HOPP                                                       
450000          PERFORM F-LAES-VISA-INFO                                        
460000         END-IF                                                           
470000       END-IF                                                             
480000*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
490000*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
500000*      COMPUTE XXXX       LENGTH OF MID-W3I15501 + 17                     
510000*     + LÄGG TILL IF HOPP = JA                                            
520000       IF NO-HOPP                                                         
530000         COMPUTE MSG-KVLL = LENGTH OF MOD-W3O15401 + 4                    
540000         PERFORM IMS-INSERT-MSG                                           
550000       END-IF                                                             
560000     END-IF                                                               
570000                                                                          
580000     MOVE ZERO TO RETURN-CODE                                             
590000     GOBACK                                                               
600000     .                                                                    
610000     EJECT                                                                
620000 A-INIT SECTION.                                                          
630000                                                                          
640000     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
650000     ACCEPT DAT-I-TIDATUM FROM DATE                                       
660000     CALL WDATKONV          USING DAT-KDDATFORM                           
670000                                  DAT-I-TIDATUM                           
680000                                  DAT-O-TIDATUM                           
690000                                  DAT-KDSVAR                              
700000     IF DAT-KDSVAR-OK                                                     
710000        MOVE DAT-TIAA-VECKA TO    AA-WS                                   
720000        MOVE DAT-TIVV       TO    VV-WS                                   
730000        SUBTRACT 1          FROM  VV-WS                                   
740000        IF VV-WS = ZERO                                                   
750000          SUBTRACT 1        FROM  AA-WS                                   
760000          MOVE 52           TO    VV-WS                                   
770000        END-IF                                                            
780000        MOVE DATUM-WS       TO    WS-AAVV                                 
790000     ELSE                                                                 
800000          MOVE ZERO           TO AA-WS                                    
810000                                 VV-WS                                    
820000          MOVE DATUM-WS       TO WS-AAVV                                  
830000     END-IF                                                               
840000                                                                          
850000     IF MSG-DUBBLA-TRANSKODER                                             
860000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I15401                 
870000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
880000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
890000     ELSE                                                                 
900000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I15401                  
910000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
920000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
930000     END-IF                                                               
940000                                                                          
950000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
960000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
970000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
980000                                                                          
990000     MOVE LOW-VALUE TO MSG-AREA                                           
000000     MOVE 'W3O154N1' TO MFS-IDMOD                                         
010000     MOVE '3154' TO MOD-IDTRANS                                           
020000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
030000                                                                          
040000                                                                          
050000     IF EGEN-MID OR HELP-MID                                              
060000       CONTINUE                                                           
070000     ELSE                                                                 
080000       MOVE SPACE TO MFS-KDTRTYP                                          
090000       MOVE '7' TO MFS-IDPFK                                              
100000     END-IF                                                               
110000                                                                          
120000     MOVE 'GB'                            TO MED-IDSKYLT                  
130000                                                                          
140000     INITIALIZE GODK-SQLCODEKODER                                         
150000     .                                                                    
160000     EJECT                                                                
170000 B-KOLLA-NYCKLAR SECTION.                                                 
180000                                                                          
190000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
200000     MOVE '001'             TO MSGI-KDCALL                                
210000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
220000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
230000     MOVE '3154'            TO MSGI-IDTRANS                               
240000                                                                          
250000     IF GODK-MID                                                          
260000*******  ÄNDRAT ETRACKER 10195747 *********                               
270000       IF MID-TIAAVV-FOM-IN = SPACE AND                                   
280000          MID-TIAAVV-TOM-IN = SPACE                                       
290000          MOVE '0001' TO MID-TIAAVV-FOM-IN                                
300000          MOVE '9952' TO MID-TIAAVV-TOM-IN                                
310000       END-IF                                                             
320000*******                                                                   
330000       MOVE MID-TIAAVV-FOM-IN  TO MSGI-TIAAVV-FOM                         
340000       MOVE MID-TIAAVV-TOM-IN  TO MSGI-TIAAVV-TOM                         
350000*      IF W-IDTRANS = '3155'                                              
360000*        MOVE MSGI-IDFKNGRP      TO MID-IDFKNGRP-IN                       
370000*      END-IF                                                             
380000       IF EGEN-MID                                                        
390000         MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                          
400000                                    SPAR-3154-IDDISTR                     
410000         MOVE MID-IDFKNGRP-IN    TO MSGI-IDFKNGRP                         
420000         MOVE MID-IDARTNR-IN     TO SPAR-3154-IDARTNR                     
430000                                    MSGI-IDARTNR                          
440000       END-IF                                                             
450000     ELSE                                                                 
460000      MOVE ZERO               TO MID-IDDISTR-IN                           
470000                                 SPAR-3154-IDDISTR                        
480000                                 MSGI-IDDISTR                             
490000                                 SPAR-IDDISTR                             
500000                                 MID-IDFKNGRP-IN                          
510000                                 MSGI-IDFKNGRP                            
520000                                 SPAR-IDFKNGRP                            
530000                                 MID-IDARTNR-IN                           
540000                                 SPAR-3154-IDARTNR                        
550000                                 MSGI-IDARTNR                             
560000                                 SPAR-IDARTNR                             
570000     END-IF                                                               
580000                                                                          
590000                                                                          
600000                                                                          
610000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
620000     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
630000     MOVE MSGI-IDDISTR TO SPAR-3154-IDDISTR                               
640000     IF EGEN-MID                                                          
650000       MOVE MID-IDARTNR-IN TO SPAR-3154-IDARTNR                           
660000       INSPECT SPAR-3154-IDARTNR REPLACING LEADING SPACE BY ZERO          
670000       INSPECT SPAR-3154-IDARTNR REPLACING ALL '+' BY ZERO                
680000     END-IF                                                               
690000                                                                          
700000     INSPECT MSGI-TIAAVV-FOM REPLACING LEADING SPACE BY ZERO              
710000     INSPECT MSGI-TIAAVV-TOM REPLACING LEADING SPACE BY ZERO              
720000                                                                          
730000     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
740000     INSPECT MSGI-IDARTNR REPLACING ALL SPACE BY ZERO                     
750000     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
760000     INSPECT MSGI-IDFKNGRP REPLACING LEADING SPACE BY ZERO                
770000                                                                          
780000     MOVE JA  TO TIAAVV-SW                                                
790000     MOVE NEJ TO IDDISTR-WS                                               
800000     MOVE NEJ TO IDFKNGRP-WS                                              
810000     MOVE NEJ TO IDARTNR-SW                                               
820000     MOVE NEJ TO ALLT-SW                                                  
830000                                                                          
840000     IF GODK-MID OR EGEN-MID                                              
850000       MOVE JA TO NYCKLAR-SW                                              
860000     ELSE                                                                 
870000       MOVE NEJ TO NYCKLAR-SW                                             
880000     END-IF                                                               
890000                                                                          
900000     IF W-IDTRANS = '3155'                                                
910000       IF MSGI-IDFKNGRP = ALL ZERO AND                                    
920000          MSGI-IDDISTR  = ALL ZERO AND                                    
930000          SPAR-3154-IDARTNR = ALL ZERO                                    
940000          CONTINUE                                                        
950000       ELSE                                                               
960000         IF MSGI-IDFKNGRP = ALL ZERO AND                                  
970000            SPAR-3154-IDARTNR = ALL ZERO                                  
980000            MOVE MSGI-IDDISTR TO MID-IDDISTR-IN                           
990000                                 SPAR-IDDISTR                             
000000         END-IF                                                           
010000         IF MSGI-IDDISTR = ALL ZERO                                       
020000            AND SPAR-3154-IDARTNR = ALL ZERO                              
030000            MOVE MSGI-IDFKNGRP TO MID-IDFKNGRP-IN                         
040000                                  SPAR-IDFKNGRP                           
050000         END-IF                                                           
060000                                                                          
070000         IF MSGI-IDFKNGRP = ALL ZERO AND                                  
080000            MSGI-IDDISTR = ALL ZERO                                       
090000            MOVE SPAR-3154-IDARTNR TO MID-IDARTNR-IN                      
100000                                      SPAR-IDARTNR                        
110000         END-IF                                                           
120000       END-IF                                                             
130000     END-IF                                                               
140000                                                                          
150000     IF GODK-MID                                                          
160000       IF MID-TIAAVV-FOM-IN = ALL '+' AND                                 
170000         MID-TIAAVV-TOM-IN = ALL '+'                                      
180000                                                                          
190000           MOVE MID-TIAAVV-FOM-UT  TO WS-AAVV-FROM                        
200000           MOVE W-FROM-IN          TO WS-FROM-IN                          
210000                                      WS-FROM-NUM                         
220000                                      SPAR-TIAAVV-FOM                     
230000           MOVE MID-TIAAVV-TOM-UT TO WS-AAVV-TOM                          
240000           MOVE W-TOM-IN          TO WS-TOM-IN                            
250000                                     WS-TOM-NUM                           
260000                                     SPAR-TIAAVV-TOM                      
270000           IF W-FROM-IN NUMERIC AND W-TOM-IN NUMERIC                      
280000              IF (WS-FROM-NUM NUMERIC) AND                                
290000                 (WS-VECKA-FROM > 0 AND                                   
300000                  WS-VECKA-FROM  < 53)   AND                              
310000                  WS-FROM-NUM <= WS-TOM-NUM                               
320000                 CONTINUE                                                 
330000              ELSE                                                        
340000                 IF (MID-IDARTNR-IN = ALL '+' AND                         
350000                     MID-IDARTNR-UT = ALL SPACE) OR                       
360000                     MID-IDARTNR-IN = ALL SPACE                           
370000                     MOVE NEJ TO NYCKLAR-SW                               
380000                 END-IF                                                   
390000              END-IF                                                      
400000           ELSE                                                           
410000             MOVE NEJ TO NYCKLAR-SW                                       
420000           END-IF                                                         
430000         IF MID-IDDISTR-IN  = ALL '+' AND                                 
440000              MID-IDDISTR-UT = ALL '+' AND                                
450000                MID-IDFKNGRP-IN  = ALL '+' AND                            
460000                  MID-IDFKNGRP-UT = ALL '+' AND                           
470000                    MID-IDARTNR-IN  = ALL '+' AND                         
480000                      MID-IDARTNR-UT = ALL '+'                            
490000                     IF NOT MFS-NEXT AND NOT                              
500000                       MFS-SPLIT                                          
510000                       MOVE NEJ TO TIAAVV-SW                              
520000                     END-IF                                               
530000         ELSE                                                             
540000           IF MID-IDDISTR-IN = '   0'                                     
550000             IF NOT MFS-NEXT AND NOT                                      
560000               MFS-SPLIT                                                  
570000               MOVE '7' TO MFS-IDPFK                                      
580000               MOVE NEJ TO TIAAVV-SW                                      
590000             END-IF                                                       
600000           END-IF                                                         
610000           IF MID-IDFKNGRP-IN = '   0'                                    
620000             IF NOT MFS-NEXT AND NOT                                      
630000               MFS-SPLIT                                                  
640000               MOVE '7' TO MFS-IDPFK                                      
650000               MOVE NEJ TO TIAAVV-SW                                      
660000             END-IF                                                       
670000           END-IF                                                         
680000           IF MID-IDARTNR-IN = '       0'                                 
690000             IF NOT MFS-NEXT AND NOT                                      
700000               MFS-SPLIT                                                  
710000               MOVE '7' TO MFS-IDPFK                                      
720000               MOVE NEJ TO TIAAVV-SW                                      
730000             END-IF                                                       
740000           END-IF                                                         
750000         END-IF                                                           
760000       ELSE                                                               
770000        IF NOT MFS-NEXT AND NOT                                           
780000               MFS-SPLIT                                                  
790000           MOVE '7' TO MFS-IDPFK                                          
800000        END-IF                                                            
810000        MOVE JA TO TIAAVV-SW                                              
820000        IF MID-TIAAVV-FOM-IN NOT = ALL '+'                                
830000        INSPECT MID-TIAAVV-FOM-IN REPLACING LEADING SPACE BY ZERO         
840000         MOVE MID-TIAAVV-FOM-IN  TO WS-AAVV-FROM                          
850000         MOVE W-FROM-IN          TO WS-FROM-NUM                           
860000                                    WS-FROM-IN                            
870000                                    SPAR-TIAAVV-FOM                       
880000        ELSE                                                              
890000          MOVE MID-TIAAVV-FOM-UT  TO WS-AAVV-FROM                         
900000          MOVE W-FROM-IN          TO WS-FROM-NUM                          
910000                                     WS-FROM-IN                           
920000                                     SPAR-TIAAVV-FOM                      
930000        END-IF                                                            
940000        IF MID-TIAAVV-TOM-IN NOT = ALL '+'                                
950000        INSPECT MID-TIAAVV-TOM-IN REPLACING LEADING SPACE BY ZERO         
960000         MOVE MID-TIAAVV-TOM-IN TO WS-AAVV-TOM                            
970000         MOVE W-TOM-IN          TO WS-TOM-NUM                             
980000                                    WS-TOM-IN                             
990000                                    SPAR-TIAAVV-TOM                       
000000        ELSE                                                              
010000          MOVE MID-TIAAVV-TOM-UT TO WS-AAVV-TOM                           
020000          MOVE W-TOM-IN          TO WS-TOM-NUM                            
030000                                     WS-TOM-IN                            
040000                                     SPAR-TIAAVV-TOM                      
050000        END-IF                                                            
060000         IF (WS-FROM-NUM NUMERIC) AND                                     
070000            (WS-VECKA-FROM > 0 AND                                        
080000            WS-VECKA-FROM  < 53)   AND                                    
090000            WS-FROM-NUM <= WS-TOM-NUM                                     
100000            CONTINUE                                                      
110000         ELSE                                                             
120000            IF (MID-IDARTNR-IN = ALL '+' AND                              
130000                MID-IDARTNR-UT = ALL SPACE) OR                            
140000                MID-IDARTNR-IN = ALL SPACE                                
150000               MOVE NEJ TO NYCKLAR-SW                                     
160000            END-IF                                                        
170000         END-IF                                                           
180000                                                                          
190000         IF (WS-TOM-NUM NUMERIC) AND                                      
200000           (WS-VECKA-TOM > 0 AND                                          
210000           WS-VECKA-TOM  < 53)   AND                                      
220000           WS-TOM-NUM >= WS-FROM-NUM                                      
230000           CONTINUE                                                       
240000         ELSE                                                             
250000           IF (MID-IDARTNR-IN = ALL '+' AND                               
260000               MID-IDARTNR-UT = ALL SPACE) OR                             
270000               MID-IDARTNR-IN = ALL SPACE                                 
280000              MOVE NEJ TO NYCKLAR-SW                                      
290000           END-IF                                                         
300000         END-IF                                                           
310000        END-IF                                                            
320000                                                                          
330000       MOVE MID-IDFKNGRP-IN TO JAMFOR-IDFKNGRP                            
340000       MOVE MID-IDDISTR-IN TO JAMFOR-IDDISTR                              
350000       MOVE MID-IDARTNR-IN TO JAMFOR-IDARTNR                              
360000                                                                          
370000       INSPECT JAMFOR-IDFKNGRP REPLACING ALL SPACE BY ZERO                
380000       INSPECT JAMFOR-IDDISTR REPLACING ALL SPACE BY ZERO                 
390000       INSPECT JAMFOR-IDARTNR REPLACING ALL SPACE BY ZERO                 
400000       INSPECT JAMFOR-IDFKNGRP REPLACING ALL '+' BY ZERO                  
410000       INSPECT JAMFOR-IDDISTR REPLACING ALL '+' BY ZERO                   
420000       INSPECT JAMFOR-IDARTNR REPLACING ALL '+' BY ZERO                   
430000                                                                          
440000*** KONTROLL AV FUNKTIONSGRUPP                                            
450000       IF MID-IDFKNGRP-IN = ALL '+' AND                                   
460000         MID-IDFKNGRP-UT NOT = ALL SPACE                                  
470000         IF MID-IDFKNGRP-UT NUMERIC                                       
480000           IF JAMFOR-IDDISTR = ALL '0'                                    
490000             IF JAMFOR-IDARTNR = ALL '0'                                  
500000               MOVE JA TO IDFKNGRP-WS                                     
510000               MOVE MID-IDFKNGRP-UT TO MOD-IDFKNGRP-UT                    
520000                                       WS-IDFKNGRP                        
530000                                       SPAR-IDFKNGRP                      
540000             ELSE                                                         
550000               MOVE NEJ TO NYCKLAR-SW                                     
560000               MOVE MID-IDARTNR-IN    TO MOD-IDARTNR-UT                   
570000               MOVE MFS-ROER-EJ-FAELT TO  MOD-IDFKNGRP-UT                 
580000             END-IF                                                       
590000           ELSE                                                           
600000             MOVE NEJ TO NYCKLAR-SW                                       
610000             MOVE MID-IDDISTR-IN    TO MOD-IDDISTR-UT                     
620000             MOVE MFS-ROER-EJ-FAELT TO  MOD-IDFKNGRP-UT                   
630000           END-IF                                                         
640000         ELSE                                                             
650000           MOVE NEJ TO NYCKLAR-SW                                         
660000           MOVE MFS-ROER-EJ-FAELT   TO MOD-IDFKNGRP-UT                    
670000         END-IF                                                           
680000       END-IF                                                             
690000       MOVE MSGI-IDFKNGRP TO MID-IDFKNGRP-IN                              
700000                                                                          
710000       IF MID-IDFKNGRP-IN NOT = ALL '+' AND                               
720000         JAMFOR-IDFKNGRP NOT = ALL '0'                                    
730000         MOVE MID-IDFKNGRP-IN TO MOD-IDFKNGRP-UT                          
740000         IF JAMFOR-IDDISTR = ALL '0'                                      
750000           IF JAMFOR-IDARTNR = ALL '0'                                    
760000             IF MID-IDFKNGRP-IN NUMERIC                                   
770000               MOVE MID-IDFKNGRP-IN TO WS-IDFKNGRP                        
780000                                       SPAR-IDFKNGRP                      
790000                                       MOD-IDFKNGRP-UT                    
800000               MOVE JA TO IDFKNGRP-WS                                     
810000               IF TIAAVV-OK                                               
820000                 IF NOT MFS-NEXT AND NOT MFS-SPLIT                        
830000                   MOVE '7' TO MFS-IDPFK                                  
840000                 END-IF                                                   
850000               END-IF                                                     
860000             ELSE                                                         
870000               MOVE NEJ TO NYCKLAR-SW                                     
880000             END-IF                                                       
890000           ELSE                                                           
900000             MOVE NEJ TO NYCKLAR-SW                                       
910000             MOVE MID-IDARTNR-IN    TO MOD-IDARTNR-UT                     
920000           END-IF                                                         
930000         ELSE                                                             
940000           MOVE NEJ TO NYCKLAR-SW                                         
950000           MOVE MID-IDDISTR-IN      TO MOD-IDDISTR-UT                     
960000         END-IF                                                           
970000       END-IF                                                             
980000*** KONTROLL AV DISTRIKT                                                  
990000       IF MID-IDDISTR-IN = ALL '+' AND                                    
000000         MID-IDDISTR-UT NOT = ALL SPACE                                   
010000           IF MID-IDDISTR-UT NUMERIC                                      
020000             IF JAMFOR-IDFKNGRP = ALL '0'                                 
030000               IF JAMFOR-IDARTNR = ALL '0'                                
040000                 MOVE JA TO IDDISTR-WS                                    
050000                 MOVE MID-IDDISTR-UT TO MOD-IDDISTR-UT                    
060000                                        WS-IDDISTR                        
070000                                        SPAR-IDDISTR                      
080000               ELSE                                                       
090000                 MOVE NEJ TO NYCKLAR-SW                                   
100000                 MOVE MID-IDARTNR-IN TO MOD-IDARTNR-UT                    
110000                 MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-UT                 
120000               END-IF                                                     
130000             ELSE                                                         
140000               MOVE NEJ TO NYCKLAR-SW                                     
150000               MOVE MID-IDFKNGRP-IN   TO MOD-IDFKNGRP-UT                  
160000               MOVE MFS-ROER-EJ-FAELT TO  MOD-IDDISTR-UT                  
170000             END-IF                                                       
180000           ELSE                                                           
190000             MOVE NEJ TO NYCKLAR-SW                                       
200000             MOVE MFS-ROER-EJ-FAELT   TO MOD-IDDISTR-UT                   
210000           END-IF                                                         
220000       END-IF                                                             
230000       MOVE MSGI-IDDISTR  TO MID-IDDISTR-IN                               
240000                                                                          
250000       INSPECT MID-IDDISTR-IN REPLACING LEADING SPACE BY ZERO             
260000       IF MID-IDDISTR-IN NOT = ALL '+' AND                                
270000         JAMFOR-IDDISTR NOT = ALL '0'                                     
280000         MOVE MID-IDDISTR-IN TO MOD-IDDISTR-UT                            
290000         IF JAMFOR-IDFKNGRP = ALL '0'                                     
300000           IF JAMFOR-IDARTNR = ALL '0'                                    
310000             IF MID-IDDISTR-IN NUMERIC                                    
320000               MOVE MID-IDDISTR-IN TO WS-IDDISTR                          
330000                                      SPAR-IDDISTR                        
340000                                      MOD-IDDISTR-UT                      
350000               MOVE JA TO IDDISTR-WS                                      
360000               IF TIAAVV-OK                                               
370000                 IF NOT MFS-NEXT AND NOT MFS-SPLIT                        
380000                   MOVE '7' TO MFS-IDPFK                                  
390000                 END-IF                                                   
400000               END-IF                                                     
410000             ELSE                                                         
420000               MOVE NEJ TO NYCKLAR-SW                                     
430000             END-IF                                                       
440000           ELSE                                                           
450000             MOVE NEJ TO NYCKLAR-SW                                       
460000             MOVE MID-IDARTNR-IN    TO MOD-IDARTNR-UT                     
470000           END-IF                                                         
480000         ELSE                                                             
490000           MOVE NEJ TO NYCKLAR-SW                                         
500000           MOVE MID-IDFKNGRP-IN  TO MOD-IDFKNGRP-UT                       
510000         END-IF                                                           
520000       END-IF                                                             
530000*** KONTROLL AV IDARTNR                                                   
540000       IF MID-IDARTNR-IN = ALL '+' AND                                    
550000         MID-IDARTNR-UT NOT = ALL SPACE                                   
560000          IF MID-IDARTNR-UT NUMERIC                                       
570000            IF JAMFOR-IDFKNGRP = ALL '0'                                  
580000              IF JAMFOR-IDDISTR = ALL '0'                                 
590000                MOVE MID-IDARTNR-UT TO TEST-IDARTNR                       
600000                                       MOD-IDARTNR-UT                     
610000                PERFORM BA-KOLLA-BYTESART                                 
620000              ELSE                                                        
630000                MOVE NEJ TO NYCKLAR-SW                                    
640000                MOVE MID-IDDISTR-IN TO MOD-IDDISTR-UT                     
650000                MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UT                  
660000              END-IF                                                      
670000            ELSE                                                          
680000              MOVE NEJ TO NYCKLAR-SW                                      
690000              MOVE MID-IDFKNGRP-IN   TO MOD-IDFKNGRP-UT                   
700000              MOVE MFS-ROER-EJ-FAELT TO  MOD-IDARTNR-UT                   
710000            END-IF                                                        
720000          ELSE                                                            
730000            MOVE NEJ TO NYCKLAR-SW                                        
740000            MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UT                      
750000          END-IF                                                          
760000       END-IF                                                             
770000       IF EGEN-MID                                                        
780000         MOVE MSGI-IDARTNR(1:8)  TO MID-IDARTNR-IN                        
790000       ELSE                                                               
800000         MOVE MSGI-IDARTNR(2:8)  TO MID-IDARTNR-IN                        
810000       END-IF                                                             
820000                                                                          
830000       INSPECT MID-IDARTNR-IN REPLACING LEADING SPACE BY ZERO             
840000       IF MID-IDARTNR-IN NOT = ALL '+' AND                                
850000         JAMFOR-IDARTNR NOT = ALL '0'                                     
860000         MOVE MID-IDARTNR-IN TO MOD-IDARTNR-UT                            
870000         IF JAMFOR-IDFKNGRP = ALL '0'                                     
880000           IF JAMFOR-IDDISTR = ALL '0'                                    
890000             IF MID-IDARTNR-IN NUMERIC                                    
900000               MOVE MID-IDARTNR-IN TO TEST-IDARTNR                        
910000                                      MOD-IDARTNR-UT                      
920000               PERFORM BA-KOLLA-BYTESART                                  
930000               IF TIAAVV-OK                                               
940000                 IF NOT MFS-NEXT AND NOT MFS-SPLIT                        
950000                   MOVE '7' TO MFS-IDPFK                                  
960000                 END-IF                                                   
970000               END-IF                                                     
980000             ELSE                                                         
990000               MOVE NEJ TO NYCKLAR-SW                                     
000000             END-IF                                                       
010000           ELSE                                                           
020000             MOVE NEJ TO NYCKLAR-SW                                       
030000             MOVE MID-IDDISTR-IN    TO MOD-IDDISTR-UT                     
040000           END-IF                                                         
050000         ELSE                                                             
060000           MOVE NEJ TO NYCKLAR-SW                                         
070000           MOVE MID-IDFKNGRP-IN  TO MOD-IDFKNGRP-UT                       
080000         END-IF                                                           
090000       END-IF                                                             
100000     ELSE                                                                 
110000       MOVE WS-AAVV           TO WS-AAVV-FROM                             
120000                                 MSGI-TIAAVV-FOM                          
130000                                 MSGI-TIAAVV-TOM                          
140000                                 WS-AAVV-TOM                              
150000                                 MOD-TIAAVV-FOM-UT                        
160000                                 MOD-TIAAVV-TOM-UT                        
170000       MOVE WS-DATUM-X        TO WS-FROM-IN                               
180000                                 WS-TOM-IN                                
190000                                 SPAR-TIAAVV-FOM                          
200000                                 SPAR-TIAAVV-TOM                          
210000       MOVE ZERO              TO WS-IDDISTR                               
220000                                 MSGI-IDDISTR                             
230000                                 SPAR-IDDISTR                             
240000                                 WS-IDFKNGRP                              
250000                                 MSGI-IDFKNGRP                            
260000                                 SPAR-IDFKNGRP                            
270000                                 WS-IDARTNR                               
280000                                 MSGI-IDARTNR                             
290000                                 SPAR-IDARTNR                             
300000     END-IF                                                               
310000                                                                          
320000     IF IDDISTR-OK                                                        
330000       IF IDARTNR-OK OR IDFKNGRP-OK                                       
340000         MOVE NEJ TO NYCKLAR-SW                                           
350000       END-IF                                                             
360000     END-IF                                                               
370000                                                                          
380000     IF IDARTNR-OK                                                        
390000       IF IDDISTR-OK OR IDFKNGRP-OK                                       
400000         MOVE NEJ TO NYCKLAR-SW                                           
410000       END-IF                                                             
420000     END-IF                                                               
430000                                                                          
440000     IF IDFKNGRP-OK                                                       
450000       IF IDDISTR-OK OR IDARTNR-OK                                        
460000         MOVE NEJ TO NYCKLAR-SW                                           
470000       END-IF                                                             
480000     END-IF                                                               
490000                                                                          
500000     MOVE MFS-RENSA-FAELT TO MOD-TIAAVV-FOM-IN                            
510000                             MOD-TIAAVV-TOM-IN                            
520000                             MOD-IDDISTR-IN                               
530000                             MOD-IDFKNGRP-IN                              
540000                             MOD-IDARTNR-IN                               
550000                                                                          
560000                                                                          
570000     IF GODK-MID                                                          
580000       MOVE WS-AAVV-FROM      TO MOD-TIAAVV-FOM-UT                        
590000       INSPECT MOD-TIAAVV-FOM-UT REPLACING LEADING SPACE                  
600000       BY ZERO                                                            
610000       MOVE WS-AAVV-TOM       TO MOD-TIAAVV-TOM-UT                        
620000       INSPECT MOD-TIAAVV-FOM-UT REPLACING LEADING SPACE                  
630000       BY ZERO                                                            
640000     ELSE                                                                 
650000       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                             
660000                               MOD-IDFKNGRP-UT                            
670000                               MOD-IDARTNR-UT                             
680000     END-IF                                                               
690000                                                                          
700000     IF NYCKLAR-FEL AND GODK-MID                                          
710000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
720000       CALL WMEDKONV USING MED-WMEDAREA                                   
730000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
740000       PERFORM MFS-RENSA-FAELT-IN                                         
750000     END-IF                                                               
760000     .                                                                    
770000     EJECT                                                                
780000 BA-KOLLA-BYTESART SECTION.                                               
790000     IF NOT BYT03-OBJEKT AND NOT BYT02-RENOV                              
800000       MOVE NEJ            TO NYCKLAR-SW                                  
810000     ELSE                                                                 
820000       IF BYT02-RENOV                                                     
830000         IF BYT16-BYTES                                                   
840000           COMPUTE TEST-IDARTNR = TEST-IDARTNR +                          
850000                                  6000                                    
860000           END-COMPUTE                                                    
870000         ELSE                                                             
880000           COMPUTE TEST-IDARTNR = TEST-IDARTNR +                          
890000                                  1000                                    
900000           END-COMPUTE                                                    
910000         END-IF                                                           
920000       END-IF                                                             
930000       MOVE TEST-IDARTNR TO WS-IDARTNR                                    
940000                            SPAR-IDARTNR                                  
950000       MOVE JA TO IDARTNR-SW                                              
960000     END-IF                                                               
970000     .                                                                    
980000     EJECT                                                                
990000 C-FOERSTA-SIDA SECTION.                                                  
000000                                                                          
010000     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
020000     CALL WMEDKONV USING MED-WMEDAREA                                     
030000     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
040000                                                                          
050000     MOVE  SPAR-TIAAVV-FOM    TO WS-FROM-IN                               
060000                                 W-FROM-IN                                
070000     MOVE  SPAR-TIAAVV-TOM    TO WS-TOM-IN                                
080000                                 W-TOM-IN                                 
090000     IF SPAR-IDDISTR NUMERIC                                              
            MOVE SPAR-IDDISTR        TO WS-IDDISTR                              
           ELSE                                                                 
            MOVE ZERO TO WS-IDDISTR                                             
           END-IF                                                               
110000     MOVE  SPAR-IDFKNGRP      TO WS-IDFKNGRP                              
120000     MOVE  SPAR-IDARTNR       TO WS-IDARTNR                               
130000     PERFORM MFS-RENSA-FAELT-IN                                           
140000     .                                                                    
150000     EJECT                                                                
160000 D-NAESTA-SIDA SECTION.                                                   
170000                                                                          
180000     IF SPAR-IDTRANS = '3154'                                             
190000       MOVE SPAR-IDARTNR-NEXT    TO W-IDARTNR-MIN                         
200000       MOVE SPAR-TIAAVV-FOM      TO WS-FROM-IN                            
210000                                    W-FROM-IN                             
220000       MOVE SPAR-TIAAVV-TOM      TO WS-TOM-IN                             
230000                                    W-TOM-IN                              
             IF SPAR-IDDISTR NUMERIC                                            
              MOVE SPAR-IDDISTR        TO WS-IDDISTR                            
             ELSE                                                               
              MOVE ZERO TO WS-IDDISTR                                           
             END-IF                                                             
250000       MOVE SPAR-IDFKNGRP        TO WS-IDFKNGRP                           
260000       MOVE SPAR-IDARTNR         TO WS-IDARTNR                            
270000     ELSE                                                                 
280000       PERFORM MFS-RENSA-FAELT-IN                                         
290000     END-IF                                                               
300000     .                                                                    
310000     EJECT                                                                
320000 E-SAMMA-SIDA SECTION.                                                    
330000                                                                          
340000     IF SPAR-IDTRANS = '3154' OR '0551'                                   
350000       MOVE SPAR-IDARTNR-ENTER  TO WS-IDARTNR-ALFA                        
360000       IF WS-IDARTNR-ALFA NUMERIC                                         
370000         MOVE SPAR-IDARTNR-ENTER  TO W-IDARTNR-MIN                        
380000         MOVE SPAR-TIAAVV-FOM     TO WS-FROM-IN                           
390000                                     W-FROM-IN                            
400000         MOVE SPAR-TIAAVV-TOM     TO WS-TOM-IN                            
410000                                     W-TOM-IN                             
              IF SPAR-IDDISTR NUMERIC                                           
420000         MOVE SPAR-IDDISTR        TO WS-IDDISTR                           
              ELSE                                                              
               MOVE ZERO TO WS-IDDISTR                                          
              END-IF                                                            
430000         MOVE SPAR-IDFKNGRP       TO WS-IDFKNGRP                          
440000         MOVE SPAR-IDARTNR        TO WS-IDARTNR                           
450000         MOVE '8'                 TO MFS-IDPFK                            
460000       ELSE                                                               
470000         PERFORM MFS-RENSA-FAELT-IN                                       
480000       END-IF                                                             
490000     ELSE                                                                 
500000       PERFORM MFS-RENSA-FAELT-IN                                         
510000     END-IF                                                               
520000     .                                                                    
530000     EJECT                                                                
540000 F-LAES-VISA-INFO SECTION.                                                
550000                                                                          
560000     MOVE WS-AAVV-FROM TO MOD-TIAAVV-FOM-UT                               
570000     MOVE WS-AAVV-TOM  TO MOD-TIAAVV-TOM-UT                               
580000                                                                          
590000     IF IDFKNGRP-OK                                                       
600000       PERFORM FC-LAES-VECKA-FGRP                                         
610000     ELSE                                                                 
620000       IF IDARTNR-OK                                                      
640000         PERFORM FD-LAES-VECKA-ARTNR                                      
650000       ELSE                                                               
660000        IF IDDISTR-OK                                                     
670000          PERFORM FA-LAES-VECKA-DISTR                                     
680000        ELSE                                                              
690000          PERFORM FB-LAES-VECKA                                           
700000        END-IF                                                            
710000       END-IF                                                             
720000     END-IF                                                               
730000     .                                                                    
740000     EJECT                                                                
750000 FA-LAES-VECKA-DISTR SECTION.                                             
760000                                                                          
770000                                                                          
780000     IF MFS-FIRST OR MFS-ENTER                                            
790000       PERFORM FAA-LAES-VECKA-DISTR-FORSTA                                
800000     ELSE                                                                 
810000       PERFORM FAB-LAES-VECKA-DISTR-FLERA                                 
820000     END-IF                                                               
830000     MOVE MOD-TAB-IDARTNR (1)     TO WS-IDARTNR-ALFA                      
840000     IF WS-IDARTNR-ALFA = LOW-VALUE                                       
850000       MOVE ZERO                  TO MOD-TAB-IDARTNR(1)                   
860000                                     IDARTNR-WS                           
870000                                     SPAR-IDARTNR-ENTER                   
880000                                     SPAR-IDARTNR-NEXT                    
890000     END-IF                                                               
900000     IF ALLT-OK AND RAD-OK                                                
910000       MOVE MOD-TAB-IDARTNR (1)     TO SPAR-IDARTNR-ENTER                 
920000       IF BYLRAD-OK                                                       
930000         MOVE IDARTNR-WS             TO SPAR-IDARTNR-NEXT                 
940000         MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                      
950000         CALL WMEDKONV USING MED-WMEDAREA                                 
960000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
970000                                                                          
980000       ELSE                                                               
990000         MOVE MOD-TAB-IDARTNR (1) TO SPAR-IDARTNR-NEXT                    
000000         MOVE INF-LAST-PAGE TO MED-IDMFSINF                               
010000         CALL WMEDKONV USING MED-WMEDAREA                                 
020000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
030000                                                                          
040000         PERFORM UNTIL INDX > MAX-INDX                                    
050000           MOVE MFS-RENSA-FAELT TO MOD-CMD           (INDX)               
060000                                   MOD-TAB-IDARTNR   (INDX)               
070000                                   MOD-TAB-BEART-ENG (INDX)               
080000                                   MOD-TAB-OUTLEV    (INDX)               
090000                                   MOD-TAB-INLEV     (INDX)               
100000                                   MOD-TAB-DIFF      (INDX)               
110000                                   MOD-TAB-REPROCENT (INDX)               
120000           MOVE ZERO            TO SPAR-IDARTNR-TAB  (INDX)               
130000                                  SPAR-OUTLEV-TAB    (INDX)               
140000                                  SPAR-INLEV-TAB     (INDX)               
150000                                  SPAR-DIFF-TAB      (INDX)               
160000                                  SPAR-REPROCENT-TAB (INDX)               
170000           MOVE SPACE          TO SPAR-BEART-ENG-TAB (INDX)               
180000           ADD +1               TO INDX                                   
190000         END-PERFORM                                                      
200000       END-IF                                                             
210000       MOVE '002'  TO MSGI-KDCALL                                         
220000       MOVE '3154' TO SPAR-IDTRANS                                        
230000       MOVE SPAR-AREA TO MSGI-SPAR-AREA                                   
240000       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
250000     ELSE                                                                 
260000       MOVE INFORMATION-MISSING TO MED-IDMFSFEL                           
270000       CALL WMEDKONV USING MED-WMEDAREA                                   
280000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
290000       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
300000     END-IF                                                               
310000                                                                          
320000     .                                                                    
330000     EJECT                                                                
340000 FAA-LAES-VECKA-DISTR-FORSTA SECTION.                                     
350000                                                                          
360000       PERFORM DB2-DCL-OPN-CRS-BYLRAD-DISTR                               
370000       PERFORM DB2-FETCH-RADDIST                                          
380000                                                                          
390000       MOVE +1 TO INDX                                                    
400000       IF BYLRAD-OK                                                       
410000         MOVE BYLRAD-IDARTNR  TO SPAR-IDARTNR-ENTER                       
420000                                 W-IDARTNR-MIN                            
430000                                 IDARTNR-WS                               
440000                                 W-601-IDARTNR                            
450000                                 W-628-IDARTNR                            
460000         MOVE JA TO ALLT-SW                                               
470000       ELSE                                                               
480000         MOVE NEJ TO ALLT-SW                                              
490000       END-IF                                                             
500000                                                                          
510000       PERFORM UNTIL INDX > MAX-INDX OR BYLRAD-SAKNAS                     
520000         MOVE ZERO TO W-IN                                                
530000                      W-OUT                                               
540000                      WS-PROCENT                                          
550000                      WS-KVANTAL                                          
560000                                                                          
570000         PERFORM UNTIL BYLRAD-IDARTNR NOT = IDARTNR-WS                    
580000                                OR BYLRAD-SAKNAS                          
590000           IF BYLRAD-IDPTYP = 'RET'                                       
600000              MOVE BYLRAD-IDDISTR TO EXCL-IDDISTR                         
610000              IF EXCL-IDDISTR-OK                                          
620000                  CONTINUE                                                
630000              ELSE                                                        
640000                IF BYLRAD-KDBYTREF = '019' OR '029' OR '030'              
650000                                OR '032' OR '041' OR '050'                
660000                  CONTINUE                                                
670000                ELSE                                                      
680000                  ADD BYLRAD-KVANTAL   TO W-IN                            
690000                END-IF                                                    
700000              END-IF                                                      
710000           ELSE                                                           
720000             IF BYLRAD-IDPTYP = 'KRE'                                     
730000                COMPUTE WS-KVANTAL = BYLRAD-KVANTAL * -1                  
740000                ADD WS-KVANTAL     TO W-OUT                               
750000             ELSE                                                         
760000               IF BYLRAD-IDPTYP = 'FAK'                                   
770000                  ADD BYLRAD-KVANTAL TO W-OUT                             
780000               END-IF                                                     
790000             END-IF                                                       
800000           END-IF                                                         
810000           PERFORM DB2-FETCH-RADDIST                                      
820000         END-PERFORM                                                      
830000                                                                          
840000         MOVE BYLRAD-IDARTNR      TO WS-S01-IDARTNR                       
850000         PERFORM S01-RAEKNA-ACKUMULERAT                                   
860000       END-PERFORM                                                        
870000                                                                          
880000       PERFORM DB2-CLOSE-RADDIST-CRS                                      
890000       .                                                                  
900000       EJECT                                                              
910000 FAB-LAES-VECKA-DISTR-FLERA SECTION.                                      
920000                                                                          
930000       PERFORM DB2-DCL-OPN-CRS-BYLRAD-DISTART                             
940000       PERFORM DB2-FETCH-RADDIST-IDART                                    
950000       MOVE +1 TO INDX                                                    
960000       IF BYLRAD-OK                                                       
970000         MOVE BYLRAD-IDARTNR  TO SPAR-IDARTNR-ENTER                       
980000                                 IDARTNR-WS                               
990000                                 W-IDARTNR-MIN                            
000000                                 W-601-IDARTNR                            
010000                                 W-628-IDARTNR                            
020000         MOVE JA TO ALLT-SW                                               
030000       ELSE                                                               
040000         MOVE NEJ TO ALLT-SW                                              
050000       END-IF                                                             
060000                                                                          
070000       PERFORM UNTIL INDX > MAX-INDX OR BYLRAD-SAKNAS                     
080000                                                                          
090000         MOVE ZERO TO W-IN                                                
100000                      W-OUT                                               
110000                      WS-PROCENT                                          
120000                      WS-KVANTAL                                          
130000                                                                          
140000         PERFORM UNTIL BYLRAD-IDARTNR NOT = IDARTNR-WS                    
150000                                OR BYLRAD-SAKNAS                          
160000           IF BYLRAD-IDPTYP = 'RET'                                       
170000                MOVE BYLRAD-IDDISTR TO EXCL-IDDISTR                       
180000             IF EXCL-IDDISTR-OK                                           
190000                CONTINUE                                                  
200000             ELSE                                                         
210000               IF BYLRAD-KDBYTREF = '019' OR '029' OR '030'               
220000                                OR '032' OR '041' OR '050'                
230000                 CONTINUE                                                 
240000               ELSE                                                       
250000                 ADD BYLRAD-KVANTAL   TO W-IN                             
260000               END-IF                                                     
270000             END-IF                                                       
280000           ELSE                                                           
290000             IF BYLRAD-IDPTYP = 'KRE'                                     
300000                COMPUTE WS-KVANTAL = BYLRAD-KVANTAL * -1                  
310000                ADD WS-KVANTAL     TO W-OUT                               
320000             ELSE                                                         
330000               IF BYLRAD-IDPTYP = 'FAK'                                   
340000                  ADD BYLRAD-KVANTAL TO W-OUT                             
350000               END-IF                                                     
360000             END-IF                                                       
370000           END-IF                                                         
380000           PERFORM DB2-FETCH-RADDIST-IDART                                
390000         END-PERFORM                                                      
400000                                                                          
410000         MOVE BYLRAD-IDARTNR      TO WS-S01-IDARTNR                       
420000         PERFORM S01-RAEKNA-ACKUMULERAT                                   
430000       END-PERFORM                                                        
440000                                                                          
450000       PERFORM DB2-CLOSE-DISTART-CRS                                      
460000       .                                                                  
470000       EJECT                                                              
480000 FB-LAES-VECKA SECTION.                                                   
490000                                                                          
500000     MOVE W-FROM-IN           TO SPAR-TIAAVV-FOM                          
510000     MOVE W-TOM-IN            TO SPAR-TIAAVV-TOM                          
520000                                                                          
530000     IF MFS-FIRST OR MFS-ENTER                                            
540000       PERFORM FBA-LAES-VECKA-FORSTA                                      
550000     ELSE                                                                 
560000       PERFORM FBB-LAES-VECKA-FLERA                                       
570000     END-IF                                                               
580000                                                                          
590000     MOVE MOD-TAB-IDARTNR (1)     TO WS-IDARTNR-ALFA                      
600000     IF WS-IDARTNR-ALFA = LOW-VALUE                                       
610000       MOVE ZERO                  TO MOD-TAB-IDARTNR(1)                   
620000                                     IDARTNR-WS                           
630000                                     SPAR-IDARTNR-ENTER                   
640000                                     SPAR-IDARTNR-NEXT                    
650000     END-IF                                                               
660000     IF ALLT-OK AND RAD-OK                                                
670000       MOVE MOD-TAB-IDARTNR (1)   TO SPAR-IDARTNR-ENTER                   
680000       IF BYLACK-OK                                                       
690000         MOVE IDARTNR-WS          TO SPAR-IDARTNR-NEXT                    
700000         MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                      
710000         CALL WMEDKONV USING MED-WMEDAREA                                 
720000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
730000                                                                          
740000       ELSE                                                               
750000         MOVE MOD-TAB-IDARTNR (1) TO SPAR-IDARTNR-NEXT                    
760000         MOVE INF-LAST-PAGE TO MED-IDMFSINF                               
770000         CALL WMEDKONV USING MED-WMEDAREA                                 
780000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
790000                                                                          
800000         PERFORM UNTIL INDX > MAX-INDX                                    
810000           MOVE MFS-RENSA-FAELT TO MOD-CMD           (INDX)               
820000                                   MOD-TAB-IDARTNR   (INDX)               
830000                                   MOD-TAB-BEART-ENG (INDX)               
840000                                   MOD-TAB-OUTLEV    (INDX)               
850000                                   MOD-TAB-INLEV     (INDX)               
860000                                   MOD-TAB-DIFF      (INDX)               
870000                                   MOD-TAB-REPROCENT (INDX)               
880000           MOVE ZERO            TO SPAR-IDARTNR-TAB  (INDX)               
890000                                  SPAR-OUTLEV-TAB    (INDX)               
900000                                  SPAR-INLEV-TAB     (INDX)               
910000                                  SPAR-DIFF-TAB      (INDX)               
920000                                  SPAR-REPROCENT-TAB (INDX)               
930000           MOVE SPACE          TO SPAR-BEART-ENG-TAB (INDX)               
940000           ADD +1             TO INDX                                     
950000         END-PERFORM                                                      
960000       END-IF                                                             
970000       MOVE '002'  TO MSGI-KDCALL                                         
980000       MOVE '3154' TO SPAR-IDTRANS                                        
990000       MOVE SPAR-AREA TO MSGI-SPAR-AREA                                   
000000       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
010000     ELSE                                                                 
020000       MOVE INFORMATION-MISSING TO MED-IDMFSFEL                           
030000       CALL WMEDKONV USING MED-WMEDAREA                                   
040000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
050000       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
060000     END-IF                                                               
070000     .                                                                    
080000     EJECT                                                                
090000 FBA-LAES-VECKA-FORSTA SECTION.                                           
100000                                                                          
110000     PERFORM DB2-DCL-OPN-CRS-BYLACK                                       
120000     PERFORM DB2-FETCH-BYLACK                                             
130000     MOVE +1 TO INDX                                                      
140000     IF BYLACK-OK                                                         
150000       MOVE BYLACK-IDARTNR  TO SPAR-IDARTNR-ENTER                         
160000                               W-IDARTNR-MIN                              
170000                               IDARTNR-WS                                 
180000                               W-601-IDARTNR                              
190000                               W-628-IDARTNR                              
200000       MOVE JA TO ALLT-SW                                                 
210000     ELSE                                                                 
220000       MOVE NEJ TO ALLT-SW                                                
230000     END-IF                                                               
240000                                                                          
250000     PERFORM UNTIL INDX > MAX-INDX OR BYLACK-SAKNAS                       
260000                                                                          
270000       MOVE ZERO TO W-IN                                                  
280000                    W-OUT                                                 
290000                    WS-PROCENT                                            
300000                    WS-KVANTAL                                            
310000                                                                          
320000       PERFORM UNTIL BYLACK-IDARTNR NOT = IDARTNR-WS                      
330000                              OR BYLACK-SAKNAS                            
340000         IF BYLACK-IDPTYP = 'RET'                                         
350000               ADD BYLACK-KVANTAL   TO W-IN                               
360000         ELSE                                                             
370000           IF BYLACK-IDPTYP = 'KRE'                                       
380000              COMPUTE WS-KVANTAL = BYLACK-KVANTAL * -1                    
390000              ADD WS-KVANTAL     TO W-OUT                                 
400000           ELSE                                                           
410000             IF BYLACK-IDPTYP = 'FAK'                                     
420000                ADD BYLACK-KVANTAL TO W-OUT                               
430000             END-IF                                                       
440000           END-IF                                                         
450000         END-IF                                                           
460000         PERFORM DB2-FETCH-BYLACK                                         
470000       END-PERFORM                                                        
480000                                                                          
490000       MOVE BYLACK-IDARTNR      TO WS-S01-IDARTNR                         
500000       PERFORM S01-RAEKNA-ACKUMULERAT                                     
510000     END-PERFORM                                                          
520000                                                                          
530000     PERFORM DB2-CLOSE-BYLACK-CRS                                         
540000     .                                                                    
550000     EJECT                                                                
560000 FBB-LAES-VECKA-FLERA SECTION.                                            
570000                                                                          
580000     PERFORM DB2-DCL-OPN-CRS-IDART-BYLACK                                 
590000     PERFORM DB2-FETCH-BYLACK-IDART                                       
600000     MOVE +1 TO INDX                                                      
610000     IF BYLACK-OK                                                         
620000       MOVE BYLACK-IDARTNR  TO SPAR-IDARTNR-ENTER                         
630000                               IDARTNR-WS                                 
640000                               W-IDARTNR-MIN                              
650000                               W-601-IDARTNR                              
660000                               W-628-IDARTNR                              
670000       MOVE JA TO ALLT-SW                                                 
680000     ELSE                                                                 
690000       MOVE NEJ TO ALLT-SW                                                
700000     END-IF                                                               
710000                                                                          
720000     PERFORM UNTIL INDX > MAX-INDX OR BYLACK-SAKNAS                       
730000                                                                          
740000       MOVE ZERO TO W-IN                                                  
750000                    W-OUT                                                 
760000                    WS-PROCENT                                            
770000                    WS-KVANTAL                                            
780000                                                                          
790000       PERFORM UNTIL BYLACK-IDARTNR NOT = IDARTNR-WS                      
800000                              OR BYLACK-SAKNAS                            
810000         IF BYLACK-IDPTYP = 'RET'                                         
820000                ADD BYLACK-KVANTAL   TO W-IN                              
830000         ELSE                                                             
840000           IF BYLACK-IDPTYP = 'KRE'                                       
850000              COMPUTE WS-KVANTAL = BYLACK-KVANTAL * -1                    
860000              ADD WS-KVANTAL     TO W-OUT                                 
870000           ELSE                                                           
880000             IF BYLACK-IDPTYP = 'FAK'                                     
890000                ADD BYLACK-KVANTAL TO W-OUT                               
900000             END-IF                                                       
910000           END-IF                                                         
920000         END-IF                                                           
930000         PERFORM DB2-FETCH-BYLACK-IDART                                   
940000       END-PERFORM                                                        
950000                                                                          
960000       MOVE BYLACK-IDARTNR      TO WS-S01-IDARTNR                         
970000       PERFORM S01-RAEKNA-ACKUMULERAT                                     
980000     END-PERFORM                                                          
990000                                                                          
000000     PERFORM DB2-CLOSE-BYLIDART-CRS                                       
010000     .                                                                    
020000     EJECT                                                                
030000 FC-LAES-VECKA-FGRP SECTION.                                              
040000     IF MFS-FIRST OR MFS-ENTER                                            
050000       PERFORM FCA-LAES-VECKA-FGRP-FORSTA                                 
060000     ELSE                                                                 
070000       PERFORM FCB-LAES-VECKA-FGRP-FLERA                                  
080000     END-IF                                                               
090000                                                                          
100000     MOVE MOD-TAB-IDARTNR (1)     TO WS-IDARTNR-ALFA                      
110000     IF WS-IDARTNR-ALFA = LOW-VALUE                                       
120000       MOVE ZERO                  TO MOD-TAB-IDARTNR(1)                   
130000                                     IDARTNR-WS                           
140000                                     SPAR-IDARTNR-ENTER                   
150000                                     SPAR-IDARTNR-NEXT                    
160000     END-IF                                                               
170000     IF ALLT-OK AND RAD-OK                                                
180000       MOVE MOD-TAB-IDARTNR (1)   TO SPAR-IDARTNR-ENTER                   
190000       IF BYLRAD-OK                                                       
200000         MOVE IDARTNR-WS          TO SPAR-IDARTNR-NEXT                    
210000         MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                      
220000         CALL WMEDKONV USING MED-WMEDAREA                                 
230000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
240000                                                                          
250000       ELSE                                                               
260000         MOVE MOD-TAB-IDARTNR (1) TO SPAR-IDARTNR-NEXT                    
270000         MOVE INF-LAST-PAGE TO MED-IDMFSINF                               
280000         CALL WMEDKONV USING MED-WMEDAREA                                 
290000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
300000                                                                          
310000         PERFORM UNTIL INDX > MAX-INDX                                    
320000           MOVE MFS-RENSA-FAELT TO MOD-CMD           (INDX)               
330000                                   MOD-TAB-IDARTNR   (INDX)               
340000                                   MOD-TAB-BEART-ENG (INDX)               
350000                                   MOD-TAB-OUTLEV    (INDX)               
360000                                   MOD-TAB-INLEV     (INDX)               
370000                                   MOD-TAB-DIFF      (INDX)               
380000                                   MOD-TAB-REPROCENT (INDX)               
390000           MOVE ZERO            TO SPAR-IDARTNR-TAB  (INDX)               
400000                                  SPAR-OUTLEV-TAB    (INDX)               
410000                                  SPAR-INLEV-TAB     (INDX)               
420000                                  SPAR-DIFF-TAB      (INDX)               
430000                                  SPAR-REPROCENT-TAB (INDX)               
440000           MOVE SPACE          TO SPAR-BEART-ENG-TAB (INDX)               
450000           ADD +1             TO INDX                                     
460000         END-PERFORM                                                      
470000       END-IF                                                             
480000       MOVE '002'  TO MSGI-KDCALL                                         
490000       MOVE '3154' TO SPAR-IDTRANS                                        
500000       MOVE SPAR-AREA TO MSGI-SPAR-AREA                                   
510000       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
520000     ELSE                                                                 
530000       MOVE INFORMATION-MISSING TO MED-IDMFSFEL                           
540000       CALL WMEDKONV USING MED-WMEDAREA                                   
550000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
560000       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
570000     END-IF                                                               
580000     .                                                                    
590000     EJECT                                                                
600000 FCA-LAES-VECKA-FGRP-FORSTA SECTION.                                      
610000                                                                          
620000       PERFORM DB2-DCL-OPN-CRS-BYLRAD-FGRP                                
630000       PERFORM DB2-FETCH-RADFGRP                                          
640000                                                                          
650000       MOVE +1 TO INDX                                                    
660000       IF BYLRAD-OK                                                       
670000         MOVE BYLRAD-IDARTNR  TO SPAR-IDARTNR-ENTER                       
680000                                 W-IDARTNR-MIN                            
690000                                 IDARTNR-WS                               
700000                                 W-601-IDARTNR                            
710000                                 W-628-IDARTNR                            
720000         MOVE JA TO ALLT-SW                                               
730000       ELSE                                                               
740000         MOVE NEJ TO ALLT-SW                                              
750000       END-IF                                                             
760000                                                                          
770000       PERFORM UNTIL INDX > MAX-INDX OR BYLRAD-SAKNAS                     
780000                                                                          
790000         MOVE ZERO TO W-IN                                                
800000                      W-OUT                                               
810000                      WS-PROCENT                                          
820000                      WS-KVANTAL                                          
830000                                                                          
840000         PERFORM UNTIL BYLRAD-IDARTNR NOT = IDARTNR-WS                    
850000                                OR BYLRAD-SAKNAS                          
860000           IF BYLRAD-IDPTYP = 'RET'                                       
870000             MOVE BYLRAD-IDDISTR TO EXCL-IDDISTR                          
880000             IF EXCL-IDDISTR-OK                                           
890000                CONTINUE                                                  
900000             ELSE                                                         
910000               IF BYLRAD-KDBYTREF = '019' OR '029' OR '030'               
920000                               OR '032' OR '041' OR '050'                 
930000                 CONTINUE                                                 
940000               ELSE                                                       
950000                   ADD BYLRAD-KVANTAL   TO W-IN                           
960000               END-IF                                                     
970000             END-IF                                                       
980000           ELSE                                                           
990000             IF BYLRAD-IDPTYP = 'KRE'                                     
000000                COMPUTE WS-KVANTAL = BYLRAD-KVANTAL * -1                  
010000                ADD WS-KVANTAL     TO W-OUT                               
020000             ELSE                                                         
030000               IF BYLRAD-IDPTYP = 'FAK'                                   
040000                  ADD BYLRAD-KVANTAL TO W-OUT                             
050000               END-IF                                                     
060000             END-IF                                                       
070000           END-IF                                                         
080000           PERFORM DB2-FETCH-RADFGRP                                      
090000         END-PERFORM                                                      
100000                                                                          
110000         MOVE BYLRAD-IDARTNR      TO WS-S01-IDARTNR                       
120000         PERFORM S01-RAEKNA-ACKUMULERAT                                   
130000                                                                          
140000       END-PERFORM                                                        
150000       PERFORM DB2-CLOSE-RADFGRP-CRS                                      
160000       .                                                                  
170000       EJECT                                                              
180000 FCB-LAES-VECKA-FGRP-FLERA SECTION.                                       
190000                                                                          
200000       PERFORM DB2-DCL-OPN-CRS-BYLRAD-FGRPART                             
210000       PERFORM DB2-FETCH-RADFGRP-IDART                                    
220000       MOVE +1 TO INDX                                                    
230000       IF BYLRAD-OK                                                       
240000         MOVE BYLRAD-IDARTNR  TO SPAR-IDARTNR-ENTER                       
250000                                 IDARTNR-WS                               
260000                                 W-IDARTNR-MIN                            
270000                                 W-601-IDARTNR                            
280000                                 W-628-IDARTNR                            
290000         MOVE JA TO ALLT-SW                                               
300000       ELSE                                                               
310000         MOVE NEJ TO ALLT-SW                                              
320000       END-IF                                                             
330000                                                                          
340000       PERFORM UNTIL INDX > MAX-INDX OR BYLRAD-SAKNAS                     
350000                                                                          
360000         MOVE ZERO TO W-IN                                                
370000                      W-OUT                                               
380000                      WS-PROCENT                                          
390000                      WS-KVANTAL                                          
400000                                                                          
410000         PERFORM UNTIL BYLRAD-IDARTNR NOT = IDARTNR-WS                    
420000                                OR BYLRAD-SAKNAS                          
430000           IF BYLRAD-IDPTYP = 'RET'                                       
440000             MOVE BYLRAD-IDDISTR TO EXCL-IDDISTR                          
450000             IF EXCL-IDDISTR-OK                                           
460000                CONTINUE                                                  
470000             ELSE                                                         
480000               IF BYLRAD-KDBYTREF = '019' OR '029' OR '030'               
490000                               OR '032' OR '041' OR '050'                 
500000                 CONTINUE                                                 
510000               ELSE                                                       
520000                   ADD BYLRAD-KVANTAL   TO W-IN                           
530000               END-IF                                                     
540000             END-IF                                                       
550000           ELSE                                                           
560000             IF BYLRAD-IDPTYP = 'KRE'                                     
570000                COMPUTE WS-KVANTAL = BYLRAD-KVANTAL * -1                  
580000                ADD WS-KVANTAL     TO W-OUT                               
590000             ELSE                                                         
600000               IF BYLRAD-IDPTYP = 'FAK'                                   
610000                  ADD BYLRAD-KVANTAL TO W-OUT                             
620000               END-IF                                                     
630000             END-IF                                                       
640000           END-IF                                                         
650000           PERFORM DB2-FETCH-RADFGRP-IDART                                
660000         END-PERFORM                                                      
670000                                                                          
680000         MOVE BYLRAD-IDARTNR      TO WS-S01-IDARTNR                       
690000         PERFORM S01-RAEKNA-ACKUMULERAT                                   
700000                                                                          
710000       END-PERFORM                                                        
720000       PERFORM DB2-CLOSE-FGRPART-CRS                                      
730000       .                                                                  
740000       EJECT                                                              
750000 FD-LAES-VECKA-ARTNR SECTION.                                             
760000                                                                          
780000     PERFORM FDA-LAES-VECKA-ARTNR                                         
790000     MOVE MOD-TAB-IDARTNR (1)     TO WS-IDARTNR-ALFA                      
800000     IF WS-IDARTNR-ALFA = LOW-VALUE                                       
810000       MOVE ZERO                  TO MOD-TAB-IDARTNR(1)                   
820000                                     IDARTNR-WS                           
830000                                     SPAR-IDARTNR-ENTER                   
840000                                     SPAR-IDARTNR-NEXT                    
850000     END-IF                                                               
860000     IF ALLT-OK AND RAD-OK                                                
870000       MOVE MOD-TAB-IDARTNR (1)     TO SPAR-IDARTNR-ENTER                 
880000       IF BYLRAD-OK                                                       
890000         MOVE IDARTNR-WS             TO SPAR-IDARTNR-NEXT                 
900000         MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                      
910000         CALL WMEDKONV USING MED-WMEDAREA                                 
920000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
930000                                                                          
940000       ELSE                                                               
950000         MOVE MOD-TAB-IDARTNR (1) TO SPAR-IDARTNR-NEXT                    
960000         MOVE INF-LAST-PAGE TO MED-IDMFSINF                               
970000         CALL WMEDKONV USING MED-WMEDAREA                                 
980000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
990000                                                                          
000000         PERFORM UNTIL INDX > MAX-INDX                                    
010000           MOVE MFS-RENSA-FAELT TO MOD-CMD           (INDX)               
020000                                   MOD-TAB-IDARTNR   (INDX)               
030000                                   MOD-TAB-BEART-ENG (INDX)               
040000                                   MOD-TAB-OUTLEV    (INDX)               
050000                                   MOD-TAB-INLEV     (INDX)               
060000                                   MOD-TAB-DIFF      (INDX)               
070000                                   MOD-TAB-REPROCENT (INDX)               
080000           MOVE ZERO            TO SPAR-IDARTNR-TAB  (INDX)               
090000                                  SPAR-OUTLEV-TAB    (INDX)               
100000                                  SPAR-INLEV-TAB     (INDX)               
110000                                  SPAR-DIFF-TAB      (INDX)               
120000                                  SPAR-REPROCENT-TAB (INDX)               
130000           MOVE SPACE          TO SPAR-BEART-ENG-TAB (INDX)               
140000           ADD +1               TO INDX                                   
150000         END-PERFORM                                                      
160000       END-IF                                                             
170000       MOVE '002'  TO MSGI-KDCALL                                         
180000       MOVE '3154' TO SPAR-IDTRANS                                        
190000       MOVE SPAR-AREA TO MSGI-SPAR-AREA                                   
200000       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
210000     ELSE                                                                 
220000       MOVE INFORMATION-MISSING TO MED-IDMFSFEL                           
230000       CALL WMEDKONV USING MED-WMEDAREA                                   
240000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
250000       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
260000     END-IF                                                               
270000                                                                          
280000     .                                                                    
290000     EJECT                                                                
300000 FDA-LAES-VECKA-ARTNR SECTION.                                            
310000                                                                          
330000       PERFORM DB2-DCL-OPN-CRS-BYLRAD-ARTNR                               
340000       PERFORM DB2-FETCH-RADARTNR                                         
350000                                                                          
360000       MOVE +1 TO INDX                                                    
370000       IF BYLRAD-OK                                                       
380000         MOVE BYLRAD-IDARTNR  TO SPAR-IDARTNR-ENTER                       
390000                                 W-IDARTNR-MIN                            
400000                                 IDARTNR-WS                               
410000                                 W-601-IDARTNR                            
420000                                 W-628-IDARTNR                            
430000         MOVE JA TO ALLT-SW                                               
440000       ELSE                                                               
450000         MOVE NEJ TO ALLT-SW                                              
460000       END-IF                                                             
470000                                                                          
480000       PERFORM UNTIL BYLRAD-SAKNAS                                        
490000         MOVE ZERO TO W-IN                                                
500000                      W-OUT                                               
510000                      WS-PROCENT                                          
520000                      WS-KVANTAL                                          
530000                                                                          
540000         PERFORM UNTIL BYLRAD-IDARTNR NOT = IDARTNR-WS                    
550000                                OR BYLRAD-SAKNAS                          
560000           IF BYLRAD-IDPTYP = 'RET'                                       
570000             MOVE BYLRAD-IDDISTR TO EXCL-IDDISTR                          
580000             IF EXCL-IDDISTR-OK                                           
590000                CONTINUE                                                  
600000             ELSE                                                         
610000               IF BYLRAD-KDBYTREF = '019' OR '029' OR '030'               
620000                               OR '032' OR '041' OR '050'                 
630000                 CONTINUE                                                 
640000               ELSE                                                       
650000                  ADD BYLRAD-KVANTAL   TO W-IN                            
660000               END-IF                                                     
670000             END-IF                                                       
680000           ELSE                                                           
690000             IF BYLRAD-IDPTYP = 'KRE'                                     
700000                COMPUTE WS-KVANTAL = BYLRAD-KVANTAL * -1                  
710000                ADD WS-KVANTAL     TO W-OUT                               
720000             ELSE                                                         
730000               IF BYLRAD-IDPTYP = 'FAK'                                   
740000                 ADD BYLRAD-KVANTAL TO W-OUT                              
750000               END-IF                                                     
760000             END-IF                                                       
770000           END-IF                                                         
780000           PERFORM DB2-FETCH-RADARTNR                                     
790000         END-PERFORM                                                      
800000                                                                          
810000         MOVE BYLRAD-IDARTNR      TO WS-S01-IDARTNR                       
820000         PERFORM S01-RAEKNA-ACKUMULERAT                                   
830000                                                                          
840000       END-PERFORM                                                        
850000       PERFORM DB2-CLOSE-ARTRAD-CRS                                       
860000       .                                                                  
870000       EJECT                                                              
880000 G-KOLLA-SELECT SECTION.                                                  
890000     MOVE NEJ TO INDATA-SW                                                
900000     MOVE +1 TO INDX                                                      
910000     PERFORM UNTIL INDX > MAX-INDX                                        
920000       IF MID-CMD (INDX) = 'S'                                            
930000         MOVE INDX TO W-INDX                                              
940000         MOVE 15 TO INDX                                                  
950000         MOVE JA TO INDATA-SW                                             
960000         MOVE NEJ TO HOPP-SW                                              
970000       END-IF                                                             
980000       ADD +1   TO   INDX                                                 
990000     END-PERFORM                                                          
000000     IF MFS-SPLIT AND INDATA-SW = NEJ                                     
010000       PERFORM MFS-ROER-EJ-FAELT-UT                                       
020000       MOVE ENTER-CMD     TO MED-IDMFSFEL                                 
030000       CALL WMEDKONV USING MED-WMEDAREA                                   
040000       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
050000     END-IF                                                               
060000                                                                          
070000     IF INDATA-OK                                                         
080000       MOVE MFS-RENSA-FAELT TO MOD-TIAAVV-FOM-IN                          
090000                               MOD-TIAAVV-TOM-IN                          
100000                               MOD-IDDISTR-IN                             
110000                               MOD-TEMFSFEL                               
120000                               MOD-TEMFSFEL                               
130000     END-IF                                                               
140000     .                                                                    
150000     EJECT                                                                
160000 H-SKICKA-TRANS SECTION.                                                  
170000                                                                          
180000     MOVE '3154' TO SPAR-IDTRANS                                          
190000* ---HÄMTAR RÄTT RAD-VÄRDE TILL 3155-BILDEN                               
200000     MOVE W-INDX TO INDX                                                  
210000     MOVE ALL '+'                   TO M-MID-W3I15501                     
220000     MOVE MSGI-TIAAVV-FOM           TO M-MID-TIAAVV-FROM-IN               
230000     MOVE MSGI-TIAAVV-TOM           TO M-MID-TIAAVV-TOM-IN                
240000     MOVE MSGI-IDDISTR              TO M-MID-IDDISTR-IN                   
250000     MOVE SPAR-IDARTNR-TAB   (INDX) TO M-MID-IDARTNR-IN(2:8)              
260000     MOVE SPACE                     TO M-MID-IDARTNR-IN(1:1)              
270000     COMPUTE MSG-KVLL = LENGTH OF M-MID-W3I15501  + 17                    
280000     PERFORM IMS-INSERT-ALT-3155                                          
290000     .                                                                    
300000     EJECT                                                                
310000 S01-RAEKNA-ACKUMULERAT SECTION.                                          
340000     COMPUTE WS-INOUT = (W-IN - W-OUT)                                    
350000     IF WS-INOUT NOT = 0                                                  
360000       IF W-OUT NOT = 0                                                   
370000         COMPUTE WS-DIFF = (WS-INOUT / W-OUT) * +100                      
380000         COMPUTE WS-PROCENT ROUNDED = WS-DIFF                             
390000         MOVE WS-PROCENT TO WS-JAMFOR-PROC                                
400000                                                                          
410000         IF WS-JAMFOR-PROC = 0 AND WS-DIFF NOT = 0                        
420000            IF WS-DIFF > ZERO                                             
430000               MOVE +1 TO WS-PROCENT                                      
440000            ELSE                                                          
450000               MOVE -1 TO WS-PROCENT                                      
460000            END-IF                                                        
470000         END-IF                                                           
480000         IF WS-PROCENT > +999                                             
490000           MOVE +999 TO WS-PROCENT                                        
500000         ELSE                                                             
510000           IF WS-PROCENT > -999                                           
520000             MOVE -999 TO WS-PROCENT                                      
530000           END-IF                                                         
540000         END-IF                                                           
550000       ELSE                                                               
560000         MOVE +999 TO WS-PROCENT                                          
570000       END-IF                                                             
580000     ELSE                                                                 
590000       MOVE ZERO TO WS-PROCENT                                            
600000     END-IF                                                               
610000                                                                          
620000     PERFORM DB2-SELECT-BYLART                                            
630000     IF BYLART-OK                                                         
640000       MOVE IDARTNR-WS       TO MOD-TAB-IDARTNR   (INDX)                  
650000                                SPAR-IDARTNR-TAB  (INDX)                  
660000       MOVE BYLART-BEART-ENG TO MOD-TAB-BEART-ENG (INDX)                  
670000                               SPAR-BEART-ENG-TAB (INDX)                  
680000       MOVE WS-PROCENT       TO MOD-TAB-REPROCENT (INDX)                  
690000                               SPAR-REPROCENT-TAB (INDX)                  
700000       MOVE W-OUT            TO MOD-TAB-OUTLEV    (INDX)                  
710000                               SPAR-OUTLEV-TAB    (INDX)                  
720000       MOVE W-IN             TO MOD-TAB-INLEV     (INDX)                  
730000                               SPAR-INLEV-TAB     (INDX)                  
750000       MOVE WS-INOUT         TO MOD-TAB-DIFF      (INDX)                  
760000                               SPAR-DIFF-TAB      (INDX)                  
770000       ADD +1 TO INDX                                                     
780000       MOVE JA TO RAD-SW                                                  
790000     ELSE                                                                 
800000       MOVE IDARTNR-WS       TO MOD-TAB-IDARTNR   (INDX)                  
810000                                SPAR-IDARTNR-TAB  (INDX)                  
820000       MOVE 'UNKNOWN'        TO MOD-TAB-BEART-ENG (INDX)                  
830000                               SPAR-BEART-ENG-TAB (INDX)                  
840000       MOVE WS-PROCENT       TO MOD-TAB-REPROCENT (INDX)                  
850000                               SPAR-REPROCENT-TAB (INDX)                  
860000       MOVE W-OUT            TO MOD-TAB-OUTLEV    (INDX)                  
870000                               SPAR-OUTLEV-TAB    (INDX)                  
880000       MOVE W-IN             TO MOD-TAB-INLEV     (INDX)                  
890000                               SPAR-INLEV-TAB     (INDX)                  
910000       MOVE WS-INOUT         TO MOD-TAB-DIFF      (INDX)                  
920000                               SPAR-DIFF-TAB      (INDX)                  
930000       ADD +1 TO INDX                                                     
940000       MOVE JA TO RAD-SW                                                  
950000     END-IF                                                               
960000                                                                          
970000     MOVE WS-S01-IDARTNR TO IDARTNR-WS                                    
980000                            W-IDARTNR-MIN                                 
990000                            W-601-IDARTNR                                 
000000                            W-628-IDARTNR                                 
010000     .                                                                    
020000     EJECT                                                                
030000*MFS-RENSA-FAELT-UT SECTION.                                              
040000*                                                                         
050000*    --- ALLA UTDATA-FÄLT                                                 
060000*    MOVE MFS-RENSA-FAELT TO MOD-KVANTAL                                  
070000*                               MOD-TEMFSINF                              
080000*                               MOD-TIAAVV-FOM-UT                         
090000*                               MOD-TIAAVV-TOM-UT                         
100000*                               MOD-IDDISTR-UT                            
110000*                               MOD-IDFKNGRP-UT                           
120000*                               MOD-IDARTNR-UT                            
130000*    MOVE +1 TO INDX                                                      
140000*    PERFORM UNTIL INDX > MAX-INDX                                        
150000*      PERFORM MFS-RENSA-RAD-FAELT-UT                                     
160000*      ADD +1 TO INDX                                                     
170000*    END-PERFORM                                                          
180000*    .                                                                    
190000*    SKIP3                                                                
200000 MFS-RENSA-RAD-FAELT-UT.                                                  
210000*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
220000     IF INDX <= MAX-INDX                                                  
230000       MOVE MFS-RENSA-FAELT TO MOD-CMD           (INDX)                   
240000                               MOD-TAB-IDARTNR   (INDX)                   
250000                               MOD-TAB-BEART-ENG (INDX)                   
260000                               MOD-TAB-OUTLEV    (INDX)                   
270000                               MOD-TAB-INLEV     (INDX)                   
280000                               MOD-TAB-DIFF      (INDX)                   
290000                               MOD-TAB-REPROCENT (INDX)                   
300000     END-IF                                                               
310000     .                                                                    
320000     SKIP3                                                                
330000 MFS-RENSA-FAELT-IN SECTION.                                              
340000                                                                          
350000*    --- ALLA INDATA-FÄLT                                                 
360000     MOVE +1 TO INDX                                                      
370000     PERFORM UNTIL INDX > MAX-INDX                                        
380000       MOVE MFS-RENSA-FAELT TO MOD-CMD (INDX)                             
390000       ADD +1 TO INDX                                                     
400000     END-PERFORM                                                          
410000     .                                                                    
420000     EJECT                                                                
430000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
440000                                                                          
450000*    --- ALLA UTDATA-FÄLT                                                 
460000     MOVE MFS-ROER-EJ-FAELT TO MOD-TIAAVV-FOM-UT                          
470000                                 MOD-TIAAVV-TOM-UT                        
480000                                 MOD-IDDISTR-UT                           
490000                                 MOD-IDFKNGRP-UT                          
500000                                 MOD-IDARTNR-UT                           
510000                                 MOD-KVANTAL                              
520000     MOVE +1 TO INDX                                                      
530000     PERFORM UNTIL INDX > MAX-INDX                                        
540000       MOVE MFS-ROER-EJ-FAELT TO       MOD-CMD    (INDX)                  
550000                               MOD-TAB-IDARTNR    (INDX)                  
560000                               MOD-TAB-BEART-ENG  (INDX)                  
570000                               MOD-TAB-OUTLEV     (INDX)                  
580000                               MOD-TAB-INLEV      (INDX)                  
590000                               MOD-TAB-DIFF       (INDX)                  
600000                               MOD-TAB-REPROCENT  (INDX)                  
610000                                                                          
620000       ADD +1 TO INDX                                                     
630000     END-PERFORM                                                          
640000     .                                                                    
650000     SKIP3                                                                
660000 IMS-GET-MSG SECTION.                                                     
670000                                                                          
680000     MOVE '  QC' TO GODK-STATUSKODER                                      
690000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
700000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
710000     PERFORM IMS-STATUSKONTROLL                                           
720000     .                                                                    
730000     SKIP3                                                                
740000 IMS-INSERT-MSG SECTION.                                                  
750000                                                                          
760000     IF MSGI-IDLAND-SPR = 'GB'                                            
770000       MOVE 'N' TO MFS-KDHUVOMR                                           
780000     END-IF                                                               
790000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
800000     MOVE SPACE TO GODK-STATUSKODER                                       
810000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
820000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
830000     PERFORM IMS-STATUSKONTROLL                                           
840000     .                                                                    
850000     EJECT                                                                
860000 IMS-INSERT-ALT-3155 SECTION.                                             
870000                                                                          
880000     MOVE SPACE TO GODK-STATUSKODER                                       
890000     CALL CBLTDLI USING ISRT ALT-PCB ALT-IO-3155                          
900000     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
910000     PERFORM IMS-STATUSKONTROLL                                           
920000     .                                                                    
930000     EJECT                                                                
940000 IMS-STATUSKONTROLL SECTION.                                              
950000                                                                          
960000     SET STATUS-IX TO 1                                                   
970000     SEARCH GODK-STATUS                                                   
980000       AT END                                                             
990000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000000         DELIMITED BY SIZE INTO FELTEXT                                   
010000         CALL FELLOG                                                      
020000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
030000         CONTINUE                                                         
040000     END-SEARCH                                                           
050000     .                                                                    
060000     EJECT                                                                
070000 DB2-DCL-OPN-CRS-BYLACK SECTION.                                          
080000                                                                          
090000     EXEC SQL DECLARE BYLACK-CRS CURSOR FOR                               
100000     SELECT   IDARTNR,                                                    
110000              IDPTYP,                                                     
120000              SUM(KVANTAL)                                                
130000                                                                          
140000     FROM     BYLACK                                                      
150000                                                                          
160000     WHERE    DAAAVV >= :WS-FROM-IN                                       
170000       AND    DAAAVV <= :WS-TOM-IN                                        
180000       AND   (IDPTYP  = :WS-RET                                           
190000       OR     IDPTYP  = :WS-FAK                                           
200000       OR     IDPTYP  = :WS-KRE)                                          
210000                                                                          
220000     GROUP BY IDARTNR,                                                    
230000              IDPTYP                                                      
240000     ORDER BY IDARTNR,                                                    
250000              IDPTYP                                                      
260000                                                                          
270000     OPTIMIZE FOR 100 ROWS                                                
280000     END-EXEC                                                             
290000                                                                          
300000     MOVE 000            TO GODK-SQLCODEKODER                             
310000     EXEC SQL OPEN BYLACK-CRS END-EXEC                                    
320000     MOVE SQLCODE        TO SQLCODE-WS                                    
330000     PERFORM DB2-STATUSKONTROLL                                           
340000     .                                                                    
350000     EJECT                                                                
360000 DB2-FETCH-BYLACK SECTION.                                                
370000     MOVE 000100         TO GODK-SQLCODEKODER                             
380000     EXEC SQL FETCH BYLACK-CRS INTO                                       
390000            :BYLACK-IDARTNR,                                              
400000            :BYLACK-IDPTYP,                                               
410000            :BYLACK-KVANTAL                                               
420000     END-EXEC                                                             
430000     MOVE SQLCODE        TO SQLCODE-WS                                    
440000                            BYLACK-WS                                     
450000     PERFORM DB2-STATUSKONTROLL                                           
460000     .                                                                    
470000     EJECT                                                                
480000 DB2-DCL-OPN-CRS-IDART-BYLACK SECTION.                                    
490000                                                                          
500000     EXEC SQL DECLARE BYLIDART-CRS CURSOR FOR                             
510000     SELECT   IDARTNR,                                                    
520000              IDPTYP,                                                     
530000              SUM(KVANTAL)                                                
540000                                                                          
550000     FROM     BYLACK                                                      
560000                                                                          
570000     WHERE    IDARTNR >= :W-IDARTNR-MIN                                   
580000       AND    DAAAVV  >= :WS-FROM-IN                                      
590000       AND    DAAAVV  <= :WS-TOM-IN                                       
600000       AND   (IDPTYP   = :WS-RET                                          
610000       OR     IDPTYP   = :WS-FAK                                          
620000       OR     IDPTYP   = :WS-KRE)                                         
630000                                                                          
640000     GROUP BY IDARTNR,                                                    
650000              IDPTYP                                                      
660000     ORDER BY IDARTNR,                                                    
670000              IDPTYP                                                      
680000                                                                          
690000     OPTIMIZE FOR 100 ROWS                                                
700000     END-EXEC                                                             
710000                                                                          
720000     MOVE 000         TO GODK-SQLCODEKODER                                
730000     EXEC SQL OPEN BYLIDART-CRS END-EXEC                                  
740000     MOVE SQLCODE        TO SQLCODE-WS                                    
750000     PERFORM DB2-STATUSKONTROLL                                           
760000     .                                                                    
770000     EJECT                                                                
780000 DB2-FETCH-BYLACK-IDART SECTION.                                          
790000     MOVE 000100         TO GODK-SQLCODEKODER                             
800000     EXEC SQL FETCH BYLIDART-CRS INTO                                     
810000            :BYLACK-IDARTNR,                                              
820000            :BYLACK-IDPTYP,                                               
830000            :BYLACK-KVANTAL                                               
840000     END-EXEC                                                             
850000     MOVE SQLCODE        TO SQLCODE-WS                                    
860000                            BYLACK-WS                                     
870000     PERFORM DB2-STATUSKONTROLL                                           
880000     .                                                                    
890000     EJECT                                                                
900000 DB2-DCL-OPN-CRS-BYLRAD-DISTR SECTION.                                    
910000                                                                          
920000     EXEC SQL DECLARE RADDIST-CRS CURSOR FOR                              
930000     SELECT   IDARTNR,                                                    
940000              IDPTYP,                                                     
950000              IDDISTR,                                                    
960000              KDBYTREF,                                                   
970000              SUM(KVANTAL)                                                
980000                                                                          
990000     FROM     BYLRAD                                                      
000000                                                                          
010000     WHERE    IDDISTR  = :WS-IDDISTR                                      
020000       AND    DAAAVV  >= :WS-FROM-IN                                      
030000       AND    DAAAVV  <= :WS-TOM-IN                                       
040000       AND   (IDPTYP   = :WS-RET                                          
050000       OR     IDPTYP   = :WS-FAK                                          
060000       OR     IDPTYP   = :WS-KRE)                                         
070000                                                                          
080000     GROUP BY IDARTNR,                                                    
090000              IDDISTR,                                                    
100000              IDPTYP,                                                     
110000              KDBYTREF                                                    
120000     ORDER BY IDARTNR,                                                    
130000              IDDISTR,                                                    
140000              IDPTYP,                                                     
150000              KDBYTREF                                                    
160000                                                                          
170000     OPTIMIZE FOR 100 ROWS                                                
180000     END-EXEC                                                             
190000                                                                          
200000     MOVE 000            TO GODK-SQLCODEKODER                             
210000     EXEC SQL OPEN RADDIST-CRS END-EXEC                                   
220000     MOVE SQLCODE        TO SQLCODE-WS                                    
230000     PERFORM DB2-STATUSKONTROLL                                           
240000     .                                                                    
250000     EJECT                                                                
260000 DB2-FETCH-RADDIST SECTION.                                               
270000     MOVE 000100         TO GODK-SQLCODEKODER                             
280000     EXEC SQL FETCH RADDIST-CRS INTO                                      
290000            :BYLRAD-IDARTNR,                                              
300000            :BYLRAD-IDPTYP,                                               
310000            :BYLRAD-IDDISTR,                                              
320000            :BYLRAD-KDBYTREF,                                             
330000            :BYLRAD-KVANTAL                                               
340000     END-EXEC                                                             
350000     MOVE SQLCODE        TO SQLCODE-WS                                    
360000                            BYLRAD-WS                                     
370000     PERFORM DB2-STATUSKONTROLL                                           
380000     .                                                                    
390000     EJECT                                                                
400000 DB2-DCL-OPN-CRS-BYLRAD-DISTART SECTION.                                  
410000                                                                          
420000     EXEC SQL DECLARE DISTART-CRS CURSOR FOR                              
430000     SELECT   IDARTNR,                                                    
440000              IDPTYP,                                                     
450000              IDDISTR,                                                    
460000              KDBYTREF,                                                   
470000              SUM(KVANTAL)                                                
480000                                                                          
490000     FROM     BYLRAD                                                      
500000                                                                          
510000     WHERE    IDARTNR >= :W-IDARTNR-MIN                                   
520000       AND    IDDISTR  = :WS-IDDISTR                                      
530000       AND    DAAAVV  >= :WS-FROM-IN                                      
540000       AND    DAAAVV  <= :WS-TOM-IN                                       
550000       AND   (IDPTYP   = :WS-RET                                          
560000       OR     IDPTYP   = :WS-FAK                                          
570000       OR     IDPTYP   = :WS-KRE)                                         
580000                                                                          
590000     GROUP BY IDARTNR,                                                    
600000              IDDISTR,                                                    
610000              IDPTYP,                                                     
620000              KDBYTREF                                                    
630000     ORDER BY IDARTNR,                                                    
640000              IDDISTR,                                                    
650000              IDPTYP,                                                     
660000              KDBYTREF                                                    
670000                                                                          
680000     OPTIMIZE FOR 100 ROWS                                                
690000     END-EXEC                                                             
700000                                                                          
710000     MOVE 000            TO GODK-SQLCODEKODER                             
720000     EXEC SQL OPEN DISTART-CRS END-EXEC                                   
730000     MOVE SQLCODE        TO SQLCODE-WS                                    
740000     PERFORM DB2-STATUSKONTROLL                                           
750000     .                                                                    
760000     EJECT                                                                
770000 DB2-FETCH-RADDIST-IDART SECTION.                                         
780000     MOVE 000100         TO GODK-SQLCODEKODER                             
790000     EXEC SQL FETCH DISTART-CRS INTO                                      
800000            :BYLRAD-IDARTNR,                                              
810000            :BYLRAD-IDPTYP,                                               
820000            :BYLRAD-IDDISTR,                                              
830000            :BYLRAD-KDBYTREF,                                             
840000            :BYLRAD-KVANTAL                                               
850000     END-EXEC                                                             
860000     MOVE SQLCODE        TO SQLCODE-WS                                    
870000                            BYLRAD-WS                                     
880000     PERFORM DB2-STATUSKONTROLL                                           
890000     .                                                                    
900000     EJECT                                                                
910000 DB2-DCL-OPN-CRS-BYLRAD-FGRP SECTION.                                     
920000                                                                          
930000     EXEC SQL DECLARE RADFGRP-CRS CURSOR FOR                              
940000     SELECT   IDARTNR,                                                    
950000              IDPTYP,                                                     
960000              IDDISTR,                                                    
970000              KDBYTREF,                                                   
980000              SUM(KVANTAL)                                                
990000                                                                          
000000     FROM     BYLRAD                                                      
010000                                                                          
020000     WHERE    DAAAVV   >= :WS-FROM-IN                                     
030000       AND    DAAAVV   <= :WS-TOM-IN                                      
040000       AND    IDFKNGRP  = :WS-IDFKNGRP                                    
050000       AND   (IDPTYP    = :WS-RET                                         
060000       OR     IDPTYP    = :WS-FAK                                         
070000       OR     IDPTYP    = :WS-KRE)                                        
080000                                                                          
090000     GROUP BY IDARTNR,                                                    
100000              IDDISTR,                                                    
110000              IDPTYP,                                                     
120000              KDBYTREF                                                    
130000     ORDER BY IDARTNR,                                                    
140000              IDDISTR,                                                    
150000              IDPTYP,                                                     
160000              KDBYTREF                                                    
170000                                                                          
180000     OPTIMIZE FOR 100 ROWS                                                
190000     END-EXEC                                                             
200000                                                                          
210000     MOVE 000            TO GODK-SQLCODEKODER                             
220000     EXEC SQL OPEN RADFGRP-CRS END-EXEC                                   
230000     MOVE SQLCODE        TO SQLCODE-WS                                    
240000     PERFORM DB2-STATUSKONTROLL                                           
250000     .                                                                    
260000     EJECT                                                                
270000 DB2-FETCH-RADFGRP SECTION.                                               
280000     MOVE 000100         TO GODK-SQLCODEKODER                             
290000     EXEC SQL FETCH RADFGRP-CRS INTO                                      
300000            :BYLRAD-IDARTNR,                                              
310000            :BYLRAD-IDPTYP,                                               
320000            :BYLRAD-IDDISTR,                                              
330000            :BYLRAD-KDBYTREF,                                             
340000            :BYLRAD-KVANTAL                                               
350000     END-EXEC                                                             
360000     MOVE SQLCODE        TO SQLCODE-WS                                    
370000                            BYLRAD-WS                                     
380000     PERFORM DB2-STATUSKONTROLL                                           
390000     .                                                                    
400000     EJECT                                                                
410000 DB2-DCL-OPN-CRS-BYLRAD-FGRPART SECTION.                                  
420000                                                                          
430000     EXEC SQL DECLARE FGRPART-CRS CURSOR FOR                              
440000     SELECT   IDARTNR,                                                    
450000              IDPTYP,                                                     
460000              IDDISTR,                                                    
470000              KDBYTREF,                                                   
480000              SUM(KVANTAL)                                                
490000                                                                          
500000     FROM     BYLRAD                                                      
510000                                                                          
520000     WHERE    IDARTNR  >= :W-IDARTNR-MIN                                  
530000       AND    DAAAVV   >= :WS-FROM-IN                                     
540000       AND    DAAAVV   <= :WS-TOM-IN                                      
550000       AND    IDFKNGRP  = :WS-IDFKNGRP                                    
560000       AND   (IDPTYP    = :WS-RET                                         
570000       OR     IDPTYP    = :WS-FAK                                         
580000       OR     IDPTYP    = :WS-KRE)                                        
590000                                                                          
600000     GROUP BY IDARTNR,                                                    
610000              IDDISTR,                                                    
620000              IDPTYP,                                                     
630000              KDBYTREF                                                    
640000     ORDER BY IDARTNR,                                                    
650000              IDDISTR,                                                    
660000              IDPTYP,                                                     
670000              KDBYTREF                                                    
680000                                                                          
690000     OPTIMIZE FOR 100 ROWS                                                
700000     END-EXEC                                                             
710000                                                                          
720000     MOVE 000            TO GODK-SQLCODEKODER                             
730000     EXEC SQL OPEN FGRPART-CRS END-EXEC                                   
740000     MOVE SQLCODE        TO SQLCODE-WS                                    
750000     PERFORM DB2-STATUSKONTROLL                                           
760000     .                                                                    
770000     EJECT                                                                
780000 DB2-FETCH-RADFGRP-IDART SECTION.                                         
790000     MOVE 000100         TO GODK-SQLCODEKODER                             
800000     EXEC SQL FETCH FGRPART-CRS INTO                                      
810000            :BYLRAD-IDARTNR,                                              
820000            :BYLRAD-IDPTYP,                                               
830000            :BYLRAD-IDDISTR,                                              
840000            :BYLRAD-KDBYTREF,                                             
850000            :BYLRAD-KVANTAL                                               
860000     END-EXEC                                                             
870000     MOVE SQLCODE        TO SQLCODE-WS                                    
880000                            BYLRAD-WS                                     
890000     PERFORM DB2-STATUSKONTROLL                                           
900000     .                                                                    
910000     EJECT                                                                
920000 DB2-DCL-OPN-CRS-BYLRAD-ARTNR SECTION.                                    
930000                                                                          
940000     EXEC SQL DECLARE ARTRAD-CRS CURSOR FOR                               
950000     SELECT   IDARTNR,                                                    
960000              IDPTYP,                                                     
970000              IDDISTR,                                                    
980000              KDBYTREF,                                                   
990000              SUM(KVANTAL)                                                
000000                                                                          
010000     FROM     BYLRAD                                                      
020000                                                                          
030000     WHERE    IDARTNR  = :WS-IDARTNR                                      
040000       AND    DAAAVV  >= :WS-FROM-IN                                      
050000       AND    DAAAVV  <= :WS-TOM-IN                                       
060000       AND   (IDPTYP   = :WS-RET                                          
070000       OR     IDPTYP   = :WS-FAK                                          
080000       OR     IDPTYP   = :WS-KRE)                                         
090000                                                                          
100000     GROUP BY IDARTNR,                                                    
110000              IDDISTR,                                                    
120000              IDPTYP,                                                     
130000              KDBYTREF                                                    
140000     ORDER BY IDARTNR,                                                    
150000              IDDISTR,                                                    
160000              IDPTYP,                                                     
170000              KDBYTREF                                                    
180000                                                                          
190000     OPTIMIZE FOR 100 ROWS                                                
200000     END-EXEC                                                             
210000                                                                          
220000     MOVE 000            TO GODK-SQLCODEKODER                             
230000     EXEC SQL OPEN ARTRAD-CRS END-EXEC                                    
240000     MOVE SQLCODE        TO SQLCODE-WS                                    
250000     PERFORM DB2-STATUSKONTROLL                                           
260000     .                                                                    
270000     EJECT                                                                
280000 DB2-FETCH-RADARTNR SECTION.                                              
290000     MOVE 000100         TO GODK-SQLCODEKODER                             
300000     EXEC SQL FETCH ARTRAD-CRS INTO                                       
310000            :BYLRAD-IDARTNR,                                              
320000            :BYLRAD-IDPTYP,                                               
330000            :BYLRAD-IDDISTR,                                              
340000            :BYLRAD-KDBYTREF,                                             
350000            :BYLRAD-KVANTAL                                               
360000     END-EXEC                                                             
370000     MOVE SQLCODE        TO SQLCODE-WS                                    
380000                            BYLRAD-WS                                     
390000     PERFORM DB2-STATUSKONTROLL                                           
400000     .                                                                    
410000     EJECT                                                                
420000 DB2-SELECT-BYLART SECTION.                                               
430000     MOVE 000100         TO GODK-SQLCODEKODER                             
440000     EXEC SQL SELECT                                                      
450000            BEART_ENG                                                     
460000          INTO                                                            
470000            :BYLART-BEART-ENG                                             
480000          FROM BYLART                                                     
490000          WHERE IDARTNR = :W-IDARTNR-MIN                                  
500000     END-EXEC                                                             
510000     MOVE SQLCODE        TO SQLCODE-WS                                    
520000                            BYLART-WS                                     
530000     PERFORM DB2-STATUSKONTROLL                                           
540000     .                                                                    
550000     EJECT                                                                
560000 DB2-CLOSE-BYLACK-CRS SECTION.                                            
570000     SKIP2                                                                
580000     EXEC SQL CLOSE BYLACK-CRS END-EXEC                                   
590000     .                                                                    
600000     EJECT                                                                
610000 DB2-CLOSE-BYLIDART-CRS SECTION.                                          
620000     SKIP2                                                                
630000     EXEC SQL CLOSE BYLIDART-CRS END-EXEC                                 
640000     .                                                                    
650000     EJECT                                                                
660000 DB2-CLOSE-RADDIST-CRS SECTION.                                           
670000     SKIP2                                                                
680000     EXEC SQL CLOSE RADDIST-CRS END-EXEC                                  
690000     .                                                                    
700000     EJECT                                                                
710000 DB2-CLOSE-DISTART-CRS SECTION.                                           
720000     SKIP2                                                                
730000     EXEC SQL CLOSE DISTART-CRS END-EXEC                                  
740000     .                                                                    
750000     EJECT                                                                
760000 DB2-CLOSE-RADFGRP-CRS SECTION.                                           
770000     SKIP2                                                                
780000     EXEC SQL CLOSE RADFGRP-CRS END-EXEC                                  
790000     .                                                                    
800000     EJECT                                                                
810000 DB2-CLOSE-FGRPART-CRS SECTION.                                           
820000     SKIP2                                                                
830000     EXEC SQL CLOSE FGRPART-CRS END-EXEC                                  
840000     .                                                                    
850000     EJECT                                                                
860000 DB2-CLOSE-ARTRAD-CRS SECTION.                                            
870000     SKIP2                                                                
880000     EXEC SQL CLOSE ARTRAD-CRS END-EXEC                                   
890000     .                                                                    
900000     EJECT                                                                
910000 DB2-STATUSKONTROLL  SECTION.                                             
920000                                                                          
930000     SET SQLCODE-IX TO 1                                                  
940000     SEARCH GODK-SQLCODE                                                  
950000       AT END CALL FELLOG                                                 
960000       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
970000     END-SEARCH                                                           
980000     .                                                                    
