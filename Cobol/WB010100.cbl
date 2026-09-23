000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WB010100.                                                
000400 AUTHOR.         CONNY EGHOLT.                                            
000500 DATE-WRITTEN.   2003/12/17.                                              
000600 DATE-COMPILED.                                                           
000700*    NAMN:       WB0101                                                   
000800*    FUNKTION:                                                            
000900*        SHOWS ALL LINES WITH A CERTAIN SELECTION FROM THE                
001000*        ACCESSORY DB2-DATABASE COMBINED WITH                             
001100*        ASSOCIATED DATA FROM  PULS DL1-DATABASES                         
001200*                                                                         
001300*        SÖKNYCKLAR:                                                      
001400*        EN AV, ELLER EN KOMBINATION AV:                                  
001500*                                                                         
001600*        INFÖRANDEVECKO-INTERVALL ÄO  - TIAOINF                           
001700*        KU UPPDRAG         - IDUPPDKU                                    
001800*        ARTIKELNUMMER      - IDARTNR                                     
001900*        SU UPPDRAG         - IDUPPDSU                                    
002000*                                                                         
002100*        UNIK POST PÅ BASEN ÄR ( IDARTNR + IDUPPDSU )                     
002200*                                                                         
002300*        MAX OUTPUT TILL WEBB ÄR 100 RADER.                               
002400*        ENDAST PGMACT 'S' 'N' OCH 'J' GODKÄNDA                           
002500*                       (SEARCH , NEXT OCH  JUMP )                        
002600*                                                                         
002700*        PROGRAM    READS   DB2 TB1ACCE                                   
002800*                   READS   DL1 WDD2                                      
002900*                   READS   DL1 WDD9                                      
003000*                   READS   DL1 WDK6                                      
003100*                   READS   DL1 WDK9                                      
003101*                   READS   DL1 WDC1                                      
003102*                   READS   DL1 WDL2                                      
003200*    INDATA.                                                              
003300*        TRANSACTION: WB0101T                                             
003400*        REQUEST:     WB0101I1                                            
003500*    UTDATA.                                                              
003600*        RESPONSE:    WB0101O1                                            
003700*                                                                         
003800*    ÄNDRINGAR:                                                           
003900*    MARS-2007: SCR 4459048.                                              
004000*        TILLÄGG FÄLT "QTY IN STOCK" (ARBETSSALDO PÅ 2102)                
004100*        TILLÄGG FÄLT "TPD WEEK"   " (TITPD PÅ WDD2) ERSÄTTER ISD         
004200*        TILLÄGG FÄLT "TPD STATUS"   (KDTPD PÅ WDD2)                      
004300*        TILLÄGG FÄLT "VOL.BOARD 3" (KVYVOL-B3)                           
004400*        PREL.   FÄLT "SUGGESTED RETAIL PRICE (PULS)" PRARTBTO-M          
004500*        PREL.   FÄLT "TARGET SUG.RETAIL PRICE (MAN.)" PRSUGRET           
004600*                                                                         
004700*        ÄNDRING FÄLT "ISA(Y/N)" -> PSW-STATUS FRÅN KDP                   
004800*        ÄNDRING FÄLT "PACK TYPE"  HÄMTA FRÅN WDK613 FÖR EMBQ1=73         
004900*        ÄNDRING FÄLT "PURCH.WK"                                          
005000*        KARIS   FÄLT "OFFER UNIT COST (MAN.)" (PRUNOFF) , BORT           
005100*                TILL "OFFER UNIT COST (PULS)" (PRARTSTD) , BORT          
005200*        ÄNDRING FÄLT "VOL.BOARD 1" (KVYVOL-JUST)->(KVYVOL-B1)            
005300*        ÄNDRING FÄLT "VOL.BOARD 2" (KVYVOL-RES)->(KVYVOL-B2)             
005400*        ÄNDRING FÄLT "ASS.YEAR VOL"(KVYVOL-RES)->(KVYVOL-ASS)            
005500*                                                                         
005600*        BORTTAG FÄLT "SPLIT.BL.FLAG" (FLFSP PÅ WDK6)                     
005700*        BORTTAG FÄLT "ISD"                                               
005800*        BORTTAG FÄLT "ISD(Y/N)"                                          
005900*        BORTTAG FÄLT "FUNCGRP"                                           
006000*        BORTTAG FÄLT "POS"                                               
006100*        BORTTAG FÄLT "CAR PROJ"                                          
006200*        BORTTAG FÄLT "PARTS PROJ"                                        
006300*        BORTTAG FÄLT "CO INTRO CHANGE"                                   
006400*        BORTTAG FÄLT "COLOR STATUS"                                      
006500*        BORTTAG FÄLT "I.INSTR MAN ORD"                                   
006600*        BORTTAG FÄLT "I.INSTR MAN READY"                                 
006700*        BORTTAG FÄLT "I.I. TO CDC"                                       
006710*        BORTTAG FÄLT "DEC.YEAR VOL. WITHOUT LAUNCH" = KVYVOL             
006720*        BORTTAG FÄLT "LAUNCH US" = KVMAM-N                               
006730*        BORTTAG FÄLT "LAUNCH EU/NORDIC" = KVMAM-E                        
006740*        BORTTAG FÄLT "LAUNCH ASIA" = KVMAM-O                             
006800*                                                                         
006801*                                                                         
006810*    NOVEMBER-2007: SCR 5814533.                                          
006820*        TILLÄGG AV 5 NYA NOTERINGSFÄLT FÖR MANUELL INPUT.                
006830*        BORTTAG AV DE 6 OLIKA PSW-PLAN-KOLUMNERNA.                       
006831*                                                                         
006832*    MARS-2008:     SCR 6488739. (EMERG)                                  
006833*        RÄTTA LÄSNING AV DATAKÄLLAN FÖR ATT VISA FLTPDWKPH1              
006834*        FRÅN ACCE-DATPDPH1  TILL  ACCE-DAPSWQP-1                         
006840*                                                                         
006841*    MAJ-2012: LOCAL SOURCING                                             
006842*        NYCKEL WDD901 UTÖKAD MED IDDC                                    
006843*                                                                         
006844*    MARCH-2016: SCR 10219962.                                            
006845*        LOT OF CHANGES DUE TO NEW WAYS-OF-WORKING.                       
006846*                                                                         
006850*                                                                         
006900     SKIP3                                                                
007000 ENVIRONMENT DIVISION.                                                    
007100     SKIP2                                                                
007200 INPUT-OUTPUT SECTION.                                                    
007300                                                                          
007400 FILE-CONTROL.                                                            
007500     EJECT                                                                
007600 DATA DIVISION.                                                           
007700     SKIP3                                                                
007800 FILE SECTION.                                                            
007900     EJECT                                                                
008000 WORKING-STORAGE SECTION.                                                 
008100 77  IDPGM                       PIC X(08)   VALUE 'WB010100'.            
008200                                                                          
008300*    --- WORK FIELD FOR ERROR MESSAGES AT  CALL ABEND/FELLOG              
008400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
008500 77  FILLER                      PIC X(16) VALUE 'DB2-SEKTION ='.         
008600 77  DB2-SEKTION                 PIC X(80) VALUE SPACE.                   
008700 77  KDRC-DISPLAY                PIC Z(5).                                
008800                                                                          
008900 77  J                           PIC X       VALUE 'J'.                   
009000 77  Y                           PIC X       VALUE 'Y'.                   
009100 77  N                           PIC X       VALUE 'N'.                   
009110 77  PROPOSED                    PIC X       VALUE 'P'.                   
009200 77  OBSOLETE                    PIC X       VALUE 'O'.                   
009300 77  CONFIRMED                   PIC X       VALUE 'C'.                   
009400 77  POS                         PIC S9(4)   VALUE ZERO BINARY.           
009500 77  RESP-IX                     PIC S9(4)   VALUE ZERO BINARY.           
009600 77  JUMP-IX                     PIC S9(4)   VALUE ZERO BINARY.           
009700 77  RESP-MAX                    PIC S9(4)   VALUE +100 BINARY.           
009800 77  DB2-IX                      PIC S9(8)   VALUE ZERO BINARY.           
009900 77  IX                          PIC S9(3)  VALUE ZERO COMP-3.            
010000 77  MAX-IX                      PIC S9(3)  VALUE +7   COMP-3.            
010010 77  LTEXT                       PIC S9(4)  COMP VALUE 35.                
010100                                                                          
010600 77  SW-FIRST-TIME               PIC X       VALUE 'J'.                   
010700     88  FIRST-TIME                          VALUE 'J'.                   
010800                                                                          
010801 77  KEYS-SW                     PIC X       VALUE 'J'.                   
010802     88  KEYS-OK                             VALUE 'J'.                   
010803     88  KEYS-NOT-OK                         VALUE 'N'.                   
010804                                                                          
010805 77  SW-WDK601                   PIC X       VALUE 'N'.                   
010806     88  WDK601                              VALUE 'J'.                   
010807                                                                          
010900 77  SW-WDK611                   PIC X       VALUE 'N'.                   
011000     88  WDK611                              VALUE 'J'.                   
011100                                                                          
011200 77  SW-WDK613                   PIC X       VALUE 'N'.                   
011300     88  WDK613                              VALUE 'J'.                   
011400                                                                          
011410 77  SW-WDK623                   PIC X       VALUE 'N'.                   
011420     88  WDK623                              VALUE 'J'.                   
011430                                                                          
011500 77  WS-NUM-IDANSK               PIC 9(3)   VALUE ZERO.                   
011600                                                                          
011610*01  -COPY WWDCKONS                                                       
011620                                                                          
011700 01  W.                                                                   
011800     03  W-ARB-SALDO         PIC S9(7)    VALUE ZERO COMP-3.              
011900     03  W-DISPONIBELT       PIC S9(7)    VALUE ZERO COMP-3.              
012000     03  W-CDC-KVLS          PIC S9(7)    VALUE ZERO COMP-3.              
012100     03  WS-KVOKS-TOT        PIC S9(9)    VALUE ZERO COMP-3.              
012200     03  WS-SUTPO-TOT        PIC S9(9)    VALUE ZERO COMP-3.              
012300     03  W-KVAKS             PIC S9(7)    VALUE ZERO COMP-3.              
012400     03  WS-SULEVANT-ACK     PIC  9(5)    VALUE ZERO.                     
012500     03  WS-SULEVANT-RED.                                                 
012600       05 WS-SULEVANT-OP     PIC  X(1)    VALUE SPACE.                    
012601       05 WS-SULEVANT-NO     PIC  9(3)    VALUE ZERO.                     
012602     03  W-TIAAAAVV.                                                      
012603       05 W-TISEKEL          PIC  9(2)    VALUE ZERO.                     
012604       05 W-TIAA             PIC  9(2)    VALUE ZERO.                     
012700       05 W-TIVV             PIC  9(2)    VALUE ZERO.                     
012800     03  W-DADISPIN          PIC  9(6)    VALUE ZERO.                     
012900     03  WS-TPO-NAESTA-INLEV PIC S9(9)    VALUE ZERO COMP-3.              
013000                                                                          
013100     03 WS-KDFRPTYP              PIC X      VALUE SPACE.                  
013200                                                                          
013300     03 W-KDVALISO2          PIC X(3)       VALUE SPACE.                  
013400     03 WS-PRKURS2           PIC S9(6)V9(5) VALUE ZERO COMP-3.            
013500     03 WS-PRKURS8           PIC S9(6)V9(5) VALUE ZERO COMP-3.            
013600     03 WS-KDVALISO-MC       PIC X(3)       VALUE SPACE.                  
013700     03 WS-KDVALISO-COST     PIC X(3)       VALUE SPACE.                  
013800     03 IDPROMR-WS           PIC X(3)       VALUE SPACE.                  
013900     03 WS-IDMARKBO              PIC X      VALUE SPACE.                  
014000                                                                          
014100     03 WS-IDPROMR               PIC X(3)    VALUE SPACE.                 
014200     03 FILLER REDEFINES WS-IDPROMR.                                      
014300         05 WS-MARKBOLAG         PIC X(1).                                
014400         05 WS-IDPROMRN          PIC X(2).                                
014500     EJECT                                                                
014600 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
014700 01  FILLER REDEFINES DAGENS-DATUM.                                       
014800     03  DAGENS-DATUM-SEKEL      PIC 9(2).                                
014900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
015000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
015100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
015200 01  W-DATUM-SOK                 PIC 9(8)    VALUE ZERO.                  
015300                                                                          
015600 01  WS-DAAVROP-AREA.                                                     
015700     03 WS-DAAVROP.                                                       
015800         05 WS-DAAVROP-SEK          PIC 9(2).                             
015900         05 WS-DAAVROP-AAVV         PIC 9(4).                             
016100                                                                          
016101 01  WS-TILEVBSK-AREA.                                                    
016102     03 WS-TILEVBSK.                                                      
016103         05 WS-TILEVBSK-SEK         PIC 9(2).                             
016104         05 WS-TILEVBSK-AAVVD       PIC 9(5).                             
016105                                                                          
016106 01  WS-TIAVRDAT-AREA.                                                    
016107     03 WS-TIAVRDAT.                                                      
016108         05 WS-TIAVRDAT-SEK         PIC 9(2).                             
016109         05 WS-TIAVRDAT-AAVVD       PIC 9(5).                             
016110                                                                          
016111 01  WS-TIAVIDAT-AREA.                                                    
016112     03 WS-TIAVIDAT.                                                      
016113         05 WS-TIAVIDAT-SEK         PIC 9(2).                             
016114         05 WS-TIAVIDAT-AAVVD       PIC 9(5).                             
016115     EJECT                                                                
016116                                                                          
016117 01  WS-DUMP             PIC X(20) VALUE 'DUMPAREA'.                      
016118 01  WS-DUMP-AREA.                                                        
016119     03 WS-KDAVROP-DUMP  PIC S9(1)      COMP-3 VALUE 0.                   
016120     03 WS-TILEVBSK-DUMP PIC S9(7)      COMP-3 VALUE 0.                   
016121     03 WS-TIAVRDAT-DUMP PIC S9(7)      COMP-3 VALUE 0.                   
016122     03 WS-PRARTBTO-DUMP PIC S9(7)V9(2) COMP-3 VALUE 0.                   
016123     03 WS-TIAVIDAT-DUMP PIC S9(7)      COMP-3 VALUE 0.                   
016124     03 WS-SULEVANT-DUMP PIC 9(5) VALUE 0.                                
016125     EJECT                                                                
016126                                                                          
016127*    --- SELECTION DATA FROM JSP PAGE                                     
016128                                                                          
016129 01  WS-AAVVD-AREA.                                                       
016130     03  WS-TIAOINF-1.                                                    
016131         05  WS-TIAOINF-SEK      PIC 9(2).                                
016132         05  WS-TIAOINF-AAVV     PIC 9(4).                                
016133         05  WS-TIAOINF-D        PIC 9.                                   
016134                                                                          
016200     03  WS-TIAOINF-2  REDEFINES WS-TIAOINF-1.                            
016300         05  WS-TIAOINF-AAAAVV   PIC 9(6).                                
016400         05  FILLER              PIC 9(1).                                
016500                                                                          
016600     03  WS-TIAOINF-3  REDEFINES WS-TIAOINF-1.                            
016700         05  FILLER              PIC 9(2).                                
016800         05  WS-TIAOINF-AAVVD    PIC 9(5).                                
016900                                                                          
017000                                                                          
017100 01  WS-ADDISPABS                PIC X(50)                                
017200     VALUE 'CARPARTS.ACCE.SELECTIVEQUERY'.                                
017300                                                                          
017400 01  WS-KVRADER-W-START            PIC S9(5) VALUE +1 COMP-3.             
017500 01  WS-KVRADER-W-VISAS            PIC S9(4) VALUE +1 COMP-3.             
017600 01  WS-KVRADER-W-TOTAL            PIC S9(5) VALUE +1 COMP-3.             
017700 01  WS-KVRADER-D-TOTAL            PIC S9(5) VALUE +1 COMP-3.             
017800 01  WS-KVRADER-D-SIST             PIC S9(5) VALUE +0 COMP-3.             
017900     EJECT                                                                
018000                                                                          
018100*    --- DATABASE KEYS & SELECTION PARAMETERS                             
018200 77  W-TIAOINF-FOM               PIC S9(7) COMP-3 VALUE +0200101.         
018300 77  W-TIAOINF-TOM               PIC S9(7) COMP-3 VALUE +0201052.         
018400 77  W-IDUPPDKU                  PIC X(8)  VALUE SPACE.                   
018500 77  W-IDUPPDSU                  PIC X(8)  VALUE SPACE.                   
018510 77  W-BEUPPDSU                  PIC X(35) VALUE SPACE.                   
018600 77  WS-TIAOINF-X                PIC 9(6)  VALUE ZERO.                    
018700                                                                          
018800 01  WS-CALLDB2-AREA.                                                     
018900     03 CALL-TABELL.                                                      
019000        05 CALL-TIAOINF          PIC X     VALUE 'N'.                     
019100        05 CALL-IDUPPDKU         PIC X     VALUE 'N'.                     
019200        05 CALL-IDUPPDSU         PIC X     VALUE 'N'.                     
019300        05 CALL-IDARTNR          PIC X     VALUE 'N'.                     
019310        05 CALL-BEUPPDSU         PIC X     VALUE 'N'.                     
019400                                                                          
019500     03 WS-CALL-TYPE-DB2 REDEFINES CALL-TABELL PIC XXXXX.                 
019600        88 CALL-TYPE-1                      VALUE 'NNNJN'.                
019700        88 CALL-TYPE-2                      VALUE 'NNJNN'.                
019800        88 CALL-TYPE-3                      VALUE 'NNJJN'.                
019900        88 CALL-TYPE-4                      VALUE 'NJNNN'.                
020000        88 CALL-TYPE-5                      VALUE 'NJNJN'.                
020010        88 CALL-TYPE-6                      VALUE 'NJJNN'.                
020020        88 CALL-TYPE-7                      VALUE 'NJJJN'.                
020100        88 CALL-TYPE-8                      VALUE 'JNNNN'.                
020200        88 CALL-TYPE-9                      VALUE 'JNNJN'.                
020300        88 CALL-TYPE-10                     VALUE 'JNJNN'.                
020310        88 CALL-TYPE-11                     VALUE 'JNJJN'.                
020400        88 CALL-TYPE-12                     VALUE 'JJNNN'.                
020410        88 CALL-TYPE-13                     VALUE 'JJNJN'.                
020420        88 CALL-TYPE-14                     VALUE 'JJJNN'.                
021100        88 CALL-TYPE-15                     VALUE 'JJJJN'.                
021101        88 CALL-TYPE-16                     VALUE 'NNNNJ'.                
021102        88 CALL-TYPE-17                     VALUE 'NNNJJ'.                
021103        88 CALL-TYPE-18                     VALUE 'NNJJJ'.                
021104        88 CALL-TYPE-19                     VALUE 'NJJJJ'.                
021105        88 CALL-TYPE-20                     VALUE 'JJJJJ'.                
021106        88 CALL-TYPE-21                     VALUE 'JNNNJ'.                
021107        88 CALL-TYPE-22                     VALUE 'JNJNJ'.                
021108        88 CALL-TYPE-23                     VALUE 'JNNJJ'.                
021109        88 CALL-TYPE-24                     VALUE 'JNJJJ'.                
021110        88 CALL-TYPE-25                     VALUE 'NNJNJ'.                
021120*                                                                         
021200     EJECT                                                                
021300                                                                          
021400*    --- SUB PROGRAMS AND PARAMETER AREAS                                 
021500 01  COMMON-SUB-PROGRAMS.                                                 
021600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
021700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
021800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
021900     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
022000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
022200     SKIP3                                                                
022300                                                                          
022400*    --- PARAMETERS FOR  ABEND                                            
022500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
022600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
022700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
022800 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
022900     SKIP3                                                                
023000                                                                          
023100 01  MESSAGE-CODES.                                                       
023200     03  ERR-INVALID-KEY-FLD-0   PIC X(3)    VALUE '022'.                 
023300     03  ERR-0-MUST-BE-NUMERIC   PIC X(3)    VALUE '024'.                 
023400     03  ERR-0-NOT-FOUND         PIC X(3)    VALUE '025'.                 
023500     03  ERR-KEYS-NOT-ENTERED    PIC X(3)    VALUE '026'.                 
023600     03  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.                 
023700     03  ERR-0-MISSING           PIC X(3)    VALUE '041'.                 
023800     03  ERR-SYSTEM-ERROR        PIC X(3)    VALUE '099'.                 
023900     03  ERR-WRONG-DATE          PIC X(3)    VALUE '102'.                 
024000     03  ERR-2-CONFLICT-FIELDS   PIC X(3)    VALUE '103'.                 
024100     03  INF-BEYOND-EXCEL        PIC X(3)    VALUE '108'.                 
024200     03  ERR-BAD-COMBINATION     PIC X(3)    VALUE '109'.                 
024300     EJECT                                                                
024400*01  -COPY WDATAREA                                                       
024500     EJECT                                                                
025100*                                                                         
025200 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
025300     SKIP3                                                                
025400*01  -COPY WZ01SUB                                                        
025500     EJECT                                                                
025600 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
025700     SKIP3                                                                
025800 01  REQU-AREA.                                                           
025900*    03  -COPY WZ01REQU                                                   
026000*    03  -COPY WB0101I1                                                   
026100     EJECT                                                                
026200                                                                          
026300 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
026400     SKIP3                                                                
026500 01  RESP-AREA.                                                           
026600*    03  -COPY WZ01RESP                                                   
026700*    03  -COPY WB0101O1                                                   
026800     EJECT                                                                
026900                                                                          
027000*    --- WORK-AREAS FOR DL1-SECTIONS                                      
027100*                                                                         
027200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
027300     SKIP3                                                                
027400 01  NYCKLAR-TILL-DLI.                                                    
027500     03  W-IDARTNR-X.                                                     
027600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
027700     03  W-IDLEVNR-X.                                                     
027800         05  W-IDLEVNR           PIC X(5)   VALUE SPACE.                  
027900*    WDD9 KEYS                                                            
028201     03  W-WDD901KY-X.                                                    
028202         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
028203         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
028204     03  W-WDD905KY-X.                                                    
028205         05  W-DAAVROP-X.                                                 
028206             07  W-DAAVROP       PIC  9(6)   VALUE ZERO.                  
028207         05  W-TILEVDAG-X.                                                
028208             07  W-TILEVDAG      PIC  S9     VALUE ZERO COMP-3.           
028209     03  W-KDAVROP-X.                                                     
028210         05  W-KDAVROP           PIC S9(1)   VALUE ZERO COMP-3.           
028220     03  W-DALEVBSK-AVS-X.                                                
028221         05 W-DALEVBSK-AVS       PIC  9(8)   VALUE ZERO.                  
028222*    WDK6 KEYS                                                            
028223     03  W-KDSEGKEY-X.                                                    
028224         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
028225*    WDC1 KEYS                                                            
028226     03  W-WDC101KY-X.                                                    
028227         05  W-IDARTNR-WDC       PIC S9(9)   VALUE ZERO COMP-3.           
028228         05  W-IDMARKBO-WDC      PIC X       VALUE SPACE.                 
028229*    WDL2 KEYS                                                            
028230     03  W-IDPTYP-X.                                                      
028231       05  W-IDPTYP              PIC X(3)    VALUE 'R32'.                 
028232*                                                                         
031400*                                                                         
031500*    --- STATUS-KOD FROM IMS                                              
031600 01  STATUS-WS                   PIC XX.                                  
031700     88  SEGMENT-FINNS                       VALUE '  '.                  
031800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
031900     SKIP2                                                                
032000 01  GODK-STATUSKODER.                                                    
032100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
032200     SKIP3                                                                
032300 01  SSA1                        PIC X(128).                              
032400 01  SSA2                        PIC X(128).                              
032500 01  SSA3                        PIC X(128).                              
032600     EJECT                                                                
032700*    --- IMS DLI FUNCTION CODES                                           
032800*01  -COPY W0003                                                          
032900     EJECT                                                                
033000*    ---  DLI INPUT-OUTPUT AREA                                           
033100                                                                          
033200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD201'.                      
033300 01  DLI-IO-WDD201.                                                       
033400*    03  -COPY WDD201  -PRE WDD2-                                         
033500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
033600 01  DLI-IO-WDD902.                                                       
033700*    03  -COPY WDD902  -PRE WDD902-                                       
033800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
033900 01  DLI-IO-WDD905.                                                       
034000*    03  -COPY WDD905  -PRE WDD905-                                       
034100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD924'.                      
034101 01  DLI-IO-WDD924.                                                       
034102*    03  -COPY WDD924  -PRE WDD924-                                       
034400     EJECT                                                                
034401 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
034402 01  DLI-IO-WDK601.                                                       
034403*    03  -COPY WDK601  -PRE WDK6-                                         
034404     EJECT                                                                
034500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
034600 01  DLI-IO-WDK611.                                                       
034700*    03  -COPY WDK611  -PRE WDK6-                                         
034800     EJECT                                                                
034900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK613'.                      
035000 01  DLI-IO-WDK613.                                                       
035100*    03  -COPY WDK613  -PRE WDK6-                                         
035200     EJECT                                                                
035300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK623'.                      
035400 01  DLI-IO-WDK623.                                                       
035500*    03  -COPY WDK623  -PRE WDK6-                                         
035600     EJECT                                                                
035601 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC101'.                      
035602 01  DLI-IO-WDC101.                                                       
035603*    03  -COPY WDC101  -PRE WDC1-                                         
035604     EJECT                                                                
035605 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL201'.                      
035606 01  DLI-IO-WDL201.                                                       
035607*    03  -COPY WDL201  -PRE WDL201-                                       
035608 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL211'.                      
035609 01  DLI-IO-WDL211.                                                       
035610*    03  -COPY WDL211  -PRE WDL211-                                       
035611 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL221'.                      
035612 01  DLI-IO-WDL221.                                                       
035613*    03  -COPY WDL221  -PRE WDL221-                                       
035614     EJECT                                                                
037500*                                                                         
037600 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
037700       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
037800                                                                          
037900 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
038000 01  DB2-WS.                                                              
038100     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
038200         88  CURSOR-OK                       VALUE 000.                   
038300         88  LINES-EXIST                     VALUE 000.                   
038400         88  LINES-MISSING                   VALUE 100.                   
038500         88  ACCESS-ERROR                    VALUE 904.                   
038600     03  GODK-SQLCODEKODER.                                               
038700         05  GODK-SQLCODE OCCURS 5                                        
038800             INDEXED BY SQLCODE-IX PIC 9(3).                              
038900     EJECT                                                                
039000                                                                          
039100 01  FILLER                      PIC X(16)  VALUE 'TB1ACCE-AREA'.         
039200                                                                          
039300*01  -COPY TB1ACCE -PRE ACCE-                                             
039400     EJECT                                                                
039500                                                                          
039600     EXEC SQL INCLUDE TB1ACCE END-EXEC.                                   
039700     EJECT                                                                
039800                                                                          
039900 LINKAGE SECTION.                                                         
040000*01  -COPY W0009   -PRE MSG-                                              
040100                                                                          
040200*01  -COPY W0008  -PRE WDD2-                                              
040300     05  FILLER                  PIC X.                                   
040400                                                                          
040500*01  -COPY W0008  -PRE WDD9-                                              
040600     05  FILLER                  PIC X.                                   
040700                                                                          
040800*01  -COPY W0008  -PRE WDK6-                                              
040900     05  FILLER                  PIC X.                                   
041000                                                                          
041001*01  -COPY W0008  -PRE WDC1-                                              
041002     05  FILLER                  PIC X.                                   
041003                                                                          
041004*01  -COPY W0008  -PRE WDL2-                                              
041005     05  FILLER                  PIC X.                                   
041006                                                                          
042400 PROCEDURE DIVISION  USING MSG-PCB WDD2-PCB WDD9-PCB WDK6-PCB             
042500                                   WDC1-PCB WDL2-PCB.                     
042700 MAIN SECTION.                                                            
042800     ENTRY 'DLITCBL' USING MSG-PCB WDD2-PCB WDD9-PCB WDK6-PCB             
042900                                   WDC1-PCB WDL2-PCB.                     
043200     PERFORM S01-GET-REQUEST                                              
043300     IF SUB-KDRC = 0                                                      
043400       PERFORM A-INIT                                                     
043500       PERFORM B-CHECK-KEYS                                               
043600       IF KEYS-OK                                                         
043700         PERFORM C-CHECK-START-POSITION                                   
043800         PERFORM F-READ-SHOW-INFO                                         
043900       END-IF                                                             
044000       PERFORM S02-RETURN-RESPONSE                                        
044100     END-IF                                                               
044200                                                                          
044210     IF KEYS-OK                                                           
044300       PERFORM Z-FINIT                                                    
044310     END-IF                                                               
044400     MOVE ZERO TO RETURN-CODE                                             
044500     GOBACK                                                               
044600     .                                                                    
044700     EJECT                                                                
044800 A-INIT SECTION.                                                          
044900                                                                          
045000     INITIALIZE GODK-SQLCODEKODER                                         
045100                                                                          
045200     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
045300                                        W-DATUM-SOK                       
045310                                                                          
045400     MOVE ALL '+' TO RESP-AREA                                            
045500                                                                          
045600     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
045700                     RESP-IDMSG-INFO                                      
045800                     RESP-IDELMT-ERROR                                    
045900     MOVE ZERO    TO RESP-KVRADER-W-START                                 
046000                     RESP-KVRADER-W-VISAS                                 
046100                     RESP-KVRADER-W-TOTAL                                 
046200                     RESP-KVRADER-D-SIST                                  
046300                     RESP-KVRADER-D-TOTAL                                 
046400                                                                          
046500     MOVE ZERO    TO W-IDARTNR                                            
046600                     W-TIAOINF-FOM                                        
046700                     W-TIAOINF-TOM                                        
046800     MOVE SPACE   TO W-IDUPPDKU                                           
046900                     W-IDUPPDSU                                           
047000                                                                          
047100     MOVE '001'   TO RESP-IDMSGVER                                        
047200     .                                                                    
047300     EJECT                                                                
047400 B-CHECK-KEYS SECTION.                                                    
047500     SKIP2                                                                
047600     MOVE J TO KEYS-SW                                                    
047610     MOVE 'NNNNN' TO CALL-TABELL                                          
047700*    -- CHECK KDPGMACT   S=SEARCH   N=NEXT  J=JUMP FROM WB0102            
047800     IF REQU-KDPGMACT = 'S' OR  'N' OR 'J'                                
047900       CONTINUE                                                           
048000     ELSE                                                                 
048100        MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                         
048200        MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                            
048300        MOVE N TO KEYS-SW                                                 
048400     END-IF                                                               
048500                                                                          
048600     IF REQU-TIAOINF-AAVV-FOM-KEY NOT = ALL '+' AND SPACE                 
048700*************************   CHECK TIAOINF-FOM ***********                 
048800       IF REQU-TIAOINF-AAVV-FOM-KEY NUMERIC                               
048900                                                                          
049000         MOVE REQU-TIAOINF-AAVV-FOM-KEY TO WS-TIAOINF-AAVV                
049100         MOVE 1 TO WS-TIAOINF-D                                           
049200         MOVE WS-TIAOINF-AAVVD        TO DAT-I-TIDATUM                    
049300                                                                          
049400         MOVE 'AAVVD' TO DAT-KDDATFORM                                    
049500         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
049600                             DAT-O-TIDATUM DAT-KDSVAR                     
049700         IF DAT-KDSVAR-OK                                                 
049800           MOVE DAT-TISEKEL        TO WS-TIAOINF-SEK                      
049900           MOVE WS-TIAOINF-AAAAVV  TO W-TIAOINF-FOM                       
050000           MOVE J TO CALL-TIAOINF                                         
050100         ELSE                                                             
050200           MOVE N TO KEYS-SW                                              
050300           MOVE ERR-WRONG-DATE     TO RESP-IDMSG-ERROR                    
050400           MOVE 'TIAOINF-AAVV-FOM' TO RESP-IDELMT-ERROR                   
050500         END-IF                                                           
050600                                                                          
050700       ELSE                                                               
050800         MOVE N TO KEYS-SW                                                
050900         MOVE ERR-0-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                   
051000         MOVE 'TIAOINF-AAVV-FOM'    TO RESP-IDELMT-ERROR                  
051100       END-IF                                                             
051200                                                                          
051300*************** CHECK RELATIVE VALUE IN TIAOINF-TOM ***********           
051400       IF KEYS-OK                                                         
051500         INSPECT REQU-TIAOINF-AAVV-TOM-KEY                                
051600                                      REPLACING ALL SPACE BY ZERO         
051700         IF REQU-TIAOINF-AAVV-TOM-KEY = ALL '+' OR SPACE                  
051800*            --- USE THE SAME WEEK AS IN FOM-KEY                          
051900             MOVE WS-TIAOINF-AAAAVV TO W-TIAOINF-TOM                      
052000             MOVE WS-TIAOINF-AAAAVV (3:)                                  
052100                                    TO RESP-TIAOINF-AAVV-TOM-KEY          
052200                                       REQU-TIAOINF-AAVV-TOM-KEY          
052300         ELSE                                                             
052400           IF REQU-TIAOINF-AAVV-TOM-KEY NUMERIC                           
052500             IF REQU-TIAOINF-AAVV-TOM-KEY                                 
052600              < REQU-TIAOINF-AAVV-FOM-KEY                                 
052700                 MOVE WS-TIAOINF-AAAAVV (3:)                              
052800                                    TO RESP-TIAOINF-AAVV-TOM-KEY          
052900                                       REQU-TIAOINF-AAVV-TOM-KEY          
053000             END-IF                                                       
053100           END-IF                                                         
053200         END-IF                                                           
053300                                                                          
053400         IF REQU-TIAOINF-AAVV-TOM-KEY NOT = ALL '+' AND SPACE             
053500           IF REQU-TIAOINF-AAVV-TOM-KEY NUMERIC                           
053600             MOVE REQU-TIAOINF-AAVV-TOM-KEY TO WS-TIAOINF-AAVV            
053700             MOVE 5 TO WS-TIAOINF-D                                       
053800             MOVE WS-TIAOINF-AAVVD TO DAT-I-TIDATUM                       
053900             MOVE 'AAVVD' TO DAT-KDDATFORM                                
054000             CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM              
054100                                 DAT-O-TIDATUM DAT-KDSVAR                 
054200             IF DAT-KDSVAR-OK                                             
054300               MOVE DAT-TISEKEL    TO WS-TIAOINF-SEK                      
054400               MOVE WS-TIAOINF-AAAAVV TO W-TIAOINF-TOM                    
054500             ELSE                                                         
054600               MOVE N TO KEYS-SW                                          
054700               MOVE ERR-WRONG-DATE   TO RESP-IDMSG-ERROR                  
054800               MOVE 'TIAOINF-AAVV-TOM' TO RESP-IDELMT-ERROR               
054900             END-IF                                                       
055000           ELSE                                                           
055100             MOVE N TO KEYS-SW                                            
055200             MOVE ERR-0-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR               
055300             MOVE 'TIAOINF-AAVV-TOM'  TO RESP-IDELMT-ERROR                
055400           END-IF                                                         
055500         ELSE                                                             
055600           MOVE N TO KEYS-SW                                              
055700           MOVE ERR-KEYS-NOT-ENTERED TO RESP-IDMSG-ERROR                  
055800           MOVE       'KEY'        TO RESP-IDELMT-ERROR                   
055900         END-IF                                                           
056000       END-IF                                                             
056100     END-IF                                                               
056200                                                                          
056300*    --- KOLLA ARTIKELNUMMER                                              
056400     IF REQU-IDARTNR-KEY NOT = ALL '+' AND ZERO                           
056500       IF REQU-IDARTNR-KEY NUMERIC                                        
056600         MOVE REQU-IDARTNR-KEY TO W-IDARTNR                               
056700                                  RESP-IDARTNR-KEY                        
056800         MOVE J TO CALL-IDARTNR                                           
056900       ELSE                                                               
057000         MOVE N TO KEYS-SW                                                
057100         MOVE ERR-0-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                   
057200         MOVE 'KEY'                 TO RESP-IDELMT-ERROR                  
057300       END-IF                                                             
057400     END-IF                                                               
057500                                                                          
057600*    --- KOLLA KU-UPPDRAG                                                 
057700     IF REQU-IDUPPDKU-KEY NOT = ALL '+' AND SPACE                         
057800       IF REQU-IDUPPDKU-KEY NUMERIC                                       
057900         MOVE REQU-IDUPPDKU-KEY TO W-IDUPPDKU                             
058000                                  RESP-IDUPPDKU-KEY                       
058100         MOVE J TO CALL-IDUPPDKU                                          
058200       ELSE                                                               
058300         MOVE N TO KEYS-SW                                                
058400         MOVE ERR-0-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                   
058500         MOVE 'KEY'                 TO RESP-IDELMT-ERROR                  
058600       END-IF                                                             
058700     END-IF                                                               
058800                                                                          
058900*    --- KOLLA SU-UPPDRAG                                                 
059000     IF REQU-IDUPPDSU-KEY NOT = ALL '+' AND SPACE                         
059100       IF REQU-IDUPPDSU-KEY NUMERIC                                       
059200         MOVE REQU-IDUPPDSU-KEY TO W-IDUPPDSU                             
059300                                  RESP-IDUPPDSU-KEY                       
059400         MOVE J TO CALL-IDUPPDSU                                          
059500       ELSE                                                               
059600         MOVE N TO KEYS-SW                                                
059700         MOVE ERR-0-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                   
059800         MOVE 'KEY'                 TO RESP-IDELMT-ERROR                  
059900       END-IF                                                             
060000     END-IF                                                               
060010*    --- KOLLA SU-DESC                                                    
060020     IF REQU-BEUPPDSU-KEY NOT = ALL '+' AND SPACE                         
060040         MOVE REQU-BEUPPDSU-KEY TO W-BEUPPDSU                             
060050                                   RESP-BEUPPDSU-KEY                      
060052         MOVE ZERO TO TALLY                                               
060053         INSPECT FUNCTION REVERSE(W-BEUPPDSU)                             
060054         TALLYING TALLY FOR LEADING SPACE                                 
060055         IF TALLY > ZERO                                                  
060056           INSPECT W-BEUPPDSU(LTEXT - TALLY:)                             
060057           CONVERTING SPACE TO '%'                                        
060058         END-IF                                                           
060060         MOVE J TO CALL-BEUPPDSU                                          
060070     ELSE                                                                 
060080         MOVE N TO CALL-BEUPPDSU                                          
060093     END-IF                                                               
060100*    --- KOLLA CALL-TYPE                                                  
060200     IF CALL-TYPE-1                                                       
060300     OR CALL-TYPE-2                                                       
060400     OR CALL-TYPE-3                                                       
060500     OR CALL-TYPE-4                                                       
060600     OR CALL-TYPE-5                                                       
060610     OR CALL-TYPE-6                                                       
060620     OR CALL-TYPE-7                                                       
060700     OR CALL-TYPE-8                                                       
060800     OR CALL-TYPE-9                                                       
060900     OR CALL-TYPE-10                                                      
060910     OR CALL-TYPE-11                                                      
061000     OR CALL-TYPE-12                                                      
061010     OR CALL-TYPE-13                                                      
061020     OR CALL-TYPE-14                                                      
061030     OR CALL-TYPE-15                                                      
061040     OR CALL-TYPE-16                                                      
061050     OR CALL-TYPE-17                                                      
061060     OR CALL-TYPE-18                                                      
061070     OR CALL-TYPE-19                                                      
061080     OR CALL-TYPE-20                                                      
061090     OR CALL-TYPE-21                                                      
061091     OR CALL-TYPE-22                                                      
061092     OR CALL-TYPE-23                                                      
061093     OR CALL-TYPE-24                                                      
061094     OR CALL-TYPE-25                                                      
061100        CONTINUE                                                          
061200     ELSE                                                                 
061300        MOVE N TO KEYS-SW                                                 
061400        MOVE ERR-BAD-COMBINATION  TO RESP-IDMSG-ERROR                     
061500        MOVE 'KEY' TO RESP-IDELMT-ERROR                                   
061600     END-IF                                                               
061700     .                                                                    
061800     EJECT                                                                
061900                                                                          
062000 C-CHECK-START-POSITION SECTION.                                          
062100     SKIP2                                                                
062200*    --- START POSITION OF SHOWN LINES                                    
062300*    --- SET THE PROPER START AND END LINE FOR THIS SET OF LINES          
062400     IF REQU-KVRADER-W-START NOT = ALL '+'                                
062500       IF REQU-KVRADER-W-START NUMERIC                                    
062600         EVALUATE REQU-KDPGMACT                                           
062700                                                                          
062800           WHEN 'S'                                                       
062900               MOVE +1   TO WS-KVRADER-W-START                            
063000               MOVE ZERO TO WS-KVRADER-W-VISAS                            
063100               MOVE ZERO TO WS-KVRADER-W-TOTAL                            
063200               MOVE ZERO TO WS-KVRADER-D-TOTAL                            
063300               MOVE ZERO TO WS-KVRADER-D-SIST                             
063400           WHEN 'N'                                                       
063500*              -- INGEN NY TOTAL BEHÖVS FÖR GAMLA NYCKLAR                 
063600               MOVE REQU-KVRADER-W-TOTAL   TO RESP-KVRADER-W-TOTAL        
063700               MOVE REQU-KVRADER-D-TOTAL   TO RESP-KVRADER-D-TOTAL        
063800               MOVE REQU-KVRADER-D-SIST    TO WS-KVRADER-D-SIST           
063900*              -- BERÄKNA NYA STARTRADEN                                  
064000               ADD REQU-KVRADER-W-START                                   
064100                              RESP-MAX GIVING WS-KVRADER-W-START          
064200           WHEN 'J'                                                       
064300               MOVE REQU-KVRADER-W-START   TO WS-KVRADER-W-START          
064400               MOVE ZERO TO WS-KVRADER-W-VISAS                            
064500               MOVE ZERO TO WS-KVRADER-W-TOTAL                            
064600               MOVE ZERO TO WS-KVRADER-D-TOTAL                            
064700               MOVE ZERO TO WS-KVRADER-D-SIST                             
064800           WHEN OTHER CONTINUE                                            
064900         END-EVALUATE                                                     
065000       ELSE                                                               
065100           MOVE +1 TO WS-KVRADER-W-START                                  
065200       END-IF                                                             
065300     ELSE                                                                 
065400         MOVE +1 TO WS-KVRADER-W-START                                    
065500     END-IF                                                               
065600                                                                          
065700     MOVE WS-KVRADER-W-START TO RESP-KVRADER-W-START                      
065800     .                                                                    
065900     EJECT                                                                
066000                                                                          
066100 F-READ-SHOW-INFO SECTION.                                                
066200     SKIP2                                                                
066300     MOVE ZERO TO RESP-IX, DB2-IX                                         
066400                                                                          
066700     IF NOT CALL-TYPE-3                                                   
066800*      -- DE ANDRA CALL-TYPERNA, KAN GE FLER ÄN EN RAD                    
066900*      -- BESTÄM FÖRST  WS-KVRADER-D-TOTAL                                
067000*      -- KDPGMACT 'J' (JUMP) SÄTTS FRÅN WB0102B OCH MENINGEN ÄR          
067100*      --                     ATT FORTSÄTTA VISNING PÅ SAMMA SIDA         
067200       IF REQU-KDPGMACT = 'S' OR 'J'                                      
067300       OR ( REQU-KDPGMACT NOT = 'S'                                       
067400       AND ( REQU-KVRADER-D-TOTAL = ZERO                                  
067500          OR REQU-KVRADER-D-TOTAL = ALL '+' ) )                           
067600                                                                          
067700         PERFORM FA-READ-TOTAL-LINES-DB2                                  
067800         MOVE WS-KVRADER-D-TOTAL TO WS-KVRADER-W-TOTAL                    
067900       ELSE                                                               
068000**       --- ANVÄND FÖRSTA 'S'-LÄSNINGS VÄRDE                             
068100         MOVE REQU-KVRADER-D-TOTAL TO WS-KVRADER-D-TOTAL                  
068200         MOVE REQU-KVRADER-W-TOTAL TO WS-KVRADER-W-TOTAL                  
068300       END-IF                                                             
068400       MOVE WS-KVRADER-D-TOTAL TO RESP-KVRADER-D-TOTAL                    
068500       MOVE WS-KVRADER-W-TOTAL TO RESP-KVRADER-W-TOTAL                    
068600                                                                          
068700*      -- NU ÄR TOTAL-SIFFRORNA BESTÄMDA                                  
068800*      -- LÄS RADERNA OCH SKRIV UT DATA                                   
068900                                                                          
069000*      --LÄS FÖRSTA RADEN I DB2 FÖR URVALET                               
069100*      --OCH LÄS FRAM TILL RÄTT STARTRAD                                  
069200       PERFORM FD-OPEN-TB1ACCE-CURSOR                                     
069300       IF CURSOR-OK                                                       
069400         PERFORM FE-FETCH-TB1ACCE-CRS                                     
069500         MOVE +1 TO DB2-IX                                                
069600       END-IF                                                             
069700       IF LINES-EXIST                                                     
069800         PERFORM UNTIL LINES-MISSING                                      
069900                 OR  DB2-IX > WS-KVRADER-D-SIST                           
070000*                -- VID 'S' ÄR D-SIST = ZERO, DB2 STÅR PÅ POST 1          
070100*                -- VID 'J' ÄR D-SIST = ZERO, DB2 STÅR PÅ POST 1          
070200*                -- VID 'N' ÄR D-SIST = DEN SIST LÄSTA DB2-RADEN          
070300           PERFORM FE-FETCH-TB1ACCE-CRS                                   
070400           ADD +1 TO DB2-IX                                               
070500         END-PERFORM                                                      
070600                                                                          
070700         IF LINES-EXIST                                                   
070800*          --MOVE ALL HITS UNTIL RESP-AREA IS FULL OR NO-DATA             
070900           IF REQU-KDPGMACT = 'S' OR 'N'                                  
071000               PERFORM UNTIL LINES-MISSING                                
071100                          OR RESP-IX >= RESP-MAX                          
071200                   PERFORM FC1-CHECK-WDK6                                 
071300                   ADD +1 TO RESP-IX                                      
071400                   PERFORM FC2-FILL-THE-RESPONSE-AREA                     
071500                   PERFORM FE-FETCH-TB1ACCE-CRS                           
071600                   ADD +1 TO DB2-IX                                       
071700               END-PERFORM                                                
071800           ELSE                                                           
071900               MOVE +1   TO JUMP-IX                                       
072000               PERFORM UNTIL LINES-MISSING                                
072100                          OR RESP-IX >= RESP-MAX                          
072200                   PERFORM FC1-CHECK-WDK6                                 
072300                   IF JUMP-IX >= WS-KVRADER-W-START                       
072400                       ADD +1 TO RESP-IX                                  
072500                       PERFORM FC2-FILL-THE-RESPONSE-AREA                 
072600                   END-IF                                                 
072700                   ADD +1 TO JUMP-IX                                      
072800                   PERFORM FE-FETCH-TB1ACCE-CRS                           
072900                   ADD +1 TO DB2-IX                                       
073000               END-PERFORM                                                
073100           END-IF                                                         
073200                                                                          
073300           MOVE RESP-IX        TO RESP-KVRADER-W-VISAS                    
073400           ADD   DB2-IX -1 GIVING RESP-KVRADER-D-SIST                     
073500                                                                          
073600*          --BLANK OUT REMAINING ROWS IN RESP-AREA                        
073700           ADD +1 TO RESP-IX                                              
073800           PERFORM UNTIL RESP-IX >= RESP-MAX                              
073900               MOVE SPACE TO RESP-TABELLRAD (RESP-IX)                     
074000               ADD +1 TO RESP-IX                                          
074100           END-PERFORM                                                    
074200         END-IF                                                           
074300       ELSE                                                               
074400         IF LINES-MISSING                                                 
074500           MOVE +0                  TO RESP-KVRADER-D-TOTAL               
074600                                       RESP-KVRADER-W-TOTAL               
074700                                       RESP-KVRADER-W-VISAS               
074800                                       RESP-KVRADER-W-START               
074900           MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                   
075000           MOVE 'TIOAINF-FOM'       TO RESP-IDELMT-ERROR                  
075100         ELSE                                                             
075200           MOVE ERR-0-NOT-FOUND TO RESP-IDMSG-ERROR                       
075300           MOVE 'TB1ACCE'     TO RESP-IDELMT-ERROR                        
075400         END-IF                                                           
075500       END-IF                                                             
075600     ELSE                                                                 
075700*      --- SÖKNING MED (IDARTNR + IDUPPDSU) SOM GER MAX 1 RAD             
075800       PERFORM FB-READ-ONE-PARTLINE-DB2                                   
075900                                                                          
076000       IF LINES-EXIST                                                     
076100         PERFORM FC1-CHECK-WDK6                                           
076200*        --- MOVE OUT THE DESIRED HIT. FILL UP WITH DL1-DATA              
076300         MOVE +1 TO RESP-IX                                               
076400                                                                          
076500         PERFORM FC2-FILL-THE-RESPONSE-AREA                               
076600                                                                          
076700         MOVE RESP-IX TO RESP-KVRADER-W-VISAS                             
076800                         RESP-KVRADER-W-TOTAL                             
076900                         RESP-KVRADER-W-START                             
077000                         RESP-KVRADER-D-TOTAL                             
077100                         RESP-KVRADER-D-SIST                              
077200                                                                          
077300*        --- BLANK OUT REMAINING ROWS IN RESP-AREA                        
077400         ADD +1 TO RESP-IX                                                
077500         PERFORM UNTIL RESP-IX >= RESP-MAX                                
077600             MOVE SPACE TO RESP-TABELLRAD (RESP-IX)                       
077700             ADD +1 TO RESP-IX                                            
077800         END-PERFORM                                                      
077900       END-IF                                                             
078000     END-IF                                                               
078100     .                                                                    
078200     EJECT                                                                
078300                                                                          
078400 FA-READ-TOTAL-LINES-DB2 SECTION.                                         
078500     SKIP2                                                                
078600*    ---LÄS FÖR RÄTT NYCKELBEGREPP                                        
078700     EVALUATE TRUE                                                        
078900       WHEN CALL-TYPE-1  PERFORM DB2-GET-TOTAL-LINES-TYPE-1               
079100       WHEN CALL-TYPE-2  PERFORM DB2-GET-TOTAL-LINES-TYPE-2               
079200*****       CALL-TYPE-3  KÖRS I SEKTION "FB-READ-ONE-PARTLINE"            
079300       WHEN CALL-TYPE-4  PERFORM DB2-GET-TOTAL-LINES-TYPE-4               
079500       WHEN CALL-TYPE-5  PERFORM DB2-GET-TOTAL-LINES-TYPE-5               
079600       WHEN CALL-TYPE-6  PERFORM DB2-GET-TOTAL-LINES-TYPE-6               
079610       WHEN CALL-TYPE-7  PERFORM DB2-GET-TOTAL-LINES-TYPE-7               
079700       WHEN CALL-TYPE-8  PERFORM DB2-GET-TOTAL-LINES-TYPE-8               
079900       WHEN CALL-TYPE-9  PERFORM DB2-GET-TOTAL-LINES-TYPE-9               
080100       WHEN CALL-TYPE-10 PERFORM DB2-GET-TOTAL-LINES-TYPE-10              
080200       WHEN CALL-TYPE-11 PERFORM DB2-GET-TOTAL-LINES-TYPE-11              
080300       WHEN CALL-TYPE-12 PERFORM DB2-GET-TOTAL-LINES-TYPE-12              
080400       WHEN CALL-TYPE-13 PERFORM DB2-GET-TOTAL-LINES-TYPE-13              
080410       WHEN CALL-TYPE-14 PERFORM DB2-GET-TOTAL-LINES-TYPE-14              
080420       WHEN CALL-TYPE-15 PERFORM DB2-GET-TOTAL-LINES-TYPE-15              
080430       WHEN CALL-TYPE-16 PERFORM DB2-GET-TOTAL-LINES-TYPE-16              
080440       WHEN CALL-TYPE-17 PERFORM DB2-GET-TOTAL-LINES-TYPE-17              
080450       WHEN CALL-TYPE-18 PERFORM DB2-GET-TOTAL-LINES-TYPE-18              
080460       WHEN CALL-TYPE-19 PERFORM DB2-GET-TOTAL-LINES-TYPE-19              
080470       WHEN CALL-TYPE-20 PERFORM DB2-GET-TOTAL-LINES-TYPE-20              
080480       WHEN CALL-TYPE-21 PERFORM DB2-GET-TOTAL-LINES-TYPE-21              
080490       WHEN CALL-TYPE-22 PERFORM DB2-GET-TOTAL-LINES-TYPE-22              
080491       WHEN CALL-TYPE-23 PERFORM DB2-GET-TOTAL-LINES-TYPE-23              
080492       WHEN CALL-TYPE-24 PERFORM DB2-GET-TOTAL-LINES-TYPE-24              
080493       WHEN CALL-TYPE-25 PERFORM DB2-GET-TOTAL-LINES-TYPE-25              
080500       WHEN OTHER        CALL ABEND USING RKOD-ABEND-UTAN-DUMP            
080600     END-EVALUATE                                                         
080700                                                                          
080800*    --- WS-KVRADER-D-TOTAL ÄR NU ALLA RADER ENLIGT URVAL                 
080900     IF LINES-MISSING OR ACCESS-ERROR                                     
081000       MOVE +0                  TO RESP-KVRADER-D-TOTAL                   
081100       MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                       
081110       EVALUATE TRUE                                                      
081120         WHEN CALL-TYPE-1                                                 
081130                    MOVE 'ARTNR         ' TO RESP-IDELMT-ERROR            
081140         WHEN CALL-TYPE-2                                                 
081141                    MOVE 'SU            ' TO RESP-IDELMT-ERROR            
081142*****         CALL-TYPE-3  KÖRS I SEKTION "FB-READ-ONE-PARTLINE"          
081160         WHEN CALL-TYPE-4                                                 
081170                    MOVE 'KU            ' TO RESP-IDELMT-ERROR            
081180         WHEN CALL-TYPE-5                                                 
081182                    MOVE 'KU+ARTNR      ' TO RESP-IDELMT-ERROR            
081183         WHEN CALL-TYPE-6                                                 
081184                    MOVE 'SU+KU         ' TO RESP-IDELMT-ERROR            
081185         WHEN CALL-TYPE-7                                                 
081186                    MOVE 'SU+KU+ARTNR   ' TO RESP-IDELMT-ERROR            
081191         WHEN CALL-TYPE-8                                                 
081192                    MOVE 'ÄO            ' TO RESP-IDELMT-ERROR            
081193         WHEN CALL-TYPE-9                                                 
081194                    MOVE 'ÄO+ARTNR      ' TO RESP-IDELMT-ERROR            
081195         WHEN CALL-TYPE-10                                                
081196                    MOVE 'ÄO+SU         ' TO RESP-IDELMT-ERROR            
081197         WHEN CALL-TYPE-11                                                
081198                    MOVE 'ÄO+SU+ARTNR   ' TO RESP-IDELMT-ERROR            
081199         WHEN CALL-TYPE-12                                                
081200                    MOVE 'ÄO+KU         ' TO RESP-IDELMT-ERROR            
081201         WHEN CALL-TYPE-13                                                
081202                    MOVE 'ÄO+KU+ARTNR   ' TO RESP-IDELMT-ERROR            
081210         WHEN CALL-TYPE-14                                                
081220                    MOVE 'ÄO+KU+SU      ' TO RESP-IDELMT-ERROR            
081230         WHEN CALL-TYPE-15                                                
081240                    MOVE 'ÄO+KU+SU+ARTNR' TO RESP-IDELMT-ERROR            
081241         WHEN CALL-TYPE-16                                                
081242                    MOVE 'BESU          ' TO RESP-IDELMT-ERROR            
081243         WHEN CALL-TYPE-17                                                
081244                    MOVE 'ARTNR+BESU    ' TO RESP-IDELMT-ERROR            
081245         WHEN CALL-TYPE-18                                                
081246                    MOVE 'SU+ARTNR+BESU ' TO RESP-IDELMT-ERROR            
081247         WHEN CALL-TYPE-19                                                
081248                    MOVE 'KU+SU+ARTNR+BESU'                               
081249                                          TO RESP-IDELMT-ERROR            
081250         WHEN CALL-TYPE-20                                                
081251                    MOVE 'ÄO+KU+SU+ARTNR+BESU'                            
081252                                          TO RESP-IDELMT-ERROR            
081253         WHEN CALL-TYPE-21                                                
081254                    MOVE 'ÄO+BESU'                                        
081255                                          TO RESP-IDELMT-ERROR            
081256         WHEN CALL-TYPE-22                                                
081257                    MOVE 'ÄO+SU+BESU'                                     
081258                                          TO RESP-IDELMT-ERROR            
081259         WHEN CALL-TYPE-23                                                
081260                    MOVE 'ÄO+ARTNR+BESU'                                  
081261                                          TO RESP-IDELMT-ERROR            
081262         WHEN CALL-TYPE-24                                                
081263                    MOVE 'ÄO++SU+ARTNR+BESU'                              
081264                                          TO RESP-IDELMT-ERROR            
081265         WHEN CALL-TYPE-25                                                
081266                    MOVE 'SU+BESU'                                        
081267                                          TO RESP-IDELMT-ERROR            
081270         END-EVALUATE                                                     
081300     ELSE                                                                 
081400       IF WS-KVRADER-D-TOTAL > +65536                                     
081500         MOVE INF-BEYOND-EXCEL TO RESP-IDMSG-INFO                         
081600       END-IF                                                             
081700     END-IF                                                               
081800     .                                                                    
081900     EJECT                                                                
082000                                                                          
082100 FB-READ-ONE-PARTLINE-DB2 SECTION.                                        
082200     SKIP2                                                                
082300     IF W-IDARTNR > ZERO                                                  
082400*      READ DIRECTLY 1 LINE, A SOLELY REQUESTED PARTNO.                   
082500       PERFORM DB2-SELECT-TB1ACCE-TAB                                     
082600       IF LINES-EXIST                                                     
082700         MOVE +1                  TO RESP-KVRADER-D-TOTAL                 
082800                                     RESP-KVRADER-W-START                 
082900                                     RESP-KVRADER-W-TOTAL                 
083000                                     RESP-KVRADER-W-VISAS                 
083100       ELSE                                                               
083200         MOVE +0                  TO RESP-KVRADER-D-TOTAL                 
083300                                     RESP-KVRADER-W-TOTAL                 
083400                                     RESP-KVRADER-W-START                 
083500                                     RESP-KVRADER-W-VISAS                 
083600         MOVE ERR-LINES-NOT-FOUND TO RESP-IDMSG-ERROR                     
083700         MOVE 'ARTNR+SU   '       TO RESP-IDELMT-ERROR                    
083800       END-IF                                                             
083900     END-IF                                                               
084000     .                                                                    
084100     EJECT                                                                
084200                                                                          
084300 FC1-CHECK-WDK6    SECTION.                                               
084400     SKIP2                                                                
084500     MOVE ACCE-IDARTNR TO W-IDARTNR                                       
084600                                                                          
084700     MOVE N TO SW-WDK601 SW-WDK611 SW-WDK613 SW-WDK623                    
084800                                                                          
084900     PERFORM DLI-GU-WDK601                                                
085000     IF SEGMENT-FINNS                                                     
085100       SET WDK601 TO TRUE                                                 
085200                                                                          
085300       PERFORM DLI-GNP-WDK611                                             
085400       IF SEGMENT-FINNS                                                   
085500         SET WDK611 TO TRUE                                               
085501                                                                          
085510         PERFORM DLI-GNP-WDK623                                           
085520         IF SEGMENT-FINNS                                                 
085530           SET WDK623 TO TRUE                                             
085600         END-IF                                                           
085601                                                                          
085610       END-IF                                                             
085700       PERFORM DLI-GNP-WDK613                                             
085800       IF SEGMENT-FINNS                                                   
085900         SET WDK613 TO TRUE                                               
086000       END-IF                                                             
086100     END-IF                                                               
086200     .                                                                    
086300     EJECT                                                                
086400 FC2-FILL-THE-RESPONSE-AREA  SECTION.                                     
086500     SKIP2                                                                
086600     INITIALIZE RESP-TABELLRAD (RESP-IX)                                  
086700                                                                          
086800     PERFORM FCA-MOVE-CURRENT-DB2-RECORD                                  
086900                                                                          
087000*    --- ADD UP WITH DATA FROM PULS DL1-BASES                             
087100                                                                          
087200     IF WDK601                                                            
087300         PERFORM FCB0-MOVE-CURRENT-WDK601-DATA                            
087400         IF WDK611                                                        
087500           PERFORM FCB1-MOVE-CURRENT-WDK611-DATA                          
087801           IF WDK623                                                      
087803             IF  WDK6-CLAG-KDAVT = +1                                     
087804             AND WDK6-AVT-IDLEVNR-AVT = WDK6-ART-IDLEVNR                  
087805               PERFORM FCB2-MOVE-CURRENT-WDK623-DATA                      
087830             END-IF                                                       
087840           END-IF                                                         
087900         ELSE                                                             
088000*            --- CLEAR ALL WDK611-DATA                                    
088100             MOVE OBSOLETE TO RESP-FLRPULS (RESP-IX)                      
088200             MOVE '-'    TO RESP-IDINK     (RESP-IX)                      
088300             MOVE ZERO   TO RESP-IDANSK    (RESP-IX)                      
088400                              WS-NUM-IDANSK                               
088500             MOVE ZERO   TO RESP-KDEMBKOD-2(RESP-IX)                      
088600             MOVE ZERO   TO RESP-KVLS      (RESP-IX)                      
088610             MOVE ZERO   TO RESP-TIAVTAL   (RESP-IX)                      
088700         END-IF                                                           
088800                                                                          
088900         IF WDK613                                                        
089000           IF WDK6-EMB-KDEMBKOD = +073                                    
089100             MOVE 'S'    TO RESP-KDFRPTYP(RESP-IX)                        
089200           ELSE                                                           
089300             MOVE 'P'    TO RESP-KDFRPTYP(RESP-IX)                        
089400           END-IF                                                         
089500         ELSE                                                             
089600***        --- UPPGIFT SAKNAS I PULS                                      
089700           IF WS-KDFRPTYP = SPACE                                         
089800             MOVE '-'    TO RESP-KDFRPTYP(RESP-IX)                        
089900           ELSE                                                           
090000***          --- MANUELLT INLAGD KDFRPTYP LÄGGS UT                        
090100             MOVE WS-KDFRPTYP TO RESP-KDFRPTYP(RESP-IX)                   
090200           END-IF                                                         
090300         END-IF                                                           
090400     ELSE                                                                 
090500         PERFORM FCC-CLEAR-RESP-WDK6-DATA                                 
090600         PERFORM FCG-CLEAR-RESP-WDD9-DATA                                 
090700         MOVE ZERO TO RESP-KVLS(RESP-IX)                                  
090800     END-IF                                                               
090900                                                                          
091000*    WDD9                                                                 
091100     PERFORM FCG-CLEAR-RESP-WDD9-DATA                                     
091200     MOVE W-IDARTNR  TO W-IDARTNR-D9                                      
091300     MOVE WC-CDC-SE  TO W-IDDC-D9                                         
091400     PERFORM DLI-GU-WDD902                                                
091500     IF SEGMENT-FINNS                                                     
091501*    WDD905:1                                                             
091502*    HÄMTA KDLEVPST FÖR NÄSTA AVROP                                       
091503         PERFORM FCF-MOVE-WDD905-DATA-TO-RESP                             
091504     END-IF                                                               
091505                                                                          
091506     MOVE W-IDARTNR  TO W-IDARTNR-D9                                      
091507     MOVE WC-CDC-SE  TO W-IDDC-D9                                         
091508     PERFORM DLI-GU-WDD902                                                
091509     IF SEGMENT-FINNS                                                     
091510*    WDD924                                                               
091520*    HÄMTA TILEVBSK-INL FRÅN AKTUELL LEVERANSPLAN                         
091530         PERFORM FCH-MOVE-WDD924-DATA-TO-RESP                             
091540        IF SEGMENT-SAKNAS                                                 
091550            MOVE W-IDARTNR  TO W-IDARTNR-D9                               
091560            MOVE WC-CDC-SE  TO W-IDDC-D9                                  
091561            PERFORM DLI-GU-WDD902                                         
091562*    WDD905:2                                                             
091563*    HÄMTA TIAVRDAT-INL FÖR AKTUELLT GODKÄNT AVROP                        
091564            PERFORM FCI-MOVE-WDD905-DATA-TO-RESP                          
091565        END-IF                                                            
091566     END-IF                                                               
091600                                                                          
091700*    WDD2                                                                 
091800     PERFORM DLI-GU-WDD201                                                
091900     IF SEGMENT-FINNS                                                     
092000         PERFORM FCD-MOVE-CURRENT-WDD2-DATA                               
092100     ELSE                                                                 
092101         PERFORM FCE-CLEAR-RESP-WDD2-DATA                                 
092102     END-IF                                                               
092103                                                                          
092104*    WDC1                                                                 
092200     PERFORM FCK-CLEAR-RESP-WDC1-DATA                                     
097500     MOVE W-IDARTNR                TO W-IDARTNR-WDC                       
097501     MOVE 'B'                      TO W-IDMARKBO-WDC                      
097502*    HÄMTA PRARTBTO FRÅN WDC101                                           
097503     PERFORM DLI-GU-WDC101                                                
097504     IF SEGMENT-FINNS                                                     
097505       MOVE WDC1-ART-PRARTBTO-MARK TO WS-PRARTBTO-DUMP                    
097506       PERFORM FCJ-MOVE-CURRENT-WDC1-DATA                                 
097507     END-IF                                                               
131601                                                                          
131602*    WDL2                                                                 
131603     PERFORM FCN-CLEAR-RESP-WDL2-DATA                                     
131604*    HÄMTA SENASTE TIAVIDAT                                               
131607*    SUMMERA KVAVIS HISTORISKT TILLS > 99                                 
131608     PERFORM FCL-MOVE-CURRENT-WDL2-DATA                                   
131609     .                                                                    
131610     EJECT                                                                
131611                                                                          
131612 FCA-MOVE-CURRENT-DB2-RECORD  SECTION.                                    
131613     SKIP2                                                                
131614     MOVE  ACCE-FLANNULL       TO RESP-FLANNULL   (RESP-IX)               
131615     MOVE  ACCE-IDARTNR        TO RESP-IDARTNR    (RESP-IX)               
131616     MOVE  ACCE-KDARTTYP       TO RESP-KDARTTYP   (RESP-IX)               
131617     MOVE  ACCE-TEARTUTFG      TO RESP-TEARTUTFG  (RESP-IX)               
131618     MOVE  ACCE-BEART          TO RESP-BEART      (RESP-IX)               
131619     MOVE  ACCE-TENOTE         TO RESP-TENOTE     (RESP-IX)               
131620     MOVE  ACCE-IDPRODGR       TO RESP-IDPRODGR   (RESP-IX)               
131621     MOVE  ACCE-IDUPPDSU       TO RESP-IDUPPDSU   (RESP-IX)               
131622     MOVE  ACCE-IDUPPDKU       TO RESP-IDUPPDKU   (RESP-IX)               
131623     MOVE  ACCE-TESTATUPP      TO RESP-TESTATUPP  (RESP-IX)               
131624     MOVE  ACCE-IDAOT          TO RESP-IDAOT      (RESP-IX)               
131625     MOVE  ACCE-TIAOINF        TO WS-TIAOINF-X                            
131626     MOVE  WS-TIAOINF-X(3:)    TO RESP-TIAOINF-AAVV(RESP-IX)              
131627     MOVE  ACCE-BEASSTYP       TO RESP-BEASSTYP   (RESP-IX)               
131628     MOVE  ACCE-KDFRPTYP       TO WS-KDFRPTYP                             
131629     IF ACCE-KDMDS = J                                                    
131630       MOVE Y                  TO RESP-KDMDS      (RESP-IX)               
131631     ELSE                                                                 
131632       MOVE ACCE-KDMDS         TO RESP-KDMDS      (RESP-IX)               
131634     END-IF                                                               
131636     MOVE  ACCE-IDLEVNR-GSDB   TO RESP-IDLEVNR    (RESP-IX)               
131637                                  W-IDLEVNR                               
131638     IF ACCE-DAPSWQP-1 = SPACE                                            
131639       MOVE N                  TO RESP-FLTPDWKPH1 (RESP-IX)               
131640     ELSE                                                                 
131641       MOVE Y                  TO RESP-FLTPDWKPH1 (RESP-IX)               
131642     END-IF                                                               
131643     MOVE   ACCE-DAPSWQA-1     TO RESP-DAPSWQA-1  (RESP-IX)               
131644     MOVE   ACCE-DAPSWPA-2     TO RESP-DAPSWPA-2  (RESP-IX)               
131645     MOVE   ACCE-DAPSWCA-3     TO RESP-DAPSWCA-3  (RESP-IX)               
131646     MOVE   ACCE-KDPSWQA-1     TO RESP-KDPSWQA-1  (RESP-IX)               
131647     MOVE   ACCE-KDPSWPA-2     TO RESP-KDPSWPA-2  (RESP-IX)               
131648     MOVE   ACCE-KDPSWCA-3     TO RESP-KDPSWCA-3  (RESP-IX)               
131649     MOVE   ACCE-KVYVOL-INT    TO RESP-KVYVOL-INT (RESP-IX)               
131650     MOVE   ACCE-KVYVOL-B3     TO RESP-KVYVOL-B3  (RESP-IX)               
131651     MOVE   ACCE-KVYVOL-B2     TO RESP-KVYVOL-B2  (RESP-IX)               
131652     MOVE   ACCE-KVYVOL-B1     TO RESP-KVYVOL-B1  (RESP-IX)               
131653     MOVE   ACCE-KVYVOL-ASS    TO RESP-KVYVOL-ASS (RESP-IX)               
131654     MOVE   ACCE-BEMAPP        TO RESP-BEMAPP     (RESP-IX)               
131655     MOVE   ACCE-KVFOTO        TO RESP-KVFOTO     (RESP-IX)               
131656     MOVE   ACCE-TIFOTO        TO RESP-TIFOTO     (RESP-IX)               
131657     MOVE   ACCE-TEVERKTYG     TO RESP-TEVERKTYG  (RESP-IX)               
131658     MOVE   ACCE-TESTATXT      TO RESP-TESTATXT   (RESP-IX)               
131659     MOVE   ACCE-TEMATXT       TO RESP-TEMATXT    (RESP-IX)               
131660     MOVE   ACCE-TEINKTXT      TO RESP-TEINKTXT   (RESP-IX)               
131661     MOVE   ACCE-TEANSTXT      TO RESP-TEANSTXT   (RESP-IX)               
131662     MOVE   ACCE-TEAUXTXT      TO RESP-TEAUXTXT   (RESP-IX)               
131663     MOVE   ACCE-IDARTNR-OFARG TO RESP-IDARTNR-OFARG (RESP-IX)            
131664     MOVE   ACCE-IDPSLAG       TO RESP-IDPSLAG    (RESP-IX)               
131665     MOVE   ACCE-BETYP         TO RESP-BETYP      (RESP-IX)               
131666     MOVE   ACCE-IDFKNGRP      TO RESP-IDFKNGRP   (RESP-IX)               
131667     MOVE   ACCE-IDKDPPOS      TO RESP-IDKDPPOS   (RESP-IX)               
131668     MOVE   ACCE-IDAOTUTG      TO RESP-IDAOTUTG   (RESP-IX)               
131669     MOVE   ACCE-BEANST-KU     TO RESP-BEANST-KU  (RESP-IX)               
131670     MOVE   ACCE-BEANST-SU     TO RESP-BEANST-SU  (RESP-IX)               
131671     MOVE   ACCE-IDPSS         TO RESP-IDPSS      (RESP-IX)               
131672     MOVE   ACCE-DAPSWQP-1     TO RESP-DAPSWQP-1  (RESP-IX)               
131673     MOVE   ACCE-KDPSWQP-1     TO RESP-KDPSWQP-1  (RESP-IX)               
131674     MOVE   ACCE-DAPSWPP-2     TO RESP-DAPSWPP-2  (RESP-IX)               
131675     MOVE   ACCE-KDPSWPP-2     TO RESP-KDPSWPP-2  (RESP-IX)               
131676     MOVE   ACCE-DAPSWCP-3     TO RESP-DAPSWCP-3  (RESP-IX)               
131677     MOVE   ACCE-KDPSWCP-3     TO RESP-KDPSWCP-3  (RESP-IX)               
131678     MOVE   ACCE-KDFARGST      TO RESP-KDFARGST   (RESP-IX)               
131679     MOVE   ACCE-IDPROJK       TO RESP-IDPROJK    (RESP-IX)               
131680     MOVE   ACCE-VKART-KDP     TO RESP-VKART-KDP  (RESP-IX)               
131681     MOVE   ACCE-KDANNULL      TO RESP-KDANNULL   (RESP-IX)               
131682     MOVE   ACCE-BEUPPDSU      TO RESP-BEUPPDSU   (RESP-IX)               
131683     .                                                                    
131684     EJECT                                                                
131685                                                                          
131686 FCB0-MOVE-CURRENT-WDK601-DATA  SECTION.                                  
131687     SKIP2                                                                
131688     MOVE Y                    TO RESP-FLRPULS    (RESP-IX)               
131689     MOVE WDK6-ART-IDLEVNR     TO RESP-IDLEVNR    (RESP-IX)               
131690                                  W-IDLEVNR                               
131691     .                                                                    
131692     EJECT                                                                
131693 FCB1-MOVE-CURRENT-WDK611-DATA  SECTION.                                  
131694     SKIP2                                                                
131695     MOVE WDK6-CLAG-IDINK      TO RESP-IDINK      (RESP-IX)               
131696     MOVE WDK6-CLAG-IDANSK     TO RESP-IDANSK     (RESP-IX)               
131697                                  WS-NUM-IDANSK                           
131698     MOVE WDK6-CLAG-KDEMBKOD-2 TO RESP-KDEMBKOD-2 (RESP-IX)               
131699     MOVE WDK6-CLAG-KVLS       TO RESP-KVLS       (RESP-IX)               
131700     .                                                                    
131701     EJECT                                                                
131702                                                                          
131703                                                                          
131704 FCB2-MOVE-CURRENT-WDK623-DATA  SECTION.                                  
131705     SKIP2                                                                
131706     IF WDK6-AVT-TIAVTAL > ZERO                                           
131707         MOVE 'AAMMDD'           TO DAT-KDDATFORM                         
131708         MOVE WDK6-AVT-TIAVTAL   TO DAT-I-TIDATUM                         
131709         CALL WDATKONV        USING DAT-KDDATFORM DAT-I-TIDATUM           
131710                                    DAT-O-TIDATUM DAT-KDSVAR              
131711         IF DAT-KDSVAR-OK                                                 
131712           MOVE DAT-TIAAVVD(1:4) TO RESP-TIAVTAL (RESP-IX)                
131713         ELSE                                                             
131714           MOVE 9999             TO RESP-TIAVTAL (RESP-IX)                
131715         END-IF                                                           
131716     ELSE                                                                 
131717       MOVE ZERO                 TO RESP-TIAVTAL (RESP-IX)                
131718     END-IF                                                               
131719     .                                                                    
131720     EJECT                                                                
131721                                                                          
131722                                                                          
131723 FCC-CLEAR-RESP-WDK6-DATA     SECTION.                                    
131724     SKIP2                                                                
131725*    --- CLEAR WDK613                                                     
131726     MOVE '-'    TO RESP-KDFRPTYP   (RESP-IX)                             
131727*    --- CLEAR WDK611                                                     
131728     MOVE '-'    TO RESP-IDINK      (RESP-IX)                             
131729     MOVE ZERO   TO RESP-IDANSK     (RESP-IX) WS-NUM-IDANSK               
131730     MOVE ZERO   TO RESP-KDEMBKOD-2 (RESP-IX)                             
131731*    --- CLEAR WDK623                                                     
131732     MOVE ZERO   TO RESP-TIAVTAL    (RESP-IX)                             
131733*    --- AND WDK601                                                       
131734     IF ACCE-IDLEVNR-GSDB = SPACE                                         
131735       MOVE '-'  TO RESP-IDLEVNR    (RESP-IX)                             
131736     END-IF                                                               
131737     MOVE N      TO RESP-FLRPULS    (RESP-IX)                             
131738     .                                                                    
131739     EJECT                                                                
131740 FCD-MOVE-CURRENT-WDD2-DATA  SECTION.                                     
131741     SKIP2                                                                
131742*    --- DON´T REPLACE WDK6 CORRESPONDING AVAILABLE FIELD DATA            
131743     IF RESP-IDLEVNR  (RESP-IX) = '-    '                                 
131744       MOVE WDD2-ART-IDLEVNR     TO RESP-IDLEVNR  (RESP-IX)               
131745     END-IF                                                               
131746     IF WS-NUM-IDANSK = ZERO                                              
131747       MOVE WDD2-ART-IDANSK      TO RESP-IDANSK   (RESP-IX)               
131748     END-IF                                                               
131749     IF RESP-IDINK (RESP-IX) = '-   '                                     
131750       MOVE WDD2-ART-IDINK       TO RESP-IDINK    (RESP-IX)               
131751     END-IF                                                               
131752                                                                          
131753*    --- MOVE ALL THE REMAINING FIELDS                                    
131754     IF RESP-IDLEVNR (RESP-IX) = '1002 '                                  
131755       MOVE  '1002 '             TO RESP-BETEXT-OTP (RESP-IX)             
131756     ELSE                                                                 
131757       IF WDD2-ART-KVLEVBEG > +0                                          
131758          MOVE Y             TO RESP-BETEXT-OTP (RESP-IX)                 
131759       ELSE                                                               
131760          MOVE N                 TO RESP-BETEXT-OTP (RESP-IX)             
131761       END-IF                                                             
131762     END-IF                                                               
131763     MOVE WDD2-ART-IDSTEKN       TO RESP-IDSTEKN    (RESP-IX)             
131764                                                                          
131781********>> FLUPB "ISD" ERSÄTTS AV                                         
131782* TPD STATUS                                                              
131783     IF WDD2-ART-KDTPD = 'D' OR 'S' OR 'P' OR 'R' OR 'A'                  
131784       MOVE WDD2-ART-KDTPD       TO RESP-KDTPD (RESP-IX)                  
131785     ELSE                                                                 
131786       MOVE '-'                  TO RESP-KDTPD (RESP-IX)                  
131787     END-IF                                                               
131788                                                                          
131813* TPD WEEK                                                                
131814     IF WDD2-ART-TITPD > ZERO                                             
131815         MOVE 'AAMMDD'           TO DAT-KDDATFORM                         
131816         MOVE WDD2-ART-TITPD     TO DAT-I-TIDATUM                         
131817         CALL WDATKONV        USING DAT-KDDATFORM DAT-I-TIDATUM           
131818                                    DAT-O-TIDATUM DAT-KDSVAR              
131819         IF DAT-KDSVAR-OK                                                 
131820           MOVE DAT-TIAAVVD(1:4) TO RESP-TITPD-AAVV (RESP-IX)             
131821         ELSE                                                             
131822           MOVE 9999             TO RESP-TITPD-AAVV (RESP-IX)             
131823         END-IF                                                           
131824     ELSE                                                                 
131825       MOVE ZERO                 TO RESP-TITPD-AAVV (RESP-IX)             
131826     END-IF                                                               
131848     .                                                                    
131849     EJECT                                                                
131850                                                                          
131851 FCE-CLEAR-RESP-WDD2-DATA    SECTION.                                     
131852     SKIP2                                                                
131853*    -- DONT CLEAR FIELDS CORRESPONDING WITH WDK6                         
131854     MOVE '-'                    TO RESP-BETEXT-OTP (RESP-IX)             
131855     MOVE SPACE                  TO RESP-IDSTEKN    (RESP-IX)             
131856     MOVE ZERO                   TO RESP-TITPD-AAVV (RESP-IX)             
131857     MOVE '-'                    TO RESP-KDTPD      (RESP-IX)             
131866     .                                                                    
131867     EJECT                                                                
131868                                                                          
131869 FCF-MOVE-WDD905-DATA-TO-RESP  SECTION.                                   
131870     SKIP2                                                                
131871     MOVE DAGENS-DATUM           TO DAT-I-TIDATUM                         
131872     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
131873     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
131874                         DAT-O-TIDATUM DAT-KDSVAR                         
131875     IF DAT-KDSVAR-OK                                                     
131876       MOVE DAT-TISEKEL          TO WS-DAAVROP-SEK                        
131877       MOVE DAT-TIAAVV-GRP       TO WS-DAAVROP-AAVV                       
131878     ELSE                                                                 
131879       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
131880     END-IF                                                               
131881                                                                          
131882     MOVE WS-DAAVROP             TO W-DAAVROP                             
131883     MOVE +1 TO W-KDAVROP                                                 
131884     PERFORM DLI-GNPF-WDD905                                              
131885     IF SEGMENT-FINNS                                                     
131886       MOVE PROPOSED             TO RESP-KDLEVPST   (RESP-IX)             
131887       MOVE WDD905-KDAVROP       TO WS-KDAVROP-DUMP                       
131888     ELSE                                                                 
131889       MOVE +2 TO W-KDAVROP                                               
131890       PERFORM DLI-GNPF-WDD905                                            
131891       IF SEGMENT-FINNS                                                   
131892         MOVE CONFIRMED          TO RESP-KDLEVPST   (RESP-IX)             
131893         MOVE WDD905-KDAVROP     TO WS-KDAVROP-DUMP                       
131894       ELSE                                                               
131895         MOVE N                  TO RESP-KDLEVPST   (RESP-IX)             
131896       END-IF                                                             
131897     END-IF                                                               
131898     .                                                                    
131899     EJECT                                                                
131900                                                                          
131901 FCG-CLEAR-RESP-WDD9-DATA    SECTION.                                     
131902     SKIP2                                                                
131903     MOVE '-'                TO RESP-KDLEVPST       (RESP-IX)             
131904     MOVE '0'                TO RESP-TILEVBSK       (RESP-IX)             
131905     .                                                                    
131906     EJECT                                                                
131907                                                                          
131908 FCH-MOVE-WDD924-DATA-TO-RESP  SECTION.                                   
131909     MOVE DAGENS-DATUM TO W-DALEVBSK-AVS                                  
131910     PERFORM DLI-GNP-WDD924                                               
131911                                                                          
131912     IF SEGMENT-FINNS                                                     
131913       MOVE WDD924-LEV-TILEVBSK-INL TO DAT-I-TIDATUM                      
131914                                       WS-TILEVBSK-DUMP                   
131915       MOVE 'AAMMDD'                TO DAT-KDDATFORM                      
131916       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
131917                           DAT-O-TIDATUM DAT-KDSVAR                       
131918       IF DAT-KDSVAR-OK                                                   
131919         MOVE DAT-TISEKEL           TO WS-TILEVBSK-SEK                    
131920         MOVE DAT-TIAAVVD           TO WS-TILEVBSK-AAVVD                  
131921       ELSE                                                               
131922         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
131923       END-IF                                                             
131924                                                                          
131925       MOVE WS-TILEVBSK             TO RESP-TILEVBSK (RESP-IX)            
131926     END-IF                                                               
131927     .                                                                    
131928     EJECT                                                                
131929                                                                          
131930 FCI-MOVE-WDD905-DATA-TO-RESP  SECTION.                                   
131931     SKIP2                                                                
131932     MOVE DAGENS-DATUM           TO DAT-I-TIDATUM                         
131933     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
131934     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
131935                         DAT-O-TIDATUM DAT-KDSVAR                         
131936     IF DAT-KDSVAR-OK                                                     
131937       MOVE DAT-TISEKEL          TO WS-DAAVROP-SEK                        
131938       MOVE DAT-TIAAVV-GRP       TO WS-DAAVROP-AAVV                       
131939     ELSE                                                                 
131940       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
131941     END-IF                                                               
131942                                                                          
131943     MOVE WS-DAAVROP             TO W-DAAVROP                             
131944     MOVE +2 TO W-KDAVROP                                                 
131945     PERFORM DLI-GNP-WDD905                                               
131946                                                                          
131947     IF SEGMENT-FINNS                                                     
131948       MOVE WDD905-TIAVRDAT-INL  TO DAT-I-TIDATUM                         
131949                                    WS-TIAVRDAT-DUMP                      
131950       MOVE 'AAMMDD'             TO DAT-KDDATFORM                         
131951       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
131952                           DAT-O-TIDATUM DAT-KDSVAR                       
131953       IF DAT-KDSVAR-OK                                                   
131954         MOVE DAT-TISEKEL        TO WS-TIAVRDAT-SEK                       
131955         MOVE DAT-TIAAVVD        TO WS-TIAVRDAT-AAVVD                     
131956       ELSE                                                               
131957         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
131958       END-IF                                                             
131959                                                                          
131960       MOVE WS-TIAVRDAT          TO RESP-TILEVBSK (RESP-IX)               
131961     END-IF                                                               
131962     .                                                                    
131970     EJECT                                                                
131971                                                                          
131972 FCJ-MOVE-CURRENT-WDC1-DATA  SECTION.                                     
131973     IF WDC1-ART-PRARTBTO-MARK > 0                                        
131974       MOVE 'Y'                  TO RESP-FLPULSPR   (RESP-IX)             
131975     ELSE                                                                 
131976       MOVE 'N'                  TO RESP-FLPULSPR   (RESP-IX)             
131977     END-IF                                                               
131978     .                                                                    
131979     EJECT                                                                
131980                                                                          
131981 FCK-CLEAR-RESP-WDC1-DATA    SECTION.                                     
131982     MOVE 'N'                    TO RESP-FLPULSPR   (RESP-IX)             
131983     MOVE ZERO                   TO WS-PRARTBTO-DUMP                      
131984     .                                                                    
131985     EJECT                                                                
131986                                                                          
131987 FCL-MOVE-CURRENT-WDL2-DATA  SECTION.                                     
131988     MOVE 'J'                         TO SW-FIRST-TIME                    
131989     MOVE ZERO                        TO WS-SULEVANT-ACK                  
131990                                                                          
131991     PERFORM DLI-GU-WDL201                                                
131992     IF SEGMENT-FINNS                                                     
132000       PERFORM DLI-GNP-WDL221                                             
132100       PERFORM UNTIL SEGMENT-SAKNAS                                       
132200         IF WDL221-MOT-KDRT     = 0 AND                                   
132300            WDL221-MOT-IDDC     = WC-CDC-SE AND                           
132310            WDL221-MOT-KVANTMOT > 0                                       
132400           IF FIRST-TIME                                                  
132500             MOVE 'N'                 TO SW-FIRST-TIME                    
132600             MOVE WDL221-MOT-TIAVIDAT TO DAT-I-TIDATUM                    
132601                                         WS-TIAVIDAT-DUMP                 
132602             MOVE 'AAMMDD'            TO DAT-KDDATFORM                    
132603             CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM              
132604                                 DAT-O-TIDATUM DAT-KDSVAR                 
132605             IF DAT-KDSVAR-OK                                             
132606               MOVE DAT-TISEKEL       TO WS-TIAVIDAT-SEK                  
132607               MOVE DAT-TIAAVVD       TO WS-TIAVIDAT-AAVVD                
132608             ELSE                                                         
132609               CALL ABEND USING RKOD-ABEND-MED-DUMP                       
132610             END-IF                                                       
132620                                                                          
132630             MOVE WS-TIAVIDAT         TO RESP-TIAVIDAT (RESP-IX)          
132640           END-IF                                                         
132650           COMPUTE WS-SULEVANT-ACK = WS-SULEVANT-ACK +                    
132660                                 WDL221-MOT-KVANTMOT                      
132670           END-COMPUTE                                                    
132680         END-IF                                                           
132690                                                                          
132700         IF WS-SULEVANT-ACK > 99                                          
132800           MOVE 'GE'                  TO STATUS-WS                        
132900         ELSE                                                             
133000           PERFORM DLI-GNP-WDL221                                         
133100         END-IF                                                           
133200       END-PERFORM                                                        
133300           MOVE WS-SULEVANT-ACK       TO WS-SULEVANT-DUMP                 
133400     END-IF                                                               
133500                                                                          
133600     IF WS-SULEVANT-ACK > 99                                              
133700       MOVE '>'                    TO WS-SULEVANT-OP                      
133800       MOVE 99                     TO WS-SULEVANT-NO                      
133900     ELSE                                                                 
134000       IF WS-SULEVANT-ACK > 0                                             
134100         MOVE ' '                  TO WS-SULEVANT-OP                      
134200         MOVE WS-SULEVANT-ACK      TO WS-SULEVANT-NO                      
134210       ELSE                                                               
134220         MOVE ' '                  TO WS-SULEVANT-OP                      
134230         MOVE ZERO                 TO WS-SULEVANT-NO                      
134300       END-IF                                                             
134400     END-IF                                                               
134500                                                                          
134501     MOVE WS-SULEVANT-OP           TO RESP-KDSIGN    (RESP-IX)            
134502     MOVE WS-SULEVANT-NO           TO RESP-SULEVANT  (RESP-IX)            
134503     .                                                                    
134504     EJECT                                                                
134505                                                                          
134506 FCN-CLEAR-RESP-WDL2-DATA    SECTION.                                     
134507     MOVE '0'                    TO RESP-TIAVIDAT     (RESP-IX)           
134508     MOVE '000'                  TO RESP-SULEVANT     (RESP-IX)           
134509     MOVE ' '                    TO RESP-KDSIGN       (RESP-IX)           
134510     .                                                                    
134511     EJECT                                                                
134512                                                                          
134513 FD-OPEN-TB1ACCE-CURSOR SECTION.                                          
134514     SKIP2                                                                
134515*    --- ÖPPNA FÖR RÄTT NYCKELKOMBINATION                                 
134516     EVALUATE TRUE                                                        
134517       WHEN CALL-TYPE-1  PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-1            
134518       WHEN CALL-TYPE-2  PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-2            
134519*****       CALL-TYPE-3 KÖRS UTAN CRS I DB2-SELECT-TB1ACCE-TAB            
134520       WHEN CALL-TYPE-4  PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-4            
134521       WHEN CALL-TYPE-5  PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-5            
134522       WHEN CALL-TYPE-6  PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-6            
134523       WHEN CALL-TYPE-7  PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-7            
134524       WHEN CALL-TYPE-8  PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-8            
134525       WHEN CALL-TYPE-9  PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-9            
134526       WHEN CALL-TYPE-10 PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-10           
134527       WHEN CALL-TYPE-11 PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-11           
134528       WHEN CALL-TYPE-12 PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-12           
134529       WHEN CALL-TYPE-13 PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-13           
134530       WHEN CALL-TYPE-14 PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-14           
134531       WHEN CALL-TYPE-15 PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-15           
134532       WHEN CALL-TYPE-16 PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-16           
134533       WHEN CALL-TYPE-17 PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-17           
134534       WHEN CALL-TYPE-18 PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-18           
134535       WHEN CALL-TYPE-19 PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-19           
134536       WHEN CALL-TYPE-20 PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-20           
134537       WHEN CALL-TYPE-21 PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-21           
134538       WHEN CALL-TYPE-22 PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-22           
134539       WHEN CALL-TYPE-23 PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-23           
134540       WHEN CALL-TYPE-24 PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-24           
134541       WHEN CALL-TYPE-25 PERFORM DB2-DCL-OPN-TB1ACCE-CRS-TYP-25           
134542       WHEN OTHER        CALL ABEND USING RKOD-ABEND-UTAN-DUMP            
134543     END-EVALUATE                                                         
134544     .                                                                    
134545     EJECT                                                                
134546                                                                          
134547 FE-FETCH-TB1ACCE-CRS SECTION.                                            
134548     SKIP2                                                                
134549*    --- HÄMTA FÖR RÄTT NYCKELKOMBINATION                                 
134550     EVALUATE TRUE                                                        
134551       WHEN CALL-TYPE-1  PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-1              
134552       WHEN CALL-TYPE-2  PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-2              
134553       WHEN CALL-TYPE-3                                                   
134554*                       KÖRS I DB2-SELECT-TB1ACCE-TAB                     
134555            CONTINUE                                                      
134556       WHEN CALL-TYPE-4  PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-4              
134557       WHEN CALL-TYPE-5  PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-5              
134558       WHEN CALL-TYPE-6  PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-6              
134559       WHEN CALL-TYPE-7  PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-7              
134560       WHEN CALL-TYPE-8  PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-8              
134561       WHEN CALL-TYPE-9  PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-9              
134562       WHEN CALL-TYPE-10 PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-10             
134563       WHEN CALL-TYPE-11 PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-11             
134564       WHEN CALL-TYPE-12 PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-12             
134565       WHEN CALL-TYPE-13 PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-13             
134566       WHEN CALL-TYPE-14 PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-14             
134567       WHEN CALL-TYPE-15 PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-15             
134568       WHEN CALL-TYPE-16 PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-16             
134569       WHEN CALL-TYPE-17 PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-17             
134570       WHEN CALL-TYPE-18 PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-18             
134571       WHEN CALL-TYPE-19 PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-19             
134572       WHEN CALL-TYPE-20 PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-20             
134573       WHEN CALL-TYPE-21 PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-21             
134574       WHEN CALL-TYPE-22 PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-22             
134575       WHEN CALL-TYPE-23 PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-23             
134576       WHEN CALL-TYPE-24 PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-24             
134577       WHEN CALL-TYPE-25 PERFORM DB2-FETCH-TB1ACCE-CRS-TYP-25             
134578       WHEN OTHER        CALL ABEND USING RKOD-ABEND-UTAN-DUMP            
134579     END-EVALUATE                                                         
134580     .                                                                    
134581     EJECT                                                                
134582                                                                          
134583 Z-FINIT SECTION.                                                         
134584     SKIP2                                                                
134585     EVALUATE TRUE                                                        
134586       WHEN CALL-TYPE-1   PERFORM DB2-CLOSE-TB1ACCE-CRS-1                 
134587       WHEN CALL-TYPE-2   PERFORM DB2-CLOSE-TB1ACCE-CRS-2                 
134588       WHEN CALL-TYPE-3                                                   
134589*                       KÖRS I DB2-SELECT-TB1ACCE-TAB                     
134590            CONTINUE                                                      
134591       WHEN CALL-TYPE-4   PERFORM DB2-CLOSE-TB1ACCE-CRS-4                 
134592       WHEN CALL-TYPE-5   PERFORM DB2-CLOSE-TB1ACCE-CRS-5                 
134593       WHEN CALL-TYPE-6   PERFORM DB2-CLOSE-TB1ACCE-CRS-6                 
134594       WHEN CALL-TYPE-7   PERFORM DB2-CLOSE-TB1ACCE-CRS-7                 
134595       WHEN CALL-TYPE-8   PERFORM DB2-CLOSE-TB1ACCE-CRS-8                 
134596       WHEN CALL-TYPE-9   PERFORM DB2-CLOSE-TB1ACCE-CRS-9                 
134597       WHEN CALL-TYPE-10  PERFORM DB2-CLOSE-TB1ACCE-CRS-10                
134598       WHEN CALL-TYPE-11  PERFORM DB2-CLOSE-TB1ACCE-CRS-11                
134599       WHEN CALL-TYPE-12  PERFORM DB2-CLOSE-TB1ACCE-CRS-12                
134600       WHEN CALL-TYPE-13  PERFORM DB2-CLOSE-TB1ACCE-CRS-13                
134601       WHEN CALL-TYPE-14  PERFORM DB2-CLOSE-TB1ACCE-CRS-14                
134602       WHEN CALL-TYPE-15  PERFORM DB2-CLOSE-TB1ACCE-CRS-15                
134603       WHEN CALL-TYPE-16  PERFORM DB2-CLOSE-TB1ACCE-CRS-16                
134604       WHEN CALL-TYPE-17  PERFORM DB2-CLOSE-TB1ACCE-CRS-17                
134605       WHEN CALL-TYPE-18  PERFORM DB2-CLOSE-TB1ACCE-CRS-18                
134606       WHEN CALL-TYPE-19  PERFORM DB2-CLOSE-TB1ACCE-CRS-19                
134607       WHEN CALL-TYPE-20  PERFORM DB2-CLOSE-TB1ACCE-CRS-20                
134608       WHEN CALL-TYPE-21  PERFORM DB2-CLOSE-TB1ACCE-CRS-21                
134609       WHEN CALL-TYPE-22  PERFORM DB2-CLOSE-TB1ACCE-CRS-22                
134610       WHEN CALL-TYPE-23  PERFORM DB2-CLOSE-TB1ACCE-CRS-23                
134611       WHEN CALL-TYPE-24  PERFORM DB2-CLOSE-TB1ACCE-CRS-24                
134612       WHEN CALL-TYPE-25  PERFORM DB2-CLOSE-TB1ACCE-CRS-25                
134613       WHEN OTHER         CALL ABEND USING RKOD-ABEND-UTAN-DUMP           
134614     END-EVALUATE                                                         
134615     CONTINUE                                                             
134616     .                                                                    
134617     EJECT                                                                
134618                                                                          
134619******************** DISPATCHER SECTIONS  ****************                
134620 S01-GET-REQUEST SECTION.                                                 
134621     SKIP2                                                                
134622     MOVE 'GETARG'               TO SUB-KDFUNC                            
134623     MOVE WS-ADDISPABS           TO SUB-ADDISPABS                         
134624     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
134625                                                                          
134626     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
134627                                                                          
134628     IF SUB-KDRC > 0                                                      
134629       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
134630       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
134631       DELIMITED BY SIZE INTO FELTEXT                                     
134632       IF SUB-KDRC = 10                                                   
134633         STRING FELTEXT                   DELIMITED BY '    '             
134634         '. ADDRESS NOT FOUND IN ATAB '   DELIMITED BY SIZE               
134635                                          INTO FELTEXT                    
134636       END-IF                                                             
134637       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
134638     END-IF                                                               
134639     .                                                                    
134640     EJECT                                                                
134641 S02-RETURN-RESPONSE SECTION.                                             
134642     SKIP2                                                                
134643     MOVE 'RETURN'                   TO SUB-KDFUNC                        
134644     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
134645                                                                          
134646     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
134647                                                                          
134648     IF SUB-KDRC > 0                                                      
134649       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
134650       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
134651       DELIMITED BY SIZE INTO FELTEXT                                     
134652       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
134653     END-IF                                                               
134660     .                                                                    
138800     EJECT                                                                
138900                                                                          
139000* --- IMS DLI SECTIONS  ---                                               
139100     SKIP3                                                                
139200                                                                          
139300 DLI-GU-WDD201 SECTION.                                                   
139400                                                                          
139500     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
139600          DELIMITED BY SIZE INTO SSA1                                     
139700     MOVE '  GE' TO GODK-STATUSKODER                                      
139800     CALL CBLTDLI USING GU WDD2-PCB DLI-IO-WDD201 SSA1                    
139900     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
140000     PERFORM DLI-STATUS-CONTROL                                           
140100     .                                                                    
140200     EJECT                                                                
140300 DLI-GU-WDD902 SECTION.                                                   
140400                                                                          
140500     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
140600          DELIMITED BY SIZE INTO SSA1                                     
140700     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
140800          DELIMITED BY SIZE INTO SSA2                                     
140900     MOVE '  GE' TO GODK-STATUSKODER                                      
141000     CALL CBLTDLI USING GU  WDD9-PCB DLI-IO-WDD902 SSA1 SSA2              
141100     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
141200     PERFORM DLI-STATUS-CONTROL                                           
141300     .                                                                    
141400     SKIP2                                                                
141500 DLI-GNPF-WDD905 SECTION.                                                 
141600                                                                          
141700     STRING 'WDD905  *F(KDAVROP  =' W-KDAVROP-X ')'                       
141800          DELIMITED BY SIZE INTO SSA1                                     
141900     MOVE '  GE' TO GODK-STATUSKODER                                      
142000     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
142100     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
142200     PERFORM DLI-STATUS-CONTROL                                           
142300     .                                                                    
142301     EJECT                                                                
142302 DLI-GNP-WDD905 SECTION.                                                  
142303                                                                          
142304     STRING 'WDD905  (WDD905KY>=' W-WDD905KY-X     '&'                    
142305                     'KDAVROP  =' W-KDAVROP-X ')'                         
142306          DELIMITED BY SIZE INTO SSA1                                     
142307     MOVE '  GE' TO GODK-STATUSKODER                                      
142308     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
142309     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
142310     PERFORM DLI-STATUS-CONTROL                                           
142320     .                                                                    
142400     SKIP2                                                                
142401 DLI-GNP-WDD924 SECTION.                                                  
142402                                                                          
142403     STRING 'WDD924  (DALEVBSK>=' W-DALEVBSK-AVS-X ')'                    
142404          DELIMITED BY SIZE INTO SSA1                                     
142405     MOVE '  GE' TO GODK-STATUSKODER                                      
142406     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD924 SSA1                   
142407     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
142408     PERFORM DLI-STATUS-CONTROL                                           
142409     .                                                                    
142410     EJECT                                                                
142420 DLI-GU-WDC101 SECTION.                                                   
142421                                                                          
142422     STRING 'WDC101  (WDC101KY =' W-WDC101KY-X ')'                        
142423          DELIMITED BY SIZE INTO SSA1                                     
142424     MOVE '  GE' TO GODK-STATUSKODER                                      
142425     CALL CBLTDLI USING GU WDC1-PCB DLI-IO-WDC101 SSA1                    
142426     MOVE WDC1-STATUS-CODE TO STATUS-WS                                   
142427     PERFORM DLI-STATUS-CONTROL                                           
142428     .                                                                    
142429     EJECT                                                                
142430 DLI-GU-WDL201 SECTION.                                                   
142440                                                                          
142450     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
142460          DELIMITED BY SIZE INTO SSA1                                     
142470     MOVE '  GE'           TO GODK-STATUSKODER                            
142480     CALL CBLTDLI USING GU WDL2-PCB DLI-IO-WDL201 SSA1                    
142490     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
142500     PERFORM DLI-STATUS-CONTROL                                           
161500     .                                                                    
161600     SKIP2                                                                
161700 DLI-GNP-WDL221 SECTION.                                                  
161800                                                                          
161900     MOVE   'WDL211' TO SSA1                                              
162000     STRING 'WDL221  (IDPTYP   =' W-IDPTYP-X ')'                          
162100             DELIMITED BY SIZE INTO SSA2                                  
162200     MOVE '  GE' TO GODK-STATUSKODER                                      
162300     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL221 SSA1 SSA2              
162400     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
162410     PERFORM DLI-STATUS-CONTROL                                           
162500     .                                                                    
162600     EJECT                                                                
162601 DLI-GU-WDK601 SECTION.                                                   
162602                                                                          
162603     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
162604          DELIMITED BY SIZE INTO SSA1                                     
162605     MOVE '  GE' TO GODK-STATUSKODER                                      
162606     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
162607     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
162608     PERFORM DLI-STATUS-CONTROL                                           
162609     .                                                                    
162610     EJECT                                                                
162611 DLI-GNP-WDK611 SECTION.                                                  
162612                                                                          
162613     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
162614          DELIMITED BY SIZE INTO SSA1                                     
162615     MOVE '  GE' TO GODK-STATUSKODER                                      
162616     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
162617     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
162618     PERFORM DLI-STATUS-CONTROL                                           
162619     .                                                                    
162620     EJECT                                                                
162621 DLI-GNP-WDK613 SECTION.                                                  
162622                                                                          
162623     MOVE 'WDK613  (KDEMBAL  =Q1 )'  TO SSA1                              
162624     MOVE '  GEGB' TO GODK-STATUSKODER                                    
162625     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK613 SSA1                   
162626     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
162627     PERFORM DLI-STATUS-CONTROL                                           
162628     .                                                                    
162629     EJECT                                                                
162630 DLI-GNP-WDK623 SECTION.                                                  
162631                                                                          
162632     MOVE 'WDK623   '  TO SSA1                                            
162633     MOVE '  GEGB' TO GODK-STATUSKODER                                    
162634     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK623 SSA1                   
162635     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
162636     PERFORM DLI-STATUS-CONTROL                                           
162637     .                                                                    
162638     EJECT                                                                
162639 DLI-STATUS-CONTROL SECTION.                                              
162640                                                                          
162641     SET STATUS-IX TO 1                                                   
162642     SEARCH GODK-STATUS                                                   
162643       AT END                                                             
162644         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
162645         DELIMITED BY SIZE INTO FELTEXT                                   
162646         CALL FELLOG                                                      
162647       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
162648         CONTINUE                                                         
162649     END-SEARCH                                                           
162650     .                                                                    
162651     EJECT                                                                
162652                                                                          
162653* --- DB2 SECTIONS  ---                                                   
162654     SKIP3                                                                
162655                                                                          
162656                                                                          
162657 DB2-GET-TOTAL-LINES-TYPE-1 SECTION.                                      
162658     SKIP2                                                                
162659     MOVE 'DB2-GET-TOTAL-LINES-TYPE-1   ' TO DB2-SEKTION                  
162660     EXEC SQL                                                             
162661         SELECT COUNT(*)                                                  
162662         INTO :WS-KVRADER-D-TOTAL                                         
162663         FROM TB1ACCE                                                     
162664         WHERE  IDARTNR  = :W-IDARTNR                                     
162665     END-EXEC                                                             
162666     MOVE 000100  TO GODK-SQLCODEKODER                                    
162667     MOVE SQLCODE TO SQLCODE-WS                                           
162668     PERFORM DB2-STATUS-CONTROL                                           
162669     .                                                                    
162670     EJECT                                                                
162671 DB2-GET-TOTAL-LINES-TYPE-2 SECTION.                                      
162672     SKIP2                                                                
162673     MOVE 'DB2-GET-TOTAL-LINES-TYPE-2   ' TO DB2-SEKTION                  
162674     EXEC SQL                                                             
162675         SELECT COUNT(*)                                                  
162676         INTO :WS-KVRADER-D-TOTAL                                         
162677         FROM TB1ACCE                                                     
162678         WHERE  IDUPPDSU = :W-IDUPPDSU                                    
162679     END-EXEC                                                             
162680     MOVE 000100  TO GODK-SQLCODEKODER                                    
162681     MOVE SQLCODE TO SQLCODE-WS                                           
162682     PERFORM DB2-STATUS-CONTROL                                           
162683     .                                                                    
162684     EJECT                                                                
162685 DB2-GET-TOTAL-LINES-TYPE-4 SECTION.                                      
162686     SKIP2                                                                
162687     MOVE 'DB2-GET-TOTAL-LINES-TYPE-4   ' TO DB2-SEKTION                  
162688     EXEC SQL                                                             
162689         SELECT COUNT(*)                                                  
162690         INTO :WS-KVRADER-D-TOTAL                                         
162691         FROM TB1ACCE                                                     
162692         WHERE  IDUPPDKU = :W-IDUPPDKU                                    
162693     END-EXEC                                                             
162694     MOVE 000100  TO GODK-SQLCODEKODER                                    
162695     MOVE SQLCODE TO SQLCODE-WS                                           
162696     PERFORM DB2-STATUS-CONTROL                                           
162697     .                                                                    
162698     EJECT                                                                
162699 DB2-GET-TOTAL-LINES-TYPE-5 SECTION.                                      
162700     SKIP2                                                                
162701     MOVE 'DB2-GET-TOTAL-LINES-TYPE-5   ' TO DB2-SEKTION                  
162702     EXEC SQL                                                             
162703         SELECT COUNT(*)                                                  
162704         INTO :WS-KVRADER-D-TOTAL                                         
162705         FROM TB1ACCE                                                     
162706         WHERE ( IDARTNR  = :W-IDARTNR                                    
162707           AND   IDUPPDKU = :W-IDUPPDKU   )                               
162708     END-EXEC                                                             
162709     MOVE 000100  TO GODK-SQLCODEKODER                                    
162710     MOVE SQLCODE TO SQLCODE-WS                                           
162711     PERFORM DB2-STATUS-CONTROL                                           
162712     .                                                                    
162713     EJECT                                                                
162714 DB2-GET-TOTAL-LINES-TYPE-6 SECTION.                                      
162715     SKIP2                                                                
162716     MOVE 'DB2-GET-TOTAL-LINES-TYPE-6   ' TO DB2-SEKTION                  
162717     EXEC SQL                                                             
162718         SELECT COUNT(*)                                                  
162719         INTO :WS-KVRADER-D-TOTAL                                         
162720         FROM TB1ACCE                                                     
162721         WHERE ( IDUPPDSU = :W-IDUPPDSU                                   
162722           AND   IDUPPDKU = :W-IDUPPDKU   )                               
162723     END-EXEC                                                             
162724     MOVE 000100  TO GODK-SQLCODEKODER                                    
162725     MOVE SQLCODE TO SQLCODE-WS                                           
162726     PERFORM DB2-STATUS-CONTROL                                           
162727     .                                                                    
162728     EJECT                                                                
162729 DB2-GET-TOTAL-LINES-TYPE-7 SECTION.                                      
162730     SKIP2                                                                
162731     MOVE 'DB2-GET-TOTAL-LINES-TYPE-7   ' TO DB2-SEKTION                  
162732     EXEC SQL                                                             
162733         SELECT COUNT(*)                                                  
162734         INTO :WS-KVRADER-D-TOTAL                                         
162735         FROM TB1ACCE                                                     
162736         WHERE ( IDUPPDSU = :W-IDUPPDSU                                   
162737           AND   IDUPPDKU = :W-IDUPPDKU                                   
162738           AND   IDARTNR  = :W-IDARTNR )                                  
162739     END-EXEC                                                             
162740     MOVE 000100  TO GODK-SQLCODEKODER                                    
162741     MOVE SQLCODE TO SQLCODE-WS                                           
162742     PERFORM DB2-STATUS-CONTROL                                           
162743     .                                                                    
162744     EJECT                                                                
162745 DB2-GET-TOTAL-LINES-TYPE-8 SECTION.                                      
162746     SKIP2                                                                
162747     MOVE 'DB2-GET-TOTAL-LINES-TYPE-8   ' TO DB2-SEKTION                  
162748     EXEC SQL                                                             
162749         SELECT COUNT(*)                                                  
162750         INTO :WS-KVRADER-D-TOTAL                                         
162751         FROM TB1ACCE                                                     
162752         WHERE ( TIAOINF >= :W-TIAOINF-FOM                                
162753           AND   TIAOINF <= :W-TIAOINF-TOM  )                             
162754     END-EXEC                                                             
162755     MOVE 000100  TO GODK-SQLCODEKODER                                    
162756     MOVE SQLCODE TO SQLCODE-WS                                           
162757     PERFORM DB2-STATUS-CONTROL                                           
162758     .                                                                    
162759     EJECT                                                                
162760 DB2-GET-TOTAL-LINES-TYPE-9  SECTION.                                     
162761     SKIP2                                                                
162762     MOVE 'DB2-GET-TOTAL-LINES-TYPE-9  ' TO DB2-SEKTION                   
162763     EXEC SQL                                                             
162764         SELECT COUNT(*)                                                  
162765         INTO :WS-KVRADER-D-TOTAL                                         
162766         FROM TB1ACCE                                                     
162767         WHERE (IDARTNR   = :W-IDARTNR                                    
162768           AND   TIAOINF >= :W-TIAOINF-FOM                                
162769           AND   TIAOINF <= :W-TIAOINF-TOM )                              
162770     END-EXEC                                                             
162771     MOVE 000100  TO GODK-SQLCODEKODER                                    
162772     MOVE SQLCODE TO SQLCODE-WS                                           
162773     PERFORM DB2-STATUS-CONTROL                                           
162774     .                                                                    
162775     EJECT                                                                
162776 DB2-GET-TOTAL-LINES-TYPE-10 SECTION.                                     
162777     SKIP2                                                                
162778     MOVE 'DB2-GET-TOTAL-LINES-TYPE-10 ' TO DB2-SEKTION                   
162779     EXEC SQL                                                             
162780         SELECT COUNT(*)                                                  
162781         INTO :WS-KVRADER-D-TOTAL                                         
162782         FROM TB1ACCE                                                     
162783         WHERE ( TIAOINF >= :W-TIAOINF-FOM                                
162784           AND   TIAOINF <= :W-TIAOINF-TOM                                
162785           AND  IDUPPDSU  = :W-IDUPPDSU    )                              
162800     END-EXEC                                                             
162900     MOVE 000100  TO GODK-SQLCODEKODER                                    
163000     MOVE SQLCODE TO SQLCODE-WS                                           
163100     PERFORM DB2-STATUS-CONTROL                                           
163200     .                                                                    
163300     EJECT                                                                
163310 DB2-GET-TOTAL-LINES-TYPE-11 SECTION.                                     
163320     SKIP2                                                                
163330     MOVE 'DB2-GET-TOTAL-LINES-TYPE-11 ' TO DB2-SEKTION                   
163340     EXEC SQL                                                             
163350         SELECT COUNT(*)                                                  
163360         INTO :WS-KVRADER-D-TOTAL                                         
163370         FROM TB1ACCE                                                     
163380         WHERE ( TIAOINF >= :W-TIAOINF-FOM                                
163390           AND   TIAOINF <= :W-TIAOINF-TOM                                
163391           AND  IDUPPDSU  = :W-IDUPPDSU                                   
163392           AND  IDARTNR   = :W-IDARTNR     )                              
163393     END-EXEC                                                             
163394     MOVE 000100  TO GODK-SQLCODEKODER                                    
163395     MOVE SQLCODE TO SQLCODE-WS                                           
163396     PERFORM DB2-STATUS-CONTROL                                           
163397     .                                                                    
163398     EJECT                                                                
163400 DB2-GET-TOTAL-LINES-TYPE-12 SECTION.                                     
163500     SKIP2                                                                
163600     MOVE 'DB2-GET-TOTAL-LINES-TYPE-12 ' TO DB2-SEKTION                   
163800     EXEC SQL                                                             
163900         SELECT COUNT(*)                                                  
164000         INTO :WS-KVRADER-D-TOTAL                                         
164100         FROM TB1ACCE                                                     
164200         WHERE ( TIAOINF >= :W-TIAOINF-FOM                                
164300           AND   TIAOINF <= :W-TIAOINF-TOM                                
164400           AND  IDUPPDKU  = :W-IDUPPDKU    )                              
164500     END-EXEC                                                             
164600     MOVE 000100  TO GODK-SQLCODEKODER                                    
164700     MOVE SQLCODE TO SQLCODE-WS                                           
164800     PERFORM DB2-STATUS-CONTROL                                           
164900     .                                                                    
165000     EJECT                                                                
165100                                                                          
165110 DB2-GET-TOTAL-LINES-TYPE-13 SECTION.                                     
165120     SKIP2                                                                
165130     MOVE 'DB2-GET-TOTAL-LINES-TYPE-13 ' TO DB2-SEKTION                   
165150     EXEC SQL                                                             
165160         SELECT COUNT(*)                                                  
165170         INTO :WS-KVRADER-D-TOTAL                                         
165180         FROM TB1ACCE                                                     
165190         WHERE ( TIAOINF >= :W-TIAOINF-FOM                                
165191           AND   TIAOINF <= :W-TIAOINF-TOM                                
165192           AND  IDUPPDKU  = :W-IDUPPDKU                                   
165193           AND  IDARTNR   = :W-IDARTNR     )                              
165194     END-EXEC                                                             
165195     MOVE 000100  TO GODK-SQLCODEKODER                                    
165196     MOVE SQLCODE TO SQLCODE-WS                                           
165197     PERFORM DB2-STATUS-CONTROL                                           
165198     .                                                                    
165199     EJECT                                                                
165200                                                                          
165201 DB2-GET-TOTAL-LINES-TYPE-14 SECTION.                                     
165202     SKIP2                                                                
165203     MOVE 'DB2-GET-TOTAL-LINES-TYPE-14 ' TO DB2-SEKTION                   
165205     EXEC SQL                                                             
165206         SELECT COUNT(*)                                                  
165207         INTO :WS-KVRADER-D-TOTAL                                         
165208         FROM TB1ACCE                                                     
165209         WHERE ( TIAOINF >= :W-TIAOINF-FOM                                
165210           AND   TIAOINF <= :W-TIAOINF-TOM                                
165211           AND  IDUPPDKU  = :W-IDUPPDKU                                   
165212           AND  IDUPPDSU  = :W-IDUPPDSU    )                              
165213     END-EXEC                                                             
165214     MOVE 000100  TO GODK-SQLCODEKODER                                    
165215     MOVE SQLCODE TO SQLCODE-WS                                           
165216     PERFORM DB2-STATUS-CONTROL                                           
165217     .                                                                    
165218     EJECT                                                                
165219                                                                          
165220 DB2-GET-TOTAL-LINES-TYPE-15 SECTION.                                     
165221     SKIP2                                                                
165222     MOVE 'DB2-GET-TOTAL-LINES-TYPE-15 ' TO DB2-SEKTION                   
165223     EXEC SQL                                                             
165224         SELECT COUNT(*)                                                  
165225         INTO :WS-KVRADER-D-TOTAL                                         
165226         FROM TB1ACCE                                                     
165227         WHERE ( TIAOINF >= :W-TIAOINF-FOM                                
165228           AND   TIAOINF <= :W-TIAOINF-TOM                                
165229           AND  IDUPPDKU  = :W-IDUPPDKU                                   
165230           AND  IDUPPDSU  = :W-IDUPPDSU                                   
165231           AND  IDARTNR   = :W-IDARTNR     )                              
165232     END-EXEC                                                             
165233     MOVE 000100  TO GODK-SQLCODEKODER                                    
165234     MOVE SQLCODE TO SQLCODE-WS                                           
165235     PERFORM DB2-STATUS-CONTROL                                           
165236     .                                                                    
165237     EJECT                                                                
165238                                                                          
165239 DB2-GET-TOTAL-LINES-TYPE-16 SECTION.                                     
165240     SKIP2                                                                
165241     MOVE 'DB2-GET-TOTAL-LINES-TYPE-16 ' TO DB2-SEKTION                   
165242     EXEC SQL                                                             
165243         SELECT COUNT(*)                                                  
165244         INTO :WS-KVRADER-D-TOTAL                                         
165245         FROM TB1ACCE                                                     
165247         WHERE  UPPER(BEUPPDSU) LIKE UPPER(:W-BEUPPDSU)                   
165251     END-EXEC                                                             
165252     MOVE 000100  TO GODK-SQLCODEKODER                                    
165253     MOVE SQLCODE TO SQLCODE-WS                                           
165254     PERFORM DB2-STATUS-CONTROL                                           
165255     .                                                                    
165256     EJECT                                                                
165257                                                                          
165258 DB2-GET-TOTAL-LINES-TYPE-17 SECTION.                                     
165259     SKIP2                                                                
165260     MOVE 'DB2-GET-TOTAL-LINES-TYPE-17 ' TO DB2-SEKTION                   
165261     EXEC SQL                                                             
165262         SELECT COUNT(*)                                                  
165263         INTO :WS-KVRADER-D-TOTAL                                         
165264         FROM TB1ACCE                                                     
165269         WHERE ( IDARTNR   = :W-IDARTNR                                   
165270           AND   UPPER(BEUPPDSU) LIKE UPPER(:W-BEUPPDSU))                 
165271     END-EXEC                                                             
165272     MOVE 000100  TO GODK-SQLCODEKODER                                    
165273     MOVE SQLCODE TO SQLCODE-WS                                           
165274     PERFORM DB2-STATUS-CONTROL                                           
165275     .                                                                    
165276     EJECT                                                                
165277                                                                          
165294 DB2-GET-TOTAL-LINES-TYPE-18 SECTION.                                     
165295     SKIP2                                                                
165296     MOVE 'DB2-GET-TOTAL-LINES-TYPE-18 ' TO DB2-SEKTION                   
165297     EXEC SQL                                                             
165298         SELECT COUNT(*)                                                  
165299         INTO :WS-KVRADER-D-TOTAL                                         
165300         FROM TB1ACCE                                                     
165301         WHERE ( IDUPPDSU  = :W-IDUPPDSU                                  
165302           AND   IDARTNR   = :W-IDARTNR                                   
165303           AND   UPPER(BEUPPDSU) LIKE UPPER(:W-BEUPPDSU))                 
165304     END-EXEC                                                             
165305     MOVE 000100  TO GODK-SQLCODEKODER                                    
165306     MOVE SQLCODE TO SQLCODE-WS                                           
165307     PERFORM DB2-STATUS-CONTROL                                           
165308     .                                                                    
165309     EJECT                                                                
165310                                                                          
165311 DB2-GET-TOTAL-LINES-TYPE-19 SECTION.                                     
165312     SKIP2                                                                
165313     MOVE 'DB2-GET-TOTAL-LINES-TYPE-19 ' TO DB2-SEKTION                   
165314     EXEC SQL                                                             
165315         SELECT COUNT(*)                                                  
165316         INTO :WS-KVRADER-D-TOTAL                                         
165317         FROM TB1ACCE                                                     
165318         WHERE ( IDUPPDKU  = :W-IDUPPDKU                                  
165319           AND   IDUPPDSU  = :W-IDUPPDSU                                  
165320           AND   IDARTNR   = :W-IDARTNR                                   
165321           AND   UPPER(BEUPPDSU) LIKE UPPER(:W-BEUPPDSU))                 
165322     END-EXEC                                                             
165323     MOVE 000100  TO GODK-SQLCODEKODER                                    
165324     MOVE SQLCODE TO SQLCODE-WS                                           
165325     PERFORM DB2-STATUS-CONTROL                                           
165326     .                                                                    
165327     EJECT                                                                
165328                                                                          
165329 DB2-GET-TOTAL-LINES-TYPE-20 SECTION.                                     
165330     SKIP2                                                                
165331     MOVE 'DB2-GET-TOTAL-LINES-TYPE-20 ' TO DB2-SEKTION                   
165332     EXEC SQL                                                             
165333         SELECT COUNT(*)                                                  
165334         INTO :WS-KVRADER-D-TOTAL                                         
165335         FROM TB1ACCE                                                     
165336         WHERE ( TIAOINF >= :W-TIAOINF-FOM                                
165337           AND   TIAOINF <= :W-TIAOINF-TOM                                
165339           AND   IDUPPDKU  = :W-IDUPPDKU                                  
165340           AND   IDUPPDSU  = :W-IDUPPDSU                                  
165341           AND   IDARTNR   = :W-IDARTNR                                   
165342           AND   UPPER(BEUPPDSU) LIKE UPPER(:W-BEUPPDSU))                 
165343     END-EXEC                                                             
165344     MOVE 000100  TO GODK-SQLCODEKODER                                    
165345     MOVE SQLCODE TO SQLCODE-WS                                           
165346     PERFORM DB2-STATUS-CONTROL                                           
165347     .                                                                    
165348     EJECT                                                                
165349                                                                          
165350 DB2-GET-TOTAL-LINES-TYPE-21 SECTION.                                     
165351     SKIP2                                                                
165352     MOVE 'DB2-GET-TOTAL-LINES-TYPE-21 ' TO DB2-SEKTION                   
165353     EXEC SQL                                                             
165354         SELECT COUNT(*)                                                  
165355         INTO :WS-KVRADER-D-TOTAL                                         
165356         FROM TB1ACCE                                                     
165357         WHERE ( TIAOINF >= :W-TIAOINF-FOM                                
165358           AND   TIAOINF <= :W-TIAOINF-TOM                                
165362           AND   UPPER(BEUPPDSU) LIKE UPPER(:W-BEUPPDSU))                 
165363     END-EXEC                                                             
165364     MOVE 000100  TO GODK-SQLCODEKODER                                    
165365     MOVE SQLCODE TO SQLCODE-WS                                           
165366     PERFORM DB2-STATUS-CONTROL                                           
165367     .                                                                    
165368     EJECT                                                                
165369                                                                          
165370 DB2-GET-TOTAL-LINES-TYPE-22 SECTION.                                     
165371     SKIP2                                                                
165372     MOVE 'DB2-GET-TOTAL-LINES-TYPE-22 ' TO DB2-SEKTION                   
165373     EXEC SQL                                                             
165374         SELECT COUNT(*)                                                  
165375         INTO :WS-KVRADER-D-TOTAL                                         
165376         FROM TB1ACCE                                                     
165377         WHERE ( TIAOINF >= :W-TIAOINF-FOM                                
165378           AND   TIAOINF <= :W-TIAOINF-TOM                                
165380           AND   IDUPPDSU  = :W-IDUPPDSU                                  
165382           AND   UPPER(BEUPPDSU) LIKE UPPER(:W-BEUPPDSU))                 
165383     END-EXEC                                                             
165384     MOVE 000100  TO GODK-SQLCODEKODER                                    
165385     MOVE SQLCODE TO SQLCODE-WS                                           
165386     PERFORM DB2-STATUS-CONTROL                                           
165387     .                                                                    
165388     EJECT                                                                
165389                                                                          
165390 DB2-GET-TOTAL-LINES-TYPE-23 SECTION.                                     
165391     SKIP2                                                                
165392     MOVE 'DB2-GET-TOTAL-LINES-TYPE-23 ' TO DB2-SEKTION                   
165393     EXEC SQL                                                             
165394         SELECT COUNT(*)                                                  
165395         INTO :WS-KVRADER-D-TOTAL                                         
165396         FROM TB1ACCE                                                     
165397         WHERE ( TIAOINF >= :W-TIAOINF-FOM                                
165398           AND   TIAOINF <= :W-TIAOINF-TOM                                
165401           AND   IDARTNR   = :W-IDARTNR                                   
165402           AND   UPPER(BEUPPDSU) LIKE UPPER(:W-BEUPPDSU))                 
165403     END-EXEC                                                             
165404     MOVE 000100  TO GODK-SQLCODEKODER                                    
165405     MOVE SQLCODE TO SQLCODE-WS                                           
165406     PERFORM DB2-STATUS-CONTROL                                           
165407     .                                                                    
165408     EJECT                                                                
165409                                                                          
165410 DB2-GET-TOTAL-LINES-TYPE-24 SECTION.                                     
165411     SKIP2                                                                
165412     MOVE 'DB2-GET-TOTAL-LINES-TYPE-24 ' TO DB2-SEKTION                   
165413     EXEC SQL                                                             
165414         SELECT COUNT(*)                                                  
165415         INTO :WS-KVRADER-D-TOTAL                                         
165416         FROM TB1ACCE                                                     
165417         WHERE ( TIAOINF >= :W-TIAOINF-FOM                                
165418           AND   TIAOINF <= :W-TIAOINF-TOM                                
165420           AND   IDUPPDSU  = :W-IDUPPDSU                                  
165421           AND   IDARTNR   = :W-IDARTNR                                   
165422           AND   UPPER(BEUPPDSU) LIKE UPPER(:W-BEUPPDSU))                 
165423     END-EXEC                                                             
165424     MOVE 000100  TO GODK-SQLCODEKODER                                    
165425     MOVE SQLCODE TO SQLCODE-WS                                           
165426     PERFORM DB2-STATUS-CONTROL                                           
165427     .                                                                    
165428     EJECT                                                                
165429                                                                          
165430 DB2-GET-TOTAL-LINES-TYPE-25 SECTION.                                     
165431     SKIP2                                                                
165432     MOVE 'DB2-GET-TOTAL-LINES-TYPE-25 ' TO DB2-SEKTION                   
165433     EXEC SQL                                                             
165434         SELECT COUNT(*)                                                  
165435         INTO :WS-KVRADER-D-TOTAL                                         
165436         FROM TB1ACCE                                                     
165439         WHERE ( IDUPPDSU  = :W-IDUPPDSU                                  
165441           AND   UPPER(BEUPPDSU) LIKE UPPER(:W-BEUPPDSU))                 
165442     END-EXEC                                                             
165443     MOVE 000100  TO GODK-SQLCODEKODER                                    
165444     MOVE SQLCODE TO SQLCODE-WS                                           
165445     PERFORM DB2-STATUS-CONTROL                                           
165446     .                                                                    
165447     EJECT                                                                
165448                                                                          
165449 DB2-DCL-OPN-TB1ACCE-CRS-TYP-1  SECTION.                                  
165450     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-1' TO DB2-SEKTION                  
165460     EXEC SQL                                                             
165500          DECLARE TB1ACCE-CRS-1 CURSOR FOR SELECT                         
165610                                                                          
165700          IDARTNR                                                         
165800         ,BEART                                                           
165900         ,IDPRODGR                                                        
166000         ,IDUPPDSU                                                        
166100         ,IDUPPDKU                                                        
166200         ,IDAOT                                                           
166300         ,TIAOINF                                                         
166400         ,BEASSTYP                                                        
166410         ,KDARTTYP                                                        
166430         ,KDMDS                                                           
166440         ,KDFRPTYP                                                        
166450         ,TEARTUTFG                                                       
166460         ,TESTATUPP                                                       
166470         ,IDLEVNR_GSDB                                                    
166480         ,KDTPD_PH1                                                       
166490         ,DATPDPH1                                                        
166491         ,DAPSWQP_1                                                       
166492         ,KDPSWQP_1                                                       
166493         ,DAPSWQA_1                                                       
166494         ,KDPSWQA_1                                                       
166495         ,DAPSWPP_2                                                       
166496         ,KDPSWPP_2                                                       
166497         ,DAPSWPA_2                                                       
166498         ,KDPSWPA_2                                                       
166499         ,DAPSWCP_3                                                       
166500         ,KDPSWCP_3                                                       
166501         ,DAPSWCA_3                                                       
166502         ,KDPSWCA_3                                                       
166503         ,KVYVOL_B3                                                       
166504         ,KVYVOL_B2                                                       
166506         ,KVYVOL_INT                                                      
166507         ,KVYVOL_B1                                                       
166508         ,KVYVOL_ASS                                                      
166512         ,BEMAPP                                                          
166513         ,KVFOTO                                                          
166514         ,TIFOTO                                                          
166515         ,TENOTE                                                          
166516         ,FLANNULL                                                        
166517         ,TEVERKTYG                                                       
166518         ,TESTATXT                                                        
166519         ,TEMATXT                                                         
166520         ,TEINKTXT                                                        
166521         ,TEANSTXT                                                        
166522         ,TEAUXTXT                                                        
166523         ,IDARTNR_OFARG                                                   
166524         ,IDPSLAG                                                         
166525         ,BETYP                                                           
166526         ,IDFKNGRP                                                        
166527         ,IDKDPPOS                                                        
166528         ,IDAOTUTG                                                        
166529         ,BEANST_KU                                                       
166530         ,BEANST_SU                                                       
166531         ,IDPSS                                                           
166532         ,DAPSWQP_1                                                       
166533         ,KDPSWQP_1                                                       
166534         ,DAPSWPP_2                                                       
166535         ,KDPSWPP_2                                                       
166536         ,DAPSWCP_3                                                       
166537         ,KDPSWCP_3                                                       
166538         ,KDFARGST                                                        
166539         ,IDPROJK                                                         
166540         ,VKART_KDP                                                       
166541         ,KDANNULL                                                        
166542         ,BEUPPDSU                                                        
166550                                                                          
166600          FROM   TB1ACCE                                                  
166700           WHERE IDARTNR = :W-IDARTNR                                     
166800          ORDER BY IDARTNR                                                
166900          FOR FETCH ONLY                                                  
167000     END-EXEC                                                             
167100     MOVE 000100  TO GODK-SQLCODEKODER                                    
167200     EXEC SQL                                                             
167300        OPEN TB1ACCE-CRS-1                                                
167400     END-EXEC                                                             
167500     MOVE SQLCODE TO SQLCODE-WS                                           
167600     PERFORM DB2-STATUS-CONTROL                                           
167700     .                                                                    
167800     EJECT                                                                
167900 DB2-DCL-OPN-TB1ACCE-CRS-TYP-2  SECTION.                                  
168000     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-2' TO DB2-SEKTION                  
168100     EXEC SQL                                                             
168200          DECLARE TB1ACCE-CRS-2 CURSOR FOR SELECT                         
168300                                                                          
168400          IDARTNR                                                         
168500         ,BEART                                                           
168600         ,IDPRODGR                                                        
168700         ,IDUPPDSU                                                        
168800         ,IDUPPDKU                                                        
168900         ,IDAOT                                                           
169000         ,TIAOINF                                                         
169100         ,BEASSTYP                                                        
169110         ,KDARTTYP                                                        
169130         ,KDMDS                                                           
169140         ,KDFRPTYP                                                        
169150         ,TEARTUTFG                                                       
169160         ,TESTATUPP                                                       
169170         ,IDLEVNR_GSDB                                                    
169180         ,KDTPD_PH1                                                       
169190         ,DATPDPH1                                                        
169191         ,DAPSWQP_1                                                       
169192         ,KDPSWQP_1                                                       
169193         ,DAPSWQA_1                                                       
169194         ,KDPSWQA_1                                                       
169195         ,DAPSWPP_2                                                       
169196         ,KDPSWPP_2                                                       
169197         ,DAPSWPA_2                                                       
169198         ,KDPSWPA_2                                                       
169199         ,DAPSWCP_3                                                       
169200         ,KDPSWCP_3                                                       
169201         ,DAPSWCA_3                                                       
169202         ,KDPSWCA_3                                                       
169203         ,KVYVOL_B3                                                       
169204         ,KVYVOL_B2                                                       
169206         ,KVYVOL_INT                                                      
169207         ,KVYVOL_B1                                                       
169208         ,KVYVOL_ASS                                                      
169212         ,BEMAPP                                                          
169213         ,KVFOTO                                                          
169214         ,TIFOTO                                                          
169215         ,TENOTE                                                          
169216         ,FLANNULL                                                        
169217         ,TEVERKTYG                                                       
169218         ,TESTATXT                                                        
169219         ,TEMATXT                                                         
169220         ,TEINKTXT                                                        
169221         ,TEANSTXT                                                        
169222         ,TEAUXTXT                                                        
169223         ,IDARTNR_OFARG                                                   
169224         ,IDPSLAG                                                         
169225         ,BETYP                                                           
169226         ,IDFKNGRP                                                        
169227         ,IDKDPPOS                                                        
169228         ,IDAOTUTG                                                        
169229         ,BEANST_KU                                                       
169230         ,BEANST_SU                                                       
169231         ,IDPSS                                                           
169232         ,DAPSWQP_1                                                       
169233         ,KDPSWQP_1                                                       
169234         ,DAPSWPP_2                                                       
169235         ,KDPSWPP_2                                                       
169236         ,DAPSWCP_3                                                       
169237         ,KDPSWCP_3                                                       
169238         ,KDFARGST                                                        
169239         ,IDPROJK                                                         
169240         ,VKART_KDP                                                       
169241         ,KDANNULL                                                        
169242         ,BEUPPDSU                                                        
169250                                                                          
169300          FROM   TB1ACCE                                                  
169400          WHERE  IDUPPDSU = :W-IDUPPDSU                                   
169500          ORDER BY IDARTNR                                                
169600          FOR FETCH ONLY                                                  
169700     END-EXEC                                                             
169800     MOVE 000100  TO GODK-SQLCODEKODER                                    
169900     EXEC SQL                                                             
170000        OPEN TB1ACCE-CRS-2                                                
170100     END-EXEC                                                             
170200     MOVE SQLCODE TO SQLCODE-WS                                           
170300     PERFORM DB2-STATUS-CONTROL                                           
170400     .                                                                    
170500     EJECT                                                                
170650 DB2-DCL-OPN-TB1ACCE-CRS-TYP-4  SECTION.                                  
170700     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-4' TO DB2-SEKTION                  
170800     EXEC SQL                                                             
170900          DECLARE TB1ACCE-CRS-4 CURSOR FOR SELECT                         
171000                                                                          
171100          IDARTNR                                                         
171200         ,BEART                                                           
171300         ,IDPRODGR                                                        
171400         ,IDUPPDSU                                                        
171500         ,IDUPPDKU                                                        
171600         ,IDAOT                                                           
171700         ,TIAOINF                                                         
171800         ,BEASSTYP                                                        
171810         ,KDARTTYP                                                        
171830         ,KDMDS                                                           
171840         ,KDFRPTYP                                                        
171850         ,TEARTUTFG                                                       
171860         ,TESTATUPP                                                       
171870         ,IDLEVNR_GSDB                                                    
171880         ,KDTPD_PH1                                                       
171890         ,DATPDPH1                                                        
171891         ,DAPSWQP_1                                                       
171892         ,KDPSWQP_1                                                       
171893         ,DAPSWQA_1                                                       
171894         ,KDPSWQA_1                                                       
171895         ,DAPSWPP_2                                                       
171896         ,KDPSWPP_2                                                       
171897         ,DAPSWPA_2                                                       
171898         ,KDPSWPA_2                                                       
171899         ,DAPSWCP_3                                                       
171900         ,KDPSWCP_3                                                       
171901         ,DAPSWCA_3                                                       
171902         ,KDPSWCA_3                                                       
171903         ,KVYVOL_B3                                                       
171904         ,KVYVOL_B2                                                       
171906         ,KVYVOL_INT                                                      
171907         ,KVYVOL_B1                                                       
171908         ,KVYVOL_ASS                                                      
171912         ,BEMAPP                                                          
171913         ,KVFOTO                                                          
171914         ,TIFOTO                                                          
171915         ,TENOTE                                                          
171916         ,FLANNULL                                                        
171917         ,TEVERKTYG                                                       
171918         ,TESTATXT                                                        
171919         ,TEMATXT                                                         
171920         ,TEINKTXT                                                        
171921         ,TEANSTXT                                                        
171922         ,TEAUXTXT                                                        
171923         ,IDARTNR_OFARG                                                   
171924         ,IDPSLAG                                                         
171925         ,BETYP                                                           
171926         ,IDFKNGRP                                                        
171927         ,IDKDPPOS                                                        
171928         ,IDAOTUTG                                                        
171929         ,BEANST_KU                                                       
171930         ,BEANST_SU                                                       
171931         ,IDPSS                                                           
171932         ,DAPSWQP_1                                                       
171933         ,KDPSWQP_1                                                       
171934         ,DAPSWPP_2                                                       
171935         ,KDPSWPP_2                                                       
171936         ,DAPSWCP_3                                                       
171937         ,KDPSWCP_3                                                       
171938         ,KDFARGST                                                        
171939         ,IDPROJK                                                         
171940         ,VKART_KDP                                                       
171941         ,KDANNULL                                                        
171942         ,BEUPPDSU                                                        
171950                                                                          
172000          FROM   TB1ACCE                                                  
172100             WHERE IDUPPDKU = :W-IDUPPDKU                                 
172200          ORDER BY IDARTNR                                                
172300          FOR FETCH ONLY                                                  
172400     END-EXEC                                                             
172500     MOVE 000100  TO GODK-SQLCODEKODER                                    
172600     EXEC SQL                                                             
172700        OPEN TB1ACCE-CRS-4                                                
172800     END-EXEC                                                             
172900     MOVE SQLCODE TO SQLCODE-WS                                           
173000     PERFORM DB2-STATUS-CONTROL                                           
173100     .                                                                    
173200     EJECT                                                                
173300 DB2-DCL-OPN-TB1ACCE-CRS-TYP-5  SECTION.                                  
173400     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-5' TO DB2-SEKTION                  
173500     EXEC SQL                                                             
173600          DECLARE TB1ACCE-CRS-5 CURSOR FOR SELECT                         
173700                                                                          
173800          IDARTNR                                                         
173900         ,BEART                                                           
174000         ,IDPRODGR                                                        
174100         ,IDUPPDSU                                                        
174200         ,IDUPPDKU                                                        
174300         ,IDAOT                                                           
174400         ,TIAOINF                                                         
174500         ,BEASSTYP                                                        
174510         ,KDARTTYP                                                        
174530         ,KDMDS                                                           
174540         ,KDFRPTYP                                                        
174550         ,TEARTUTFG                                                       
174560         ,TESTATUPP                                                       
174570         ,IDLEVNR_GSDB                                                    
174580         ,KDTPD_PH1                                                       
174590         ,DATPDPH1                                                        
174591         ,DAPSWQP_1                                                       
174592         ,KDPSWQP_1                                                       
174593         ,DAPSWQA_1                                                       
174594         ,KDPSWQA_1                                                       
174595         ,DAPSWPP_2                                                       
174596         ,KDPSWPP_2                                                       
174597         ,DAPSWPA_2                                                       
174598         ,KDPSWPA_2                                                       
174599         ,DAPSWCP_3                                                       
174600         ,KDPSWCP_3                                                       
174601         ,DAPSWCA_3                                                       
174602         ,KDPSWCA_3                                                       
174603         ,KVYVOL_B3                                                       
174604         ,KVYVOL_B2                                                       
174606         ,KVYVOL_INT                                                      
174607         ,KVYVOL_B1                                                       
174608         ,KVYVOL_ASS                                                      
174612         ,BEMAPP                                                          
174613         ,KVFOTO                                                          
174614         ,TIFOTO                                                          
174615         ,TENOTE                                                          
174616         ,FLANNULL                                                        
174617         ,TEVERKTYG                                                       
174618         ,TESTATXT                                                        
174619         ,TEMATXT                                                         
174620         ,TEINKTXT                                                        
174621         ,TEANSTXT                                                        
174622         ,TEAUXTXT                                                        
174623         ,IDARTNR_OFARG                                                   
174624         ,IDPSLAG                                                         
174625         ,BETYP                                                           
174626         ,IDFKNGRP                                                        
174627         ,IDKDPPOS                                                        
174628         ,IDAOTUTG                                                        
174629         ,BEANST_KU                                                       
174630         ,BEANST_SU                                                       
174631         ,IDPSS                                                           
174632         ,DAPSWQP_1                                                       
174633         ,KDPSWQP_1                                                       
174634         ,DAPSWPP_2                                                       
174635         ,KDPSWPP_2                                                       
174636         ,DAPSWCP_3                                                       
174637         ,KDPSWCP_3                                                       
174638         ,KDFARGST                                                        
174639         ,IDPROJK                                                         
174640         ,VKART_KDP                                                       
174641         ,KDANNULL                                                        
174642         ,BEUPPDSU                                                        
174650                                                                          
174700          FROM   TB1ACCE                                                  
174900             WHERE ( IDARTNR  = :W-IDARTNR                                
174910               AND   IDUPPDKU = :W-IDUPPDKU  )                            
175000          ORDER BY IDARTNR                                                
175100          FOR FETCH ONLY                                                  
175200     END-EXEC                                                             
175300     MOVE 000100  TO GODK-SQLCODEKODER                                    
175400     EXEC SQL                                                             
175500        OPEN TB1ACCE-CRS-5                                                
175600     END-EXEC                                                             
175700     MOVE SQLCODE TO SQLCODE-WS                                           
175800     PERFORM DB2-STATUS-CONTROL                                           
175900     .                                                                    
176000     EJECT                                                                
176010 DB2-DCL-OPN-TB1ACCE-CRS-TYP-6  SECTION.                                  
176020     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-6' TO DB2-SEKTION                  
176030     EXEC SQL                                                             
176040          DECLARE TB1ACCE-CRS-6 CURSOR FOR SELECT                         
176050                                                                          
176060          IDARTNR                                                         
176070         ,BEART                                                           
176080         ,IDPRODGR                                                        
176090         ,IDUPPDSU                                                        
176091         ,IDUPPDKU                                                        
176092         ,IDAOT                                                           
176093         ,TIAOINF                                                         
176094         ,BEASSTYP                                                        
176095         ,KDARTTYP                                                        
176096         ,KDMDS                                                           
176097         ,KDFRPTYP                                                        
176098         ,TEARTUTFG                                                       
176099         ,TESTATUPP                                                       
176100         ,IDLEVNR_GSDB                                                    
176101         ,KDTPD_PH1                                                       
176102         ,DATPDPH1                                                        
176103         ,DAPSWQP_1                                                       
176104         ,KDPSWQP_1                                                       
176105         ,DAPSWQA_1                                                       
176106         ,KDPSWQA_1                                                       
176107         ,DAPSWPP_2                                                       
176108         ,KDPSWPP_2                                                       
176109         ,DAPSWPA_2                                                       
176110         ,KDPSWPA_2                                                       
176111         ,DAPSWCP_3                                                       
176112         ,KDPSWCP_3                                                       
176113         ,DAPSWCA_3                                                       
176114         ,KDPSWCA_3                                                       
176115         ,KVYVOL_B3                                                       
176116         ,KVYVOL_B2                                                       
176117         ,KVYVOL_INT                                                      
176118         ,KVYVOL_B1                                                       
176119         ,KVYVOL_ASS                                                      
176120         ,BEMAPP                                                          
176121         ,KVFOTO                                                          
176122         ,TIFOTO                                                          
176123         ,TENOTE                                                          
176124         ,FLANNULL                                                        
176125         ,TEVERKTYG                                                       
176126         ,TESTATXT                                                        
176127         ,TEMATXT                                                         
176128         ,TEINKTXT                                                        
176129         ,TEANSTXT                                                        
176130         ,TEAUXTXT                                                        
176131         ,IDARTNR_OFARG                                                   
176132         ,IDPSLAG                                                         
176133         ,BETYP                                                           
176134         ,IDFKNGRP                                                        
176135         ,IDKDPPOS                                                        
176136         ,IDAOTUTG                                                        
176137         ,BEANST_KU                                                       
176138         ,BEANST_SU                                                       
176139         ,IDPSS                                                           
176140         ,DAPSWQP_1                                                       
176141         ,KDPSWQP_1                                                       
176142         ,DAPSWPP_2                                                       
176143         ,KDPSWPP_2                                                       
176144         ,DAPSWCP_3                                                       
176145         ,KDPSWCP_3                                                       
176146         ,KDFARGST                                                        
176147         ,IDPROJK                                                         
176148         ,VKART_KDP                                                       
176149         ,KDANNULL                                                        
176150         ,BEUPPDSU                                                        
176151                                                                          
176152          FROM   TB1ACCE                                                  
176153             WHERE ( IDUPPDSU = :W-IDUPPDSU                               
176154               AND   IDUPPDKU = :W-IDUPPDKU  )                            
176155          ORDER BY IDARTNR                                                
176156          FOR FETCH ONLY                                                  
176157     END-EXEC                                                             
176158     MOVE 000100  TO GODK-SQLCODEKODER                                    
176159     EXEC SQL                                                             
176160        OPEN TB1ACCE-CRS-6                                                
176161     END-EXEC                                                             
176162     MOVE SQLCODE TO SQLCODE-WS                                           
176163     PERFORM DB2-STATUS-CONTROL                                           
176164     .                                                                    
176165     EJECT                                                                
176166 DB2-DCL-OPN-TB1ACCE-CRS-TYP-7  SECTION.                                  
176167     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-7' TO DB2-SEKTION                  
176168     EXEC SQL                                                             
176169          DECLARE TB1ACCE-CRS-7 CURSOR FOR SELECT                         
176170                                                                          
176171          IDARTNR                                                         
176172         ,BEART                                                           
176173         ,IDPRODGR                                                        
176174         ,IDUPPDSU                                                        
176175         ,IDUPPDKU                                                        
176176         ,IDAOT                                                           
176177         ,TIAOINF                                                         
176178         ,BEASSTYP                                                        
176179         ,KDARTTYP                                                        
176180         ,KDMDS                                                           
176181         ,KDFRPTYP                                                        
176182         ,TEARTUTFG                                                       
176183         ,TESTATUPP                                                       
176184         ,IDLEVNR_GSDB                                                    
176185         ,KDTPD_PH1                                                       
176186         ,DATPDPH1                                                        
176187         ,DAPSWQP_1                                                       
176188         ,KDPSWQP_1                                                       
176189         ,DAPSWQA_1                                                       
176190         ,KDPSWQA_1                                                       
176191         ,DAPSWPP_2                                                       
176192         ,KDPSWPP_2                                                       
176193         ,DAPSWPA_2                                                       
176194         ,KDPSWPA_2                                                       
176195         ,DAPSWCP_3                                                       
176196         ,KDPSWCP_3                                                       
176197         ,DAPSWCA_3                                                       
176198         ,KDPSWCA_3                                                       
176199         ,KVYVOL_B3                                                       
176200         ,KVYVOL_B2                                                       
176201         ,KVYVOL_INT                                                      
176202         ,KVYVOL_B1                                                       
176203         ,KVYVOL_ASS                                                      
176204         ,BEMAPP                                                          
176205         ,KVFOTO                                                          
176206         ,TIFOTO                                                          
176207         ,TENOTE                                                          
176208         ,FLANNULL                                                        
176209         ,TEVERKTYG                                                       
176210         ,TESTATXT                                                        
176211         ,TEMATXT                                                         
176212         ,TEINKTXT                                                        
176213         ,TEANSTXT                                                        
176214         ,TEAUXTXT                                                        
176215         ,IDARTNR_OFARG                                                   
176216         ,IDPSLAG                                                         
176217         ,BETYP                                                           
176218         ,IDFKNGRP                                                        
176219         ,IDKDPPOS                                                        
176220         ,IDAOTUTG                                                        
176221         ,BEANST_KU                                                       
176222         ,BEANST_SU                                                       
176223         ,IDPSS                                                           
176224         ,DAPSWQP_1                                                       
176225         ,KDPSWQP_1                                                       
176226         ,DAPSWPP_2                                                       
176227         ,KDPSWPP_2                                                       
176228         ,DAPSWCP_3                                                       
176229         ,KDPSWCP_3                                                       
176230         ,KDFARGST                                                        
176231         ,IDPROJK                                                         
176232         ,VKART_KDP                                                       
176233         ,KDANNULL                                                        
176234         ,BEUPPDSU                                                        
176235                                                                          
176236          FROM   TB1ACCE                                                  
176237             WHERE ( IDUPPDSU = :W-IDUPPDSU                               
176238               AND   IDUPPDKU = :W-IDUPPDKU                               
176239               AND   IDARTNR  = :W-IDARTNR   )                            
176240          ORDER BY IDARTNR                                                
176241          FOR FETCH ONLY                                                  
176242     END-EXEC                                                             
176243     MOVE 000100  TO GODK-SQLCODEKODER                                    
176244     EXEC SQL                                                             
176245        OPEN TB1ACCE-CRS-7                                                
176246     END-EXEC                                                             
176247     MOVE SQLCODE TO SQLCODE-WS                                           
176248     PERFORM DB2-STATUS-CONTROL                                           
176249     .                                                                    
176250     EJECT                                                                
176251 DB2-DCL-OPN-TB1ACCE-CRS-TYP-8  SECTION.                                  
176260     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-8' TO DB2-SEKTION                  
176300     EXEC SQL                                                             
176400          DECLARE TB1ACCE-CRS-8 CURSOR FOR SELECT                         
176500                                                                          
176600          IDARTNR                                                         
176700         ,BEART                                                           
176800         ,IDPRODGR                                                        
176900         ,IDUPPDSU                                                        
177000         ,IDUPPDKU                                                        
177100         ,IDAOT                                                           
177200         ,TIAOINF                                                         
177300         ,BEASSTYP                                                        
177310         ,KDARTTYP                                                        
177330         ,KDMDS                                                           
177340         ,KDFRPTYP                                                        
177350         ,TEARTUTFG                                                       
177360         ,TESTATUPP                                                       
177370         ,IDLEVNR_GSDB                                                    
177380         ,KDTPD_PH1                                                       
177390         ,DATPDPH1                                                        
177391         ,DAPSWQP_1                                                       
177392         ,KDPSWQP_1                                                       
177393         ,DAPSWQA_1                                                       
177394         ,KDPSWQA_1                                                       
177395         ,DAPSWPP_2                                                       
177396         ,KDPSWPP_2                                                       
177397         ,DAPSWPA_2                                                       
177398         ,KDPSWPA_2                                                       
177399         ,DAPSWCP_3                                                       
177400         ,KDPSWCP_3                                                       
177401         ,DAPSWCA_3                                                       
177402         ,KDPSWCA_3                                                       
177403         ,KVYVOL_B3                                                       
177404         ,KVYVOL_B2                                                       
177406         ,KVYVOL_INT                                                      
177407         ,KVYVOL_B1                                                       
177408         ,KVYVOL_ASS                                                      
177412         ,BEMAPP                                                          
177413         ,KVFOTO                                                          
177414         ,TIFOTO                                                          
177415         ,TENOTE                                                          
177416         ,FLANNULL                                                        
177417         ,TEVERKTYG                                                       
177418         ,TESTATXT                                                        
177419         ,TEMATXT                                                         
177420         ,TEINKTXT                                                        
177421         ,TEANSTXT                                                        
177422         ,TEAUXTXT                                                        
177423         ,IDARTNR_OFARG                                                   
177424         ,IDPSLAG                                                         
177425         ,BETYP                                                           
177426         ,IDFKNGRP                                                        
177427         ,IDKDPPOS                                                        
177428         ,IDAOTUTG                                                        
177429         ,BEANST_KU                                                       
177430         ,BEANST_SU                                                       
177431         ,IDPSS                                                           
177432         ,DAPSWQP_1                                                       
177433         ,KDPSWQP_1                                                       
177434         ,DAPSWPP_2                                                       
177435         ,KDPSWPP_2                                                       
177436         ,DAPSWCP_3                                                       
177437         ,KDPSWCP_3                                                       
177438         ,KDFARGST                                                        
177439         ,IDPROJK                                                         
177440         ,VKART_KDP                                                       
177441         ,KDANNULL                                                        
177442         ,BEUPPDSU                                                        
177450                                                                          
177500          FROM   TB1ACCE                                                  
177600          WHERE ( TIAOINF >= :W-TIAOINF-FOM                               
177700            AND   TIAOINF <= :W-TIAOINF-TOM )                             
177800          ORDER BY IDARTNR                                                
177900          FOR FETCH ONLY                                                  
178000     END-EXEC                                                             
178100     MOVE 000100  TO GODK-SQLCODEKODER                                    
178200     EXEC SQL                                                             
178300        OPEN TB1ACCE-CRS-8                                                
178400     END-EXEC                                                             
178500     MOVE SQLCODE TO SQLCODE-WS                                           
178600     PERFORM DB2-STATUS-CONTROL                                           
178700     .                                                                    
178800     EJECT                                                                
178900 DB2-DCL-OPN-TB1ACCE-CRS-TYP-9  SECTION.                                  
179000     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-9' TO DB2-SEKTION                  
179100     EXEC SQL                                                             
179200          DECLARE TB1ACCE-CRS-9 CURSOR FOR SELECT                         
179300                                                                          
179400          IDARTNR                                                         
179500         ,BEART                                                           
179600         ,IDPRODGR                                                        
179700         ,IDUPPDSU                                                        
179800         ,IDUPPDKU                                                        
179900         ,IDAOT                                                           
180000         ,TIAOINF                                                         
180100         ,BEASSTYP                                                        
180110         ,KDARTTYP                                                        
180130         ,KDMDS                                                           
180140         ,KDFRPTYP                                                        
180150         ,TEARTUTFG                                                       
180160         ,TESTATUPP                                                       
180170         ,IDLEVNR_GSDB                                                    
180180         ,KDTPD_PH1                                                       
180190         ,DATPDPH1                                                        
180191         ,DAPSWQP_1                                                       
180192         ,KDPSWQP_1                                                       
180193         ,DAPSWQA_1                                                       
180194         ,KDPSWQA_1                                                       
180195         ,DAPSWPP_2                                                       
180196         ,KDPSWPP_2                                                       
180197         ,DAPSWPA_2                                                       
180198         ,KDPSWPA_2                                                       
180199         ,DAPSWCP_3                                                       
180200         ,KDPSWCP_3                                                       
180201         ,DAPSWCA_3                                                       
180202         ,KDPSWCA_3                                                       
180203         ,KVYVOL_B3                                                       
180204         ,KVYVOL_B2                                                       
180206         ,KVYVOL_INT                                                      
180207         ,KVYVOL_B1                                                       
180208         ,KVYVOL_ASS                                                      
180212         ,BEMAPP                                                          
180213         ,KVFOTO                                                          
180214         ,TIFOTO                                                          
180215         ,TENOTE                                                          
180216         ,FLANNULL                                                        
180217         ,TEVERKTYG                                                       
180218         ,TESTATXT                                                        
180219         ,TEMATXT                                                         
180220         ,TEINKTXT                                                        
180221         ,TEANSTXT                                                        
180222         ,TEAUXTXT                                                        
180223         ,IDARTNR_OFARG                                                   
180224         ,IDPSLAG                                                         
180225         ,BETYP                                                           
180226         ,IDFKNGRP                                                        
180227         ,IDKDPPOS                                                        
180228         ,IDAOTUTG                                                        
180229         ,BEANST_KU                                                       
180230         ,BEANST_SU                                                       
180231         ,IDPSS                                                           
180232         ,DAPSWQP_1                                                       
180233         ,KDPSWQP_1                                                       
180234         ,DAPSWPP_2                                                       
180235         ,KDPSWPP_2                                                       
180236         ,DAPSWCP_3                                                       
180237         ,KDPSWCP_3                                                       
180238         ,KDFARGST                                                        
180239         ,IDPROJK                                                         
180240         ,VKART_KDP                                                       
180241         ,KDANNULL                                                        
180242         ,BEUPPDSU                                                        
180250                                                                          
180300          FROM   TB1ACCE                                                  
180310             WHERE ( IDARTNR = :W-IDARTNR                                 
180400               AND   TIAOINF >= :W-TIAOINF-FOM                            
180500               AND   TIAOINF <= :W-TIAOINF-TOM )                          
180700          ORDER BY IDARTNR                                                
180800          FOR FETCH ONLY                                                  
180900     END-EXEC                                                             
181000     MOVE 000100  TO GODK-SQLCODEKODER                                    
181100     EXEC SQL                                                             
181200        OPEN TB1ACCE-CRS-9                                                
181300     END-EXEC                                                             
181400     MOVE SQLCODE TO SQLCODE-WS                                           
181500     PERFORM DB2-STATUS-CONTROL                                           
181600     .                                                                    
181700     EJECT                                                                
181710                                                                          
181800 DB2-DCL-OPN-TB1ACCE-CRS-TYP-10 SECTION.                                  
181900     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-10' TO DB2-SEKTION                 
182000     EXEC SQL                                                             
182100          DECLARE TB1ACCE-CRS-10 CURSOR FOR SELECT                        
182200                                                                          
182300          IDARTNR                                                         
182400         ,BEART                                                           
182500         ,IDPRODGR                                                        
182600         ,IDUPPDSU                                                        
182700         ,IDUPPDKU                                                        
182800         ,IDAOT                                                           
182900         ,TIAOINF                                                         
183000         ,BEASSTYP                                                        
183010         ,KDARTTYP                                                        
183030         ,KDMDS                                                           
183040         ,KDFRPTYP                                                        
183050         ,TEARTUTFG                                                       
183060         ,TESTATUPP                                                       
183070         ,IDLEVNR_GSDB                                                    
183080         ,KDTPD_PH1                                                       
183090         ,DATPDPH1                                                        
183091         ,DAPSWQP_1                                                       
183092         ,KDPSWQP_1                                                       
183093         ,DAPSWQA_1                                                       
183094         ,KDPSWQA_1                                                       
183095         ,DAPSWPP_2                                                       
183096         ,KDPSWPP_2                                                       
183097         ,DAPSWPA_2                                                       
183098         ,KDPSWPA_2                                                       
183099         ,DAPSWCP_3                                                       
183100         ,KDPSWCP_3                                                       
183101         ,DAPSWCA_3                                                       
183102         ,KDPSWCA_3                                                       
183103         ,KVYVOL_B3                                                       
183104         ,KVYVOL_B2                                                       
183106         ,KVYVOL_INT                                                      
183107         ,KVYVOL_B1                                                       
183108         ,KVYVOL_ASS                                                      
183112         ,BEMAPP                                                          
183113         ,KVFOTO                                                          
183114         ,TIFOTO                                                          
183115         ,TENOTE                                                          
183116         ,FLANNULL                                                        
183117         ,TEVERKTYG                                                       
183118         ,TESTATXT                                                        
183119         ,TEMATXT                                                         
183120         ,TEINKTXT                                                        
183121         ,TEANSTXT                                                        
183122         ,TEAUXTXT                                                        
183123         ,IDARTNR_OFARG                                                   
183124         ,IDPSLAG                                                         
183125         ,BETYP                                                           
183126         ,IDFKNGRP                                                        
183127         ,IDKDPPOS                                                        
183128         ,IDAOTUTG                                                        
183129         ,BEANST_KU                                                       
183130         ,BEANST_SU                                                       
183131         ,IDPSS                                                           
183132         ,DAPSWQP_1                                                       
183133         ,KDPSWQP_1                                                       
183134         ,DAPSWPP_2                                                       
183135         ,KDPSWPP_2                                                       
183136         ,DAPSWCP_3                                                       
183137         ,KDPSWCP_3                                                       
183138         ,KDFARGST                                                        
183139         ,IDPROJK                                                         
183140         ,VKART_KDP                                                       
183141         ,KDANNULL                                                        
183142         ,BEUPPDSU                                                        
183150                                                                          
183200          FROM   TB1ACCE                                                  
183300             WHERE ( TIAOINF >= :W-TIAOINF-FOM                            
183400               AND   TIAOINF <= :W-TIAOINF-TOM                            
183500               AND   IDUPPDSU = :W-IDUPPDSU  )                            
183600          ORDER BY IDARTNR                                                
183700          FOR FETCH ONLY                                                  
183800     END-EXEC                                                             
183900     MOVE 000100  TO GODK-SQLCODEKODER                                    
184000     EXEC SQL                                                             
184100        OPEN TB1ACCE-CRS-10                                               
184200     END-EXEC                                                             
184300     MOVE SQLCODE TO SQLCODE-WS                                           
184400     PERFORM DB2-STATUS-CONTROL                                           
184500     .                                                                    
184600     EJECT                                                                
184601                                                                          
184610 DB2-DCL-OPN-TB1ACCE-CRS-TYP-11 SECTION.                                  
184620     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-11' TO DB2-SEKTION                 
184630     EXEC SQL                                                             
184640          DECLARE TB1ACCE-CRS-11 CURSOR FOR SELECT                        
184650                                                                          
184660          IDARTNR                                                         
184670         ,BEART                                                           
184680         ,IDPRODGR                                                        
184690         ,IDUPPDSU                                                        
184691         ,IDUPPDKU                                                        
184692         ,IDAOT                                                           
184693         ,TIAOINF                                                         
184694         ,BEASSTYP                                                        
184695         ,KDARTTYP                                                        
184696         ,KDMDS                                                           
184697         ,KDFRPTYP                                                        
184698         ,TEARTUTFG                                                       
184699         ,TESTATUPP                                                       
184700         ,IDLEVNR_GSDB                                                    
184701         ,KDTPD_PH1                                                       
184702         ,DATPDPH1                                                        
184703         ,DAPSWQP_1                                                       
184704         ,KDPSWQP_1                                                       
184705         ,DAPSWQA_1                                                       
184706         ,KDPSWQA_1                                                       
184707         ,DAPSWPP_2                                                       
184708         ,KDPSWPP_2                                                       
184709         ,DAPSWPA_2                                                       
184710         ,KDPSWPA_2                                                       
184711         ,DAPSWCP_3                                                       
184712         ,KDPSWCP_3                                                       
184713         ,DAPSWCA_3                                                       
184714         ,KDPSWCA_3                                                       
184715         ,KVYVOL_B3                                                       
184716         ,KVYVOL_B2                                                       
184717         ,KVYVOL_INT                                                      
184718         ,KVYVOL_B1                                                       
184719         ,KVYVOL_ASS                                                      
184720         ,BEMAPP                                                          
184721         ,KVFOTO                                                          
184722         ,TIFOTO                                                          
184723         ,TENOTE                                                          
184724         ,FLANNULL                                                        
184725         ,TEVERKTYG                                                       
184726         ,TESTATXT                                                        
184727         ,TEMATXT                                                         
184728         ,TEINKTXT                                                        
184729         ,TEANSTXT                                                        
184730         ,TEAUXTXT                                                        
184731         ,IDARTNR_OFARG                                                   
184732         ,IDPSLAG                                                         
184733         ,BETYP                                                           
184734         ,IDFKNGRP                                                        
184735         ,IDKDPPOS                                                        
184736         ,IDAOTUTG                                                        
184737         ,BEANST_KU                                                       
184738         ,BEANST_SU                                                       
184739         ,IDPSS                                                           
184740         ,DAPSWQP_1                                                       
184741         ,KDPSWQP_1                                                       
184742         ,DAPSWPP_2                                                       
184743         ,KDPSWPP_2                                                       
184744         ,DAPSWCP_3                                                       
184745         ,KDPSWCP_3                                                       
184746         ,KDFARGST                                                        
184747         ,IDPROJK                                                         
184748         ,VKART_KDP                                                       
184749         ,KDANNULL                                                        
184750         ,BEUPPDSU                                                        
184751                                                                          
184752          FROM   TB1ACCE                                                  
184753             WHERE ( TIAOINF >= :W-TIAOINF-FOM                            
184754               AND   TIAOINF <= :W-TIAOINF-TOM                            
184755               AND   IDUPPDSU = :W-IDUPPDSU                               
184756               AND   IDARTNR  = :W-IDARTNR   )                            
184757          ORDER BY IDARTNR                                                
184758          FOR FETCH ONLY                                                  
184759     END-EXEC                                                             
184760     MOVE 000100  TO GODK-SQLCODEKODER                                    
184761     EXEC SQL                                                             
184762        OPEN TB1ACCE-CRS-11                                               
184763     END-EXEC                                                             
184764     MOVE SQLCODE TO SQLCODE-WS                                           
184765     PERFORM DB2-STATUS-CONTROL                                           
184766     .                                                                    
184767     EJECT                                                                
184770 DB2-DCL-OPN-TB1ACCE-CRS-TYP-12 SECTION.                                  
184800     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-12' TO DB2-SEKTION                 
184900     EXEC SQL                                                             
185000          DECLARE TB1ACCE-CRS-12 CURSOR FOR SELECT                        
185100                                                                          
185200          IDARTNR                                                         
185300         ,BEART                                                           
185400         ,IDPRODGR                                                        
185500         ,IDUPPDSU                                                        
185600         ,IDUPPDKU                                                        
185700         ,IDAOT                                                           
185800         ,TIAOINF                                                         
185900         ,BEASSTYP                                                        
185910         ,KDARTTYP                                                        
185930         ,KDMDS                                                           
185940         ,KDFRPTYP                                                        
185950         ,TEARTUTFG                                                       
185960         ,TESTATUPP                                                       
185970         ,IDLEVNR_GSDB                                                    
185980         ,KDTPD_PH1                                                       
185990         ,DATPDPH1                                                        
185991         ,DAPSWQP_1                                                       
185992         ,KDPSWQP_1                                                       
185993         ,DAPSWQA_1                                                       
185994         ,KDPSWQA_1                                                       
185995         ,DAPSWPP_2                                                       
185996         ,KDPSWPP_2                                                       
185997         ,DAPSWPA_2                                                       
185998         ,KDPSWPA_2                                                       
185999         ,DAPSWCP_3                                                       
186000         ,KDPSWCP_3                                                       
186001         ,DAPSWCA_3                                                       
186002         ,KDPSWCA_3                                                       
186003         ,KVYVOL_B3                                                       
186004         ,KVYVOL_B2                                                       
186006         ,KVYVOL_INT                                                      
186007         ,KVYVOL_B1                                                       
186008         ,KVYVOL_ASS                                                      
186012         ,BEMAPP                                                          
186013         ,KVFOTO                                                          
186014         ,TIFOTO                                                          
186015         ,TENOTE                                                          
186016         ,FLANNULL                                                        
186017         ,TEVERKTYG                                                       
186018         ,TESTATXT                                                        
186019         ,TEMATXT                                                         
186020         ,TEINKTXT                                                        
186021         ,TEANSTXT                                                        
186022         ,TEAUXTXT                                                        
186023         ,IDARTNR_OFARG                                                   
186024         ,IDPSLAG                                                         
186025         ,BETYP                                                           
186026         ,IDFKNGRP                                                        
186027         ,IDKDPPOS                                                        
186028         ,IDAOTUTG                                                        
186029         ,BEANST_KU                                                       
186030         ,BEANST_SU                                                       
186031         ,IDPSS                                                           
186032         ,DAPSWQP_1                                                       
186033         ,KDPSWQP_1                                                       
186034         ,DAPSWPP_2                                                       
186035         ,KDPSWPP_2                                                       
186036         ,DAPSWCP_3                                                       
186037         ,KDPSWCP_3                                                       
186038         ,KDFARGST                                                        
186039         ,IDPROJK                                                         
186040         ,VKART_KDP                                                       
186041         ,KDANNULL                                                        
186042         ,BEUPPDSU                                                        
186050                                                                          
186100          FROM   TB1ACCE                                                  
186200             WHERE ( TIAOINF >= :W-TIAOINF-FOM                            
186300               AND   TIAOINF <= :W-TIAOINF-TOM                            
186400               AND   IDUPPDKU = :W-IDUPPDKU  )                            
186500          ORDER BY IDARTNR                                                
186600          FOR FETCH ONLY                                                  
186700     END-EXEC                                                             
186800     MOVE 000100  TO GODK-SQLCODEKODER                                    
186900     EXEC SQL                                                             
187000        OPEN TB1ACCE-CRS-12                                               
187100     END-EXEC                                                             
187200     MOVE SQLCODE TO SQLCODE-WS                                           
187300     PERFORM DB2-STATUS-CONTROL                                           
187400     .                                                                    
187500     EJECT                                                                
187501                                                                          
187510 DB2-DCL-OPN-TB1ACCE-CRS-TYP-13 SECTION.                                  
187520     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-13' TO DB2-SEKTION                 
187530     EXEC SQL                                                             
187540          DECLARE TB1ACCE-CRS-13 CURSOR FOR SELECT                        
187550                                                                          
187560          IDARTNR                                                         
187570         ,BEART                                                           
187580         ,IDPRODGR                                                        
187590         ,IDUPPDSU                                                        
187591         ,IDUPPDKU                                                        
187592         ,IDAOT                                                           
187593         ,TIAOINF                                                         
187594         ,BEASSTYP                                                        
187595         ,KDARTTYP                                                        
187596         ,KDMDS                                                           
187597         ,KDFRPTYP                                                        
187598         ,TEARTUTFG                                                       
187599         ,TESTATUPP                                                       
187600         ,IDLEVNR_GSDB                                                    
187601         ,KDTPD_PH1                                                       
187602         ,DATPDPH1                                                        
187603         ,DAPSWQP_1                                                       
187604         ,KDPSWQP_1                                                       
187605         ,DAPSWQA_1                                                       
187606         ,KDPSWQA_1                                                       
187607         ,DAPSWPP_2                                                       
187608         ,KDPSWPP_2                                                       
187609         ,DAPSWPA_2                                                       
187610         ,KDPSWPA_2                                                       
187611         ,DAPSWCP_3                                                       
187612         ,KDPSWCP_3                                                       
187613         ,DAPSWCA_3                                                       
187614         ,KDPSWCA_3                                                       
187615         ,KVYVOL_B3                                                       
187616         ,KVYVOL_B2                                                       
187617         ,KVYVOL_INT                                                      
187618         ,KVYVOL_B1                                                       
187619         ,KVYVOL_ASS                                                      
187620         ,BEMAPP                                                          
187621         ,KVFOTO                                                          
187622         ,TIFOTO                                                          
187623         ,TENOTE                                                          
187624         ,FLANNULL                                                        
187625         ,TEVERKTYG                                                       
187626         ,TESTATXT                                                        
187627         ,TEMATXT                                                         
187628         ,TEINKTXT                                                        
187629         ,TEANSTXT                                                        
187630         ,TEAUXTXT                                                        
187631         ,IDARTNR_OFARG                                                   
187632         ,IDPSLAG                                                         
187633         ,BETYP                                                           
187634         ,IDFKNGRP                                                        
187635         ,IDKDPPOS                                                        
187636         ,IDAOTUTG                                                        
187637         ,BEANST_KU                                                       
187638         ,BEANST_SU                                                       
187639         ,IDPSS                                                           
187640         ,DAPSWQP_1                                                       
187641         ,KDPSWQP_1                                                       
187642         ,DAPSWPP_2                                                       
187643         ,KDPSWPP_2                                                       
187644         ,DAPSWCP_3                                                       
187645         ,KDPSWCP_3                                                       
187646         ,KDFARGST                                                        
187647         ,IDPROJK                                                         
187648         ,VKART_KDP                                                       
187649         ,KDANNULL                                                        
187650         ,BEUPPDSU                                                        
187651                                                                          
187652          FROM   TB1ACCE                                                  
187653             WHERE ( TIAOINF >= :W-TIAOINF-FOM                            
187654               AND   TIAOINF <= :W-TIAOINF-TOM                            
187655               AND   IDUPPDKU = :W-IDUPPDKU                               
187656               AND   IDARTNR  = :W-IDARTNR   )                            
187657          ORDER BY IDARTNR                                                
187658          FOR FETCH ONLY                                                  
187659     END-EXEC                                                             
187660     MOVE 000100  TO GODK-SQLCODEKODER                                    
187661     EXEC SQL                                                             
187662        OPEN TB1ACCE-CRS-13                                               
187663     END-EXEC                                                             
187664     MOVE SQLCODE TO SQLCODE-WS                                           
187665     PERFORM DB2-STATUS-CONTROL                                           
187666     .                                                                    
187667     EJECT                                                                
187668                                                                          
187669 DB2-DCL-OPN-TB1ACCE-CRS-TYP-14 SECTION.                                  
187670     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-14' TO DB2-SEKTION                 
187671     EXEC SQL                                                             
187672          DECLARE TB1ACCE-CRS-14 CURSOR FOR SELECT                        
187673                                                                          
187674          IDARTNR                                                         
187675         ,BEART                                                           
187676         ,IDPRODGR                                                        
187677         ,IDUPPDSU                                                        
187678         ,IDUPPDKU                                                        
187679         ,IDAOT                                                           
187680         ,TIAOINF                                                         
187681         ,BEASSTYP                                                        
187682         ,KDARTTYP                                                        
187683         ,KDMDS                                                           
187684         ,KDFRPTYP                                                        
187685         ,TEARTUTFG                                                       
187686         ,TESTATUPP                                                       
187687         ,IDLEVNR_GSDB                                                    
187688         ,KDTPD_PH1                                                       
187689         ,DATPDPH1                                                        
187690         ,DAPSWQP_1                                                       
187691         ,KDPSWQP_1                                                       
187692         ,DAPSWQA_1                                                       
187693         ,KDPSWQA_1                                                       
187694         ,DAPSWPP_2                                                       
187695         ,KDPSWPP_2                                                       
187696         ,DAPSWPA_2                                                       
187697         ,KDPSWPA_2                                                       
187698         ,DAPSWCP_3                                                       
187699         ,KDPSWCP_3                                                       
187700         ,DAPSWCA_3                                                       
187701         ,KDPSWCA_3                                                       
187702         ,KVYVOL_B3                                                       
187703         ,KVYVOL_B2                                                       
187704         ,KVYVOL_INT                                                      
187705         ,KVYVOL_B1                                                       
187706         ,KVYVOL_ASS                                                      
187707         ,BEMAPP                                                          
187708         ,KVFOTO                                                          
187709         ,TIFOTO                                                          
187710         ,TENOTE                                                          
187711         ,FLANNULL                                                        
187712         ,TEVERKTYG                                                       
187713         ,TESTATXT                                                        
187714         ,TEMATXT                                                         
187715         ,TEINKTXT                                                        
187716         ,TEANSTXT                                                        
187717         ,TEAUXTXT                                                        
187718         ,IDARTNR_OFARG                                                   
187719         ,IDPSLAG                                                         
187720         ,BETYP                                                           
187721         ,IDFKNGRP                                                        
187722         ,IDKDPPOS                                                        
187723         ,IDAOTUTG                                                        
187724         ,BEANST_KU                                                       
187725         ,BEANST_SU                                                       
187726         ,IDPSS                                                           
187727         ,DAPSWQP_1                                                       
187728         ,KDPSWQP_1                                                       
187729         ,DAPSWPP_2                                                       
187730         ,KDPSWPP_2                                                       
187731         ,DAPSWCP_3                                                       
187732         ,KDPSWCP_3                                                       
187733         ,KDFARGST                                                        
187734         ,IDPROJK                                                         
187735         ,VKART_KDP                                                       
187736         ,KDANNULL                                                        
187737         ,BEUPPDSU                                                        
187738                                                                          
187739          FROM   TB1ACCE                                                  
187740             WHERE ( TIAOINF >= :W-TIAOINF-FOM                            
187741               AND   TIAOINF <= :W-TIAOINF-TOM                            
187742               AND   IDUPPDKU = :W-IDUPPDKU                               
187743               AND   IDUPPDSU = :W-IDUPPDSU  )                            
187744          ORDER BY IDARTNR                                                
187745          FOR FETCH ONLY                                                  
187746     END-EXEC                                                             
187747     MOVE 000100  TO GODK-SQLCODEKODER                                    
187748     EXEC SQL                                                             
187749        OPEN TB1ACCE-CRS-14                                               
187750     END-EXEC                                                             
187751     MOVE SQLCODE TO SQLCODE-WS                                           
187752     PERFORM DB2-STATUS-CONTROL                                           
187753     .                                                                    
187754     EJECT                                                                
187755                                                                          
187756 DB2-DCL-OPN-TB1ACCE-CRS-TYP-15 SECTION.                                  
187757     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-15' TO DB2-SEKTION                 
187758     EXEC SQL                                                             
187759          DECLARE TB1ACCE-CRS-15 CURSOR FOR SELECT                        
187760                                                                          
187761          IDARTNR                                                         
187762         ,BEART                                                           
187763         ,IDPRODGR                                                        
187764         ,IDUPPDSU                                                        
187765         ,IDUPPDKU                                                        
187766         ,IDAOT                                                           
187767         ,TIAOINF                                                         
187768         ,BEASSTYP                                                        
187769         ,KDARTTYP                                                        
187770         ,KDMDS                                                           
187771         ,KDFRPTYP                                                        
187772         ,TEARTUTFG                                                       
187773         ,TESTATUPP                                                       
187774         ,IDLEVNR_GSDB                                                    
187775         ,KDTPD_PH1                                                       
187776         ,DATPDPH1                                                        
187777         ,DAPSWQP_1                                                       
187778         ,KDPSWQP_1                                                       
187779         ,DAPSWQA_1                                                       
187780         ,KDPSWQA_1                                                       
187781         ,DAPSWPP_2                                                       
187782         ,KDPSWPP_2                                                       
187783         ,DAPSWPA_2                                                       
187784         ,KDPSWPA_2                                                       
187785         ,DAPSWCP_3                                                       
187786         ,KDPSWCP_3                                                       
187787         ,DAPSWCA_3                                                       
187788         ,KDPSWCA_3                                                       
187789         ,KVYVOL_B3                                                       
187790         ,KVYVOL_B2                                                       
187791         ,KVYVOL_INT                                                      
187792         ,KVYVOL_B1                                                       
187793         ,KVYVOL_ASS                                                      
187794         ,BEMAPP                                                          
187795         ,KVFOTO                                                          
187796         ,TIFOTO                                                          
187797         ,TENOTE                                                          
187798         ,FLANNULL                                                        
187799         ,TEVERKTYG                                                       
187800         ,TESTATXT                                                        
187801         ,TEMATXT                                                         
187802         ,TEINKTXT                                                        
187803         ,TEANSTXT                                                        
187804         ,TEAUXTXT                                                        
187805         ,IDARTNR_OFARG                                                   
187806         ,IDPSLAG                                                         
187807         ,BETYP                                                           
187808         ,IDFKNGRP                                                        
187809         ,IDKDPPOS                                                        
187810         ,IDAOTUTG                                                        
187811         ,BEANST_KU                                                       
187812         ,BEANST_SU                                                       
187813         ,IDPSS                                                           
187814         ,DAPSWQP_1                                                       
187815         ,KDPSWQP_1                                                       
187816         ,DAPSWPP_2                                                       
187817         ,KDPSWPP_2                                                       
187818         ,DAPSWCP_3                                                       
187819         ,KDPSWCP_3                                                       
187820         ,KDFARGST                                                        
187821         ,IDPROJK                                                         
187822         ,VKART_KDP                                                       
187823         ,KDANNULL                                                        
187824         ,BEUPPDSU                                                        
187825                                                                          
187826          FROM   TB1ACCE                                                  
187827             WHERE ( TIAOINF >= :W-TIAOINF-FOM                            
187828               AND   TIAOINF <= :W-TIAOINF-TOM                            
187829               AND   IDUPPDKU = :W-IDUPPDKU                               
187830               AND   IDUPPDSU = :W-IDUPPDSU                               
187831               AND   IDARTNR  = :W-IDARTNR   )                            
187832          ORDER BY IDARTNR                                                
187833          FOR FETCH ONLY                                                  
187834     END-EXEC                                                             
187835     MOVE 000100  TO GODK-SQLCODEKODER                                    
187836     EXEC SQL                                                             
187837        OPEN TB1ACCE-CRS-15                                               
187838     END-EXEC                                                             
187839     MOVE SQLCODE TO SQLCODE-WS                                           
187840     PERFORM DB2-STATUS-CONTROL                                           
187841     .                                                                    
187842     EJECT                                                                
187843                                                                          
187844 DB2-DCL-OPN-TB1ACCE-CRS-TYP-16 SECTION.                                  
187845     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-16' TO DB2-SEKTION                 
187846     EXEC SQL                                                             
187847          DECLARE TB1ACCE-CRS-16 CURSOR FOR SELECT                        
187848                                                                          
187849          IDARTNR                                                         
187850         ,BEART                                                           
187851         ,IDPRODGR                                                        
187852         ,IDUPPDSU                                                        
187853         ,IDUPPDKU                                                        
187854         ,IDAOT                                                           
187855         ,TIAOINF                                                         
187856         ,BEASSTYP                                                        
187857         ,KDARTTYP                                                        
187858         ,KDMDS                                                           
187859         ,KDFRPTYP                                                        
187860         ,TEARTUTFG                                                       
187861         ,TESTATUPP                                                       
187862         ,IDLEVNR_GSDB                                                    
187863         ,KDTPD_PH1                                                       
187864         ,DATPDPH1                                                        
187865         ,DAPSWQP_1                                                       
187866         ,KDPSWQP_1                                                       
187867         ,DAPSWQA_1                                                       
187868         ,KDPSWQA_1                                                       
187869         ,DAPSWPP_2                                                       
187870         ,KDPSWPP_2                                                       
187871         ,DAPSWPA_2                                                       
187872         ,KDPSWPA_2                                                       
187873         ,DAPSWCP_3                                                       
187874         ,KDPSWCP_3                                                       
187875         ,DAPSWCA_3                                                       
187876         ,KDPSWCA_3                                                       
187877         ,KVYVOL_B3                                                       
187878         ,KVYVOL_B2                                                       
187879         ,KVYVOL_INT                                                      
187880         ,KVYVOL_B1                                                       
187881         ,KVYVOL_ASS                                                      
187882         ,BEMAPP                                                          
187883         ,KVFOTO                                                          
187884         ,TIFOTO                                                          
187885         ,TENOTE                                                          
187886         ,FLANNULL                                                        
187887         ,TEVERKTYG                                                       
187888         ,TESTATXT                                                        
187889         ,TEMATXT                                                         
187890         ,TEINKTXT                                                        
187891         ,TEANSTXT                                                        
187892         ,TEAUXTXT                                                        
187893         ,IDARTNR_OFARG                                                   
187894         ,IDPSLAG                                                         
187895         ,BETYP                                                           
187896         ,IDFKNGRP                                                        
187897         ,IDKDPPOS                                                        
187898         ,IDAOTUTG                                                        
187899         ,BEANST_KU                                                       
187900         ,BEANST_SU                                                       
187901         ,IDPSS                                                           
187902         ,DAPSWQP_1                                                       
187903         ,KDPSWQP_1                                                       
187904         ,DAPSWPP_2                                                       
187905         ,KDPSWPP_2                                                       
187906         ,DAPSWCP_3                                                       
187907         ,KDPSWCP_3                                                       
187908         ,KDFARGST                                                        
187909         ,IDPROJK                                                         
187910         ,VKART_KDP                                                       
187911         ,KDANNULL                                                        
187912         ,BEUPPDSU                                                        
187913                                                                          
187914          FROM   TB1ACCE                                                  
187916             WHERE   UPPER(BEUPPDSU) LIKE UPPER(:W-BEUPPDSU)              
187917          ORDER BY IDARTNR                                                
187918          FOR FETCH ONLY                                                  
187919     END-EXEC                                                             
187920     MOVE 000100  TO GODK-SQLCODEKODER                                    
187921     EXEC SQL                                                             
187922        OPEN TB1ACCE-CRS-16                                               
187923     END-EXEC                                                             
187924     MOVE SQLCODE TO SQLCODE-WS                                           
187925     PERFORM DB2-STATUS-CONTROL                                           
187926     .                                                                    
187927     EJECT                                                                
187928                                                                          
187929 DB2-DCL-OPN-TB1ACCE-CRS-TYP-17 SECTION.                                  
187930     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-17' TO DB2-SEKTION                 
187931     EXEC SQL                                                             
187932          DECLARE TB1ACCE-CRS-17 CURSOR FOR SELECT                        
187933                                                                          
187934          IDARTNR                                                         
187935         ,BEART                                                           
187936         ,IDPRODGR                                                        
187937         ,IDUPPDSU                                                        
187938         ,IDUPPDKU                                                        
187939         ,IDAOT                                                           
187940         ,TIAOINF                                                         
187941         ,BEASSTYP                                                        
187942         ,KDARTTYP                                                        
187943         ,KDMDS                                                           
187944         ,KDFRPTYP                                                        
187945         ,TEARTUTFG                                                       
187946         ,TESTATUPP                                                       
187947         ,IDLEVNR_GSDB                                                    
187948         ,KDTPD_PH1                                                       
187949         ,DATPDPH1                                                        
187950         ,DAPSWQP_1                                                       
187951         ,KDPSWQP_1                                                       
187952         ,DAPSWQA_1                                                       
187953         ,KDPSWQA_1                                                       
187954         ,DAPSWPP_2                                                       
187955         ,KDPSWPP_2                                                       
187956         ,DAPSWPA_2                                                       
187957         ,KDPSWPA_2                                                       
187958         ,DAPSWCP_3                                                       
187959         ,KDPSWCP_3                                                       
187960         ,DAPSWCA_3                                                       
187961         ,KDPSWCA_3                                                       
187962         ,KVYVOL_B3                                                       
187963         ,KVYVOL_B2                                                       
187964         ,KVYVOL_INT                                                      
187965         ,KVYVOL_B1                                                       
187966         ,KVYVOL_ASS                                                      
187967         ,BEMAPP                                                          
187968         ,KVFOTO                                                          
187969         ,TIFOTO                                                          
187970         ,TENOTE                                                          
187971         ,FLANNULL                                                        
187972         ,TEVERKTYG                                                       
187973         ,TESTATXT                                                        
187974         ,TEMATXT                                                         
187975         ,TEINKTXT                                                        
187976         ,TEANSTXT                                                        
187977         ,TEAUXTXT                                                        
187978         ,IDARTNR_OFARG                                                   
187979         ,IDPSLAG                                                         
187980         ,BETYP                                                           
187981         ,IDFKNGRP                                                        
187982         ,IDKDPPOS                                                        
187983         ,IDAOTUTG                                                        
187984         ,BEANST_KU                                                       
187985         ,BEANST_SU                                                       
187986         ,IDPSS                                                           
187987         ,DAPSWQP_1                                                       
187988         ,KDPSWQP_1                                                       
187989         ,DAPSWPP_2                                                       
187990         ,KDPSWPP_2                                                       
187991         ,DAPSWCP_3                                                       
187992         ,KDPSWCP_3                                                       
187993         ,KDFARGST                                                        
187994         ,IDPROJK                                                         
187995         ,VKART_KDP                                                       
187996         ,KDANNULL                                                        
187997         ,BEUPPDSU                                                        
187998                                                                          
187999          FROM   TB1ACCE                                                  
188000             WHERE ( IDARTNR  = :W-IDARTNR                                
188001               AND   UPPER(BEUPPDSU) LIKE UPPER(:W-BEUPPDSU))             
188002          ORDER BY IDARTNR                                                
188003          FOR FETCH ONLY                                                  
188004     END-EXEC                                                             
188005     MOVE 000100  TO GODK-SQLCODEKODER                                    
188006     EXEC SQL                                                             
188007        OPEN TB1ACCE-CRS-17                                               
188008     END-EXEC                                                             
188009     MOVE SQLCODE TO SQLCODE-WS                                           
188010     PERFORM DB2-STATUS-CONTROL                                           
188011     .                                                                    
188012     EJECT                                                                
188013                                                                          
188014 DB2-DCL-OPN-TB1ACCE-CRS-TYP-18 SECTION.                                  
188015     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-18' TO DB2-SEKTION                 
188016     EXEC SQL                                                             
188017          DECLARE TB1ACCE-CRS-18 CURSOR FOR SELECT                        
188018                                                                          
188019          IDARTNR                                                         
188020         ,BEART                                                           
188021         ,IDPRODGR                                                        
188022         ,IDUPPDSU                                                        
188023         ,IDUPPDKU                                                        
188024         ,IDAOT                                                           
188025         ,TIAOINF                                                         
188026         ,BEASSTYP                                                        
188027         ,KDARTTYP                                                        
188028         ,KDMDS                                                           
188029         ,KDFRPTYP                                                        
188030         ,TEARTUTFG                                                       
188031         ,TESTATUPP                                                       
188032         ,IDLEVNR_GSDB                                                    
188033         ,KDTPD_PH1                                                       
188034         ,DATPDPH1                                                        
188035         ,DAPSWQP_1                                                       
188036         ,KDPSWQP_1                                                       
188037         ,DAPSWQA_1                                                       
188038         ,KDPSWQA_1                                                       
188039         ,DAPSWPP_2                                                       
188040         ,KDPSWPP_2                                                       
188041         ,DAPSWPA_2                                                       
188042         ,KDPSWPA_2                                                       
188043         ,DAPSWCP_3                                                       
188044         ,KDPSWCP_3                                                       
188045         ,DAPSWCA_3                                                       
188046         ,KDPSWCA_3                                                       
188047         ,KVYVOL_B3                                                       
188048         ,KVYVOL_B2                                                       
188049         ,KVYVOL_INT                                                      
188050         ,KVYVOL_B1                                                       
188051         ,KVYVOL_ASS                                                      
188052         ,BEMAPP                                                          
188053         ,KVFOTO                                                          
188054         ,TIFOTO                                                          
188055         ,TENOTE                                                          
188056         ,FLANNULL                                                        
188057         ,TEVERKTYG                                                       
188058         ,TESTATXT                                                        
188059         ,TEMATXT                                                         
188060         ,TEINKTXT                                                        
188061         ,TEANSTXT                                                        
188062         ,TEAUXTXT                                                        
188063         ,IDARTNR_OFARG                                                   
188064         ,IDPSLAG                                                         
188065         ,BETYP                                                           
188066         ,IDFKNGRP                                                        
188067         ,IDKDPPOS                                                        
188068         ,IDAOTUTG                                                        
188069         ,BEANST_KU                                                       
188070         ,BEANST_SU                                                       
188071         ,IDPSS                                                           
188072         ,DAPSWQP_1                                                       
188073         ,KDPSWQP_1                                                       
188074         ,DAPSWPP_2                                                       
188075         ,KDPSWPP_2                                                       
188076         ,DAPSWCP_3                                                       
188077         ,KDPSWCP_3                                                       
188078         ,KDFARGST                                                        
188079         ,IDPROJK                                                         
188080         ,VKART_KDP                                                       
188081         ,KDANNULL                                                        
188082         ,BEUPPDSU                                                        
188083                                                                          
188084          FROM   TB1ACCE                                                  
188085             WHERE ( IDUPPDSU = :W-IDUPPDSU                               
188086               AND   IDARTNR  = :W-IDARTNR                                
188087               AND   UPPER(BEUPPDSU) LIKE UPPER(:W-BEUPPDSU))             
188088          ORDER BY IDARTNR                                                
188089          FOR FETCH ONLY                                                  
188090     END-EXEC                                                             
188091     MOVE 000100  TO GODK-SQLCODEKODER                                    
188092     EXEC SQL                                                             
188093        OPEN TB1ACCE-CRS-18                                               
188094     END-EXEC                                                             
188095     MOVE SQLCODE TO SQLCODE-WS                                           
188096     PERFORM DB2-STATUS-CONTROL                                           
188097     .                                                                    
188098     EJECT                                                                
188099                                                                          
188100                                                                          
188101 DB2-DCL-OPN-TB1ACCE-CRS-TYP-19 SECTION.                                  
188102     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-19' TO DB2-SEKTION                 
188103     EXEC SQL                                                             
188104          DECLARE TB1ACCE-CRS-19 CURSOR FOR SELECT                        
188105                                                                          
188106          IDARTNR                                                         
188107         ,BEART                                                           
188108         ,IDPRODGR                                                        
188109         ,IDUPPDSU                                                        
188110         ,IDUPPDKU                                                        
188111         ,IDAOT                                                           
188112         ,TIAOINF                                                         
188113         ,BEASSTYP                                                        
188114         ,KDARTTYP                                                        
188115         ,KDMDS                                                           
188116         ,KDFRPTYP                                                        
188117         ,TEARTUTFG                                                       
188118         ,TESTATUPP                                                       
188119         ,IDLEVNR_GSDB                                                    
188120         ,KDTPD_PH1                                                       
188121         ,DATPDPH1                                                        
188122         ,DAPSWQP_1                                                       
188123         ,KDPSWQP_1                                                       
188124         ,DAPSWQA_1                                                       
188125         ,KDPSWQA_1                                                       
188126         ,DAPSWPP_2                                                       
188127         ,KDPSWPP_2                                                       
188128         ,DAPSWPA_2                                                       
188129         ,KDPSWPA_2                                                       
188130         ,DAPSWCP_3                                                       
188131         ,KDPSWCP_3                                                       
188132         ,DAPSWCA_3                                                       
188133         ,KDPSWCA_3                                                       
188134         ,KVYVOL_B3                                                       
188135         ,KVYVOL_B2                                                       
188136         ,KVYVOL_INT                                                      
188137         ,KVYVOL_B1                                                       
188138         ,KVYVOL_ASS                                                      
188139         ,BEMAPP                                                          
188140         ,KVFOTO                                                          
188141         ,TIFOTO                                                          
188142         ,TENOTE                                                          
188143         ,FLANNULL                                                        
188144         ,TEVERKTYG                                                       
188145         ,TESTATXT                                                        
188146         ,TEMATXT                                                         
188147         ,TEINKTXT                                                        
188148         ,TEANSTXT                                                        
188149         ,TEAUXTXT                                                        
188150         ,IDARTNR_OFARG                                                   
188151         ,IDPSLAG                                                         
188152         ,BETYP                                                           
188153         ,IDFKNGRP                                                        
188154         ,IDKDPPOS                                                        
188155         ,IDAOTUTG                                                        
188156         ,BEANST_KU                                                       
188157         ,BEANST_SU                                                       
188158         ,IDPSS                                                           
188159         ,DAPSWQP_1                                                       
188160         ,KDPSWQP_1                                                       
188161         ,DAPSWPP_2                                                       
188162         ,KDPSWPP_2                                                       
188163         ,DAPSWCP_3                                                       
188164         ,KDPSWCP_3                                                       
188165         ,KDFARGST                                                        
188166         ,IDPROJK                                                         
188167         ,VKART_KDP                                                       
188168         ,KDANNULL                                                        
188169         ,BEUPPDSU                                                        
188170                                                                          
188171          FROM   TB1ACCE                                                  
188172             WHERE ( IDUPPDKU = :W-IDUPPDKU                               
188173               AND   IDUPPDSU = :W-IDUPPDSU                               
188174               AND   IDARTNR  = :W-IDARTNR                                
188175               AND   UPPER(BEUPPDSU) LIKE UPPER(:W-BEUPPDSU))             
188176          ORDER BY IDARTNR                                                
188177          FOR FETCH ONLY                                                  
188178     END-EXEC                                                             
188179     MOVE 000100  TO GODK-SQLCODEKODER                                    
188180     EXEC SQL                                                             
188181        OPEN TB1ACCE-CRS-19                                               
188182     END-EXEC                                                             
188183     MOVE SQLCODE TO SQLCODE-WS                                           
188184     PERFORM DB2-STATUS-CONTROL                                           
188185     .                                                                    
188186     EJECT                                                                
188187                                                                          
188188 DB2-DCL-OPN-TB1ACCE-CRS-TYP-20 SECTION.                                  
188189     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-20' TO DB2-SEKTION                 
188190     EXEC SQL                                                             
188191          DECLARE TB1ACCE-CRS-20 CURSOR FOR SELECT                        
188192                                                                          
188193          IDARTNR                                                         
188194         ,BEART                                                           
188195         ,IDPRODGR                                                        
188196         ,IDUPPDSU                                                        
188197         ,IDUPPDKU                                                        
188198         ,IDAOT                                                           
188199         ,TIAOINF                                                         
188200         ,BEASSTYP                                                        
188201         ,KDARTTYP                                                        
188202         ,KDMDS                                                           
188203         ,KDFRPTYP                                                        
188204         ,TEARTUTFG                                                       
188205         ,TESTATUPP                                                       
188206         ,IDLEVNR_GSDB                                                    
188207         ,KDTPD_PH1                                                       
188208         ,DATPDPH1                                                        
188209         ,DAPSWQP_1                                                       
188210         ,KDPSWQP_1                                                       
188211         ,DAPSWQA_1                                                       
188212         ,KDPSWQA_1                                                       
188213         ,DAPSWPP_2                                                       
188214         ,KDPSWPP_2                                                       
188215         ,DAPSWPA_2                                                       
188216         ,KDPSWPA_2                                                       
188217         ,DAPSWCP_3                                                       
188218         ,KDPSWCP_3                                                       
188219         ,DAPSWCA_3                                                       
188220         ,KDPSWCA_3                                                       
188221         ,KVYVOL_B3                                                       
188222         ,KVYVOL_B2                                                       
188223         ,KVYVOL_INT                                                      
188224         ,KVYVOL_B1                                                       
188225         ,KVYVOL_ASS                                                      
188226         ,BEMAPP                                                          
188227         ,KVFOTO                                                          
188228         ,TIFOTO                                                          
188229         ,TENOTE                                                          
188230         ,FLANNULL                                                        
188231         ,TEVERKTYG                                                       
188232         ,TESTATXT                                                        
188233         ,TEMATXT                                                         
188234         ,TEINKTXT                                                        
188235         ,TEANSTXT                                                        
188236         ,TEAUXTXT                                                        
188237         ,IDARTNR_OFARG                                                   
188238         ,IDPSLAG                                                         
188239         ,BETYP                                                           
188240         ,IDFKNGRP                                                        
188241         ,IDKDPPOS                                                        
188242         ,IDAOTUTG                                                        
188243         ,BEANST_KU                                                       
188244         ,BEANST_SU                                                       
188245         ,IDPSS                                                           
188246         ,DAPSWQP_1                                                       
188247         ,KDPSWQP_1                                                       
188248         ,DAPSWPP_2                                                       
188249         ,KDPSWPP_2                                                       
188250         ,DAPSWCP_3                                                       
188251         ,KDPSWCP_3                                                       
188252         ,KDFARGST                                                        
188253         ,IDPROJK                                                         
188254         ,VKART_KDP                                                       
188255         ,KDANNULL                                                        
188256         ,BEUPPDSU                                                        
188257                                                                          
188258          FROM   TB1ACCE                                                  
188259             WHERE ( TIAOINF >= :W-TIAOINF-FOM                            
188260               AND   TIAOINF <= :W-TIAOINF-TOM                            
188261               AND   IDUPPDKU = :W-IDUPPDKU                               
188262               AND   IDUPPDSU = :W-IDUPPDSU                               
188263               AND   IDARTNR  = :W-IDARTNR                                
188264               AND   UPPER(BEUPPDSU) LIKE UPPER(:W-BEUPPDSU))             
188265          ORDER BY IDARTNR                                                
188266          FOR FETCH ONLY                                                  
188267     END-EXEC                                                             
188268     MOVE 000100  TO GODK-SQLCODEKODER                                    
188269     EXEC SQL                                                             
188270        OPEN TB1ACCE-CRS-20                                               
188271     END-EXEC                                                             
188272     MOVE SQLCODE TO SQLCODE-WS                                           
188273     PERFORM DB2-STATUS-CONTROL                                           
188274     .                                                                    
188275     EJECT                                                                
188276                                                                          
188277 DB2-DCL-OPN-TB1ACCE-CRS-TYP-21 SECTION.                                  
188278     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-21' TO DB2-SEKTION                 
188279     EXEC SQL                                                             
188280          DECLARE TB1ACCE-CRS-21 CURSOR FOR SELECT                        
188281                                                                          
188282          IDARTNR                                                         
188283         ,BEART                                                           
188284         ,IDPRODGR                                                        
188285         ,IDUPPDSU                                                        
188286         ,IDUPPDKU                                                        
188287         ,IDAOT                                                           
188288         ,TIAOINF                                                         
188289         ,BEASSTYP                                                        
188290         ,KDARTTYP                                                        
188291         ,KDMDS                                                           
188292         ,KDFRPTYP                                                        
188293         ,TEARTUTFG                                                       
188294         ,TESTATUPP                                                       
188295         ,IDLEVNR_GSDB                                                    
188296         ,KDTPD_PH1                                                       
188297         ,DATPDPH1                                                        
188298         ,DAPSWQP_1                                                       
188299         ,KDPSWQP_1                                                       
188300         ,DAPSWQA_1                                                       
188301         ,KDPSWQA_1                                                       
188302         ,DAPSWPP_2                                                       
188303         ,KDPSWPP_2                                                       
188304         ,DAPSWPA_2                                                       
188305         ,KDPSWPA_2                                                       
188306         ,DAPSWCP_3                                                       
188307         ,KDPSWCP_3                                                       
188308         ,DAPSWCA_3                                                       
188309         ,KDPSWCA_3                                                       
188310         ,KVYVOL_B3                                                       
188311         ,KVYVOL_B2                                                       
188312         ,KVYVOL_INT                                                      
188313         ,KVYVOL_B1                                                       
188314         ,KVYVOL_ASS                                                      
188315         ,BEMAPP                                                          
188316         ,KVFOTO                                                          
188317         ,TIFOTO                                                          
188318         ,TENOTE                                                          
188319         ,FLANNULL                                                        
188320         ,TEVERKTYG                                                       
188321         ,TESTATXT                                                        
188322         ,TEMATXT                                                         
188323         ,TEINKTXT                                                        
188324         ,TEANSTXT                                                        
188325         ,TEAUXTXT                                                        
188326         ,IDARTNR_OFARG                                                   
188327         ,IDPSLAG                                                         
188328         ,BETYP                                                           
188329         ,IDFKNGRP                                                        
188330         ,IDKDPPOS                                                        
188331         ,IDAOTUTG                                                        
188332         ,BEANST_KU                                                       
188333         ,BEANST_SU                                                       
188334         ,IDPSS                                                           
188335         ,DAPSWQP_1                                                       
188336         ,KDPSWQP_1                                                       
188337         ,DAPSWPP_2                                                       
188338         ,KDPSWPP_2                                                       
188339         ,DAPSWCP_3                                                       
188340         ,KDPSWCP_3                                                       
188341         ,KDFARGST                                                        
188342         ,IDPROJK                                                         
188343         ,VKART_KDP                                                       
188344         ,KDANNULL                                                        
188345         ,BEUPPDSU                                                        
188346                                                                          
188347          FROM   TB1ACCE                                                  
188348             WHERE ( TIAOINF >= :W-TIAOINF-FOM                            
188349               AND   TIAOINF <= :W-TIAOINF-TOM                            
188353               AND   UPPER(BEUPPDSU) LIKE UPPER(:W-BEUPPDSU))             
188354          ORDER BY IDARTNR                                                
188355          FOR FETCH ONLY                                                  
188356     END-EXEC                                                             
188357     MOVE 000100  TO GODK-SQLCODEKODER                                    
188358     EXEC SQL                                                             
188359        OPEN TB1ACCE-CRS-21                                               
188360     END-EXEC                                                             
188361     MOVE SQLCODE TO SQLCODE-WS                                           
188362     PERFORM DB2-STATUS-CONTROL                                           
188363     .                                                                    
188364     EJECT                                                                
188365                                                                          
188366 DB2-DCL-OPN-TB1ACCE-CRS-TYP-22 SECTION.                                  
188367     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-22' TO DB2-SEKTION                 
188368     EXEC SQL                                                             
188369          DECLARE TB1ACCE-CRS-22 CURSOR FOR SELECT                        
188370                                                                          
188371          IDARTNR                                                         
188372         ,BEART                                                           
188373         ,IDPRODGR                                                        
188374         ,IDUPPDSU                                                        
188375         ,IDUPPDKU                                                        
188376         ,IDAOT                                                           
188377         ,TIAOINF                                                         
188378         ,BEASSTYP                                                        
188379         ,KDARTTYP                                                        
188380         ,KDMDS                                                           
188381         ,KDFRPTYP                                                        
188382         ,TEARTUTFG                                                       
188383         ,TESTATUPP                                                       
188384         ,IDLEVNR_GSDB                                                    
188385         ,KDTPD_PH1                                                       
188386         ,DATPDPH1                                                        
188387         ,DAPSWQP_1                                                       
188388         ,KDPSWQP_1                                                       
188389         ,DAPSWQA_1                                                       
188390         ,KDPSWQA_1                                                       
188391         ,DAPSWPP_2                                                       
188392         ,KDPSWPP_2                                                       
188393         ,DAPSWPA_2                                                       
188394         ,KDPSWPA_2                                                       
188395         ,DAPSWCP_3                                                       
188396         ,KDPSWCP_3                                                       
188397         ,DAPSWCA_3                                                       
188398         ,KDPSWCA_3                                                       
188399         ,KVYVOL_B3                                                       
188400         ,KVYVOL_B2                                                       
188401         ,KVYVOL_INT                                                      
188402         ,KVYVOL_B1                                                       
188403         ,KVYVOL_ASS                                                      
188404         ,BEMAPP                                                          
188405         ,KVFOTO                                                          
188406         ,TIFOTO                                                          
188407         ,TENOTE                                                          
188408         ,FLANNULL                                                        
188409         ,TEVERKTYG                                                       
188410         ,TESTATXT                                                        
188411         ,TEMATXT                                                         
188412         ,TEINKTXT                                                        
188413         ,TEANSTXT                                                        
188414         ,TEAUXTXT                                                        
188415         ,IDARTNR_OFARG                                                   
188416         ,IDPSLAG                                                         
188417         ,BETYP                                                           
188418         ,IDFKNGRP                                                        
188419         ,IDKDPPOS                                                        
188420         ,IDAOTUTG                                                        
188421         ,BEANST_KU                                                       
188422         ,BEANST_SU                                                       
188423         ,IDPSS                                                           
188424         ,DAPSWQP_1                                                       
188425         ,KDPSWQP_1                                                       
188426         ,DAPSWPP_2                                                       
188427         ,KDPSWPP_2                                                       
188428         ,DAPSWCP_3                                                       
188429         ,KDPSWCP_3                                                       
188430         ,KDFARGST                                                        
188431         ,IDPROJK                                                         
188432         ,VKART_KDP                                                       
188433         ,KDANNULL                                                        
188434         ,BEUPPDSU                                                        
188435                                                                          
188436          FROM   TB1ACCE                                                  
188437             WHERE ( TIAOINF >= :W-TIAOINF-FOM                            
188438               AND   TIAOINF <= :W-TIAOINF-TOM                            
188439               AND   IDUPPDSU = :W-IDUPPDSU                               
188440               AND   UPPER(BEUPPDSU) LIKE UPPER(:W-BEUPPDSU))             
188441          ORDER BY IDARTNR                                                
188442          FOR FETCH ONLY                                                  
188443     END-EXEC                                                             
188444     MOVE 000100  TO GODK-SQLCODEKODER                                    
188445     EXEC SQL                                                             
188446        OPEN TB1ACCE-CRS-22                                               
188447     END-EXEC                                                             
188448     MOVE SQLCODE TO SQLCODE-WS                                           
188449     PERFORM DB2-STATUS-CONTROL                                           
188450     .                                                                    
188451     EJECT                                                                
188452                                                                          
188453 DB2-DCL-OPN-TB1ACCE-CRS-TYP-23 SECTION.                                  
188454     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-23' TO DB2-SEKTION                 
188455     EXEC SQL                                                             
188456          DECLARE TB1ACCE-CRS-23 CURSOR FOR SELECT                        
188457                                                                          
188458          IDARTNR                                                         
188459         ,BEART                                                           
188460         ,IDPRODGR                                                        
188461         ,IDUPPDSU                                                        
188462         ,IDUPPDKU                                                        
188463         ,IDAOT                                                           
188464         ,TIAOINF                                                         
188465         ,BEASSTYP                                                        
188466         ,KDARTTYP                                                        
188467         ,KDMDS                                                           
188468         ,KDFRPTYP                                                        
188469         ,TEARTUTFG                                                       
188470         ,TESTATUPP                                                       
188471         ,IDLEVNR_GSDB                                                    
188472         ,KDTPD_PH1                                                       
188473         ,DATPDPH1                                                        
188474         ,DAPSWQP_1                                                       
188475         ,KDPSWQP_1                                                       
188476         ,DAPSWQA_1                                                       
188477         ,KDPSWQA_1                                                       
188478         ,DAPSWPP_2                                                       
188479         ,KDPSWPP_2                                                       
188480         ,DAPSWPA_2                                                       
188481         ,KDPSWPA_2                                                       
188482         ,DAPSWCP_3                                                       
188483         ,KDPSWCP_3                                                       
188484         ,DAPSWCA_3                                                       
188485         ,KDPSWCA_3                                                       
188486         ,KVYVOL_B3                                                       
188487         ,KVYVOL_B2                                                       
188488         ,KVYVOL_INT                                                      
188489         ,KVYVOL_B1                                                       
188490         ,KVYVOL_ASS                                                      
188491         ,BEMAPP                                                          
188492         ,KVFOTO                                                          
188493         ,TIFOTO                                                          
188494         ,TENOTE                                                          
188495         ,FLANNULL                                                        
188496         ,TEVERKTYG                                                       
188497         ,TESTATXT                                                        
188498         ,TEMATXT                                                         
188499         ,TEINKTXT                                                        
188500         ,TEANSTXT                                                        
188501         ,TEAUXTXT                                                        
188502         ,IDARTNR_OFARG                                                   
188503         ,IDPSLAG                                                         
188504         ,BETYP                                                           
188505         ,IDFKNGRP                                                        
188506         ,IDKDPPOS                                                        
188507         ,IDAOTUTG                                                        
188508         ,BEANST_KU                                                       
188509         ,BEANST_SU                                                       
188510         ,IDPSS                                                           
188511         ,DAPSWQP_1                                                       
188512         ,KDPSWQP_1                                                       
188513         ,DAPSWPP_2                                                       
188514         ,KDPSWPP_2                                                       
188515         ,DAPSWCP_3                                                       
188516         ,KDPSWCP_3                                                       
188517         ,KDFARGST                                                        
188518         ,IDPROJK                                                         
188519         ,VKART_KDP                                                       
188520         ,KDANNULL                                                        
188521         ,BEUPPDSU                                                        
188522                                                                          
188523          FROM   TB1ACCE                                                  
188524             WHERE ( TIAOINF >= :W-TIAOINF-FOM                            
188525               AND   TIAOINF <= :W-TIAOINF-TOM                            
188528               AND   IDARTNR  = :W-IDARTNR                                
188529               AND   UPPER(BEUPPDSU) LIKE UPPER(:W-BEUPPDSU))             
188530          ORDER BY IDARTNR                                                
188531          FOR FETCH ONLY                                                  
188532     END-EXEC                                                             
188533     MOVE 000100  TO GODK-SQLCODEKODER                                    
188534     EXEC SQL                                                             
188535        OPEN TB1ACCE-CRS-23                                               
188536     END-EXEC                                                             
188537     MOVE SQLCODE TO SQLCODE-WS                                           
188538     PERFORM DB2-STATUS-CONTROL                                           
188539     .                                                                    
188540     EJECT                                                                
188541                                                                          
188542 DB2-DCL-OPN-TB1ACCE-CRS-TYP-24 SECTION.                                  
188543     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-24' TO DB2-SEKTION                 
188544     EXEC SQL                                                             
188545          DECLARE TB1ACCE-CRS-24 CURSOR FOR SELECT                        
188546                                                                          
188547          IDARTNR                                                         
188548         ,BEART                                                           
188549         ,IDPRODGR                                                        
188550         ,IDUPPDSU                                                        
188560         ,IDUPPDKU                                                        
188561         ,IDAOT                                                           
188562         ,TIAOINF                                                         
188563         ,BEASSTYP                                                        
188564         ,KDARTTYP                                                        
188565         ,KDMDS                                                           
188566         ,KDFRPTYP                                                        
188567         ,TEARTUTFG                                                       
188568         ,TESTATUPP                                                       
188569         ,IDLEVNR_GSDB                                                    
188570         ,KDTPD_PH1                                                       
188571         ,DATPDPH1                                                        
188572         ,DAPSWQP_1                                                       
188573         ,KDPSWQP_1                                                       
188574         ,DAPSWQA_1                                                       
188575         ,KDPSWQA_1                                                       
188576         ,DAPSWPP_2                                                       
188577         ,KDPSWPP_2                                                       
188578         ,DAPSWPA_2                                                       
188579         ,KDPSWPA_2                                                       
188580         ,DAPSWCP_3                                                       
188581         ,KDPSWCP_3                                                       
188582         ,DAPSWCA_3                                                       
188583         ,KDPSWCA_3                                                       
188584         ,KVYVOL_B3                                                       
188585         ,KVYVOL_B2                                                       
188586         ,KVYVOL_INT                                                      
188587         ,KVYVOL_B1                                                       
188588         ,KVYVOL_ASS                                                      
188589         ,BEMAPP                                                          
188590         ,KVFOTO                                                          
188591         ,TIFOTO                                                          
188592         ,TENOTE                                                          
188593         ,FLANNULL                                                        
188594         ,TEVERKTYG                                                       
188595         ,TESTATXT                                                        
188596         ,TEMATXT                                                         
188597         ,TEINKTXT                                                        
188598         ,TEANSTXT                                                        
188599         ,TEAUXTXT                                                        
188600         ,IDARTNR_OFARG                                                   
188601         ,IDPSLAG                                                         
188602         ,BETYP                                                           
188603         ,IDFKNGRP                                                        
188604         ,IDKDPPOS                                                        
188605         ,IDAOTUTG                                                        
188606         ,BEANST_KU                                                       
188607         ,BEANST_SU                                                       
188608         ,IDPSS                                                           
188609         ,DAPSWQP_1                                                       
188610         ,KDPSWQP_1                                                       
188611         ,DAPSWPP_2                                                       
188612         ,KDPSWPP_2                                                       
188613         ,DAPSWCP_3                                                       
188614         ,KDPSWCP_3                                                       
188615         ,KDFARGST                                                        
188616         ,IDPROJK                                                         
188617         ,VKART_KDP                                                       
188618         ,KDANNULL                                                        
188619         ,BEUPPDSU                                                        
188620                                                                          
188621          FROM   TB1ACCE                                                  
188622             WHERE ( TIAOINF >= :W-TIAOINF-FOM                            
188623               AND   TIAOINF <= :W-TIAOINF-TOM                            
188625               AND   IDUPPDSU = :W-IDUPPDSU                               
188626               AND   IDARTNR  = :W-IDARTNR                                
188627               AND   UPPER(BEUPPDSU) LIKE UPPER(:W-BEUPPDSU))             
188628          ORDER BY IDARTNR                                                
188629          FOR FETCH ONLY                                                  
188630     END-EXEC                                                             
188631     MOVE 000100  TO GODK-SQLCODEKODER                                    
188632     EXEC SQL                                                             
188633        OPEN TB1ACCE-CRS-24                                               
188634     END-EXEC                                                             
188635     MOVE SQLCODE TO SQLCODE-WS                                           
188636     PERFORM DB2-STATUS-CONTROL                                           
188637     .                                                                    
188638     EJECT                                                                
188639 DB2-DCL-OPN-TB1ACCE-CRS-TYP-25 SECTION.                                  
188640     MOVE 'DB2-DCL-OPN-TB1ACCE-CRS-TYP-25' TO DB2-SEKTION                 
188641     EXEC SQL                                                             
188642          DECLARE TB1ACCE-CRS-25 CURSOR FOR SELECT                        
188643                                                                          
188644          IDARTNR                                                         
188645         ,BEART                                                           
188646         ,IDPRODGR                                                        
188647         ,IDUPPDSU                                                        
188648         ,IDUPPDKU                                                        
188649         ,IDAOT                                                           
188650         ,TIAOINF                                                         
188651         ,BEASSTYP                                                        
188652         ,KDARTTYP                                                        
188653         ,KDMDS                                                           
188654         ,KDFRPTYP                                                        
188655         ,TEARTUTFG                                                       
188656         ,TESTATUPP                                                       
188657         ,IDLEVNR_GSDB                                                    
188658         ,KDTPD_PH1                                                       
188659         ,DATPDPH1                                                        
188660         ,DAPSWQP_1                                                       
188661         ,KDPSWQP_1                                                       
188662         ,DAPSWQA_1                                                       
188663         ,KDPSWQA_1                                                       
188664         ,DAPSWPP_2                                                       
188665         ,KDPSWPP_2                                                       
188666         ,DAPSWPA_2                                                       
188667         ,KDPSWPA_2                                                       
188668         ,DAPSWCP_3                                                       
188669         ,KDPSWCP_3                                                       
188670         ,DAPSWCA_3                                                       
188671         ,KDPSWCA_3                                                       
188672         ,KVYVOL_B3                                                       
188673         ,KVYVOL_B2                                                       
188674         ,KVYVOL_INT                                                      
188675         ,KVYVOL_B1                                                       
188676         ,KVYVOL_ASS                                                      
188677         ,BEMAPP                                                          
188678         ,KVFOTO                                                          
188679         ,TIFOTO                                                          
188680         ,TENOTE                                                          
188681         ,FLANNULL                                                        
188682         ,TEVERKTYG                                                       
188683         ,TESTATXT                                                        
188684         ,TEMATXT                                                         
188685         ,TEINKTXT                                                        
188686         ,TEANSTXT                                                        
188687         ,TEAUXTXT                                                        
188688         ,IDARTNR_OFARG                                                   
188689         ,IDPSLAG                                                         
188690         ,BETYP                                                           
188691         ,IDFKNGRP                                                        
188692         ,IDKDPPOS                                                        
188693         ,IDAOTUTG                                                        
188694         ,BEANST_KU                                                       
188695         ,BEANST_SU                                                       
188696         ,IDPSS                                                           
188697         ,DAPSWQP_1                                                       
188698         ,KDPSWQP_1                                                       
188699         ,DAPSWPP_2                                                       
188700         ,KDPSWPP_2                                                       
188701         ,DAPSWCP_3                                                       
188702         ,KDPSWCP_3                                                       
188703         ,KDFARGST                                                        
188704         ,IDPROJK                                                         
188705         ,VKART_KDP                                                       
188706         ,KDANNULL                                                        
188707         ,BEUPPDSU                                                        
188708                                                                          
188709          FROM   TB1ACCE                                                  
188712             WHERE ( IDUPPDSU = :W-IDUPPDSU                               
188714               AND   UPPER(BEUPPDSU) LIKE UPPER(:W-BEUPPDSU))             
188715          ORDER BY IDARTNR                                                
188716          FOR FETCH ONLY                                                  
188717     END-EXEC                                                             
188718     MOVE 000100  TO GODK-SQLCODEKODER                                    
188719     EXEC SQL                                                             
188720        OPEN TB1ACCE-CRS-25                                               
188721     END-EXEC                                                             
188722     MOVE SQLCODE TO SQLCODE-WS                                           
188723     PERFORM DB2-STATUS-CONTROL                                           
188724     .                                                                    
188725     EJECT                                                                
188726                                                                          
188727 DB2-FETCH-TB1ACCE-CRS-TYP-1  SECTION.                                    
188728     SKIP2                                                                
188729     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-1  ' TO DB2-SEKTION                  
188730     MOVE 000100  TO GODK-SQLCODEKODER                                    
188731     EXEC SQL                                                             
188732         FETCH TB1ACCE-CRS-1                                              
188733         INTO                                                             
188734         :ACCE-IDARTNR                                                    
188735        ,:ACCE-BEART                                                      
188736        ,:ACCE-IDPRODGR                                                   
188737        ,:ACCE-IDUPPDSU                                                   
188740        ,:ACCE-IDUPPDKU                                                   
188800        ,:ACCE-IDAOT                                                      
188900        ,:ACCE-TIAOINF                                                    
189000        ,:ACCE-BEASSTYP                                                   
189100        ,:ACCE-KDARTTYP                                                   
189210        ,:ACCE-KDMDS                                                      
189220        ,:ACCE-KDFRPTYP                                                   
189230        ,:ACCE-TEARTUTFG                                                  
189240        ,:ACCE-TESTATUPP                                                  
189250        ,:ACCE-IDLEVNR-GSDB                                               
189260        ,:ACCE-KDTPD-PH1                                                  
189270        ,:ACCE-DATPDPH1                                                   
189280        ,:ACCE-DAPSWQP-1                                                  
189290        ,:ACCE-KDPSWQP-1                                                  
189291        ,:ACCE-DAPSWQA-1                                                  
189292        ,:ACCE-KDPSWQA-1                                                  
189293        ,:ACCE-DAPSWPP-2                                                  
189294        ,:ACCE-KDPSWPP-2                                                  
189295        ,:ACCE-DAPSWPA-2                                                  
189296        ,:ACCE-KDPSWPA-2                                                  
189297        ,:ACCE-DAPSWCP-3                                                  
189298        ,:ACCE-KDPSWCP-3                                                  
189299        ,:ACCE-DAPSWCA-3                                                  
189300        ,:ACCE-KDPSWCA-3                                                  
189301        ,:ACCE-KVYVOL-B3                                                  
189302        ,:ACCE-KVYVOL-B2                                                  
189304        ,:ACCE-KVYVOL-INT                                                 
189305        ,:ACCE-KVYVOL-B1                                                  
189306        ,:ACCE-KVYVOL-ASS                                                 
189310        ,:ACCE-BEMAPP                                                     
189311        ,:ACCE-KVFOTO                                                     
189312        ,:ACCE-TIFOTO                                                     
189313        ,:ACCE-TENOTE                                                     
189314        ,:ACCE-FLANNULL                                                   
189315        ,:ACCE-TEVERKTYG                                                  
189316        ,:ACCE-TESTATXT                                                   
189317        ,:ACCE-TEMATXT                                                    
189318        ,:ACCE-TEINKTXT                                                   
189319        ,:ACCE-TEANSTXT                                                   
189320        ,:ACCE-TEAUXTXT                                                   
189321        ,:ACCE-IDARTNR-OFARG                                              
189322        ,:ACCE-IDPSLAG                                                    
189323        ,:ACCE-BETYP                                                      
189324        ,:ACCE-IDFKNGRP                                                   
189325        ,:ACCE-IDKDPPOS                                                   
189326        ,:ACCE-IDAOTUTG                                                   
189327        ,:ACCE-BEANST-KU                                                  
189328        ,:ACCE-BEANST-SU                                                  
189329        ,:ACCE-IDPSS                                                      
189330        ,:ACCE-DAPSWQP-1                                                  
189331        ,:ACCE-KDPSWQP-1                                                  
189332        ,:ACCE-DAPSWPP-2                                                  
189333        ,:ACCE-KDPSWPP-2                                                  
189334        ,:ACCE-DAPSWCP-3                                                  
189335        ,:ACCE-KDPSWCP-3                                                  
189336        ,:ACCE-KDFARGST                                                   
189337        ,:ACCE-IDPROJK                                                    
189338        ,:ACCE-VKART-KDP                                                  
189339        ,:ACCE-KDANNULL                                                   
189340        ,:ACCE-BEUPPDSU                                                   
189350     END-EXEC                                                             
189400     MOVE SQLCODE TO SQLCODE-WS                                           
189500     PERFORM DB2-STATUS-CONTROL                                           
189600     .                                                                    
189700     EJECT                                                                
189800                                                                          
189900 DB2-FETCH-TB1ACCE-CRS-TYP-2  SECTION.                                    
190000     SKIP2                                                                
190100     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-2  ' TO DB2-SEKTION                  
190200     MOVE 000100  TO GODK-SQLCODEKODER                                    
190300     EXEC SQL                                                             
190400         FETCH TB1ACCE-CRS-2                                              
190500         INTO                                                             
190600         :ACCE-IDARTNR                                                    
190700        ,:ACCE-BEART                                                      
190800        ,:ACCE-IDPRODGR                                                   
190900        ,:ACCE-IDUPPDSU                                                   
191000        ,:ACCE-IDUPPDKU                                                   
191100        ,:ACCE-IDAOT                                                      
191200        ,:ACCE-TIAOINF                                                    
191300        ,:ACCE-BEASSTYP                                                   
191400        ,:ACCE-KDARTTYP                                                   
191510        ,:ACCE-KDMDS                                                      
191520        ,:ACCE-KDFRPTYP                                                   
191530        ,:ACCE-TEARTUTFG                                                  
191540        ,:ACCE-TESTATUPP                                                  
191550        ,:ACCE-IDLEVNR-GSDB                                               
191560        ,:ACCE-KDTPD-PH1                                                  
191570        ,:ACCE-DATPDPH1                                                   
191580        ,:ACCE-DAPSWQP-1                                                  
191590        ,:ACCE-KDPSWQP-1                                                  
191591        ,:ACCE-DAPSWQA-1                                                  
191592        ,:ACCE-KDPSWQA-1                                                  
191593        ,:ACCE-DAPSWPP-2                                                  
191594        ,:ACCE-KDPSWPP-2                                                  
191595        ,:ACCE-DAPSWPA-2                                                  
191596        ,:ACCE-KDPSWPA-2                                                  
191597        ,:ACCE-DAPSWCP-3                                                  
191598        ,:ACCE-KDPSWCP-3                                                  
191599        ,:ACCE-DAPSWCA-3                                                  
191600        ,:ACCE-KDPSWCA-3                                                  
191601        ,:ACCE-KVYVOL-B3                                                  
191602        ,:ACCE-KVYVOL-B2                                                  
191604        ,:ACCE-KVYVOL-INT                                                 
191605        ,:ACCE-KVYVOL-B1                                                  
191606        ,:ACCE-KVYVOL-ASS                                                 
191610        ,:ACCE-BEMAPP                                                     
191611        ,:ACCE-KVFOTO                                                     
191612        ,:ACCE-TIFOTO                                                     
191613        ,:ACCE-TENOTE                                                     
191614        ,:ACCE-FLANNULL                                                   
191615        ,:ACCE-TEVERKTYG                                                  
191616        ,:ACCE-TESTATXT                                                   
191617        ,:ACCE-TEMATXT                                                    
191618        ,:ACCE-TEINKTXT                                                   
191619        ,:ACCE-TEANSTXT                                                   
191620        ,:ACCE-TEAUXTXT                                                   
191621        ,:ACCE-IDARTNR-OFARG                                              
191622        ,:ACCE-IDPSLAG                                                    
191623        ,:ACCE-BETYP                                                      
191624        ,:ACCE-IDFKNGRP                                                   
191625        ,:ACCE-IDKDPPOS                                                   
191626        ,:ACCE-IDAOTUTG                                                   
191627        ,:ACCE-BEANST-KU                                                  
191628        ,:ACCE-BEANST-SU                                                  
191629        ,:ACCE-IDPSS                                                      
191630        ,:ACCE-DAPSWQP-1                                                  
191631        ,:ACCE-KDPSWQP-1                                                  
191632        ,:ACCE-DAPSWPP-2                                                  
191633        ,:ACCE-KDPSWPP-2                                                  
191634        ,:ACCE-DAPSWCP-3                                                  
191635        ,:ACCE-KDPSWCP-3                                                  
191636        ,:ACCE-KDFARGST                                                   
191637        ,:ACCE-IDPROJK                                                    
191638        ,:ACCE-VKART-KDP                                                  
191639        ,:ACCE-KDANNULL                                                   
191640        ,:ACCE-BEUPPDSU                                                   
191650     END-EXEC                                                             
191700     MOVE SQLCODE TO SQLCODE-WS                                           
191800     PERFORM DB2-STATUS-CONTROL                                           
191900     .                                                                    
192000     EJECT                                                                
192100                                                                          
192232                                                                          
192240 DB2-FETCH-TB1ACCE-CRS-TYP-4  SECTION.                                    
192300     SKIP2                                                                
192400     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-4  ' TO DB2-SEKTION                  
192500     MOVE 000100  TO GODK-SQLCODEKODER                                    
192600     EXEC SQL                                                             
192700         FETCH TB1ACCE-CRS-4                                              
192800         INTO                                                             
192900         :ACCE-IDARTNR                                                    
193000        ,:ACCE-BEART                                                      
193100        ,:ACCE-IDPRODGR                                                   
193200        ,:ACCE-IDUPPDSU                                                   
193300        ,:ACCE-IDUPPDKU                                                   
193400        ,:ACCE-IDAOT                                                      
193500        ,:ACCE-TIAOINF                                                    
193600        ,:ACCE-BEASSTYP                                                   
193700        ,:ACCE-KDARTTYP                                                   
193810        ,:ACCE-KDMDS                                                      
193820        ,:ACCE-KDFRPTYP                                                   
193830        ,:ACCE-TEARTUTFG                                                  
193840        ,:ACCE-TESTATUPP                                                  
193850        ,:ACCE-IDLEVNR-GSDB                                               
193860        ,:ACCE-KDTPD-PH1                                                  
193870        ,:ACCE-DATPDPH1                                                   
193880        ,:ACCE-DAPSWQP-1                                                  
193890        ,:ACCE-KDPSWQP-1                                                  
193891        ,:ACCE-DAPSWQA-1                                                  
193892        ,:ACCE-KDPSWQA-1                                                  
193893        ,:ACCE-DAPSWPP-2                                                  
193894        ,:ACCE-KDPSWPP-2                                                  
193895        ,:ACCE-DAPSWPA-2                                                  
193896        ,:ACCE-KDPSWPA-2                                                  
193897        ,:ACCE-DAPSWCP-3                                                  
193898        ,:ACCE-KDPSWCP-3                                                  
193899        ,:ACCE-DAPSWCA-3                                                  
193900        ,:ACCE-KDPSWCA-3                                                  
193901        ,:ACCE-KVYVOL-B3                                                  
193902        ,:ACCE-KVYVOL-B2                                                  
193904        ,:ACCE-KVYVOL-INT                                                 
193905        ,:ACCE-KVYVOL-B1                                                  
193906        ,:ACCE-KVYVOL-ASS                                                 
193910        ,:ACCE-BEMAPP                                                     
193911        ,:ACCE-KVFOTO                                                     
193912        ,:ACCE-TIFOTO                                                     
193913        ,:ACCE-TENOTE                                                     
193914        ,:ACCE-FLANNULL                                                   
193915        ,:ACCE-TEVERKTYG                                                  
193916        ,:ACCE-TESTATXT                                                   
193917        ,:ACCE-TEMATXT                                                    
193918        ,:ACCE-TEINKTXT                                                   
193919        ,:ACCE-TEANSTXT                                                   
193920        ,:ACCE-TEAUXTXT                                                   
193921        ,:ACCE-IDARTNR-OFARG                                              
193922        ,:ACCE-IDPSLAG                                                    
193923        ,:ACCE-BETYP                                                      
193924        ,:ACCE-IDFKNGRP                                                   
193925        ,:ACCE-IDKDPPOS                                                   
193926        ,:ACCE-IDAOTUTG                                                   
193927        ,:ACCE-BEANST-KU                                                  
193928        ,:ACCE-BEANST-SU                                                  
193929        ,:ACCE-IDPSS                                                      
193930        ,:ACCE-DAPSWQP-1                                                  
193931        ,:ACCE-KDPSWQP-1                                                  
193932        ,:ACCE-DAPSWPP-2                                                  
193933        ,:ACCE-KDPSWPP-2                                                  
193934        ,:ACCE-DAPSWCP-3                                                  
193935        ,:ACCE-KDPSWCP-3                                                  
193936        ,:ACCE-KDFARGST                                                   
193937        ,:ACCE-IDPROJK                                                    
193938        ,:ACCE-VKART-KDP                                                  
193939        ,:ACCE-KDANNULL                                                   
193940        ,:ACCE-BEUPPDSU                                                   
193950     END-EXEC                                                             
194000     MOVE SQLCODE TO SQLCODE-WS                                           
194100     PERFORM DB2-STATUS-CONTROL                                           
194200     .                                                                    
194300     EJECT                                                                
194400                                                                          
194500 DB2-FETCH-TB1ACCE-CRS-TYP-5  SECTION.                                    
194600     SKIP2                                                                
194700     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-5  ' TO DB2-SEKTION                  
194800     MOVE 000100  TO GODK-SQLCODEKODER                                    
194900     EXEC SQL                                                             
195000         FETCH TB1ACCE-CRS-5                                              
195100         INTO                                                             
195200         :ACCE-IDARTNR                                                    
195300        ,:ACCE-BEART                                                      
195400        ,:ACCE-IDPRODGR                                                   
195500        ,:ACCE-IDUPPDSU                                                   
195600        ,:ACCE-IDUPPDKU                                                   
195700        ,:ACCE-IDAOT                                                      
195800        ,:ACCE-TIAOINF                                                    
195900        ,:ACCE-BEASSTYP                                                   
196000        ,:ACCE-KDARTTYP                                                   
196110        ,:ACCE-KDMDS                                                      
196120        ,:ACCE-KDFRPTYP                                                   
196130        ,:ACCE-TEARTUTFG                                                  
196140        ,:ACCE-TESTATUPP                                                  
196150        ,:ACCE-IDLEVNR-GSDB                                               
196160        ,:ACCE-KDTPD-PH1                                                  
196170        ,:ACCE-DATPDPH1                                                   
196180        ,:ACCE-DAPSWQP-1                                                  
196190        ,:ACCE-KDPSWQP-1                                                  
196191        ,:ACCE-DAPSWQA-1                                                  
196192        ,:ACCE-KDPSWQA-1                                                  
196193        ,:ACCE-DAPSWPP-2                                                  
196194        ,:ACCE-KDPSWPP-2                                                  
196195        ,:ACCE-DAPSWPA-2                                                  
196196        ,:ACCE-KDPSWPA-2                                                  
196197        ,:ACCE-DAPSWCP-3                                                  
196198        ,:ACCE-KDPSWCP-3                                                  
196199        ,:ACCE-DAPSWCA-3                                                  
196200        ,:ACCE-KDPSWCA-3                                                  
196201        ,:ACCE-KVYVOL-B3                                                  
196202        ,:ACCE-KVYVOL-B2                                                  
196204        ,:ACCE-KVYVOL-INT                                                 
196205        ,:ACCE-KVYVOL-B1                                                  
196206        ,:ACCE-KVYVOL-ASS                                                 
196210        ,:ACCE-BEMAPP                                                     
196211        ,:ACCE-KVFOTO                                                     
196212        ,:ACCE-TIFOTO                                                     
196213        ,:ACCE-TENOTE                                                     
196214        ,:ACCE-FLANNULL                                                   
196215        ,:ACCE-TEVERKTYG                                                  
196216        ,:ACCE-TESTATXT                                                   
196217        ,:ACCE-TEMATXT                                                    
196218        ,:ACCE-TEINKTXT                                                   
196219        ,:ACCE-TEANSTXT                                                   
196220        ,:ACCE-TEAUXTXT                                                   
196221        ,:ACCE-IDARTNR-OFARG                                              
196222        ,:ACCE-IDPSLAG                                                    
196223        ,:ACCE-BETYP                                                      
196224        ,:ACCE-IDFKNGRP                                                   
196225        ,:ACCE-IDKDPPOS                                                   
196226        ,:ACCE-IDAOTUTG                                                   
196227        ,:ACCE-BEANST-KU                                                  
196228        ,:ACCE-BEANST-SU                                                  
196229        ,:ACCE-IDPSS                                                      
196230        ,:ACCE-DAPSWQP-1                                                  
196231        ,:ACCE-KDPSWQP-1                                                  
196232        ,:ACCE-DAPSWPP-2                                                  
196233        ,:ACCE-KDPSWPP-2                                                  
196234        ,:ACCE-DAPSWCP-3                                                  
196235        ,:ACCE-KDPSWCP-3                                                  
196236        ,:ACCE-KDFARGST                                                   
196237        ,:ACCE-IDPROJK                                                    
196238        ,:ACCE-VKART-KDP                                                  
196239        ,:ACCE-KDANNULL                                                   
196240        ,:ACCE-BEUPPDSU                                                   
196250     END-EXEC                                                             
196300     MOVE SQLCODE TO SQLCODE-WS                                           
196400     PERFORM DB2-STATUS-CONTROL                                           
196500     .                                                                    
196600     EJECT                                                                
196700                                                                          
196710 DB2-FETCH-TB1ACCE-CRS-TYP-6  SECTION.                                    
196720     SKIP2                                                                
196730     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-6  ' TO DB2-SEKTION                  
196740     MOVE 000100  TO GODK-SQLCODEKODER                                    
196750     EXEC SQL                                                             
196760         FETCH TB1ACCE-CRS-6                                              
196770         INTO                                                             
196780         :ACCE-IDARTNR                                                    
196790        ,:ACCE-BEART                                                      
196791        ,:ACCE-IDPRODGR                                                   
196792        ,:ACCE-IDUPPDSU                                                   
196793        ,:ACCE-IDUPPDKU                                                   
196794        ,:ACCE-IDAOT                                                      
196795        ,:ACCE-TIAOINF                                                    
196796        ,:ACCE-BEASSTYP                                                   
196797        ,:ACCE-KDARTTYP                                                   
196798        ,:ACCE-KDMDS                                                      
196799        ,:ACCE-KDFRPTYP                                                   
196800        ,:ACCE-TEARTUTFG                                                  
196801        ,:ACCE-TESTATUPP                                                  
196802        ,:ACCE-IDLEVNR-GSDB                                               
196803        ,:ACCE-KDTPD-PH1                                                  
196804        ,:ACCE-DATPDPH1                                                   
196805        ,:ACCE-DAPSWQP-1                                                  
196806        ,:ACCE-KDPSWQP-1                                                  
196807        ,:ACCE-DAPSWQA-1                                                  
196808        ,:ACCE-KDPSWQA-1                                                  
196809        ,:ACCE-DAPSWPP-2                                                  
196810        ,:ACCE-KDPSWPP-2                                                  
196811        ,:ACCE-DAPSWPA-2                                                  
196812        ,:ACCE-KDPSWPA-2                                                  
196813        ,:ACCE-DAPSWCP-3                                                  
196814        ,:ACCE-KDPSWCP-3                                                  
196815        ,:ACCE-DAPSWCA-3                                                  
196816        ,:ACCE-KDPSWCA-3                                                  
196817        ,:ACCE-KVYVOL-B3                                                  
196818        ,:ACCE-KVYVOL-B2                                                  
196819        ,:ACCE-KVYVOL-INT                                                 
196820        ,:ACCE-KVYVOL-B1                                                  
196821        ,:ACCE-KVYVOL-ASS                                                 
196822        ,:ACCE-BEMAPP                                                     
196823        ,:ACCE-KVFOTO                                                     
196824        ,:ACCE-TIFOTO                                                     
196825        ,:ACCE-TENOTE                                                     
196826        ,:ACCE-FLANNULL                                                   
196827        ,:ACCE-TEVERKTYG                                                  
196828        ,:ACCE-TESTATXT                                                   
196829        ,:ACCE-TEMATXT                                                    
196830        ,:ACCE-TEINKTXT                                                   
196831        ,:ACCE-TEANSTXT                                                   
196832        ,:ACCE-TEAUXTXT                                                   
196833        ,:ACCE-IDARTNR-OFARG                                              
196834        ,:ACCE-IDPSLAG                                                    
196835        ,:ACCE-BETYP                                                      
196836        ,:ACCE-IDFKNGRP                                                   
196837        ,:ACCE-IDKDPPOS                                                   
196838        ,:ACCE-IDAOTUTG                                                   
196839        ,:ACCE-BEANST-KU                                                  
196840        ,:ACCE-BEANST-SU                                                  
196841        ,:ACCE-IDPSS                                                      
196842        ,:ACCE-DAPSWQP-1                                                  
196843        ,:ACCE-KDPSWQP-1                                                  
196844        ,:ACCE-DAPSWPP-2                                                  
196845        ,:ACCE-KDPSWPP-2                                                  
196846        ,:ACCE-DAPSWCP-3                                                  
196847        ,:ACCE-KDPSWCP-3                                                  
196848        ,:ACCE-KDFARGST                                                   
196849        ,:ACCE-IDPROJK                                                    
196850        ,:ACCE-VKART-KDP                                                  
196851        ,:ACCE-KDANNULL                                                   
196852        ,:ACCE-BEUPPDSU                                                   
196853     END-EXEC                                                             
196854     MOVE SQLCODE TO SQLCODE-WS                                           
196855     PERFORM DB2-STATUS-CONTROL                                           
196856     .                                                                    
196857     EJECT                                                                
196858                                                                          
196859 DB2-FETCH-TB1ACCE-CRS-TYP-7  SECTION.                                    
196860     SKIP2                                                                
196861     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-7  ' TO DB2-SEKTION                  
196862     MOVE 000100  TO GODK-SQLCODEKODER                                    
196863     EXEC SQL                                                             
196864         FETCH TB1ACCE-CRS-7                                              
196865         INTO                                                             
196866         :ACCE-IDARTNR                                                    
196867        ,:ACCE-BEART                                                      
196868        ,:ACCE-IDPRODGR                                                   
196869        ,:ACCE-IDUPPDSU                                                   
196870        ,:ACCE-IDUPPDKU                                                   
196871        ,:ACCE-IDAOT                                                      
196872        ,:ACCE-TIAOINF                                                    
196873        ,:ACCE-BEASSTYP                                                   
196874        ,:ACCE-KDARTTYP                                                   
196875        ,:ACCE-KDMDS                                                      
196876        ,:ACCE-KDFRPTYP                                                   
196877        ,:ACCE-TEARTUTFG                                                  
196878        ,:ACCE-TESTATUPP                                                  
196879        ,:ACCE-IDLEVNR-GSDB                                               
196880        ,:ACCE-KDTPD-PH1                                                  
196881        ,:ACCE-DATPDPH1                                                   
196882        ,:ACCE-DAPSWQP-1                                                  
196883        ,:ACCE-KDPSWQP-1                                                  
196884        ,:ACCE-DAPSWQA-1                                                  
196885        ,:ACCE-KDPSWQA-1                                                  
196886        ,:ACCE-DAPSWPP-2                                                  
196887        ,:ACCE-KDPSWPP-2                                                  
196888        ,:ACCE-DAPSWPA-2                                                  
196889        ,:ACCE-KDPSWPA-2                                                  
196890        ,:ACCE-DAPSWCP-3                                                  
196891        ,:ACCE-KDPSWCP-3                                                  
196892        ,:ACCE-DAPSWCA-3                                                  
196893        ,:ACCE-KDPSWCA-3                                                  
196894        ,:ACCE-KVYVOL-B3                                                  
196895        ,:ACCE-KVYVOL-B2                                                  
196896        ,:ACCE-KVYVOL-INT                                                 
196897        ,:ACCE-KVYVOL-B1                                                  
196898        ,:ACCE-KVYVOL-ASS                                                 
196899        ,:ACCE-BEMAPP                                                     
196900        ,:ACCE-KVFOTO                                                     
196901        ,:ACCE-TIFOTO                                                     
196902        ,:ACCE-TENOTE                                                     
196903        ,:ACCE-FLANNULL                                                   
196904        ,:ACCE-TEVERKTYG                                                  
196905        ,:ACCE-TESTATXT                                                   
196906        ,:ACCE-TEMATXT                                                    
196907        ,:ACCE-TEINKTXT                                                   
196908        ,:ACCE-TEANSTXT                                                   
196909        ,:ACCE-TEAUXTXT                                                   
196910        ,:ACCE-IDARTNR-OFARG                                              
196911        ,:ACCE-IDPSLAG                                                    
196912        ,:ACCE-BETYP                                                      
196913        ,:ACCE-IDFKNGRP                                                   
196914        ,:ACCE-IDKDPPOS                                                   
196915        ,:ACCE-IDAOTUTG                                                   
196916        ,:ACCE-BEANST-KU                                                  
196917        ,:ACCE-BEANST-SU                                                  
196918        ,:ACCE-IDPSS                                                      
196919        ,:ACCE-DAPSWQP-1                                                  
196920        ,:ACCE-KDPSWQP-1                                                  
196921        ,:ACCE-DAPSWPP-2                                                  
196922        ,:ACCE-KDPSWPP-2                                                  
196923        ,:ACCE-DAPSWCP-3                                                  
196924        ,:ACCE-KDPSWCP-3                                                  
196925        ,:ACCE-KDFARGST                                                   
196926        ,:ACCE-IDPROJK                                                    
196927        ,:ACCE-VKART-KDP                                                  
196928        ,:ACCE-KDANNULL                                                   
196929        ,:ACCE-BEUPPDSU                                                   
196930     END-EXEC                                                             
196931     MOVE SQLCODE TO SQLCODE-WS                                           
196932     PERFORM DB2-STATUS-CONTROL                                           
196933     .                                                                    
196934     EJECT                                                                
196935 DB2-FETCH-TB1ACCE-CRS-TYP-8  SECTION.                                    
196940     SKIP2                                                                
197000     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-8  ' TO DB2-SEKTION                  
197100     MOVE 000100  TO GODK-SQLCODEKODER                                    
197200     EXEC SQL                                                             
197300         FETCH TB1ACCE-CRS-8                                              
197400         INTO                                                             
197500         :ACCE-IDARTNR                                                    
197600        ,:ACCE-BEART                                                      
197700        ,:ACCE-IDPRODGR                                                   
197800        ,:ACCE-IDUPPDSU                                                   
197900        ,:ACCE-IDUPPDKU                                                   
198000        ,:ACCE-IDAOT                                                      
198100        ,:ACCE-TIAOINF                                                    
198200        ,:ACCE-BEASSTYP                                                   
198300        ,:ACCE-KDARTTYP                                                   
198410        ,:ACCE-KDMDS                                                      
198420        ,:ACCE-KDFRPTYP                                                   
198430        ,:ACCE-TEARTUTFG                                                  
198440        ,:ACCE-TESTATUPP                                                  
198450        ,:ACCE-IDLEVNR-GSDB                                               
198460        ,:ACCE-KDTPD-PH1                                                  
198470        ,:ACCE-DATPDPH1                                                   
198480        ,:ACCE-DAPSWQP-1                                                  
198490        ,:ACCE-KDPSWQP-1                                                  
198491        ,:ACCE-DAPSWQA-1                                                  
198492        ,:ACCE-KDPSWQA-1                                                  
198493        ,:ACCE-DAPSWPP-2                                                  
198494        ,:ACCE-KDPSWPP-2                                                  
198495        ,:ACCE-DAPSWPA-2                                                  
198496        ,:ACCE-KDPSWPA-2                                                  
198497        ,:ACCE-DAPSWCP-3                                                  
198498        ,:ACCE-KDPSWCP-3                                                  
198499        ,:ACCE-DAPSWCA-3                                                  
198500        ,:ACCE-KDPSWCA-3                                                  
198501        ,:ACCE-KVYVOL-B3                                                  
198502        ,:ACCE-KVYVOL-B2                                                  
198504        ,:ACCE-KVYVOL-INT                                                 
198505        ,:ACCE-KVYVOL-B1                                                  
198506        ,:ACCE-KVYVOL-ASS                                                 
198510        ,:ACCE-BEMAPP                                                     
198511        ,:ACCE-KVFOTO                                                     
198512        ,:ACCE-TIFOTO                                                     
198513        ,:ACCE-TENOTE                                                     
198514        ,:ACCE-FLANNULL                                                   
198515        ,:ACCE-TEVERKTYG                                                  
198516        ,:ACCE-TESTATXT                                                   
198517        ,:ACCE-TEMATXT                                                    
198518        ,:ACCE-TEINKTXT                                                   
198519        ,:ACCE-TEANSTXT                                                   
198520        ,:ACCE-TEAUXTXT                                                   
198521        ,:ACCE-IDARTNR-OFARG                                              
198522        ,:ACCE-IDPSLAG                                                    
198523        ,:ACCE-BETYP                                                      
198524        ,:ACCE-IDFKNGRP                                                   
198525        ,:ACCE-IDKDPPOS                                                   
198526        ,:ACCE-IDAOTUTG                                                   
198527        ,:ACCE-BEANST-KU                                                  
198528        ,:ACCE-BEANST-SU                                                  
198529        ,:ACCE-IDPSS                                                      
198530        ,:ACCE-DAPSWQP-1                                                  
198531        ,:ACCE-KDPSWQP-1                                                  
198532        ,:ACCE-DAPSWPP-2                                                  
198533        ,:ACCE-KDPSWPP-2                                                  
198534        ,:ACCE-DAPSWCP-3                                                  
198535        ,:ACCE-KDPSWCP-3                                                  
198536        ,:ACCE-KDFARGST                                                   
198537        ,:ACCE-IDPROJK                                                    
198538        ,:ACCE-VKART-KDP                                                  
198539        ,:ACCE-KDANNULL                                                   
198540        ,:ACCE-BEUPPDSU                                                   
198550     END-EXEC                                                             
198600     MOVE SQLCODE TO SQLCODE-WS                                           
198700     PERFORM DB2-STATUS-CONTROL                                           
198800     .                                                                    
198900     EJECT                                                                
199000                                                                          
199100 DB2-FETCH-TB1ACCE-CRS-TYP-9  SECTION.                                    
199200     SKIP2                                                                
199300     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-9  ' TO DB2-SEKTION                  
199400     MOVE 000100  TO GODK-SQLCODEKODER                                    
199500     EXEC SQL                                                             
199600         FETCH TB1ACCE-CRS-9                                              
199700         INTO                                                             
199800         :ACCE-IDARTNR                                                    
199900        ,:ACCE-BEART                                                      
200000        ,:ACCE-IDPRODGR                                                   
200100        ,:ACCE-IDUPPDSU                                                   
200200        ,:ACCE-IDUPPDKU                                                   
200300        ,:ACCE-IDAOT                                                      
200400        ,:ACCE-TIAOINF                                                    
200500        ,:ACCE-BEASSTYP                                                   
200600        ,:ACCE-KDARTTYP                                                   
200710        ,:ACCE-KDMDS                                                      
200720        ,:ACCE-KDFRPTYP                                                   
200730        ,:ACCE-TEARTUTFG                                                  
200740        ,:ACCE-TESTATUPP                                                  
200750        ,:ACCE-IDLEVNR-GSDB                                               
200760        ,:ACCE-KDTPD-PH1                                                  
200770        ,:ACCE-DATPDPH1                                                   
200780        ,:ACCE-DAPSWQP-1                                                  
200790        ,:ACCE-KDPSWQP-1                                                  
200791        ,:ACCE-DAPSWQA-1                                                  
200792        ,:ACCE-KDPSWQA-1                                                  
200793        ,:ACCE-DAPSWPP-2                                                  
200794        ,:ACCE-KDPSWPP-2                                                  
200795        ,:ACCE-DAPSWPA-2                                                  
200796        ,:ACCE-KDPSWPA-2                                                  
200797        ,:ACCE-DAPSWCP-3                                                  
200798        ,:ACCE-KDPSWCP-3                                                  
200799        ,:ACCE-DAPSWCA-3                                                  
200800        ,:ACCE-KDPSWCA-3                                                  
200801        ,:ACCE-KVYVOL-B3                                                  
200802        ,:ACCE-KVYVOL-B2                                                  
200804        ,:ACCE-KVYVOL-INT                                                 
200805        ,:ACCE-KVYVOL-B1                                                  
200806        ,:ACCE-KVYVOL-ASS                                                 
200810        ,:ACCE-BEMAPP                                                     
200811        ,:ACCE-KVFOTO                                                     
200812        ,:ACCE-TIFOTO                                                     
200813        ,:ACCE-TENOTE                                                     
200814        ,:ACCE-FLANNULL                                                   
200815        ,:ACCE-TEVERKTYG                                                  
200816        ,:ACCE-TESTATXT                                                   
200817        ,:ACCE-TEMATXT                                                    
200818        ,:ACCE-TEINKTXT                                                   
200819        ,:ACCE-TEANSTXT                                                   
200820        ,:ACCE-TEAUXTXT                                                   
200821        ,:ACCE-IDARTNR-OFARG                                              
200822        ,:ACCE-IDPSLAG                                                    
200823        ,:ACCE-BETYP                                                      
200824        ,:ACCE-IDFKNGRP                                                   
200825        ,:ACCE-IDKDPPOS                                                   
200826        ,:ACCE-IDAOTUTG                                                   
200827        ,:ACCE-BEANST-KU                                                  
200828        ,:ACCE-BEANST-SU                                                  
200829        ,:ACCE-IDPSS                                                      
200830        ,:ACCE-DAPSWQP-1                                                  
200831        ,:ACCE-KDPSWQP-1                                                  
200832        ,:ACCE-DAPSWPP-2                                                  
200833        ,:ACCE-KDPSWPP-2                                                  
200834        ,:ACCE-DAPSWCP-3                                                  
200835        ,:ACCE-KDPSWCP-3                                                  
200836        ,:ACCE-KDFARGST                                                   
200837        ,:ACCE-IDPROJK                                                    
200838        ,:ACCE-VKART-KDP                                                  
200839        ,:ACCE-KDANNULL                                                   
200840        ,:ACCE-BEUPPDSU                                                   
200850     END-EXEC                                                             
200900     MOVE SQLCODE TO SQLCODE-WS                                           
201000     PERFORM DB2-STATUS-CONTROL                                           
201100     .                                                                    
201200     EJECT                                                                
201300                                                                          
201400 DB2-FETCH-TB1ACCE-CRS-TYP-10 SECTION.                                    
201500     SKIP2                                                                
201600     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-10 ' TO DB2-SEKTION                  
201700     MOVE 000100  TO GODK-SQLCODEKODER                                    
201800     EXEC SQL                                                             
201900         FETCH TB1ACCE-CRS-10                                             
202000         INTO                                                             
202100         :ACCE-IDARTNR                                                    
202200        ,:ACCE-BEART                                                      
202300        ,:ACCE-IDPRODGR                                                   
202400        ,:ACCE-IDUPPDSU                                                   
202500        ,:ACCE-IDUPPDKU                                                   
202600        ,:ACCE-IDAOT                                                      
202700        ,:ACCE-TIAOINF                                                    
202800        ,:ACCE-BEASSTYP                                                   
202900        ,:ACCE-KDARTTYP                                                   
203100        ,:ACCE-KDMDS                                                      
203200        ,:ACCE-KDFRPTYP                                                   
203300        ,:ACCE-TEARTUTFG                                                  
203400        ,:ACCE-TESTATUPP                                                  
203500        ,:ACCE-IDLEVNR-GSDB                                               
203600        ,:ACCE-KDTPD-PH1                                                  
203700        ,:ACCE-DATPDPH1                                                   
203800        ,:ACCE-DAPSWQP-1                                                  
203900        ,:ACCE-KDPSWQP-1                                                  
204000        ,:ACCE-DAPSWQA-1                                                  
204100        ,:ACCE-KDPSWQA-1                                                  
204200        ,:ACCE-DAPSWPP-2                                                  
204300        ,:ACCE-KDPSWPP-2                                                  
204400        ,:ACCE-DAPSWPA-2                                                  
204500        ,:ACCE-KDPSWPA-2                                                  
204600        ,:ACCE-DAPSWCP-3                                                  
204700        ,:ACCE-KDPSWCP-3                                                  
204800        ,:ACCE-DAPSWCA-3                                                  
204900        ,:ACCE-KDPSWCA-3                                                  
205000        ,:ACCE-KVYVOL-B3                                                  
205100        ,:ACCE-KVYVOL-B2                                                  
205300        ,:ACCE-KVYVOL-INT                                                 
205400        ,:ACCE-KVYVOL-B1                                                  
205500        ,:ACCE-KVYVOL-ASS                                                 
205900        ,:ACCE-BEMAPP                                                     
206000        ,:ACCE-KVFOTO                                                     
206100        ,:ACCE-TIFOTO                                                     
206200        ,:ACCE-TENOTE                                                     
206300        ,:ACCE-FLANNULL                                                   
206310        ,:ACCE-TEVERKTYG                                                  
206320        ,:ACCE-TESTATXT                                                   
206330        ,:ACCE-TEMATXT                                                    
206340        ,:ACCE-TEINKTXT                                                   
206350        ,:ACCE-TEANSTXT                                                   
206360        ,:ACCE-TEAUXTXT                                                   
206370        ,:ACCE-IDARTNR-OFARG                                              
206380        ,:ACCE-IDPSLAG                                                    
206390        ,:ACCE-BETYP                                                      
206391        ,:ACCE-IDFKNGRP                                                   
206392        ,:ACCE-IDKDPPOS                                                   
206393        ,:ACCE-IDAOTUTG                                                   
206394        ,:ACCE-BEANST-KU                                                  
206395        ,:ACCE-BEANST-SU                                                  
206396        ,:ACCE-IDPSS                                                      
206397        ,:ACCE-DAPSWQP-1                                                  
206398        ,:ACCE-KDPSWQP-1                                                  
206399        ,:ACCE-DAPSWPP-2                                                  
206400        ,:ACCE-KDPSWPP-2                                                  
206401        ,:ACCE-DAPSWCP-3                                                  
206402        ,:ACCE-KDPSWCP-3                                                  
206403        ,:ACCE-KDFARGST                                                   
206404        ,:ACCE-IDPROJK                                                    
206405        ,:ACCE-VKART-KDP                                                  
206406        ,:ACCE-KDANNULL                                                   
206407        ,:ACCE-BEUPPDSU                                                   
206410     END-EXEC                                                             
206500     MOVE SQLCODE TO SQLCODE-WS                                           
206600     PERFORM DB2-STATUS-CONTROL                                           
206700     .                                                                    
206800     EJECT                                                                
206900                                                                          
206910 DB2-FETCH-TB1ACCE-CRS-TYP-11 SECTION.                                    
206920     SKIP2                                                                
206930     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-11 ' TO DB2-SEKTION                  
206940     MOVE 000100  TO GODK-SQLCODEKODER                                    
206950     EXEC SQL                                                             
206960         FETCH TB1ACCE-CRS-11                                             
206970         INTO                                                             
206980         :ACCE-IDARTNR                                                    
206990        ,:ACCE-BEART                                                      
206991        ,:ACCE-IDPRODGR                                                   
206992        ,:ACCE-IDUPPDSU                                                   
206993        ,:ACCE-IDUPPDKU                                                   
206994        ,:ACCE-IDAOT                                                      
206995        ,:ACCE-TIAOINF                                                    
206996        ,:ACCE-BEASSTYP                                                   
206997        ,:ACCE-KDARTTYP                                                   
206998        ,:ACCE-KDMDS                                                      
206999        ,:ACCE-KDFRPTYP                                                   
207000        ,:ACCE-TEARTUTFG                                                  
207001        ,:ACCE-TESTATUPP                                                  
207002        ,:ACCE-IDLEVNR-GSDB                                               
207003        ,:ACCE-KDTPD-PH1                                                  
207004        ,:ACCE-DATPDPH1                                                   
207005        ,:ACCE-DAPSWQP-1                                                  
207006        ,:ACCE-KDPSWQP-1                                                  
207007        ,:ACCE-DAPSWQA-1                                                  
207008        ,:ACCE-KDPSWQA-1                                                  
207009        ,:ACCE-DAPSWPP-2                                                  
207010        ,:ACCE-KDPSWPP-2                                                  
207011        ,:ACCE-DAPSWPA-2                                                  
207012        ,:ACCE-KDPSWPA-2                                                  
207013        ,:ACCE-DAPSWCP-3                                                  
207014        ,:ACCE-KDPSWCP-3                                                  
207015        ,:ACCE-DAPSWCA-3                                                  
207016        ,:ACCE-KDPSWCA-3                                                  
207017        ,:ACCE-KVYVOL-B3                                                  
207018        ,:ACCE-KVYVOL-B2                                                  
207019        ,:ACCE-KVYVOL-INT                                                 
207020        ,:ACCE-KVYVOL-B1                                                  
207021        ,:ACCE-KVYVOL-ASS                                                 
207022        ,:ACCE-BEMAPP                                                     
207023        ,:ACCE-KVFOTO                                                     
207024        ,:ACCE-TIFOTO                                                     
207025        ,:ACCE-TENOTE                                                     
207026        ,:ACCE-FLANNULL                                                   
207027        ,:ACCE-TEVERKTYG                                                  
207028        ,:ACCE-TESTATXT                                                   
207029        ,:ACCE-TEMATXT                                                    
207030        ,:ACCE-TEINKTXT                                                   
207031        ,:ACCE-TEANSTXT                                                   
207032        ,:ACCE-TEAUXTXT                                                   
207033        ,:ACCE-IDARTNR-OFARG                                              
207034        ,:ACCE-IDPSLAG                                                    
207035        ,:ACCE-BETYP                                                      
207036        ,:ACCE-IDFKNGRP                                                   
207037        ,:ACCE-IDKDPPOS                                                   
207038        ,:ACCE-IDAOTUTG                                                   
207039        ,:ACCE-BEANST-KU                                                  
207040        ,:ACCE-BEANST-SU                                                  
207041        ,:ACCE-IDPSS                                                      
207042        ,:ACCE-DAPSWQP-1                                                  
207043        ,:ACCE-KDPSWQP-1                                                  
207044        ,:ACCE-DAPSWPP-2                                                  
207045        ,:ACCE-KDPSWPP-2                                                  
207046        ,:ACCE-DAPSWCP-3                                                  
207047        ,:ACCE-KDPSWCP-3                                                  
207048        ,:ACCE-KDFARGST                                                   
207049        ,:ACCE-IDPROJK                                                    
207050        ,:ACCE-VKART-KDP                                                  
207051        ,:ACCE-KDANNULL                                                   
207052        ,:ACCE-BEUPPDSU                                                   
207053     END-EXEC                                                             
207054     MOVE SQLCODE TO SQLCODE-WS                                           
207055     PERFORM DB2-STATUS-CONTROL                                           
207056     .                                                                    
207057     EJECT                                                                
207058                                                                          
207060 DB2-FETCH-TB1ACCE-CRS-TYP-12 SECTION.                                    
207100     SKIP2                                                                
207200     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-12 ' TO DB2-SEKTION                  
207300     MOVE 000100  TO GODK-SQLCODEKODER                                    
207400     EXEC SQL                                                             
207500         FETCH TB1ACCE-CRS-12                                             
207600         INTO                                                             
207700         :ACCE-IDARTNR                                                    
207800        ,:ACCE-BEART                                                      
207900        ,:ACCE-IDPRODGR                                                   
208000        ,:ACCE-IDUPPDSU                                                   
208100        ,:ACCE-IDUPPDKU                                                   
208200        ,:ACCE-IDAOT                                                      
208300        ,:ACCE-TIAOINF                                                    
208400        ,:ACCE-BEASSTYP                                                   
208500        ,:ACCE-KDARTTYP                                                   
208700        ,:ACCE-KDMDS                                                      
208800        ,:ACCE-KDFRPTYP                                                   
208900        ,:ACCE-TEARTUTFG                                                  
209000        ,:ACCE-TESTATUPP                                                  
209100        ,:ACCE-IDLEVNR-GSDB                                               
209200        ,:ACCE-KDTPD-PH1                                                  
209300        ,:ACCE-DATPDPH1                                                   
209400        ,:ACCE-DAPSWQP-1                                                  
209500        ,:ACCE-KDPSWQP-1                                                  
209600        ,:ACCE-DAPSWQA-1                                                  
209700        ,:ACCE-KDPSWQA-1                                                  
209800        ,:ACCE-DAPSWPP-2                                                  
209900        ,:ACCE-KDPSWPP-2                                                  
210000        ,:ACCE-DAPSWPA-2                                                  
210100        ,:ACCE-KDPSWPA-2                                                  
210200        ,:ACCE-DAPSWCP-3                                                  
210300        ,:ACCE-KDPSWCP-3                                                  
210400        ,:ACCE-DAPSWCA-3                                                  
210500        ,:ACCE-KDPSWCA-3                                                  
210600        ,:ACCE-KVYVOL-B3                                                  
210700        ,:ACCE-KVYVOL-B2                                                  
210900        ,:ACCE-KVYVOL-INT                                                 
211000        ,:ACCE-KVYVOL-B1                                                  
211100        ,:ACCE-KVYVOL-ASS                                                 
211500        ,:ACCE-BEMAPP                                                     
211600        ,:ACCE-KVFOTO                                                     
211700        ,:ACCE-TIFOTO                                                     
211800        ,:ACCE-TENOTE                                                     
211900        ,:ACCE-FLANNULL                                                   
211910        ,:ACCE-TEVERKTYG                                                  
211920        ,:ACCE-TESTATXT                                                   
211930        ,:ACCE-TEMATXT                                                    
211940        ,:ACCE-TEINKTXT                                                   
211950        ,:ACCE-TEANSTXT                                                   
211960        ,:ACCE-TEAUXTXT                                                   
211970        ,:ACCE-IDARTNR-OFARG                                              
211980        ,:ACCE-IDPSLAG                                                    
211990        ,:ACCE-BETYP                                                      
211991        ,:ACCE-IDFKNGRP                                                   
211992        ,:ACCE-IDKDPPOS                                                   
211993        ,:ACCE-IDAOTUTG                                                   
211994        ,:ACCE-BEANST-KU                                                  
211995        ,:ACCE-BEANST-SU                                                  
211996        ,:ACCE-IDPSS                                                      
211997        ,:ACCE-DAPSWQP-1                                                  
211998        ,:ACCE-KDPSWQP-1                                                  
211999        ,:ACCE-DAPSWPP-2                                                  
212000        ,:ACCE-KDPSWPP-2                                                  
212001        ,:ACCE-DAPSWCP-3                                                  
212002        ,:ACCE-KDPSWCP-3                                                  
212003        ,:ACCE-KDFARGST                                                   
212004        ,:ACCE-IDPROJK                                                    
212005        ,:ACCE-VKART-KDP                                                  
212006        ,:ACCE-KDANNULL                                                   
212007        ,:ACCE-BEUPPDSU                                                   
212010     END-EXEC                                                             
212100     MOVE SQLCODE TO SQLCODE-WS                                           
212200     PERFORM DB2-STATUS-CONTROL                                           
212300     .                                                                    
212400     EJECT                                                                
212500                                                                          
212510 DB2-FETCH-TB1ACCE-CRS-TYP-13 SECTION.                                    
212520     SKIP2                                                                
212530     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-13 ' TO DB2-SEKTION                  
212540     MOVE 000100  TO GODK-SQLCODEKODER                                    
212550     EXEC SQL                                                             
212560         FETCH TB1ACCE-CRS-13                                             
212570         INTO                                                             
212580         :ACCE-IDARTNR                                                    
212590        ,:ACCE-BEART                                                      
212591        ,:ACCE-IDPRODGR                                                   
212592        ,:ACCE-IDUPPDSU                                                   
212593        ,:ACCE-IDUPPDKU                                                   
212594        ,:ACCE-IDAOT                                                      
212595        ,:ACCE-TIAOINF                                                    
212596        ,:ACCE-BEASSTYP                                                   
212597        ,:ACCE-KDARTTYP                                                   
212598        ,:ACCE-KDMDS                                                      
212599        ,:ACCE-KDFRPTYP                                                   
212600        ,:ACCE-TEARTUTFG                                                  
212601        ,:ACCE-TESTATUPP                                                  
212602        ,:ACCE-IDLEVNR-GSDB                                               
212603        ,:ACCE-KDTPD-PH1                                                  
212604        ,:ACCE-DATPDPH1                                                   
212605        ,:ACCE-DAPSWQP-1                                                  
212606        ,:ACCE-KDPSWQP-1                                                  
212607        ,:ACCE-DAPSWQA-1                                                  
212608        ,:ACCE-KDPSWQA-1                                                  
212609        ,:ACCE-DAPSWPP-2                                                  
212610        ,:ACCE-KDPSWPP-2                                                  
212611        ,:ACCE-DAPSWPA-2                                                  
212612        ,:ACCE-KDPSWPA-2                                                  
212613        ,:ACCE-DAPSWCP-3                                                  
212614        ,:ACCE-KDPSWCP-3                                                  
212615        ,:ACCE-DAPSWCA-3                                                  
212616        ,:ACCE-KDPSWCA-3                                                  
212617        ,:ACCE-KVYVOL-B3                                                  
212618        ,:ACCE-KVYVOL-B2                                                  
212619        ,:ACCE-KVYVOL-INT                                                 
212620        ,:ACCE-KVYVOL-B1                                                  
212621        ,:ACCE-KVYVOL-ASS                                                 
212622        ,:ACCE-BEMAPP                                                     
212623        ,:ACCE-KVFOTO                                                     
212624        ,:ACCE-TIFOTO                                                     
212625        ,:ACCE-TENOTE                                                     
212626        ,:ACCE-FLANNULL                                                   
212627        ,:ACCE-TEVERKTYG                                                  
212628        ,:ACCE-TESTATXT                                                   
212629        ,:ACCE-TEMATXT                                                    
212630        ,:ACCE-TEINKTXT                                                   
212631        ,:ACCE-TEANSTXT                                                   
212632        ,:ACCE-TEAUXTXT                                                   
212633        ,:ACCE-IDARTNR-OFARG                                              
212634        ,:ACCE-IDPSLAG                                                    
212635        ,:ACCE-BETYP                                                      
212636        ,:ACCE-IDFKNGRP                                                   
212637        ,:ACCE-IDKDPPOS                                                   
212638        ,:ACCE-IDAOTUTG                                                   
212639        ,:ACCE-BEANST-KU                                                  
212640        ,:ACCE-BEANST-SU                                                  
212641        ,:ACCE-IDPSS                                                      
212642        ,:ACCE-DAPSWQP-1                                                  
212643        ,:ACCE-KDPSWQP-1                                                  
212644        ,:ACCE-DAPSWPP-2                                                  
212645        ,:ACCE-KDPSWPP-2                                                  
212646        ,:ACCE-DAPSWCP-3                                                  
212647        ,:ACCE-KDPSWCP-3                                                  
212648        ,:ACCE-KDFARGST                                                   
212649        ,:ACCE-IDPROJK                                                    
212650        ,:ACCE-VKART-KDP                                                  
212651        ,:ACCE-KDANNULL                                                   
212652        ,:ACCE-BEUPPDSU                                                   
212653     END-EXEC                                                             
212654     MOVE SQLCODE TO SQLCODE-WS                                           
212655     PERFORM DB2-STATUS-CONTROL                                           
212656     .                                                                    
212657     EJECT                                                                
212658                                                                          
212659 DB2-FETCH-TB1ACCE-CRS-TYP-14 SECTION.                                    
212660     SKIP2                                                                
212661     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-14 ' TO DB2-SEKTION                  
212662     MOVE 000100  TO GODK-SQLCODEKODER                                    
212663     EXEC SQL                                                             
212664         FETCH TB1ACCE-CRS-14                                             
212665         INTO                                                             
212666         :ACCE-IDARTNR                                                    
212667        ,:ACCE-BEART                                                      
212668        ,:ACCE-IDPRODGR                                                   
212669        ,:ACCE-IDUPPDSU                                                   
212670        ,:ACCE-IDUPPDKU                                                   
212671        ,:ACCE-IDAOT                                                      
212672        ,:ACCE-TIAOINF                                                    
212673        ,:ACCE-BEASSTYP                                                   
212674        ,:ACCE-KDARTTYP                                                   
212675        ,:ACCE-KDMDS                                                      
212676        ,:ACCE-KDFRPTYP                                                   
212677        ,:ACCE-TEARTUTFG                                                  
212678        ,:ACCE-TESTATUPP                                                  
212679        ,:ACCE-IDLEVNR-GSDB                                               
212680        ,:ACCE-KDTPD-PH1                                                  
212681        ,:ACCE-DATPDPH1                                                   
212682        ,:ACCE-DAPSWQP-1                                                  
212683        ,:ACCE-KDPSWQP-1                                                  
212684        ,:ACCE-DAPSWQA-1                                                  
212685        ,:ACCE-KDPSWQA-1                                                  
212686        ,:ACCE-DAPSWPP-2                                                  
212687        ,:ACCE-KDPSWPP-2                                                  
212688        ,:ACCE-DAPSWPA-2                                                  
212689        ,:ACCE-KDPSWPA-2                                                  
212690        ,:ACCE-DAPSWCP-3                                                  
212691        ,:ACCE-KDPSWCP-3                                                  
212692        ,:ACCE-DAPSWCA-3                                                  
212693        ,:ACCE-KDPSWCA-3                                                  
212694        ,:ACCE-KVYVOL-B3                                                  
212695        ,:ACCE-KVYVOL-B2                                                  
212696        ,:ACCE-KVYVOL-INT                                                 
212697        ,:ACCE-KVYVOL-B1                                                  
212698        ,:ACCE-KVYVOL-ASS                                                 
212699        ,:ACCE-BEMAPP                                                     
212700        ,:ACCE-KVFOTO                                                     
212701        ,:ACCE-TIFOTO                                                     
212702        ,:ACCE-TENOTE                                                     
212703        ,:ACCE-FLANNULL                                                   
212704        ,:ACCE-TEVERKTYG                                                  
212705        ,:ACCE-TESTATXT                                                   
212706        ,:ACCE-TEMATXT                                                    
212707        ,:ACCE-TEINKTXT                                                   
212708        ,:ACCE-TEANSTXT                                                   
212709        ,:ACCE-TEAUXTXT                                                   
212710        ,:ACCE-IDARTNR-OFARG                                              
212711        ,:ACCE-IDPSLAG                                                    
212712        ,:ACCE-BETYP                                                      
212713        ,:ACCE-IDFKNGRP                                                   
212714        ,:ACCE-IDKDPPOS                                                   
212715        ,:ACCE-IDAOTUTG                                                   
212716        ,:ACCE-BEANST-KU                                                  
212717        ,:ACCE-BEANST-SU                                                  
212718        ,:ACCE-IDPSS                                                      
212719        ,:ACCE-DAPSWQP-1                                                  
212720        ,:ACCE-KDPSWQP-1                                                  
212721        ,:ACCE-DAPSWPP-2                                                  
212722        ,:ACCE-KDPSWPP-2                                                  
212723        ,:ACCE-DAPSWCP-3                                                  
212724        ,:ACCE-KDPSWCP-3                                                  
212725        ,:ACCE-KDFARGST                                                   
212726        ,:ACCE-IDPROJK                                                    
212727        ,:ACCE-VKART-KDP                                                  
212728        ,:ACCE-KDANNULL                                                   
212729        ,:ACCE-BEUPPDSU                                                   
212730     END-EXEC                                                             
212731     MOVE SQLCODE TO SQLCODE-WS                                           
212732     PERFORM DB2-STATUS-CONTROL                                           
212733     .                                                                    
212734     EJECT                                                                
212735                                                                          
212736 DB2-FETCH-TB1ACCE-CRS-TYP-15 SECTION.                                    
212737     SKIP2                                                                
212738     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-15 ' TO DB2-SEKTION                  
212739     MOVE 000100  TO GODK-SQLCODEKODER                                    
212740     EXEC SQL                                                             
212741         FETCH TB1ACCE-CRS-15                                             
212742         INTO                                                             
212743         :ACCE-IDARTNR                                                    
212744        ,:ACCE-BEART                                                      
212745        ,:ACCE-IDPRODGR                                                   
212746        ,:ACCE-IDUPPDSU                                                   
212747        ,:ACCE-IDUPPDKU                                                   
212748        ,:ACCE-IDAOT                                                      
212749        ,:ACCE-TIAOINF                                                    
212750        ,:ACCE-BEASSTYP                                                   
212751        ,:ACCE-KDARTTYP                                                   
212752        ,:ACCE-KDMDS                                                      
212753        ,:ACCE-KDFRPTYP                                                   
212754        ,:ACCE-TEARTUTFG                                                  
212755        ,:ACCE-TESTATUPP                                                  
212756        ,:ACCE-IDLEVNR-GSDB                                               
212757        ,:ACCE-KDTPD-PH1                                                  
212758        ,:ACCE-DATPDPH1                                                   
212759        ,:ACCE-DAPSWQP-1                                                  
212760        ,:ACCE-KDPSWQP-1                                                  
212761        ,:ACCE-DAPSWQA-1                                                  
212762        ,:ACCE-KDPSWQA-1                                                  
212763        ,:ACCE-DAPSWPP-2                                                  
212764        ,:ACCE-KDPSWPP-2                                                  
212765        ,:ACCE-DAPSWPA-2                                                  
212766        ,:ACCE-KDPSWPA-2                                                  
212767        ,:ACCE-DAPSWCP-3                                                  
212768        ,:ACCE-KDPSWCP-3                                                  
212769        ,:ACCE-DAPSWCA-3                                                  
212770        ,:ACCE-KDPSWCA-3                                                  
212771        ,:ACCE-KVYVOL-B3                                                  
212772        ,:ACCE-KVYVOL-B2                                                  
212773        ,:ACCE-KVYVOL-INT                                                 
212774        ,:ACCE-KVYVOL-B1                                                  
212775        ,:ACCE-KVYVOL-ASS                                                 
212776        ,:ACCE-BEMAPP                                                     
212777        ,:ACCE-KVFOTO                                                     
212778        ,:ACCE-TIFOTO                                                     
212779        ,:ACCE-TENOTE                                                     
212780        ,:ACCE-FLANNULL                                                   
212781        ,:ACCE-TEVERKTYG                                                  
212782        ,:ACCE-TESTATXT                                                   
212783        ,:ACCE-TEMATXT                                                    
212784        ,:ACCE-TEINKTXT                                                   
212785        ,:ACCE-TEANSTXT                                                   
212786        ,:ACCE-TEAUXTXT                                                   
212787        ,:ACCE-IDARTNR-OFARG                                              
212788        ,:ACCE-IDPSLAG                                                    
212789        ,:ACCE-BETYP                                                      
212790        ,:ACCE-IDFKNGRP                                                   
212791        ,:ACCE-IDKDPPOS                                                   
212792        ,:ACCE-IDAOTUTG                                                   
212793        ,:ACCE-BEANST-KU                                                  
212794        ,:ACCE-BEANST-SU                                                  
212795        ,:ACCE-IDPSS                                                      
212796        ,:ACCE-DAPSWQP-1                                                  
212797        ,:ACCE-KDPSWQP-1                                                  
212798        ,:ACCE-DAPSWPP-2                                                  
212799        ,:ACCE-KDPSWPP-2                                                  
212800        ,:ACCE-DAPSWCP-3                                                  
212801        ,:ACCE-KDPSWCP-3                                                  
212802        ,:ACCE-KDFARGST                                                   
212803        ,:ACCE-IDPROJK                                                    
212804        ,:ACCE-VKART-KDP                                                  
212805        ,:ACCE-KDANNULL                                                   
212806        ,:ACCE-BEUPPDSU                                                   
212807     END-EXEC                                                             
212808     MOVE SQLCODE TO SQLCODE-WS                                           
212809     PERFORM DB2-STATUS-CONTROL                                           
212810     .                                                                    
212811     EJECT                                                                
212812                                                                          
212813 DB2-FETCH-TB1ACCE-CRS-TYP-16 SECTION.                                    
212814     SKIP2                                                                
212815     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-16 ' TO DB2-SEKTION                  
212816     MOVE 000100  TO GODK-SQLCODEKODER                                    
212817     EXEC SQL                                                             
212818         FETCH TB1ACCE-CRS-16                                             
212819         INTO                                                             
212820         :ACCE-IDARTNR                                                    
212821        ,:ACCE-BEART                                                      
212822        ,:ACCE-IDPRODGR                                                   
212823        ,:ACCE-IDUPPDSU                                                   
212824        ,:ACCE-IDUPPDKU                                                   
212825        ,:ACCE-IDAOT                                                      
212826        ,:ACCE-TIAOINF                                                    
212827        ,:ACCE-BEASSTYP                                                   
212828        ,:ACCE-KDARTTYP                                                   
212829        ,:ACCE-KDMDS                                                      
212830        ,:ACCE-KDFRPTYP                                                   
212831        ,:ACCE-TEARTUTFG                                                  
212832        ,:ACCE-TESTATUPP                                                  
212833        ,:ACCE-IDLEVNR-GSDB                                               
212834        ,:ACCE-KDTPD-PH1                                                  
212835        ,:ACCE-DATPDPH1                                                   
212836        ,:ACCE-DAPSWQP-1                                                  
212837        ,:ACCE-KDPSWQP-1                                                  
212838        ,:ACCE-DAPSWQA-1                                                  
212839        ,:ACCE-KDPSWQA-1                                                  
212840        ,:ACCE-DAPSWPP-2                                                  
212841        ,:ACCE-KDPSWPP-2                                                  
212842        ,:ACCE-DAPSWPA-2                                                  
212843        ,:ACCE-KDPSWPA-2                                                  
212844        ,:ACCE-DAPSWCP-3                                                  
212845        ,:ACCE-KDPSWCP-3                                                  
212846        ,:ACCE-DAPSWCA-3                                                  
212847        ,:ACCE-KDPSWCA-3                                                  
212848        ,:ACCE-KVYVOL-B3                                                  
212849        ,:ACCE-KVYVOL-B2                                                  
212850        ,:ACCE-KVYVOL-INT                                                 
212851        ,:ACCE-KVYVOL-B1                                                  
212852        ,:ACCE-KVYVOL-ASS                                                 
212853        ,:ACCE-BEMAPP                                                     
212854        ,:ACCE-KVFOTO                                                     
212855        ,:ACCE-TIFOTO                                                     
212856        ,:ACCE-TENOTE                                                     
212857        ,:ACCE-FLANNULL                                                   
212858        ,:ACCE-TEVERKTYG                                                  
212859        ,:ACCE-TESTATXT                                                   
212860        ,:ACCE-TEMATXT                                                    
212861        ,:ACCE-TEINKTXT                                                   
212862        ,:ACCE-TEANSTXT                                                   
212863        ,:ACCE-TEAUXTXT                                                   
212864        ,:ACCE-IDARTNR-OFARG                                              
212865        ,:ACCE-IDPSLAG                                                    
212866        ,:ACCE-BETYP                                                      
212867        ,:ACCE-IDFKNGRP                                                   
212868        ,:ACCE-IDKDPPOS                                                   
212869        ,:ACCE-IDAOTUTG                                                   
212870        ,:ACCE-BEANST-KU                                                  
212871        ,:ACCE-BEANST-SU                                                  
212872        ,:ACCE-IDPSS                                                      
212873        ,:ACCE-DAPSWQP-1                                                  
212874        ,:ACCE-KDPSWQP-1                                                  
212875        ,:ACCE-DAPSWPP-2                                                  
212876        ,:ACCE-KDPSWPP-2                                                  
212877        ,:ACCE-DAPSWCP-3                                                  
212878        ,:ACCE-KDPSWCP-3                                                  
212879        ,:ACCE-KDFARGST                                                   
212880        ,:ACCE-IDPROJK                                                    
212881        ,:ACCE-VKART-KDP                                                  
212882        ,:ACCE-KDANNULL                                                   
212883        ,:ACCE-BEUPPDSU                                                   
212884     END-EXEC                                                             
212885     MOVE SQLCODE TO SQLCODE-WS                                           
212886     PERFORM DB2-STATUS-CONTROL                                           
212887     .                                                                    
212888     EJECT                                                                
212889                                                                          
212891 DB2-FETCH-TB1ACCE-CRS-TYP-17 SECTION.                                    
212892     SKIP2                                                                
212893     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-17 ' TO DB2-SEKTION                  
212894     MOVE 000100  TO GODK-SQLCODEKODER                                    
212895     EXEC SQL                                                             
212896         FETCH TB1ACCE-CRS-17                                             
212897         INTO                                                             
212898         :ACCE-IDARTNR                                                    
212899        ,:ACCE-BEART                                                      
212900        ,:ACCE-IDPRODGR                                                   
212901        ,:ACCE-IDUPPDSU                                                   
212902        ,:ACCE-IDUPPDKU                                                   
212903        ,:ACCE-IDAOT                                                      
212904        ,:ACCE-TIAOINF                                                    
212905        ,:ACCE-BEASSTYP                                                   
212906        ,:ACCE-KDARTTYP                                                   
212907        ,:ACCE-KDMDS                                                      
212908        ,:ACCE-KDFRPTYP                                                   
212909        ,:ACCE-TEARTUTFG                                                  
212910        ,:ACCE-TESTATUPP                                                  
212911        ,:ACCE-IDLEVNR-GSDB                                               
212912        ,:ACCE-KDTPD-PH1                                                  
212913        ,:ACCE-DATPDPH1                                                   
212914        ,:ACCE-DAPSWQP-1                                                  
212915        ,:ACCE-KDPSWQP-1                                                  
212916        ,:ACCE-DAPSWQA-1                                                  
212917        ,:ACCE-KDPSWQA-1                                                  
212918        ,:ACCE-DAPSWPP-2                                                  
212919        ,:ACCE-KDPSWPP-2                                                  
212920        ,:ACCE-DAPSWPA-2                                                  
212921        ,:ACCE-KDPSWPA-2                                                  
212922        ,:ACCE-DAPSWCP-3                                                  
212923        ,:ACCE-KDPSWCP-3                                                  
212924        ,:ACCE-DAPSWCA-3                                                  
212925        ,:ACCE-KDPSWCA-3                                                  
212926        ,:ACCE-KVYVOL-B3                                                  
212927        ,:ACCE-KVYVOL-B2                                                  
212928        ,:ACCE-KVYVOL-INT                                                 
212929        ,:ACCE-KVYVOL-B1                                                  
212930        ,:ACCE-KVYVOL-ASS                                                 
212931        ,:ACCE-BEMAPP                                                     
212932        ,:ACCE-KVFOTO                                                     
212933        ,:ACCE-TIFOTO                                                     
212934        ,:ACCE-TENOTE                                                     
212935        ,:ACCE-FLANNULL                                                   
212936        ,:ACCE-TEVERKTYG                                                  
212937        ,:ACCE-TESTATXT                                                   
212938        ,:ACCE-TEMATXT                                                    
212939        ,:ACCE-TEINKTXT                                                   
212940        ,:ACCE-TEANSTXT                                                   
212941        ,:ACCE-TEAUXTXT                                                   
212942        ,:ACCE-IDARTNR-OFARG                                              
212943        ,:ACCE-IDPSLAG                                                    
212944        ,:ACCE-BETYP                                                      
212945        ,:ACCE-IDFKNGRP                                                   
212946        ,:ACCE-IDKDPPOS                                                   
212947        ,:ACCE-IDAOTUTG                                                   
212948        ,:ACCE-BEANST-KU                                                  
212949        ,:ACCE-BEANST-SU                                                  
212950        ,:ACCE-IDPSS                                                      
212951        ,:ACCE-DAPSWQP-1                                                  
212952        ,:ACCE-KDPSWQP-1                                                  
212953        ,:ACCE-DAPSWPP-2                                                  
212954        ,:ACCE-KDPSWPP-2                                                  
212955        ,:ACCE-DAPSWCP-3                                                  
212956        ,:ACCE-KDPSWCP-3                                                  
212957        ,:ACCE-KDFARGST                                                   
212958        ,:ACCE-IDPROJK                                                    
212959        ,:ACCE-VKART-KDP                                                  
212960        ,:ACCE-KDANNULL                                                   
212961        ,:ACCE-BEUPPDSU                                                   
212962     END-EXEC                                                             
212963     MOVE SQLCODE TO SQLCODE-WS                                           
212964     PERFORM DB2-STATUS-CONTROL                                           
212965     .                                                                    
212966     EJECT                                                                
212967                                                                          
212968 DB2-FETCH-TB1ACCE-CRS-TYP-18 SECTION.                                    
212969     SKIP2                                                                
212970     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-18 ' TO DB2-SEKTION                  
212971     MOVE 000100  TO GODK-SQLCODEKODER                                    
212972     EXEC SQL                                                             
212973         FETCH TB1ACCE-CRS-18                                             
212974         INTO                                                             
212975         :ACCE-IDARTNR                                                    
212976        ,:ACCE-BEART                                                      
212977        ,:ACCE-IDPRODGR                                                   
212978        ,:ACCE-IDUPPDSU                                                   
212979        ,:ACCE-IDUPPDKU                                                   
212980        ,:ACCE-IDAOT                                                      
212981        ,:ACCE-TIAOINF                                                    
212982        ,:ACCE-BEASSTYP                                                   
212983        ,:ACCE-KDARTTYP                                                   
212984        ,:ACCE-KDMDS                                                      
212985        ,:ACCE-KDFRPTYP                                                   
212986        ,:ACCE-TEARTUTFG                                                  
212987        ,:ACCE-TESTATUPP                                                  
212988        ,:ACCE-IDLEVNR-GSDB                                               
212989        ,:ACCE-KDTPD-PH1                                                  
212990        ,:ACCE-DATPDPH1                                                   
212991        ,:ACCE-DAPSWQP-1                                                  
212992        ,:ACCE-KDPSWQP-1                                                  
212993        ,:ACCE-DAPSWQA-1                                                  
212994        ,:ACCE-KDPSWQA-1                                                  
212995        ,:ACCE-DAPSWPP-2                                                  
212996        ,:ACCE-KDPSWPP-2                                                  
212997        ,:ACCE-DAPSWPA-2                                                  
212998        ,:ACCE-KDPSWPA-2                                                  
212999        ,:ACCE-DAPSWCP-3                                                  
213000        ,:ACCE-KDPSWCP-3                                                  
213001        ,:ACCE-DAPSWCA-3                                                  
213002        ,:ACCE-KDPSWCA-3                                                  
213003        ,:ACCE-KVYVOL-B3                                                  
213004        ,:ACCE-KVYVOL-B2                                                  
213005        ,:ACCE-KVYVOL-INT                                                 
213006        ,:ACCE-KVYVOL-B1                                                  
213007        ,:ACCE-KVYVOL-ASS                                                 
213008        ,:ACCE-BEMAPP                                                     
213009        ,:ACCE-KVFOTO                                                     
213010        ,:ACCE-TIFOTO                                                     
213011        ,:ACCE-TENOTE                                                     
213012        ,:ACCE-FLANNULL                                                   
213013        ,:ACCE-TEVERKTYG                                                  
213014        ,:ACCE-TESTATXT                                                   
213015        ,:ACCE-TEMATXT                                                    
213016        ,:ACCE-TEINKTXT                                                   
213017        ,:ACCE-TEANSTXT                                                   
213018        ,:ACCE-TEAUXTXT                                                   
213019        ,:ACCE-IDARTNR-OFARG                                              
213020        ,:ACCE-IDPSLAG                                                    
213021        ,:ACCE-BETYP                                                      
213022        ,:ACCE-IDFKNGRP                                                   
213023        ,:ACCE-IDKDPPOS                                                   
213024        ,:ACCE-IDAOTUTG                                                   
213025        ,:ACCE-BEANST-KU                                                  
213026        ,:ACCE-BEANST-SU                                                  
213027        ,:ACCE-IDPSS                                                      
213028        ,:ACCE-DAPSWQP-1                                                  
213029        ,:ACCE-KDPSWQP-1                                                  
213030        ,:ACCE-DAPSWPP-2                                                  
213031        ,:ACCE-KDPSWPP-2                                                  
213032        ,:ACCE-DAPSWCP-3                                                  
213033        ,:ACCE-KDPSWCP-3                                                  
213034        ,:ACCE-KDFARGST                                                   
213035        ,:ACCE-IDPROJK                                                    
213036        ,:ACCE-VKART-KDP                                                  
213037        ,:ACCE-KDANNULL                                                   
213038        ,:ACCE-BEUPPDSU                                                   
213039     END-EXEC                                                             
213040     MOVE SQLCODE TO SQLCODE-WS                                           
213041     PERFORM DB2-STATUS-CONTROL                                           
213042     .                                                                    
213043     EJECT                                                                
213044                                                                          
213045 DB2-FETCH-TB1ACCE-CRS-TYP-19 SECTION.                                    
213046     SKIP2                                                                
213047     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-19 ' TO DB2-SEKTION                  
213048     MOVE 000100  TO GODK-SQLCODEKODER                                    
213049     EXEC SQL                                                             
213050         FETCH TB1ACCE-CRS-19                                             
213051         INTO                                                             
213052         :ACCE-IDARTNR                                                    
213053        ,:ACCE-BEART                                                      
213054        ,:ACCE-IDPRODGR                                                   
213055        ,:ACCE-IDUPPDSU                                                   
213056        ,:ACCE-IDUPPDKU                                                   
213057        ,:ACCE-IDAOT                                                      
213058        ,:ACCE-TIAOINF                                                    
213059        ,:ACCE-BEASSTYP                                                   
213060        ,:ACCE-KDARTTYP                                                   
213061        ,:ACCE-KDMDS                                                      
213062        ,:ACCE-KDFRPTYP                                                   
213063        ,:ACCE-TEARTUTFG                                                  
213064        ,:ACCE-TESTATUPP                                                  
213065        ,:ACCE-IDLEVNR-GSDB                                               
213066        ,:ACCE-KDTPD-PH1                                                  
213067        ,:ACCE-DATPDPH1                                                   
213068        ,:ACCE-DAPSWQP-1                                                  
213069        ,:ACCE-KDPSWQP-1                                                  
213070        ,:ACCE-DAPSWQA-1                                                  
213071        ,:ACCE-KDPSWQA-1                                                  
213072        ,:ACCE-DAPSWPP-2                                                  
213073        ,:ACCE-KDPSWPP-2                                                  
213074        ,:ACCE-DAPSWPA-2                                                  
213075        ,:ACCE-KDPSWPA-2                                                  
213076        ,:ACCE-DAPSWCP-3                                                  
213077        ,:ACCE-KDPSWCP-3                                                  
213078        ,:ACCE-DAPSWCA-3                                                  
213079        ,:ACCE-KDPSWCA-3                                                  
213080        ,:ACCE-KVYVOL-B3                                                  
213081        ,:ACCE-KVYVOL-B2                                                  
213082        ,:ACCE-KVYVOL-INT                                                 
213083        ,:ACCE-KVYVOL-B1                                                  
213084        ,:ACCE-KVYVOL-ASS                                                 
213085        ,:ACCE-BEMAPP                                                     
213086        ,:ACCE-KVFOTO                                                     
213087        ,:ACCE-TIFOTO                                                     
213088        ,:ACCE-TENOTE                                                     
213089        ,:ACCE-FLANNULL                                                   
213090        ,:ACCE-TEVERKTYG                                                  
213091        ,:ACCE-TESTATXT                                                   
213092        ,:ACCE-TEMATXT                                                    
213093        ,:ACCE-TEINKTXT                                                   
213094        ,:ACCE-TEANSTXT                                                   
213095        ,:ACCE-TEAUXTXT                                                   
213096        ,:ACCE-IDARTNR-OFARG                                              
213097        ,:ACCE-IDPSLAG                                                    
213098        ,:ACCE-BETYP                                                      
213099        ,:ACCE-IDFKNGRP                                                   
213100        ,:ACCE-IDKDPPOS                                                   
213101        ,:ACCE-IDAOTUTG                                                   
213102        ,:ACCE-BEANST-KU                                                  
213103        ,:ACCE-BEANST-SU                                                  
213104        ,:ACCE-IDPSS                                                      
213105        ,:ACCE-DAPSWQP-1                                                  
213106        ,:ACCE-KDPSWQP-1                                                  
213107        ,:ACCE-DAPSWPP-2                                                  
213108        ,:ACCE-KDPSWPP-2                                                  
213109        ,:ACCE-DAPSWCP-3                                                  
213110        ,:ACCE-KDPSWCP-3                                                  
213111        ,:ACCE-KDFARGST                                                   
213112        ,:ACCE-IDPROJK                                                    
213113        ,:ACCE-VKART-KDP                                                  
213114        ,:ACCE-KDANNULL                                                   
213115        ,:ACCE-BEUPPDSU                                                   
213116     END-EXEC                                                             
213117     MOVE SQLCODE TO SQLCODE-WS                                           
213118     PERFORM DB2-STATUS-CONTROL                                           
213119     .                                                                    
213120     EJECT                                                                
213121                                                                          
213122 DB2-FETCH-TB1ACCE-CRS-TYP-20 SECTION.                                    
213123     SKIP2                                                                
213124     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-20 ' TO DB2-SEKTION                  
213125     MOVE 000100  TO GODK-SQLCODEKODER                                    
213126     EXEC SQL                                                             
213127         FETCH TB1ACCE-CRS-20                                             
213128         INTO                                                             
213129         :ACCE-IDARTNR                                                    
213130        ,:ACCE-BEART                                                      
213131        ,:ACCE-IDPRODGR                                                   
213132        ,:ACCE-IDUPPDSU                                                   
213133        ,:ACCE-IDUPPDKU                                                   
213134        ,:ACCE-IDAOT                                                      
213135        ,:ACCE-TIAOINF                                                    
213136        ,:ACCE-BEASSTYP                                                   
213137        ,:ACCE-KDARTTYP                                                   
213138        ,:ACCE-KDMDS                                                      
213139        ,:ACCE-KDFRPTYP                                                   
213140        ,:ACCE-TEARTUTFG                                                  
213141        ,:ACCE-TESTATUPP                                                  
213142        ,:ACCE-IDLEVNR-GSDB                                               
213143        ,:ACCE-KDTPD-PH1                                                  
213144        ,:ACCE-DATPDPH1                                                   
213145        ,:ACCE-DAPSWQP-1                                                  
213146        ,:ACCE-KDPSWQP-1                                                  
213147        ,:ACCE-DAPSWQA-1                                                  
213148        ,:ACCE-KDPSWQA-1                                                  
213149        ,:ACCE-DAPSWPP-2                                                  
213150        ,:ACCE-KDPSWPP-2                                                  
213151        ,:ACCE-DAPSWPA-2                                                  
213152        ,:ACCE-KDPSWPA-2                                                  
213153        ,:ACCE-DAPSWCP-3                                                  
213154        ,:ACCE-KDPSWCP-3                                                  
213155        ,:ACCE-DAPSWCA-3                                                  
213156        ,:ACCE-KDPSWCA-3                                                  
213157        ,:ACCE-KVYVOL-B3                                                  
213158        ,:ACCE-KVYVOL-B2                                                  
213159        ,:ACCE-KVYVOL-INT                                                 
213160        ,:ACCE-KVYVOL-B1                                                  
213161        ,:ACCE-KVYVOL-ASS                                                 
213162        ,:ACCE-BEMAPP                                                     
213163        ,:ACCE-KVFOTO                                                     
213164        ,:ACCE-TIFOTO                                                     
213165        ,:ACCE-TENOTE                                                     
213166        ,:ACCE-FLANNULL                                                   
213167        ,:ACCE-TEVERKTYG                                                  
213168        ,:ACCE-TESTATXT                                                   
213169        ,:ACCE-TEMATXT                                                    
213170        ,:ACCE-TEINKTXT                                                   
213171        ,:ACCE-TEANSTXT                                                   
213172        ,:ACCE-TEAUXTXT                                                   
213173        ,:ACCE-IDARTNR-OFARG                                              
213174        ,:ACCE-IDPSLAG                                                    
213175        ,:ACCE-BETYP                                                      
213176        ,:ACCE-IDFKNGRP                                                   
213177        ,:ACCE-IDKDPPOS                                                   
213178        ,:ACCE-IDAOTUTG                                                   
213179        ,:ACCE-BEANST-KU                                                  
213180        ,:ACCE-BEANST-SU                                                  
213181        ,:ACCE-IDPSS                                                      
213182        ,:ACCE-DAPSWQP-1                                                  
213183        ,:ACCE-KDPSWQP-1                                                  
213184        ,:ACCE-DAPSWPP-2                                                  
213185        ,:ACCE-KDPSWPP-2                                                  
213186        ,:ACCE-DAPSWCP-3                                                  
213187        ,:ACCE-KDPSWCP-3                                                  
213188        ,:ACCE-KDFARGST                                                   
213189        ,:ACCE-IDPROJK                                                    
213190        ,:ACCE-VKART-KDP                                                  
213191        ,:ACCE-KDANNULL                                                   
213192        ,:ACCE-BEUPPDSU                                                   
213193     END-EXEC                                                             
213194     MOVE SQLCODE TO SQLCODE-WS                                           
213195     PERFORM DB2-STATUS-CONTROL                                           
213196     .                                                                    
213197     EJECT                                                                
213198                                                                          
213199 DB2-FETCH-TB1ACCE-CRS-TYP-21 SECTION.                                    
213200     SKIP2                                                                
213201     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-21 ' TO DB2-SEKTION                  
213202     MOVE 000100  TO GODK-SQLCODEKODER                                    
213203     EXEC SQL                                                             
213204         FETCH TB1ACCE-CRS-21                                             
213205         INTO                                                             
213206         :ACCE-IDARTNR                                                    
213207        ,:ACCE-BEART                                                      
213208        ,:ACCE-IDPRODGR                                                   
213209        ,:ACCE-IDUPPDSU                                                   
213210        ,:ACCE-IDUPPDKU                                                   
213211        ,:ACCE-IDAOT                                                      
213212        ,:ACCE-TIAOINF                                                    
213213        ,:ACCE-BEASSTYP                                                   
213214        ,:ACCE-KDARTTYP                                                   
213215        ,:ACCE-KDMDS                                                      
213216        ,:ACCE-KDFRPTYP                                                   
213217        ,:ACCE-TEARTUTFG                                                  
213218        ,:ACCE-TESTATUPP                                                  
213219        ,:ACCE-IDLEVNR-GSDB                                               
213220        ,:ACCE-KDTPD-PH1                                                  
213221        ,:ACCE-DATPDPH1                                                   
213222        ,:ACCE-DAPSWQP-1                                                  
213223        ,:ACCE-KDPSWQP-1                                                  
213224        ,:ACCE-DAPSWQA-1                                                  
213225        ,:ACCE-KDPSWQA-1                                                  
213226        ,:ACCE-DAPSWPP-2                                                  
213227        ,:ACCE-KDPSWPP-2                                                  
213228        ,:ACCE-DAPSWPA-2                                                  
213229        ,:ACCE-KDPSWPA-2                                                  
213230        ,:ACCE-DAPSWCP-3                                                  
213231        ,:ACCE-KDPSWCP-3                                                  
213232        ,:ACCE-DAPSWCA-3                                                  
213233        ,:ACCE-KDPSWCA-3                                                  
213234        ,:ACCE-KVYVOL-B3                                                  
213235        ,:ACCE-KVYVOL-B2                                                  
213236        ,:ACCE-KVYVOL-INT                                                 
213237        ,:ACCE-KVYVOL-B1                                                  
213238        ,:ACCE-KVYVOL-ASS                                                 
213239        ,:ACCE-BEMAPP                                                     
213240        ,:ACCE-KVFOTO                                                     
213241        ,:ACCE-TIFOTO                                                     
213242        ,:ACCE-TENOTE                                                     
213243        ,:ACCE-FLANNULL                                                   
213244        ,:ACCE-TEVERKTYG                                                  
213245        ,:ACCE-TESTATXT                                                   
213246        ,:ACCE-TEMATXT                                                    
213247        ,:ACCE-TEINKTXT                                                   
213248        ,:ACCE-TEANSTXT                                                   
213249        ,:ACCE-TEAUXTXT                                                   
213250        ,:ACCE-IDARTNR-OFARG                                              
213251        ,:ACCE-IDPSLAG                                                    
213252        ,:ACCE-BETYP                                                      
213253        ,:ACCE-IDFKNGRP                                                   
213254        ,:ACCE-IDKDPPOS                                                   
213255        ,:ACCE-IDAOTUTG                                                   
213256        ,:ACCE-BEANST-KU                                                  
213257        ,:ACCE-BEANST-SU                                                  
213258        ,:ACCE-IDPSS                                                      
213259        ,:ACCE-DAPSWQP-1                                                  
213260        ,:ACCE-KDPSWQP-1                                                  
213261        ,:ACCE-DAPSWPP-2                                                  
213262        ,:ACCE-KDPSWPP-2                                                  
213263        ,:ACCE-DAPSWCP-3                                                  
213264        ,:ACCE-KDPSWCP-3                                                  
213265        ,:ACCE-KDFARGST                                                   
213266        ,:ACCE-IDPROJK                                                    
213267        ,:ACCE-VKART-KDP                                                  
213268        ,:ACCE-KDANNULL                                                   
213269        ,:ACCE-BEUPPDSU                                                   
213270     END-EXEC                                                             
213271     MOVE SQLCODE TO SQLCODE-WS                                           
213272     PERFORM DB2-STATUS-CONTROL                                           
213273     .                                                                    
213274     EJECT                                                                
213275                                                                          
213276 DB2-FETCH-TB1ACCE-CRS-TYP-22 SECTION.                                    
213277     SKIP2                                                                
213278     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-22 ' TO DB2-SEKTION                  
213279     MOVE 000100  TO GODK-SQLCODEKODER                                    
213280     EXEC SQL                                                             
213281         FETCH TB1ACCE-CRS-22                                             
213282         INTO                                                             
213283         :ACCE-IDARTNR                                                    
213284        ,:ACCE-BEART                                                      
213285        ,:ACCE-IDPRODGR                                                   
213286        ,:ACCE-IDUPPDSU                                                   
213287        ,:ACCE-IDUPPDKU                                                   
213288        ,:ACCE-IDAOT                                                      
213289        ,:ACCE-TIAOINF                                                    
213290        ,:ACCE-BEASSTYP                                                   
213291        ,:ACCE-KDARTTYP                                                   
213292        ,:ACCE-KDMDS                                                      
213293        ,:ACCE-KDFRPTYP                                                   
213294        ,:ACCE-TEARTUTFG                                                  
213295        ,:ACCE-TESTATUPP                                                  
213296        ,:ACCE-IDLEVNR-GSDB                                               
213297        ,:ACCE-KDTPD-PH1                                                  
213298        ,:ACCE-DATPDPH1                                                   
213299        ,:ACCE-DAPSWQP-1                                                  
213300        ,:ACCE-KDPSWQP-1                                                  
213301        ,:ACCE-DAPSWQA-1                                                  
213302        ,:ACCE-KDPSWQA-1                                                  
213303        ,:ACCE-DAPSWPP-2                                                  
213304        ,:ACCE-KDPSWPP-2                                                  
213305        ,:ACCE-DAPSWPA-2                                                  
213306        ,:ACCE-KDPSWPA-2                                                  
213307        ,:ACCE-DAPSWCP-3                                                  
213308        ,:ACCE-KDPSWCP-3                                                  
213309        ,:ACCE-DAPSWCA-3                                                  
213310        ,:ACCE-KDPSWCA-3                                                  
213311        ,:ACCE-KVYVOL-B3                                                  
213312        ,:ACCE-KVYVOL-B2                                                  
213313        ,:ACCE-KVYVOL-INT                                                 
213314        ,:ACCE-KVYVOL-B1                                                  
213315        ,:ACCE-KVYVOL-ASS                                                 
213316        ,:ACCE-BEMAPP                                                     
213317        ,:ACCE-KVFOTO                                                     
213318        ,:ACCE-TIFOTO                                                     
213319        ,:ACCE-TENOTE                                                     
213320        ,:ACCE-FLANNULL                                                   
213321        ,:ACCE-TEVERKTYG                                                  
213322        ,:ACCE-TESTATXT                                                   
213323        ,:ACCE-TEMATXT                                                    
213324        ,:ACCE-TEINKTXT                                                   
213325        ,:ACCE-TEANSTXT                                                   
213326        ,:ACCE-TEAUXTXT                                                   
213327        ,:ACCE-IDARTNR-OFARG                                              
213328        ,:ACCE-IDPSLAG                                                    
213329        ,:ACCE-BETYP                                                      
213330        ,:ACCE-IDFKNGRP                                                   
213331        ,:ACCE-IDKDPPOS                                                   
213332        ,:ACCE-IDAOTUTG                                                   
213333        ,:ACCE-BEANST-KU                                                  
213334        ,:ACCE-BEANST-SU                                                  
213335        ,:ACCE-IDPSS                                                      
213336        ,:ACCE-DAPSWQP-1                                                  
213337        ,:ACCE-KDPSWQP-1                                                  
213338        ,:ACCE-DAPSWPP-2                                                  
213339        ,:ACCE-KDPSWPP-2                                                  
213340        ,:ACCE-DAPSWCP-3                                                  
213341        ,:ACCE-KDPSWCP-3                                                  
213342        ,:ACCE-KDFARGST                                                   
213343        ,:ACCE-IDPROJK                                                    
213344        ,:ACCE-VKART-KDP                                                  
213345        ,:ACCE-KDANNULL                                                   
213346        ,:ACCE-BEUPPDSU                                                   
213347     END-EXEC                                                             
213348     MOVE SQLCODE TO SQLCODE-WS                                           
213349     PERFORM DB2-STATUS-CONTROL                                           
213350     .                                                                    
213351     EJECT                                                                
213352                                                                          
213353 DB2-FETCH-TB1ACCE-CRS-TYP-23 SECTION.                                    
213354     SKIP2                                                                
213355     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-23 ' TO DB2-SEKTION                  
213356     MOVE 000100  TO GODK-SQLCODEKODER                                    
213357     EXEC SQL                                                             
213358         FETCH TB1ACCE-CRS-23                                             
213359         INTO                                                             
213360         :ACCE-IDARTNR                                                    
213361        ,:ACCE-BEART                                                      
213362        ,:ACCE-IDPRODGR                                                   
213363        ,:ACCE-IDUPPDSU                                                   
213364        ,:ACCE-IDUPPDKU                                                   
213365        ,:ACCE-IDAOT                                                      
213366        ,:ACCE-TIAOINF                                                    
213367        ,:ACCE-BEASSTYP                                                   
213368        ,:ACCE-KDARTTYP                                                   
213369        ,:ACCE-KDMDS                                                      
213370        ,:ACCE-KDFRPTYP                                                   
213371        ,:ACCE-TEARTUTFG                                                  
213372        ,:ACCE-TESTATUPP                                                  
213373        ,:ACCE-IDLEVNR-GSDB                                               
213374        ,:ACCE-KDTPD-PH1                                                  
213375        ,:ACCE-DATPDPH1                                                   
213376        ,:ACCE-DAPSWQP-1                                                  
213377        ,:ACCE-KDPSWQP-1                                                  
213378        ,:ACCE-DAPSWQA-1                                                  
213379        ,:ACCE-KDPSWQA-1                                                  
213380        ,:ACCE-DAPSWPP-2                                                  
213381        ,:ACCE-KDPSWPP-2                                                  
213382        ,:ACCE-DAPSWPA-2                                                  
213383        ,:ACCE-KDPSWPA-2                                                  
213384        ,:ACCE-DAPSWCP-3                                                  
213385        ,:ACCE-KDPSWCP-3                                                  
213386        ,:ACCE-DAPSWCA-3                                                  
213387        ,:ACCE-KDPSWCA-3                                                  
213388        ,:ACCE-KVYVOL-B3                                                  
213389        ,:ACCE-KVYVOL-B2                                                  
213390        ,:ACCE-KVYVOL-INT                                                 
213391        ,:ACCE-KVYVOL-B1                                                  
213392        ,:ACCE-KVYVOL-ASS                                                 
213393        ,:ACCE-BEMAPP                                                     
213394        ,:ACCE-KVFOTO                                                     
213395        ,:ACCE-TIFOTO                                                     
213396        ,:ACCE-TENOTE                                                     
213397        ,:ACCE-FLANNULL                                                   
213398        ,:ACCE-TEVERKTYG                                                  
213399        ,:ACCE-TESTATXT                                                   
213400        ,:ACCE-TEMATXT                                                    
213401        ,:ACCE-TEINKTXT                                                   
213402        ,:ACCE-TEANSTXT                                                   
213403        ,:ACCE-TEAUXTXT                                                   
213404        ,:ACCE-IDARTNR-OFARG                                              
213405        ,:ACCE-IDPSLAG                                                    
213406        ,:ACCE-BETYP                                                      
213407        ,:ACCE-IDFKNGRP                                                   
213408        ,:ACCE-IDKDPPOS                                                   
213409        ,:ACCE-IDAOTUTG                                                   
213410        ,:ACCE-BEANST-KU                                                  
213411        ,:ACCE-BEANST-SU                                                  
213412        ,:ACCE-IDPSS                                                      
213413        ,:ACCE-DAPSWQP-1                                                  
213414        ,:ACCE-KDPSWQP-1                                                  
213415        ,:ACCE-DAPSWPP-2                                                  
213416        ,:ACCE-KDPSWPP-2                                                  
213417        ,:ACCE-DAPSWCP-3                                                  
213418        ,:ACCE-KDPSWCP-3                                                  
213419        ,:ACCE-KDFARGST                                                   
213420        ,:ACCE-IDPROJK                                                    
213421        ,:ACCE-VKART-KDP                                                  
213422        ,:ACCE-KDANNULL                                                   
213423        ,:ACCE-BEUPPDSU                                                   
213424     END-EXEC                                                             
213425     MOVE SQLCODE TO SQLCODE-WS                                           
213426     PERFORM DB2-STATUS-CONTROL                                           
213427     .                                                                    
213428     EJECT                                                                
213429                                                                          
213430 DB2-FETCH-TB1ACCE-CRS-TYP-24 SECTION.                                    
213431     SKIP2                                                                
213432     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-24 ' TO DB2-SEKTION                  
213433     MOVE 000100  TO GODK-SQLCODEKODER                                    
213434     EXEC SQL                                                             
213435         FETCH TB1ACCE-CRS-24                                             
213436         INTO                                                             
213437         :ACCE-IDARTNR                                                    
213438        ,:ACCE-BEART                                                      
213439        ,:ACCE-IDPRODGR                                                   
213440        ,:ACCE-IDUPPDSU                                                   
213441        ,:ACCE-IDUPPDKU                                                   
213442        ,:ACCE-IDAOT                                                      
213443        ,:ACCE-TIAOINF                                                    
213444        ,:ACCE-BEASSTYP                                                   
213445        ,:ACCE-KDARTTYP                                                   
213446        ,:ACCE-KDMDS                                                      
213447        ,:ACCE-KDFRPTYP                                                   
213448        ,:ACCE-TEARTUTFG                                                  
213449        ,:ACCE-TESTATUPP                                                  
213450        ,:ACCE-IDLEVNR-GSDB                                               
213451        ,:ACCE-KDTPD-PH1                                                  
213452        ,:ACCE-DATPDPH1                                                   
213453        ,:ACCE-DAPSWQP-1                                                  
213454        ,:ACCE-KDPSWQP-1                                                  
213455        ,:ACCE-DAPSWQA-1                                                  
213456        ,:ACCE-KDPSWQA-1                                                  
213457        ,:ACCE-DAPSWPP-2                                                  
213458        ,:ACCE-KDPSWPP-2                                                  
213459        ,:ACCE-DAPSWPA-2                                                  
213460        ,:ACCE-KDPSWPA-2                                                  
213461        ,:ACCE-DAPSWCP-3                                                  
213462        ,:ACCE-KDPSWCP-3                                                  
213463        ,:ACCE-DAPSWCA-3                                                  
213464        ,:ACCE-KDPSWCA-3                                                  
213465        ,:ACCE-KVYVOL-B3                                                  
213466        ,:ACCE-KVYVOL-B2                                                  
213467        ,:ACCE-KVYVOL-INT                                                 
213468        ,:ACCE-KVYVOL-B1                                                  
213469        ,:ACCE-KVYVOL-ASS                                                 
213470        ,:ACCE-BEMAPP                                                     
213471        ,:ACCE-KVFOTO                                                     
213472        ,:ACCE-TIFOTO                                                     
213473        ,:ACCE-TENOTE                                                     
213474        ,:ACCE-FLANNULL                                                   
213475        ,:ACCE-TEVERKTYG                                                  
213476        ,:ACCE-TESTATXT                                                   
213477        ,:ACCE-TEMATXT                                                    
213478        ,:ACCE-TEINKTXT                                                   
213479        ,:ACCE-TEANSTXT                                                   
213480        ,:ACCE-TEAUXTXT                                                   
213481        ,:ACCE-IDARTNR-OFARG                                              
213482        ,:ACCE-IDPSLAG                                                    
213483        ,:ACCE-BETYP                                                      
213484        ,:ACCE-IDFKNGRP                                                   
213485        ,:ACCE-IDKDPPOS                                                   
213486        ,:ACCE-IDAOTUTG                                                   
213487        ,:ACCE-BEANST-KU                                                  
213488        ,:ACCE-BEANST-SU                                                  
213489        ,:ACCE-IDPSS                                                      
213490        ,:ACCE-DAPSWQP-1                                                  
213491        ,:ACCE-KDPSWQP-1                                                  
213492        ,:ACCE-DAPSWPP-2                                                  
213493        ,:ACCE-KDPSWPP-2                                                  
213494        ,:ACCE-DAPSWCP-3                                                  
213495        ,:ACCE-KDPSWCP-3                                                  
213496        ,:ACCE-KDFARGST                                                   
213497        ,:ACCE-IDPROJK                                                    
213498        ,:ACCE-VKART-KDP                                                  
213499        ,:ACCE-KDANNULL                                                   
213500        ,:ACCE-BEUPPDSU                                                   
213501     END-EXEC                                                             
213502     MOVE SQLCODE TO SQLCODE-WS                                           
213503     PERFORM DB2-STATUS-CONTROL                                           
213504     .                                                                    
213505     EJECT                                                                
213506                                                                          
213507 DB2-FETCH-TB1ACCE-CRS-TYP-25 SECTION.                                    
213508     SKIP2                                                                
213509     MOVE 'DB2-FETCH-TB1ACCE-CRS-TYP-25 ' TO DB2-SEKTION                  
213510     MOVE 000100  TO GODK-SQLCODEKODER                                    
213511     EXEC SQL                                                             
213512         FETCH TB1ACCE-CRS-25                                             
213513         INTO                                                             
213514         :ACCE-IDARTNR                                                    
213515        ,:ACCE-BEART                                                      
213516        ,:ACCE-IDPRODGR                                                   
213517        ,:ACCE-IDUPPDSU                                                   
213518        ,:ACCE-IDUPPDKU                                                   
213519        ,:ACCE-IDAOT                                                      
213520        ,:ACCE-TIAOINF                                                    
213521        ,:ACCE-BEASSTYP                                                   
213522        ,:ACCE-KDARTTYP                                                   
213523        ,:ACCE-KDMDS                                                      
213524        ,:ACCE-KDFRPTYP                                                   
213525        ,:ACCE-TEARTUTFG                                                  
213526        ,:ACCE-TESTATUPP                                                  
213527        ,:ACCE-IDLEVNR-GSDB                                               
213528        ,:ACCE-KDTPD-PH1                                                  
213529        ,:ACCE-DATPDPH1                                                   
213530        ,:ACCE-DAPSWQP-1                                                  
213531        ,:ACCE-KDPSWQP-1                                                  
213532        ,:ACCE-DAPSWQA-1                                                  
213533        ,:ACCE-KDPSWQA-1                                                  
213534        ,:ACCE-DAPSWPP-2                                                  
213535        ,:ACCE-KDPSWPP-2                                                  
213536        ,:ACCE-DAPSWPA-2                                                  
213537        ,:ACCE-KDPSWPA-2                                                  
213538        ,:ACCE-DAPSWCP-3                                                  
213539        ,:ACCE-KDPSWCP-3                                                  
213540        ,:ACCE-DAPSWCA-3                                                  
213541        ,:ACCE-KDPSWCA-3                                                  
213542        ,:ACCE-KVYVOL-B3                                                  
213543        ,:ACCE-KVYVOL-B2                                                  
213544        ,:ACCE-KVYVOL-INT                                                 
213545        ,:ACCE-KVYVOL-B1                                                  
213546        ,:ACCE-KVYVOL-ASS                                                 
213547        ,:ACCE-BEMAPP                                                     
213548        ,:ACCE-KVFOTO                                                     
213549        ,:ACCE-TIFOTO                                                     
213550        ,:ACCE-TENOTE                                                     
213551        ,:ACCE-FLANNULL                                                   
213552        ,:ACCE-TEVERKTYG                                                  
213553        ,:ACCE-TESTATXT                                                   
213554        ,:ACCE-TEMATXT                                                    
213555        ,:ACCE-TEINKTXT                                                   
213556        ,:ACCE-TEANSTXT                                                   
213557        ,:ACCE-TEAUXTXT                                                   
213558        ,:ACCE-IDARTNR-OFARG                                              
213559        ,:ACCE-IDPSLAG                                                    
213560        ,:ACCE-BETYP                                                      
213561        ,:ACCE-IDFKNGRP                                                   
213562        ,:ACCE-IDKDPPOS                                                   
213563        ,:ACCE-IDAOTUTG                                                   
213564        ,:ACCE-BEANST-KU                                                  
213565        ,:ACCE-BEANST-SU                                                  
213566        ,:ACCE-IDPSS                                                      
213567        ,:ACCE-DAPSWQP-1                                                  
213568        ,:ACCE-KDPSWQP-1                                                  
213569        ,:ACCE-DAPSWPP-2                                                  
213570        ,:ACCE-KDPSWPP-2                                                  
213571        ,:ACCE-DAPSWCP-3                                                  
213572        ,:ACCE-KDPSWCP-3                                                  
213573        ,:ACCE-KDFARGST                                                   
213574        ,:ACCE-IDPROJK                                                    
213575        ,:ACCE-VKART-KDP                                                  
213576        ,:ACCE-KDANNULL                                                   
213577        ,:ACCE-BEUPPDSU                                                   
213578     END-EXEC                                                             
213579     MOVE SQLCODE TO SQLCODE-WS                                           
213580     PERFORM DB2-STATUS-CONTROL                                           
213581     .                                                                    
213582     EJECT                                                                
213583*  - - - - - -                                                            
213584                                                                          
213585 DB2-SELECT-TB1ACCE-TAB  SECTION.                                         
213586     SKIP2                                                                
213587     MOVE 'DB2-SELECT-TB1ACCE-TAB       ' TO DB2-SEKTION                  
213588     MOVE 000100  TO GODK-SQLCODEKODER                                    
213589     EXEC SQL                                                             
213590       SELECT                                                             
213591          IDARTNR                                                         
213592         ,BEART                                                           
213600         ,IDPRODGR                                                        
213700         ,IDUPPDSU                                                        
213800         ,IDUPPDKU                                                        
213900         ,IDAOT                                                           
214000         ,TIAOINF                                                         
214100         ,BEASSTYP                                                        
214200         ,KDARTTYP                                                        
214400         ,KDMDS                                                           
214500         ,KDFRPTYP                                                        
214600         ,TEARTUTFG                                                       
214700         ,TESTATUPP                                                       
214800         ,IDLEVNR_GSDB                                                    
214900         ,KDTPD_PH1                                                       
215000         ,DATPDPH1                                                        
215100         ,DAPSWQP_1                                                       
215200         ,KDPSWQP_1                                                       
215300         ,DAPSWQA_1                                                       
215400         ,KDPSWQA_1                                                       
215500         ,DAPSWPP_2                                                       
215600         ,KDPSWPP_2                                                       
215700         ,DAPSWPA_2                                                       
215800         ,KDPSWPA_2                                                       
215900         ,DAPSWCP_3                                                       
216000         ,KDPSWCP_3                                                       
216100         ,DAPSWCA_3                                                       
216200         ,KDPSWCA_3                                                       
216300         ,KVYVOL_B3                                                       
216400         ,KVYVOL_B2                                                       
216600         ,KVYVOL_INT                                                      
216700         ,KVYVOL_B1                                                       
216800         ,KVYVOL_ASS                                                      
217200         ,BEMAPP                                                          
217300         ,KVFOTO                                                          
217400         ,TIFOTO                                                          
217500         ,TENOTE                                                          
217600         ,FLANNULL                                                        
217610         ,TEVERKTYG                                                       
217620         ,TESTATXT                                                        
217630         ,TEMATXT                                                         
217640         ,TEINKTXT                                                        
217650         ,TEANSTXT                                                        
217660         ,TEAUXTXT                                                        
217670         ,IDARTNR_OFARG                                                   
217680         ,IDPSLAG                                                         
217690         ,BETYP                                                           
217691         ,IDFKNGRP                                                        
217692         ,IDKDPPOS                                                        
217693         ,IDAOTUTG                                                        
217694         ,BEANST_KU                                                       
217695         ,BEANST_SU                                                       
217696         ,IDPSS                                                           
217697         ,DAPSWQP_1                                                       
217698         ,KDPSWQP_1                                                       
217699         ,DAPSWPP_2                                                       
217700         ,KDPSWPP_2                                                       
217701         ,DAPSWCP_3                                                       
217702         ,KDPSWCP_3                                                       
217703         ,KDFARGST                                                        
217704         ,IDPROJK                                                         
217705         ,VKART_KDP                                                       
217706         ,KDANNULL                                                        
217707         ,BEUPPDSU                                                        
217710       INTO                                                               
217800         :ACCE-IDARTNR                                                    
217900        ,:ACCE-BEART                                                      
218000        ,:ACCE-IDPRODGR                                                   
218100        ,:ACCE-IDUPPDSU                                                   
218200        ,:ACCE-IDUPPDKU                                                   
218300        ,:ACCE-IDAOT                                                      
218400        ,:ACCE-TIAOINF                                                    
218500        ,:ACCE-BEASSTYP                                                   
218600        ,:ACCE-KDARTTYP                                                   
218800        ,:ACCE-KDMDS                                                      
218900        ,:ACCE-KDFRPTYP                                                   
219000        ,:ACCE-TEARTUTFG                                                  
219100        ,:ACCE-TESTATUPP                                                  
219200        ,:ACCE-IDLEVNR-GSDB                                               
219300        ,:ACCE-KDTPD-PH1                                                  
219400        ,:ACCE-DATPDPH1                                                   
219500        ,:ACCE-DAPSWQP-1                                                  
219600        ,:ACCE-KDPSWQP-1                                                  
219700        ,:ACCE-DAPSWQA-1                                                  
219800        ,:ACCE-KDPSWQA-1                                                  
219900        ,:ACCE-DAPSWPP-2                                                  
220000        ,:ACCE-KDPSWPP-2                                                  
220100        ,:ACCE-DAPSWPA-2                                                  
220200        ,:ACCE-KDPSWPA-2                                                  
220300        ,:ACCE-DAPSWCP-3                                                  
220400        ,:ACCE-KDPSWCP-3                                                  
220500        ,:ACCE-DAPSWCA-3                                                  
220600        ,:ACCE-KDPSWCA-3                                                  
220700        ,:ACCE-KVYVOL-B3                                                  
220800        ,:ACCE-KVYVOL-B2                                                  
221000        ,:ACCE-KVYVOL-INT                                                 
221100        ,:ACCE-KVYVOL-B1                                                  
221200        ,:ACCE-KVYVOL-ASS                                                 
221600        ,:ACCE-BEMAPP                                                     
221700        ,:ACCE-KVFOTO                                                     
221800        ,:ACCE-TIFOTO                                                     
221900        ,:ACCE-TENOTE                                                     
222000        ,:ACCE-FLANNULL                                                   
222010        ,:ACCE-TEVERKTYG                                                  
222020        ,:ACCE-TESTATXT                                                   
222030        ,:ACCE-TEMATXT                                                    
222040        ,:ACCE-TEINKTXT                                                   
222050        ,:ACCE-TEANSTXT                                                   
222060        ,:ACCE-TEAUXTXT                                                   
222070        ,:ACCE-IDARTNR-OFARG                                              
222080        ,:ACCE-IDPSLAG                                                    
222090        ,:ACCE-BETYP                                                      
222091        ,:ACCE-IDFKNGRP                                                   
222092        ,:ACCE-IDKDPPOS                                                   
222093        ,:ACCE-IDAOTUTG                                                   
222094        ,:ACCE-BEANST-KU                                                  
222095        ,:ACCE-BEANST-SU                                                  
222096        ,:ACCE-IDPSS                                                      
222097        ,:ACCE-DAPSWQP-1                                                  
222098        ,:ACCE-KDPSWQP-1                                                  
222099        ,:ACCE-DAPSWPP-2                                                  
222100        ,:ACCE-KDPSWPP-2                                                  
222101        ,:ACCE-DAPSWCP-3                                                  
222102        ,:ACCE-KDPSWCP-3                                                  
222103        ,:ACCE-KDFARGST                                                   
222104        ,:ACCE-IDPROJK                                                    
222105        ,:ACCE-VKART-KDP                                                  
222106        ,:ACCE-KDANNULL                                                   
222107        ,:ACCE-BEUPPDSU                                                   
222110                                                                          
222200        FROM   TB1ACCE                                                    
222300                                                                          
222400        WHERE ( IDARTNR = :W-IDARTNR                                      
222500          AND  IDUPPDSU = :W-IDUPPDSU )                                   
222600                                                                          
222700     END-EXEC                                                             
222800                                                                          
222900     MOVE SQLCODE TO SQLCODE-WS                                           
223000     PERFORM DB2-STATUS-CONTROL                                           
223100     .                                                                    
223200     EJECT                                                                
223300                                                                          
223400 DB2-CLOSE-TB1ACCE-CRS-1  SECTION.                                        
223500     SKIP2                                                                
223600     MOVE 'DB2-CLOSE-TB1ACCE-CRS-1      ' TO DB2-SEKTION                  
223700     EXEC SQL                                                             
223800         CLOSE TB1ACCE-CRS-1                                              
223900     END-EXEC                                                             
224000     .                                                                    
224100     EJECT                                                                
224110 DB2-CLOSE-TB1ACCE-CRS-2  SECTION.                                        
224120     SKIP2                                                                
224130     MOVE 'DB2-CLOSE-TB1ACCE-CRS-2      ' TO DB2-SEKTION                  
224140     EXEC SQL                                                             
224160         CLOSE TB1ACCE-CRS-2                                              
224194     END-EXEC                                                             
224195     .                                                                    
224196     EJECT                                                                
224211 DB2-CLOSE-TB1ACCE-CRS-4  SECTION.                                        
224212     SKIP2                                                                
224213     MOVE 'DB2-CLOSE-TB1ACCE-CRS-4      ' TO DB2-SEKTION                  
224214     EXEC SQL                                                             
224215         CLOSE TB1ACCE-CRS-4                                              
224216     END-EXEC                                                             
224217     .                                                                    
224218     EJECT                                                                
224219 DB2-CLOSE-TB1ACCE-CRS-5  SECTION.                                        
224220     SKIP2                                                                
224221     MOVE 'DB2-CLOSE-TB1ACCE-CRS-5      ' TO DB2-SEKTION                  
224222     EXEC SQL                                                             
224223         CLOSE TB1ACCE-CRS-5                                              
224224     END-EXEC                                                             
224225     .                                                                    
224226     EJECT                                                                
224227 DB2-CLOSE-TB1ACCE-CRS-6  SECTION.                                        
224228     SKIP2                                                                
224229     MOVE 'DB2-CLOSE-TB1ACCE-CRS-6      ' TO DB2-SEKTION                  
224230     EXEC SQL                                                             
224231         CLOSE TB1ACCE-CRS-6                                              
224232     END-EXEC                                                             
224233     .                                                                    
224234     EJECT                                                                
224235 DB2-CLOSE-TB1ACCE-CRS-7  SECTION.                                        
224236     SKIP2                                                                
224237     MOVE 'DB2-CLOSE-TB1ACCE-CRS-7      ' TO DB2-SEKTION                  
224238     EXEC SQL                                                             
224239         CLOSE TB1ACCE-CRS-7                                              
224240     END-EXEC                                                             
224241     .                                                                    
224242     EJECT                                                                
224243 DB2-CLOSE-TB1ACCE-CRS-8  SECTION.                                        
224244     SKIP2                                                                
224245     MOVE 'DB2-CLOSE-TB1ACCE-CRS-8      ' TO DB2-SEKTION                  
224246     EXEC SQL                                                             
224247         CLOSE TB1ACCE-CRS-8                                              
224248     END-EXEC                                                             
224249     .                                                                    
224250     EJECT                                                                
224251 DB2-CLOSE-TB1ACCE-CRS-9  SECTION.                                        
224252     SKIP2                                                                
224253     MOVE 'DB2-CLOSE-TB1ACCE-CRS-9      ' TO DB2-SEKTION                  
224254     EXEC SQL                                                             
224255         CLOSE TB1ACCE-CRS-9                                              
224256     END-EXEC                                                             
224257     .                                                                    
224258     EJECT                                                                
224259 DB2-CLOSE-TB1ACCE-CRS-10 SECTION.                                        
224260     SKIP2                                                                
224261     MOVE 'DB2-CLOSE-TB1ACCE-CRS-10     ' TO DB2-SEKTION                  
224262     EXEC SQL                                                             
224263         CLOSE TB1ACCE-CRS-10                                             
224264     END-EXEC                                                             
224265     .                                                                    
224266     EJECT                                                                
224267 DB2-CLOSE-TB1ACCE-CRS-11 SECTION.                                        
224268     SKIP2                                                                
224269     MOVE 'DB2-CLOSE-TB1ACCE-CRS-11     ' TO DB2-SEKTION                  
224270     EXEC SQL                                                             
224271         CLOSE TB1ACCE-CRS-11                                             
224272     END-EXEC                                                             
224273     .                                                                    
224274     EJECT                                                                
224275 DB2-CLOSE-TB1ACCE-CRS-12 SECTION.                                        
224276     SKIP2                                                                
224277     MOVE 'DB2-CLOSE-TB1ACCE-CRS-12     ' TO DB2-SEKTION                  
224278     EXEC SQL                                                             
224279         CLOSE TB1ACCE-CRS-12                                             
224280     END-EXEC                                                             
224281     .                                                                    
224282     EJECT                                                                
224283 DB2-CLOSE-TB1ACCE-CRS-13 SECTION.                                        
224284     SKIP2                                                                
224285     MOVE 'DB2-CLOSE-TB1ACCE-CRS-13     ' TO DB2-SEKTION                  
224286     EXEC SQL                                                             
224287         CLOSE TB1ACCE-CRS-13                                             
224288     END-EXEC                                                             
224289     .                                                                    
224290     EJECT                                                                
224291 DB2-CLOSE-TB1ACCE-CRS-14 SECTION.                                        
224292     SKIP2                                                                
224293     MOVE 'DB2-CLOSE-TB1ACCE-CRS-14     ' TO DB2-SEKTION                  
224294     EXEC SQL                                                             
224295         CLOSE TB1ACCE-CRS-14                                             
224296     END-EXEC                                                             
224297     .                                                                    
224298     EJECT                                                                
224299 DB2-CLOSE-TB1ACCE-CRS-15 SECTION.                                        
224300     SKIP2                                                                
224301     MOVE 'DB2-CLOSE-TB1ACCE-CRS-15     ' TO DB2-SEKTION                  
224302     EXEC SQL                                                             
224303         CLOSE TB1ACCE-CRS-15                                             
224304     END-EXEC                                                             
224305     .                                                                    
224306     EJECT                                                                
224307 DB2-CLOSE-TB1ACCE-CRS-16 SECTION.                                        
224308     SKIP2                                                                
224309     MOVE 'DB2-CLOSE-TB1ACCE-CRS-16     ' TO DB2-SEKTION                  
224310     EXEC SQL                                                             
224311         CLOSE TB1ACCE-CRS-16                                             
224312     END-EXEC                                                             
224313     .                                                                    
224314     EJECT                                                                
224315 DB2-CLOSE-TB1ACCE-CRS-17 SECTION.                                        
224316     SKIP2                                                                
224317     MOVE 'DB2-CLOSE-TB1ACCE-CRS-17     ' TO DB2-SEKTION                  
224318     EXEC SQL                                                             
224319         CLOSE TB1ACCE-CRS-17                                             
224320     END-EXEC                                                             
224321     .                                                                    
224322     EJECT                                                                
224323 DB2-CLOSE-TB1ACCE-CRS-18 SECTION.                                        
224324     SKIP2                                                                
224325     MOVE 'DB2-CLOSE-TB1ACCE-CRS-18     ' TO DB2-SEKTION                  
224326     EXEC SQL                                                             
224327         CLOSE TB1ACCE-CRS-18                                             
224328     END-EXEC                                                             
224329     .                                                                    
224330     EJECT                                                                
224331 DB2-CLOSE-TB1ACCE-CRS-19 SECTION.                                        
224332     SKIP2                                                                
224333     MOVE 'DB2-CLOSE-TB1ACCE-CRS-19     ' TO DB2-SEKTION                  
224334     EXEC SQL                                                             
224335         CLOSE TB1ACCE-CRS-19                                             
224336     END-EXEC                                                             
224337     .                                                                    
224338     EJECT                                                                
224339 DB2-CLOSE-TB1ACCE-CRS-20 SECTION.                                        
224340     SKIP2                                                                
224341     MOVE 'DB2-CLOSE-TB1ACCE-CRS-20     ' TO DB2-SEKTION                  
224342     EXEC SQL                                                             
224343         CLOSE TB1ACCE-CRS-20                                             
224344     END-EXEC                                                             
224345     .                                                                    
224346     EJECT                                                                
224347 DB2-CLOSE-TB1ACCE-CRS-21 SECTION.                                        
224348     SKIP2                                                                
224349     MOVE 'DB2-CLOSE-TB1ACCE-CRS-21     ' TO DB2-SEKTION                  
224350     EXEC SQL                                                             
224351         CLOSE TB1ACCE-CRS-21                                             
224352     END-EXEC                                                             
224353     .                                                                    
224354     EJECT                                                                
224355 DB2-CLOSE-TB1ACCE-CRS-22 SECTION.                                        
224356     SKIP2                                                                
224357     MOVE 'DB2-CLOSE-TB1ACCE-CRS-22     ' TO DB2-SEKTION                  
224358     EXEC SQL                                                             
224359         CLOSE TB1ACCE-CRS-22                                             
224360     END-EXEC                                                             
224361     .                                                                    
224362     EJECT                                                                
224363 DB2-CLOSE-TB1ACCE-CRS-23 SECTION.                                        
224364     SKIP2                                                                
224365     MOVE 'DB2-CLOSE-TB1ACCE-CRS-23     ' TO DB2-SEKTION                  
224366     EXEC SQL                                                             
224367         CLOSE TB1ACCE-CRS-23                                             
224368     END-EXEC                                                             
224369     .                                                                    
224370     EJECT                                                                
224371 DB2-CLOSE-TB1ACCE-CRS-24 SECTION.                                        
224372     SKIP2                                                                
224373     MOVE 'DB2-CLOSE-TB1ACCE-CRS-24     ' TO DB2-SEKTION                  
224374     EXEC SQL                                                             
224375         CLOSE TB1ACCE-CRS-24                                             
224376     END-EXEC                                                             
224377     .                                                                    
224378     EJECT                                                                
224379 DB2-CLOSE-TB1ACCE-CRS-25 SECTION.                                        
224380     SKIP2                                                                
224381     MOVE 'DB2-CLOSE-TB1ACCE-CRS-25     ' TO DB2-SEKTION                  
224382     EXEC SQL                                                             
224383         CLOSE TB1ACCE-CRS-25                                             
224384     END-EXEC                                                             
224385     .                                                                    
224386     EJECT                                                                
224387 DB2-STATUS-CONTROL   SECTION.                                            
224390                                                                          
224400     SET SQLCODE-IX TO 1                                                  
224500     SEARCH GODK-SQLCODE                                                  
224600       AT END                                                             
224700          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
224800          DELIMITED BY SIZE INTO FELTEXT                                  
224900          CALL ABEND USING RKOD-ABEND-DB2                                 
225000       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
225100     END-SEARCH                                                           
225200     .                                                                    
