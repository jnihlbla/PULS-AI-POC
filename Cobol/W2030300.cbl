000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2030300.                                                
000400 AUTHOR.         LARS THELL      (MG).                                    
000500 DATE-WRITTEN.   90/12/18.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SATSRADER ÅTGÄRD                                                 
001100*                                                                         
001200*        PROGRAMMET UPPATERAR WLSATG (WDJ2)                               
001300*        PROGRAMMET UPPATERAR WLARTC (WDK6)                               
001400*        PROGRAMMET UPPATERAR WLORDP (WDA5)                               
001500*        PROGRAMMET LÄSER     WLBENA (WDD3)                               
001600*        PROGRAMMET LÄSER     WLARTC (WDK6)                               
001700*        PROGRAMMET LÄSER     WLORDR (WDA5B)                              
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W2T303                                              
002100*        MID:         W2I30101                                            
002200*        MID:         W2I30301                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W2O30301                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W2030300'.            
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100                                                                          
004200 01  FILLER                      PIC X(15) VALUE 'ABENDKODER '.           
004300 01  ABENDKODER.                                                          
004400  03 ABEND-MED-DUMP              PIC S9(4)  VALUE +1000 COMP SYNC.        
004500  03 ABEND-UTAN-DUMP             PIC S9(4)  VALUE +16   COMP SYNC.        
004600*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004700 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004800 77  MAX-RAD-2301                PIC S9(4)  VALUE +14   COMP SYNC.        
004900 77  MAX-RAD-2303                PIC S9(4)  VALUE +8    COMP SYNC.        
005000 77  MAX-RAD-MID2                PIC S9(4)  VALUE +18   COMP SYNC.        
005100 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005200 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +703  COMP SYNC.        
005300 77  MAX-CLAGER                  PIC S9(4)  VALUE +2    COMP SYNC.        
005400                                                                          
005500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005600 01  WS-IDORDNST.                                                         
005700   05  WS-IDORDNSB               PIC 9(4)    VALUE ZERO.                  
005800   05  WS-IDORDNSS               PIC 9(1)    VALUE ZERO.                  
005900 01    WS-IDANSK-FOM             PIC X(3)    VALUE SPACE.                 
006000 01    WS-IDANSK-TOM             PIC X(3)    VALUE SPACE.                 
006100 01    WS-FLBYGGB                PIC X(1)    VALUE SPACE.                 
006200 01    WS-IDARTNR                PIC X(9)    VALUE SPACE.                 
006300 01    WS-ING-IDARTNR            PIC X(9)    VALUE SPACE.                 
006400 01    WS-KDSATKMB               PIC X(1)    VALUE SPACE.                 
006500 01    W-IDARTNR-RAD             PIC X(9)    VALUE SPACE.                 
006600                                                                          
006700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006800     88  INDATA-OK                           VALUE 'J'.                   
006900     88  INDATA-FEL                          VALUE 'N'.                   
007000                                                                          
007100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007200     88  NYCKLAR-OK                          VALUE 'J'.                   
007300     88  NYCKLAR-FEL                         VALUE 'N'.                   
007400                                                                          
007500 77  ALLT-SW                     PIC X       VALUE 'J'.                   
007600     88  ALLT-OK                             VALUE 'J'.                   
007700                                                                          
007800 77  CMD-IFYLLT-SW               PIC X       VALUE 'N'.                   
007900     88  CMD-IFYLLT                          VALUE 'J'.                   
008000                                                                          
008100 77  ING-IDARTNR-SW              PIC X       VALUE 'N'.                   
008200     88  ING-IDARTNR-FINNS                   VALUE 'J'.                   
008300     88  ING-IDARTNR-SAKNAS                  VALUE 'N'.                   
008400                                                                          
008500 77  ING-IDARTNR-SPARRAD-SW      PIC X       VALUE 'N'.                   
008600     88  ING-IDARTNR-SPARRAD                 VALUE 'J'.                   
008700                                                                          
008800 77  KDSATAND1-SW                PIC X       VALUE 'N'.                   
008900     88  KDSATAND1-OK                        VALUE 'J'.                   
009000                                                                          
009100 77  HOPP-FRAN-2301-SW           PIC X       VALUE 'N'.                   
009200     88  HOPP-FRAN-2301                      VALUE 'J'.                   
009300                                                                          
009400 77  REBEART-MINSKNING-SW        PIC X       VALUE 'N'.                   
009500     88  REBEART-MINSKNING                   VALUE 'J'.                   
009600                                                                          
009700 77  KOMB-TRAFF-SW               PIC X       VALUE 'N'.                   
009800     88 KOMB-TRAFF                           VALUE 'J'.                   
009900                                                                          
010000 77  KOMB-KVBYGGB-TRAFF-SW       PIC X       VALUE 'N'.                   
010100     88 KOMB-KVBYGGB-TRAFF                   VALUE 'J'.                   
010200                                                                          
010300 77  SATS-SPARRAD-SW             PIC X       VALUE 'N'.                   
010400     88 SATS-SPARRAD                         VALUE 'J'.                   
010500                                                                          
010600 77  REANTPSA-ANDRAD-SW          PIC X       VALUE 'N'.                   
010700     88 REANTPSA-ANDRAD                      VALUE 'J'.                   
010800                                                                          
010900 77  SW-PPSW-2109                PIC X       VALUE 'N'.                   
011000                                                                          
011100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011200     88  EGEN-MID                            VALUE '2303'.                
011300     88  GODK-MID                            VALUE '2301' '2302'          
011400                                                   '2303'.                
011500 01  W-IDARTNR-11.                                                        
011600   05  W-IDARTNR1-9              PIC 9(9)    VALUE ZERO.                  
011700   05  FILLER                    PIC X(1)    VALUE '-'.                   
011800   05  W-REKSIFFR                PIC 9(1)    VALUE ZERO.                  
011900                                                                          
012000 77  W-OLD-KVSATRES              PIC S9(7)   VALUE ZERO  COMP-3.          
012100 77  W1-KVBYGGB                  PIC S9(7)      VALUE ZERO COMP-3.        
012200 77  W2-KVBYGGB                  PIC S9(7)      VALUE ZERO COMP-3.        
012300 77  W-SHUV-KDSATKMB             PIC  X(1)   VALUE SPACE.                 
012400 77  W-KDSATKMB-UPDATE           PIC  X(1)   VALUE SPACE.                 
012500 77  W-KDSATKMB-KVBYGGB          PIC S9(7)      VALUE ZERO COMP-3.        
012600 77  W-KVBYGGB                   PIC S9(7)      VALUE ZERO COMP-3.        
012700 77  W-MAX-KVBYGGB               PIC S9(7)      VALUE ZERO COMP-3.        
012800 77  W-KVBEART                   PIC S9(7)      VALUE ZERO COMP-3.        
012900 77  W-SPAR-KVSATRES             PIC S9(7)      VALUE ZERO COMP-3.        
013000 77  W-SPAR-KVSATROS             PIC S9(7)      VALUE ZERO COMP-3.        
013100 77  W-KVSATROS                  PIC S9(7)      VALUE ZERO COMP-3.        
013200 77  W-KVSATRES                  PIC S9(7)      VALUE ZERO COMP-3.        
013300 77  W-KVRADER                   PIC S9(3)      VALUE ZERO COMP-3.        
013400 77  W-ING-IDARTNR-UPDATE        PIC  9(9)      VALUE ZERO.               
013500 77  W-ING-IDARTNR1-UPDATE       PIC  9(9)      VALUE ZERO.               
013600 77  W-ING-IDARTNR2-UPDATE       PIC  9(9)      VALUE ZERO.               
013700 77  W-REBEART-UPDATE            PIC  9(7)      VALUE ZERO.               
013800 77  W-REBEART1-UPDATE           PIC  9(7)V9(1) VALUE ZERO.               
013900 77  W-REBEART2-UPDATE           PIC  9(7)V9(1) VALUE ZERO.               
014000 77  W-REANTPSA-UPDATE           PIC  9(2)V9(3) VALUE ZERO.               
014100 77  W-REANTPSA1-UPDATE          PIC  9(2)V9(3) VALUE ZERO.               
014200 77  W-REANTPSA2-UPDATE          PIC  9(2)V9(3) VALUE ZERO.               
014300 77  W-PRARTSTD                  PIC S9(7)V9(2) VALUE ZERO COMP-3.        
014400 77  W-EJ-TACKBART               PIC S9(7)      VALUE ZERO COMP-3.        
014500 77  W-OKNING                    PIC S9(7)      VALUE ZERO COMP-3.        
014600 77  W-RZD-OKNING                PIC S9(7)      VALUE ZERO COMP-3.        
014700 77  W-MINSKNING                 PIC S9(7)      VALUE ZERO COMP-3.        
014800 77  W-RZD-MINSKNING             PIC S9(7)      VALUE ZERO COMP-3.        
014900 77  W-SKILLNAD                  PIC S9(7)      VALUE ZERO COMP-3.        
015000 77  W-TILLG-KVLS                PIC S9(9)      VALUE ZERO COMP-3.        
015100 77  W-KVPREAVB                  PIC S9(9)      VALUE ZERO COMP-3.        
015200 77  W-MULTIPEL                  PIC S9(9)      VALUE ZERO COMP-3.        
015300 77  W-REST                      PIC S9(7)V9(4) VALUE ZERO COMP-3.        
015400 77  W-KDROO-1                   PIC  9(1)      VALUE 1.                  
015500 77  W-KDROO                     PIC  9(1)      VALUE ZERO.               
015600 77  W-KVRO                      PIC S9(7)      VALUE ZERO COMP-3.        
015700 77  W-KVART                     PIC S9(7)      VALUE ZERO COMP-3.        
015800 77  W-DATUM                     PIC  X(6)      VALUE SPACE.              
015900 77  W-DATUM-Y2K                 PIC  9(8)      VALUE ZERO.               
016000 77  W-TID                       PIC  X(8)      VALUE SPACE.              
016100 77  W-VKARTNTO                  PIC S9(4)V9(3) VALUE ZERO COMP-3.        
016200 77  W-VLARTNTO                  PIC S9(8)V9(1) VALUE ZERO COMP-3.        
016300 77  W-ANTAL-I-MID               PIC S9(3)      VALUE ZERO COMP-3.        
016400 01  W-HELP-IDORDNST.                                                     
016500  05 W-HELP-IDORDNSB             PIC  9(4)      VALUE ZERO.               
016600  05 W-HELP-IDORDNSS             PIC  9(1)      VALUE ZERO.               
016700                                                                          
016800 01  FILLER                      PIC X(12)   VALUE 'WS-KDSATKOMB'.        
016900 01  WS-KDSATKOMB.                                                        
017000     03  FILLER                  PIC X(13) VALUE 'ABCDEFGHIJKLM'.         
017100     03  FILLER                  PIC X(13) VALUE 'NOPQRSTUVWXYZ'.         
017200 01  FILLLER REDEFINES WS-KDSATKOMB.                                      
017300     03 KOMB-TAB-RAD      OCCURS 26                                       
017400                          INDEXED BY KOMB-IDX1.                           
017500        05 KOMB-BOKSTAV   PIC X.                                          
017600*                                                                         
017700     SKIP3                                                                
017800 01  FILLER                   PIC X(16) VALUE 'KOMB-KVBYGGB-TAB'.         
017900 01  KOMB-KVBYGGB-TAB.                                                    
018000     03 KOMB-KVBYGGB-RAD  OCCURS 26                                       
018100                          INDEXED BY KOMB-IDX2.                           
018200       05 KOMB-KVBYGGB-ING-IDARTNR  PIC S9(9)  COMP-3.                    
018300       05 KOMB-KVBYGGB-KDSATKMB     PIC X.                                
018400       05 KOMB-KVBYGGB              PIC S9(7)      COMP-3.                
018500     EJECT                                                                
018600*01  -COPY WDJ211     -PRE W1-                                            
018700     EJECT                                                                
018800*01  -COPY WDJ211     -PRE W2-                                            
018900     EJECT                                                                
019000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
019100 01  GENERELLA-SUBPROGRAM.                                                
019200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
019300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
019400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
019500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
019600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
019700     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
019800     03  W416PTID                PIC X(8)    VALUE 'W416PTID'.            
019900     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
020000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
020100     EJECT                                                                
020200*      --- VALID IDDC CODES                                               
020300*                                                                         
020400*01    -COPY WWDCKONS                                                     
020500*                                                                         
020600*01    -COPY WWPRODSL                                                     
020700                                                                          
020800*      --- BYTESARTIKLAR                                                  
020900*                                                                         
021000*01    -COPY WWBYT03                                                      
021100       EJECT                                                              
021200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
021300*   -COPY WMSGINIT                                                        
021400     EJECT                                                                
021500*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
021600*   -COPY WDECAREA                                                        
021700     EJECT                                                                
021800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
021900*   -COPY WMEDAREA                                                        
022000     EJECT                                                                
022100*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
022200*   -COPY WDATAREA                                                        
022300     EJECT                                                                
022400 01  MESSAGE-CODES.                                                       
022500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
022600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
022700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
022800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
022900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
023000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
023100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
023200     EJECT                                                                
023300*    --- LÄNKAREA TILL SUBPROGRAM W416PTID                                
023400*   -COPY W416PTID                                                        
023500     SKIP3                                                                
023600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
023700*                                                                         
023800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
023900     SKIP3                                                                
024000*01  MID -COPY W2I30101        -PRE 2301-                                 
024100     EJECT                                                                
024200*01  MID -COPY W2I30301        -PRE 2303-                                 
024300     EJECT                                                                
024400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
024500     SKIP3                                                                
024600*01  -COPY WMSGAREA                                                       
024700     EJECT                                                                
024800     03  MOD REDEFINES MSG-AREA.                                          
024900*      05  -COPY W2O30301                                                 
025000     EJECT                                                                
025100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
025200     SKIP3                                                                
025300*01  -COPY WMFSAREA                                                       
025400     EJECT                                                                
025500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025600*                                                                         
025700     EJECT                                                                
025800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025900     SKIP3                                                                
026000 01  NYCKLAR-TILL-DLI.                                                    
026100     03  W-WDA501KY-X.                                                    
026200         05  W-A501KY-IDDISTR       PIC S9(5)   VALUE ZERO COMP-3.        
026300         05  W-A501KY-IDKUNDNR      PIC S9(7)   VALUE ZERO COMP-3.        
026400         05  W-A501KY-IDKUNDRF      PIC X(10)   VALUE SPACE.              
026500         05  W-A501KY-IDARTNR       PIC S9(9)   VALUE ZERO COMP-3.        
026600         05  W-A501KY-IDLOPNR       PIC S9(3)   VALUE ZERO COMP-3.        
026700                                                                          
026800     03  W-WDA501KY-MIN-X.                                                
026900         05  W-A501KY-MIN-IDDISTR   PIC S9(5)   VALUE ZERO COMP-3.        
027000         05  W-A501KY-MIN-IDKUNDNR  PIC S9(7)   VALUE ZERO COMP-3.        
027100         05  W-A501KY-MIN-IDKUNDRF.                                       
027200          07 W-A501KY-MIN-IDORDNR5  PIC  9(5)   VALUE ZERO.               
027300          07 FILLER                 PIC  X(5)   VALUE SPACE.              
027400         05  W-A501KY-MIN-IDARTNR   PIC S9(9)   VALUE ZERO COMP-3.        
027500         05  W-A501KY-MIN-IDLOPNR   PIC S9(3)   VALUE ZERO COMP-3.        
027600                                                                          
027700     03  W-WDA501KY-MAX-X.                                                
027800         05  W-A501KY-MAX-IDDISTR   PIC S9(5)   VALUE ZERO COMP-3.        
027900         05  W-A501KY-MAX-IDKUNDNR  PIC S9(7)   VALUE ZERO COMP-3.        
028000         05  W-A501KY-MAX-IDKUNDRF.                                       
028100          07 W-A501KY-MAX-IDORDNR5  PIC  9(5)   VALUE ZERO.               
028200          07 FILLER                 PIC  X(5)   VALUE SPACE.              
028300         05  W-A501KY-MAX-IDARTNR   PIC S9(9)   VALUE ZERO COMP-3.        
028400         05  W-A501KY-MAX-IDLOPNR   PIC S9(3)   VALUE ZERO COMP-3.        
028500                                                                          
028600     03  W-WDA5B1KY-MIN-X.                                                
028700         05  W-A5B1KY-MIN-IDDISTR   PIC S9(5)   VALUE ZERO COMP-3.        
028800         05  W-A5B1KY-MIN-IDKUNDNR  PIC S9(7)   VALUE ZERO COMP-3.        
028900         05  W-A5B1KY-MIN-IDDC      PIC X(2)    VALUE SPACE.              
029000         05  W-A5B1KY-MIN-IDARTNR   PIC S9(9)   VALUE ZERO COMP-3.        
029100         05  W-A5B1KY-MIN-IDKUNDRF.                                       
029200          07 W-A5B1KY-MIN-IDORDNR5  PIC  9(5)   VALUE ZERO.               
029300          07 FILLER                 PIC  X(5)   VALUE SPACE.              
029400         05  W-A5B1KY-MIN-IDLOPNR   PIC S9(3)   VALUE ZERO COMP-3.        
029500                                                                          
029600     03  W-WDA5B1KY-MAX-X.                                                
029700         05  W-A5B1KY-MAX-IDDISTR   PIC S9(5)   VALUE ZERO COMP-3.        
029800         05  W-A5B1KY-MAX-IDKUNDNR  PIC S9(7)   VALUE ZERO COMP-3.        
029900         05  W-A5B1KY-MAX-IDDC      PIC X(2)    VALUE SPACE.              
030000         05  W-A5B1KY-MAX-IDARTNR   PIC S9(9)   VALUE ZERO COMP-3.        
030100         05  W-A5B1KY-MAX-IDKUNDRF.                                       
030200          07 W-A5B1KY-MAX-IDORDNR5  PIC  9(5)   VALUE ZERO.               
030300          07 FILLER                 PIC  X(5)   VALUE SPACE.              
030400         05  W-A5B1KY-MAX-IDLOPNR   PIC S9(3)   VALUE ZERO COMP-3.        
030500                                                                          
030600     03  W-WDJ201KY-X.                                                    
030700         05  W-WDJ201KY          PIC S9(5)   VALUE ZERO COMP-3.           
030800     03  W-IDARTNR-X.                                                     
030900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
031000     03  W-ING-IDARTNR-X.                                                 
031100         05  W-ING-IDARTNR       PIC S9(9)   VALUE ZERO COMP-3.           
031200     03  W-KDSATKMB-MIN-X.                                                
031300         05  W-KDSATKMB-MIN      PIC  X(1)   VALUE SPACE.                 
031400     03  W-KDSATKMB-MAX-X.                                                
031500         05  W-KDSATKMB-MAX      PIC  X(1)   VALUE SPACE.                 
031600     03  W-SATG2-KDSATKMB-MIN-X.                                          
031700         05  W-SATG2-KDSATKMB-MIN  PIC  X(1)   VALUE SPACE.               
031800     03  W-SATG2-KDSATKMB-MAX-X.                                          
031900         05  W-SATG2-KDSATKMB-MAX  PIC  X(1)   VALUE SPACE.               
032000     03  W-KDSEGKEY-X.                                                    
032100         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
032200     03  W-IDORDNST-X.                                                    
032300         05  W-IDORDNSB          PIC S9(5)   VALUE ZERO COMP-3.           
032400         05  W-IDORDNSS          PIC S9(1)   VALUE ZERO COMP-3.           
032500     03  W-WDD3BSEQ-X.                                                    
032600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
032700     03  W-IDSKYLT-X.                                                     
032800         05  W-IDSKYLT           PIC  X(3)   VALUE SPACE.                 
032900                                                                          
033000     03  W-4511-IDHTYP-X.                                                 
033100         05  W-4511-IDHTYP       PIC  X(04) VALUE '4511'.                 
033200         05  W-LOW-VALUE         PIC  X(26) VALUE LOW-VALUE.              
033300                                                                          
033400     03  W-4512-KDTPOTYP-X.                                               
033500         05  W-4512-KDTPOTYP     PIC  S9(1) COMP-3.                       
033600     03  W-4512-KDORDKL-X.                                                
033700         05  W-4512-KDORDKL      PIC  S9(1) COMP-3.                       
033800     03  W-4512-IDDISTR-FOM-X.                                            
033900         05  W-4512-IDDISTR-FOM  PIC  S9(5) COMP-3.                       
034000     03  W-4512-IDDISTR-TOM-X.                                            
034100         05  W-4512-IDDISTR-TOM  PIC  S9(5) COMP-3.                       
034200     SKIP2                                                                
034300*    --- STATUS-KOD FRÅN IMS                                              
034400 01  STATUS-WS                   PIC XX.                                  
034500     88  SEGMENT-FINNS                       VALUE '  '.                  
034600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
034700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
034800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
034900     SKIP2                                                                
035000 01  GODK-STATUSKODER.                                                    
035100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
035200     SKIP3                                                                
035300 01  SSA1                        PIC X(96).                               
035400 01  SSA2                        PIC X(64).                               
035500 01  SSA3                        PIC X(64).                               
035600     EJECT                                                                
035700*    --- IMS FUNKTIONSKODER                                               
035800*01  -COPY W0003                                                          
035900     EJECT                                                                
036000*    ---  DLI INPUT-OUTPUT AREA                                           
036100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
036200     SKIP3                                                                
036300 01  DLI-IO-AREA1.                                                        
036400     03  WLSATG01.                                                        
036500*        05  -COPY WDJ201                                                 
036600     EJECT                                                                
036700 01  DLI-IO-AREA2.                                                        
036800     03  WLSATG11.                                                        
036900*        05  -COPY WDJ211                                                 
037000     EJECT                                                                
037100 01  DLI-IO-AREA3.                                                        
037200     03  WLBENA11.                                                        
037300*        05  -COPY WDD311                                                 
037400     EJECT                                                                
037500 01  DLI-IO-AREA4.                                                        
037600     03  WLORDP01.                                                        
037700*        05  -COPY WDA501                                                 
037800     EJECT                                                                
037900 01  DLI-IO-AREA5.                                                        
038000     03  WLORDR01.                                                        
038100*        05  -COPY WDA5B1                                                 
038200     EJECT                                                                
038300 01  DLI-IO-AREA6.                                                        
038400     03  WLXXJN11.                                                        
038500*        05  -COPY WDGX4512                                               
038600     EJECT                                                                
038700 01  DLI-IO-AREA7.                                                        
038800     03  WLARTM01.                                                        
038900*        05  -COPY WDK901                                                 
039000     EJECT                                                                
039100 01  DLI-IO-AREA10.                                                       
039200     03  IO-AREA10               PIC X(900)  VALUE SPACE.                 
039300     03  WLARTC01 REDEFINES IO-AREA10.                                    
039400*        05  -COPY WDK601                                                 
039500     EJECT                                                                
039600     03  WLARTC11 REDEFINES IO-AREA10.                                    
039700*        05  -COPY WDK611   -PRE C3-                                      
039800     EJECT                                                                
039900 01  DLI-IO-AREA11.                                                       
040000     03  WLARTC11.                                                        
040100*        05  -COPY WDK611                                                 
040200     EJECT                                                                
040300 01  DLI-IO-AREA12.                                                       
040400     03  WLARTC11.                                                        
040500*        05  -COPY WDK611   -PRE C2-                                      
040600     EJECT                                                                
040700*    ---  MSG INPUT-OUTPUT AREA  W006KOM                                  
040800 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
040900                                                                          
041000*01  -COPY WMSGAREA -PRE K                                                
041100     EJECT                                                                
041200     05  FILLER REDEFINES KMSG-MID-OUT.                                   
041300        07  -COPY W2I10902  -PRE KOM-.                                    
041400     EJECT                                                                
041500*    ---  AREA FÖR W006KOM SUBMODUL                                       
041600 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
041700     SKIP3                                                                
041800 01  KOM-IO-AREA.                                                         
041900*    03  -COPY WMSGKOM                                                    
042000     EJECT                                                                
042100 LINKAGE SECTION.                                                         
042200                                                                          
042300*01  -COPY W0009      -PRE MSG-                                           
042400     EJECT                                                                
042500*01  -COPY W0009      -PRE ALT-                                           
042600     EJECT                                                                
042700*01  -COPY W0008      -PRE KOMA-                                          
042800     05  FILLER                  PIC X.                                   
042900     EJECT                                                                
043000*01  -COPY W0008      -PRE USEA-                                          
043100     05  FILLER                  PIC X.                                   
043200     EJECT                                                                
043300*01  -COPY W0008      -PRE SATG1-                                         
043400     05  FILLER                  PIC X.                                   
043500*01  -COPY W0008      -PRE SATG2-                                         
043600     05  FILLER                  PIC X.                                   
043700     EJECT                                                                
043800*01  -COPY W0008      -PRE ARTC1-                                         
043900     05  FILLER                  PIC X.                                   
044000     EJECT                                                                
044100*01  -COPY W0008      -PRE ORDP-                                          
044200     05  FILLER                  PIC X.                                   
044300     EJECT                                                                
044400*01  -COPY W0008      -PRE BENA-                                          
044500     05  FILLER                  PIC X.                                   
044600     EJECT                                                                
044700*01  -COPY W0008      -PRE ARTC3-                                         
044800     05  FILLER                  PIC X.                                   
044900*01  -COPY W0008      -PRE ORDR-                                          
045000     05  FILLER                  PIC X.                                   
045100     EJECT                                                                
045200*01  -COPY W0008      -PRE ARTC2-                                         
045300     05  FILLER                  PIC X.                                   
045400     EJECT                                                                
045500*01  -COPY W0008      -PRE XXJN-                                          
045600     05  FILLER                  PIC X.                                   
045700     EJECT                                                                
045800*01  -COPY W0008      -PRE ARTM-                                          
045900     05  FILLER                  PIC X.                                   
046000     EJECT                                                                
046100****** PCB FÖR SUB PGM W416PTID                                           
046200 01 XXKH-PCB                     PIC X.                                   
046300                                                                          
046400 01 XXKI-PCB                     PIC X.                                   
046500                                                                          
046600 01 SATB-PCB                     PIC X.                                   
046700     EJECT                                                                
046800 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB KOMA-PCB USEA-PCB              
046900                                   SATG1-PCB SATG2-PCB ARTC1-PCB          
047000                                   ORDP-PCB BENA-PCB                      
047100                                   ARTC3-PCB ORDR-PCB                     
047200                                   ARTC2-PCB XXJN-PCB ARTM-PCB            
047300                                   XXKH-PCB XXKI-PCB SATB-PCB.            
047400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB KOMA-PCB USEA-PCB              
047500                                   SATG1-PCB SATG2-PCB ARTC1-PCB          
047600                                   ORDP-PCB BENA-PCB                      
047700                                   ARTC3-PCB ORDR-PCB                     
047800                                   ARTC2-PCB XXJN-PCB ARTM-PCB            
047900                                   XXKH-PCB XXKI-PCB SATB-PCB.            
048000                                                                          
048100     PERFORM IMS-GET-MSG                                                  
048200     IF SEGMENT-FINNS                                                     
048300       PERFORM A-INIT                                                     
048400       PERFORM B-KOLLA-NYCKLAR                                            
048500       IF NYCKLAR-OK                                                      
048600         IF MFS-UPDATE                                                    
048700           PERFORM G-KOLLA-INPUT                                          
048800           IF INDATA-OK                                                   
048900             PERFORM H-UPPDATERA                                          
049000             IF SW-PPSW-2109 = JA                                         
049100                PERFORM K-PPSW-2109-DISPATCH                              
049200             END-IF                                                       
049300           END-IF                                                         
049400         ELSE                                                             
049500           IF MFS-FIRST                                                   
049600             PERFORM C-FOERSTA-SIDA                                       
049700           ELSE                                                           
049800             IF MFS-NEXT                                                  
049900               PERFORM D-NAESTA-SIDA                                      
050000             ELSE                                                         
050100               PERFORM E-SAMMA-SIDA                                       
050200             END-IF                                                       
050300           END-IF                                                         
050400         END-IF                                                           
050500         IF ALLT-OK AND INDATA-OK                                         
050600             PERFORM F-LAES-VISA-INFO                                     
050700         END-IF                                                           
050800       END-IF                                                             
050900       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
051000       PERFORM IMS-INSERT-MSG                                             
051100     END-IF                                                               
051200                                                                          
051300     MOVE ZERO TO RETURN-CODE                                             
051400     GOBACK                                                               
051500     .                                                                    
051600     EJECT                                                                
051700 A-INIT SECTION.                                                          
051800                                                                          
051900     ACCEPT W-DATUM            FROM DATE                                  
052000     ACCEPT W-TID              FROM TIME                                  
052100     MOVE FUNCTION CURRENT-DATE (1:8) TO W-DATUM-Y2K                      
052200                                                                          
052300     MOVE NEJ                  TO  CMD-IFYLLT-SW                          
052400                                   HOPP-FRAN-2301-SW                      
052500                                   KDSATAND1-SW                           
052600                                   REBEART-MINSKNING-SW                   
052700                                   REANTPSA-ANDRAD-SW                     
052800                                   SW-PPSW-2109                           
052900                                                                          
053000     MOVE ZERO                 TO  W-VLARTNTO                             
053100                                   W-VKARTNTO                             
053200                                                                          
053300     MOVE ZERO                 TO  W-ANTAL-I-MID                          
053400                                                                          
053500     PERFORM AA-INIT-KOMB-TAB                                             
053600                                                                          
053700     IF MSG-IDTRANS-2      =  '2301'                                      
053800         PERFORM AB-BEHANDLA-2301-MID                                     
053900      ELSE                                                                
054000                                                                          
054100         IF MSG-DUBBLA-TRANSKODER                                         
054200           MOVE MSG-INDATA-MINUS-2-TRANSKODER TO 2303-MID-W2I30301        
054300           MOVE MSG-IDTRANS-2             TO MFS-IDTRANS                  
054400           MOVE MSG-KDMFSFOR-2            TO MFS-KDMFSFOR                 
054500         ELSE                                                             
054600           MOVE MSG-INDATA-MINUS-1-TRANSKOD TO 2303-MID-W2I30301          
054700           MOVE MSG-IDTRANS-1            TO MFS-IDTRANS                   
054800           MOVE MSG-KDMFSFOR-1           TO MFS-KDMFSFOR                  
054900         END-IF                                                           
055000     END-IF                                                               
055100                                                                          
055200     MOVE MSG-KDTRTYP                    TO MFS-KDTRTYP                   
055300     MOVE MSG-IDPFK                      TO MFS-IDPFK                     
055400     MOVE MFS-IDTRANS                    TO W-IDTRANS                     
055500                                                                          
055600     MOVE LOW-VALUE                      TO MSG-AREA                      
055700     MOVE 'W2O30301'                     TO MFS-IDMOD                     
055800     MOVE '2303'                         TO MOD-IDTRANS                   
055900     MOVE MFS-RENSA-FAELT                TO MOD-TEMFSFEL                  
056000                                            MOD-TEMFSINF                  
056100                                                                          
056200     IF NOT EGEN-MID                                                      
056300       MOVE SPACE              TO MFS-KDTRTYP                             
056400       MOVE '7'                TO MFS-IDPFK                               
056500     END-IF                                                               
056600                                                                          
056700     IF ENGLISH-TEXT                                                      
056800       MOVE +2                 TO SPRAK-IX                                
056900       MOVE 'GB '              TO MED-IDSKYLT                             
057000     ELSE                                                                 
057100       MOVE +1                 TO SPRAK-IX                                
057200       MOVE 'S  '              TO MED-IDSKYLT                             
057300     END-IF                                                               
057400     .                                                                    
057500     EJECT                                                                
057600 AA-INIT-KOMB-TAB      SECTION.                                           
057700                                                                          
057800     SET KOMB-IDX2             TO +1                                      
057900     PERFORM UNTIL KOMB-IDX2   > 26                                       
058000         MOVE ZERO     TO KOMB-KVBYGGB-ING-IDARTNR (KOMB-IDX2)            
058100                          KOMB-KVBYGGB             (KOMB-IDX2)            
058200         MOVE SPACE    TO KOMB-KVBYGGB-KDSATKMB    (KOMB-IDX2)            
058300         SET KOMB-IDX2 UP BY +1                                           
058400     END-PERFORM                                                          
058500     .                                                                    
058600     EJECT                                                                
058700 AB-BEHANDLA-2301-MID  SECTION.                                           
058800                                                                          
058900     IF MSG-DUBBLA-TRANSKODER                                             
059000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO 2301-MID-W2I30101            
059100                                             2303-MID-W2I30301            
059200       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
059300       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
059400     ELSE                                                                 
059500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO  2301-MID-W2I30101            
059600                                             2303-MID-W2I30301            
059700       MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                   
059800       MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                  
059900     END-IF                                                               
060000                                                                          
060100     MOVE JA                   TO HOPP-FRAN-2301-SW                       
060200     MOVE +1                   TO INDX                                    
060300     PERFORM UNTIL INDX        >  MAX-RAD-2301 OR                         
060400                   CMD-IFYLLT                                             
060500        IF 2301-MID-CMD-RAD(INDX)   = '+' OR SPACE                        
060600            ADD +1             TO INDX                                    
060700         ELSE                                                             
060800            MOVE 2301-MID-IDORDNST-RAD(INDX) (1:4) TO WS-IDORDNSB         
060900            MOVE 2301-MID-IDORDNST-RAD(INDX) (5:1) TO WS-IDORDNSS         
061000            MOVE JA                   TO CMD-IFYLLT-SW                    
061100            INSPECT WS-IDORDNST REPLACING LEADING SPACE BY ZERO           
061200        END-IF                                                            
061300     END-PERFORM                                                          
061400                                                                          
061500     .                                                                    
061600     EJECT                                                                
061700 B-KOLLA-NYCKLAR SECTION.                                                 
061800                                                                          
061900     MOVE JA                   TO NYCKLAR-SW                              
062000                                                                          
062100     MOVE MFS-RENSA-FAELT      TO MOD-IDORDNSB-IN                         
062200                                  MOD-IDORDNSS-IN                         
062300                                  MOD-ING-IDARTNR-IN                      
062400                                  MOD-KDSATKMB-IN                         
062500                                  MOD-IDANSK-FOM-IN                       
062600                                  MOD-IDANSK-TOM-IN                       
062700                                  MOD-FLBYGGB-IN                          
062800                                  MOD-IDARTNR-IN                          
062900                                                                          
063000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
063100     MOVE '001'             TO MSGI-KDCALL                                
063200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
063300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
063400     MOVE '2303'            TO MSGI-IDTRANS                               
063500                                                                          
063600     PERFORM BA-KOLLA-IDORDNST                                            
063700     PERFORM BB-KOLLA-ING-IDARTNR                                         
063800     PERFORM BC-KOLLA-KDSATKMB                                            
063900     PERFORM BD-FLYTTA-ANDRA-NYCKLAR                                      
064000                                                                          
064100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
064200     MOVE MSGI-IDORDNR5-FILLER TO WS-IDORDNST                             
064300     INSPECT WS-IDORDNST REPLACING LEADING SPACE BY ZERO                  
064400                                                                          
064500     IF WS-IDORDNST NUMERIC AND WS-IDORDNST > ZERO                        
064600       MOVE WS-IDORDNST (1:4)  TO W-IDORDNSB                              
064700       MOVE WS-IDORDNST (5:1)  TO W-IDORDNSS                              
064800     ELSE                                                                 
064900       MOVE NEJ                TO NYCKLAR-SW                              
065000     END-IF                                                               
065100                                                                          
065200     IF GODK-MID OR NYCKLAR-OK                                            
065300       IF GODK-MID                                                        
065400          MOVE WS-IDORDNSB        TO MOD-IDORDNSB-UT                      
065500          INSPECT MOD-IDORDNSB-UT REPLACING LEADING ZERO BY SPACE         
065600          MOVE WS-IDORDNSS        TO MOD-IDORDNSS-UT                      
065700          MOVE WS-ING-IDARTNR     TO MOD-ING-IDARTNR-UT                   
065800          INSPECT MOD-ING-IDARTNR-UT REPLACING LEADING ZERO               
065900                                  BY SPACE                                
066000          MOVE WS-KDSATKMB        TO MOD-KDSATKMB-UT                      
066100          MOVE WS-IDANSK-FOM      TO MOD-IDANSK-FOM-UT                    
066200          INSPECT MOD-IDANSK-FOM-UT REPLACING LEADING ZERO                
066300                                  BY SPACE                                
066400          MOVE WS-IDANSK-TOM      TO MOD-IDANSK-TOM-UT                    
066500          INSPECT MOD-IDANSK-TOM-UT REPLACING LEADING ZERO                
066600                                  BY SPACE                                
066700          MOVE WS-FLBYGGB         TO MOD-FLBYGGB-UT                       
066800          MOVE WS-IDARTNR         TO MOD-IDARTNR-UT                       
066900          INSPECT MOD-IDARTNR-UT    REPLACING LEADING ZERO                
067000                                  BY SPACE                                
067100       ELSE                                                               
067200          IF NYCKLAR-OK                                                   
067300             MOVE WS-IDORDNSB        TO MOD-IDORDNSB-UT                   
067400             INSPECT MOD-IDORDNSB-UT REPLACING LEADING ZERO               
067500                                  BY SPACE                                
067600             MOVE WS-IDORDNSS        TO MOD-IDORDNSS-UT                   
067700             MOVE MFS-RENSA-FAELT    TO MOD-ING-IDARTNR-UT                
067800                                        MOD-IDANSK-FOM-UT                 
067900                                        MOD-IDANSK-TOM-UT                 
068000                                        MOD-FLBYGGB-UT                    
068100                                        MOD-IDARTNR-UT                    
068200          END-IF                                                          
068300       END-IF                                                             
068400     ELSE                                                                 
068500       MOVE MFS-RENSA-FAELT    TO MOD-IDORDNSB-UT                         
068600                                  MOD-IDORDNSS-UT                         
068700                                  MOD-ING-IDARTNR-UT                      
068800                                  MOD-IDANSK-FOM-UT                       
068900                                  MOD-IDANSK-TOM-UT                       
069000                                  MOD-FLBYGGB-UT                          
069100                                  MOD-IDARTNR-UT                          
069200     END-IF                                                               
069300                                                                          
069400     IF NOT GODK-MID                                                      
069500       MOVE MFS-RENSA-FAELT    TO MOD-ING-IDARTNR-UT                      
069600                                  MOD-IDANSK-FOM-UT                       
069700                                  MOD-IDANSK-TOM-UT                       
069800                                  MOD-FLBYGGB-UT                          
069900                                  MOD-IDARTNR-UT                          
070000     END-IF                                                               
070100                                                                          
070200     IF NYCKLAR-FEL                                                       
070300       MOVE ERR-WRONG-KEY      TO MED-IDMFSFEL                            
070400       CALL WMEDKONV USING MED-WMEDAREA                                   
070500       MOVE MED-TEMFSFEL         TO MOD-TEMFSFEL                          
070600       PERFORM MFS-RENSA-FAELT-IN                                         
070700       PERFORM MFS-RENSA-FAELT-UT                                         
070800     END-IF                                                               
070900     .                                                                    
071000     EJECT                                                                
071100 BA-KOLLA-IDORDNST      SECTION.                                          
071200                                                                          
071300*    -- KONTROLL AV IDORDNST                                              
071400                                                                          
071500     IF HOPP-FRAN-2301 AND CMD-IFYLLT                                     
071600        MOVE WS-IDORDNST TO MSGI-IDORDNR5-FILLER                          
071700     ELSE                                                                 
071800        IF 2303-MID-IDORDNSB-IN = ALL '+'                                 
071900           IF MFS-IDTRANS = '2301' OR '2302'                              
072000              MOVE 2303-MID-IDORDNSB-UT TO WS-IDORDNSB                    
072100              MOVE 2303-MID-IDORDNSS-UT TO WS-IDORDNSS                    
072200              IF (WS-IDORDNST NUMERIC                                     
072300              AND WS-IDORDNST > ZERO)                                     
072400                 MOVE WS-IDORDNST TO MSGI-IDORDNR5-FILLER                 
072500              END-IF                                                      
072600           END-IF                                                         
072700        ELSE                                                              
072800          MOVE 2303-MID-IDORDNSB-IN TO WS-IDORDNSB                        
072900          MOVE 2303-MID-IDORDNSS-IN TO WS-IDORDNSS                        
073000          IF MFS-IDTRANS = '2303'                                         
073100          OR (WS-IDORDNST NUMERIC                                         
073200          AND WS-IDORDNST > ZERO)                                         
073300             MOVE WS-IDORDNST TO MSGI-IDORDNR5-FILLER                     
073400          END-IF                                                          
073500        END-IF                                                            
073600     END-IF                                                               
073700                                                                          
073800     IF 2303-MID-IDORDNSB-IN    = ALL '+'                                 
073900        CONTINUE                                                          
074000     ELSE                                                                 
074100        MOVE '7'                  TO MFS-IDPFK                            
074200        MOVE SPACE                TO MFS-KDTRTYP                          
074300     END-IF                                                               
074400     .                                                                    
074500     EJECT                                                                
074600 BB-KOLLA-ING-IDARTNR   SECTION.                                          
074700                                                                          
074800*    -- KONTROLL AV INGÅENDE ARTIKELNUMMER                                
074900                                                                          
075000     IF 2303-MID-ING-IDARTNR-IN      = ALL '+'                            
075100       MOVE 2303-MID-ING-IDARTNR-UT  TO WS-ING-IDARTNR                    
075200       INSPECT WS-ING-IDARTNR REPLACING LEADING SPACE BY ZERO             
075300     ELSE                                                                 
075400       MOVE 2303-MID-ING-IDARTNR-IN  TO WS-ING-IDARTNR                    
075500       MOVE '7'                      TO MFS-IDPFK                         
075600       MOVE SPACE                    TO MFS-KDTRTYP                       
075700     END-IF                                                               
075800                                                                          
075900     IF GODK-MID                                                          
076000        CONTINUE                                                          
076100     ELSE                                                                 
076200        MOVE ZERO TO WS-ING-IDARTNR                                       
076300     END-IF                                                               
076400                                                                          
076500     IF WS-ING-IDARTNR NUMERIC                                            
076600       IF WS-ING-IDARTNR       >  ZERO                                    
076700           MOVE WS-ING-IDARTNR TO W-ING-IDARTNR                           
076800        ELSE                                                              
076900           MOVE LOW-VALUE      TO W-ING-IDARTNR-X                         
077000       END-IF                                                             
077100     ELSE                                                                 
077200       MOVE NEJ                TO NYCKLAR-SW                              
077300     END-IF                                                               
077400     .                                                                    
077500     EJECT                                                                
077600 BC-KOLLA-KDSATKMB      SECTION.                                          
077700                                                                          
077800*    -- KONTROLL AV KOMBINATIONSKOD                                       
077900                                                                          
078000     IF 2303-MID-KDSATKMB-IN         = ALL '+'                            
078100       MOVE 2303-MID-KDSATKMB-UT     TO WS-KDSATKMB                       
078200     ELSE                                                                 
078300       MOVE 2303-MID-KDSATKMB-IN     TO WS-KDSATKMB                       
078400       MOVE '7'                      TO MFS-IDPFK                         
078500       MOVE SPACE                    TO MFS-KDTRTYP                       
078600     END-IF                                                               
078700                                                                          
078800     IF GODK-MID                                                          
078900        CONTINUE                                                          
079000     ELSE                                                                 
079100        MOVE SPACE TO WS-KDSATKMB                                         
079200     END-IF                                                               
079300                                                                          
079400     IF WS-KDSATKMB NUMERIC                                               
079500       MOVE NEJ                TO NYCKLAR-SW                              
079600     ELSE                                                                 
079700       IF WS-KDSATKMB          =  SPACE                                   
079800           MOVE LOW-VALUE      TO W-KDSATKMB-MIN-X                        
079900           MOVE HIGH-VALUE     TO W-KDSATKMB-MAX-X                        
080000        ELSE                                                              
080100           MOVE WS-KDSATKMB    TO W-KDSATKMB-MIN-X                        
080200                                  W-KDSATKMB-MAX-X                        
080300       END-IF                                                             
080400     END-IF                                                               
080500     .                                                                    
080600     EJECT                                                                
080700 BD-FLYTTA-ANDRA-NYCKLAR        SECTION.                                  
080800                                                                          
080900*    -- FLYTTA ANDRA NYCKLAR TILL NYCKLAR-UT                              
081000                                                                          
081100     IF HOPP-FRAN-2301                                                    
081200         IF 2301-MID-IDANSK-FOM-IN  = ALL '+'                             
081300             MOVE 2301-MID-IDANSK-FOM-UT TO WS-IDANSK-FOM                 
081400             INSPECT WS-IDANSK-FOM REPLACING LEADING SPACE BY ZERO        
081500          ELSE                                                            
081600             MOVE 2301-MID-IDANSK-FOM-IN TO WS-IDANSK-FOM                 
081700         END-IF                                                           
081800                                                                          
081900         IF 2301-MID-IDANSK-TOM-IN  = ALL '+'                             
082000             MOVE 2301-MID-IDANSK-TOM-UT TO WS-IDANSK-TOM                 
082100             INSPECT WS-IDANSK-TOM REPLACING LEADING SPACE BY ZERO        
082200          ELSE                                                            
082300             MOVE 2301-MID-IDANSK-TOM-IN TO WS-IDANSK-TOM                 
082400         END-IF                                                           
082500                                                                          
082600         IF 2301-MID-FLBYGGB-IN      = ALL '+'                            
082700             MOVE 2301-MID-FLBYGGB-UT     TO WS-FLBYGGB                   
082800          ELSE                                                            
082900             MOVE 2301-MID-FLBYGGB-IN     TO WS-FLBYGGB                   
083000         END-IF                                                           
083100                                                                          
083200         IF 2301-MID-IDARTNR-IN      = ALL '+'                            
083300             MOVE 2301-MID-IDARTNR-UT    TO WS-IDARTNR                    
083400             INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO           
083500          ELSE                                                            
083600             MOVE 2301-MID-IDARTNR-IN    TO WS-IDARTNR                    
083700         END-IF                                                           
083800      ELSE                                                                
083900         IF 2303-MID-IDANSK-FOM-IN  = ALL '+'                             
084000             MOVE 2303-MID-IDANSK-FOM-UT TO WS-IDANSK-FOM                 
084100             INSPECT WS-IDANSK-FOM REPLACING LEADING SPACE BY ZERO        
084200         ELSE                                                             
084300             MOVE 2303-MID-IDANSK-FOM-IN TO WS-IDANSK-FOM                 
084400         END-IF                                                           
084500                                                                          
084600         IF 2303-MID-IDANSK-TOM-IN  = ALL '+'                             
084700             MOVE 2303-MID-IDANSK-TOM-UT TO WS-IDANSK-TOM                 
084800             INSPECT WS-IDANSK-TOM REPLACING LEADING SPACE BY ZERO        
084900          ELSE                                                            
085000             MOVE 2303-MID-IDANSK-TOM-IN TO WS-IDANSK-TOM                 
085100         END-IF                                                           
085200                                                                          
085300         IF 2303-MID-FLBYGGB-IN      = ALL '+'                            
085400             MOVE 2303-MID-FLBYGGB-UT     TO WS-FLBYGGB                   
085500          ELSE                                                            
085600             MOVE 2303-MID-FLBYGGB-IN     TO WS-FLBYGGB                   
085700         END-IF                                                           
085800                                                                          
085900         IF 2303-MID-IDARTNR-IN      = ALL '+'                            
086000             MOVE 2303-MID-IDARTNR-UT    TO WS-IDARTNR                    
086100             INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO           
086200         ELSE                                                             
086300             MOVE 2303-MID-IDARTNR-IN    TO WS-IDARTNR                    
086400         END-IF                                                           
086500     END-IF                                                               
086600     .                                                                    
086700     EJECT                                                                
086800 C-FOERSTA-SIDA SECTION.                                                  
086900                                                                          
087000     MOVE INF-FIRST-PAGE       TO MED-IDMFSINF                            
087100     CALL WMEDKONV USING MED-WMEDAREA                                     
087200     MOVE MED-TEMFSINF           TO MOD-TEMFSINF                          
087300                                                                          
087400     MOVE ZERO                 TO W-IDARTNR IN W-IDARTNR-X                
087500     MOVE JA                   TO ALLT-SW                                 
087600     .                                                                    
087700     EJECT                                                                
087800 D-NAESTA-SIDA SECTION.                                                   
087900                                                                          
088000     MOVE 2303-MID-IDARTNR-NEXT    TO W-ING-IDARTNR                       
088100     MOVE JA                       TO ALLT-SW                             
088200     .                                                                    
088300     EJECT                                                                
088400 E-SAMMA-SIDA SECTION.                                                    
088500                                                                          
088600     IF 2303-MID-INPUT              = ALL '+'                             
088700       MOVE 2303-MID-IDARTNR-ENTER  TO W-ING-IDARTNR                      
088800       MOVE JA                      TO ALLT-SW                            
088900     ELSE                                                                 
089000       MOVE NEJ                TO ALLT-SW                                 
089100       MOVE INF-PRESS-PF11     TO MED-IDMFSINF                            
089200       CALL WMEDKONV USING MED-WMEDAREA                                   
089300       MOVE MED-TEMFSINF         TO MOD-TEMFSINF                          
089400       PERFORM MFS-ROER-EJ-FAELT-IN                                       
089500       PERFORM MFS-ROER-EJ-FAELT-UT                                       
089600       PERFORM MFS-LAES-IN-IGEN                                           
089700     END-IF                                                               
089800     .                                                                    
089900     EJECT                                                                
090000 F-LAES-VISA-INFO SECTION.                                                
090100                                                                          
090200     PERFORM FA-LAES-GRUNDDATA                                            
090300                                                                          
090400     IF SEGMENT-SAKNAS                                                    
090500         MOVE '701'            TO MED-IDMFSFEL                            
090600*****    ORDERN SAKNAS                                                    
090700         CALL WMEDKONV USING MED-WMEDAREA                                 
090800         MOVE MED-TEMFSFEL       TO MOD-TEMFSFEL                          
090900         PERFORM MFS-RENSA-FAELT-UT                                       
091000     ELSE                                                                 
091100         PERFORM FC-VISA-BENAEMNING                                       
091200         PERFORM FB-LAES-VISA-ING-IDARTNR                                 
091300     END-IF                                                               
091400     PERFORM MFS-RENSA-FAELT-IN                                           
091500     .                                                                    
091600     EJECT                                                                
091700 FA-LAES-GRUNDDATA SECTION.                                               
091800                                                                          
091900     PERFORM IMS-GU-SATG1-SATG01                                          
092000                                                                          
092100     IF SEGMENT-FINNS                                                     
092200         MOVE SHUV-IDARTNR     TO  W-IDARTNR1-9 (1:9)                     
092300                                   W-IDARTNR                              
092400                               IN  W-IDARTNR-X                            
092500         MOVE SHUV-REKSIFFR    TO  W-REKSIFFR                             
092600         MOVE W-IDARTNR-11     TO  MOD-IDARTNR                            
092700         INSPECT MOD-IDARTNR REPLACING LEADING ZERO BY SPACE              
092800         MOVE SHUV-KDCLAGER    TO  MOD-KDCLAGER                           
092900         MOVE SHUV-IDPRC       TO  MOD-IDPRC                              
093000         MOVE SHUV-FLSATPRI    TO  MOD-FLSATPRI                           
093100         MOVE SHUV-FLBYGGB     TO  MOD-FLBYGGB                            
093200         MOVE SHUV-KDSATKMB    TO  MOD-KDSATKMB                           
093300         IF SHUV-KDSATSTA      =   'U'                                    
093400             MOVE SHUV-IDPRODNR TO MOD-IDPRODNR                           
093500          ELSE                                                            
093600             MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR                         
093700         END-IF                                                           
093800                                                                          
093900         MOVE SHUV-KVBEART     TO  MOD-KVBEART                            
094000         MOVE SHUV-KVBYGGB     TO  MOD-KVBYGGB                            
094100     END-IF                                                               
094200     .                                                                    
094300     EJECT                                                                
094400                                                                          
094500 FB-LAES-VISA-ING-IDARTNR       SECTION.                                  
094600                                                                          
094700     MOVE +1                   TO INDX                                    
094800     IF MFS-UPDATE                                                        
094900         IF 2303-MID-ING-IDARTNR2-UPDATE = SPACE OR ALL '+'               
095000             MOVE W-ING-IDARTNR1-UPDATE  TO W-ING-IDARTNR                 
095100          ELSE                                                            
095200             IF W-ING-IDARTNR1-UPDATE <  W-ING-IDARTNR2-UPDATE            
095300                 MOVE W-ING-IDARTNR1-UPDATE TO W-ING-IDARTNR              
095400              ELSE                                                        
095500                 MOVE W-ING-IDARTNR2-UPDATE TO W-ING-IDARTNR              
095600             END-IF                                                       
095700         END-IF                                                           
095800     END-IF                                                               
095900                                                                          
096000     PERFORM IMS-GU-SATG1-SATG11-OKVAL                                    
096100     IF SEGMENT-FINNS                                                     
096200         MOVE SRAD-IDARTNR TO MOD-IDARTNR-ENTER                           
096300      ELSE                                                                
096400         MOVE ZERO             TO MOD-IDARTNR-ENTER                       
096500     END-IF                                                               
096600                                                                          
096700     PERFORM UNTIL INDX > MAX-RAD-2303                                    
096800        IF SEGMENT-FINNS                                                  
096900             IF SRAD-KVSATROS  >  ZERO                                    
097000                 MOVE SRAD-IDARTNR TO W-IDARTNR                           
097100                                   IN W-IDARTNR-X                         
097200                 PERFORM IMS-GU-ARTC1-ARTC11-OKVAL                        
097300             END-IF                                                       
097400             PERFORM FBA-REDIGERA-RAD                                     
097500             ADD 1 TO INDX                                                
097600             PERFORM IMS-GN-SATG1-SATG11                                  
097700         ELSE                                                             
097800           IF MFS-NEXT AND INDX      = +1                                 
097900               MOVE '115'            TO MED-IDMFSFEL                      
098000*****          SISTA SIDAN REDAN VISAD                                    
098100               CALL WMEDKONV USING MED-WMEDAREA                           
098200               MOVE MED-TEMFSFEL       TO MOD-TEMFSFEL                    
098300           END-IF                                                         
098400           PERFORM MFS-RENSA-RAD-FAELT-UT                                 
098500           ADD 1 TO INDX                                                  
098600        END-IF                                                            
098700     END-PERFORM                                                          
098800                                                                          
098900     MOVE 999999999            TO MOD-IDARTNR-NEXT                        
099000     IF SEGMENT-FINNS                                                     
099100         MOVE SRAD-IDARTNR                 TO MOD-IDARTNR-NEXT            
099200         MOVE INF-MORE-INFO-EXISTS         TO MED-IDMFSINF                
099300         CALL WMEDKONV USING MED-WMEDAREA                                 
099400         MOVE MED-TEMFSINF                 TO MOD-TEMFSINF                
099500     END-IF                                                               
099600                                                                          
099700     .                                                                    
099800     EJECT                                                                
099900 FBA-REDIGERA-RAD SECTION.                                                
100000                                                                          
100100     MOVE SRAD-IDARTNR         TO  W-IDARTNR1-9 (1:9)                     
100200     MOVE SRAD-REKSIFFR        TO  W-REKSIFFR                             
100300     MOVE W-IDARTNR-11         TO MOD-ING-IDARTNR-RAD   (INDX)            
100400     INSPECT MOD-ING-IDARTNR-RAD (INDX)                                   
100500                           REPLACING LEADING ZERO BY SPACE                
100600     MOVE SRAD-REANTPSA        TO MOD-REANTPSA-RAD      (INDX)            
100700     MOVE SRAD-REBEART         TO MOD-REBEART-RAD       (INDX)            
100800     MOVE SRAD-KVSATRES        TO MOD-KVSATRES-RAD      (INDX)            
100900     MOVE SRAD-KVSATROS        TO MOD-KVSATROS-RAD      (INDX)            
101000     MOVE SRAD-KDSATAND        TO MOD-KDSATAND-RAD      (INDX)            
101100     MOVE SRAD-FLSATRAS        TO MOD-FLSATRAS-RAD      (INDX)            
101200     MOVE SRAD-KDSATKMB        TO MOD-KDSATKMB-RAD      (INDX)            
101300                                                                          
101400     IF SRAD-FLSATSPR          =  JA                                      
101500         MOVE SRAD-FLSATSPR    TO MOD-FLSATSPR-RAD      (INDX)            
101600      ELSE                                                                
101700         MOVE MFS-RENSA-FAELT  TO MOD-FLSATSPR-RAD      (INDX)            
101800     END-IF                                                               
101900                                                                          
102000     IF SRAD-KVSATROS          >  ZERO                                    
102100         MOVE CLAG-TIDISPIN    TO MOD-TIDISPIN-RAD      (INDX)            
102200      ELSE                                                                
102300         MOVE MFS-RENSA-FAELT  TO MOD-TIDISPIN-RAD      (INDX)            
102400     END-IF                                                               
102500     .                                                                    
102600     EJECT                                                                
102700 FC-VISA-BENAEMNING SECTION.                                              
102800                                                                          
102900     MOVE W-IDARTNR        IN  W-IDARTNR-X                                
103000                           TO  W-IDARTNR                                  
103100                           IN  W-WDD3BSEQ-X                               
103200     MOVE MED-IDSKYLT      TO  W-IDSKYLT                                  
103300     PERFORM IMS-GU-BENA11                                                
103400     IF SEGMENT-FINNS                                                     
103500       MOVE TEXT-BEART      TO MOD-BEART                                  
103600     ELSE                                                                 
103700       MOVE MFS-RENSA-FAELT TO MOD-BEART                                  
103800     END-IF                                                               
103900     .                                                                    
104000     EJECT                                                                
104100 G-KOLLA-INPUT SECTION.                                                   
104200                                                                          
104300     MOVE JA                   TO INDATA-SW                               
104400     IF 2303-MID-INPUT         = ALL '+'                                  
104500       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
104600       CALL WMEDKONV USING MED-WMEDAREA                                   
104700       MOVE MED-TEMFSFEL         TO MOD-TEMFSFEL                          
104800       PERFORM MFS-ROER-EJ-FAELT-IN                                       
104900       PERFORM MFS-ROER-EJ-FAELT-UT                                       
105000       MOVE NEJ                TO INDATA-SW                               
105100     ELSE                                                                 
105200                                                                          
105300       PERFORM IMS-GU-SATG1-SATG01                                        
105400       IF SEGMENT-FINNS                                                   
105500           IF SHUV-KDSATSTA    = 'U'                                      
105600             MOVE '124'        TO MED-IDMFSFEL                            
105700******       ORDERN REDAN UTSKRIVEN                                       
105800             CALL WMEDKONV USING MED-WMEDAREA                             
105900             MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                          
106000             PERFORM MFS-ROER-EJ-FAELT-UT                                 
106100             PERFORM MFS-ROER-EJ-FAELT-IN                                 
106200             MOVE NEJ          TO INDATA-SW                               
106300           ELSE                                                           
106400             MOVE SHUV-KDPRODSL  TO TEST-KDPRODSL                         
106500             IF KDPRODSL-LOCAL                                            
106600                MOVE NEJ       TO INDATA-SW                               
106700             END-IF                                                       
106800             IF (2303-MID-ING-IDARTNR2-UPDATE = SPACE OR ALL '+')         
106801             AND (2303-MID-ING-IDARTNR1-UPDATE = SPACE OR ALL '+')        
106802                                                                          
106803               MOVE NEJ        TO INDATA-SW                               
106804             ELSE                                                         
106810               IF 2303-MID-ING-IDARTNR2-UPDATE = SPACE OR ALL '+'         
106900                   PERFORM GA-KOLLA-EN-ING-ARTIKEL                        
107000               ELSE                                                       
107100                   IF 2303-MID-KDSATKMB-UPDATE = SPACE OR '+'             
107200                       PERFORM GB-KOLLA-TVA-ING-ARTIKEL                   
107300                   ELSE                                                   
107400                       PERFORM GC-KOLLA-UPPD-MED-KDSATKMB                 
107500                   END-IF                                                 
107600               END-IF                                                     
107610             END-IF                                                       
107700             IF INDATA-FEL                                                
107800               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
107900               CALL WMEDKONV USING MED-WMEDAREA                           
108000               MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                          
108100               PERFORM MFS-ROER-EJ-FAELT-UT                               
108200               PERFORM MFS-ROER-EJ-FAELT-IN                               
108300             END-IF                                                       
108400           END-IF                                                         
108500       ELSE                                                               
108600         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
108700         MOVE NEJ              TO INDATA-SW                               
108800         CALL WMEDKONV USING MED-WMEDAREA                                 
108900         MOVE MED-TEMFSFEL       TO MOD-TEMFSFEL                          
109000         PERFORM MFS-RENSA-FAELT-IN                                       
109100         PERFORM MFS-RENSA-FAELT-UT                                       
109200       END-IF                                                             
109300     END-IF                                                               
109400     .                                                                    
109500     EJECT                                                                
109600 GA-KOLLA-EN-ING-ARTIKEL   SECTION.                                       
109700                                                                          
109800     MOVE 2303-MID-ING-IDARTNR1-UPDATE TO W-ING-IDARTNR-UPDATE            
109900                                          W-ING-IDARTNR1-UPDATE           
110000                                          W-ING-IDARTNR                   
110100                                          W-IDARTNR                       
110200                                       IN W-IDARTNR-X                     
110300                                                                          
110400     MOVE SHUV-KDSATKMB                TO W-SHUV-KDSATKMB                 
110500                                                                          
110600     PERFORM GAA-KOLLA-KDSATAND                                           
110700     PERFORM GAB-KOLLA-KDSATKMB                                           
110800                                                                          
110900     EVALUATE TRUE                                                        
111000     WHEN 2303-MID-KDSATAND1-UPDATE    = 'B'                              
111100         PERFORM GAC-KOLLA-ANNULLERING                                    
111200                                                                          
111300     WHEN 2303-MID-KDSATAND1-UPDATE    = 'N'                              
111400         PERFORM GAD-KOLLA-TILLAGG                                        
111500                                                                          
111600     WHEN OTHER                                                           
111700         MOVE MFS-ALFA-FAELT-RAETT TO                                     
111800                                  MOD-ING-IDARTNR1-UPDATE-ATTR            
111900                                  MOD-REANTPSA1-UPDATE-ATTR               
112000                                  MOD-REBEART1-UPDATE-ATTR                
112100     END-EVALUATE                                                         
112200     .                                                                    
112300     EJECT                                                                
112400 GAA-KOLLA-KDSATAND      SECTION.                                         
112500                                                                          
112600     IF 2303-MID-KDSATAND1-UPDATE  =  'B' OR 'N'                          
112700         MOVE MFS-ALFA-FAELT-RAETT TO  MOD-KDSATAND1-UPDATE-ATTR          
112800         MOVE JA                   TO  KDSATAND1-SW                       
112900      ELSE                                                                
113000         MOVE MFS-ALFA-FAELT-FEL   TO  MOD-KDSATAND1-UPDATE-ATTR          
113100         MOVE NEJ                  TO  KDSATAND1-SW                       
113200                                       INDATA-SW                          
113300     END-IF                                                               
113400                                                                          
113500     .                                                                    
113600     EJECT                                                                
113700 GAB-KOLLA-KDSATKMB      SECTION.                                         
113800                                                                          
113900     IF 2303-MID-KDSATKMB-UPDATE       = '+' OR SPACE                     
114000         MOVE MFS-ALFA-FAELT-RAETT TO  MOD-KDSATKMB-UPDATE-ATTR           
114100         MOVE SPACE                TO  W-KDSATKMB-UPDATE                  
114200      ELSE                                                                
114300         MOVE 2303-MID-KDSATKMB-UPDATE   TO  W-KDSATKMB-UPDATE            
114400         IF 2303-MID-KDSATAND1-UPDATE  =  'B'                             
114500             MOVE MFS-ALFA-FAELT-RAETT TO                                 
114600                                       MOD-KDSATKMB-UPDATE-ATTR           
114700          ELSE                                                            
114800             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSATKMB-UPDATE-ATTR          
114900             MOVE NEJ                TO INDATA-SW                         
115000         END-IF                                                           
115100     END-IF                                                               
115200                                                                          
115300                                                                          
115400     .                                                                    
115500     EJECT                                                                
115600 GAC-KOLLA-ANNULLERING    SECTION.                                        
115700                                                                          
115800     PERFORM S01-KOLLA-ING-IDARTNR                                        
115900                                                                          
116000     IF ING-IDARTNR-FINNS                                                 
116100          MOVE MFS-ALFA-FAELT-RAETT  TO                                   
116200                            MOD-ING-IDARTNR1-UPDATE-ATTR                  
116300      ELSE                                                                
116400          MOVE MFS-ALFA-FAELT-FEL    TO                                   
116500                            MOD-ING-IDARTNR1-UPDATE-ATTR                  
116600          MOVE NEJ                   TO  INDATA-SW                        
116700     END-IF                                                               
116800                                                                          
116900     PERFORM IMS-GHU-SATG1-SATG11                                         
117000     IF SEGMENT-FINNS                                                     
117100         IF SRAD-KDSATAND = 'B' AND                                       
117200            SRAD-FLSATSPR = 'J' AND                                       
117300            2303-MID-KDSATAND1-UPDATE = 'B'                               
117400              MOVE MFS-ALFA-FAELT-RAETT TO                                
117500                           MOD-KDSATAND1-UPDATE-ATTR                      
117600         ELSE                                                             
117700*           IF (SRAD-KDSATAND = 'B' OR 'U')  AND                          
117800*               SRAD-KDSATKMB = SPACE                                     
117900*                MOVE MFS-ALFA-FAELT-FEL    TO                            
118000*                         MOD-KDSATAND1-UPDATE-ATTR                       
118100*                MOVE NEJ         TO  INDATA-SW                           
118200*             ELSE                                                        
118300                 MOVE MFS-ALFA-FAELT-RAETT TO                             
118400                           MOD-KDSATAND1-UPDATE-ATTR                      
118500*           END-IF                                                        
118600         END-IF                                                           
118700         IF 2303-MID-KDSATKMB-UPDATE   =  SPACE OR '+'                    
118800             CONTINUE                                                     
118900          ELSE                                                            
119000             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSATKMB-UPDATE-ATTR          
119100             MOVE NEJ                TO  INDATA-SW                        
119200         END-IF                                                           
119300      ELSE                                                                
119400         MOVE MFS-ALFA-FAELT-FEL TO  MOD-REANTPSA1-UPDATE-ATTR            
119500                                     MOD-KDSATAND1-UPDATE-ATTR            
119600                                     MOD-ING-IDARTNR1-UPDATE-ATTR         
119700         MOVE NEJ                TO  INDATA-SW                            
119800     END-IF                                                               
119900     .                                                                    
120000     EJECT                                                                
120100 GAD-KOLLA-TILLAGG        SECTION.                                        
120200                                                                          
120300     PERFORM IMS-GHU-SATG1-SATG11                                         
120400     IF SEGMENT-FINNS AND SRAD-KDSATAND NOT = 'B'                         
120500         MOVE MFS-ALFA-FAELT-FEL            TO                            
120600                   MOD-ING-IDARTNR1-UPDATE-ATTR                           
120700                   MOD-KDSATAND1-UPDATE-ATTR                              
120800         MOVE NEJ              TO  INDATA-SW                              
120900       ELSE                                                               
121000          MOVE MFS-ALFA-FAELT-RAETT         TO                            
121100                    MOD-KDSATAND1-UPDATE-ATTR                             
121200                    MOD-ING-IDARTNR1-UPDATE-ATTR                          
121300     END-IF                                                               
121400                                                                          
121500     IF SRAD-KDSATAND = 'B' AND SRAD-FLSATSPR = 'J'                       
121600        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSATAND1-UPDATE-ATTR              
121700        MOVE NEJ                TO INDATA-SW                              
121800     END-IF                                                               
121900                                                                          
122000     IF 2303-MID-REANTPSA1-UPDATE = SPACE OR ALL '+' OR ZERO              
122100         MOVE MFS-ALFA-FAELT-FEL TO                                       
122200                   MOD-REANTPSA1-UPDATE-ATTR                              
122300         MOVE NEJ                    TO  INDATA-SW                        
122400      ELSE                                                                
122500         PERFORM S02-KOLLA-REANTPSA                                       
122600     END-IF                                                               
122700                                                                          
122800     PERFORM S03-KOLLA-REBEART                                            
122900                                                                          
123000     PERFORM S04-KOLLA-SPARRAR                                            
123100     IF ING-IDARTNR-SPARRAD                                               
123200         MOVE MFS-ALFA-FAELT-FEL TO                                       
123300                             MOD-ING-IDARTNR1-UPDATE-ATTR                 
123400         MOVE NEJ                  TO  INDATA-SW                          
123500     END-IF                                                               
123600     .                                                                    
123700     EJECT                                                                
123800 GB-KOLLA-TVA-ING-ARTIKEL   SECTION.                                      
123900                                                                          
124000     MOVE 2303-MID-ING-IDARTNR1-UPDATE TO W-ING-IDARTNR-UPDATE            
124100                                         W-ING-IDARTNR1-UPDATE            
124200     MOVE 2303-MID-ING-IDARTNR2-UPDATE TO                                 
124300                                         W-ING-IDARTNR2-UPDATE            
124400     PERFORM S01-KOLLA-ING-IDARTNR                                        
124500     IF ING-IDARTNR-FINNS                                                 
124600         MOVE MFS-ALFA-FAELT-RAETT TO                                     
124700                        MOD-ING-IDARTNR1-UPDATE-ATTR                      
124800      ELSE                                                                
124900         MOVE MFS-ALFA-FAELT-FEL TO                                       
125000                        MOD-ING-IDARTNR1-UPDATE-ATTR                      
125100         MOVE NEJ                     TO  INDATA-SW                       
125200     END-IF                                                               
125300                                                                          
125400     MOVE 2303-MID-ING-IDARTNR1-UPDATE TO W-ING-IDARTNR                   
125500     PERFORM IMS-GHU-SATG1-SATG11                                         
125600     IF SEGMENT-FINNS                                                     
125700         IF (SRAD-KDSATAND = 'B' OR 'U')      OR                          
125800            (SRAD-KDSATAND = 'E' AND                                      
125900             SRAD-KDSATKMB = SPACE)                                       
126000              MOVE MFS-ALFA-FAELT-FEL TO                                  
126100                       MOD-KDSATAND1-UPDATE-ATTR                          
126200              MOVE NEJ         TO  INDATA-SW                              
126300           ELSE                                                           
126400              MOVE MFS-ALFA-FAELT-RAETT TO                                
126500                        MOD-KDSATAND1-UPDATE-ATTR                         
126600         END-IF                                                           
126700     END-IF                                                               
126800                                                                          
126900     MOVE W-ING-IDARTNR2-UPDATE TO W-ING-IDARTNR                          
127000     PERFORM IMS-GHU-SATG1-SATG11                                         
127100     IF SEGMENT-FINNS AND SRAD-KDSATAND NOT = 'B'                         
127200         MOVE NEJ                    TO  INDATA-SW                        
127300         MOVE MFS-ALFA-FAELT-FEL TO                                       
127400                        MOD-ING-IDARTNR2-UPDATE-ATTR                      
127500      ELSE                                                                
127600         MOVE MFS-ALFA-FAELT-RAETT TO                                     
127700                        MOD-ING-IDARTNR2-UPDATE-ATTR                      
127800     END-IF                                                               
127900                                                                          
128000     PERFORM GBA-KOLLA-KDSATKMB                                           
128100     PERFORM GBB-KOLLA-KDSATAND                                           
128200     PERFORM GBC-KOLLA-REANTPSA-REBEART                                   
128300                                                                          
128400     .                                                                    
128500     EJECT                                                                
128600 GBA-KOLLA-KDSATKMB         SECTION.                                      
128700                                                                          
128800     IF 2303-MID-KDSATKMB-UPDATE = SPACE OR ALL '+'                       
128900         MOVE MFS-ALFA-FAELT-RAETT  TO MOD-KDSATKMB-UPDATE-ATTR           
129000      ELSE                                                                
129100         MOVE NEJ                 TO INDATA-SW                            
129200         MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDSATKMB-UPDATE-ATTR             
129300     END-IF                                                               
129400     .                                                                    
129500     EJECT                                                                
129600 GBB-KOLLA-KDSATAND      SECTION.                                         
129700                                                                          
129800     IF 2303-MID-KDSATAND1-UPDATE = SPACE OR ALL '+'                      
129900         MOVE NEJ                 TO INDATA-SW                            
130000         MOVE MFS-ALFA-FAELT-FEL  TO                                      
130100                                  MOD-KDSATAND1-UPDATE-ATTR               
130200      ELSE                                                                
130300         MOVE MFS-ALFA-FAELT-RAETT  TO                                    
130400                                  MOD-KDSATAND1-UPDATE-ATTR               
130500     END-IF                                                               
130600                                                                          
130700     IF 2303-MID-KDSATAND2-UPDATE = SPACE OR ALL '+'                      
130800         MOVE NEJ                 TO INDATA-SW                            
130900         MOVE MFS-ALFA-FAELT-FEL  TO                                      
131000                                  MOD-KDSATAND1-UPDATE-ATTR               
131100      ELSE                                                                
131200         MOVE MFS-ALFA-FAELT-RAETT  TO                                    
131300                                  MOD-KDSATAND2-UPDATE-ATTR               
131400     END-IF                                                               
131500                                                                          
131600     IF 2303-MID-KDSATAND1-UPDATE    = 'B'                                
131700          MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSATAND1-UPDATE-ATTR          
131800      ELSE                                                                
131900          MOVE NEJ                  TO INDATA-SW                          
132000          MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDSATAND1-UPDATE-ATTR          
132100     END-IF                                                               
132200                                                                          
132300     IF 2303-MID-KDSATAND2-UPDATE   = 'N'                                 
132400          MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSATAND2-UPDATE-ATTR          
132500      ELSE                                                                
132600          MOVE NEJ                  TO INDATA-SW                          
132700          MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDSATAND2-UPDATE-ATTR          
132800     END-IF                                                               
132900                                                                          
133000     MOVE W-ING-IDARTNR2-UPDATE     TO W-IDARTNR                          
133100                                    IN W-IDARTNR-X                        
133200     PERFORM S04-KOLLA-SPARRAR                                            
133300     IF ING-IDARTNR-SPARRAD                                               
133400         MOVE MFS-ALFA-FAELT-FEL   TO                                     
133500                             MOD-ING-IDARTNR2-UPDATE-ATTR                 
133600         MOVE NEJ                  TO  INDATA-SW                          
133700     END-IF                                                               
133800     .                                                                    
133900     EJECT                                                                
134000 GBC-KOLLA-REANTPSA-REBEART      SECTION.                                 
134100                                                                          
134200                                                                          
134300     IF 2303-MID-REANTPSA2-UPDATE = SPACE OR ALL '+' OR ZERO              
134400         MOVE NEJ                 TO INDATA-SW                            
134500         MOVE MFS-ALFA-FAELT-FEL TO                                       
134600                                  MOD-REANTPSA2-UPDATE-ATTR               
134700      ELSE                                                                
134800         MOVE 2303-MID-REANTPSA2-UPDATE TO DEC-IDFRIDATA                  
134900         MOVE +2                     TO DEC-KVHELTAL                      
135000         MOVE +3                     TO DEC-KVDECIMAL                     
135100         CALL WDECEDIT USING DEC-WDECAREA                                 
135200                                                                          
135300         IF DEC-KDSVAR-OK                                                 
135400             MOVE DEC-IDEDITDATA TO W-REANTPSA2-UPDATE                    
135500         END-IF                                                           
135600                                                                          
135700         IF DEC-KDSVAR-OK AND W-REANTPSA2-UPDATE > ZERO                   
135800             MOVE MFS-ALFA-FAELT-RAETT      TO                            
135900                       MOD-REANTPSA2-UPDATE-ATTR                          
136000          ELSE                                                            
136100             MOVE MFS-ALFA-FAELT-FEL        TO                            
136200                       MOD-REANTPSA2-UPDATE-ATTR                          
136300             MOVE NEJ          TO  INDATA-SW                              
136400         END-IF                                                           
136500     END-IF                                                               
136600                                                                          
136700     IF 2303-MID-REBEART2-UPDATE    =  SPACE OR ALL '+'                   
136800         COMPUTE W-REBEART2-UPDATE  =                                     
136900                  (SHUV-KVBEART * W-REANTPSA2-UPDATE) + 0.999             
137000         MOVE MFS-ALFA-FAELT-RAETT  TO  MOD-REBEART2-UPDATE-ATTR          
137100      ELSE                                                                
137200         COMPUTE W-REBEART2-UPDATE  =                                     
137300                  (SHUV-KVBEART * W-REANTPSA2-UPDATE) + 0.999             
137400         MOVE W-REBEART2-UPDATE        TO W-REBEART-UPDATE                
137500         IF 2303-MID-REBEART2-UPDATE   =  W-REBEART-UPDATE                
137600             MOVE MFS-ALFA-FAELT-RAETT TO MOD-REBEART2-UPDATE-ATTR        
137700          ELSE                                                            
137800             MOVE MFS-ALFA-FAELT-FEL   TO MOD-REBEART2-UPDATE-ATTR        
137900             MOVE NEJ                  TO INDATA-SW                       
138000         END-IF                                                           
138100     END-IF                                                               
138200     .                                                                    
138300     EJECT                                                                
138400 GC-KOLLA-UPPD-MED-KDSATKMB SECTION.                                      
138500                                                                          
138600     MOVE MFS-ALFA-FAELT-RAETT TO MOD-ING-IDARTNR1-UPDATE-ATTR            
138700                                  MOD-ING-IDARTNR2-UPDATE-ATTR            
138800                                  MOD-REANTPSA1-UPDATE-ATTR               
138900                                  MOD-REANTPSA2-UPDATE-ATTR               
139000                                  MOD-REBEART1-UPDATE-ATTR                
139100                                  MOD-REBEART2-UPDATE-ATTR                
139200                                  MOD-KDSATAND1-UPDATE-ATTR               
139300                                  MOD-KDSATAND2-UPDATE-ATTR               
139400                                  MOD-KDSATKMB-UPDATE-ATTR                
139500                                                                          
139600     IF SHUV-FLBYGGB = JA AND (2303-MID-KDSATAND1-UPDATE = 'Ä' OR         
139700                               2303-MID-KDSATAND2-UPDATE = 'Ä')           
139800         IF SHUV-FLBYGGB = JA AND 2303-MID-KDSATAND1-UPDATE = 'Ä'         
139900             MOVE MFS-ALFA-FAELT-FEL TO                                   
140000                                  MOD-KDSATAND1-UPDATE-ATTR               
140100         END-IF                                                           
140200         IF SHUV-FLBYGGB = JA AND 2303-MID-KDSATAND2-UPDATE = 'Ä'         
140300             MOVE MFS-ALFA-FAELT-FEL TO                                   
140400                                  MOD-KDSATAND2-UPDATE-ATTR               
140500         END-IF                                                           
140600         MOVE NEJ              TO INDATA-SW                               
140700      ELSE                                                                
140800         MOVE 2303-MID-ING-IDARTNR1-UPDATE TO                             
140900                                             W-ING-IDARTNR1-UPDATE        
141000         MOVE 2303-MID-ING-IDARTNR2-UPDATE TO                             
141100                                             W-ING-IDARTNR2-UPDATE        
141200         PERFORM GCA-KOLLA-IDARTNR                                        
141300                                                                          
141400         IF INDATA-OK                                                     
141500             PERFORM GCB-KOLLA-KDSATKMB                                   
141600             PERFORM GCC-KOLLA-KDSATAND                                   
141700                                                                          
141800             IF 2303-MID-KDSATKMB-UPDATE = SHUV-KDSATKMB                  
141900                 PERFORM S92-TA-FRAM-NASTA-KDSATKMB                       
142000                 PERFORM GCD-KOLLA-NY-KDSATKMB                            
142100              ELSE                                                        
142200                 MOVE SHUV-KDSATKMB    TO W-SHUV-KDSATKMB                 
142300                 PERFORM GCE-KOLLA-GAMMAL-KDSATKMB                        
142400             END-IF                                                       
142500         END-IF                                                           
142600     END-IF                                                               
142700     .                                                                    
142800     EJECT                                                                
142900 GCA-KOLLA-IDARTNR          SECTION.                                      
143000                                                                          
143100     IF 2303-MID-KDSATAND1-UPDATE     = 'Ä' OR 'B'                        
143200         MOVE 2303-MID-ING-IDARTNR1-UPDATE                                
143300                                     TO W-ING-IDARTNR-UPDATE              
143400         PERFORM S01-KOLLA-ING-IDARTNR                                    
143500         IF ING-IDARTNR-FINNS                                             
143600             MOVE MFS-ALFA-FAELT-RAETT TO                                 
143700                            MOD-ING-IDARTNR1-UPDATE-ATTR                  
143800          ELSE                                                            
143900             MOVE MFS-ALFA-FAELT-FEL TO                                   
144000                            MOD-ING-IDARTNR1-UPDATE-ATTR                  
144100             MOVE NEJ                 TO  INDATA-SW                       
144200         END-IF                                                           
144300     END-IF                                                               
144400                                                                          
144500     IF 2303-MID-KDSATAND2-UPDATE   = 'Ä' OR 'B'                          
144600         MOVE 2303-MID-ING-IDARTNR2-UPDATE                                
144700                                    TO W-ING-IDARTNR                      
144800         PERFORM IMS-GU-SATG1-SATG11-KVAL                                 
144900         IF SEGMENT-FINNS                                                 
145000             MOVE MFS-ALFA-FAELT-RAETT TO                                 
145100                            MOD-ING-IDARTNR2-UPDATE-ATTR                  
145200          ELSE                                                            
145300             MOVE MFS-ALFA-FAELT-FEL TO                                   
145400                            MOD-ING-IDARTNR2-UPDATE-ATTR                  
145500             MOVE NEJ                 TO  INDATA-SW                       
145600         END-IF                                                           
145700     END-IF                                                               
145800     .                                                                    
145900     EJECT                                                                
146000 GCB-KOLLA-KDSATKMB         SECTION.                                      
146100                                                                          
146200     IF 2303-MID-KDSATKMB-UPDATE = SPACE OR ALL '+'                       
146300         MOVE NEJ                 TO INDATA-SW                            
146400         MOVE MFS-ALFA-FAELT-FEL  TO                                      
146500                                  MOD-KDSATKMB-UPDATE-ATTR                
146600      ELSE                                                                
146700         MOVE 2303-MID-KDSATKMB-UPDATE TO W-KDSATKMB-UPDATE               
146800         MOVE MFS-ALFA-FAELT-RAETT  TO                                    
146900                                  MOD-KDSATKMB-UPDATE-ATTR                
147000     END-IF                                                               
147100     .                                                                    
147200     EJECT                                                                
147300 GCC-KOLLA-KDSATAND      SECTION.                                         
147400                                                                          
147500     IF 2303-MID-KDSATAND1-UPDATE = SPACE OR ALL '+'                      
147600         MOVE NEJ                 TO INDATA-SW                            
147700         MOVE MFS-ALFA-FAELT-FEL  TO                                      
147800                                  MOD-KDSATAND1-UPDATE-ATTR               
147900      ELSE                                                                
148000         MOVE MFS-ALFA-FAELT-RAETT  TO                                    
148100                                  MOD-KDSATAND1-UPDATE-ATTR               
148200     END-IF                                                               
148300                                                                          
148400     IF 2303-MID-KDSATAND2-UPDATE = SPACE OR ALL '+'                      
148500         MOVE NEJ                 TO INDATA-SW                            
148600         MOVE MFS-ALFA-FAELT-FEL  TO                                      
148700                                  MOD-KDSATAND1-UPDATE-ATTR               
148800      ELSE                                                                
148900         MOVE MFS-ALFA-FAELT-RAETT  TO                                    
149000                                  MOD-KDSATAND2-UPDATE-ATTR               
149100     END-IF                                                               
149200                                                                          
149300     IF (2303-MID-KDSATAND1-UPDATE = 'N' AND                              
149400         2303-MID-KDSATAND2-UPDATE = 'Ä')           OR                    
149500        (2303-MID-KDSATAND1-UPDATE = 'Ä' AND                              
149600         2303-MID-KDSATAND2-UPDATE = 'B')           OR                    
149700        (2303-MID-KDSATAND1-UPDATE = 'N' AND                              
149800         2303-MID-KDSATAND2-UPDATE = 'B')                                 
149900          MOVE NEJ                 TO INDATA-SW                           
150000          MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDSATAND1-UPDATE-ATTR           
150100                                      MOD-KDSATAND2-UPDATE-ATTR           
150200                                      MOD-KDSATKMB-UPDATE-ATTR            
150300     END-IF                                                               
150400     .                                                                    
150500     EJECT                                                                
150600                                                                          
150700 GCD-KOLLA-NY-KDSATKMB                  SECTION.                          
150800                                                                          
150900     EVALUATE TRUE                                                        
151000                                                                          
151100      WHEN 2303-MID-KDSATAND1-UPDATE       =  'Ä'  AND                    
151200           2303-MID-KDSATAND2-UPDATE       =  'Ä'                         
151300            PERFORM GCDA-KOLLA-TVA-ANDRADE-RADER                          
151400                                                                          
151500      WHEN 2303-MID-KDSATAND1-UPDATE       =  'Ä'  AND                    
151600           2303-MID-KDSATAND2-UPDATE       =  'N'                         
151700            PERFORM GCDB-KOLLA-EN-ANDRAD-EN-NY-RAD                        
151800                                                                          
151900      WHEN OTHER                                                          
152000            MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSATAND1-UPDATE-ATTR          
152100                                       MOD-KDSATAND2-UPDATE-ATTR          
152200            MOVE NEJ                TO INDATA-SW                          
152300     END-EVALUATE                                                         
152400     .                                                                    
152500     EJECT                                                                
152600 GCDA-KOLLA-TVA-ANDRADE-RADER      SECTION.                               
152700                                                                          
152800     MOVE W-ING-IDARTNR1-UPDATE         TO W-ING-IDARTNR                  
152900     PERFORM IMS-GU-SATG1-SATG11-KVAL                                     
153000     MOVE SRAD-WDJ211                   TO W1-SRAD-WDJ211                 
153100                                                                          
153200     MOVE W-ING-IDARTNR2-UPDATE         TO W-ING-IDARTNR                  
153300     PERFORM IMS-GU-SATG1-SATG11-KVAL                                     
153400     MOVE SRAD-WDJ211                   TO W2-SRAD-WDJ211                 
153500                                                                          
153600                                                                          
153700                                                                          
153800     IF W1-SRAD-KDSATAND       =  'T' OR 'R' OR 'Ä' OR                    
153900                                  'N' OR SPACE                            
154000         CONTINUE                                                         
154100      ELSE                                                                
154200         MOVE MFS-ALFA-FAELT-FEL    TO                                    
154300                            MOD-KDSATAND1-UPDATE-ATTR                     
154400         MOVE NEJ                   TO  INDATA-SW                         
154500     END-IF                                                               
154600                                                                          
154700     IF W2-SRAD-KDSATAND       =  'T' OR 'R' OR 'Ä' OR                    
154800                                  'N' OR SPACE                            
154900         CONTINUE                                                         
155000      ELSE                                                                
155100         MOVE MFS-ALFA-FAELT-FEL    TO                                    
155200                            MOD-KDSATAND2-UPDATE-ATTR                     
155300         MOVE NEJ                   TO  INDATA-SW                         
155400     END-IF                                                               
155500                                                                          
155600     IF W1-SRAD-KDSATKMB       =  SPACE                                   
155700         CONTINUE                                                         
155800      ELSE                                                                
155900         MOVE MFS-ALFA-FAELT-FEL    TO                                    
156000                            MOD-ING-IDARTNR1-UPDATE-ATTR                  
156100                            MOD-KDSATKMB-UPDATE-ATTR                      
156200         MOVE NEJ                   TO  INDATA-SW                         
156300     END-IF                                                               
156400                                                                          
156500     IF W2-SRAD-KDSATKMB       =  SPACE                                   
156600         CONTINUE                                                         
156700      ELSE                                                                
156800         MOVE MFS-ALFA-FAELT-FEL    TO                                    
156900                            MOD-ING-IDARTNR2-UPDATE-ATTR                  
157000                            MOD-KDSATKMB-UPDATE-ATTR                      
157100         MOVE NEJ                   TO  INDATA-SW                         
157200     END-IF                                                               
157300                                                                          
157400     PERFORM S05-KOLLA-REBEART-REANTPSA                                   
157500     .                                                                    
157600     EJECT                                                                
157700 GCDB-KOLLA-EN-ANDRAD-EN-NY-RAD    SECTION.                               
157800                                                                          
157900     MOVE W-ING-IDARTNR1-UPDATE         TO W-ING-IDARTNR                  
158000     PERFORM IMS-GU-SATG1-SATG11-KVAL                                     
158100     MOVE SRAD-WDJ211                   TO W1-SRAD-WDJ211                 
158200                                                                          
158300     MOVE W-ING-IDARTNR2-UPDATE  TO W-ING-IDARTNR                         
158400     PERFORM IMS-GU-SATG1-SATG11-KVAL                                     
158500     IF SEGMENT-FINNS AND SRAD-KDSATAND NOT = 'B'                         
158600         MOVE MFS-ALFA-FAELT-FEL TO  MOD-ING-IDARTNR2-UPDATE-ATTR         
158700         MOVE NEJ                TO  INDATA-SW                            
158800     END-IF                                                               
158900                                                                          
159000     MOVE W-ING-IDARTNR2-UPDATE         TO W-IDARTNR                      
159100                                        IN W-IDARTNR-X                    
159200     PERFORM S04-KOLLA-SPARRAR                                            
159300     IF ING-IDARTNR-SPARRAD                                               
159400         MOVE MFS-ALFA-FAELT-FEL TO                                       
159500                             MOD-ING-IDARTNR2-UPDATE-ATTR                 
159600         MOVE NEJ                  TO  INDATA-SW                          
159700     END-IF                                                               
159800                                                                          
159900     IF W1-SRAD-KDSATAND       =  'T' OR 'R' OR 'Ä' OR                    
160000                                  'N' OR SPACE                            
160100         CONTINUE                                                         
160200      ELSE                                                                
160300         MOVE MFS-ALFA-FAELT-FEL    TO                                    
160400                            MOD-KDSATAND1-UPDATE-ATTR                     
160500         MOVE NEJ                   TO  INDATA-SW                         
160600     END-IF                                                               
160700                                                                          
160800     IF W1-SRAD-KDSATKMB       =  SPACE                                   
160900         CONTINUE                                                         
161000      ELSE                                                                
161100         MOVE MFS-ALFA-FAELT-FEL    TO                                    
161200                            MOD-ING-IDARTNR1-UPDATE-ATTR                  
161300         MOVE NEJ                   TO  INDATA-SW                         
161400     END-IF                                                               
161500                                                                          
161600     PERFORM S05-KOLLA-REBEART-REANTPSA                                   
161700     .                                                                    
161800     EJECT                                                                
161900 GCE-KOLLA-GAMMAL-KDSATKMB              SECTION.                          
162000                                                                          
162100     IF 2303-MID-KDSATAND1-UPDATE           =  'B'  AND                   
162200        2303-MID-KDSATAND2-UPDATE           =  'B'                        
162300         PERFORM GCEA-KOLLA-TVA-ANNULL-RADER                              
162400     END-IF                                                               
162500                                                                          
162600     IF (2303-MID-KDSATAND1-UPDATE           =  'B'  AND                  
162700         2303-MID-KDSATAND2-UPDATE           =  'Ä')                      
162800         PERFORM GCEB-KOLLA-EN-ANNUL-RAD-EN-AND                           
162900     END-IF                                                               
163000                                                                          
163100     IF 2303-MID-KDSATAND1-UPDATE            =  'B'  AND                  
163200        2303-MID-KDSATAND2-UPDATE            =  'N'                       
163300         PERFORM GCEC-KOLLA-EN-ANNULL-EN-NY-RAD                           
163400     END-IF                                                               
163500                                                                          
163600     IF 2303-MID-KDSATAND1-UPDATE            =  'Ä'  AND                  
163700        2303-MID-KDSATAND2-UPDATE            =  'Ä'                       
163800         PERFORM GCED-KOLLA-TVA-ANDRADE-RADER                             
163900     END-IF                                                               
164000                                                                          
164100     IF 2303-MID-KDSATAND1-UPDATE            =  'Ä'  AND                  
164200        2303-MID-KDSATAND2-UPDATE            =  'N'                       
164300         PERFORM GCEE-KOLLA-EN-ANDRAD-EN-NY-RAD                           
164400     END-IF                                                               
164500                                                                          
164600     IF 2303-MID-KDSATAND1-UPDATE            =  'N'  AND                  
164700        2303-MID-KDSATAND2-UPDATE            =  'N'                       
164800         PERFORM GCEF-KOLLA-TVA-NYA-RADER                                 
164900     END-IF                                                               
165000     .                                                                    
165100     EJECT                                                                
165200 GCEA-KOLLA-TVA-ANNULL-RADER  SECTION.                                    
165300                                                                          
165400     MOVE W-ING-IDARTNR1-UPDATE         TO W-ING-IDARTNR                  
165500     PERFORM IMS-GU-SATG1-SATG11-KVAL                                     
165600     MOVE SRAD-WDJ211                   TO W1-SRAD-WDJ211                 
165700                                                                          
165800     MOVE W-ING-IDARTNR2-UPDATE         TO W-ING-IDARTNR                  
165900     PERFORM IMS-GU-SATG1-SATG11-KVAL                                     
166000     MOVE SRAD-WDJ211                   TO W2-SRAD-WDJ211                 
166100                                                                          
166200     IF W1-SRAD-KDSATKMB           =  W2-SRAD-KDSATKMB AND                
166300        W1-SRAD-KDSATKMB           =  2303-MID-KDSATKMB-UPDATE            
166400         MOVE MFS-ALFA-FAELT-RAETT      TO                                
166500                            MOD-ING-IDARTNR2-UPDATE-ATTR                  
166600      ELSE                                                                
166700         MOVE MFS-ALFA-FAELT-FEL        TO                                
166800                            MOD-ING-IDARTNR1-UPDATE-ATTR                  
166900                            MOD-KDSATKMB-UPDATE-ATTR                      
167000                            MOD-ING-IDARTNR2-UPDATE-ATTR                  
167100         MOVE NEJ                   TO  INDATA-SW                         
167200     END-IF                                                               
167300                                                                          
167400                                                                          
167500     IF W1-SRAD-KDSATAND       =  'T' OR 'R' OR 'Ä' OR 'E' OR             
167600                                  'N' OR SPACE                            
167700         CONTINUE                                                         
167800      ELSE                                                                
167900         MOVE MFS-ALFA-FAELT-FEL    TO                                    
168000                            MOD-KDSATAND1-UPDATE-ATTR                     
168100         MOVE NEJ                   TO  INDATA-SW                         
168200     END-IF                                                               
168300                                                                          
168400     IF W2-SRAD-KDSATAND       =  'T' OR 'R' OR 'Ä' OR 'E' OR             
168500                                  'N' OR SPACE                            
168600         CONTINUE                                                         
168700      ELSE                                                                
168800         MOVE MFS-ALFA-FAELT-FEL    TO                                    
168900                            MOD-KDSATAND2-UPDATE-ATTR                     
169000         MOVE NEJ                   TO  INDATA-SW                         
169100     END-IF                                                               
169200     .                                                                    
169300     EJECT                                                                
169400 GCEB-KOLLA-EN-ANNUL-RAD-EN-AND   SECTION.                                
169500                                                                          
169600     MOVE W-ING-IDARTNR1-UPDATE         TO W-ING-IDARTNR                  
169700     PERFORM IMS-GU-SATG1-SATG11-KVAL                                     
169800     MOVE SRAD-WDJ211                   TO W1-SRAD-WDJ211                 
169900                                                                          
170000     IF W1-SRAD-KDSATKMB       = 2303-MID-KDSATKMB-UPDATE                 
170100         CONTINUE                                                         
170200      ELSE                                                                
170300         MOVE MFS-ALFA-FAELT-FEL TO  MOD-KDSATKMB-UPDATE-ATTR             
170400         MOVE NEJ                TO  INDATA-SW                            
170500     END-IF                                                               
170600     MOVE W-ING-IDARTNR2-UPDATE         TO W-ING-IDARTNR                  
170700     PERFORM IMS-GU-SATG1-SATG11-KVAL                                     
170800     MOVE SRAD-WDJ211                   TO W2-SRAD-WDJ211                 
170900                                                                          
171000     IF W2-SRAD-KDSATKMB                =  SPACE                          
171100         CONTINUE                                                         
171200      ELSE                                                                
171300         IF W2-SRAD-KDSATKMB         =  W1-SRAD-KDSATKMB  OR              
171400            W2-SRAD-KDSATKMB         =  2303-MID-KDSATKMB-UPDATE          
171500             CONTINUE                                                     
171600          ELSE                                                            
171700             MOVE MFS-ALFA-FAELT-FEL TO                                   
171800                        MOD-ING-IDARTNR2-UPDATE-ATTR                      
171900                        MOD-KDSATKMB-UPDATE-ATTR                          
172000             MOVE NEJ          TO  INDATA-SW                              
172100         END-IF                                                           
172200     END-IF                                                               
172300     IF W2-SRAD-KDSATAND       =  'T' OR 'R' OR 'Ä' OR                    
172400                              'N' OR SPACE                                
172500         CONTINUE                                                         
172600      ELSE                                                                
172700         MOVE MFS-ALFA-FAELT-FEL TO                                       
172800                            MOD-KDSATAND2-UPDATE-ATTR                     
172900         MOVE NEJ                   TO  INDATA-SW                         
173000     END-IF                                                               
173100                                                                          
173200     IF W1-SRAD-KDSATAND       =  'T' OR 'R' OR 'Ä' OR 'E' OR             
173300                              'N' OR SPACE                                
173400         CONTINUE                                                         
173500      ELSE                                                                
173600         MOVE MFS-ALFA-FAELT-FEL TO                                       
173700                            MOD-KDSATAND1-UPDATE-ATTR                     
173800         MOVE NEJ                   TO  INDATA-SW                         
173900     END-IF                                                               
174000                                                                          
174100     PERFORM S05-KOLLA-REBEART-REANTPSA                                   
174200     .                                                                    
174300     EJECT                                                                
174400 GCEC-KOLLA-EN-ANNULL-EN-NY-RAD   SECTION.                                
174500                                                                          
174600     MOVE W-ING-IDARTNR1-UPDATE     TO W-ING-IDARTNR                      
174700     PERFORM IMS-GU-SATG1-SATG11-KVAL                                     
174800     MOVE SRAD-WDJ211               TO W1-SRAD-WDJ211                     
174900                                                                          
175000     IF W1-SRAD-KDSATKMB       = 2303-MID-KDSATKMB-UPDATE                 
175100         CONTINUE                                                         
175200      ELSE                                                                
175300         MOVE MFS-ALFA-FAELT-FEL TO  MOD-KDSATKMB-UPDATE-ATTR             
175400         MOVE NEJ                TO  INDATA-SW                            
175500     END-IF                                                               
175600                                                                          
175700     MOVE W-ING-IDARTNR2-UPDATE     TO W-ING-IDARTNR                      
175800     PERFORM IMS-GU-SATG1-SATG11-KVAL                                     
175900     IF SEGMENT-FINNS AND SRAD-KDSATAND NOT = 'B'                         
176000         MOVE MFS-ALFA-FAELT-FEL    TO                                    
176100                            MOD-ING-IDARTNR2-UPDATE-ATTR                  
176200         MOVE NEJ                   TO  INDATA-SW                         
176300     END-IF                                                               
176400                                                                          
176500     MOVE W-ING-IDARTNR2-UPDATE     TO W-IDARTNR                          
176600                                    IN W-IDARTNR-X                        
176700     PERFORM S04-KOLLA-SPARRAR                                            
176800     IF ING-IDARTNR-SPARRAD                                               
176900         MOVE MFS-ALFA-FAELT-FEL   TO                                     
177000                             MOD-ING-IDARTNR2-UPDATE-ATTR                 
177100         MOVE NEJ                  TO  INDATA-SW                          
177200     END-IF                                                               
177300                                                                          
177400     IF W1-SRAD-KDSATKMB             =  2303-MID-KDSATKMB-UPDATE          
177500         CONTINUE                                                         
177600      ELSE                                                                
177700         MOVE MFS-ALFA-FAELT-FEL TO                                       
177800                    MOD-ING-IDARTNR1-UPDATE-ATTR                          
177900                    MOD-KDSATKMB-UPDATE-ATTR                              
178000         MOVE NEJ              TO  INDATA-SW                              
178100     END-IF                                                               
178200                                                                          
178300     IF W1-SRAD-KDSATAND       =  'T' OR 'R' OR 'Ä' OR 'E' OR             
178400                                  'N' OR SPACE                            
178500         CONTINUE                                                         
178600      ELSE                                                                
178700         MOVE MFS-ALFA-FAELT-FEL TO                                       
178800                            MOD-KDSATAND1-UPDATE-ATTR                     
178900         MOVE NEJ                   TO  INDATA-SW                         
179000     END-IF                                                               
179100                                                                          
179200     PERFORM S05-KOLLA-REBEART-REANTPSA                                   
179300     .                                                                    
179400     EJECT                                                                
179500 GCED-KOLLA-TVA-ANDRADE-RADER   SECTION.                                  
179600                                                                          
179700     MOVE W-ING-IDARTNR1-UPDATE  TO W-ING-IDARTNR                         
179800     PERFORM IMS-GU-SATG1-SATG11-KVAL                                     
179900     MOVE SRAD-WDJ211            TO W1-SRAD-WDJ211                        
180000                                                                          
180100     MOVE W-ING-IDARTNR2-UPDATE  TO W-ING-IDARTNR                         
180200     PERFORM IMS-GU-SATG1-SATG11-KVAL                                     
180300     MOVE SRAD-WDJ211            TO W2-SRAD-WDJ211                        
180400                                                                          
180500     IF W1-SRAD-KDSATKMB             =  W2-SRAD-KDSATKMB AND              
180600        W1-SRAD-KDSATKMB             =  2303-MID-KDSATKMB-UPDATE          
180700         CONTINUE                                                         
180800      ELSE                                                                
180900         MOVE MFS-ALFA-FAELT-FEL TO                                       
181000                    MOD-ING-IDARTNR1-UPDATE-ATTR                          
181100                    MOD-ING-IDARTNR2-UPDATE-ATTR                          
181200                    MOD-KDSATKMB-UPDATE-ATTR                              
181300         MOVE NEJ              TO  INDATA-SW                              
181400     END-IF                                                               
181500                                                                          
181600     IF W1-SRAD-KDSATAND       =  'T' OR 'R' OR 'Ä' OR                    
181700                                  'N' OR SPACE                            
181800         CONTINUE                                                         
181900      ELSE                                                                
182000         MOVE MFS-ALFA-FAELT-FEL TO                                       
182100                            MOD-KDSATAND1-UPDATE-ATTR                     
182200         MOVE NEJ                   TO  INDATA-SW                         
182300     END-IF                                                               
182400                                                                          
182500     IF W2-SRAD-KDSATAND       =  'T' OR 'R' OR 'Ä' OR                    
182600                                  'N' OR SPACE                            
182700         CONTINUE                                                         
182800      ELSE                                                                
182900         MOVE MFS-ALFA-FAELT-FEL TO                                       
183000                            MOD-KDSATAND2-UPDATE-ATTR                     
183100         MOVE NEJ                   TO  INDATA-SW                         
183200     END-IF                                                               
183300                                                                          
183400     PERFORM S05-KOLLA-REBEART-REANTPSA                                   
183500     .                                                                    
183600     EJECT                                                                
183700 GCEE-KOLLA-EN-ANDRAD-EN-NY-RAD SECTION.                                  
183800                                                                          
183900     MOVE W-ING-IDARTNR1-UPDATE  TO W-ING-IDARTNR                         
184000     PERFORM IMS-GU-SATG1-SATG11-KVAL                                     
184100     MOVE SRAD-WDJ211            TO W1-SRAD-WDJ211                        
184200                                                                          
184300     MOVE W-ING-IDARTNR2-UPDATE  TO W-ING-IDARTNR                         
184400     PERFORM IMS-GU-SATG1-SATG11-KVAL                                     
184500     IF SEGMENT-FINNS AND SRAD-KDSATAND NOT = 'B'                         
184600         MOVE MFS-ALFA-FAELT-FEL TO MOD-ING-IDARTNR2-UPDATE-ATTR          
184700         MOVE NEJ                TO INDATA-SW                             
184800     END-IF                                                               
184900                                                                          
185000     MOVE W-ING-IDARTNR2-UPDATE  TO W-IDARTNR                             
185100                                 IN W-IDARTNR-X                           
185200     PERFORM S04-KOLLA-SPARRAR                                            
185300     IF ING-IDARTNR-SPARRAD                                               
185400         MOVE MFS-ALFA-FAELT-FEL TO                                       
185500                             MOD-ING-IDARTNR2-UPDATE-ATTR                 
185600         MOVE NEJ                  TO  INDATA-SW                          
185700     END-IF                                                               
185800                                                                          
185900     IF W1-SRAD-KDSATKMB             =  2303-MID-KDSATKMB-UPDATE          
186000         CONTINUE                                                         
186100      ELSE                                                                
186200         MOVE MFS-ALFA-FAELT-FEL TO                                       
186300                    MOD-ING-IDARTNR1-UPDATE-ATTR                          
186400                    MOD-KDSATKMB-UPDATE-ATTR                              
186500         MOVE NEJ              TO  INDATA-SW                              
186600     END-IF                                                               
186700                                                                          
186800     IF W1-SRAD-KDSATAND       =  'T' OR 'R' OR 'Ä' OR                    
186900                                  'N' OR SPACE                            
187000         CONTINUE                                                         
187100      ELSE                                                                
187200         MOVE MFS-ALFA-FAELT-FEL TO                                       
187300                            MOD-KDSATAND1-UPDATE-ATTR                     
187400         MOVE NEJ                   TO  INDATA-SW                         
187500     END-IF                                                               
187600                                                                          
187700     PERFORM S05-KOLLA-REBEART-REANTPSA                                   
187800     .                                                                    
187900     EJECT                                                                
188000 GCEF-KOLLA-TVA-NYA-RADER       SECTION.                                  
188100                                                                          
188200     MOVE W-ING-IDARTNR1-UPDATE  TO W-ING-IDARTNR                         
188300     PERFORM IMS-GU-SATG1-SATG11-KVAL                                     
188400     MOVE SRAD-WDJ211            TO W1-SRAD-WDJ211                        
188500     IF SEGMENT-FINNS AND W1-SRAD-KDSATAND = 'B'                          
188600         CONTINUE                                                         
188700      ELSE                                                                
188800         MOVE MFS-ALFA-FAELT-FEL TO MOD-ING-IDARTNR1-UPDATE-ATTR          
188900         MOVE NEJ                TO INDATA-SW                             
189000     END-IF                                                               
189100     MOVE W-ING-IDARTNR1-UPDATE TO W-IDARTNR                              
189200                                 IN W-IDARTNR-X                           
189300     PERFORM S04-KOLLA-SPARRAR                                            
189400     IF ING-IDARTNR-SPARRAD                                               
189500         MOVE MFS-ALFA-FAELT-FEL TO                                       
189600                             MOD-ING-IDARTNR1-UPDATE-ATTR                 
189700         MOVE NEJ                  TO  INDATA-SW                          
189800     END-IF                                                               
189900                                                                          
190000     MOVE W-ING-IDARTNR2-UPDATE TO W-ING-IDARTNR                          
190100     PERFORM IMS-GU-SATG1-SATG11-KVAL                                     
190200     MOVE SRAD-WDJ211           TO W2-SRAD-WDJ211                         
190300     IF SEGMENT-FINNS AND W2-SRAD-KDSATAND = 'B'                          
190400         CONTINUE                                                         
190500      ELSE                                                                
190600         MOVE MFS-ALFA-FAELT-FEL                                          
190700                             TO MOD-ING-IDARTNR2-UPDATE-ATTR              
190800         MOVE NEJ                TO INDATA-SW                             
190900     END-IF                                                               
191000                                                                          
191100     MOVE W-ING-IDARTNR2-UPDATE TO W-IDARTNR                              
191200                                 IN W-IDARTNR-X                           
191300     PERFORM S04-KOLLA-SPARRAR                                            
191400     IF ING-IDARTNR-SPARRAD                                               
191500         MOVE MFS-ALFA-FAELT-FEL TO                                       
191600                             MOD-ING-IDARTNR2-UPDATE-ATTR                 
191700         MOVE NEJ                  TO  INDATA-SW                          
191800     END-IF                                                               
191900                                                                          
192000     IF INDATA-OK                                                         
192100         IF W1-SRAD-KDSATKMB    =  2303-MID-KDSATKMB-UPDATE AND           
192200            W2-SRAD-KDSATKMB    =  2303-MID-KDSATKMB-UPDATE               
192300             CONTINUE                                                     
192400          ELSE                                                            
192500             MOVE MFS-ALFA-FAELT-FEL TO                                   
192600                        MOD-KDSATKMB-UPDATE-ATTR                          
192700             MOVE NEJ          TO  INDATA-SW                              
192800         END-IF                                                           
192900                                                                          
193000         PERFORM S05-KOLLA-REBEART-REANTPSA                               
193100     END-IF                                                               
193200     .                                                                    
193300     EJECT                                                                
193400 H-UPPDATERA SECTION.                                                     
193500                                                                          
193600     MOVE 'IDAG  '           TO DAT-KDDATFORM                             
193700     CALL WDATKONV USING        DAT-KDDATFORM DAT-I-TIDATUM               
193800                                DAT-O-TIDATUM DAT-KDSVAR                  
193900                                                                          
194000     IF DAT-KDSVAR-FEL                                                    
194100        MOVE '*** W20303  - FEL I DATKONV ***'  TO FELTEXT                
194200        CALL ABEND USING ABEND-UTAN-DUMP                                  
194300     END-IF                                                               
194400                                                                          
194500     MOVE SHUV-IDORDNSB        TO W-HELP-IDORDNSB                         
194600     MOVE SHUV-IDORDNSS        TO W-HELP-IDORDNSS                         
194700                                                                          
194800     IF 2303-MID-ING-IDARTNR2-UPDATE   =  SPACE OR ALL '+'                
194900         PERFORM HA-UPPD-EN-ING-ARTIKEL                                   
195000      ELSE                                                                
195100         IF 2303-MID-KDSATKMB-UPDATE = SPACE OR '+'                       
195200             PERFORM HB-UPPD-TVA-ING-ARTIKLAR                             
195300          ELSE                                                            
195400             PERFORM HC-UPPD-MED-KDSATKMB                                 
195500         END-IF                                                           
195600     END-IF                                                               
195700                                                                          
195800     PERFORM HD-BERAKNA-KVBYGGB-KOLLA-SPARR                               
195900                                                                          
196000     PERFORM IMS-GHU-SATG1-SATG01                                         
196100                                                                          
196200     IF W-MAX-KVBYGGB          =  9999999                                 
196300         MOVE ZERO             TO SHUV-KVBYGGB                            
196400      ELSE                                                                
196500         IF W-MAX-KVBYGGB      >  SHUV-KVBEART                            
196600             MOVE SHUV-KVBEART TO SHUV-KVBYGGB                            
196700          ELSE                                                            
196800             MOVE W-MAX-KVBYGGB TO SHUV-KVBYGGB                           
196900         END-IF                                                           
197000     END-IF                                                               
197100                                                                          
197200     IF SATS-SPARRAD                                                      
197300         MOVE ZERO             TO SHUV-KVBYGGB                            
197400      ELSE                                                                
197500         MOVE NEJ              TO SHUV-FLSATSPR                           
197600     END-IF                                                               
197700                                                                          
197800     IF SHUV-KVBYGGB           =  SHUV-KVBEART                            
197900         MOVE JA               TO SHUV-FLBYGGB                            
198000         PERFORM HE-BERAKNA-PTID                                          
198100         COMPUTE SHUV-VLORDNTO ROUNDED = W-VLARTNTO / 1000000             
198200         COMPUTE SHUV-VKORDNTO ROUNDED = W-VKARTNTO                       
198300      ELSE                                                                
198400         MOVE NEJ              TO SHUV-FLBYGGB                            
198500         MOVE ZERO             TO SHUV-SUSATPTI                           
198600                                  SHUV-VLORDNTO                           
198700                                  SHUV-VKORDNTO                           
198800     END-IF                                                               
198900                                                                          
199000     MOVE W-SHUV-KDSATKMB      TO SHUV-KDSATKMB                           
199100     MOVE DAT-TIAAMMDD         TO SHUV-TIUPPDAT                           
199200     PERFORM IMS-REPL-SATG1-SATG01                                        
199300                                                                          
199400     MOVE INF-UPDATE-DONE        TO MED-IDMFSINF                          
199500     CALL WMEDKONV USING MED-WMEDAREA                                     
199600     MOVE MED-TEMFSINF             TO MOD-TEMFSINF                        
199700     PERFORM MFS-FORM-ATTR                                                
199800     PERFORM MFS-RENSA-FAELT-IN                                           
199900     .                                                                    
200000     EJECT                                                                
200100 HA-UPPD-EN-ING-ARTIKEL         SECTION.                                  
200200                                                                          
200300     IF 2303-MID-KDSATAND1-UPDATE   =  'B'                                
200400         PERFORM S20-ANNULLERA                                            
200500     ELSE                                                                 
200600        IF 2303-MID-KDSATAND1-UPDATE   =  'N'                             
200700            PERFORM HAC-TILLAGG                                           
200800        END-IF                                                            
200900     END-IF                                                               
201000     .                                                                    
201100     EJECT                                                                
201200 HAC-TILLAGG                     SECTION.                                 
201300                                                                          
201400     MOVE W-ING-IDARTNR1-UPDATE TO  W-ING-IDARTNR-UPDATE                  
201500                                    W-ING-IDARTNR                         
201600     MOVE W-REBEART1-UPDATE     TO  W-REBEART-UPDATE                      
201700     MOVE W-REANTPSA1-UPDATE    TO  W-REANTPSA-UPDATE                     
201800     PERFORM S50-SKAPA-WDJ211                                             
201900                                                                          
202000     PERFORM IMS-GHU-ARTC1-ARTC11                                         
202100                                                                          
202200     PERFORM S51-FORSOK-RESERVERA                                         
202300     MOVE SRAD-KVSATRES        TO W-SPAR-KVSATRES                         
202400     MOVE SRAD-KVSATROS        TO W-SPAR-KVSATROS                         
202500                                                                          
202600     PERFORM S80-SKAPA-NY-RZD-O-TRANS                                     
202700                                                                          
202800     PERFORM IMS-REPL-ARTC1-ARTC11                                        
202900     PERFORM IMS-ISRT-SATG1-SATG11                                        
203000     IF SEGMENT-FINNS-REDAN                                               
203100         PERFORM IMS-GHU-SATG1-SATG11                                     
203200         MOVE W-SPAR-KVSATRES  TO SRAD-KVSATRES                           
203300         MOVE W-SPAR-KVSATROS  TO SRAD-KVSATROS                           
203400         MOVE W-REANTPSA-UPDATE TO SRAD-REANTPSA                          
203500         MOVE W-REBEART-UPDATE TO  SRAD-REBEART                           
203600         MOVE 'N'              TO SRAD-KDSATAND                           
203700         MOVE 'N'              TO SRAD-FLSATSPR                           
203800         PERFORM IMS-REPL-SATG1-SATG11                                    
203900     END-IF                                                               
204000     .                                                                    
204100     EJECT                                                                
204200 HB-UPPD-TVA-ING-ARTIKLAR       SECTION.                                  
204300                                                                          
204400     IF 2303-MID-KDSATAND1-UPDATE           = 'B' AND                     
204500        2303-MID-KDSATAND2-UPDATE           = 'N'                         
204600         PERFORM HBA-UPPD-EN-ANNULL-EN-NY-RAD                             
204700     END-IF                                                               
204800     .                                                                    
204900     EJECT                                                                
205000 HBA-UPPD-EN-ANNULL-EN-NY-RAD   SECTION.                                  
205100                                                                          
205200***   RADEN SOM SKALL ANNULLERAS                                          
205300     MOVE W-ING-IDARTNR1-UPDATE TO W-ING-IDARTNR                          
205400     PERFORM S20-ANNULLERA                                                
205500                                                                          
205600***   RADEN SOM SKALL LÄGGAS TILL                                         
205700     MOVE W-ING-IDARTNR2-UPDATE TO  W-ING-IDARTNR-UPDATE                  
205800                                    W-ING-IDARTNR                         
205900                                    W-IDARTNR                             
206000                                IN  W-IDARTNR-X                           
206100     MOVE W-REBEART2-UPDATE     TO  W-REBEART-UPDATE                      
206200     MOVE W-REANTPSA2-UPDATE    TO  W-REANTPSA-UPDATE                     
206300     PERFORM S50-SKAPA-WDJ211                                             
206400                                                                          
206500     PERFORM IMS-GHU-ARTC1-ARTC11                                         
206600                                                                          
206700     PERFORM S51-FORSOK-RESERVERA                                         
206800     MOVE SRAD-KVSATRES        TO W-SPAR-KVSATRES                         
206900     MOVE SRAD-KVSATROS        TO W-SPAR-KVSATROS                         
207000                                                                          
207100     PERFORM S80-SKAPA-NY-RZD-O-TRANS                                     
207200                                                                          
207300     PERFORM IMS-REPL-ARTC1-ARTC11                                        
207400     PERFORM IMS-ISRT-SATG1-SATG11                                        
207500     IF SEGMENT-FINNS-REDAN                                               
207600         PERFORM IMS-GHU-SATG1-SATG11                                     
207700         MOVE W-SPAR-KVSATRES  TO SRAD-KVSATRES                           
207800         MOVE W-SPAR-KVSATROS  TO SRAD-KVSATROS                           
207900         MOVE W-REANTPSA-UPDATE TO SRAD-REANTPSA                          
208000         MOVE W-REBEART-UPDATE TO  SRAD-REBEART                           
208100         MOVE 'N'              TO SRAD-KDSATAND                           
208200         MOVE 'N'              TO SRAD-FLSATSPR                           
208300         PERFORM IMS-REPL-SATG1-SATG11                                    
208400     END-IF                                                               
208500     .                                                                    
208600     EJECT                                                                
208700 HC-UPPD-MED-KDSATKMB           SECTION.                                  
208800                                                                          
208900     IF 2303-MID-KDSATAND1-UPDATE           = 'Ä' AND                     
209000        2303-MID-KDSATAND2-UPDATE           = 'Ä'                         
209100         PERFORM HCA-UPPD-TVA-ANDRADE-RADER                               
209200     END-IF                                                               
209300                                                                          
209400     IF 2303-MID-KDSATAND1-UPDATE           =  'Ä'  AND                   
209500        2303-MID-KDSATAND2-UPDATE           =  'N'                        
209600         PERFORM HCB-UPPD-EN-ANDRAD-EN-NY-RAD                             
209700     END-IF                                                               
209800                                                                          
209900     IF 2303-MID-KDSATAND1-UPDATE           =  'B'  AND                   
210000        2303-MID-KDSATAND2-UPDATE           =  'B'                        
210100         PERFORM HCC-UPPD-TVA-ANNULL-RADER                                
210200     END-IF                                                               
210300                                                                          
210400     IF (2303-MID-KDSATAND1-UPDATE          =  'B'  AND                   
210500         2303-MID-KDSATAND2-UPDATE          =  'Ä')                       
210600         PERFORM HCD-UPPD-EN-AND-EN-ANNUL-RAD                             
210700     END-IF                                                               
210800                                                                          
210900     IF 2303-MID-KDSATAND1-UPDATE           =  'B'  AND                   
211000        2303-MID-KDSATAND2-UPDATE           =  'N'                        
211100         PERFORM HCE-UPPD-EN-ANNULL-EN-NY-RAD                             
211200     END-IF                                                               
211300                                                                          
211400     IF 2303-MID-KDSATAND1-UPDATE           =  'N'  AND                   
211500        2303-MID-KDSATAND2-UPDATE           =  'N'                        
211600         PERFORM HCF-UPPD-TVA-NYA-RADER                                   
211700     END-IF                                                               
211800     .                                                                    
211900     EJECT                                                                
212000 HCA-UPPD-TVA-ANDRADE-RADER       SECTION.                                
212100                                                                          
212200*** FÖRSTA RADEN SOM SKALL ÄNDRAS                                         
212300     MOVE W-ING-IDARTNR1-UPDATE TO W-ING-IDARTNR                          
212400                                   W-IDARTNR                              
212500                                IN W-IDARTNR-X                            
212600                                                                          
212700     MOVE W-REBEART1-UPDATE     TO  W-REBEART-UPDATE                      
212800     MOVE W-REANTPSA1-UPDATE    TO  W-REANTPSA-UPDATE                     
212900     MOVE JA                    TO  REANTPSA-ANDRAD-SW                    
213000     PERFORM S30-ANDRA                                                    
213100                                                                          
213200*** ANDRA RADEN SOM SKALL ÄNDRAS                                          
213300     MOVE W-ING-IDARTNR2-UPDATE TO W-ING-IDARTNR                          
213400                                   W-IDARTNR                              
213500                                IN W-IDARTNR-X                            
213600                                                                          
213700     MOVE W-REBEART2-UPDATE     TO  W-REBEART-UPDATE                      
213800     MOVE W-REANTPSA2-UPDATE    TO  W-REANTPSA-UPDATE                     
213900                                                                          
214000     MOVE JA                    TO  REANTPSA-ANDRAD-SW                    
214100     PERFORM S30-ANDRA                                                    
214200                                                                          
214300     .                                                                    
214400     EJECT                                                                
214500 HCB-UPPD-EN-ANDRAD-EN-NY-RAD   SECTION.                                  
214600                                                                          
214700****  RADEN SOM SKALL ÄNDRAS                                              
214800     MOVE W-ING-IDARTNR1-UPDATE TO W-ING-IDARTNR                          
214900                                   W-IDARTNR                              
215000                                IN W-IDARTNR-X                            
215100                                                                          
215200     MOVE W-REBEART1-UPDATE     TO  W-REBEART-UPDATE                      
215300     MOVE W-REANTPSA1-UPDATE    TO  W-REANTPSA-UPDATE                     
215400     MOVE JA                    TO  REANTPSA-ANDRAD-SW                    
215500     PERFORM S30-ANDRA                                                    
215600                                                                          
215700****  RADEN SOM SKALL LÄGGAS TILL                                         
215800     MOVE W-ING-IDARTNR2-UPDATE TO  W-ING-IDARTNR-UPDATE                  
215900                                    W-ING-IDARTNR                         
216000                                    W-IDARTNR                             
216100                                IN  W-IDARTNR-X                           
216200     MOVE W-REBEART2-UPDATE     TO  W-REBEART-UPDATE                      
216300     MOVE W-REANTPSA2-UPDATE    TO  W-REANTPSA-UPDATE                     
216400     PERFORM S50-SKAPA-WDJ211                                             
216500                                                                          
216600     PERFORM IMS-GHU-ARTC1-ARTC11                                         
216700                                                                          
216800     PERFORM S51-FORSOK-RESERVERA                                         
216900     MOVE SRAD-KVSATRES        TO W-SPAR-KVSATRES                         
217000     MOVE SRAD-KVSATROS        TO W-SPAR-KVSATROS                         
217100                                                                          
217200     PERFORM S80-SKAPA-NY-RZD-O-TRANS                                     
217300                                                                          
217400     PERFORM IMS-REPL-ARTC1-ARTC11                                        
217500     PERFORM IMS-ISRT-SATG1-SATG11                                        
217600     IF SEGMENT-FINNS-REDAN                                               
217700         PERFORM IMS-GHU-SATG1-SATG11                                     
217800         MOVE W-SPAR-KVSATRES  TO SRAD-KVSATRES                           
217900         MOVE W-SPAR-KVSATROS  TO SRAD-KVSATROS                           
218000         MOVE W-REANTPSA-UPDATE TO SRAD-REANTPSA                          
218100         MOVE W-REBEART-UPDATE TO  SRAD-REBEART                           
218200         MOVE 'N'              TO SRAD-KDSATAND                           
218300         MOVE 'N'              TO SRAD-FLSATSPR                           
218400         PERFORM IMS-REPL-SATG1-SATG11                                    
218500     END-IF                                                               
218600     .                                                                    
218700     EJECT                                                                
218800 HCC-UPPD-TVA-ANNULL-RADER      SECTION.                                  
218900                                                                          
219000***  FÖRSTA RADEN SOM SKALL ANNULLERAS                                    
219100     MOVE W-ING-IDARTNR1-UPDATE TO W-ING-IDARTNR                          
219200     PERFORM S20-ANNULLERA                                                
219300                                                                          
219400***  ANDRA RADEN SOM SKALL ANNULLERAS                                     
219500     MOVE W-ING-IDARTNR2-UPDATE TO W-ING-IDARTNR                          
219600     PERFORM S20-ANNULLERA                                                
219700     .                                                                    
219800     EJECT                                                                
219900 HCD-UPPD-EN-AND-EN-ANNUL-RAD   SECTION.                                  
220000                                                                          
220100***  RADEN SOM SKALL ANNULLERAS                                           
220200     MOVE W-ING-IDARTNR1-UPDATE TO W-ING-IDARTNR                          
220300     PERFORM S20-ANNULLERA                                                
220400                                                                          
220500***  RADEN SOM SKALL ÄNDRAS                                               
220600     MOVE W-ING-IDARTNR2-UPDATE TO W-ING-IDARTNR                          
220700                                   W-IDARTNR                              
220800                                IN W-IDARTNR-X                            
220900                                                                          
221000     MOVE W-REBEART2-UPDATE     TO  W-REBEART-UPDATE                      
221100     MOVE W-REANTPSA2-UPDATE    TO  W-REANTPSA-UPDATE                     
221200     MOVE JA                    TO  REANTPSA-ANDRAD-SW                    
221300     PERFORM S30-ANDRA                                                    
221400     .                                                                    
221500     EJECT                                                                
221600 HCE-UPPD-EN-ANNULL-EN-NY-RAD   SECTION.                                  
221700                                                                          
221800***   RADEN SOM SKALL ANNULLERAS                                          
221900     MOVE W-ING-IDARTNR1-UPDATE TO W-ING-IDARTNR                          
222000     PERFORM S20-ANNULLERA                                                
222100                                                                          
222200***   RADEN SOM SKALL LÄGGAS TILL                                         
222300     MOVE W-ING-IDARTNR2-UPDATE TO  W-ING-IDARTNR-UPDATE                  
222400                                    W-ING-IDARTNR                         
222500                                    W-IDARTNR                             
222600                                IN  W-IDARTNR-X                           
222700     MOVE W-REBEART2-UPDATE     TO  W-REBEART-UPDATE                      
222800     MOVE W-REANTPSA2-UPDATE    TO  W-REANTPSA-UPDATE                     
222900     PERFORM S50-SKAPA-WDJ211                                             
223000                                                                          
223100     PERFORM IMS-GHU-ARTC1-ARTC11                                         
223200                                                                          
223300     PERFORM S51-FORSOK-RESERVERA                                         
223400                                                                          
223500     MOVE SRAD-KVSATRES        TO W-SPAR-KVSATRES                         
223600     MOVE SRAD-KVSATROS        TO W-SPAR-KVSATROS                         
223700                                                                          
223800     PERFORM S80-SKAPA-NY-RZD-O-TRANS                                     
223900                                                                          
224000     PERFORM IMS-REPL-ARTC1-ARTC11                                        
224100     PERFORM IMS-ISRT-SATG1-SATG11                                        
224200     IF SEGMENT-FINNS-REDAN                                               
224300         PERFORM IMS-GHU-SATG1-SATG11                                     
224400         MOVE W-SPAR-KVSATRES  TO SRAD-KVSATRES                           
224500         MOVE W-SPAR-KVSATROS  TO SRAD-KVSATROS                           
224600         MOVE W-REANTPSA-UPDATE TO SRAD-REANTPSA                          
224700         MOVE W-REBEART-UPDATE TO  SRAD-REBEART                           
224800         MOVE 'N'              TO SRAD-KDSATAND                           
224900         MOVE 'N'              TO SRAD-FLSATSPR                           
225000         PERFORM IMS-REPL-SATG1-SATG11                                    
225100     END-IF                                                               
225200     .                                                                    
225300     EJECT                                                                
225400 HCF-UPPD-TVA-NYA-RADER         SECTION.                                  
225500                                                                          
225600***  1:A RADEN SOM SKALL LÄGGAS TILL                                      
225700     MOVE W-ING-IDARTNR1-UPDATE TO  W-ING-IDARTNR-UPDATE                  
225800                                    W-ING-IDARTNR                         
225900                                    W-IDARTNR                             
226000                                IN  W-IDARTNR-X                           
226100     MOVE W-REBEART1-UPDATE     TO  W-REBEART-UPDATE                      
226200     MOVE W-REANTPSA1-UPDATE    TO  W-REANTPSA-UPDATE                     
226300     PERFORM S50-SKAPA-WDJ211                                             
226400                                                                          
226500     PERFORM IMS-GHU-ARTC1-ARTC11                                         
226600                                                                          
226700     PERFORM S51-FORSOK-RESERVERA                                         
226800                                                                          
226900     MOVE SRAD-KVSATRES        TO W-SPAR-KVSATRES                         
227000     MOVE SRAD-KVSATROS        TO W-SPAR-KVSATROS                         
227100                                                                          
227200     PERFORM S80-SKAPA-NY-RZD-O-TRANS                                     
227300                                                                          
227400     PERFORM IMS-REPL-ARTC1-ARTC11                                        
227500     PERFORM IMS-ISRT-SATG1-SATG11                                        
227600     IF SEGMENT-FINNS-REDAN                                               
227700         PERFORM IMS-GHU-SATG1-SATG11                                     
227800         MOVE W-SPAR-KVSATRES  TO SRAD-KVSATRES                           
227900         MOVE W-SPAR-KVSATROS  TO SRAD-KVSATROS                           
228000         MOVE W-REANTPSA-UPDATE TO SRAD-REANTPSA                          
228100         MOVE W-REBEART-UPDATE TO  SRAD-REBEART                           
228200         MOVE 'N'              TO SRAD-KDSATAND                           
228300         MOVE 'N'              TO SRAD-FLSATSPR                           
228400         PERFORM IMS-REPL-SATG1-SATG11                                    
228500     END-IF                                                               
228600                                                                          
228700***  2:A RADEN SOM SKALL LÄGGAS TILL                                      
228800     MOVE W-ING-IDARTNR2-UPDATE TO  W-ING-IDARTNR-UPDATE                  
228900                                    W-ING-IDARTNR                         
229000                                    W-IDARTNR                             
229100                                IN  W-IDARTNR-X                           
229200     MOVE W-REBEART2-UPDATE     TO  W-REBEART-UPDATE                      
229300     MOVE W-REANTPSA2-UPDATE    TO  W-REANTPSA-UPDATE                     
229400     PERFORM S50-SKAPA-WDJ211                                             
229500                                                                          
229600     PERFORM IMS-GHU-ARTC1-ARTC11                                         
229700                                                                          
229800     PERFORM S51-FORSOK-RESERVERA                                         
229900                                                                          
230000     MOVE SRAD-KVSATRES        TO W-SPAR-KVSATRES                         
230100     MOVE SRAD-KVSATROS        TO W-SPAR-KVSATROS                         
230200                                                                          
230300     PERFORM S80-SKAPA-NY-RZD-O-TRANS                                     
230400                                                                          
230500     PERFORM IMS-REPL-ARTC1-ARTC11                                        
230600     PERFORM IMS-ISRT-SATG1-SATG11                                        
230700     IF SEGMENT-FINNS-REDAN                                               
230800         PERFORM IMS-GHU-SATG1-SATG11                                     
230900         MOVE W-SPAR-KVSATRES  TO SRAD-KVSATRES                           
231000         MOVE W-SPAR-KVSATROS  TO SRAD-KVSATROS                           
231100         MOVE W-REANTPSA-UPDATE TO SRAD-REANTPSA                          
231200         MOVE W-REBEART-UPDATE TO  SRAD-REBEART                           
231300         MOVE 'N'              TO SRAD-KDSATAND                           
231400         MOVE 'N'              TO SRAD-FLSATSPR                           
231500         PERFORM IMS-REPL-SATG1-SATG11                                    
231600     END-IF                                                               
231700     .                                                                    
231800     EJECT                                                                
231900 HD-BERAKNA-KVBYGGB-KOLLA-SPARR SECTION.                                  
232000                                                                          
232100     MOVE ZERO                 TO W-KVRADER                               
232200     MOVE NEJ                  TO SATS-SPARRAD-SW                         
232300     MOVE 9999999              TO W-MAX-KVBYGGB                           
232400     PERFORM IMS-GU-SATG1-SATG11                                          
232500     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
232600         IF SRAD-KDSATAND      =  'B'  OR                                 
232700            (SRAD-KVSATRES     =  ZERO AND                                
232800             SRAD-KVSATROS     =  ZERO)                                   
232900             CONTINUE                                                     
233000          ELSE                                                            
233100             ADD +1            TO W-KVRADER                               
233200             IF SRAD-FLSATSPR           =  JA                             
233300                 MOVE JA                TO SATS-SPARRAD-SW                
233400             END-IF                                                       
233500             COMPUTE W-VKARTNTO ROUNDED = W-VKARTNTO    +                 
233600                                   (SRAD-VKARTNTO * SRAD-REBEART)         
233700             COMPUTE W-VLARTNTO ROUNDED = W-VLARTNTO    +                 
233800                                   (SRAD-VLARTNTO * SRAD-REBEART)         
233900             IF SRAD-KDSATKMB  = SPACE                                    
234000                 COMPUTE W-KVBYGGB = SRAD-KVSATRES /                      
234100                                      SRAD-REANTPSA                       
234200                 IF W-KVBYGGB  <  W-MAX-KVBYGGB                           
234300                     MOVE W-KVBYGGB TO W-MAX-KVBYGGB                      
234400                 END-IF                                                   
234500              ELSE                                                        
234600                 PERFORM HDA-BEHANDLA-KOMB-KVBYGGB                        
234700             END-IF                                                       
234800         END-IF                                                           
234900         PERFORM IMS-GN-SATG1-SATG11                                      
235000     END-PERFORM                                                          
235100                                                                          
235200     SET KOMB-IDX2             TO +1                                      
235300     PERFORM UNTIL KOMB-IDX2   > 26 OR                                    
235400                   KOMB-KVBYGGB-ING-IDARTNR(KOMB-IDX2) = ZERO             
235500         MOVE KOMB-KVBYGGB(KOMB-IDX2) TO  W-KVBYGGB                       
235600         IF W-KVBYGGB          <  W-MAX-KVBYGGB                           
235700             MOVE W-KVBYGGB    TO W-MAX-KVBYGGB                           
235800         END-IF                                                           
235900         SET KOMB-IDX2 UP BY +1                                           
236000     END-PERFORM                                                          
236100     .                                                                    
236200     EJECT                                                                
236300 HDA-BEHANDLA-KOMB-KVBYGGB       SECTION.                                 
236400                                                                          
236500     SET KOMB-IDX2             TO +1                                      
236600     MOVE NEJ                  TO KOMB-KVBYGGB-TRAFF-SW                   
236700     PERFORM UNTIL KOMB-IDX2   > 26 OR KOMB-KVBYGGB-TRAFF OR              
236800                   KOMB-KVBYGGB-ING-IDARTNR(KOMB-IDX2) = ZERO             
236900         IF SRAD-KDSATKMB      =  KOMB-KVBYGGB-KDSATKMB(KOMB-IDX2)        
237000             MOVE JA           TO KOMB-KVBYGGB-TRAFF-SW                   
237100          ELSE                                                            
237200             SET KOMB-IDX2 UP BY +1                                       
237300         END-IF                                                           
237400     END-PERFORM                                                          
237500                                                                          
237600     IF KOMB-KVBYGGB-TRAFF                                                
237700         CONTINUE                                                         
237800      ELSE                                                                
237900         PERFORM HDAA-BEHANDLA-NY-KDSATKMB                                
238000     END-IF                                                               
238100     .                                                                    
238200     EJECT                                                                
238300 HDAA-BEHANDLA-NY-KDSATKMB        SECTION.                                
238400                                                                          
238500     MOVE SRAD-IDARTNR         TO KOMB-KVBYGGB-ING-IDARTNR                
238600                                  (KOMB-IDX2)                             
238700                                  W-ING-IDARTNR                           
238800     COMPUTE KOMB-KVBYGGB(KOMB-IDX2) = SRAD-KVSATRES /                    
238900                                       SRAD-REANTPSA                      
239000     MOVE SRAD-KDSATKMB        TO KOMB-KVBYGGB-KDSATKMB(KOMB-IDX2)        
239100                                  W-SATG2-KDSATKMB-MIN                    
239200                                  W-SATG2-KDSATKMB-MAX                    
239300                                                                          
239400     PERFORM IMS-GU-SATG2-SATG11                                          
239500     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
239600         COMPUTE KOMB-KVBYGGB(KOMB-IDX2) = KOMB-KVBYGGB(KOMB-IDX2)        
239700                              +   (SRAD-KVSATRES / SRAD-REANTPSA)         
239800         PERFORM IMS-GN-SATG2-SATG11                                      
239900     END-PERFORM                                                          
240000                                                                          
240100     .                                                                    
240200     EJECT                                                                
240300 HE-BERAKNA-PTID                SECTION.                                  
240400                                                                          
240500     MOVE 'SATS'               TO  PTID-IDSYSTEM                          
240600     MOVE SHUV-IDORDNSB        TO  PTID-IDORDNSB                          
240700     MOVE SHUV-IDORDNSS        TO  PTID-IDORDNSS                          
240800     MOVE SHUV-IDARTNR         TO  PTID-IDARTNR                           
240900     MOVE SHUV-IDPRC           TO  PTID-IDPRC                             
241000     MOVE SHUV-KDCLAGER        TO  PTID-KDCLAGER                          
241100     MOVE SHUV-KVBYGGB         TO  PTID-KVBYGGB                           
241200     MOVE W-KVRADER            TO  PTID-KVRADER                           
241300                                   PTID-KVANTART                          
241400     MOVE ZERO                 TO  PTID-SUSATPTI                          
241500     MOVE SHUV-VKORDNTO        TO  PTID-VKORDNTO                          
241600     MOVE SHUV-VLORDNTO        TO  PTID-VLORDNTO                          
241700     MOVE SPACE                TO  PTID-KDSVAR                            
241800                                                                          
241900     CALL W416PTID USING PTID-W416PTID XXKH-PCB XXKI-PCB                  
242000                                       SATB-PCB                           
242100                                                                          
242200     IF PTID-KDSVAR            =   ZERO                                   
242300         MOVE PTID-SUSATPTI    TO  SHUV-SUSATPTI                          
242400      ELSE                                                                
242500         MOVE ZERO             TO  SHUV-SUSATPTI                          
242600     END-IF                                                               
242700     .                                                                    
242800     EJECT                                                                
242900 K-PPSW-2109-DISPATCH           SECTION.                                  
243000                                                                          
243100     MOVE +1350                 TO KMSG-KVLL                              
243200     MOVE LOW-VALUE             TO KMSG-KDZ1                              
243300                                   KMSG-KDZ2                              
243400     MOVE 'W2T109X'             TO KMSG-KDTRANS-1                         
243500     MOVE '2303'                TO KMSG-IDTRANS-1                         
243600     MOVE '1'                   TO KMSG-KDMFSFOR-1                        
243700                                                                          
243800     MOVE W-ANTAL-I-MID TO KOM-MID2-KVANTART                              
243900                                                                          
244000     CALL W006KOM USING MSG-PCB                                           
244100                        ALT-PCB                                           
244200                        KOMA-PCB                                          
244300                        MSG-KOM-WMSGKOM                                   
244400                        KMSG-IO-AREA                                      
244500     .                                                                    
244600     EJECT                                                                
244700 L-INITIERA-TRANS-DISPATCH      SECTION.                                  
244800                                                                          
244900     MOVE +54                   TO MSG-KOM-KVLL                           
245000     MOVE LOW-VALUE             TO MSG-KOM-KDZ1                           
245100                                   MSG-KOM-KDZ2                           
245200     MOVE SPACE                 TO MSG-KOM-KDTRANS                        
245300     MOVE 'W2I10902'            TO MSG-KOM-IDCPYTXT                       
245400     MOVE 'ORDERING'            TO MSG-KOM-IDSNDNOD                       
245500     MOVE IDPGM                 TO MSG-KOM-IDSNDJOB                       
245600                                                                          
245700     MOVE SPACE                 TO MSG-KOM-IDMFSMED                       
245800                                   MSG-KOM-KDSVAR                         
245900                                                                          
246000     ACCEPT W-DATUM             FROM DATE                                 
246100     ACCEPT W-TID               FROM TIME                                 
246200     MOVE W-DATUM               TO MSG-KOM-TIREGDAT                       
246300     MOVE W-TID                 TO MSG-KOM-TIKLOCK                        
246400                                                                          
246500     MOVE SPACE                 TO KOM-MID2-W2I10902                      
246600     .                                                                    
246700     EJECT                                                                
246800 S01-KOLLA-ING-IDARTNR   SECTION.                                         
246900                                                                          
247000     MOVE +1                   TO  INDX                                   
247100     MOVE NEJ                  TO  ING-IDARTNR-SW                         
247200                                                                          
247300     PERFORM UNTIL ING-IDARTNR-FINNS     OR                               
247400                   INDX        >   MAX-RAD-2303                           
247500         MOVE 2303-MID-ING-IDARTNR-RAD (INDX) (1:9) TO                    
247600                                             W-IDARTNR-RAD                
247700         INSPECT W-IDARTNR-RAD REPLACING LEADING SPACE BY ZERO            
247800         IF W-ING-IDARTNR-UPDATE =  W-IDARTNR-RAD                         
247900             MOVE JA             TO ING-IDARTNR-SW                        
248000          ELSE                                                            
248100             ADD +1              TO INDX                                  
248200         END-IF                                                           
248300     END-PERFORM                                                          
248400                                                                          
248500     .                                                                    
248600     EJECT                                                                
248700 S02-KOLLA-REANTPSA      SECTION.                                         
248800                                                                          
248900     MOVE 2303-MID-REANTPSA1-UPDATE  TO DEC-IDFRIDATA                     
249000     MOVE +2                         TO DEC-KVHELTAL                      
249100     MOVE +3                         TO DEC-KVDECIMAL                     
249200     CALL WDECEDIT USING DEC-WDECAREA                                     
249300                                                                          
249400     IF DEC-KDSVAR-OK                                                     
249500         MOVE DEC-IDEDITDATA   TO W-REANTPSA1-UPDATE                      
249600     END-IF                                                               
249700                                                                          
249800     IF DEC-KDSVAR-OK AND W-REANTPSA1-UPDATE > ZERO                       
249900         MOVE MFS-ALFA-FAELT-RAETT          TO                            
250000                   MOD-REANTPSA1-UPDATE-ATTR                              
250100      ELSE                                                                
250200         MOVE MFS-ALFA-FAELT-FEL            TO                            
250300                   MOD-REANTPSA1-UPDATE-ATTR                              
250400         MOVE NEJ              TO  INDATA-SW                              
250500     END-IF                                                               
250600     .                                                                    
250700     EJECT                                                                
250800 S03-KOLLA-REBEART       SECTION.                                         
250900                                                                          
251000     IF 2303-MID-REBEART1-UPDATE    =  SPACE OR ALL '+'                   
251100         COMPUTE W-REBEART1-UPDATE  =                                     
251200                  (SHUV-KVBEART * W-REANTPSA1-UPDATE) + 0.999             
251300         MOVE MFS-ALFA-FAELT-RAETT  TO  MOD-REBEART1-UPDATE-ATTR          
251400         MOVE W-REBEART1-UPDATE     TO W-REBEART-UPDATE                   
251500      ELSE                                                                
251600         COMPUTE W-REBEART1-UPDATE  =                                     
251700                  (SHUV-KVBEART * W-REANTPSA1-UPDATE) + 0.999             
251800         MOVE W-REBEART1-UPDATE        TO W-REBEART-UPDATE                
251900         IF 2303-MID-REBEART1-UPDATE   =  W-REBEART-UPDATE                
252000             MOVE MFS-ALFA-FAELT-RAETT TO MOD-REBEART1-UPDATE-ATTR        
252100          ELSE                                                            
252200             MOVE MFS-ALFA-FAELT-FEL   TO MOD-REBEART1-UPDATE-ATTR        
252300             MOVE NEJ                  TO INDATA-SW                       
252400         END-IF                                                           
252500     END-IF                                                               
252600                                                                          
252700     .                                                                    
252800     EJECT                                                                
252900 S04-KOLLA-SPARRAR       SECTION.                                         
253000                                                                          
253100     MOVE NEJ                  TO ING-IDARTNR-SPARRAD-SW                  
253200                                                                          
253300     PERFORM IMS-GU-ARTC1-ARTC11                                          
253400     IF SEGMENT-FINNS                                                     
253500        IF CLAG-PRARTSTD     =  ZERO                                      
253600            MOVE JA            TO ING-IDARTNR-SPARRAD-SW                  
253700        END-IF                                                            
253800                                                                          
253900        IF (SHUV-KDCLAGER = +1 AND (CLAG-KDLEVSP = 20 OR 21)) OR          
254000           (SHUV-KDCLAGER = +2 AND (CLAG-KDLEVSP = 20 OR 22))             
254100            MOVE JA            TO ING-IDARTNR-SPARRAD-SW                  
254200        END-IF                                                            
254300                                                                          
254400        IF CLAG-KDERS          >  +10                                     
254500            MOVE JA            TO ING-IDARTNR-SPARRAD-SW                  
254600        END-IF                                                            
254700      ELSE                                                                
254800            MOVE JA            TO ING-IDARTNR-SPARRAD-SW                  
254900     END-IF                                                               
255000     .                                                                    
255100     EJECT                                                                
255200 S05-KOLLA-REBEART-REANTPSA       SECTION.                                
255300                                                                          
255400     IF 2303-MID-KDSATAND1-UPDATE  = 'N' OR 'Ä'                           
255500       IF 2303-MID-REBEART1-UPDATE NUMERIC                                
255600         MOVE MFS-ALFA-FAELT-RAETT TO  MOD-REBEART1-UPDATE-ATTR           
255700         MOVE 2303-MID-REBEART1-UPDATE TO  W-REBEART1-UPDATE              
255800        ELSE                                                              
255900         MOVE MFS-ALFA-FAELT-FEL   TO  MOD-REBEART1-UPDATE-ATTR           
256000         MOVE NEJ                  TO  INDATA-SW                          
256100       END-IF                                                             
256200                                                                          
256300       MOVE 2303-MID-REANTPSA1-UPDATE TO DEC-IDFRIDATA                    
256400       MOVE +2                         TO DEC-KVHELTAL                    
256500       MOVE +3                         TO DEC-KVDECIMAL                   
256600       CALL WDECEDIT USING DEC-WDECAREA                                   
256700                                                                          
256800       IF DEC-KDSVAR-OK                                                   
256900           MOVE DEC-IDEDITDATA TO W-REANTPSA1-UPDATE                      
257000       END-IF                                                             
257100                                                                          
257200       IF DEC-KDSVAR-OK AND W-REANTPSA1-UPDATE > ZERO                     
257300          MOVE MFS-ALFA-FAELT-RAETT TO  MOD-REANTPSA1-UPDATE-ATTR         
257400          IF INDATA-OK                                                    
257500              DIVIDE W-REBEART1-UPDATE BY W-REANTPSA1-UPDATE              
257600                     GIVING     W-MULTIPEL                                
257700                     REMAINDER  W-REST                                    
257800              IF W-REST             >   ZERO                              
257900                  MOVE MFS-ALFA-FAELT-FEL TO                              
258000                                      MOD-REANTPSA1-UPDATE-ATTR           
258100                                      MOD-REBEART1-UPDATE-ATTR            
258200                  MOVE NEJ                TO  INDATA-SW                   
258300              END-IF                                                      
258400          END-IF                                                          
258500         ELSE                                                             
258600          MOVE MFS-ALFA-FAELT-FEL   TO  MOD-REANTPSA1-UPDATE-ATTR         
258700          MOVE NEJ                  TO  INDATA-SW                         
258800       END-IF                                                             
258900     END-IF                                                               
259000                                                                          
259100     IF 2303-MID-REBEART2-UPDATE  NUMERIC                                 
259200         MOVE MFS-ALFA-FAELT-RAETT TO  MOD-REBEART2-UPDATE-ATTR           
259300         MOVE 2303-MID-REBEART2-UPDATE TO  W-REBEART2-UPDATE              
259400      ELSE                                                                
259500         MOVE MFS-ALFA-FAELT-FEL   TO  MOD-REBEART2-UPDATE-ATTR           
259600         MOVE NEJ                  TO  INDATA-SW                          
259700     END-IF                                                               
259800                                                                          
259900     MOVE 2303-MID-REANTPSA2-UPDATE  TO DEC-IDFRIDATA                     
260000     MOVE +2                         TO DEC-KVHELTAL                      
260100     MOVE +3                         TO DEC-KVDECIMAL                     
260200     CALL WDECEDIT USING DEC-WDECAREA                                     
260300                                                                          
260400     IF DEC-KDSVAR-OK                                                     
260500         MOVE DEC-IDEDITDATA TO W-REANTPSA2-UPDATE                        
260600     END-IF                                                               
260700                                                                          
260800     IF DEC-KDSVAR-OK AND W-REANTPSA2-UPDATE > ZERO                       
260900         MOVE MFS-ALFA-FAELT-RAETT  TO  MOD-REANTPSA2-UPDATE-ATTR         
261000         IF INDATA-OK                                                     
261100             DIVIDE W-REBEART2-UPDATE BY W-REANTPSA2-UPDATE               
261200                     GIVING     W-MULTIPEL                                
261300                     REMAINDER  W-REST                                    
261400             IF W-REST             >   ZERO                               
261500                 MOVE MFS-ALFA-FAELT-FEL TO                               
261600                                     MOD-REANTPSA2-UPDATE-ATTR            
261700                                     MOD-REBEART2-UPDATE-ATTR             
261800                 MOVE NEJ                TO  INDATA-SW                    
261900             END-IF                                                       
262000         END-IF                                                           
262100      ELSE                                                                
262200         MOVE MFS-ALFA-FAELT-FEL    TO  MOD-REANTPSA2-UPDATE-ATTR         
262300         MOVE NEJ                   TO  INDATA-SW                         
262400     END-IF                                                               
262500     .                                                                    
262600     EJECT                                                                
262700 S20-ANNULLERA                   SECTION.                                 
262800                                                                          
262900     PERFORM IMS-GHU-SATG1-SATG11                                         
263000     MOVE SRAD-IDARTNR         TO  W-IDARTNR                              
263100                               IN  W-IDARTNR-X                            
263200                                                                          
263300     IF SRAD-KVSATRES          =  SRAD-REBEART                            
263400         PERFORM S21-UPPD-RESERV-ANNULLERA                                
263500      ELSE                                                                
263600         IF SRAD-KVSATROS      =  SRAD-REBEART                            
263700             PERFORM S22-UPPD-REST-ANNULLERA                              
263800          ELSE                                                            
263900             PERFORM S20A-DELVIS-RESERV-REST                              
264000         END-IF                                                           
264100     END-IF                                                               
264200                                                                          
264300     MOVE 'N'                  TO  SRAD-FLSATSPR                          
264400     MOVE 'B'                  TO  SRAD-KDSATAND                          
264500     MOVE SPACE                TO  SRAD-KDSATKMB                          
264600     MOVE ZERO                 TO  SRAD-REBEART                           
264700     PERFORM IMS-REPL-SATG1-SATG11                                        
264800                                                                          
264900     MOVE SRAD-REBEART         TO W-RZD-MINSKNING                         
265000     PERFORM S82-SKAPA-MINSK-RZD-A-TRANS                                  
265100     .                                                                    
265200     EJECT                                                                
265300 S20A-DELVIS-RESERV-REST         SECTION.                                 
265400                                                                          
265500     PERFORM S21-UPPD-RESERV-ANNULLERA                                    
265600     PERFORM S22-UPPD-REST-ANNULLERA                                      
265700     .                                                                    
265800     EJECT                                                                
265900                                                                          
266000 S21-UPPD-RESERV-ANNULLERA           SECTION.                             
266100                                                                          
266200     PERFORM IMS-GHU-ARTC1-ARTC11                                         
266300                                                                          
266400     COMPUTE CLAG-KVRESS       =   CLAG-KVRESS - SRAD-KVSATRES            
266500     PERFORM IMS-REPL-ARTC1-ARTC11                                        
266600                                                                          
266700     MOVE ZERO                 TO  SRAD-KVSATRES                          
266800     .                                                                    
266900     EJECT                                                                
267000 S22-UPPD-REST-ANNULLERA             SECTION.                             
267100                                                                          
267200     PERFORM S91-SKAPA-WDA5-NYCKEL                                        
267300     PERFORM IMS-GHU-ARTC1-ARTC11                                         
267400                                                                          
267500     PERFORM IMS-GHU-ORDP01-OKVAL                                         
267510     IF SEGMENT-FINNS                                                     
267600      IF RAD-KVART              =   SRAD-KVSATROS  AND                    
267700        RAD-KDSTARAD           =   '2'                                    
267800         COMPUTE CLAG-KVROS    =   CLAG-KVROS - RAD-KVART                 
267900         PERFORM IMS-DLET-ORDP01                                          
268000      ELSE                                                                
268100         PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                     
268200             IF RAD-KDSTARAD   =   '2'                                    
268300                 COMPUTE CLAG-KVROS    =                                  
268400                                   CLAG-KVROS - RAD-KVART                 
268500                 PERFORM IMS-DLET-ORDP01                                  
268600              ELSE                                                        
268700                 IF RAD-KDSTARAD  = '3'                                   
268800                     COMPUTE CLAG-KVRESS = CLAG-KVRESS -                  
268900                                             RAD-KVART                    
269000                     PERFORM IMS-DLET-ORDP01                              
269100                 END-IF                                                   
269200             END-IF                                                       
269300             PERFORM IMS-GHN-ORDP01                                       
269400         END-PERFORM                                                      
269500      END-IF                                                              
269600                                                                          
269700      PERFORM IMS-REPL-ARTC1-ARTC11                                       
269710     END-IF                                                               
269800                                                                          
269900     MOVE ZERO                 TO  SRAD-KVSATROS                          
270000     .                                                                    
270100     EJECT                                                                
270200 S30-ANDRA                       SECTION.                                 
270300                                                                          
270400     PERFORM IMS-GHU-SATG1-SATG11                                         
270500     MOVE SRAD-IDARTNR         TO  W-IDARTNR                              
270600                               IN  W-IDARTNR-X                            
270700                                                                          
270800     IF W-REBEART-UPDATE       <  SRAD-REBEART                            
270900            MOVE JA            TO REBEART-MINSKNING-SW                    
271000     END-IF                                                               
271100                                                                          
271200     IF REBEART-MINSKNING                                                 
271300         PERFORM  S30A-REBEART-MINSKNING                                  
271400         PERFORM  S82-SKAPA-MINSK-RZD-A-TRANS                             
271500      ELSE                                                                
271600         PERFORM  S30B-REBEART-OKNING                                     
271700         PERFORM  S81-SKAPA-OKNING-RZD-O-TRANS                            
271800     END-IF                                                               
271900                                                                          
272000     MOVE W-REBEART-UPDATE       TO  SRAD-REBEART                         
272100     IF REANTPSA-ANDRAD                                                   
272200         MOVE W-REANTPSA-UPDATE  TO SRAD-REANTPSA                         
272300     END-IF                                                               
272400     MOVE 'N'                  TO  SRAD-FLSATSPR                          
272500     MOVE 'Ä'                  TO  SRAD-KDSATAND                          
272600     MOVE W-KDSATKMB-UPDATE    TO  SRAD-KDSATKMB                          
272700     PERFORM IMS-REPL-SATG1-SATG11                                        
272800     .                                                                    
272900     EJECT                                                                
273000 S30A-REBEART-MINSKNING          SECTION.                                 
273100                                                                          
273200     COMPUTE W-MINSKNING       =  SRAD-REBEART - W-REBEART-UPDATE         
273300     MOVE W-MINSKNING          TO W-RZD-MINSKNING                         
273400     IF SRAD-KVSATRES          =  SRAD-REBEART                            
273500         PERFORM S31-UPPD-RESERV-ANDR-MINSKNING                           
273600      ELSE                                                                
273700         IF SRAD-KVSATROS      =  SRAD-REBEART                            
273800             PERFORM S32-UPPD-REST-ANDR-MINSKNING                         
273900          ELSE                                                            
274000             PERFORM S30AA-DELVIS-RESERV-REST                             
274100         END-IF                                                           
274200     END-IF                                                               
274300                                                                          
274400     .                                                                    
274500     EJECT                                                                
274600 S30AA-DELVIS-RESERV-REST         SECTION.                                
274700                                                                          
274800     IF W-MINSKNING            <= SRAD-KVSATROS                           
274900         PERFORM S32-UPPD-REST-ANDR-MINSKNING                             
275000      ELSE                                                                
275100         MOVE SRAD-KVSATROS    TO W-SPAR-KVSATROS                         
275200         PERFORM S32-UPPD-REST-ANDR-MINSKNING                             
275300         PERFORM S31-UPPD-RESERV-ANDR-MINSKNING                           
275400     END-IF                                                               
275500     .                                                                    
275600     EJECT                                                                
275700 S30B-REBEART-OKNING             SECTION.                                 
275800                                                                          
275900     COMPUTE W-OKNING          =  W-REBEART-UPDATE - SRAD-REBEART         
276000     MOVE W-OKNING             TO W-RZD-OKNING                            
276100     PERFORM IMS-GHU-ARTC1-ARTC11                                         
276200                                                                          
276300     MOVE ZERO                 TO W-KVPREAVB                              
276400     PERFORM IMS-GU-ARTM01                                                
276500     IF SEGMENT-FINNS                                                     
276600         COMPUTE W-KVPREAVB    =  ART-KVPREAVB-DAG +                      
276700                                  ART-KVPREAVB-VOR                        
276800      ELSE                                                                
276900         MOVE ZERO             TO W-KVPREAVB                              
277000     END-IF                                                               
277100                                                                          
277200     COMPUTE W-TILLG-KVLS      =  CLAG-KVLS -                             
277300                                  CLAG-KVRESS -                           
277400                                  CLAG-KVSPANT -                          
277500                                  CLAG-KVUTRS    -                        
277600                                  W-KVPREAVB                              
277700     END-COMPUTE                                                          
277800                                                                          
277900     IF SRAD-KVSATRES          =  SRAD-REBEART                            
278000         PERFORM S40-UPPD-ANDR-OKNING                                     
278100      ELSE                                                                
278200         PERFORM S40-UPPD-ANDR-OKNING                                     
278300         PERFORM S30BA-BEHANDLA-WDA5                                      
278400     END-IF                                                               
278500                                                                          
278600     PERFORM IMS-REPL-ARTC1-ARTC11                                        
278700     .                                                                    
278800     EJECT                                                                
278900 S30BA-BEHANDLA-WDA5             SECTION.                                 
279000                                                                          
279100     MOVE LOW-VALUE            TO W-WDA5B1KY-MIN-X                        
279200     MOVE HIGH-VALUE           TO W-WDA5B1KY-MAX-X                        
279300                                                                          
279400     MOVE SHUV-IDDISTR         TO W-A5B1KY-MIN-IDDISTR                    
279500                                  W-A5B1KY-MAX-IDDISTR                    
279600                                                                          
279700     MOVE SHUV-IDKUNDNR        TO W-A5B1KY-MIN-IDKUNDNR                   
279800                                  W-A5B1KY-MAX-IDKUNDNR                   
279900                                                                          
280000     MOVE WC-CDC-SE            TO W-A5B1KY-MIN-IDDC                       
280100                                  W-A5B1KY-MAX-IDDC                       
280200                                                                          
280300     MOVE SPACE                TO W-A5B1KY-MIN-IDKUNDRF                   
280400                                  W-A5B1KY-MAX-IDKUNDRF                   
280500                                                                          
280600     MOVE W-HELP-IDORDNST      TO W-A5B1KY-MIN-IDORDNR5                   
280700                                  W-A5B1KY-MAX-IDORDNR5                   
280800                                                                          
280900     MOVE SRAD-IDARTNR         TO W-A5B1KY-MIN-IDARTNR                    
281000                                  W-A5B1KY-MAX-IDARTNR                    
281100                                                                          
281200     PERFORM IMS-GHU-ORDR01                                               
281300     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
281400         COMPUTE SRAD-KVSATRES = SRAD-KVSATRES + SEQB-KVART               
281500         COMPUTE SRAD-KVSATROS = SRAD-KVSATROS - SEQB-KVART               
281600                                                                          
281700         PERFORM S30BAA-SKAPA-WDA5-NYCKEL                                 
281800         PERFORM IMS-GHU-ORDP01-KVAL                                      
281900         PERFORM IMS-DLET-ORDP01                                          
282000                                                                          
282100         PERFORM IMS-GHN-ORDR01                                           
282200     END-PERFORM                                                          
282300     .                                                                    
282400     EJECT                                                                
282500 S30BAA-SKAPA-WDA5-NYCKEL        SECTION.                                 
282600                                                                          
282700     MOVE SEQB-IDDISTR         TO W-A501KY-IDDISTR                        
282800     MOVE SEQB-IDKUNDNR        TO W-A501KY-IDKUNDNR                       
282900     MOVE SEQB-IDKUNDRF        TO W-A501KY-IDKUNDRF                       
283000     MOVE SEQB-IDARTNR         TO W-A501KY-IDARTNR                        
283100     MOVE SEQB-IDLOPNR         TO W-A501KY-IDLOPNR                        
283200     .                                                                    
283300     EJECT                                                                
283400 S31-UPPD-RESERV-ANDR-MINSKNING      SECTION.                             
283500                                                                          
283600     PERFORM IMS-GHU-ARTC1-ARTC11                                         
283700                                                                          
283800     COMPUTE CLAG-KVRESS       =   CLAG-KVRESS - W-MINSKNING              
283900     COMPUTE SRAD-KVSATRES     =   SRAD-KVSATRES - W-MINSKNING            
284000     PERFORM IMS-REPL-ARTC1-ARTC11                                        
284100     .                                                                    
284200     EJECT                                                                
284300 S32-UPPD-REST-ANDR-MINSKNING        SECTION.                             
284400                                                                          
284500     PERFORM S91-SKAPA-WDA5-NYCKEL                                        
284600     PERFORM IMS-GHU-ARTC1-ARTC11                                         
284700                                                                          
284800     PERFORM IMS-GHU-ORDP01-OKVAL                                         
284900     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                      
285000                   W-MINSKNING = ZERO                                     
285100         IF RAD-KDSTARAD       =   '2'                                    
285200             PERFORM S32A-BEHANDLA-EJ-TACKT-RAD                           
285300         END-IF                                                           
285400                                                                          
285500         IF RAD-KDSTARAD       = '3'                                      
285600             PERFORM S32B-BEHANDLA-TACKT-RAD                              
285700         END-IF                                                           
285800         PERFORM IMS-GHN-ORDP01                                           
285900     END-PERFORM                                                          
286000                                                                          
286100     PERFORM IMS-REPL-ARTC1-ARTC11                                        
286200     .                                                                    
286300     EJECT                                                                
286400 S32A-BEHANDLA-EJ-TACKT-RAD    SECTION.                                   
286500                                                                          
286600     EVALUATE TRUE                                                        
286700                                                                          
286800     WHEN RAD-KVART            =   W-MINSKNING                            
286900         COMPUTE SRAD-KVSATROS =   SRAD-KVSATROS  - W-MINSKNING           
287000         COMPUTE CLAG-KVROS    =   CLAG-KVROS     - W-MINSKNING           
287100         COMPUTE W-MINSKNING   =   W-MINSKNING    - RAD-KVART             
287200         PERFORM IMS-DLET-ORDP01                                          
287300                                                                          
287400     WHEN RAD-KVART            > W-MINSKNING                              
287500         COMPUTE SRAD-KVSATROS =   SRAD-KVSATROS  - W-MINSKNING           
287600         COMPUTE CLAG-KVROS    =   CLAG-KVROS     - W-MINSKNING           
287700         COMPUTE RAD-KVART     =   RAD-KVART      - W-MINSKNING           
287800         PERFORM IMS-REPL-ORDP01                                          
287900                                                                          
288000     WHEN RAD-KVART            < W-MINSKNING                              
288100         COMPUTE CLAG-KVROS    =   CLAG-KVROS    - RAD-KVART              
288200         COMPUTE SRAD-KVSATROS =   SRAD-KVSATROS - RAD-KVART              
288300         COMPUTE W-MINSKNING   =   W-MINSKNING   - RAD-KVART              
288400         PERFORM IMS-DLET-ORDP01                                          
288500                                                                          
288600     END-EVALUATE                                                         
288700     .                                                                    
288800     EJECT                                                                
288900 S32B-BEHANDLA-TACKT-RAD       SECTION.                                   
289000                                                                          
289100     EVALUATE TRUE                                                        
289200                                                                          
289300     WHEN RAD-KVART            =  W-MINSKNING                             
289400         COMPUTE SRAD-KVSATROS =  SRAD-KVSATROS - W-MINSKNING             
289500         COMPUTE CLAG-KVRESS   =  CLAG-KVRESS   - W-MINSKNING             
289600         COMPUTE W-MINSKNING   =  W-MINSKNING   - RAD-KVART               
289700         PERFORM IMS-DLET-ORDP01                                          
289800                                                                          
289900     WHEN RAD-KVART            >  W-MINSKNING                             
290000         COMPUTE W-SKILLNAD    =  RAD-KVART     - W-MINSKNING             
290100         COMPUTE CLAG-KVRESS   =  CLAG-KVRESS   - W-MINSKNING             
290200         COMPUTE SRAD-KVSATROS =  SRAD-KVSATROS - RAD-KVART               
290300         COMPUTE SRAD-KVSATRES =  SRAD-KVSATRES + W-SKILLNAD              
290400         PERFORM IMS-DLET-ORDP01                                          
290500                                                                          
290600     WHEN RAD-KVART            <  W-MINSKNING                             
290700         COMPUTE SRAD-KVSATROS =  SRAD-KVSATROS - RAD-KVART               
290800         COMPUTE CLAG-KVRESS   =  CLAG-KVRESS   - RAD-KVART               
290900         COMPUTE W-MINSKNING   =  W-MINSKNING   - RAD-KVART               
291000         PERFORM IMS-DLET-ORDP01                                          
291100                                                                          
291200     END-EVALUATE                                                         
291300     .                                                                    
291400     EJECT                                                                
291500 S40-UPPD-ANDR-OKNING   SECTION.                                          
291600                                                                          
291700     IF W-TILLG-KVLS          <=  ZERO   OR                               
291800        ING-IDARTNR-SPARRAD                                               
291900                                                                          
292000         MOVE W-OKNING         TO  W-KVRO                                 
292100                                   W-KVART                                
292200                                                                          
292300         PERFORM S90-SKAPA-RESTORDER                                      
292400                                                                          
292500         COMPUTE CLAG-KVROS      = CLAG-KVROS    + W-OKNING               
292600         COMPUTE SRAD-KVSATROS   = SRAD-KVSATROS + W-OKNING               
292700      ELSE                                                                
292800         IF W-TILLG-KVLS         >=  W-OKNING                             
292900             COMPUTE CLAG-KVRESS   = CLAG-KVRESS   + W-OKNING             
293000             COMPUTE SRAD-KVSATRES = SRAD-KVSATRES + W-OKNING             
293100         END-IF                                                           
293200                                                                          
293300         IF W-TILLG-KVLS       >  ZERO AND                                
293400            W-TILLG-KVLS       <  W-OKNING                                
293500             COMPUTE CLAG-KVRESS   = CLAG-KVRESS   + W-TILLG-KVLS         
293600             COMPUTE SRAD-KVSATRES = SRAD-KVSATRES + W-TILLG-KVLS         
293700                                                                          
293800             COMPUTE W-EJ-TACKBART = W-OKNING - W-TILLG-KVLS              
293900                                                                          
294000             MOVE W-EJ-TACKBART    TO W-KVRO                              
294100                                      W-KVART                             
294200                                                                          
294300             PERFORM S90-SKAPA-RESTORDER                                  
294400                                                                          
294500             COMPUTE CLAG-KVROS    = CLAG-KVROS    + W-EJ-TACKBART        
294600             COMPUTE SRAD-KVSATROS = SRAD-KVSATROS + W-EJ-TACKBART        
294700         END-IF                                                           
294800     END-IF                                                               
294900     .                                                                    
295000     EJECT                                                                
295100 S50-SKAPA-WDJ211              SECTION.                                   
295200                                                                          
295300     PERFORM IMS-GU-ARTC3-ARTC01                                          
295400                                                                          
295500     MOVE W-ING-IDARTNR-UPDATE TO  SRAD-IDARTNR                           
295600     MOVE SHUV-KDCLAGER        TO  SRAD-KDCLAGER                          
295700     MOVE SHUV-IDKONTO         TO  SRAD-IDKONTO                           
295800     MOVE SHUV-IDKST           TO  SRAD-IDKST                             
295900     MOVE SHUV-IDANALYS        TO  SRAD-IDANALYS                          
296000     MOVE W-REANTPSA-UPDATE    TO  SRAD-REANTPSA                          
296100     MOVE W-REBEART-UPDATE     TO  SRAD-REBEART                           
296200     MOVE 'N'                  TO  SRAD-KDSATAND                          
296300     MOVE NEJ                  TO  SRAD-FLSATRAS                          
296400                                   SRAD-FLSATUTS                          
296500     MOVE SPACE                TO  SRAD-KDSATKMB                          
296600     MOVE +0                   TO  SRAD-PRARTSTD                          
296700                                   SRAD-KVSATRES                          
296800                                   SRAD-KVSATROS                          
296900     MOVE ART-REKSIFFR         TO  SRAD-REKSIFFR                          
297000     MOVE ART-KDSORT           TO  SRAD-KDSORT                            
297100     MOVE ART-KDPRODSL         TO  SRAD-KDPRODSL                          
297200                                   TEST-KDPRODSL                          
297300     PERFORM IMS-GNP-ARTC3-ARTC11                                         
297400     MOVE C3-CLAG-VLARTNTO     TO  SRAD-VLARTNTO                          
297500     COMPUTE SRAD-VKARTNTO     =   C3-CLAG-VKART / 1000                   
297600     MOVE W-KDSATKMB-UPDATE    TO  SRAD-KDSATKMB                          
297700     IF KDPRODSL-VOLVO-EMB                                                
297800         MOVE '9'              TO  SRAD-KDSTRRAD                          
297900      ELSE                                                                
298000         MOVE '0'              TO  SRAD-KDSTRRAD                          
298100     END-IF                                                               
298200     .                                                                    
298300     EJECT                                                                
298400 S51-FORSOK-RESERVERA          SECTION.                                   
298500                                                                          
298600     MOVE ZERO                 TO W-KVPREAVB                              
298700     PERFORM IMS-GU-ARTM01                                                
298800     IF SEGMENT-FINNS                                                     
298900         COMPUTE W-KVPREAVB    =  ART-KVPREAVB-DAG +                      
299000                                  ART-KVPREAVB-VOR                        
299100      ELSE                                                                
299200         MOVE ZERO             TO W-KVPREAVB                              
299300     END-IF                                                               
299400                                                                          
299500     COMPUTE W-TILLG-KVLS      =  CLAG-KVLS -                             
299600                                  CLAG-KVRESS -                           
299700                                  CLAG-KVSPANT -                          
299800                                  CLAG-KVUTRS    -                        
299900                                  W-KVPREAVB                              
300000     END-COMPUTE                                                          
300100                                                                          
300200     IF W-TILLG-KVLS          <=  ZERO                                    
300300                                                                          
300400         MOVE W-REBEART-UPDATE TO  W-KVRO                                 
300500                                   W-KVART                                
300600                                                                          
300700         PERFORM S90-SKAPA-RESTORDER                                      
300800                                                                          
300900         COMPUTE CLAG-KVROS    = CLAG-KVROS    + W-REBEART-UPDATE         
301000         COMPUTE SRAD-KVSATROS = SRAD-KVSATROS + W-REBEART-UPDATE         
301100     END-IF                                                               
301200                                                                          
301300     IF W-TILLG-KVLS           >= W-REBEART-UPDATE                        
301400         COMPUTE CLAG-KVRESS   = CLAG-KVRESS   + W-REBEART-UPDATE         
301500         COMPUTE SRAD-KVSATRES = SRAD-KVSATRES + W-REBEART-UPDATE         
301600     END-IF                                                               
301700                                                                          
301800     IF W-TILLG-KVLS           >  ZERO AND                                
301900        W-TILLG-KVLS           <  W-REBEART-UPDATE                        
302000         COMPUTE CLAG-KVRESS   = CLAG-KVRESS + W-TILLG-KVLS               
302100         COMPUTE SRAD-KVSATRES = SRAD-KVSATRES + W-TILLG-KVLS             
302200                                                                          
302300         COMPUTE W-EJ-TACKBART = W-REBEART-UPDATE -                       
302400                                 W-TILLG-KVLS                             
302500                                                                          
302600         MOVE W-EJ-TACKBART    TO W-KVRO                                  
302700                                  W-KVART                                 
302800                                                                          
302900         PERFORM S90-SKAPA-RESTORDER                                      
303000                                                                          
303100         COMPUTE CLAG-KVROS    = CLAG-KVROS    + W-EJ-TACKBART            
303200         COMPUTE SRAD-KVSATROS = SRAD-KVSATROS + W-EJ-TACKBART            
303300     END-IF                                                               
303400     .                                                                    
303500     EJECT                                                                
303600 S80-SKAPA-NY-RZD-O-TRANS SECTION.                                        
303700                                                                          
303800                                                                          
303900*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
304000     MOVE SRAD-IDARTNR       TO BYT03-IDARTNR                             
304100     IF NOT BYT03-OBJEKT                                                  
304200                                                                          
304300      ADD +1 TO W-ANTAL-I-MID                                             
304400                                                                          
304500      IF W-ANTAL-I-MID > MAX-RAD-MID2                                     
304600         MOVE MAX-RAD-MID2 TO W-ANTAL-I-MID                               
304700         PERFORM K-PPSW-2109-DISPATCH                                     
304800         MOVE +1 TO W-ANTAL-I-MID                                         
304900         MOVE NEJ TO SW-PPSW-2109                                         
305000      END-IF                                                              
305100                                                                          
305200      IF SW-PPSW-2109 = NEJ                                               
305300         MOVE JA TO SW-PPSW-2109                                          
305400         PERFORM L-INITIERA-TRANS-DISPATCH                                
305500      END-IF                                                              
305600                                                                          
305700      MOVE SRAD-IDARTNR   TO KOM-MID2-IDARTNR   (W-ANTAL-I-MID)           
305800      MOVE WC-CDC-SE      TO KOM-MID2-IDDC      (W-ANTAL-I-MID)           
305900      MOVE DAT-TIAAMMDD   TO KOM-MID2-TIUPPDAT  (W-ANTAL-I-MID)           
306000      MOVE '+'            TO KOM-MID2-KDTECKEN  (W-ANTAL-I-MID)           
306100      MOVE 'KI'           TO KOM-MID2-KDOI      (W-ANTAL-I-MID)           
306200      MOVE SPACE          TO KOM-MID2-CLEARGROUP(W-ANTAL-I-MID)           
306300      MOVE SRAD-REBEART   TO KOM-MID2-KVOI      (W-ANTAL-I-MID)           
306400     END-IF                                                               
306500     .                                                                    
306600     EJECT                                                                
306700 S81-SKAPA-OKNING-RZD-O-TRANS SECTION.                                    
306800                                                                          
306900                                                                          
307000*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
307100     MOVE SRAD-IDARTNR       TO BYT03-IDARTNR                             
307200     IF NOT BYT03-OBJEKT                                                  
307300                                                                          
307400      ADD +1 TO W-ANTAL-I-MID                                             
307500                                                                          
307600      IF W-ANTAL-I-MID > MAX-RAD-MID2                                     
307700         MOVE MAX-RAD-MID2 TO W-ANTAL-I-MID                               
307800         PERFORM K-PPSW-2109-DISPATCH                                     
307900         MOVE +1 TO W-ANTAL-I-MID                                         
308000         MOVE NEJ TO SW-PPSW-2109                                         
308100      END-IF                                                              
308200                                                                          
308300      IF SW-PPSW-2109 = NEJ                                               
308400         MOVE JA TO SW-PPSW-2109                                          
308500         PERFORM L-INITIERA-TRANS-DISPATCH                                
308600      END-IF                                                              
308700                                                                          
308800      MOVE SRAD-IDARTNR   TO KOM-MID2-IDARTNR   (W-ANTAL-I-MID)           
308900      MOVE WC-CDC-SE      TO KOM-MID2-IDDC      (W-ANTAL-I-MID)           
309000      MOVE DAT-TIAAMMDD   TO KOM-MID2-TIUPPDAT  (W-ANTAL-I-MID)           
309100      MOVE '+'            TO KOM-MID2-KDTECKEN  (W-ANTAL-I-MID)           
309200      MOVE 'KI'           TO KOM-MID2-KDOI      (W-ANTAL-I-MID)           
309300      MOVE SPACE          TO KOM-MID2-CLEARGROUP(W-ANTAL-I-MID)           
309400      MOVE W-RZD-OKNING   TO KOM-MID2-KVOI      (W-ANTAL-I-MID)           
309500     END-IF                                                               
309600     .                                                                    
309700     EJECT                                                                
309800 S82-SKAPA-MINSK-RZD-A-TRANS SECTION.                                     
309900                                                                          
310000                                                                          
310100*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
310200     MOVE SRAD-IDARTNR       TO BYT03-IDARTNR                             
310300     IF NOT BYT03-OBJEKT                                                  
310400                                                                          
310500      ADD +1 TO W-ANTAL-I-MID                                             
310600                                                                          
310700      IF W-ANTAL-I-MID > MAX-RAD-MID2                                     
310800         MOVE MAX-RAD-MID2 TO W-ANTAL-I-MID                               
310900         PERFORM K-PPSW-2109-DISPATCH                                     
311000         MOVE +1 TO W-ANTAL-I-MID                                         
311100         MOVE NEJ TO SW-PPSW-2109                                         
311200      END-IF                                                              
311300                                                                          
311400      IF SW-PPSW-2109 = NEJ                                               
311500         MOVE JA TO SW-PPSW-2109                                          
311600         PERFORM L-INITIERA-TRANS-DISPATCH                                
311700      END-IF                                                              
311800                                                                          
311900      MOVE SRAD-IDARTNR    TO KOM-MID2-IDARTNR   (W-ANTAL-I-MID)          
312000      MOVE WC-CDC-SE       TO KOM-MID2-IDDC      (W-ANTAL-I-MID)          
312100      MOVE DAT-TIAAMMDD    TO KOM-MID2-TIUPPDAT  (W-ANTAL-I-MID)          
312200      MOVE '-'             TO KOM-MID2-KDTECKEN  (W-ANTAL-I-MID)          
312300      MOVE 'KI'            TO KOM-MID2-KDOI      (W-ANTAL-I-MID)          
312400      MOVE SPACE           TO KOM-MID2-CLEARGROUP(W-ANTAL-I-MID)          
312500      MOVE W-RZD-MINSKNING TO KOM-MID2-KVOI      (W-ANTAL-I-MID)          
312600     END-IF                                                               
312700     .                                                                    
312800     EJECT                                                                
312900 S90-SKAPA-RESTORDER    SECTION.                                          
313000                                                                          
313100     MOVE SHUV-IDDISTR            TO RAD-IDDISTR                          
313200     MOVE SHUV-IDKUNDNR           TO RAD-IDKUNDNR                         
313300     MOVE SPACE                   TO RAD-IDKUNDRF                         
313400     MOVE SHUV-IDORDNSB           TO RAD-IDORDNR5                         
313500     MOVE RAD-IDKUNDRF (2:4)      TO RAD-IDKUNDRF (1:4)                   
313600     MOVE SHUV-IDORDNSS           TO RAD-IDKUNDRF (5:1)                   
313700     MOVE SRAD-IDARTNR            TO RAD-IDARTNR                          
313800     MOVE 1                       TO RAD-IDLOPNR                          
313900     MOVE SPACE                   TO RAD-BEKUNDRF                         
314000     MOVE SPACE                   TO RAD-BERADREF                         
314100     MOVE SPACE                   TO RAD-IDDC-RO                          
314200     MOVE NEJ                     TO RAD-FLERS                            
314300     MOVE SHUV-IDKONTO            TO RAD-IDKONTO                          
314400     MOVE SHUV-IDKST              TO RAD-IDKST                            
314500     MOVE SPACE                   TO RAD-IDKUNDRF-LEV                     
314600     MOVE WC-CDC-SE               TO RAD-IDDC                             
314700     MOVE ZERO                    TO RAD-KDDSP                            
314800     MOVE SHUV-KDFAKTYP           TO RAD-KDFAKTYP                         
314900     MOVE ZERO                    TO RAD-KDFRAKT                          
315000     MOVE ZERO                    TO RAD-KDKVBRYT                         
315100     MOVE 1                       TO RAD-KDORDING                         
315200     MOVE SHUV-KDORDKL            TO RAD-KDORDKL                          
315300     MOVE SRAD-KDPRODSL           TO RAD-KDPRODSL                         
315400     MOVE W-KVRO                  TO RAD-KVRO                             
315500     MOVE '2'                     TO RAD-KDSTARAD                         
315600     MOVE ZERO                    TO RAD-KDTPOTYP                         
315700     MOVE ZERO                    TO RAD-KDVRINFO                         
315800     MOVE W-KVART                 TO RAD-KVART                            
315900     MOVE ZERO                    TO RAD-PRARTNTO                         
316000     MOVE SRAD-REKSIFFR           TO RAD-REKSIFFR                         
316100     MOVE ZERO                    TO RAD-TIAVBOKN                         
316200     MOVE SHUV-DAREGDAT (3:6)     TO RAD-TIREGDAT                         
316300     MOVE ZERO                    TO RAD-TIRES                            
316400     MOVE W-DATUM-Y2K             TO RAD-DARODAT                          
316500     MOVE ZERO                    TO RAD-TITPO                            
316600     MOVE SPACE                   TO RAD-KDPRTYP                          
316700     MOVE SPACE                   TO RAD-BEVOLREF                         
316800     MOVE NEJ                     TO RAD-FLINVEST                         
316900     MOVE NEJ                     TO RAD-FLPRTILL                         
317000     MOVE JA                      TO RAD-FLTPOBEK                         
317100     MOVE ZERO                    TO RAD-IDKAMPRF                         
317200     MOVE SPACE                   TO RAD-IDLEVNR                          
317300     MOVE 'SATS'                  TO RAD-IDSYSTEM                         
317400     MOVE SRAD-REBEART            TO RAD-KVBEART-Q                        
317500     MOVE W-TID (1:6)             TO RAD-TIREGTID                         
317600     MOVE 999                     TO RAD-DASENDAT                         
317700                                     RAD-TISENBEK-KL                      
317800     INITIALIZE                      RAD-DEAL-PR-LINE                     
317900     IF ING-IDARTNR-SPARRAD                                               
318000         MOVE W-KDROO             TO RAD-KDROO                            
318100      ELSE                                                                
318200         MOVE W-KDROO-1           TO RAD-KDROO                            
318300     END-IF                                                               
318400                                                                          
318500     MOVE SPACE                   TO RAD-KDORDTYP-LDC                     
318600     MOVE ZERO                    TO RAD-TIREPDAT                         
318700     MOVE SPACE                   TO RAD-IDKUNDRF-WIP                     
318900     MOVE +0                      TO RAD-PRAVCOST                         
318910     MOVE SPACE                   TO RAD-KDROPACK                         
318920     MOVE SPACE                   TO RAD-IDARBREF                         
319000                                                                          
319100     PERFORM S90A-HAMTA-PRIORITETSKOD                                     
319200     PERFORM S90B-HAMTA-ANSKAFFARE                                        
319300                                                                          
319400     PERFORM IMS-ISRT-ORDP01                                              
319500                                                                          
319600     PERFORM UNTIL SEGMENT-FINNS                                          
319700        ADD 1                  TO RAD-IDLOPNR                             
319800        PERFORM IMS-ISRT-ORDP01                                           
319900     END-PERFORM                                                          
320000     .                                                                    
320100     EJECT                                                                
320200 S90A-HAMTA-PRIORITETSKOD SECTION.                                        
320300                                                                          
320400     MOVE RAD-KDTPOTYP         TO W-4512-KDTPOTYP                         
320500     MOVE SHUV-KDORDKL         TO W-4512-KDORDKL                          
320600     MOVE SHUV-IDDISTR         TO W-4512-IDDISTR-FOM                      
320700                                  W-4512-IDDISTR-TOM                      
320800                                                                          
320900     PERFORM IMS-GU-XXJN11                                                
321000     MOVE 4512-KDRAPRIO        TO RAD-KDRAPRIO                            
321100     .                                                                    
321200     EJECT                                                                
321300 S90B-HAMTA-ANSKAFFARE SECTION.                                           
321400                                                                          
321500     PERFORM IMS-GU-ARTC2-ARTC11                                          
321600     MOVE C2-CLAG-IDANSK       TO RAD-IDANSK                              
321700     .                                                                    
321800     EJECT                                                                
321900 S91-SKAPA-WDA5-NYCKEL               SECTION.                             
322000                                                                          
322100     MOVE LOW-VALUE            TO W-WDA501KY-MIN-X                        
322200     MOVE HIGH-VALUE           TO W-WDA501KY-MAX-X                        
322300                                                                          
322400     MOVE SHUV-IDDISTR         TO W-A501KY-MIN-IDDISTR                    
322500                                  W-A501KY-MAX-IDDISTR                    
322600                                                                          
322700     MOVE SHUV-IDKUNDNR        TO W-A501KY-MIN-IDKUNDNR                   
322800                                  W-A501KY-MAX-IDKUNDNR                   
322900                                                                          
323000     MOVE SPACE                TO W-A501KY-MIN-IDKUNDRF                   
323100                                  W-A501KY-MAX-IDKUNDRF                   
323200                                                                          
323300     MOVE W-HELP-IDORDNST      TO W-A501KY-MIN-IDORDNR5                   
323400                                  W-A501KY-MAX-IDORDNR5                   
323500                                                                          
323600     MOVE SRAD-IDARTNR         TO W-A501KY-MIN-IDARTNR                    
323700                                  W-A501KY-MAX-IDARTNR                    
323800                                                                          
323900     .                                                                    
324000     EJECT                                                                
324100 S92-TA-FRAM-NASTA-KDSATKMB          SECTION.                             
324200                                                                          
324300****************************************************************          
324400* ORDNA SÅ ATT NÄSTA SATS-KOMBINATIONSKOD FINNS TILLGÄNGLIG    *          
324500* KDSATKMB = BOKSTÄVER I ALFABETS-ORDNING                      *          
324600****************************************************************          
324700                                                                          
324800     MOVE NEJ                TO KOMB-TRAFF-SW                             
324900     SET KOMB-IDX1           TO +1                                        
325000                                                                          
325100     PERFORM UNTIL KOMB-IDX1 > 26 OR                                      
325200                   KOMB-TRAFF                                             
325300                                                                          
325400        IF KOMB-BOKSTAV(KOMB-IDX1) = SHUV-KDSATKMB                        
325500           SET KOMB-IDX1 UP   BY +1                                       
325600           MOVE KOMB-BOKSTAV(KOMB-IDX1)                                   
325700                             TO W-SHUV-KDSATKMB                           
325800           MOVE JA           TO KOMB-TRAFF-SW                             
325900        ELSE                                                              
326000           SET KOMB-IDX1 UP  BY +1                                        
326100        END-IF                                                            
326200                                                                          
326300     END-PERFORM                                                          
326400     .                                                                    
326500     EJECT                                                                
326600 MFS-RENSA-FAELT-UT SECTION.                                              
326700                                                                          
326800*    --- ALLA UTDATA-FÄLT                                                 
326900*    --- INKL. BLÄDDRINGSNYCKLAR                                          
327000     MOVE MFS-RENSA-FAELT      TO  MOD-IDARTNR                            
327100                                   MOD-KDCLAGER                           
327200                                   MOD-IDPRC                              
327300                                   MOD-FLSATPRI                           
327400                                   MOD-FLBYGGB                            
327500                                   MOD-KDSATKMB                           
327600                                   MOD-IDPRODNR                           
327700                                   MOD-BEART                              
327800                                   MOD-KVBEART                            
327900                                   MOD-KVBYGGB                            
328000                                   MOD-IDARTNR-ENTER                      
328100                                   MOD-IDARTNR-NEXT                       
328200                                                                          
328300                                                                          
328400     MOVE +1                   TO  INDX                                   
328500     PERFORM UNTIL             INDX > MAX-RAD-2303                        
328600         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
328700         ADD +1                TO  INDX                                   
328800     END-PERFORM                                                          
328900                                                                          
329000     .                                                                    
329100     SKIP2                                                                
329200 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
329300                                                                          
329400*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
329500     MOVE MFS-RENSA-FAELT      TO MOD-ING-IDARTNR-RAD (INDX)              
329600                                  MOD-REANTPSA-RAD    (INDX)              
329700                                  MOD-REBEART-RAD     (INDX)              
329800                                  MOD-KVSATRES-RAD    (INDX)              
329900                                  MOD-KVSATROS-RAD    (INDX)              
330000                                  MOD-KDSATAND-RAD    (INDX)              
330100                                  MOD-FLSATRAS-RAD    (INDX)              
330200                                  MOD-KDSATKMB-RAD    (INDX)              
330300                                  MOD-FLSATSPR-RAD    (INDX)              
330400                                  MOD-TIDISPIN-RAD    (INDX)              
330500     .                                                                    
330600     SKIP2                                                                
330700 MFS-RENSA-FAELT-IN SECTION.                                              
330800                                                                          
330900     MOVE MFS-RENSA-FAELT      TO MOD-ING-IDARTNR1-UPDATE                 
331000                                  MOD-REANTPSA1-UPDATE                    
331100                                  MOD-REBEART1-UPDATE                     
331200                                  MOD-KDSATAND1-UPDATE                    
331300                                  MOD-KDSATKMB-UPDATE                     
331400                                  MOD-ING-IDARTNR2-UPDATE                 
331500                                  MOD-REANTPSA2-UPDATE                    
331600                                  MOD-REBEART2-UPDATE                     
331700                                  MOD-KDSATAND2-UPDATE                    
331800     .                                                                    
331900     EJECT                                                                
332000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
332100                                                                          
332200     MOVE MFS-ROER-EJ-FAELT    TO  MOD-IDARTNR                            
332300                                   MOD-KDCLAGER                           
332400                                   MOD-IDPRC                              
332500                                   MOD-FLSATPRI                           
332600                                   MOD-FLBYGGB                            
332700                                   MOD-KDSATKMB                           
332800                                   MOD-IDPRODNR                           
332900                                   MOD-BEART                              
333000                                   MOD-KVBEART                            
333100                                   MOD-KVBYGGB                            
333200                                   MOD-IDARTNR-ENTER                      
333300                                   MOD-IDARTNR-NEXT                       
333400                                                                          
333500                                                                          
333600     MOVE +1                   TO INDX                                    
333700     PERFORM UNTIL INDX        > MAX-RAD-2303                             
333800       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
333900       ADD +1                  TO INDX                                    
334000     END-PERFORM                                                          
334100     .                                                                    
334200     SKIP2                                                                
334300 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
334400                                                                          
334500*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
334600     MOVE MFS-ROER-EJ-FAELT    TO MOD-ING-IDARTNR-RAD (INDX)              
334700                                  MOD-REANTPSA-RAD    (INDX)              
334800                                  MOD-REBEART-RAD     (INDX)              
334900                                  MOD-KVSATRES-RAD    (INDX)              
335000                                  MOD-KVSATROS-RAD    (INDX)              
335100                                  MOD-KDSATAND-RAD    (INDX)              
335200                                  MOD-FLSATRAS-RAD    (INDX)              
335300                                  MOD-KDSATKMB-RAD    (INDX)              
335400                                  MOD-FLSATSPR-RAD    (INDX)              
335500                                  MOD-TIDISPIN-RAD    (INDX)              
335600     .                                                                    
335700     SKIP2                                                                
335800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
335900                                                                          
336000*    --- ALLA INDATA-FÄLT                                                 
336100     MOVE MFS-ROER-EJ-FAELT    TO MOD-ING-IDARTNR1-UPDATE                 
336200                                  MOD-REANTPSA1-UPDATE                    
336300                                  MOD-REBEART1-UPDATE                     
336400                                  MOD-KDSATAND1-UPDATE                    
336500                                  MOD-KDSATKMB-UPDATE                     
336600                                  MOD-ING-IDARTNR2-UPDATE                 
336700                                  MOD-REANTPSA2-UPDATE                    
336800                                  MOD-REBEART2-UPDATE                     
336900                                  MOD-KDSATAND2-UPDATE                    
337000     .                                                                    
337100     EJECT                                                                
337200 MFS-FORM-ATTR SECTION.                                                   
337300                                                                          
337400*    --- ALLA INDATA-FÄLT                                                 
337500     MOVE MFS-FORMATETS-ATTR   TO MOD-ING-IDARTNR1-UPDATE-ATTR            
337600                                  MOD-REANTPSA1-UPDATE-ATTR               
337700                                  MOD-REBEART1-UPDATE-ATTR                
337800                                  MOD-KDSATAND1-UPDATE-ATTR               
337900                                  MOD-KDSATKMB-UPDATE-ATTR                
338000                                  MOD-ING-IDARTNR2-UPDATE-ATTR            
338100                                  MOD-REANTPSA2-UPDATE-ATTR               
338200                                  MOD-REBEART2-UPDATE-ATTR                
338300                                  MOD-KDSATAND2-UPDATE-ATTR               
338400     .                                                                    
338500     SKIP2                                                                
338600 MFS-LAES-IN-IGEN SECTION.                                                
338700                                                                          
338800     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ING-IDARTNR1-UPDATE-ATTR           
338900                                   MOD-REANTPSA1-UPDATE-ATTR              
339000                                   MOD-REBEART1-UPDATE-ATTR               
339100                                   MOD-KDSATAND1-UPDATE-ATTR              
339200                                   MOD-KDSATKMB-UPDATE-ATTR               
339300                                   MOD-ING-IDARTNR2-UPDATE-ATTR           
339400                                   MOD-REANTPSA2-UPDATE-ATTR              
339500                                   MOD-REBEART2-UPDATE-ATTR               
339600                                   MOD-KDSATAND2-UPDATE-ATTR              
339700     .                                                                    
339800     EJECT                                                                
339900* --- IMS SEKTIONER ---                                                   
340000     SKIP3                                                                
340100 IMS-GET-MSG SECTION.                                                     
340200                                                                          
340300     MOVE '  QC' TO GODK-STATUSKODER                                      
340400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
340500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
340600     PERFORM IMS-STATUSKONTROLL                                           
340700     .                                                                    
340800     SKIP3                                                                
340900 IMS-INSERT-MSG SECTION.                                                  
341000                                                                          
341100     IF ENGLISH-TEXT                                                      
341200       MOVE 'N' TO MFS-KDHUVOMR                                           
341300     END-IF                                                               
341400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
341500     MOVE SPACE TO GODK-STATUSKODER                                       
341600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
341700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
341800     PERFORM IMS-STATUSKONTROLL                                           
341900     .                                                                    
342000     EJECT                                                                
342100 IMS-GU-SATG1-SATG01 SECTION.                                             
342200     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
342300          DELIMITED BY SIZE INTO SSA1                                     
342400     MOVE '  GE' TO GODK-STATUSKODER                                      
342500     CALL CBLTDLI USING GU SATG1-PCB DLI-IO-AREA1 SSA1                    
342600     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
342700     PERFORM IMS-STATUSKONTROLL                                           
342800     .                                                                    
342900     SKIP3                                                                
343000 IMS-GHU-SATG1-SATG01 SECTION.                                            
343100     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
343200          DELIMITED BY SIZE INTO SSA1                                     
343300     MOVE '  GE' TO GODK-STATUSKODER                                      
343400     CALL CBLTDLI USING GHU SATG1-PCB DLI-IO-AREA1 SSA1                   
343500     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
343600     PERFORM IMS-STATUSKONTROLL                                           
343700     .                                                                    
343800     SKIP3                                                                
343900 IMS-REPL-SATG1-SATG01 SECTION.                                           
344000     MOVE '    ' TO GODK-STATUSKODER                                      
344100     CALL CBLTDLI USING REPL SATG1-PCB DLI-IO-AREA1                       
344200     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
344300     PERFORM IMS-STATUSKONTROLL                                           
344400     .                                                                    
344500     EJECT                                                                
344600 IMS-GHU-SATG1-SATG11      SECTION.                                       
344700     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
344800          DELIMITED BY SIZE INTO SSA1                                     
344900     STRING 'WLSATG11(IDARTNR  =' W-ING-IDARTNR-X ')'                     
345000          DELIMITED BY SIZE INTO SSA2                                     
345100     MOVE '  GE' TO GODK-STATUSKODER                                      
345200     CALL CBLTDLI USING GHU SATG1-PCB DLI-IO-AREA2 SSA1 SSA2              
345300     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
345400     PERFORM IMS-STATUSKONTROLL                                           
345500     .                                                                    
345600     SKIP3                                                                
345700 IMS-GU-SATG1-SATG11-KVAL SECTION.                                        
345800     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
345900          DELIMITED BY SIZE INTO SSA1                                     
346000     STRING 'WLSATG11(IDARTNR  =' W-ING-IDARTNR-X                         
346100                    '&KDSATKMB>=' W-KDSATKMB-MIN-X                        
346200                    '&KDSATKMB<=' W-KDSATKMB-MAX-X ')'                    
346300          DELIMITED BY SIZE INTO SSA2                                     
346400     MOVE '  GE' TO GODK-STATUSKODER                                      
346500     CALL CBLTDLI USING GU SATG1-PCB DLI-IO-AREA2 SSA1 SSA2               
346600     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
346700     PERFORM IMS-STATUSKONTROLL                                           
346800     .                                                                    
346900     SKIP3                                                                
347000 IMS-GU-SATG1-SATG11-OKVAL SECTION.                                       
347100     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
347200          DELIMITED BY SIZE INTO SSA1                                     
347300     STRING 'WLSATG11(IDARTNR >=' W-ING-IDARTNR-X                         
347400                    '&KDSATKMB>=' W-KDSATKMB-MIN-X                        
347500                    '&KDSATKMB<=' W-KDSATKMB-MAX-X ')'                    
347600          DELIMITED BY SIZE INTO SSA2                                     
347700     MOVE '  GE' TO GODK-STATUSKODER                                      
347800     CALL CBLTDLI USING GU SATG1-PCB DLI-IO-AREA2 SSA1 SSA2               
347900     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
348000     PERFORM IMS-STATUSKONTROLL                                           
348100     .                                                                    
348200     SKIP3                                                                
348300 IMS-GN-SATG1-SATG11 SECTION.                                             
348400     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
348500          DELIMITED BY SIZE INTO SSA1                                     
348600     STRING 'WLSATG11(KDSATKMB>=' W-KDSATKMB-MIN-X                        
348700                    '&KDSATKMB<=' W-KDSATKMB-MAX-X ')'                    
348800          DELIMITED BY SIZE INTO SSA2                                     
348900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
349000     CALL CBLTDLI USING GN SATG1-PCB DLI-IO-AREA2 SSA1 SSA2               
349100     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
349200     PERFORM IMS-STATUSKONTROLL                                           
349300     .                                                                    
349400     SKIP3                                                                
349500 IMS-GU-SATG1-SATG11 SECTION.                                             
349600     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
349700          DELIMITED BY SIZE INTO SSA1                                     
349800     MOVE   'WLSATG11'   TO SSA2                                          
349900     MOVE '  GE' TO GODK-STATUSKODER                                      
350000     CALL CBLTDLI USING GU SATG1-PCB DLI-IO-AREA2 SSA1 SSA2               
350100     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
350200     PERFORM IMS-STATUSKONTROLL                                           
350300     .                                                                    
350400     SKIP3                                                                
350500 IMS-REPL-SATG1-SATG11 SECTION.                                           
350600     MOVE '  GE' TO GODK-STATUSKODER                                      
350700     CALL CBLTDLI USING REPL SATG1-PCB DLI-IO-AREA2                       
350800     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
350900     PERFORM IMS-STATUSKONTROLL                                           
351000     .                                                                    
351100     SKIP3                                                                
351200 IMS-ISRT-SATG1-SATG11 SECTION.                                           
351300                                                                          
351400     MOVE 'WLSATG11 ' TO SSA1                                             
351500     MOVE '  II' TO GODK-STATUSKODER                                      
351600     CALL CBLTDLI USING ISRT SATG1-PCB DLI-IO-AREA2 SSA1                  
351700     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
351800     PERFORM IMS-STATUSKONTROLL                                           
351900     .                                                                    
352000     EJECT                                                                
352100 IMS-GU-SATG2-SATG11       SECTION.                                       
352200     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
352300          DELIMITED BY SIZE INTO SSA1                                     
352400     STRING 'WLSATG11(IDARTNR > ' W-ING-IDARTNR-X                         
352500                    '&KDSATKMB>=' W-SATG2-KDSATKMB-MIN-X                  
352600                    '&KDSATKMB<=' W-SATG2-KDSATKMB-MAX-X ')'              
352700          DELIMITED BY SIZE INTO SSA2                                     
352800     MOVE '  GE' TO GODK-STATUSKODER                                      
352900     CALL CBLTDLI USING GU SATG2-PCB DLI-IO-AREA2 SSA1 SSA2               
353000     MOVE SATG2-STATUS-CODE TO STATUS-WS                                  
353100     PERFORM IMS-STATUSKONTROLL                                           
353200     .                                                                    
353300     SKIP3                                                                
353400 IMS-GN-SATG2-SATG11       SECTION.                                       
353500     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
353600          DELIMITED BY SIZE INTO SSA1                                     
353700     STRING 'WLSATG11(IDARTNR > ' W-ING-IDARTNR-X                         
353800                    '&KDSATKMB>=' W-SATG2-KDSATKMB-MIN-X                  
353900                    '&KDSATKMB<=' W-SATG2-KDSATKMB-MAX-X ')'              
354000          DELIMITED BY SIZE INTO SSA2                                     
354100     MOVE '  GE' TO GODK-STATUSKODER                                      
354200     CALL CBLTDLI USING GN SATG2-PCB DLI-IO-AREA2 SSA1 SSA2               
354300     MOVE SATG2-STATUS-CODE TO STATUS-WS                                  
354400     PERFORM IMS-STATUSKONTROLL                                           
354500     .                                                                    
354600     SKIP3                                                                
354700 IMS-GHU-ARTC1-ARTC11 SECTION.                                            
354800     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
354900          DELIMITED BY SIZE INTO SSA1                                     
355000     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
355100          DELIMITED BY SIZE INTO SSA2                                     
355200     MOVE '    ' TO GODK-STATUSKODER                                      
355300     CALL CBLTDLI USING GHU ARTC1-PCB DLI-IO-AREA11 SSA1 SSA2             
355400     MOVE ARTC1-STATUS-CODE TO STATUS-WS                                  
355500     PERFORM IMS-STATUSKONTROLL                                           
355600     .                                                                    
355700     SKIP3                                                                
355800 IMS-REPL-ARTC1-ARTC11 SECTION.                                           
355900                                                                          
356000     MOVE '  ' TO GODK-STATUSKODER                                        
356100     CALL CBLTDLI USING REPL ARTC1-PCB DLI-IO-AREA11                      
356200     MOVE ARTC1-STATUS-CODE TO STATUS-WS                                  
356300     PERFORM IMS-STATUSKONTROLL                                           
356400     .                                                                    
356500     EJECT                                                                
356600 IMS-GU-ARTC2-ARTC11 SECTION.                                             
356700     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
356800          DELIMITED BY SIZE INTO SSA1                                     
356900     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
357000          DELIMITED BY SIZE INTO SSA2                                     
357100     MOVE '    ' TO GODK-STATUSKODER                                      
357200     CALL CBLTDLI USING GU ARTC2-PCB DLI-IO-AREA12 SSA1 SSA2              
357300     MOVE ARTC2-STATUS-CODE TO STATUS-WS                                  
357400     PERFORM IMS-STATUSKONTROLL                                           
357500     .                                                                    
357600     SKIP3                                                                
357700 IMS-GU-ARTC1-ARTC11 SECTION.                                             
357800     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
357900          DELIMITED BY SIZE INTO SSA1                                     
358000     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
358100          DELIMITED BY SIZE INTO SSA2                                     
358200     MOVE '  GE' TO GODK-STATUSKODER                                      
358300     CALL CBLTDLI USING GU ARTC1-PCB DLI-IO-AREA11 SSA1 SSA2              
358400     MOVE ARTC1-STATUS-CODE TO STATUS-WS                                  
358500     PERFORM IMS-STATUSKONTROLL                                           
358600     .                                                                    
358700     EJECT                                                                
358800 IMS-GU-ARTC1-ARTC11-OKVAL SECTION.                                       
358900     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
359000          DELIMITED BY SIZE INTO SSA1                                     
359100     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
359200          DELIMITED BY SIZE INTO SSA2                                     
359300     MOVE '    ' TO GODK-STATUSKODER                                      
359400     CALL CBLTDLI USING GU ARTC1-PCB DLI-IO-AREA11 SSA1 SSA2              
359500     MOVE ARTC1-STATUS-CODE TO STATUS-WS                                  
359600     PERFORM IMS-STATUSKONTROLL                                           
359700     .                                                                    
359800     EJECT                                                                
359900 IMS-GHU-ORDP01-KVAL SECTION.                                             
360000     STRING 'WLORDP01(WDA501KY =' W-WDA501KY-X ')'                        
360100          DELIMITED BY SIZE INTO SSA1                                     
360200     MOVE '    ' TO GODK-STATUSKODER                                      
360300     CALL CBLTDLI USING GHU ORDP-PCB DLI-IO-AREA4 SSA1                    
360400     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
360500     PERFORM IMS-STATUSKONTROLL                                           
360600     .                                                                    
360700     SKIP3                                                                
360800 IMS-GHU-ORDP01-OKVAL SECTION.                                            
360900     STRING 'WLORDP01(WDA501KY>=' W-WDA501KY-MIN-X                        
361000                    '&WDA501KY<=' W-WDA501KY-MAX-X ')'                    
361100          DELIMITED BY SIZE INTO SSA1                                     
361200     MOVE '  GE' TO GODK-STATUSKODER                                      
361300     CALL CBLTDLI USING GHU ORDP-PCB DLI-IO-AREA4 SSA1                    
361400     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
361500     PERFORM IMS-STATUSKONTROLL                                           
361600     .                                                                    
361700     SKIP3                                                                
361800 IMS-GHN-ORDP01 SECTION.                                                  
361900     STRING 'WLORDP01(WDA501KY>=' W-WDA501KY-MIN-X                        
362000                    '&WDA501KY<=' W-WDA501KY-MAX-X ')'                    
362100          DELIMITED BY SIZE INTO SSA1                                     
362200     MOVE '  GE' TO GODK-STATUSKODER                                      
362300     CALL CBLTDLI USING GHN ORDP-PCB DLI-IO-AREA4 SSA1                    
362400     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
362500     PERFORM IMS-STATUSKONTROLL                                           
362600     .                                                                    
362700     SKIP3                                                                
362800 IMS-ISRT-ORDP01 SECTION.                                                 
362900                                                                          
363000     MOVE 'WLORDP01 ' TO SSA1                                             
363100     MOVE '  II' TO GODK-STATUSKODER                                      
363200     CALL CBLTDLI USING ISRT ORDP-PCB DLI-IO-AREA4 SSA1                   
363300     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
363400     PERFORM IMS-STATUSKONTROLL                                           
363500     .                                                                    
363600     SKIP3                                                                
363700 IMS-REPL-ORDP01 SECTION.                                                 
363800                                                                          
363900     MOVE '  ' TO GODK-STATUSKODER                                        
364000     CALL CBLTDLI USING REPL ORDP-PCB DLI-IO-AREA4                        
364100     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
364200     PERFORM IMS-STATUSKONTROLL                                           
364300     .                                                                    
364400     EJECT                                                                
364500 IMS-DLET-ORDP01 SECTION.                                                 
364600                                                                          
364700     MOVE '  ' TO GODK-STATUSKODER                                        
364800     CALL CBLTDLI USING DLET ORDP-PCB DLI-IO-AREA4                        
364900     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
365000     PERFORM IMS-STATUSKONTROLL                                           
365100     .                                                                    
365200     EJECT                                                                
365300 IMS-GHU-ORDR01 SECTION.                                                  
365400     STRING 'WLORDR01(WDA5B1KY>=' W-WDA5B1KY-MIN-X                        
365500                    '&WDA5B1KY<=' W-WDA5B1KY-MAX-X ')'                    
365600          DELIMITED BY SIZE INTO SSA1                                     
365700     MOVE '  GE' TO GODK-STATUSKODER                                      
365800     CALL CBLTDLI USING GHU ORDR-PCB DLI-IO-AREA5 SSA1                    
365900     MOVE ORDR-STATUS-CODE TO STATUS-WS                                   
366000     PERFORM IMS-STATUSKONTROLL                                           
366100     .                                                                    
366200     SKIP3                                                                
366300 IMS-GHN-ORDR01 SECTION.                                                  
366400     STRING 'WLORDR01(WDA5B1KY>=' W-WDA5B1KY-MIN-X                        
366500                    '&WDA5B1KY<=' W-WDA5B1KY-MAX-X ')'                    
366600          DELIMITED BY SIZE INTO SSA1                                     
366700     MOVE '  GE' TO GODK-STATUSKODER                                      
366800     CALL CBLTDLI USING GHN ORDR-PCB DLI-IO-AREA5 SSA1                    
366900     MOVE ORDR-STATUS-CODE TO STATUS-WS                                   
367000     PERFORM IMS-STATUSKONTROLL                                           
367100     .                                                                    
367200     SKIP3                                                                
367300 IMS-GU-BENA11 SECTION.                                                   
367400     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
367500          DELIMITED BY SIZE INTO SSA1                                     
367600     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
367700          DELIMITED BY SIZE INTO SSA2                                     
367800     MOVE '  GE' TO GODK-STATUSKODER                                      
367900     CALL CBLTDLI USING GU  BENA-PCB DLI-IO-AREA3 SSA1 SSA2               
368000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
368100     PERFORM IMS-STATUSKONTROLL                                           
368200     .                                                                    
368300     EJECT                                                                
368400 IMS-GU-ARTC3-ARTC01 SECTION.                                             
368500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
368600          DELIMITED BY SIZE INTO SSA1                                     
368700     MOVE '  GE' TO GODK-STATUSKODER                                      
368800     CALL CBLTDLI USING GU  ARTC3-PCB DLI-IO-AREA10 SSA1                  
368900     MOVE ARTC3-STATUS-CODE TO STATUS-WS                                  
369000     PERFORM IMS-STATUSKONTROLL                                           
369100     .                                                                    
369200     SKIP3                                                                
369300 IMS-GNP-ARTC3-ARTC11 SECTION.                                            
369400     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
369500          DELIMITED BY SIZE INTO SSA1                                     
369600     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
369700          DELIMITED BY SIZE INTO SSA2                                     
369800     MOVE '  GE' TO GODK-STATUSKODER                                      
369900     CALL CBLTDLI USING GU ARTC3-PCB DLI-IO-AREA10 SSA1 SSA2              
370000     MOVE ARTC3-STATUS-CODE TO STATUS-WS                                  
370100     PERFORM IMS-STATUSKONTROLL                                           
370200     .                                                                    
370300     SKIP3                                                                
370400 IMS-GU-XXJN11 SECTION.                                                   
370500                                                                          
370600     STRING 'WLXXJN01(WDGXKEY  =' W-4511-IDHTYP-X  ')'                    
370700          DELIMITED BY SIZE INTO SSA1                                     
370800     STRING 'WLXXJN11(KDTPOTYP =' W-4512-KDTPOTYP-X                       
370900                    '&KDORDKL  =' W-4512-KDORDKL-X                        
371000                    '&IDDISTRF<=' W-4512-IDDISTR-FOM-X                    
371100                    '&IDDISTRT>=' W-4512-IDDISTR-TOM-X ')'                
371200          DELIMITED BY SIZE INTO SSA2                                     
371300     MOVE '    ' TO GODK-STATUSKODER                                      
371400     CALL CBLTDLI USING GU XXJN-PCB DLI-IO-AREA6 SSA1 SSA2                
371500     MOVE XXJN-STATUS-CODE TO STATUS-WS                                   
371600     PERFORM IMS-STATUSKONTROLL                                           
371700     .                                                                    
371800     SKIP3                                                                
371900 IMS-GU-ARTM01 SECTION.                                                   
372000     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
372100          DELIMITED BY SIZE INTO SSA1                                     
372200     MOVE '  GE' TO GODK-STATUSKODER                                      
372300     CALL CBLTDLI USING GU  ARTM-PCB DLI-IO-AREA7 SSA1                    
372400     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
372500     PERFORM IMS-STATUSKONTROLL                                           
372600     .                                                                    
372700     EJECT                                                                
372800 IMS-STATUSKONTROLL SECTION.                                              
372900                                                                          
373000     SET STATUS-IX TO 1                                                   
373100     SEARCH GODK-STATUS                                                   
373200       AT END                                                             
373300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
373400         DELIMITED BY SIZE INTO FELTEXT                                   
373500         CALL FELLOG                                                      
373600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
373700     END-SEARCH                                                           
373800     .                                                                    
