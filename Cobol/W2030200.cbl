000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2030200.                                                
000400 AUTHOR.         LARS THELL      (MG).                                    
000500 DATE-WRITTEN.   90/12/18.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SATSORDER ÅTGÄRD                                                 
001100*                                                                         
001200*        PROGRAMMET UPPATERAR WLSATG (WDJ2)                               
001300*        PROGRAMMET UPPATERAR WLARTC (WDK6)                               
001400*        PROGRAMMET UPPATERAR WLORDP (WDA5)                               
001500*        PROGRAMMET LÄSER     WLBENA (WDD3)                               
001600*        PROGRAMMET LÄSER     WLARTA (WDD1)                               
001700*        PROGRAMMET LÄSER     WLARTM (WDK9)                               
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W2T302                                              
002100*        MID:         W2I30101                                            
002200*        MID:         W2I30201                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W2O30201                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W2030200'.            
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
004600                                                                          
004700*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004900 77  MAX-RAD-2301                PIC S9(4)  VALUE +14   COMP SYNC.        
005000 77  MAX-RAD-2302                PIC S9(4)  VALUE +7    COMP SYNC.        
005100 77  MAX-RAD-MID2                PIC S9(4)  VALUE +18   COMP SYNC.        
005200 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005300 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +656  COMP SYNC.        
005400 77  MAX-CLAGER                  PIC S9(4)  VALUE +2    COMP SYNC.        
005500                                                                          
005600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005700 01  WS-IDORDNST.                                                         
005800   05  WS-IDORDNSB               PIC 9(4)    VALUE ZERO.                  
005900   05  WS-IDORDNSS               PIC 9(1)    VALUE ZERO.                  
006000 01    WS-IDANSK-FOM             PIC X(3)    VALUE SPACE.                 
006100 01    WS-IDANSK-TOM             PIC X(3)    VALUE SPACE.                 
006200 01    WS-FLBYGGB                PIC X(1)    VALUE SPACE.                 
006300 01    WS-IDARTNR                PIC X(9)    VALUE SPACE.                 
006400 01    WS-ING-IDARTNR            PIC X(9)    VALUE SPACE.                 
006500 01    WS-KDSATKMB               PIC X(1)    VALUE SPACE.                 
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
008100 77  HOPP-FRAN-2301-SW           PIC X       VALUE 'N'.                   
008200     88  HOPP-FRAN-2301                      VALUE 'J'.                   
008300                                                                          
008400 77  EJ-TACKT-RAD-SW             PIC X       VALUE 'N'.                   
008500     88  EJ-TACKT-RAD                        VALUE 'J'.                   
008600                                                                          
008700 77  ARTM01-FINNS-SW             PIC X       VALUE 'N'.                   
008800     88  ARTM01-FINNS                        VALUE 'J'.                   
008900                                                                          
009000 77  IDORDER-BORTTAGEN-SW        PIC X       VALUE 'N'.                   
009100     88 IDORDER-BORTTAGEN                    VALUE 'J'.                   
009200                                                                          
009300 77  KVBYGGB-TRAFF-SW            PIC X       VALUE 'N'.                   
009400     88 KVBYGGB-TRAFF                        VALUE 'J'.                   
009500                                                                          
009600 77  KOMB-TRAFF-SW               PIC X       VALUE 'N'.                   
009700     88 KOMB-TRAFF                           VALUE 'J'.                   
009800                                                                          
009900 77  FELAKTIG-KDSATAND-SW        PIC X       VALUE 'N'.                   
010000     88 FELAKTIG-KDSATAND                    VALUE 'J'.                   
010100                                                                          
010200 77  SW-PPSW-2109                PIC X       VALUE 'N'.                   
010300                                                                          
010400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010500     88  EGEN-MID                            VALUE '2302'.                
010600     88  GODK-MID                            VALUE '2301' '2302'          
010700                                                   '2303'.                
010800 01  W-IDARTNR-11.                                                        
010900   05  W-IDARTNR1-9              PIC 9(9)    VALUE ZERO.                  
011000   05  FILLER                    PIC X(1)    VALUE '-'.                   
011100   05  W-REKSIFFR                PIC 9(1)    VALUE ZERO.                  
011200                                                                          
011300 77  W-OKS                       PIC S9(7)   VALUE ZERO  COMP-3.          
011400 77  W-OLD-KVSATRES              PIC S9(7)   VALUE ZERO  COMP-3.          
011500 77  W-MID-KVDELA-UPDATE         PIC  9(6)   VALUE ZERO.                  
011600 77  W1-KVRADER                  PIC S9(3)   VALUE ZERO  COMP-3.          
011700 77  W2-KVRADER                  PIC S9(3)   VALUE ZERO  COMP-3.          
011800 77  W1-VLARTNTO                 PIC S9(8)V9(1) VALUE ZERO COMP-3.        
011900 77  W1-VKARTNTO                 PIC S9(4)V9(3) VALUE ZERO COMP-3.        
012000 77  W2-VLARTNTO                 PIC S9(8)V9(1) VALUE ZERO COMP-3.        
012100 77  W2-VKARTNTO                 PIC S9(4)V9(3) VALUE ZERO COMP-3.        
012200 77  W-KVBYGGB                   PIC S9(7)      VALUE ZERO COMP-3.        
012300 77  W-MAX-KVBYGGB               PIC S9(7)      VALUE ZERO COMP-3.        
012400 77  W-KVBEART                   PIC S9(7)      VALUE ZERO COMP-3.        
012500 77  W-REBEART                   PIC S9(7)      VALUE ZERO COMP-3.        
012600 77  W-RZD-MINSKNING             PIC S9(7)      VALUE ZERO COMP-3.        
012700 77  W-RZD-OKNING                PIC S9(7)      VALUE ZERO COMP-3.        
012800 77  W-KVSATROS                  PIC S9(7)      VALUE ZERO COMP-3.        
012900 77  W-KVSATRES                  PIC S9(7)      VALUE ZERO COMP-3.        
013000 77  W-TOT-REBEART               PIC S9(7)      VALUE ZERO COMP-3.        
013100 77  W-HELP-REBEART              PIC S9(7)V9(1) VALUE ZERO COMP-3.        
013200 77  W-KVANNANT                  PIC S9(7)      VALUE ZERO COMP-3.        
013300 77  W-KDROO-1                   PIC  9(1)      VALUE 1.                  
013400 77  W-KDROO                     PIC  9(1)      VALUE ZERO.               
013500 77  W-KVRO                      PIC S9(7)      VALUE ZERO COMP-3.        
013600 77  W-KVART                     PIC S9(7)      VALUE ZERO COMP-3.        
013700 77  W-KVPREAVB                  PIC S9(9)      VALUE ZERO COMP-3.        
013800 77  W-TILLG-KVLS                PIC S9(9)      VALUE ZERO COMP-3.        
013900 77  W-DATUM                     PIC  X(6)      VALUE SPACE.              
014000 77  W-DATUM-Y2K                 PIC  9(8)      VALUE ZERO.               
014100 77  W-TID                       PIC  X(8)      VALUE SPACE.              
014200 77  W-ANTAL-I-MID               PIC S9(3)      VALUE ZERO COMP-3.        
014300                                                                          
014400 01  W-BYGGB-IDORDNST.                                                    
014500   05  W-BYGGB-IDORDNSB          PIC S9(4)   VALUE ZERO COMP-3.           
014600   05  W-BYGGB-IDORDNSS          PIC S9(1)   VALUE ZERO COMP-3.           
014700                                                                          
014800 01  W-NY-IDORDNST.                                                       
014900   05  W-NY-IDORDNSB             PIC S9(4)   VALUE ZERO COMP-3.           
015000   05  W-NY-IDORDNSS             PIC S9(1)   VALUE ZERO COMP-3.           
015100     EJECT                                                                
015200 01  FILLER                   PIC X(16) VALUE 'KOMB-TAB'.                 
015300 01  KOMB-TAB.                                                            
015400     03 KOMB-RAD  OCCURS 26 INDEXED BY IDX1.                              
015500       05 KOMB-KDSATKMB       PIC X.                                      
015600       05 KOMB-KVBYGGB        PIC S9(7)      COMP-3.                      
015700       05 KOMB-REST-KVBYGGB   PIC S9(7)      COMP-3.                      
015800       05 FILLER  OCCURS 5  INDEXED BY IDX2.                              
015900         07 KOMB-ING-IDARTNR  PIC S9(9)  COMP-3.                          
016000         07 KOMB-REANTPSA     PIC S9(2)V9(3) COMP-3.                      
016100         07 KOMB-REBEART      PIC S9(7)   COMP-3.                         
016200         07 KOMB-NY-REBEART   PIC S9(7)   COMP-3.                         
016300         07 KOMB-KDSATAND     PIC X(1).                                   
016400                                                                          
016500                                                                          
016600 01  FILLER                   PIC X(16) VALUE 'KVBYGGB-TAB'.              
016700 01  KVBYGGB-TAB.                                                         
016800     03 KVBYGGB-RAD  OCCURS 26 INDEXED BY IDX3.                           
016900       05 KVBYGGB-KDSATKMB    PIC X.                                      
017000       05 KVBYGGB-KVBYGGB     PIC S9(7)      COMP-3.                      
017100                                                                          
017200     EJECT                                                                
017300*   ARBETSAREOR FÖR SKAPANDE AV NYA ORDERHUVUD OCH ORDERRRADER            
017400*   W1 ANVÄNDS FÖR BYGGBARA ORDER OCH W2 FÖR EJ BYGGBARA                  
017500*01  -COPY WDJ201       -PRE W1-                                          
017600     EJECT                                                                
017700*01  -COPY WDJ211       -PRE W1-                                          
017800     EJECT                                                                
017900*01  -COPY WDJ201       -PRE W2-                                          
018000     EJECT                                                                
018100*01  -COPY WDJ211       -PRE W2-                                          
018200     EJECT                                                                
018300*      --- VALID IDDC CODES                                               
018400*                                                                         
018500*01    -COPY WWDCKONS                                                     
018600*                                                                         
018700*01    -COPY WWPRODSL                                                     
018800                                                                          
018900*      --- BYTESARTIKLAR                                                  
019000*                                                                         
019100*01    -COPY WWBYT03                                                      
019200       EJECT                                                              
019300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
019400 01  GENERELLA-SUBPROGRAM.                                                
019500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
019600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
019700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
019800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
019900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
020000     03  W416PTID                PIC X(8)    VALUE 'W416PTID'.            
020100     03  W215LEVP                PIC X(8)    VALUE 'W215LEVP'.            
020200     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
020300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
020400     EJECT                                                                
020500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
020600*   -COPY WMEDAREA                                                        
020700     EJECT                                                                
020800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
020900*   -COPY WMSGINIT                                                        
021000     EJECT                                                                
021100*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
021200*   -COPY WDATAREA                                                        
021300     EJECT                                                                
021400*    --- LÄNKAREA TILL SUBPROGRAM W416PTID                                
021500*   -COPY W416PTID                                                        
021600     EJECT                                                                
021700*    --- LÄNKAREA TILL SUBPROGRAM W215LEVP                                
021800*   -COPY W215LEVP                                                        
021900     EJECT                                                                
022000 01  MESSAGE-CODES.                                                       
022100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
022200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
022300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
022400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
022500     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
022600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
022700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
022800     EJECT                                                                
022900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
023000*                                                                         
023100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
023200     SKIP3                                                                
023300*01  MID -COPY W2I30101        -PRE 2301-                                 
023400     EJECT                                                                
023500*01  MID -COPY W2I30201        -PRE 2302-                                 
023600     EJECT                                                                
023700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
023800     SKIP3                                                                
023900*01  -COPY WMSGAREA                                                       
024000     EJECT                                                                
024100     03  MOD REDEFINES MSG-AREA.                                          
024200*      05  -COPY W2O30201                                                 
024300     EJECT                                                                
024400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
024500     SKIP3                                                                
024600*01  -COPY WMFSAREA                                                       
024700     EJECT                                                                
024800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024900*                                                                         
025000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025100     SKIP3                                                                
025200 01  NYCKLAR-TILL-DLI.                                                    
025300     03  W-WDA501KY-MIN-X.                                                
025400         05  W-A501-MIN-IDDISTR    PIC S9(5)   VALUE ZERO COMP-3.         
025500         05  W-A501-MIN-IDKUNDNR   PIC S9(7)   VALUE ZERO COMP-3.         
025600         05  W-A501-MIN-IDKUNDRF.                                         
025700          07 W-A501-MIN-IDORDNR5   PIC  9(5)   VALUE ZERO.                
025800          07 FILLER                PIC  X(5)   VALUE SPACE.               
025900         05  W-A501-MIN-IDARTNR    PIC S9(9)   VALUE ZERO COMP-3.         
026000         05  W-A501-MIN-IDLOPNR    PIC S9(3)   VALUE ZERO COMP-3.         
026100                                                                          
026200     03  W-WDA501KY-MAX-X.                                                
026300         05  W-A501-MAX-IDDISTR    PIC S9(5)   VALUE ZERO COMP-3.         
026400         05  W-A501-MAX-IDKUNDNR   PIC S9(7)   VALUE ZERO COMP-3.         
026500         05  W-A501-MAX-IDKUNDRF.                                         
026600          07 W-A501-MAX-IDORDNR5   PIC  9(5)   VALUE ZERO.                
026700          07 FILLER                PIC  X(5)   VALUE SPACE.               
026800         05  W-A501-MAX-IDARTNR    PIC S9(9)   VALUE ZERO COMP-3.         
026900         05  W-A501-MAX-IDLOPNR    PIC S9(3)   VALUE ZERO COMP-3.         
027000                                                                          
027100     03  W-WDJ201KY-X.                                                    
027200         05  W-WDJ201KY          PIC S9(5)   VALUE ZERO COMP-3.           
027300     03  W-IDARTNR-X.                                                     
027400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
027500     03  W-ING-IDARTNR-X.                                                 
027600         05  W-ING-IDARTNR       PIC S9(9)   VALUE ZERO COMP-3.           
027700     03  W-KDSEGKEY-X.                                                    
027800         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
027900     03  W-IDORDNST-X.                                                    
028000         05  W-IDORDNSB          PIC S9(5)   VALUE ZERO COMP-3.           
028100         05  W-IDORDNSS          PIC S9(1)   VALUE ZERO COMP-3.           
028200     03  W-WDD3BSEQ-X.                                                    
028300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
028400     03  W-IDSKYLT-X.                                                     
028500         05  W-IDSKYLT           PIC  X(3)   VALUE SPACE.                 
028600     03  W-KDSATKMB-MIN-X.                                                
028700         05  W-KDSATKMB-MIN      PIC  X(1)   VALUE SPACE.                 
028800     03  W-KDSATKMB-MAX-X.                                                
028900         05  W-KDSATKMB-MAX      PIC  X(1)   VALUE SPACE.                 
029000                                                                          
029100     03  W-4511-IDHTYP-X.                                                 
029200         05  W-4511-IDHTYP       PIC  X(04) VALUE '4511'.                 
029300         05  W-LOW-VALUE         PIC  X(26) VALUE LOW-VALUE.              
029400                                                                          
029500     03  W-4512-KDTPOTYP-X.                                               
029600         05  W-4512-KDTPOTYP     PIC  S9(1) COMP-3.                       
029700     03  W-4512-KDORDKL-X.                                                
029800         05  W-4512-KDORDKL      PIC  S9(1) COMP-3.                       
029900     03  W-4512-IDDISTR-FOM-X.                                            
030000         05  W-4512-IDDISTR-FOM  PIC  S9(5) COMP-3.                       
030100     03  W-4512-IDDISTR-TOM-X.                                            
030200         05  W-4512-IDDISTR-TOM  PIC  S9(5) COMP-3.                       
030300     EJECT                                                                
030400*    --- STATUS-KOD FRÅN IMS                                              
030500 01  STATUS-WS                   PIC XX.                                  
030600     88  SEGMENT-FINNS                       VALUE '  '.                  
030700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
030800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
030900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
031000 01  SATG01-STATUS-WS            PIC XX.                                  
031100     88  SATG01-SEGMENT-SAKNAS               VALUE 'GE'.                  
031200     SKIP2                                                                
031300 01  GODK-STATUSKODER.                                                    
031400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031500     SKIP3                                                                
031600 01  SSA1                        PIC X(96).                               
031700 01  SSA2                        PIC X(64).                               
031800 01  SSA3                        PIC X(64).                               
031900     EJECT                                                                
032000*    --- IMS FUNKTIONSKODER                                               
032100*01  -COPY W0003                                                          
032200     EJECT                                                                
032300*    ---  DLI INPUT-OUTPUT AREA                                           
032400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
032500     SKIP3                                                                
032600 01  DLI-IO-AREA1.                                                        
032700     03  IO-AREA1                PIC X(150)  VALUE SPACE.                 
032800     SKIP3                                                                
032900     03  WLSATG01 REDEFINES IO-AREA1.                                     
033000*        05  -COPY WDJ201                                                 
033100     EJECT                                                                
033200 01  DLI-IO-AREA2.                                                        
033300     03  IO-AREA2                PIC X(150)  VALUE SPACE.                 
033400     SKIP3                                                                
033500     03  WLSATG11 REDEFINES IO-AREA2.                                     
033600*        05  -COPY WDJ211                                                 
033700     EJECT                                                                
033800 01  DLI-IO-AREA3.                                                        
033900     03  IO-AREA3                PIC X(200)  VALUE SPACE.                 
034000     03  WLBENA11 REDEFINES IO-AREA3.                                     
034100*        05  -COPY WDD311                                                 
034200     EJECT                                                                
034300 01  DLI-IO-AREA4.                                                        
034400     03  IO-AREA4                PIC X(300)  VALUE SPACE.                 
034500     03  WLORDP01 REDEFINES IO-AREA4.                                     
034600*        05  -COPY WDA501                                                 
034700     EJECT                                                                
034800     03  WLARTM01 REDEFINES IO-AREA4.                                     
034900*        05  -COPY WDK901                                                 
035000     EJECT                                                                
035100 01  DLI-IO-AREA6.                                                        
035200     03  IO-AREA6                PIC X(200)  VALUE SPACE.                 
035300     03  WLXXJN11 REDEFINES IO-AREA6.                                     
035400*        05  -COPY WDGX4512                                               
035500     EJECT                                                                
035600 01  DLI-IO-AREA7.                                                        
035700     03  IO-AREA7                PIC X(900)  VALUE SPACE.                 
035800     03  WLARTC11 REDEFINES IO-AREA7.                                     
035900*        05  -COPY WDK611                                                 
036000     EJECT                                                                
036100 01  DLI-IO-AREA8.                                                        
036200     03  IO-AREA8                PIC X(900)  VALUE SPACE.                 
036300     03  WLARTC11 REDEFINES IO-AREA8.                                     
036400*        05  -COPY WDK611  -PRE C2-                                       
036500     EJECT                                                                
036600*    ---  MSG INPUT-OUTPUT AREA  W006KOM                                  
036700 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
036800                                                                          
036900*01  -COPY WMSGAREA -PRE K                                                
037000     EJECT                                                                
037100     05  FILLER REDEFINES KMSG-MID-OUT.                                   
037200        07  -COPY W2I10902  -PRE KOM-.                                    
037300     EJECT                                                                
037400*    ---  AREA FÖR W006KOM SUBMODUL                                       
037500 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
037600     SKIP3                                                                
037700 01  KOM-IO-AREA.                                                         
037800*    03  -COPY WMSGKOM                                                    
037900     EJECT                                                                
038000 LINKAGE SECTION.                                                         
038100                                                                          
038200*01  -COPY W0009      -PRE MSG-                                           
038300     EJECT                                                                
038400*01  -COPY W0009      -PRE ALT-                                           
038500     EJECT                                                                
038600*01  -COPY W0008      -PRE KOMA-                                          
038700     05  FILLER                  PIC X.                                   
038800*01  -COPY W0008      -PRE USEA-                                          
038900     05  FILLER                  PIC X.                                   
039000     EJECT                                                                
039100*01  -COPY W0008      -PRE SATG1-                                         
039200     05  FILLER                  PIC X.                                   
039300     EJECT                                                                
039400*01  -COPY W0008      -PRE SATG2-                                         
039500     05  FILLER                  PIC X.                                   
039600     EJECT                                                                
039700*01  -COPY W0008      -PRE SATG3-                                         
039800     05  FILLER                  PIC X.                                   
039900     EJECT                                                                
040000*01  -COPY W0008      -PRE ARTC1-                                         
040100     05  FILLER                  PIC X.                                   
040200     EJECT                                                                
040300*01  -COPY W0008      -PRE ARTC2-                                         
040400     05  FILLER                  PIC X.                                   
040500     EJECT                                                                
040600*01  -COPY W0008      -PRE OWDP1-                                         
040700     05  FILLER                  PIC X.                                   
040800     EJECT                                                                
040900*01  -COPY W0008      -PRE OWDP2-                                         
041000     05  FILLER                  PIC X.                                   
041100     EJECT                                                                
041200*01  -COPY W0008      -PRE BENA-                                          
041300     05  FILLER                  PIC X.                                   
041400     EJECT                                                                
041500*01  -COPY W0008      -PRE ARTM-                                          
041600     05  FILLER                  PIC X.                                   
041700     EJECT                                                                
041800*01  -COPY W0008      -PRE XXJN-                                          
041900     05  FILLER                  PIC X.                                   
042000     SKIP3                                                                
042100***  PCB FÖR SUB PGM W215LEVP                                             
042200                                                                          
042300 01  ARTC3-PCB                   PIC X.                                   
042400 01  INLB1-PCB                   PIC X.                                   
042500 01  INLB2-PCB                   PIC X.                                   
042600 01  XXBM-PCB                    PIC X.                                   
042700 01  XXBW-PCB                    PIC X.                                   
042800                                                                          
042900***  PCB FÖR SUB PGM W416PTID                                             
043000                                                                          
043100 01  XXKH-PCB                    PIC X.                                   
043200                                                                          
043300 01  XXKI-PCB                    PIC X.                                   
043400                                                                          
043500 01  SATB-PCB                    PIC X.                                   
043600     EJECT                                                                
043700 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB KOMA-PCB USEA-PCB              
043800                                   SATG1-PCB SATG2-PCB SATG3-PCB          
043900                                   ARTC1-PCB ARTC2-PCB OWDP1-PCB          
044000                                   OWDP2-PCB BENA-PCB  ARTM-PCB           
044100                                   XXJN-PCB                               
044200                                   ARTC3-PCB INLB1-PCB INLB2-PCB          
044300                                   XXBM-PCB XXBW-PCB                      
044400                                   XXKH-PCB XXKI-PCB SATB-PCB.            
044500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB KOMA-PCB USEA-PCB              
044600                                   SATG1-PCB SATG2-PCB SATG3-PCB          
044700                                   ARTC1-PCB ARTC2-PCB OWDP1-PCB          
044800                                   OWDP2-PCB BENA-PCB  ARTM-PCB           
044900                                   XXJN-PCB                               
045000                                   ARTC3-PCB INLB1-PCB INLB2-PCB          
045100                                   XXBM-PCB XXBW-PCB                      
045200                                   XXKH-PCB XXKI-PCB SATB-PCB.            
045300                                                                          
045400     PERFORM IMS-GET-MSG                                                  
045500     IF SEGMENT-FINNS                                                     
045600       PERFORM A-INIT                                                     
045700       PERFORM B-KOLLA-NYCKLAR                                            
045800       IF NYCKLAR-OK                                                      
045900         IF MFS-UPDATE                                                    
046000           PERFORM G-KOLLA-INPUT                                          
046100           IF INDATA-OK                                                   
046200             PERFORM H-UPPDATERA                                          
046300             IF SW-PPSW-2109 = JA                                         
046400                PERFORM K-PPSW-2109-DISPATCH                              
046500             END-IF                                                       
046600           END-IF                                                         
046700         ELSE                                                             
046800           IF MFS-FIRST                                                   
046900             PERFORM C-FOERSTA-SIDA                                       
047000           ELSE                                                           
047100             IF MFS-NEXT                                                  
047200               PERFORM D-NAESTA-SIDA                                      
047300             ELSE                                                         
047400               PERFORM E-SAMMA-SIDA                                       
047500             END-IF                                                       
047600           END-IF                                                         
047700         END-IF                                                           
047800         IF ALLT-OK AND INDATA-OK                                         
047900             IF MFS-UPDATE AND IDORDER-BORTTAGEN                          
048000                 PERFORM MFS-FORM-ATTR                                    
048100                 PERFORM MFS-RENSA-FAELT-IN                               
048200              ELSE                                                        
048300                 MOVE WS-IDORDNST (1:4)  TO W-IDORDNSB                    
048400                 MOVE WS-IDORDNST (5:1)  TO W-IDORDNSS                    
048500                 PERFORM F-LAES-VISA-INFO                                 
048600             END-IF                                                       
048700         END-IF                                                           
048800       END-IF                                                             
048900       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
049000       PERFORM IMS-INSERT-MSG                                             
049100     END-IF                                                               
049200                                                                          
049300     MOVE ZERO TO RETURN-CODE                                             
049400     GOBACK                                                               
049500     .                                                                    
049600     EJECT                                                                
049700 A-INIT SECTION.                                                          
049800                                                                          
049900     MOVE NEJ                  TO  CMD-IFYLLT-SW                          
050000                                   HOPP-FRAN-2301-SW                      
050100                                   IDORDER-BORTTAGEN-SW                   
050200                                   SW-PPSW-2109                           
050300                                                                          
050400     MOVE ZERO                 TO  W-ANTAL-I-MID                          
050500                                                                          
050600     IF MSG-IDTRANS-2          =  '2301'                                  
050700         PERFORM AA-BEHANDLA-2301-MID                                     
050800      ELSE                                                                
050900                                                                          
051000         IF MSG-DUBBLA-TRANSKODER                                         
051100           MOVE MSG-INDATA-MINUS-2-TRANSKODER TO 2302-MID-W2I30201        
051200           MOVE MSG-IDTRANS-2             TO MFS-IDTRANS                  
051300           MOVE MSG-KDMFSFOR-2            TO MFS-KDMFSFOR                 
051400         ELSE                                                             
051500           MOVE MSG-INDATA-MINUS-1-TRANSKOD TO 2302-MID-W2I30201          
051600           MOVE MSG-IDTRANS-1            TO MFS-IDTRANS                   
051700           MOVE MSG-KDMFSFOR-1           TO MFS-KDMFSFOR                  
051800         END-IF                                                           
051900     END-IF                                                               
052000                                                                          
052100     MOVE MSG-KDTRTYP                    TO MFS-KDTRTYP                   
052200     MOVE MSG-IDPFK                      TO MFS-IDPFK                     
052300     MOVE MFS-IDTRANS                    TO W-IDTRANS                     
052400                                                                          
052500     MOVE LOW-VALUE                      TO MSG-AREA                      
052600     MOVE 'W2O30201'                     TO MFS-IDMOD                     
052700     MOVE '2302'                         TO MOD-IDTRANS                   
052800     MOVE MFS-RENSA-FAELT                TO MOD-TEMFSFEL                  
052900                                            MOD-TEMFSINF                  
053000                                                                          
053100     IF NOT EGEN-MID                                                      
053200       MOVE SPACE              TO MFS-KDTRTYP                             
053300       MOVE '7'                TO MFS-IDPFK                               
053400     END-IF                                                               
053500                                                                          
053600     IF ENGLISH-TEXT                                                      
053700       MOVE +2                 TO SPRAK-IX                                
053800       MOVE 'GB '              TO MED-IDSKYLT                             
053900     ELSE                                                                 
054000       MOVE +1                 TO SPRAK-IX                                
054100       MOVE 'S  '              TO MED-IDSKYLT                             
054200     END-IF                                                               
054300                                                                          
054400     PERFORM AB-INIT-KOMB-TABELLER                                        
054500                                                                          
054600**  NEJ LÄGGS I ANNULLERA REST FLAGGAN SOM DEFAULT                        
054700     IF MFS-UPDATE                                                        
054800         CONTINUE                                                         
054900      ELSE                                                                
055000         MOVE 'N'              TO MOD-FLANNULL-REST-UPDATE                
055100     END-IF                                                               
055200     .                                                                    
055300     EJECT                                                                
055400 AA-BEHANDLA-2301-MID  SECTION.                                           
055500                                                                          
055600     IF MSG-DUBBLA-TRANSKODER                                             
055700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO 2301-MID-W2I30101            
055800                                             2302-MID-W2I30201            
055900       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
056000       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
056100     ELSE                                                                 
056200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO  2301-MID-W2I30101            
056300                                             2302-MID-W2I30201            
056400       MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                   
056500       MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                  
056600     END-IF                                                               
056700                                                                          
056800     MOVE JA                   TO HOPP-FRAN-2301-SW                       
056900     MOVE +1                   TO INDX                                    
057000     PERFORM UNTIL INDX        >  MAX-RAD-2301 OR                         
057100                   CMD-IFYLLT                                             
057200        IF 2301-MID-CMD-RAD(INDX)   = '+' OR SPACE                        
057300            ADD +1             TO INDX                                    
057400         ELSE                                                             
057500            MOVE 2301-MID-IDORDNST-RAD(INDX) (1:4) TO WS-IDORDNSB         
057600            MOVE 2301-MID-IDORDNST-RAD(INDX) (5:1) TO WS-IDORDNSS         
057700            MOVE JA                   TO CMD-IFYLLT-SW                    
057800            INSPECT WS-IDORDNST REPLACING LEADING SPACE BY ZERO           
057900        END-IF                                                            
058000     END-PERFORM                                                          
058100     .                                                                    
058200     EJECT                                                                
058300 AB-INIT-KOMB-TABELLER SECTION.                                           
058400                                                                          
058500     SET IDX1                    TO +1                                    
058600     SET IDX3                    TO +1                                    
058700     PERFORM UNTIL IDX1          > 26                                     
058800         SET IDX2                TO +1                                    
058900         PERFORM UNTIL IDX2 > +5                                          
059000             MOVE ZERO         TO KOMB-ING-IDARTNR (IDX1, IDX2)           
059100                                  KOMB-REANTPSA    (IDX1, IDX2)           
059200                                  KOMB-REBEART     (IDX1, IDX2)           
059300                                  KOMB-NY-REBEART  (IDX1, IDX2)           
059400             MOVE SPACE        TO KOMB-KDSATAND    (IDX1, IDX2)           
059500             SET IDX2 UP BY +1                                            
059600         END-PERFORM                                                      
059700         MOVE ZERO     TO KOMB-KVBYGGB             (IDX1)                 
059800                          KVBYGGB-KVBYGGB          (IDX3)                 
059900                          KOMB-REST-KVBYGGB        (IDX1)                 
060000         MOVE SPACE    TO KOMB-KDSATKMB            (IDX1)                 
060100                          KVBYGGB-KDSATKMB         (IDX3)                 
060200         SET IDX1 UP BY +1                                                
060300         SET IDX3 UP BY +1                                                
060400     END-PERFORM                                                          
060500     .                                                                    
060600     EJECT                                                                
060700 B-KOLLA-NYCKLAR SECTION.                                                 
060800                                                                          
060900     MOVE JA                   TO NYCKLAR-SW                              
061000                                                                          
061100     MOVE MFS-RENSA-FAELT      TO MOD-IDORDNSB-IN                         
061200                                  MOD-IDORDNSS-IN                         
061300                                  MOD-IDANSK-FOM-IN                       
061400                                  MOD-IDANSK-TOM-IN                       
061500                                  MOD-FLBYGGB-IN                          
061600                                  MOD-IDARTNR-IN                          
061700                                                                          
061800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
061900     MOVE '001'             TO MSGI-KDCALL                                
062000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
062100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
062200     MOVE '2302'            TO MSGI-IDTRANS                               
062300                                                                          
062400     PERFORM BA-KOLLA-IDORDNST                                            
062500     PERFORM BB-FLYTTA-ANDRA-NYCKLAR                                      
062600                                                                          
062700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
062800     MOVE MSGI-IDORDNR5-FILLER TO WS-IDORDNST                             
062900     INSPECT WS-IDORDNST REPLACING LEADING SPACE BY ZERO                  
063000                                                                          
063100     IF WS-IDORDNST NUMERIC AND WS-IDORDNST > ZERO                        
063200       MOVE WS-IDORDNST (1:4)  TO W-IDORDNSB                              
063300       MOVE WS-IDORDNST (5:1)  TO W-IDORDNSS                              
063400     ELSE                                                                 
063500       MOVE NEJ                TO NYCKLAR-SW                              
063600     END-IF                                                               
063700                                                                          
063800                                                                          
063900     IF GODK-MID OR NYCKLAR-OK                                            
064000       IF GODK-MID                                                        
064100          MOVE WS-IDORDNSB        TO MOD-IDORDNSB-UT                      
064200          INSPECT MOD-IDORDNSB-UT REPLACING LEADING ZERO BY SPACE         
064300          MOVE WS-IDORDNSS        TO MOD-IDORDNSS-UT                      
064400          MOVE WS-IDANSK-FOM      TO MOD-IDANSK-FOM-UT                    
064500          INSPECT MOD-IDANSK-FOM-UT REPLACING LEADING ZERO                
064600                                  BY SPACE                                
064700          MOVE WS-IDANSK-TOM      TO MOD-IDANSK-TOM-UT                    
064800          INSPECT MOD-IDANSK-TOM-UT REPLACING LEADING ZERO                
064900                                  BY SPACE                                
065000          MOVE WS-FLBYGGB         TO MOD-FLBYGGB-UT                       
065100          MOVE WS-IDARTNR         TO MOD-IDARTNR-UT                       
065200          INSPECT MOD-IDARTNR-UT  REPLACING LEADING ZERO BY SPACE         
065300          MOVE WS-ING-IDARTNR     TO MOD-ING-IDARTNR-UT                   
065400          INSPECT MOD-ING-IDARTNR-UT REPLACING LEADING ZERO               
065500                                  BY SPACE                                
065600          MOVE WS-KDSATKMB        TO MOD-KDSATKMB-UT                      
065700       ELSE                                                               
065800          IF NYCKLAR-OK                                                   
065900             MOVE WS-IDORDNSB        TO MOD-IDORDNSB-UT                   
066000             INSPECT MOD-IDORDNSB-UT REPLACING LEADING ZERO               
066100                                     BY SPACE                             
066200             MOVE WS-IDORDNSS        TO MOD-IDORDNSS-UT                   
066300             MOVE MFS-RENSA-FAELT TO MOD-IDANSK-FOM-UT                    
066400                                     MOD-IDANSK-TOM-UT                    
066500                                     MOD-FLBYGGB-UT                       
066600                                     MOD-IDARTNR-UT                       
066700                                     MOD-KDSATKMB-UT                      
066800          END-IF                                                          
066900       END-IF                                                             
067000     ELSE                                                                 
067100       MOVE MFS-RENSA-FAELT    TO MOD-IDORDNSB-UT                         
067200                                  MOD-IDORDNSS-UT                         
067300                                  MOD-IDANSK-FOM-UT                       
067400                                  MOD-IDANSK-TOM-UT                       
067500                                  MOD-FLBYGGB-UT                          
067600                                  MOD-IDARTNR-UT                          
067700                                  MOD-KDSATKMB-UT                         
067800     END-IF                                                               
067900                                                                          
068000     IF NYCKLAR-FEL                                                       
068100       MOVE ERR-WRONG-KEY      TO MED-IDMFSFEL                            
068200       CALL WMEDKONV USING MED-WMEDAREA                                   
068300       MOVE MED-MFSFEL         TO MOD-TEMFSFEL                            
068400       PERFORM MFS-RENSA-FAELT-IN                                         
068500       PERFORM MFS-RENSA-FAELT-UT                                         
068600     END-IF                                                               
068700     .                                                                    
068800     EJECT                                                                
068900 BA-KOLLA-IDORDNST      SECTION.                                          
069000                                                                          
069100*    -- KONTROLL AV IDORDNST                                              
069200                                                                          
069300     IF HOPP-FRAN-2301 AND CMD-IFYLLT                                     
069400        MOVE WS-IDORDNST TO MSGI-IDORDNR5-FILLER                          
069500     ELSE                                                                 
069600        IF 2302-MID-IDORDNSB-IN = ALL '+'                                 
069700           IF MFS-IDTRANS = '2301' OR '2303'                              
069800              MOVE 2302-MID-IDORDNSB-UT TO WS-IDORDNSB                    
069900              MOVE 2302-MID-IDORDNSS-UT TO WS-IDORDNSS                    
070000              IF (WS-IDORDNST NUMERIC                                     
070100              AND WS-IDORDNST > ZERO)                                     
070200                 MOVE WS-IDORDNST TO MSGI-IDORDNR5-FILLER                 
070300              END-IF                                                      
070400           END-IF                                                         
070500        ELSE                                                              
070600           MOVE 2302-MID-IDORDNSB-IN TO WS-IDORDNSB                       
070700           MOVE 2302-MID-IDORDNSS-IN TO WS-IDORDNSS                       
070800           INSPECT WS-IDORDNST REPLACING LEADING SPACE BY ZERO            
070900           IF MFS-IDTRANS = '2302'                                        
071000           OR (WS-IDORDNST NUMERIC                                        
071100           AND WS-IDORDNST > ZERO)                                        
071200              MOVE WS-IDORDNST TO MSGI-IDORDNR5-FILLER                    
071300           END-IF                                                         
071400        END-IF                                                            
071500     END-IF                                                               
071600                                                                          
071700     IF 2302-MID-IDORDNSB-IN = ALL '+'                                    
071800        CONTINUE                                                          
071900     ELSE                                                                 
072000        MOVE '7'                  TO MFS-IDPFK                            
072100        MOVE SPACE                TO MFS-KDTRTYP                          
072200     END-IF                                                               
072300                                                                          
072400     .                                                                    
072500     EJECT                                                                
072600 BB-FLYTTA-ANDRA-NYCKLAR        SECTION.                                  
072700                                                                          
072800*    -- FLYTTA ANDRA NYCKLAR TILL NYCKLAR-UT                              
072900                                                                          
073000     IF HOPP-FRAN-2301                                                    
073100         IF 2301-MID-IDANSK-FOM-IN  = ALL '+'                             
073200             MOVE 2301-MID-IDANSK-FOM-UT TO WS-IDANSK-FOM                 
073300             INSPECT WS-IDANSK-FOM REPLACING LEADING SPACE BY ZERO        
073400          ELSE                                                            
073500             MOVE 2301-MID-IDANSK-FOM-IN TO WS-IDANSK-FOM                 
073600         END-IF                                                           
073700                                                                          
073800         IF 2301-MID-IDANSK-TOM-IN  = ALL '+'                             
073900             MOVE 2301-MID-IDANSK-TOM-UT TO WS-IDANSK-TOM                 
074000             INSPECT WS-IDANSK-TOM REPLACING LEADING SPACE BY ZERO        
074100          ELSE                                                            
074200             MOVE 2301-MID-IDANSK-TOM-IN TO WS-IDANSK-TOM                 
074300         END-IF                                                           
074400                                                                          
074500         IF 2301-MID-FLBYGGB-IN      = ALL '+'                            
074600             MOVE 2301-MID-FLBYGGB-UT     TO WS-FLBYGGB                   
074700          ELSE                                                            
074800             MOVE 2301-MID-FLBYGGB-IN     TO WS-FLBYGGB                   
074900         END-IF                                                           
075000                                                                          
075100         IF 2301-MID-KDSATKMB-IN     = ALL '+'                            
075200             MOVE 2301-MID-KDSATKMB-UT    TO WS-KDSATKMB                  
075300          ELSE                                                            
075400             MOVE 2301-MID-KDSATKMB-IN    TO WS-KDSATKMB                  
075500         END-IF                                                           
075600                                                                          
075700         IF 2301-MID-IDARTNR-IN      = ALL '+'                            
075800             MOVE 2301-MID-IDARTNR-UT    TO WS-IDARTNR                    
075900             INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO           
076000          ELSE                                                            
076100             MOVE 2301-MID-IDARTNR-IN    TO WS-IDARTNR                    
076200         END-IF                                                           
076300                                                                          
076400         IF 2301-MID-ING-IDARTNR-IN  = ALL '+'                            
076500             MOVE 2301-MID-ING-IDARTNR-UT TO WS-ING-IDARTNR               
076600             INSPECT WS-ING-IDARTNR                                       
076700                                REPLACING LEADING SPACE BY ZERO           
076800          ELSE                                                            
076900             MOVE 2301-MID-ING-IDARTNR-IN TO WS-ING-IDARTNR               
077000         END-IF                                                           
077100      ELSE                                                                
077200         IF 2302-MID-IDANSK-FOM-IN  = ALL '+'                             
077300            MOVE 2302-MID-IDANSK-FOM-UT TO WS-IDANSK-FOM                  
077400            INSPECT WS-IDANSK-FOM REPLACING LEADING SPACE BY ZERO         
077500         ELSE                                                             
077600             MOVE 2302-MID-IDANSK-FOM-IN TO WS-IDANSK-FOM                 
077700         END-IF                                                           
077800                                                                          
077900         IF 2302-MID-IDANSK-TOM-IN  = ALL '+'                             
078000             MOVE 2302-MID-IDANSK-TOM-UT TO WS-IDANSK-TOM                 
078100             INSPECT WS-IDANSK-TOM REPLACING LEADING SPACE BY ZERO        
078200          ELSE                                                            
078300             MOVE 2302-MID-IDANSK-TOM-IN TO WS-IDANSK-TOM                 
078400         END-IF                                                           
078500                                                                          
078600         IF 2302-MID-FLBYGGB-IN      = ALL '+'                            
078700             MOVE 2302-MID-FLBYGGB-UT     TO WS-FLBYGGB                   
078800          ELSE                                                            
078900             MOVE 2302-MID-FLBYGGB-IN     TO WS-FLBYGGB                   
079000         END-IF                                                           
079100                                                                          
079200         IF 2302-MID-KDSATKMB-IN     = ALL '+'                            
079300             MOVE 2302-MID-KDSATKMB-UT    TO WS-KDSATKMB                  
079400          ELSE                                                            
079500             MOVE 2302-MID-KDSATKMB-IN    TO WS-KDSATKMB                  
079600         END-IF                                                           
079700                                                                          
079800         IF 2302-MID-IDARTNR-IN      = ALL '+'                            
079900            MOVE 2302-MID-IDARTNR-UT    TO WS-IDARTNR                     
080000            INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO            
080100          ELSE                                                            
080200             MOVE 2302-MID-IDARTNR-IN    TO WS-IDARTNR                    
080300         END-IF                                                           
080400                                                                          
080500         IF 2302-MID-ING-IDARTNR-IN  = ALL '+'                            
080600             MOVE 2302-MID-ING-IDARTNR-UT TO WS-ING-IDARTNR               
080700             INSPECT WS-ING-IDARTNR                                       
080800                                REPLACING LEADING SPACE BY ZERO           
080900          ELSE                                                            
081000             MOVE 2302-MID-ING-IDARTNR-IN TO WS-ING-IDARTNR               
081100         END-IF                                                           
081200     END-IF                                                               
081300     .                                                                    
081400     EJECT                                                                
081500 C-FOERSTA-SIDA SECTION.                                                  
081600                                                                          
081700     MOVE INF-FIRST-PAGE       TO MED-IDMFSINF                            
081800     CALL WMEDKONV USING MED-WMEDAREA                                     
081900     MOVE MED-MFSINF           TO MOD-TEMFSINF                            
082000                                                                          
082100     MOVE ZERO                 TO W-IDARTNR IN W-IDARTNR-X                
082200     MOVE JA                   TO ALLT-SW                                 
082300     .                                                                    
082400     EJECT                                                                
082500 D-NAESTA-SIDA SECTION.                                                   
082600                                                                          
082700     MOVE 2302-MID-IDARTNR-NEXT    TO W-ING-IDARTNR                       
082800     MOVE JA                       TO ALLT-SW                             
082900     .                                                                    
083000     EJECT                                                                
083100 E-SAMMA-SIDA SECTION.                                                    
083200                                                                          
083300     IF 2302-MID-INPUT              = ALL '+'                             
083400       MOVE 2302-MID-IDARTNR-ENTER  TO W-ING-IDARTNR                      
083500       MOVE JA                      TO ALLT-SW                            
083600     ELSE                                                                 
083700       MOVE NEJ                TO ALLT-SW                                 
083800       MOVE INF-PRESS-PF11     TO MED-IDMFSINF                            
083900       CALL WMEDKONV USING MED-WMEDAREA                                   
084000       MOVE MED-MFSINF         TO MOD-TEMFSINF                            
084100       PERFORM MFS-ROER-EJ-FAELT-IN                                       
084200       PERFORM MFS-ROER-EJ-FAELT-UT                                       
084300       PERFORM MFS-LAES-IN-IGEN                                           
084400     END-IF                                                               
084500     .                                                                    
084600     EJECT                                                                
084700 F-LAES-VISA-INFO SECTION.                                                
084800                                                                          
084900     PERFORM FA-LAES-GRUNDDATA                                            
085000                                                                          
085100     IF SATG01-SEGMENT-SAKNAS OR SHUV-KDSATSTA NOT = 'R'                  
085200         IF SATG01-SEGMENT-SAKNAS                                         
085300             MOVE '701'        TO MED-IDMFSFEL                            
085400             CALL WMEDKONV USING MED-WMEDAREA                             
085500             MOVE MED-MFSFEL   TO MOD-TEMFSFEL                            
085600             PERFORM MFS-RENSA-FAELT-UT                                   
085700          ELSE                                                            
085800             MOVE '124'        TO MED-IDMFSFEL                            
085900             CALL WMEDKONV USING MED-WMEDAREA                             
086000             MOVE MED-MFSFEL   TO MOD-TEMFSFEL                            
086100             PERFORM MFS-RENSA-FAELT-UT                                   
086200         END-IF                                                           
086300     ELSE                                                                 
086400         MOVE +1               TO INDX                                    
086500         IF MFS-FIRST                                                     
086600             PERFORM IMS-GN-SATG1-SATG11                                  
086700          ELSE                                                            
086800             PERFORM IMS-GU-SATG1-SATG11                                  
086900         END-IF                                                           
087000         IF SEGMENT-FINNS AND SRAD-KVSATROS > ZERO                        
087100             MOVE SRAD-IDARTNR TO MOD-IDARTNR-ENTER                       
087200          ELSE                                                            
087300             MOVE ZERO         TO MOD-IDARTNR-ENTER                       
087400         END-IF                                                           
087500                                                                          
087600         PERFORM UNTIL INDX > MAX-RAD-2302                                
087700            IF SEGMENT-FINNS                                              
087800               IF SRAD-KVSATROS                 > ZERO OR                 
087900                  SRAD-FLSATSPR                 = JA                      
088000                 IF MOD-IDARTNR-ENTER           = ZERO                    
088100                     MOVE SRAD-IDARTNR TO MOD-IDARTNR-ENTER               
088200                 END-IF                                                   
088300                 PERFORM IMS-GU-ARTC1-ARTC11                              
088400                 PERFORM FB-REDIGERA-RAD                                  
088500                 ADD 1 TO INDX                                            
088600               END-IF                                                     
088700               PERFORM IMS-GN-SATG1-SATG11                                
088800             ELSE                                                         
088900               IF MFS-NEXT AND INDX  = +1                                 
089000                   MOVE '115'        TO MED-IDMFSFEL                      
089100                   CALL WMEDKONV USING MED-WMEDAREA                       
089200                   MOVE MED-MFSFEL   TO MOD-TEMFSFEL                      
089300               END-IF                                                     
089400               PERFORM MFS-RENSA-RAD-FAELT-UT                             
089500               ADD 1 TO INDX                                              
089600            END-IF                                                        
089700         END-PERFORM                                                      
089800                                                                          
089900         MOVE ZERO             TO MOD-IDARTNR-NEXT                        
090000         PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                  
090100                       EJ-TACKT-RAD                                       
090200           IF SEGMENT-FINNS                AND                            
090300             (SRAD-KVSATROS >  ZERO  OR                                   
090400              SRAD-FLSATSPR =  JA)                                        
090500               MOVE SRAD-IDARTNR               TO MOD-IDARTNR-NEXT        
090600               MOVE INF-MORE-INFO-EXISTS        TO MED-IDMFSINF           
090700               CALL WMEDKONV USING MED-WMEDAREA                           
090800               MOVE MED-TEMFSINF                TO MOD-TEMFSINF           
090900               MOVE JA                          TO EJ-TACKT-RAD-SW        
091000           END-IF                                                         
091100           PERFORM IMS-GN-SATG1-SATG11                                    
091200         END-PERFORM                                                      
091300     END-IF                                                               
091400                                                                          
091500     IF SHUV-FLSATSPR          = JA                                       
091600         MOVE '125'                       TO MED-IDMFSINF                 
091700         CALL WMEDKONV USING MED-WMEDAREA                                 
091800         MOVE MED-TEMFSINF                TO MOD-TEMFSINF                 
091900     END-IF                                                               
092000                                                                          
092100     PERFORM MFS-RENSA-FAELT-IN                                           
092200     MOVE 'N'                  TO MOD-FLANNULL-REST-UPDATE                
092300     .                                                                    
092400     EJECT                                                                
092500 FA-LAES-GRUNDDATA SECTION.                                               
092600                                                                          
092700     PERFORM IMS-GU-SATG1-SATG01                                          
092800                                                                          
092900     IF SEGMENT-FINNS                                                     
093000         MOVE SHUV-IDARTNR     TO  W-IDARTNR1-9 (1:9)                     
093100                                   W-IDARTNR                              
093200                               IN  W-IDARTNR-X                            
093300         MOVE SHUV-REKSIFFR    TO  W-REKSIFFR                             
093400         MOVE W-IDARTNR-11     TO  MOD-IDARTNR                            
093500         INSPECT MOD-IDARTNR REPLACING LEADING ZERO BY SPACE              
093600         MOVE SHUV-KDCLAGER    TO  MOD-KDCLAGER                           
093700         MOVE SHUV-IDPRC       TO  MOD-IDPRC                              
093800                                                                          
093900         MOVE W-IDARTNR        IN  W-IDARTNR-X                            
094000                               TO  W-IDARTNR                              
094100                               IN  W-WDD3BSEQ-X                           
094200         MOVE MED-IDSKYLT      TO  W-IDSKYLT                              
094300         PERFORM IMS-GU-BENA11                                            
094400         MOVE TEXT-BEART       TO  MOD-BEART                              
094500                                                                          
094600         PERFORM FAA-CLAG-INFO                                            
094700                                                                          
094800         MOVE SHUV-KVBEART     TO  MOD-KVBEART                            
094900         MOVE SHUV-FLBYGGB     TO  MOD-FLBYGGB                            
095000         MOVE SHUV-KVBYGGB     TO  MOD-KVBYGGB                            
095100         MOVE SHUV-FLSATPRI    TO  MOD-FLSATPRI                           
095200         IF SHUV-FLSATSPR      =   JA                                     
095300             MOVE SHUV-FLSATSPR TO MOD-FLSATSPR-HUV                       
095400          ELSE                                                            
095500             MOVE MFS-RENSA-FAELT  TO MOD-FLSATSPR-HUV                    
095600         END-IF                                                           
095700     END-IF                                                               
095800     .                                                                    
095900     EJECT                                                                
096000 FAA-CLAG-INFO       SECTION.                                             
096100                                                                          
096200     MOVE NEJ                  TO ARTM01-FINNS-SW                         
096300     PERFORM IMS-GU-ARTM01                                                
096400     IF SEGMENT-FINNS                                                     
096500         MOVE JA               TO ARTM01-FINNS-SW                         
096600      ELSE                                                                
096700         MOVE ZERO             TO W-OKS                                   
096800     END-IF                                                               
096900                                                                          
097000     PERFORM IMS-GU-ARTC1-ARTC11                                          
097100     MOVE +1                   TO INDX                                    
097200         IF SEGMENT-FINNS                                                 
097300             MOVE +1            TO MOD-HUV-KDCLAGER (INDX)                
097400             COMPUTE MOD-KVPB-SATS (INDX) = CLAG-KVPB-SATS +              
097500                                             CLAG-KVPB-SEP +              
097600                                             CLAG-KVPB-TPO                
097700             MOVE CLAG-KVROS    TO MOD-KVROS    (INDX)                    
097800             MOVE CLAG-KVSLAGER TO MOD-KVSLAGER (INDX)                    
097900             IF ARTM01-FINNS                                              
098000                 COMPUTE W-OKS    =  ART-KVOKS-BULK  +                    
098100                                     ART-KVOKS-DAG   +                    
098200                                     ART-KVOKS-VOR                        
098300             END-IF                                                       
098400             COMPUTE MOD-KVDISP(INDX) = CLAG-KVLS -                       
098500                                       CLAG-KVRESS -                      
098600                                       W-OKS                              
098700         ELSE                                                             
098800             MOVE MFS-RENSA-FAELT TO MOD-HUV-KDCLAGER (INDX)              
098900                                     MOD-KVPB-SATS    (INDX)              
099000             MOVE MFS-RENSA-FAELT TO MOD-KVDISP(INDX)                     
099100         END-IF                                                           
099200         MOVE MFS-RENSA-FAELT TO MOD-HUV-KDCLAGER (2)                     
099300                                 MOD-KVPB-SATS    (2)                     
099400         MOVE MFS-RENSA-FAELT TO MOD-KVDISP(2)                            
099500     .                                                                    
099600     EJECT                                                                
099700                                                                          
099800 FB-REDIGERA-RAD SECTION.                                                 
099900                                                                          
100000     MOVE SRAD-IDARTNR         TO  W-IDARTNR1-9 (1:9)                     
100100     MOVE SRAD-REKSIFFR        TO  W-REKSIFFR                             
100200     MOVE W-IDARTNR-11         TO MOD-ING-IDARTNR-RAD   (INDX)            
100300     INSPECT MOD-ING-IDARTNR-RAD (INDX)                                   
100400                         REPLACING LEADING ZERO BY SPACE                  
100500     MOVE SRAD-REANTPSA        TO MOD-REANTPSA-RAD  (INDX)                
100600     MOVE SRAD-REBEART         TO MOD-REBEART-RAD   (INDX)                
100700     MOVE SRAD-KVSATRES        TO MOD-KVSATRES-RAD  (INDX)                
100800     MOVE SRAD-KVSATROS        TO MOD-KVSATROS-RAD  (INDX)                
100900     MOVE SRAD-IDARTNR         TO  W-IDARTNR                              
101000                               IN  W-IDARTNR-X                            
101100     PERFORM IMS-GU-ARTC1-ARTC11                                          
101200     MOVE CLAG-TIDISPIN        TO MOD-TIDISPIN-RAD  (INDX)                
101300     MOVE CLAG-IDANSK          TO MOD-IDANSK-RAD    (INDX)                
101400     IF SRAD-FLSATSPR          =  JA                                      
101500         MOVE SRAD-FLSATSPR    TO MOD-FLSATSPR-RAD  (INDX)                
101600      ELSE                                                                
101700         MOVE MFS-RENSA-FAELT  TO MOD-FLSATSPR-RAD  (INDX)                
101800     END-IF                                                               
101900     .                                                                    
102000     EJECT                                                                
102100 G-KOLLA-INPUT SECTION.                                                   
102200                                                                          
102300     MOVE JA                   TO INDATA-SW                               
102400     IF 2302-MID-INPUT         = ALL '+'                                  
102500       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
102600       CALL WMEDKONV USING MED-WMEDAREA                                   
102700       MOVE MED-MFSFEL         TO MOD-TEMFSFEL                            
102800       PERFORM MFS-ROER-EJ-FAELT-IN                                       
102900       PERFORM MFS-ROER-EJ-FAELT-UT                                       
103000       MOVE NEJ                TO INDATA-SW                               
103100     ELSE                                                                 
103200                                                                          
103300       PERFORM IMS-GU-SATG1-SATG01                                        
103400       IF SEGMENT-FINNS                                                   
103500         IF SHUV-KDSATSTA = 'R'                                           
103600           MOVE SHUV-KDPRODSL    TO TEST-KDPRODSL                         
103700           IF KDPRODSL-LOCAL                                              
103800              MOVE NEJ         TO INDATA-SW                               
103900           END-IF                                                         
104000           IF 2302-MID-KVDELA-UPDATE        NOT = ALL '+' OR              
104100              2302-MID-FLANNULL-REST-UPDATE     = 'J' OR 'N'              
104200               PERFORM GA-KOLLA-DELA-INPUT                                
104300           END-IF                                                         
104400                                                                          
104500           IF 2302-MID-FLANNULL-HELA-UPDATE NOT = ALL '+'                 
104600              PERFORM GB-KOLLA-ANNULLERING-INPUT                          
104700           END-IF                                                         
104800                                                                          
104900           IF 2302-MID-FLSATPRI-UPDATE NOT = ALL '+'                      
105000              PERFORM GC-KOLLA-FLSATPRI-INPUT                             
105100           END-IF                                                         
105200                                                                          
105300           IF INDATA-FEL                                                  
105400               IF MOD-TEMFSFEL  = MFS-RENSA-FAELT                         
105500                   MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL              
105600                   CALL WMEDKONV USING MED-WMEDAREA                       
105700                   MOVE MED-MFSFEL TO MOD-TEMFSFEL                        
105800               END-IF                                                     
105900               PERFORM MFS-ROER-EJ-FAELT-UT                               
106000               PERFORM MFS-ROER-EJ-FAELT-IN                               
106100           END-IF                                                         
106200         ELSE                                                             
106300           MOVE '124'            TO MED-IDMFSFEL                          
106400           MOVE NEJ              TO INDATA-SW                             
106500           CALL WMEDKONV USING MED-WMEDAREA                               
106600           MOVE MED-MFSFEL       TO MOD-TEMFSFEL                          
106700           PERFORM MFS-RENSA-FAELT-IN                                     
106800           PERFORM MFS-RENSA-FAELT-UT                                     
106900         END-IF                                                           
107000       ELSE                                                               
107100         MOVE '701'            TO MED-IDMFSFEL                            
107200         MOVE NEJ              TO INDATA-SW                               
107300         CALL WMEDKONV USING MED-WMEDAREA                                 
107400         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
107500         PERFORM MFS-RENSA-FAELT-IN                                       
107600         PERFORM MFS-RENSA-FAELT-UT                                       
107700       END-IF                                                             
107800     END-IF                                                               
107900     .                                                                    
108000     EJECT                                                                
108100 GA-KOLLA-DELA-INPUT     SECTION.                                         
108200                                                                          
108300     IF (2302-MID-FLANNULL-REST-UPDATE   =  JA OR NEJ)      AND           
108400        (2302-MID-FLANNULL-HELA-UPDATE   =  '+' OR SPACE)   AND           
108500        (2302-MID-FLSATPRI-UPDATE        =  '+' OR SPACE)                 
108600         MOVE MFS-ALFA-FAELT-RAETT    TO                                  
108700                           MOD-FLANNULL-REST-UPDATE-ATTR                  
108800         IF 2302-MID-KVDELA-UPDATE NUMERIC                                
108900           MOVE 2302-MID-KVDELA-UPDATE   TO  W-MID-KVDELA-UPDATE          
109000            IF W-MID-KVDELA-UPDATE       <= SHUV-KVBYGGB AND              
109100               W-MID-KVDELA-UPDATE       >  ZERO                          
109200                MOVE MFS-NUM-FAELT-RAETT TO MOD-KVDELA-UPDATE-ATTR        
109300             ELSE                                                         
109400                MOVE MFS-ALFA-FAELT-FEL  TO MOD-KVDELA-UPDATE-ATTR        
109500                MOVE NEJ                 TO INDATA-SW                     
109600             END-IF                                                       
109700          ELSE                                                            
109800            MOVE MFS-ALFA-FAELT-FEL  TO MOD-KVDELA-UPDATE-ATTR            
109900            MOVE NEJ                 TO INDATA-SW                         
110000         END-IF                                                           
110100      ELSE                                                                
110200         MOVE MFS-ALFA-FAELT-FEL     TO                                   
110300                          MOD-FLANNULL-REST-UPDATE-ATTR                   
110400         MOVE NEJ                    TO INDATA-SW                         
110500     END-IF                                                               
110600                                                                          
110700     MOVE 9                    TO W-IDORDNSS                              
110800     PERFORM IMS-GU-SATG2-SATG01                                          
110900     IF SEGMENT-FINNS                                                     
111000         MOVE NEJ              TO INDATA-SW                               
111100     END-IF                                                               
111200     MOVE WS-IDORDNST (5:1)    TO W-IDORDNSS                              
111300                                                                          
111400     PERFORM GAA-KOLLA-KDSATKMB-KDSATAND                                  
111500                                                                          
111600     IF INDATA-OK AND 2302-MID-FLANNULL-REST-UPDATE = JA                  
111700         COMPUTE W-KVANNANT    =                                          
111800                      SHUV-KVBEART - W-MID-KVDELA-UPDATE                  
111900         PERFORM S03-UPPDATERA-LEVPLAN                                    
112000         IF LEVP-KDSVAR        = '1'                                      
112100             MOVE NEJ          TO INDATA-SW                               
112200             MOVE '066'        TO MED-IDMFSFEL                            
112300             CALL WMEDKONV USING MED-WMEDAREA                             
112400             MOVE MED-MFSFEL   TO MOD-TEMFSFEL                            
112500         END-IF                                                           
112600     END-IF                                                               
112700     .                                                                    
112800     EJECT                                                                
112900 GAA-KOLLA-KDSATKMB-KDSATAND    SECTION.                                  
113000                                                                          
113100     PERFORM IMS-GN-SATG1-SATG11                                          
113200     PERFORM UNTIL SEGMENT-SLUT OR SEGMENT-SAKNAS                         
113300         IF SRAD-KDSATKMB      = SPACE      OR                            
113400           (SRAD-KVSATRES      = ZERO AND                                 
113500            SRAD-KVSATROS      = ZERO)                                    
113600             CONTINUE                                                     
113700          ELSE                                                            
113800             PERFORM GAAA-FYLL-KDSATKMB-TAB                               
113900         END-IF                                                           
114000         PERFORM IMS-GN-SATG1-SATG11                                      
114100     END-PERFORM                                                          
114200                                                                          
114300     PERFORM GAAB-KOLLA-KDSATAND                                          
114400                                                                          
114500     IF FELAKTIG-KDSATAND                                                 
114600         MOVE NEJ              TO INDATA-SW                               
114700         MOVE '140'            TO MED-IDMFSFEL                            
114800         CALL WMEDKONV USING MED-WMEDAREA                                 
114900         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
115000     END-IF                                                               
115100                                                                          
115200     .                                                                    
115300     EJECT                                                                
115400 GAAA-FYLL-KDSATKMB-TAB          SECTION.                                 
115500                                                                          
115600     SET IDX1                  TO +1                                      
115700     PERFORM UNTIL KOMB-TRAFF OR IDX1 > 26                                
115800         IF SRAD-KDSATKMB      = KOMB-KDSATKMB  (IDX1)                    
115900             MOVE JA           TO KOMB-TRAFF-SW                           
116000          ELSE                                                            
116100             IF KOMB-KDSATKMB (IDX1) = SPACE                              
116200                 MOVE SRAD-KDSATKMB TO KOMB-KDSATKMB (IDX1)               
116300                 MOVE JA            TO KOMB-TRAFF-SW                      
116400              ELSE                                                        
116500                 SET IDX1           UP BY +1                              
116600             END-IF                                                       
116700         END-IF                                                           
116800     END-PERFORM                                                          
116900                                                                          
117000     IF IDX1                   > 26                                       
117100         MOVE 'FELAKTIG KOMB KOD ' TO FELTEXT                             
117200         CALL ABEND USING ABEND-MED-DUMP                                  
117300     END-IF                                                               
117400                                                                          
117500     SET IDX2                  TO +1                                      
117600     PERFORM UNTIL KOMB-ING-IDARTNR (IDX1, IDX2) = ZERO OR                
117700                   IDX2        > 5                                        
117800         SET IDX2 UP BY +1                                                
117900     END-PERFORM                                                          
118000                                                                          
118100     IF IDX2                   > 5                                        
118200         MOVE 'FLER ÄN 5 ST ING ART PÅ SAMMA KOMB KOD'                    
118300              TO FELTEXT                                                  
118400         CALL ABEND USING ABEND-MED-DUMP                                  
118500     END-IF                                                               
118600                                                                          
118700     MOVE SRAD-IDARTNR         TO KOMB-ING-IDARTNR (IDX1, IDX2)           
118800     MOVE SRAD-REANTPSA        TO KOMB-REANTPSA    (IDX1, IDX2)           
118900     MOVE SRAD-REBEART         TO KOMB-REBEART     (IDX1, IDX2)           
119000     MOVE SRAD-KDSATAND        TO KOMB-KDSATAND    (IDX1, IDX2)           
119100     .                                                                    
119200     EJECT                                                                
119300 GAAB-KOLLA-KDSATAND             SECTION.                                 
119400                                                                          
119500**   FÖR ATT MASKINELL DELNING SKALL SKE PÅ EN                            
119600**   SATS ARTIKEL MED KOMBINATIONSKOD MÅSTE EN                            
119700**   AV DE ING ARTIKLARNA PER KOMB KOD HA STATUS                          
119800**   'E'. I ANNAT FALL GES FELMEDDELANDE.                                 
119900                                                                          
120000     SET IDX1                  TO +1                                      
120100     PERFORM UNTIL IDX1        > 26             OR                        
120200                   KOMB-KDSATKMB (IDX1) = SPACE OR                        
120300                   FELAKTIG-KDSATAND                                      
120400         SET IDX2              TO +1                                      
120500         MOVE JA               TO FELAKTIG-KDSATAND-SW                    
120600         PERFORM UNTIL KOMB-ING-IDARTNR (IDX1, IDX2) = ZERO OR            
120700                       NOT FELAKTIG-KDSATAND        OR                    
120800                       IDX2    > 5                                        
120900             IF KOMB-KDSATAND (IDX1, IDX2) = 'E'                          
121000                 MOVE NEJ TO FELAKTIG-KDSATAND-SW                         
121100             END-IF                                                       
121200             SET IDX2 UP BY +1                                            
121300         END-PERFORM                                                      
121400         SET IDX1 UP BY +1                                                
121500     END-PERFORM                                                          
121600     .                                                                    
121700     EJECT                                                                
121800 GB-KOLLA-ANNULLERING-INPUT    SECTION.                                   
121900                                                                          
122000     IF (2302-MID-FLANNULL-HELA-UPDATE   =  JA   AND                      
122100         SHUV-KDSATSTA                   =  'R')             AND          
122200        (2302-MID-FLANNULL-REST-UPDATE   =  '+' OR SPACE)    AND          
122300        (2302-MID-FLSATPRI-UPDATE        =  '+' OR SPACE)    AND          
122400        (2302-MID-KVDELA-UPDATE          =  ALL '+' OR SPACE)             
122500         MOVE MFS-ALFA-FAELT-RAETT    TO                                  
122600                           MOD-FLANNULL-HELA-UPDATE-ATTR                  
122700      ELSE                                                                
122800         MOVE MFS-ALFA-FAELT-FEL      TO                                  
122900                          MOD-FLANNULL-HELA-UPDATE-ATTR                   
123000         MOVE NEJ                     TO INDATA-SW                        
123100     END-IF                                                               
123200                                                                          
123300     IF INDATA-OK                                                         
123400         MOVE SHUV-KVBEART     TO W-KVANNANT                              
123500         PERFORM S03-UPPDATERA-LEVPLAN                                    
123600         IF LEVP-KDSVAR        =  '1'                                     
123700             MOVE '066'        TO MED-IDMFSFEL                            
123800             CALL WMEDKONV USING MED-WMEDAREA                             
123900             MOVE MED-MFSFEL   TO MOD-TEMFSFEL                            
124000             MOVE NEJ          TO INDATA-SW                               
124100         END-IF                                                           
124200     END-IF                                                               
124300     .                                                                    
124400     EJECT                                                                
124500 GC-KOLLA-FLSATPRI-INPUT    SECTION.                                      
124600                                                                          
124700     IF ((2302-MID-FLSATPRI-UPDATE       =  JA OR NEJ)  AND               
124800         (SHUV-FLBYGGB                   =  JA))             AND          
124900        (2302-MID-FLANNULL-REST-UPDATE   =  '+' OR SPACE)    AND          
125000        (2302-MID-FLANNULL-HELA-UPDATE   =  '+' OR SPACE)    AND          
125100        (2302-MID-KVDELA-UPDATE          =  ALL '+' OR SPACE)             
125200         MOVE MFS-ALFA-FAELT-RAETT    TO                                  
125300                           MOD-FLSATPRI-UPDATE-ATTR                       
125400      ELSE                                                                
125500         IF SHUV-FLBYGGB              =  NEJ                              
125600             MOVE '123'               TO MED-IDMFSFEL                     
125700             CALL WMEDKONV            USING MED-WMEDAREA                  
125800             MOVE MED-MFSFEL          TO MOD-TEMFSFEL                     
125900         END-IF                                                           
126000         MOVE MFS-ALFA-FAELT-FEL      TO                                  
126100                          MOD-FLSATPRI-UPDATE-ATTR                        
126200         MOVE NEJ                     TO INDATA-SW                        
126300     END-IF                                                               
126400     .                                                                    
126500     EJECT                                                                
126600 H-UPPDATERA SECTION.                                                     
126700                                                                          
126800     PERFORM IMS-GHU-SATG1-SATG01                                         
126900     IF SEGMENT-FINNS                                                     
127000         MOVE 'IDAG  '       TO DAT-KDDATFORM                             
127100         CALL WDATKONV USING    DAT-KDDATFORM DAT-I-TIDATUM               
127200                                DAT-O-TIDATUM DAT-KDSVAR                  
127300         IF DAT-KDSVAR-FEL                                                
127400            MOVE '*** W20302  - FEL I DATKONV ***' TO FELTEXT             
127500            CALL ABEND USING ABEND-UTAN-DUMP                              
127600         END-IF                                                           
127700         MOVE SHUV-KVBEART     TO W-KVBEART                               
127800         MOVE SHUV-KVBYGGB     TO W-KVBYGGB                               
127900         MOVE SHUV-WDJ201      TO W1-SHUV-WDJ201                          
128000         IF 2302-MID-FLANNULL-REST-UPDATE      = JA                       
128100             PERFORM HA-UPPD-DELA-ANNULL-REST                             
128200          ELSE                                                            
128300             IF 2302-MID-FLANNULL-REST-UPDATE  = NEJ                      
128400                 PERFORM HB-UPPD-DELA-ANNULL-EJ-REST                      
128500             END-IF                                                       
128600         END-IF                                                           
128700                                                                          
128800         IF 2302-MID-FLANNULL-HELA-UPDATE      = JA                       
128900             PERFORM HC-UPPD-ANNULL-HELA                                  
129000             MOVE JA           TO IDORDER-BORTTAGEN-SW                    
129100         END-IF                                                           
129200                                                                          
129300         IF 2302-MID-FLSATPRI-UPDATE           = JA OR NEJ                
129400             PERFORM HD-UPPD-MAN-PRIO                                     
129500         END-IF                                                           
129600                                                                          
129700         MOVE INF-UPDATE-DONE    TO MED-IDMFSINF                          
129800         CALL WMEDKONV USING MED-WMEDAREA                                 
129900         MOVE MED-MFSINF         TO MOD-TEMFSINF                          
130000*     MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
130100     END-IF                                                               
130200     .                                                                    
130300     EJECT                                                                
130400 HA-UPPD-DELA-ANNULL-REST       SECTION.                                  
130500                                                                          
130600     MOVE ZERO                 TO  W1-VLARTNTO                            
130700                                   W1-VKARTNTO                            
130800                                   W2-VLARTNTO                            
130900                                   W2-VKARTNTO                            
131000                                                                          
131100     IF KOMB-KDSATKMB (1)      = SPACE                                    
131200         CONTINUE                                                         
131300      ELSE                                                                
131400         PERFORM S04-BEHANDLA-KDSATKMB                                    
131500     END-IF                                                               
131600                                                                          
131700     PERFORM HAA-UPPD-ING-ART                                             
131800     PERFORM HAB-UPPD-SATS-ART                                            
131900     .                                                                    
132000     EJECT                                                                
132100 HAA-UPPD-ING-ART               SECTION.                                  
132200                                                                          
132300     PERFORM IMS-GHNP-SATG1-SATG11                                        
132400                                                                          
132500     MOVE ZERO                 TO W1-KVRADER                              
132600                                  W2-KVRADER                              
132700     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
132800         MOVE SRAD-WDJ211      TO W1-SRAD-WDJ211                          
132900         MOVE W1-SRAD-IDARTNR  TO W-IDARTNR                               
133000                               IN W-IDARTNR-X                             
133100         MOVE W1-SRAD-REBEART  TO W-REBEART                               
133200         PERFORM HAAA-UPPD-SATG11                                         
133300         PERFORM HAAB-UPPD-ARTC11                                         
133400         PERFORM S01-UPPD-ORDP01-ARTC11                                   
133500         PERFORM IMS-GHNP-SATG1-SATG11                                    
133600     END-PERFORM                                                          
133700     .                                                                    
133800     EJECT                                                                
133900 HAAA-UPPD-SATG11               SECTION.                                  
134000                                                                          
134100     MOVE W1-SRAD-KVSATRES     TO W-OLD-KVSATRES                          
134200     IF W1-SRAD-KVSATRES       =  ZERO AND                                
134300        W1-SRAD-KVSATROS       =  ZERO                                    
134400         MOVE ZERO             TO W1-SRAD-REBEART                         
134500      ELSE                                                                
134600         IF W1-SRAD-KDSATKMB   =  SPACE                                   
134700             COMPUTE W-HELP-REBEART = (W1-SRAD-REANTPSA *                 
134800                                   W-MID-KVDELA-UPDATE) + 0.999           
134900             MOVE W-HELP-REBEART TO W1-SRAD-REBEART                       
135000          ELSE                                                            
135100             PERFORM S05-KOLLA-KDSATKMB-TAB                               
135200             MOVE KOMB-NY-REBEART (IDX1, IDX2) TO W1-SRAD-REBEART         
135300         END-IF                                                           
135400     END-IF                                                               
135500                                                                          
135600     COMPUTE W-RZD-MINSKNING   =  W-REBEART - W1-SRAD-REBEART             
135700     MOVE W1-SRAD-REBEART      TO W1-SRAD-KVSATRES                        
135800     MOVE ZERO                 TO W1-SRAD-KVSATROS                        
135900                                                                          
136000     IF W1-SRAD-KDSATAND       = 'B' OR W1-SRAD-REBEART = ZERO            
136100         CONTINUE                                                         
136200      ELSE                                                                
136300         ADD +1                TO W1-KVRADER                              
136400         COMPUTE W1-VLARTNTO   = W1-VLARTNTO +                            
136500                              (W1-SRAD-VLARTNTO * W1-SRAD-REBEART)        
136600         COMPUTE W1-VKARTNTO   = W1-VKARTNTO +                            
136700                              (W1-SRAD-VKARTNTO * W1-SRAD-REBEART)        
136800     END-IF                                                               
136900                                                                          
137000     MOVE W1-SRAD-WDJ211       TO SRAD-WDJ211                             
137100     PERFORM IMS-REPL-SATG1-SATG11                                        
137200                                                                          
137300     PERFORM S10-SKAPA-MINSK-RZD-A-TRANS                                  
137400     .                                                                    
137500     EJECT                                                                
137600 HAAB-UPPD-ARTC11               SECTION.                                  
137700                                                                          
137800     PERFORM IMS-GHU-ARTC1-ARTC11                                         
137900                                                                          
138000     COMPUTE CLAG-KVRESS       =  CLAG-KVRESS - W-OLD-KVSATRES            
138100     COMPUTE CLAG-KVRESS       =  CLAG-KVRESS + SRAD-KVSATRES             
138200                                                                          
138300     PERFORM IMS-REPL-ARTC1-ARTC11                                        
138400     .                                                                    
138500     EJECT                                                                
138600 HAB-UPPD-SATS-ART              SECTION.                                  
138700                                                                          
138800     PERFORM IMS-GHU-SATG1-SATG01                                         
138900     MOVE SHUV-WDJ201          TO  W1-SHUV-WDJ201                         
139000                                                                          
139100     MOVE JA                   TO  W1-SHUV-FLBYGGB                        
139200     MOVE W-MID-KVDELA-UPDATE  TO  W1-SHUV-KVBEART                        
139300     IF W-MID-KVDELA-UPDATE         <   W1-SHUV-KVBYGGB                   
139400         MOVE W-MID-KVDELA-UPDATE   TO  W1-SHUV-KVBYGGB                   
139500     END-IF                                                               
139600                                                                          
139700     COMPUTE  W1-SHUV-VLORDNTO ROUNDED = W1-VLARTNTO / 1000000            
139800     COMPUTE  W1-SHUV-VKORDNTO ROUNDED = W1-VKARTNTO                      
139900                                                                          
140000     PERFORM S02-BERAKNA-PTID                                             
140100     MOVE W1-SHUV-WDJ201       TO  SHUV-WDJ201                            
140200     PERFORM IMS-REPL-SATG1-SATG01                                        
140300     .                                                                    
140400     EJECT                                                                
140500 HB-UPPD-DELA-ANNULL-EJ-REST    SECTION.                                  
140600                                                                          
140700     MOVE W-IDORDNSB           TO W-BYGGB-IDORDNSB                        
140800     MOVE W-IDORDNSS           TO W-BYGGB-IDORDNSS                        
140900                                                                          
141000     IF KOMB-KDSATKMB (1)      = SPACE                                    
141100         CONTINUE                                                         
141200      ELSE                                                                
141300         PERFORM S04-BEHANDLA-KDSATKMB                                    
141400     END-IF                                                               
141500                                                                          
141600     PERFORM HBA-BEHANDLA-NY-SATG01                                       
141700                                                                          
141800     PERFORM HBB-BEHANDLA-ING-ART                                         
141900     PERFORM HBC-UPPD-GAMMAL-SATG01                                       
142000     IF W2-SHUV-KDSATKMB       = 'A'                                      
142100         CONTINUE                                                         
142200      ELSE                                                                
142300         PERFORM HBD-KOLLA-KVBYGGB-KOMB                                   
142400     END-IF                                                               
142500     PERFORM HBE-UPPD-NY-SATG01                                           
142600     .                                                                    
142700     EJECT                                                                
142800 HBA-BEHANDLA-NY-SATG01   SECTION.                                        
142900                                                                          
143000     MOVE W1-SHUV-IDORDNSB     TO W-NY-IDORDNSB                           
143100     COMPUTE W-NY-IDORDNSS     =  W-BYGGB-IDORDNSS + 1                    
143200                                                                          
143300     PERFORM HBAA-ISRT-NY-SATG01                                          
143400     .                                                                    
143500     EJECT                                                                
143600 HBAA-ISRT-NY-SATG01         SECTION.                                     
143700                                                                          
143800     MOVE W1-SHUV-WDJ201       TO  W2-SHUV-WDJ201                         
143900     MOVE W-NY-IDORDNSS        TO  W2-SHUV-IDORDNSS                       
144000                                                                          
144100     MOVE 'R'                  TO W2-SHUV-KDSATSTA                        
144200                                                                          
144300     COMPUTE W2-SHUV-KVBEART   =  W-KVBEART - W-MID-KVDELA-UPDATE         
144400                                                                          
144500     MOVE W2-SHUV-WDJ201       TO  SHUV-WDJ201                            
144600     PERFORM IMS-ISRT-SATG2-SATG01                                        
144700     PERFORM UNTIL SEGMENT-FINNS                                          
144800         ADD +1                TO  W-NY-IDORDNSS                          
144900                                   W2-SHUV-IDORDNSS                       
145000         MOVE W2-SHUV-WDJ201   TO  SHUV-WDJ201                            
145100         PERFORM IMS-ISRT-SATG2-SATG01                                    
145200     END-PERFORM                                                          
145300     .                                                                    
145400     EJECT                                                                
145500 HBB-BEHANDLA-ING-ART           SECTION.                                  
145600                                                                          
145700     MOVE ZERO                 TO W1-KVRADER                              
145800                                  W2-KVRADER                              
145900     MOVE 9999999              TO W-MAX-KVBYGGB                           
146000     PERFORM IMS-GHNP-SATG1-SATG11                                        
146100                                                                          
146200     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
146300         MOVE SRAD-WDJ211      TO W1-SRAD-WDJ211                          
146400         MOVE W1-SRAD-REBEART  TO W-REBEART                               
146500         MOVE W1-SRAD-IDARTNR  TO W-IDARTNR                               
146600                               IN W-IDARTNR-X                             
146700         PERFORM HBBA-UPPDATERA-GAMLA-SATG11                              
146800         PERFORM HBBB-SKAPA-NYA-SATG11                                    
146900         PERFORM HBBC-UPPDATERA-ORDP01                                    
147000         PERFORM IMS-GHNP-SATG1-SATG11                                    
147100     END-PERFORM                                                          
147200     .                                                                    
147300     EJECT                                                                
147400 HBBA-UPPDATERA-GAMLA-SATG11    SECTION.                                  
147500                                                                          
147600**  OBS KVSATRES OCH REBEART AVRUNDAS ALLTID UPP TILL                     
147700**      NÄRMASTE HELTAL                                                   
147800                                                                          
147900     IF W1-SRAD-KVSATRES       =  ZERO AND                                
148000        W1-SRAD-KVSATROS       =  ZERO                                    
148100         MOVE ZERO             TO W1-SRAD-REBEART                         
148200                                  W-KVSATRES                              
148300                                  W-KVSATROS                              
148400      ELSE                                                                
148500         MOVE W1-SRAD-KVSATRES TO W-KVSATRES                              
148600         MOVE W1-SRAD-KVSATROS TO W-KVSATROS                              
148700         IF W1-SRAD-KDSATKMB   =  SPACE                                   
148800             COMPUTE W-HELP-REBEART = (W1-SRAD-REANTPSA *                 
148900                                   W-MID-KVDELA-UPDATE) + 0.999           
149000             MOVE W-HELP-REBEART TO W1-SRAD-REBEART                       
149100          ELSE                                                            
149200             PERFORM S05-KOLLA-KDSATKMB-TAB                               
149300             MOVE KOMB-NY-REBEART (IDX1, IDX2) TO W1-SRAD-REBEART         
149400         END-IF                                                           
149500     END-IF                                                               
149600                                                                          
149700     IF W1-SRAD-KDSATAND       =  'B'                                     
149800         MOVE ZERO             TO W1-SRAD-KVSATRES                        
149900      ELSE                                                                
150000         MOVE W1-SRAD-REBEART  TO W1-SRAD-KVSATRES                        
150100     END-IF                                                               
150200     MOVE ZERO                 TO W1-SRAD-KVSATROS                        
150300                                                                          
150400     IF W1-SRAD-KDSATAND       = 'B' OR W1-SRAD-REBEART = ZERO            
150500         CONTINUE                                                         
150600      ELSE                                                                
150700         ADD +1                TO W1-KVRADER                              
150800         COMPUTE W1-VLARTNTO   = W1-VLARTNTO +                            
150900                              (W1-SRAD-VLARTNTO * W1-SRAD-REBEART)        
151000         COMPUTE W1-VKARTNTO   = W1-VKARTNTO +                            
151100                              (W1-SRAD-VKARTNTO * W1-SRAD-REBEART)        
151200     END-IF                                                               
151300                                                                          
151400     MOVE W1-SRAD-WDJ211       TO SRAD-WDJ211                             
151500     PERFORM IMS-REPL-SATG1-SATG11                                        
151600     .                                                                    
151700     EJECT                                                                
151800 HBBB-SKAPA-NYA-SATG11     SECTION.                                       
151900                                                                          
152000**   REBEART AVRUNDAS ALLTID UPPÅT TILL NÄRMASTE HELTAL                   
152100                                                                          
152200     MOVE W-NY-IDORDNSB        TO W-IDORDNSB                              
152300     MOVE W-NY-IDORDNSS        TO W-IDORDNSS                              
152400                                                                          
152500     MOVE SRAD-WDJ211          TO W2-SRAD-WDJ211                          
152600     MOVE NEJ                  TO W2-SRAD-FLSATUTS                        
152700                                                                          
152800     IF W2-SRAD-KDSATAND       = 'B'                                      
152900         CONTINUE                                                         
153000      ELSE                                                                
153100         COMPUTE W-HELP-REBEART = (W2-SHUV-KVBEART *                      
153200                                       W2-SRAD-REANTPSA) + 0.999          
153300         IF W2-SRAD-KDSATKMB   =  SPACE                                   
153400             MOVE W-HELP-REBEART TO W2-SRAD-REBEART                       
153500          ELSE                                                            
153600             COMPUTE W2-SRAD-REBEART = W-REBEART -                        
153700                                       W1-SRAD-REBEART                    
153800         END-IF                                                           
153900                                                                          
154000         COMPUTE W-TOT-REBEART = W1-SRAD-REBEART +                        
154100                                 W2-SRAD-REBEART                          
154200         IF W-TOT-REBEART      > W-REBEART                                
154300             COMPUTE W-RZD-OKNING = W-TOT-REBEART -                       
154400                                    W-REBEART                             
154500             PERFORM S11-SKAPA-OKNING-RZD-O-TRANS                         
154600             PERFORM HBBBA-BEHANDLA-REBEART-OKNING                        
154700          ELSE                                                            
154800             COMPUTE W2-SRAD-KVSATRES = W-KVSATRES -                      
154900                                        W1-SRAD-KVSATRES                  
155000             COMPUTE W2-SRAD-KVSATROS = W2-SRAD-REBEART -                 
155100                                        W2-SRAD-KVSATRES                  
155200         END-IF                                                           
155300                                                                          
155400         IF W2-SRAD-REBEART = ZERO                                        
155500             CONTINUE                                                     
155600          ELSE                                                            
155700             ADD +1            TO W2-KVRADER                              
155800             COMPUTE W2-VLARTNTO = W2-VLARTNTO +                          
155900                              (W2-SRAD-VLARTNTO * W2-SRAD-REBEART)        
156000             COMPUTE W2-VKARTNTO = W2-VKARTNTO +                          
156100                              (W2-SRAD-VKARTNTO * W2-SRAD-REBEART)        
156200         END-IF                                                           
156300                                                                          
156400         PERFORM HBBBB-BERAKNA-KVBYGGB                                    
156500     END-IF                                                               
156600                                                                          
156700     MOVE W2-SRAD-WDJ211       TO SRAD-WDJ211                             
156800     PERFORM IMS-ISRT-SATG2-SATG11                                        
156900                                                                          
157000     .                                                                    
157100     EJECT                                                                
157200 HBBBA-BEHANDLA-REBEART-OKNING  SECTION.                                  
157300                                                                          
157400     PERFORM IMS-GHU-ARTC1-ARTC11                                         
157500                                                                          
157600     PERFORM HBBBAA-KOLLA-SALDO                                           
157700                                                                          
157800     IF W-TILLG-KVLS           >=  W-RZD-OKNING                           
157900         COMPUTE CLAG-KVRESS    =  CLAG-KVRESS + W-RZD-OKNING             
158000         COMPUTE W2-SRAD-KVSATRES = W-KVSATRES -                          
158100                                    W1-SRAD-KVSATRES +                    
158200                                    W-RZD-OKNING                          
158300         COMPUTE W2-SRAD-KVSATROS = W2-SRAD-REBEART -                     
158400                                    W2-SRAD-KVSATRES                      
158500      ELSE                                                                
158600                                                                          
158700         MOVE W-RZD-OKNING     TO  W-KVRO                                 
158800                                   W-KVART                                
158900                                                                          
159000         PERFORM HBBBAB-SKAPA-RESTORDER                                   
159100                                                                          
159200         COMPUTE CLAG-KVROS    = CLAG-KVROS    + W-RZD-OKNING             
159300         COMPUTE W2-SRAD-KVSATRES = W-KVSATRES -                          
159400                                    W1-SRAD-KVSATRES                      
159500         COMPUTE W2-SRAD-KVSATROS = W2-SRAD-REBEART -                     
159600                                    W2-SRAD-KVSATRES                      
159700     END-IF                                                               
159800                                                                          
159900     PERFORM IMS-REPL-ARTC1-ARTC11                                        
160000     .                                                                    
160100     EJECT                                                                
160200 HBBBAA-KOLLA-SALDO            SECTION.                                   
160300                                                                          
160400     MOVE ZERO                 TO W-KVPREAVB                              
160500     PERFORM IMS-GU-ARTM01                                                
160600     IF SEGMENT-FINNS                                                     
160700         COMPUTE W-KVPREAVB    =  ART-KVPREAVB-DAG +                      
160800                                  ART-KVPREAVB-VOR                        
160900      ELSE                                                                
161000         MOVE ZERO             TO W-KVPREAVB                              
161100     END-IF                                                               
161200                                                                          
161300     COMPUTE W-TILLG-KVLS      =  CLAG-KVLS -                             
161400                                  CLAG-KVRESS -                           
161500                                  CLAG-KVSPANT -                          
161600                                  CLAG-KVUTRS    -                        
161700                                  W-KVPREAVB                              
161800     .                                                                    
161900     EJECT                                                                
162000 HBBBAB-SKAPA-RESTORDER        SECTION.                                   
162100                                                                          
162200     ACCEPT W-DATUM            FROM DATE                                  
162300     ACCEPT W-TID              FROM TIME                                  
162400     MOVE FUNCTION CURRENT-DATE (1:8) TO W-DATUM-Y2K                      
162500                                                                          
162600     MOVE W2-SHUV-IDDISTR      TO RAD-IDDISTR                             
162700     MOVE W2-SHUV-IDKUNDNR     TO RAD-IDKUNDNR                            
162800     MOVE SPACE                TO RAD-IDKUNDRF                            
162900     MOVE W2-SHUV-IDORDNSB     TO RAD-IDORDNR5                            
163000     MOVE RAD-IDKUNDRF (2:4)   TO RAD-IDKUNDRF (1:4)                      
163100     MOVE W2-SHUV-IDORDNSS     TO RAD-IDKUNDRF (5:1)                      
163200     MOVE W2-SRAD-IDARTNR      TO RAD-IDARTNR                             
163300     MOVE 1                    TO RAD-IDLOPNR                             
163400     MOVE SPACE                TO RAD-BEKUNDRF                            
163500     MOVE SPACE                TO RAD-BERADREF                            
163600     MOVE NEJ                  TO RAD-FLERS                               
163700     MOVE W2-SHUV-IDKONTO      TO RAD-IDKONTO                             
163800     MOVE W2-SHUV-IDKST        TO RAD-IDKST                               
163900     MOVE SPACE                TO RAD-IDKUNDRF-LEV                        
164000     MOVE WC-CDC-SE            TO RAD-IDDC                                
164100     MOVE ZERO                 TO RAD-KDDSP                               
164200     MOVE W2-SHUV-KDFAKTYP     TO RAD-KDFAKTYP                            
164300     MOVE ZERO                 TO RAD-KDFRAKT                             
164400     MOVE ZERO                 TO RAD-KDKVBRYT                            
164500     MOVE 1                    TO RAD-KDORDING                            
164600     MOVE W2-SHUV-KDORDKL      TO RAD-KDORDKL                             
164700     MOVE W2-SRAD-KDPRODSL     TO RAD-KDPRODSL                            
164800     MOVE W-KVRO               TO RAD-KVRO                                
164900     MOVE '2'                  TO RAD-KDSTARAD                            
165000     MOVE ZERO                 TO RAD-KDTPOTYP                            
165100     MOVE ZERO                 TO RAD-KDVRINFO                            
165200     MOVE W-KVART              TO RAD-KVART                               
165300     MOVE ZERO                 TO RAD-PRARTNTO                            
165400     MOVE W2-SRAD-REKSIFFR     TO RAD-REKSIFFR                            
165500     MOVE ZERO                 TO RAD-TIAVBOKN                            
165600     MOVE W2-SHUV-DAREGDAT (3:6) TO RAD-TIREGDAT                          
165700     MOVE ZERO                 TO RAD-TIRES                               
165800     MOVE W-DATUM-Y2K          TO RAD-DARODAT                             
165900     MOVE ZERO                 TO RAD-TITPO                               
166000     MOVE SPACE                TO RAD-KDPRTYP                             
166100     MOVE SPACE                TO RAD-BEVOLREF                            
166200     MOVE NEJ                  TO RAD-FLINVEST                            
166300     MOVE NEJ                  TO RAD-FLPRTILL                            
166400     MOVE JA                   TO RAD-FLTPOBEK                            
166500     MOVE ZERO                 TO RAD-IDKAMPRF                            
166600     MOVE SPACE                TO RAD-IDLEVNR                             
166700     MOVE SPACE                TO RAD-IDDC-RO                             
166800     MOVE 'SATS'               TO RAD-IDSYSTEM                            
166900     MOVE W2-SRAD-REBEART      TO RAD-KVBEART-Q                           
167000     MOVE W-TID (1:6)          TO RAD-TIREGTID                            
167100     MOVE 999                  TO RAD-DASENDAT                            
167200                                  RAD-TISENBEK-KL                         
167300*    IF ING-IDARTNR-SPARRAD                                               
167400*        MOVE W-KDROO          TO RAD-KDROO                               
167500*     ELSE                                                                
167600         MOVE W-KDROO-1        TO RAD-KDROO                               
167700*    END-IF                                                               
167800     INITIALIZE                   RAD-DEAL-PR-LINE                        
167900                                                                          
168000     MOVE SPACE                TO RAD-KDORDTYP-LDC                        
168100     MOVE ZERO                 TO RAD-TIREPDAT                            
168200     MOVE SPACE                TO RAD-IDKUNDRF-WIP                        
168400     MOVE +0                   TO RAD-PRAVCOST                            
168500                                                                          
168600     PERFORM HBBBABA-HAMTA-PRIORITETSKOD                                  
168700     PERFORM HBBBABB-HAMTA-ANSKAFFARE                                     
168800                                                                          
168900     PERFORM IMS-ISRT-OWDP2-ORDP01                                        
169000                                                                          
169100     PERFORM UNTIL SEGMENT-FINNS                                          
169200        ADD 1                  TO RAD-IDLOPNR                             
169300        PERFORM IMS-ISRT-OWDP2-ORDP01                                     
169400     END-PERFORM                                                          
169500     .                                                                    
169600     EJECT                                                                
169700 HBBBABA-HAMTA-PRIORITETSKOD SECTION.                                     
169800                                                                          
169900     MOVE RAD-KDTPOTYP         TO W-4512-KDTPOTYP                         
170000     MOVE W2-SHUV-KDORDKL      TO W-4512-KDORDKL                          
170100     MOVE W2-SHUV-IDDISTR      TO W-4512-IDDISTR-FOM                      
170200                                  W-4512-IDDISTR-TOM                      
170300                                                                          
170400     PERFORM IMS-GU-XXJN11                                                
170500     MOVE 4512-KDRAPRIO        TO RAD-KDRAPRIO                            
170600     .                                                                    
170700     EJECT                                                                
170800 HBBBABB-HAMTA-ANSKAFFARE SECTION.                                        
170900                                                                          
171000     PERFORM IMS-GU-ARTC2-ARTC11                                          
171100     MOVE C2-CLAG-IDANSK       TO RAD-IDANSK                              
171200     .                                                                    
171300     EJECT                                                                
171400 HBBBB-BERAKNA-KVBYGGB          SECTION.                                  
171500                                                                          
171600     IF W2-SRAD-KDSATAND       =  'B'  OR                                 
171700        (W2-SRAD-KVSATRES      =  ZERO AND                                
171800         W2-SRAD-KVSATROS      =  ZERO)                                   
171900         CONTINUE                                                         
172000      ELSE                                                                
172100         IF W2-SRAD-KDSATKMB   = SPACE                                    
172200             COMPUTE W-KVBYGGB = W2-SRAD-KVSATRES /                       
172300                                 W2-SRAD-REANTPSA                         
172400             IF W-KVBYGGB      <  W-MAX-KVBYGGB                           
172500                 MOVE W-KVBYGGB TO W-MAX-KVBYGGB                          
172600             END-IF                                                       
172700         END-IF                                                           
172800     END-IF                                                               
172900     .                                                                    
173000     EJECT                                                                
173100 HBBC-UPPDATERA-ORDP01          SECTION.                                  
173200                                                                          
173300     MOVE LOW-VALUE            TO W-WDA501KY-MIN-X                        
173400     MOVE HIGH-VALUE           TO W-WDA501KY-MAX-X                        
173500                                                                          
173600     MOVE W1-SHUV-IDDISTR      TO W-A501-MIN-IDDISTR                      
173700                                  W-A501-MAX-IDDISTR                      
173800                                                                          
173900     MOVE W1-SHUV-IDKUNDNR     TO W-A501-MIN-IDKUNDNR                     
174000                                  W-A501-MAX-IDKUNDNR                     
174100                                                                          
174200     MOVE SPACE                TO W-A501-MIN-IDKUNDRF                     
174300                                  W-A501-MAX-IDKUNDRF                     
174400                                                                          
174500     MOVE WS-IDORDNST          TO W-A501-MIN-IDORDNR5                     
174600                                  W-A501-MAX-IDORDNR5                     
174700                                                                          
174800     MOVE W1-SRAD-IDARTNR      TO W-A501-MIN-IDARTNR                      
174900                                  W-A501-MAX-IDARTNR                      
175000                                                                          
175100                                                                          
175200     MOVE SPACE                TO RAD-KDORDTYP-LDC                        
175300     MOVE ZERO                 TO RAD-TIREPDAT                            
175400     MOVE SPACE                TO RAD-IDKUNDRF-WIP                        
175600                                                                          
175700     PERFORM IMS-GHU-OWDP1-ORDP01                                         
175800     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
175900         PERFORM IMS-DLET-OWDP1-ORDP01                                    
176000         MOVE W2-SHUV-IDORDNSS TO RAD-IDKUNDRF (5:1)                      
176100         PERFORM IMS-ISRT-OWDP2-ORDP01                                    
176200         PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                            
176300             ADD +1            TO RAD-IDLOPNR                             
176400             PERFORM IMS-ISRT-OWDP2-ORDP01                                
176500         END-PERFORM                                                      
176600         PERFORM IMS-GHN-OWDP1-ORDP01                                     
176700     END-PERFORM                                                          
176800     .                                                                    
176900     EJECT                                                                
177000 HBC-UPPD-GAMMAL-SATG01        SECTION.                                   
177100                                                                          
177200     MOVE W1-SHUV-IDORDNSB     TO  W-IDORDNSB                             
177300     MOVE W1-SHUV-IDORDNSS     TO  W-IDORDNSS                             
177400     PERFORM IMS-GHU-SATG1-SATG01                                         
177500                                                                          
177600     MOVE JA                   TO  W1-SHUV-FLBYGGB                        
177700     MOVE W-MID-KVDELA-UPDATE  TO  W1-SHUV-KVBEART                        
177800                                                                          
177900     COMPUTE  W1-SHUV-VLORDNTO ROUNDED = W1-VLARTNTO / 1000000            
178000     COMPUTE  W1-SHUV-VKORDNTO ROUNDED = W1-VKARTNTO                      
178100                                                                          
178200     IF W-MID-KVDELA-UPDATE    <   W1-SHUV-KVBYGGB                        
178300         MOVE W-MID-KVDELA-UPDATE  TO W1-SHUV-KVBYGGB                     
178400     END-IF                                                               
178500                                                                          
178600     PERFORM S02-BERAKNA-PTID                                             
178700                                                                          
178800     MOVE W1-SHUV-WDJ201       TO  SHUV-WDJ201                            
178900     PERFORM IMS-REPL-SATG1-SATG01                                        
179000     .                                                                    
179100     EJECT                                                                
179200 HBD-KOLLA-KVBYGGB-KOMB  SECTION.                                         
179300                                                                          
179400     MOVE W-NY-IDORDNSB        TO  W-IDORDNSB                             
179500     MOVE W-NY-IDORDNSS        TO  W-IDORDNSS                             
179600                                                                          
179700     PERFORM IMS-GU-SATG2-SATG11                                          
179800     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
179900         MOVE SRAD-WDJ211      TO W2-SRAD-WDJ211                          
180000         IF W2-SRAD-KDSATKMB   = SPACE OR                                 
180100            W2-SRAD-KDSATAND   = 'B'   OR                                 
180200           (W2-SRAD-KVSATRES   = ZERO  AND                                
180300            W2-SRAD-KVSATROS   = ZERO)                                    
180400             CONTINUE                                                     
180500          ELSE                                                            
180600             PERFORM HBDA-BEHANDLA-KOMB-KVBYGGB                           
180700         END-IF                                                           
180800         PERFORM IMS-GN-SATG2-SATG11                                      
180900     END-PERFORM                                                          
181000     .                                                                    
181100     EJECT                                                                
181200 HBDA-BEHANDLA-KOMB-KVBYGGB       SECTION.                                
181300                                                                          
181400     SET IDX3                  TO +1                                      
181500     MOVE NEJ                  TO KVBYGGB-TRAFF-SW                        
181600     PERFORM UNTIL IDX3        > 26 OR KVBYGGB-TRAFF OR                   
181700       KVBYGGB-KDSATKMB (IDX3) = SPACE                                    
181800         IF W2-SRAD-KDSATKMB   =  KVBYGGB-KDSATKMB(IDX3)                  
181900             MOVE JA           TO KVBYGGB-TRAFF-SW                        
182000          ELSE                                                            
182100             SET IDX3 UP BY +1                                            
182200         END-IF                                                           
182300     END-PERFORM                                                          
182400                                                                          
182500     IF KVBYGGB-TRAFF                                                     
182600         CONTINUE                                                         
182700      ELSE                                                                
182800         PERFORM HBDAA-BEHANDLA-NY-KDSATKMB                               
182900     END-IF                                                               
183000     .                                                                    
183100     EJECT                                                                
183200 HBDAA-BEHANDLA-NY-KDSATKMB        SECTION.                               
183300                                                                          
183400     MOVE W2-SRAD-IDARTNR          TO W-ING-IDARTNR                       
183500     COMPUTE KVBYGGB-KVBYGGB(IDX3) =  W2-SRAD-KVSATRES /                  
183600                                      W2-SRAD-REANTPSA                    
183700     MOVE W2-SRAD-KDSATKMB         TO KVBYGGB-KDSATKMB(IDX3)              
183800                                      W-KDSATKMB-MIN                      
183900                                      W-KDSATKMB-MAX                      
184000                                                                          
184100     PERFORM IMS-GU-SATG3-SATG11                                          
184200     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
184300         IF SRAD-KVSATRES      >  ZERO OR                                 
184400            SRAD-KVSATROS      >  ZERO                                    
184500             COMPUTE KVBYGGB-KVBYGGB(IDX3) =                              
184600                     KVBYGGB-KVBYGGB(IDX3) +                              
184700                    (SRAD-KVSATRES / SRAD-REANTPSA)                       
184800         END-IF                                                           
184900         PERFORM IMS-GN-SATG3-SATG11                                      
185000     END-PERFORM                                                          
185100     .                                                                    
185200     EJECT                                                                
185300 HBE-UPPD-NY-SATG01     SECTION.                                          
185400                                                                          
185500     MOVE W-NY-IDORDNSB        TO  W-IDORDNSB                             
185600     MOVE W-NY-IDORDNSS        TO  W-IDORDNSS                             
185700                                                                          
185800     PERFORM IMS-GHU-SATG2-SATG01                                         
185900     MOVE SHUV-WDJ201          TO  W2-SHUV-WDJ201                         
186000                                                                          
186100     PERFORM HBEA-UPPD-NY                                                 
186200     .                                                                    
186300     EJECT                                                                
186400 HBEA-UPPD-NY                   SECTION.                                  
186500                                                                          
186600     SET IDX3                  TO +1                                      
186700     PERFORM UNTIL IDX3        > 26 OR                                    
186800        KVBYGGB-KDSATKMB(IDX3) = SPACE                                    
186900         MOVE KVBYGGB-KVBYGGB(IDX3) TO W-KVBYGGB                          
187000         IF W-KVBYGGB               <  W-MAX-KVBYGGB                      
187100             MOVE W-KVBYGGB TO W-MAX-KVBYGGB                              
187200         END-IF                                                           
187300         SET IDX3 UP BY +1                                                
187400     END-PERFORM                                                          
187500                                                                          
187600     COMPUTE  W2-SHUV-VLORDNTO ROUNDED = W2-VLARTNTO / 1000000            
187700     COMPUTE  W2-SHUV-VKORDNTO ROUNDED = W2-VKARTNTO                      
187800     MOVE ZERO                 TO W2-SHUV-IDPRODNR                        
187900                                                                          
188000     MOVE W-MAX-KVBYGGB        TO W2-SHUV-KVBYGGB                         
188100     IF W2-SHUV-KVBYGGB        <  W2-SHUV-KVBEART                         
188200         MOVE NEJ              TO W2-SHUV-FLBYGGB                         
188300      ELSE                                                                
188400         MOVE JA               TO W2-SHUV-FLBYGGB                         
188500         PERFORM HBEAA-BERAKNA-PTID                                       
188600     END-IF                                                               
188700                                                                          
188800     MOVE W2-SHUV-WDJ201       TO SHUV-WDJ201                             
188900     PERFORM IMS-REPL-SATG2-SATG01                                        
189000     .                                                                    
189100     EJECT                                                                
189200 HBEAA-BERAKNA-PTID             SECTION.                                  
189300                                                                          
189400     MOVE 'SATS'               TO  PTID-IDSYSTEM                          
189500     MOVE W2-SHUV-IDORDNSB     TO  PTID-IDORDNSB                          
189600     MOVE W2-SHUV-IDORDNSS     TO  PTID-IDORDNSS                          
189700     MOVE W2-SHUV-IDARTNR      TO  PTID-IDARTNR                           
189800     MOVE W2-SHUV-IDPRC        TO  PTID-IDPRC                             
189900     MOVE W2-SHUV-KDCLAGER     TO  PTID-KDCLAGER                          
190000     MOVE W2-SHUV-KVBYGGB      TO  PTID-KVBYGGB                           
190100     MOVE W2-KVRADER           TO  PTID-KVRADER                           
190200                                   PTID-KVANTART                          
190300     MOVE ZERO                 TO  PTID-SUSATPTI                          
190400     MOVE W2-SHUV-VKORDNTO     TO  PTID-VKORDNTO                          
190500     MOVE W2-SHUV-VLORDNTO     TO  PTID-VLORDNTO                          
190600     MOVE SPACE                TO  PTID-KDSVAR                            
190700                                                                          
190800     CALL W416PTID USING PTID-W416PTID XXKH-PCB XXKI-PCB                  
190900                                       SATB-PCB                           
191000                                                                          
191100     IF PTID-KDSVAR            =   ZERO                                   
191200         MOVE PTID-SUSATPTI    TO  W2-SHUV-SUSATPTI                       
191300      ELSE                                                                
191400         MOVE ZERO             TO  W2-SHUV-SUSATPTI                       
191500     END-IF                                                               
191600     .                                                                    
191700     EJECT                                                                
191800 HC-UPPD-ANNULL-HELA            SECTION.                                  
191900                                                                          
192000     PERFORM IMS-GHNP-SATG1-SATG11                                        
192100     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
192200         IF SRAD-REBEART       = ZERO AND                                 
192300            SRAD-KVSATRES      = ZERO AND                                 
192400            SRAD-KVSATROS      = ZERO                                     
192500             CONTINUE                                                     
192600          ELSE                                                            
192700             MOVE SRAD-IDARTNR TO W-IDARTNR                               
192800                               IN W-IDARTNR-X                             
192900             PERFORM HCA-UPPD-ARTC11                                      
193000             PERFORM S01-UPPD-ORDP01-ARTC11                               
193100             MOVE SRAD-REBEART TO W-RZD-MINSKNING                         
193200             PERFORM S10-SKAPA-MINSK-RZD-A-TRANS                          
193300         END-IF                                                           
193400         PERFORM IMS-GHNP-SATG1-SATG11                                    
193500     END-PERFORM                                                          
193600                                                                          
193700     PERFORM IMS-GHU-SATG1-SATG01                                         
193800     PERFORM IMS-DLET-SATG1-SATG01                                        
193900     .                                                                    
194000     EJECT                                                                
194100 HCA-UPPD-ARTC11                SECTION.                                  
194200                                                                          
194300     PERFORM IMS-GHU-ARTC1-ARTC11                                         
194400                                                                          
194500     COMPUTE CLAG-KVRESS       =  CLAG-KVRESS - SRAD-KVSATRES             
194600                                                                          
194700     PERFORM IMS-REPL-ARTC1-ARTC11                                        
194800     .                                                                    
194900     EJECT                                                                
195000 HD-UPPD-MAN-PRIO               SECTION.                                  
195100                                                                          
195200     MOVE 2302-MID-FLSATPRI-UPDATE  TO W1-SHUV-FLSATPRI                   
195300     MOVE W1-SHUV-WDJ201            TO SHUV-WDJ201                        
195400                                                                          
195500     PERFORM IMS-REPL-SATG1-SATG01                                        
195600     .                                                                    
195700     EJECT                                                                
195800 K-PPSW-2109-DISPATCH           SECTION.                                  
195900                                                                          
196000     MOVE +1350                 TO KMSG-KVLL                              
196100     MOVE LOW-VALUE             TO KMSG-KDZ1                              
196200                                   KMSG-KDZ2                              
196300     MOVE 'W2T109X'             TO KMSG-KDTRANS-1                         
196400     MOVE '2302'                TO KMSG-IDTRANS-1                         
196500     MOVE '1'                   TO KMSG-KDMFSFOR-1                        
196600                                                                          
196700     MOVE W-ANTAL-I-MID TO KOM-MID2-KVANTART                              
196800                                                                          
196900     CALL W006KOM USING MSG-PCB                                           
197000                        ALT-PCB                                           
197100                        KOMA-PCB                                          
197200                        MSG-KOM-WMSGKOM                                   
197300                        KMSG-IO-AREA                                      
197400     .                                                                    
197500     EJECT                                                                
197600 L-INITIERA-TRANS-DISPATCH      SECTION.                                  
197700                                                                          
197800     MOVE +54                   TO MSG-KOM-KVLL                           
197900     MOVE LOW-VALUE             TO MSG-KOM-KDZ1                           
198000                                   MSG-KOM-KDZ2                           
198100     MOVE SPACE                 TO MSG-KOM-KDTRANS                        
198200     MOVE 'W2I10902'            TO MSG-KOM-IDCPYTXT                       
198300     MOVE 'ORDERING'            TO MSG-KOM-IDSNDNOD                       
198400     MOVE IDPGM                 TO MSG-KOM-IDSNDJOB                       
198500                                                                          
198600     MOVE SPACE                 TO MSG-KOM-IDMFSMED                       
198700                                   MSG-KOM-KDSVAR                         
198800                                                                          
198900     ACCEPT W-DATUM             FROM DATE                                 
199000     ACCEPT W-TID               FROM TIME                                 
199100     MOVE W-DATUM               TO MSG-KOM-TIREGDAT                       
199200     MOVE W-TID                 TO MSG-KOM-TIKLOCK                        
199300                                                                          
199400     MOVE SPACE                 TO KOM-MID2-W2I10902                      
199500     .                                                                    
199600     EJECT                                                                
199700 S01-UPPD-ORDP01-ARTC11         SECTION.                                  
199800                                                                          
199900     MOVE LOW-VALUE            TO W-WDA501KY-MIN-X                        
200000     MOVE HIGH-VALUE           TO W-WDA501KY-MAX-X                        
200100                                                                          
200200     MOVE W1-SHUV-IDDISTR      TO W-A501-MIN-IDDISTR                      
200300                                  W-A501-MAX-IDDISTR                      
200400                                                                          
200500     MOVE W1-SHUV-IDKUNDNR     TO W-A501-MIN-IDKUNDNR                     
200600                                  W-A501-MAX-IDKUNDNR                     
200700                                                                          
200800     MOVE SPACE                TO W-A501-MIN-IDKUNDRF                     
200900                                  W-A501-MAX-IDKUNDRF                     
201000                                                                          
201100     MOVE WS-IDORDNST          TO W-A501-MIN-IDORDNR5                     
201200                                  W-A501-MAX-IDORDNR5                     
201300                                                                          
201400     MOVE W-IDARTNR            IN W-IDARTNR-X                             
201500                               TO W-A501-MIN-IDARTNR                      
201600                                  W-A501-MAX-IDARTNR                      
201700                                                                          
201800     PERFORM IMS-GHU-ARTC1-ARTC11                                         
201900     PERFORM IMS-GHU-OWDP1-ORDP01                                         
202000     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
202100         IF RAD-KDSTARAD       =  '2'                                     
202200             COMPUTE CLAG-KVROS      =   CLAG-KVROS - RAD-KVART           
202300          ELSE                                                            
202400             IF RAD-KDSTARAD   =  '3'                                     
202500                 COMPUTE CLAG-KVRESS = CLAG-KVRESS -  RAD-KVART           
202600             END-IF                                                       
202700         END-IF                                                           
202800         PERFORM IMS-DLET-OWDP1-ORDP01                                    
202900         PERFORM IMS-GHN-OWDP1-ORDP01                                     
203000     END-PERFORM                                                          
203100     PERFORM IMS-REPL-ARTC1-ARTC11                                        
203200     .                                                                    
203300     EJECT                                                                
203400 S02-BERAKNA-PTID               SECTION.                                  
203500                                                                          
203600     MOVE 'SATS'               TO  PTID-IDSYSTEM                          
203700     MOVE W1-SHUV-IDORDNSB     TO  PTID-IDORDNSB                          
203800     MOVE W1-SHUV-IDORDNSS     TO  PTID-IDORDNSS                          
203900     MOVE W1-SHUV-IDARTNR      TO  PTID-IDARTNR                           
204000     MOVE W1-SHUV-IDPRC        TO  PTID-IDPRC                             
204100     MOVE W1-SHUV-KDCLAGER     TO  PTID-KDCLAGER                          
204200     MOVE W1-SHUV-KVBYGGB      TO  PTID-KVBYGGB                           
204300     MOVE W1-KVRADER           TO  PTID-KVRADER                           
204400                                   PTID-KVANTART                          
204500     MOVE ZERO                 TO  PTID-SUSATPTI                          
204600     MOVE W1-SHUV-VKORDNTO     TO  PTID-VKORDNTO                          
204700     MOVE W1-SHUV-VLORDNTO     TO  PTID-VLORDNTO                          
204800     MOVE SPACE                TO  PTID-KDSVAR                            
204900                                                                          
205000     CALL W416PTID USING PTID-W416PTID XXKH-PCB XXKI-PCB                  
205100                                       SATB-PCB                           
205200                                                                          
205300     IF PTID-KDSVAR            =   ZERO                                   
205400         MOVE PTID-SUSATPTI    TO  W1-SHUV-SUSATPTI                       
205500      ELSE                                                                
205600         MOVE ZERO             TO  W1-SHUV-SUSATPTI                       
205700     END-IF                                                               
205800     .                                                                    
205900     EJECT                                                                
206000 S03-UPPDATERA-LEVPLAN          SECTION.                                  
206100                                                                          
206200     MOVE 'SATS'               TO  LEVP-IDSYSTEM                          
206300     MOVE SHUV-IDARTNR         TO  LEVP-IDARTNR                           
206400     MOVE SHUV-IDLEVNR         TO  LEVP-IDLEVNR                           
206500     MOVE SHUV-IDORDNSB        TO  LEVP-IDORDNSB                          
206600     MOVE SHUV-KDCLAGER        TO  LEVP-KDCLAGER                          
206700     MOVE SHUV-KVBEART         TO  LEVP-KVBEART                           
206800     MOVE W-KVANNANT           TO  LEVP-KVANNANT                          
206900     MOVE SHUV-TIBEGPAC        TO  LEVP-TIBEGPAC                          
207000     MOVE SHUV-DAREGDAT (3:6)  TO  LEVP-TIREGDAT                          
207100     MOVE SPACE                TO  LEVP-KDSVAR                            
207200                                                                          
207300     CALL W215LEVP USING LEVP-W215LEVP ARTC3-PCB INLB1-PCB                
207400                                       INLB2-PCB XXBM-PCB                 
207500                                       XXBW-PCB                           
207600     .                                                                    
207700     EJECT                                                                
207800 S04-BEHANDLA-KDSATKMB          SECTION.                                  
207900                                                                          
208000     SET IDX1                  TO  +1                                     
208100     PERFORM UNTIL IDX1        > +26 OR                                   
208200                   KOMB-KDSATKMB (IDX1) = SPACE                           
208300         SET IDX2              TO  +1                                     
208400         PERFORM UNTIL IDX2    >   +5 OR                                  
208500                       KOMB-KDSATAND (IDX1, IDX2) = 'E'                   
208600             SET IDX2 UP BY +1                                            
208700         END-PERFORM                                                      
208800         COMPUTE W-HELP-REBEART =  (KOMB-REANTPSA (IDX1, IDX2) *          
208900                                    W-MID-KVDELA-UPDATE) +                
209000                                    0.999                                 
209100         IF W-HELP-REBEART     <=  KOMB-REBEART (IDX1, IDX2)              
209200             MOVE W-HELP-REBEART TO KOMB-NY-REBEART(IDX1, IDX2)           
209300          ELSE                                                            
209400             MOVE KOMB-REBEART(IDX1, IDX2) TO                             
209500                                     KOMB-NY-REBEART(IDX1, IDX2)          
209600         END-IF                                                           
209700         COMPUTE KOMB-KVBYGGB (IDX1) =                                    
209800                                 KOMB-NY-REBEART (IDX1, IDX2) /           
209900                                 KOMB-REANTPSA   (IDX1, IDX2)             
210000         COMPUTE KOMB-REST-KVBYGGB (IDX1) = W-MID-KVDELA-UPDATE -         
210100                                            KOMB-KVBYGGB (IDX1)           
210200         IF KOMB-REST-KVBYGGB (IDX1) >  ZERO                              
210300             PERFORM S04A-RAKNA-UT-OVRIGA-REBEART                         
210400         END-IF                                                           
210500         SET IDX1                    UP BY +1                             
210600     END-PERFORM                                                          
210700     .                                                                    
210800     EJECT                                                                
210900 S04A-RAKNA-UT-OVRIGA-REBEART    SECTION.                                 
211000                                                                          
211100     SET IDX2                  TO +1                                      
211200     PERFORM UNTIL IDX2        > 5 OR                                     
211300             KOMB-ING-IDARTNR (IDX1, IDX2) = ZERO                         
211400         IF KOMB-NY-REBEART (IDX1, IDX2)   = ZERO                         
211500             COMPUTE W-HELP-REBEART =                                     
211600                    (KOMB-REANTPSA (IDX1, IDX2) *                         
211700                     KOMB-REST-KVBYGGB (IDX1))   + 0.999                  
211800             IF W-HELP-REBEART <=  KOMB-REBEART (IDX1, IDX2)              
211900                MOVE W-HELP-REBEART TO KOMB-NY-REBEART(IDX1, IDX2)        
212000              ELSE                                                        
212100                 MOVE KOMB-REBEART(IDX1, IDX2) TO                         
212200                                       KOMB-NY-REBEART(IDX1, IDX2)        
212300             END-IF                                                       
212400             COMPUTE KOMB-KVBYGGB (IDX1) =  KOMB-KVBYGGB (IDX1) +         
212500                 (KOMB-NY-REBEART (IDX1, IDX2) /                          
212600                  KOMB-REANTPSA (IDX1, IDX2))                             
212700             COMPUTE KOMB-REST-KVBYGGB (IDX1) =                           
212800                                            W-MID-KVDELA-UPDATE -         
212900                                            KOMB-KVBYGGB (IDX1)           
213000         END-IF                                                           
213100         SET IDX2 UP BY +1                                                
213200     END-PERFORM                                                          
213300     .                                                                    
213400     EJECT                                                                
213500 S05-KOLLA-KDSATKMB-TAB   SECTION.                                        
213600                                                                          
213700     SET IDX1                  TO +1                                      
213800     MOVE NEJ                  TO KOMB-TRAFF-SW                           
213900     PERFORM UNTIL IDX1        > 26 OR KOMB-TRAFF                         
214000         IF W1-SRAD-KDSATKMB   =  KOMB-KDSATKMB (IDX1)                    
214100             SET IDX2          TO +1                                      
214200             PERFORM UNTIL IDX2 > 5 OR KOMB-TRAFF                         
214300                 IF KOMB-ING-IDARTNR (IDX1, IDX2) =                       
214400                    W1-SRAD-IDARTNR                                       
214500                     MOVE JA   TO KOMB-TRAFF-SW                           
214600                  ELSE                                                    
214700                     SET IDX2  UP BY +1                                   
214800                 END-IF                                                   
214900             END-PERFORM                                                  
215000         END-IF                                                           
215100         IF KOMB-TRAFF                                                    
215200             CONTINUE                                                     
215300          ELSE                                                            
215400             SET IDX1          UP BY +1                                   
215500         END-IF                                                           
215600     END-PERFORM                                                          
215700                                                                          
215800     IF KOMB-TRAFF                                                        
215900         CONTINUE                                                         
216000      ELSE                                                                
216100         MOVE 'MISS I TAB SEC HAAAA-' TO FELTEXT                          
216200         CALL ABEND USING ABEND-MED-DUMP                                  
216300     END-IF                                                               
216400     .                                                                    
216500     EJECT                                                                
216600 S10-SKAPA-MINSK-RZD-A-TRANS SECTION.                                     
216700                                                                          
216800*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
216900     MOVE SRAD-IDARTNR       TO BYT03-IDARTNR                             
217000     IF NOT BYT03-OBJEKT                                                  
217100                                                                          
217200      ADD +1 TO W-ANTAL-I-MID                                             
217300                                                                          
217400      IF W-ANTAL-I-MID > MAX-RAD-MID2                                     
217500         MOVE MAX-RAD-MID2 TO W-ANTAL-I-MID                               
217600         PERFORM K-PPSW-2109-DISPATCH                                     
217700         MOVE +1 TO W-ANTAL-I-MID                                         
217800         MOVE NEJ TO SW-PPSW-2109                                         
217900      END-IF                                                              
218000                                                                          
218100      IF SW-PPSW-2109 = NEJ                                               
218200         MOVE JA TO SW-PPSW-2109                                          
218300         PERFORM L-INITIERA-TRANS-DISPATCH                                
218400      END-IF                                                              
218500                                                                          
218600      MOVE SRAD-IDARTNR    TO KOM-MID2-IDARTNR   (W-ANTAL-I-MID)          
218700      MOVE WC-CDC-SE       TO KOM-MID2-IDDC      (W-ANTAL-I-MID)          
218800      MOVE DAT-TIAAMMDD    TO KOM-MID2-TIUPPDAT  (W-ANTAL-I-MID)          
218900      MOVE '-'             TO KOM-MID2-KDTECKEN  (W-ANTAL-I-MID)          
219000      MOVE 'KI'            TO KOM-MID2-KDOI      (W-ANTAL-I-MID)          
219100      MOVE SPACE           TO KOM-MID2-CLEARGROUP(W-ANTAL-I-MID)          
219200      MOVE W-RZD-MINSKNING TO KOM-MID2-KVOI      (W-ANTAL-I-MID)          
219300     END-IF                                                               
219400     .                                                                    
219500      EJECT                                                               
219600 S11-SKAPA-OKNING-RZD-O-TRANS SECTION.                                    
219700                                                                          
219800*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
219900     MOVE SRAD-IDARTNR       TO BYT03-IDARTNR                             
220000     IF NOT BYT03-OBJEKT                                                  
220100                                                                          
220200      ADD +1 TO W-ANTAL-I-MID                                             
220300                                                                          
220400      IF W-ANTAL-I-MID > MAX-RAD-MID2                                     
220500         MOVE MAX-RAD-MID2 TO W-ANTAL-I-MID                               
220600         PERFORM K-PPSW-2109-DISPATCH                                     
220700         MOVE +1 TO W-ANTAL-I-MID                                         
220800         MOVE NEJ TO SW-PPSW-2109                                         
220900      END-IF                                                              
221000                                                                          
221100      IF SW-PPSW-2109 = NEJ                                               
221200         MOVE JA TO SW-PPSW-2109                                          
221300         PERFORM L-INITIERA-TRANS-DISPATCH                                
221400      END-IF                                                              
221500                                                                          
221600      MOVE SRAD-IDARTNR   TO KOM-MID2-IDARTNR   (W-ANTAL-I-MID)           
221700      MOVE WC-CDC-SE      TO KOM-MID2-IDDC      (W-ANTAL-I-MID)           
221800      MOVE DAT-TIAAMMDD   TO KOM-MID2-TIUPPDAT  (W-ANTAL-I-MID)           
221900      MOVE '+'            TO KOM-MID2-KDTECKEN  (W-ANTAL-I-MID)           
222000      MOVE 'KI'           TO KOM-MID2-KDOI      (W-ANTAL-I-MID)           
222100      MOVE SPACE          TO KOM-MID2-CLEARGROUP(W-ANTAL-I-MID)           
222200      MOVE W-RZD-OKNING   TO KOM-MID2-KVOI      (W-ANTAL-I-MID)           
222300     END-IF                                                               
222400     .                                                                    
222500      EJECT                                                               
222600 MFS-RENSA-FAELT-UT SECTION.                                              
222700                                                                          
222800*    --- ALLA UTDATA-FÄLT                                                 
222900*    --- INKL. BLÄDDRINGSNYCKLAR                                          
223000     MOVE MFS-RENSA-FAELT      TO  MOD-IDARTNR                            
223100                                   MOD-KDCLAGER                           
223200                                   MOD-IDPRC                              
223300                                   MOD-BEART                              
223400                                   MOD-KVBEART                            
223500                                   MOD-FLBYGGB                            
223600                                   MOD-KVBYGGB                            
223700                                   MOD-FLSATSPR-HUV                       
223800                                   MOD-FLSATPRI                           
223900                                   MOD-IDARTNR-ENTER                      
224000                                   MOD-IDARTNR-NEXT                       
224100                                                                          
224200     MOVE +1                   TO INDX                                    
224300     PERFORM UNTIL INDX        >  MAX-CLAGER                              
224400         MOVE MFS-RENSA-FAELT  TO MOD-HUV-KDCLAGER (INDX)                 
224500                                  MOD-KVPB-SATS    (INDX)                 
224600                                  MOD-KVROS        (INDX)                 
224700                                  MOD-KVSLAGER     (INDX)                 
224800                                  MOD-KVDISP       (INDX)                 
224900         ADD +1                TO INDX                                    
225000     END-PERFORM                                                          
225100                                                                          
225200     MOVE +1                   TO  INDX                                   
225300     PERFORM UNTIL             INDX > MAX-RAD-2302                        
225400         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
225500         ADD +1                TO  INDX                                   
225600     END-PERFORM                                                          
225700                                                                          
225800     .                                                                    
225900     SKIP2                                                                
226000 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
226100                                                                          
226200*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
226300     MOVE MFS-RENSA-FAELT      TO MOD-ING-IDARTNR-RAD (INDX)              
226400                                  MOD-REANTPSA-RAD    (INDX)              
226500                                  MOD-REBEART-RAD     (INDX)              
226600                                  MOD-KVSATRES-RAD    (INDX)              
226700                                  MOD-KVSATROS-RAD    (INDX)              
226800                                  MOD-FLSATSPR-RAD    (INDX)              
226900                                  MOD-TIDISPIN-RAD    (INDX)              
227000                                  MOD-IDANSK-RAD      (INDX)              
227100     .                                                                    
227200     SKIP2                                                                
227300 MFS-RENSA-FAELT-IN SECTION.                                              
227400                                                                          
227500     MOVE MFS-RENSA-FAELT      TO MOD-KVDELA-UPDATE                       
227600                                  MOD-FLANNULL-HELA-UPDATE                
227700                                  MOD-FLSATPRI-UPDATE                     
227800     .                                                                    
227900     EJECT                                                                
228000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
228100                                                                          
228200     MOVE MFS-ROER-EJ-FAELT    TO  MOD-IDARTNR                            
228300                                   MOD-KDCLAGER                           
228400                                   MOD-IDPRC                              
228500                                   MOD-BEART                              
228600                                   MOD-KVBEART                            
228700                                   MOD-FLBYGGB                            
228800                                   MOD-KVBYGGB                            
228900                                   MOD-FLSATPRI                           
229000                                   MOD-FLSATSPR-HUV                       
229100                                   MOD-IDARTNR-ENTER                      
229200                                   MOD-IDARTNR-NEXT                       
229300                                                                          
229400     MOVE +1                    TO INDX                                   
229500     PERFORM UNTIL INDX         >  MAX-CLAGER                             
229600         MOVE MFS-ROER-EJ-FAELT TO MOD-HUV-KDCLAGER (INDX)                
229700                                   MOD-KVPB-SATS    (INDX)                
229800                                   MOD-KVROS        (INDX)                
229900                                   MOD-KVSLAGER     (INDX)                
230000                                   MOD-KVDISP       (INDX)                
230100         ADD +1                TO INDX                                    
230200     END-PERFORM                                                          
230300                                                                          
230400     MOVE +1                   TO INDX                                    
230500     PERFORM UNTIL INDX        > MAX-RAD-2302                             
230600       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
230700       ADD +1                  TO INDX                                    
230800     END-PERFORM                                                          
230900     .                                                                    
231000     SKIP2                                                                
231100 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
231200                                                                          
231300*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
231400     MOVE MFS-ROER-EJ-FAELT    TO MOD-ING-IDARTNR-RAD   (INDX)            
231500                                  MOD-REANTPSA-RAD      (INDX)            
231600                                  MOD-REBEART-RAD       (INDX)            
231700                                  MOD-KVSATRES-RAD      (INDX)            
231800                                  MOD-KVSATROS-RAD      (INDX)            
231900                                  MOD-FLSATSPR-RAD      (INDX)            
232000                                  MOD-TIDISPIN-RAD      (INDX)            
232100                                  MOD-IDANSK-RAD        (INDX)            
232200     .                                                                    
232300     SKIP2                                                                
232400 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
232500                                                                          
232600*    --- ALLA INDATA-FÄLT                                                 
232700     MOVE MFS-ROER-EJ-FAELT    TO MOD-KVDELA-UPDATE                       
232800                                  MOD-FLANNULL-REST-UPDATE                
232900                                  MOD-FLANNULL-HELA-UPDATE                
233000                                  MOD-FLSATPRI-UPDATE                     
233100     .                                                                    
233200     EJECT                                                                
233300 MFS-FORM-ATTR SECTION.                                                   
233400                                                                          
233500*    --- ALLA INDATA-FÄLT                                                 
233600     MOVE MFS-FORMATETS-ATTR   TO MOD-KVDELA-UPDATE-ATTR                  
233700                                  MOD-FLANNULL-REST-UPDATE-ATTR           
233800                                  MOD-FLANNULL-HELA-UPDATE-ATTR           
233900                                  MOD-FLSATPRI-UPDATE-ATTR                
234000     .                                                                    
234100     SKIP2                                                                
234200 MFS-LAES-IN-IGEN SECTION.                                                
234300                                                                          
234400     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVDELA-UPDATE-ATTR                 
234500                                   MOD-FLANNULL-REST-UPDATE-ATTR          
234600                                   MOD-FLANNULL-HELA-UPDATE-ATTR          
234700                                   MOD-FLSATPRI-UPDATE-ATTR               
234800     .                                                                    
234900     EJECT                                                                
235000* --- IMS SEKTIONER ---                                                   
235100     SKIP3                                                                
235200 IMS-GET-MSG SECTION.                                                     
235300                                                                          
235400     MOVE '  QC' TO GODK-STATUSKODER                                      
235500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
235600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
235700     PERFORM IMS-STATUSKONTROLL                                           
235800     .                                                                    
235900     SKIP3                                                                
236000 IMS-INSERT-MSG SECTION.                                                  
236100                                                                          
236200     IF ENGLISH-TEXT                                                      
236300       MOVE 'N' TO MFS-KDHUVOMR                                           
236400     END-IF                                                               
236500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
236600     MOVE SPACE TO GODK-STATUSKODER                                       
236700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
236800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
236900     PERFORM IMS-STATUSKONTROLL                                           
237000     .                                                                    
237100     EJECT                                                                
237200 IMS-GU-SATG1-SATG01 SECTION.                                             
237300     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
237400          DELIMITED BY SIZE INTO SSA1                                     
237500     MOVE '  GE' TO GODK-STATUSKODER                                      
237600     CALL CBLTDLI USING GU SATG1-PCB DLI-IO-AREA1 SSA1                    
237700     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
237800                               SATG01-STATUS-WS                           
237900     PERFORM IMS-STATUSKONTROLL                                           
238000     .                                                                    
238100     SKIP3                                                                
238200 IMS-GHU-SATG1-SATG01 SECTION.                                            
238300     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
238400          DELIMITED BY SIZE INTO SSA1                                     
238500     MOVE '    ' TO GODK-STATUSKODER                                      
238600     CALL CBLTDLI USING GHU SATG1-PCB DLI-IO-AREA1 SSA1                   
238700     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
238800     PERFORM IMS-STATUSKONTROLL                                           
238900     .                                                                    
239000     SKIP3                                                                
239100 IMS-REPL-SATG1-SATG01 SECTION.                                           
239200                                                                          
239300     MOVE '  ' TO GODK-STATUSKODER                                        
239400     CALL CBLTDLI USING REPL SATG1-PCB DLI-IO-AREA1                       
239500     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
239600     PERFORM IMS-STATUSKONTROLL                                           
239700     .                                                                    
239800     SKIP3                                                                
239900 IMS-DLET-SATG1-SATG01  SECTION.                                          
240000                                                                          
240100     MOVE '  ' TO GODK-STATUSKODER                                        
240200     CALL CBLTDLI USING DLET SATG1-PCB DLI-IO-AREA1                       
240300     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
240400     PERFORM IMS-STATUSKONTROLL                                           
240500     .                                                                    
240600     EJECT                                                                
240700 IMS-GU-SATG1-SATG11 SECTION.                                             
240800     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
240900          DELIMITED BY SIZE INTO SSA1                                     
241000     STRING 'WLSATG11(IDARTNR  =' W-ING-IDARTNR-X ')'                     
241100          DELIMITED BY SIZE INTO SSA2                                     
241200     MOVE '  GE' TO GODK-STATUSKODER                                      
241300     CALL CBLTDLI USING GU SATG1-PCB DLI-IO-AREA2 SSA1 SSA2               
241400     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
241500     PERFORM IMS-STATUSKONTROLL                                           
241600     .                                                                    
241700     SKIP3                                                                
241800 IMS-GN-SATG1-SATG11 SECTION.                                             
241900     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
242000          DELIMITED BY SIZE INTO SSA1                                     
242100     MOVE   'WLSATG11'   TO SSA2                                          
242200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
242300     CALL CBLTDLI USING GN SATG1-PCB DLI-IO-AREA2 SSA1 SSA2               
242400     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
242500     PERFORM IMS-STATUSKONTROLL                                           
242600     .                                                                    
242700     SKIP3                                                                
242800 IMS-GHNP-SATG1-SATG11 SECTION.                                           
242900     MOVE   'WLSATG11'   TO SSA1                                          
243000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
243100     CALL CBLTDLI USING GHNP SATG1-PCB DLI-IO-AREA2 SSA1                  
243200     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
243300     PERFORM IMS-STATUSKONTROLL                                           
243400     .                                                                    
243500     SKIP3                                                                
243600 IMS-REPL-SATG1-SATG11 SECTION.                                           
243700     MOVE '  GE' TO GODK-STATUSKODER                                      
243800     CALL CBLTDLI USING REPL SATG1-PCB DLI-IO-AREA2                       
243900     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
244000     PERFORM IMS-STATUSKONTROLL                                           
244100     .                                                                    
244200     SKIP3                                                                
244300 IMS-GU-SATG2-SATG01 SECTION.                                             
244400     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
244500          DELIMITED BY SIZE INTO SSA1                                     
244600     MOVE '  GE' TO GODK-STATUSKODER                                      
244700     CALL CBLTDLI USING GU SATG2-PCB DLI-IO-AREA1 SSA1                    
244800     MOVE SATG2-STATUS-CODE TO STATUS-WS                                  
244900     PERFORM IMS-STATUSKONTROLL                                           
245000     .                                                                    
245100     SKIP3                                                                
245200 IMS-GHU-SATG2-SATG01 SECTION.                                            
245300     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
245400          DELIMITED BY SIZE INTO SSA1                                     
245500     MOVE '  GE' TO GODK-STATUSKODER                                      
245600     CALL CBLTDLI USING GHU SATG2-PCB DLI-IO-AREA1 SSA1                   
245700     MOVE SATG2-STATUS-CODE TO STATUS-WS                                  
245800     PERFORM IMS-STATUSKONTROLL                                           
245900     .                                                                    
246000     SKIP3                                                                
246100 IMS-REPL-SATG2-SATG01 SECTION.                                           
246200                                                                          
246300     MOVE '  ' TO GODK-STATUSKODER                                        
246400     CALL CBLTDLI USING REPL SATG2-PCB DLI-IO-AREA1                       
246500     MOVE SATG2-STATUS-CODE TO STATUS-WS                                  
246600     PERFORM IMS-STATUSKONTROLL                                           
246700     .                                                                    
246800     EJECT                                                                
246900 IMS-ISRT-SATG2-SATG01 SECTION.                                           
247000                                                                          
247100     MOVE 'WLSATG01 ' TO SSA1                                             
247200     MOVE '  II' TO GODK-STATUSKODER                                      
247300     CALL CBLTDLI USING ISRT SATG2-PCB DLI-IO-AREA1 SSA1                  
247400     MOVE SATG2-STATUS-CODE TO STATUS-WS                                  
247500     PERFORM IMS-STATUSKONTROLL                                           
247600     .                                                                    
247700     EJECT                                                                
247800 IMS-ISRT-SATG2-SATG11 SECTION.                                           
247900                                                                          
248000     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
248100          DELIMITED BY SIZE INTO SSA1                                     
248200     MOVE 'WLSATG11 ' TO SSA2                                             
248300     MOVE '  II' TO GODK-STATUSKODER                                      
248400     CALL CBLTDLI USING ISRT SATG2-PCB DLI-IO-AREA2 SSA1 SSA2             
248500     MOVE SATG2-STATUS-CODE TO STATUS-WS                                  
248600     PERFORM IMS-STATUSKONTROLL                                           
248700     .                                                                    
248800     SKIP3                                                                
248900 IMS-GU-SATG2-SATG11 SECTION.                                             
249000     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
249100          DELIMITED BY SIZE INTO SSA1                                     
249200     MOVE   'WLSATG11'   TO SSA2                                          
249300     MOVE '  GE' TO GODK-STATUSKODER                                      
249400     CALL CBLTDLI USING GU SATG2-PCB DLI-IO-AREA2 SSA1 SSA2               
249500     MOVE SATG2-STATUS-CODE TO STATUS-WS                                  
249600     PERFORM IMS-STATUSKONTROLL                                           
249700     .                                                                    
249800     SKIP3                                                                
249900 IMS-GN-SATG2-SATG11 SECTION.                                             
250000     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
250100          DELIMITED BY SIZE INTO SSA1                                     
250200     MOVE   'WLSATG11'   TO SSA2                                          
250300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
250400     CALL CBLTDLI USING GN SATG2-PCB DLI-IO-AREA2 SSA1 SSA2               
250500     MOVE SATG2-STATUS-CODE TO STATUS-WS                                  
250600     PERFORM IMS-STATUSKONTROLL                                           
250700     .                                                                    
250800     SKIP3                                                                
250900 IMS-GU-SATG3-SATG11       SECTION.                                       
251000     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
251100          DELIMITED BY SIZE INTO SSA1                                     
251200     STRING 'WLSATG11(IDARTNR > ' W-ING-IDARTNR-X                         
251300                    '&KDSATKMB>=' W-KDSATKMB-MIN-X                        
251400                    '&KDSATKMB<=' W-KDSATKMB-MAX-X ')'                    
251500          DELIMITED BY SIZE INTO SSA2                                     
251600     MOVE '  GE' TO GODK-STATUSKODER                                      
251700     CALL CBLTDLI USING GU SATG3-PCB DLI-IO-AREA2 SSA1 SSA2               
251800     MOVE SATG3-STATUS-CODE TO STATUS-WS                                  
251900     PERFORM IMS-STATUSKONTROLL                                           
252000     .                                                                    
252100     SKIP3                                                                
252200 IMS-GN-SATG3-SATG11       SECTION.                                       
252300     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
252400          DELIMITED BY SIZE INTO SSA1                                     
252500     STRING 'WLSATG11(IDARTNR > ' W-ING-IDARTNR-X                         
252600                    '&KDSATKMB>=' W-KDSATKMB-MIN-X                        
252700                    '&KDSATKMB<=' W-KDSATKMB-MAX-X ')'                    
252800          DELIMITED BY SIZE INTO SSA2                                     
252900     MOVE '  GE' TO GODK-STATUSKODER                                      
253000     CALL CBLTDLI USING GN SATG3-PCB DLI-IO-AREA2 SSA1 SSA2               
253100     MOVE SATG3-STATUS-CODE TO STATUS-WS                                  
253200     PERFORM IMS-STATUSKONTROLL                                           
253300     .                                                                    
253400     EJECT                                                                
253500 IMS-GU-ARTC1-ARTC11 SECTION.                                             
253600     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
253700          DELIMITED BY SIZE INTO SSA1                                     
253800     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
253900          DELIMITED BY SIZE INTO SSA2                                     
254000     MOVE '  GE' TO GODK-STATUSKODER                                      
254100     CALL CBLTDLI USING GU ARTC1-PCB DLI-IO-AREA7 SSA1 SSA2               
254200     MOVE ARTC1-STATUS-CODE TO STATUS-WS                                  
254300     PERFORM IMS-STATUSKONTROLL                                           
254400     .                                                                    
254500     SKIP3                                                                
254600 IMS-GHU-ARTC1-ARTC11 SECTION.                                            
254700     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
254800          DELIMITED BY SIZE INTO SSA1                                     
254900     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
255000          DELIMITED BY SIZE INTO SSA2                                     
255100     MOVE '    ' TO GODK-STATUSKODER                                      
255200     CALL CBLTDLI USING GHU ARTC1-PCB DLI-IO-AREA7 SSA1 SSA2              
255300     MOVE ARTC1-STATUS-CODE TO STATUS-WS                                  
255400     PERFORM IMS-STATUSKONTROLL                                           
255500     .                                                                    
255600     SKIP3                                                                
255700 IMS-REPL-ARTC1-ARTC11 SECTION.                                           
255800                                                                          
255900     MOVE '  ' TO GODK-STATUSKODER                                        
256000     CALL CBLTDLI USING REPL ARTC1-PCB DLI-IO-AREA7                       
256100     MOVE ARTC1-STATUS-CODE TO STATUS-WS                                  
256200     PERFORM IMS-STATUSKONTROLL                                           
256300     .                                                                    
256400     EJECT                                                                
256500 IMS-GU-ARTC2-ARTC11 SECTION.                                             
256600     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
256700          DELIMITED BY SIZE INTO SSA1                                     
256800     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
256900          DELIMITED BY SIZE INTO SSA2                                     
257000     MOVE '    ' TO GODK-STATUSKODER                                      
257100     CALL CBLTDLI USING GU ARTC2-PCB DLI-IO-AREA8 SSA1 SSA2               
257200     MOVE ARTC2-STATUS-CODE TO STATUS-WS                                  
257300     PERFORM IMS-STATUSKONTROLL                                           
257400     .                                                                    
257500     SKIP3                                                                
257600 IMS-GHU-OWDP1-ORDP01 SECTION.                                            
257700     STRING 'WLORDP01(WDA501KY>=' W-WDA501KY-MIN-X                        
257800                    '&WDA501KY<=' W-WDA501KY-MAX-X ')'                    
257900          DELIMITED BY SIZE INTO SSA1                                     
258000     MOVE '  GE' TO GODK-STATUSKODER                                      
258100     CALL CBLTDLI USING GHU OWDP1-PCB DLI-IO-AREA4 SSA1                   
258200     MOVE OWDP1-STATUS-CODE TO STATUS-WS                                  
258300     PERFORM IMS-STATUSKONTROLL                                           
258400     .                                                                    
258500     SKIP3                                                                
258600 IMS-GHN-OWDP1-ORDP01 SECTION.                                            
258700     STRING 'WLORDP01(WDA501KY>=' W-WDA501KY-MIN-X                        
258800                    '&WDA501KY<=' W-WDA501KY-MAX-X ')'                    
258900          DELIMITED BY SIZE INTO SSA1                                     
259000     MOVE '  GE' TO GODK-STATUSKODER                                      
259100     CALL CBLTDLI USING GHN OWDP1-PCB DLI-IO-AREA4 SSA1                   
259200     MOVE OWDP1-STATUS-CODE TO STATUS-WS                                  
259300     PERFORM IMS-STATUSKONTROLL                                           
259400     .                                                                    
259500     SKIP3                                                                
259600 IMS-DLET-OWDP1-ORDP01 SECTION.                                           
259700                                                                          
259800     MOVE '  ' TO GODK-STATUSKODER                                        
259900     CALL CBLTDLI USING DLET OWDP1-PCB DLI-IO-AREA4                       
260000     MOVE OWDP1-STATUS-CODE TO STATUS-WS                                  
260100     PERFORM IMS-STATUSKONTROLL                                           
260200     .                                                                    
260300     EJECT                                                                
260400 IMS-ISRT-OWDP2-ORDP01 SECTION.                                           
260500                                                                          
260600     MOVE 'WLORDP01 ' TO SSA1                                             
260700     MOVE '  II' TO GODK-STATUSKODER                                      
260800     CALL CBLTDLI USING ISRT OWDP2-PCB DLI-IO-AREA4 SSA1                  
260900     MOVE OWDP2-STATUS-CODE TO STATUS-WS                                  
261000     PERFORM IMS-STATUSKONTROLL                                           
261100     .                                                                    
261200     SKIP3                                                                
261300 IMS-GU-BENA11 SECTION.                                                   
261400     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
261500          DELIMITED BY SIZE INTO SSA1                                     
261600     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
261700          DELIMITED BY SIZE INTO SSA2                                     
261800     MOVE '  GE' TO GODK-STATUSKODER                                      
261900     CALL CBLTDLI USING GU  BENA-PCB DLI-IO-AREA3 SSA1 SSA2               
262000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
262100     PERFORM IMS-STATUSKONTROLL                                           
262200     .                                                                    
262300     EJECT                                                                
262400 IMS-GU-ARTM01 SECTION.                                                   
262500     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
262600          DELIMITED BY SIZE INTO SSA1                                     
262700     MOVE '  GE' TO GODK-STATUSKODER                                      
262800     CALL CBLTDLI USING GU  ARTM-PCB DLI-IO-AREA4 SSA1                    
262900     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
263000     PERFORM IMS-STATUSKONTROLL                                           
263100     .                                                                    
263200     EJECT                                                                
263300 IMS-GU-XXJN11 SECTION.                                                   
263400                                                                          
263500     STRING 'WLXXJN01(WDGXKEY  =' W-4511-IDHTYP-X  ')'                    
263600          DELIMITED BY SIZE INTO SSA1                                     
263700     STRING 'WLXXJN11(KDTPOTYP =' W-4512-KDTPOTYP-X                       
263800                    '&KDORDKL  =' W-4512-KDORDKL-X                        
263900                    '&IDDISTRF<=' W-4512-IDDISTR-FOM-X                    
264000                    '&IDDISTRT>=' W-4512-IDDISTR-TOM-X ')'                
264100          DELIMITED BY SIZE INTO SSA2                                     
264200     MOVE '    ' TO GODK-STATUSKODER                                      
264300     CALL CBLTDLI USING GU XXJN-PCB DLI-IO-AREA6 SSA1 SSA2                
264400     MOVE XXJN-STATUS-CODE TO STATUS-WS                                   
264500     PERFORM IMS-STATUSKONTROLL                                           
264600     .                                                                    
264700     SKIP3                                                                
264800 IMS-STATUSKONTROLL SECTION.                                              
264900                                                                          
265000     SET STATUS-IX TO 1                                                   
265100     SEARCH GODK-STATUS                                                   
265200       AT END                                                             
265300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
265400         DELIMITED BY SIZE INTO FELTEXT                                   
265500         CALL FELLOG                                                      
265600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
265700     END-SEARCH                                                           
265800     .                                                                    
