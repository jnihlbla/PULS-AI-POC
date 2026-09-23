000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2317100.                                                
000400*AUTHOR.         BODIL LINDAHL.                                           
000500*DATE-WRITTEN.   95/08/16.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*      - LÄSER FIL W23170                                                 
001100*      - SKAPAR LISTA REFILL UPPFÖLJNING                                  
001200*                                                                         
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700     EJECT                                                                
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*          --- GRUNDFIL ANALYS-LISTOR                                     
002100     SELECT W23170                     ASSIGN TO W23171D1.                
002200*          --- LISTA                                                      
002300     SELECT W23171-001                 ASSIGN TO W23171D3.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP3                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W23170                                                               
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS  0.                                                   
003200     SKIP2                                                                
003300*01  -COPY W231701A      -L.                                              
003400     SKIP3                                                                
003500 FD  W23171-001                                                           
003600     RECORDING       V                                                    
003700     BLOCK CONTAINS  0.                                                   
003800     SKIP2                                                                
003900 01  W23171-001-RAD              PIC X(169).                              
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200     SKIP2                                                                
004300                                                                          
004400*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W2317100'.            
004600                                                                          
004700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100 77  SW-KVOI-TRAFF               PIC X       VALUE 'N'.                   
005200 77  SW-ARTIKEL-SAKNAS-WDK7      PIC X       VALUE 'N'.                   
005300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005400 77  WS-FELTEXT                  PIC X(20)   VALUE SPACE.                 
005500 77  IX                          PIC S9(3)   VALUE ZERO COMP-3.           
005600 77  IX1                         PIC S9(3)   VALUE ZERO COMP-3.           
005700 77  IX2                         PIC S9(3)   VALUE ZERO COMP-3.           
005800 77  IX3                         PIC S9(3)   VALUE ZERO COMP-3.           
005900 77  IX4                         PIC S9(3)   VALUE ZERO COMP-3.           
006000 77  IX5                         PIC S9(3)   VALUE ZERO COMP-3.           
006100 77  IX6                         PIC S9(3)   VALUE ZERO COMP-3.           
006200 77  IX7                         PIC S9(3)   VALUE ZERO COMP-3.           
006300 77  IX8                         PIC S9(3)   VALUE ZERO COMP-3.           
006400 77  SDC-IX                      PIC 9(3)    VALUE ZERO COMP-3.           
006500 77  KVOI-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
006600 77  ART-IX                      PIC S9(3)   VALUE ZERO COMP-3.           
006700 77  ART-IX-MAX                  PIC S9(3)   VALUE +72  COMP-3.           
006800 77  PSUM-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
006900 77  PSUM-IX-MAX                 PIC S9(3)   VALUE +9   COMP-3.           
007000 77  FSUM-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
007100 77  FSUM-IX-MAX                 PIC S9(3)   VALUE +8   COMP-3.           
007200 77  WS-KVAKS                    PIC S9(11)  VALUE ZERO COMP-3.           
007300 77  WS-KVLS-TOT                 PIC S9(11)  VALUE ZERO COMP-3.           
007400 77  WS-KVOKS-TOT                PIC S9(11)  VALUE ZERO COMP-3.           
007500 77  WS-KVOI                     PIC S9(11)V9(2)                          
007600                                   VALUE ZERO COMP-3.                     
007700 77  WS-KVOI-TOT-AAR             PIC S9(11)     VALUE ZERO COMP-3.        
007800 77  WS-KVOI-TOT-VECKA           PIC S9(11)     VALUE ZERO COMP-3.        
007900 77  WS-KVOI-TOT-CDC-VECKA       PIC S9(11)     VALUE ZERO COMP-3.        
008000 77  WS-KVOI-TEO-CDC-VECKA       PIC S9(11)     VALUE ZERO COMP-3.        
008100 77  WS-KVOT-SAKNAS-WDK7         PIC 9(9)       VALUE ZERO.               
008200 77  WS-KVOT-CDC-SAKNAS-WDK7     PIC 9(9)       VALUE ZERO.               
008300 77  WS-KVDISP                   PIC S9(11)     VALUE ZERO COMP-3.        
008400 77  WS-KVDISP-PR                PIC S9(11)V9(2)                          
008500                                   VALUE ZERO COMP-3.                     
008600 77  WS-SUMMA                    PIC S9(11)V9(2)                          
008700                                   VALUE ZERO COMP-3.                     
008800 77  WS-SUMMA-TOT                PIC S9(11)V9(2)                          
008900                                   VALUE ZERO COMP-3.                     
009000 77  WS-SUMMA-KR                 PIC S9(11)      VALUE ZERO.              
009100 77  WS-SERVG                    PIC S9(7)V9(1) VALUE ZERO COMP-3.        
009200 77  WS-OLAGER                   PIC S9(11)      VALUE ZERO.              
009300 77  WS-MLAGER                   PIC S9(11)      VALUE ZERO.              
009400 77  WS-OMSHAST                  PIC 9(11)V9(1)  VALUE ZERO.              
009500 77  WS-PROC                     PIC S9(3)V9(1)  VALUE ZERO.              
009600 77  WS-KVPB-VECKA-SDC           PIC S9(6)V9(2)  VALUE ZERO.              
009700 77  WS-KVPB-DAG-SDC-NORM        PIC S9(6)V9(2)  VALUE ZERO.              
009800 77  WS-LT-BEHOV-SDC-NORM        PIC S9(7)V9(2)  VALUE ZERO.              
009900 77  WS-PREV-IDDC                PIC X(2)        VALUE SPACE.             
010000                                                                          
010100 77  W23170-EOF-SW               PIC X       VALUE 'N'.                   
010200     88  END-OF-W23170                       VALUE 'J'.                   
010300                                                                          
010400*      --- VALID IDDC CODES                                               
010500*                                                                         
010600*01    -COPY WWDCKONS                                                     
010700                                                                          
010800       EJECT                                                              
010900 01  DAGENS-DATUM                PIC 9(6).                                
011000 01  FILLER REDEFINES DAGENS-DATUM.                                       
011100     03  DAGENS-AAR              PIC 9(2).                                
011200     03  DAGENS-MAANAD           PIC 9(2).                                
011300     03  DAGENS-DAG              PIC 9(2).                                
011400                                                                          
011500 01  DAGENS-VECKA                PIC 9(4).                                
011600 01  FILLER REDEFINES DAGENS-VECKA.                                       
011700     03  D-VECKA-AAR             PIC 9(2).                                
011800     03  D-VECKA-VECKA           PIC 9(2).                                
011900                                                                          
012000 01  VECKOR.                                                              
012100     03  AAVVD                   PIC 9(5).                                
012200     03  FILLER REDEFINES AAVVD.                                          
012300         05  AAVV                PIC 9(4).                                
012400         05  D                   PIC 9(1).                                
012500                                                                          
012600     03  W009VADD-ANTAL          PIC S9(3) COMP-3.                        
012700     EJECT                                                                
012800 01  W-IDDC                      PIC X(2).                                
012900 01  W-IDLISTNR                  PIC 9(3).                                
013000 01  WS-DC-TABELL.                                                        
013100     03 DC-TABELL OCCURS 500.                                             
013200        05  WS-IDDC              PIC X(2) VALUE SPACE.                    
013300        05  WS-IDLISTNR          PIC 9(3) VALUE ZERO.                     
013400        05  WS-IDDC-REF          PIC X(2) VALUE SPACE.                    
013500        05  WS-KVDAGAR-TOT       PIC 9(3) VALUE ZERO.                     
013600 01  IDDC-IX                     PIC 9(3).                                
013700 01  IDDC-IX-MAX                 PIC 9(3) VALUE 500.                      
013800     EJECT                                                                
013900 01  ART-TABELL.                                                          
014000     03 ART-RAD OCCURS 72.                                                
014100        05  ART-KVANT-AKT        PIC S9(9)      VALUE ZERO COMP-3.        
014200        05  ART-KVANT-PAS        PIC S9(9)      VALUE ZERO COMP-3.        
014300        05  ART-PROC-KVANT-A     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
014400        05  ART-PROC-KVANT-P     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
014500        05  ART-KVDISP-AKT       PIC S9(11)     VALUE ZERO COMP-3.        
014600        05  ART-PROC-KVDISP-A    PIC S9(7)V9(1) VALUE ZERO COMP-3.        
014700        05  ART-KVDISP-PAS       PIC S9(11)     VALUE ZERO COMP-3.        
014800        05  ART-PROC-KVDISP-P    PIC S9(7)V9(1) VALUE ZERO COMP-3.        
014900        05  ART-LS-AKT           PIC S9(11)     VALUE ZERO COMP-3.        
015000        05  ART-PROC-LS-A        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015100        05  ART-LS-PAS           PIC S9(11)     VALUE ZERO COMP-3.        
015200        05  ART-PROC-LS-P        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015300        05  ART-AK-AKT           PIC S9(11)     VALUE ZERO COMP-3.        
015400        05  ART-PROC-AK-A        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015500        05  ART-AK-PAS           PIC S9(11)     VALUE ZERO COMP-3.        
015600        05  ART-PROC-AK-P        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015700        05  ART-OLAGER           PIC S9(9)      VALUE ZERO COMP-3.        
015800        05  ART-PROC-OLAGER      PIC S9(7)V9(1) VALUE ZERO COMP-3.        
015900        05  ART-SLAGER           PIC S9(9)      VALUE ZERO COMP-3.        
016000        05  ART-PROC-SLAGER      PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016100        05  ART-MLAGER           PIC S9(9)      VALUE ZERO COMP-3.        
016200        05  ART-PROC-MLAGER      PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016300        05  ART-KVOT             PIC S9(9)      VALUE ZERO COMP-3.        
016400        05  ART-PROC-KVOT        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016500        05  ART-SPLIT            PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016600        05  ART-OMSHAST-DISP     PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016700        05  ART-OMSHAST-PROC-D   PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016800        05  ART-OMSHAST-LS       PIC S9(7)V9(1) VALUE ZERO COMP-3.        
016900        05  ART-OMSHAST-PROC-LS  PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017000        05  ART-SERVG-TOT        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017100        05  ART-SERVG-AKT        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017200        05  ART-SERVG-PAS        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017300        05  ART-SERVG-TEO        PIC S9(7)V9(1) VALUE ZERO COMP-3.        
017400*******  ARBETSFÄLT                                                       
017500        05  WS-ART-SLAGER        PIC S9(11)V9(2) VALUE ZERO.              
017600        05  WS-ART-OLAGER        PIC S9(11)V9(2) VALUE ZERO.              
017700        05  WS-ART-MLAGER        PIC S9(11)V9(2) VALUE ZERO.              
017800        05  WS-ART-LS-AKT        PIC S9(11)      VALUE ZERO.              
017900        05  WS-ART-LS-PAS        PIC S9(11)      VALUE ZERO.              
018000        05  WS-ART-LS-PR-AKT     PIC S9(11)V9(2) VALUE ZERO.              
018100        05  WS-ART-LS-PR-PAS     PIC S9(11)V9(2) VALUE ZERO.              
018200        05  WS-ART-KVLS-AKT      PIC S9(11)      VALUE ZERO.              
018300        05  WS-ART-KVLS-PAS      PIC S9(11)      VALUE ZERO.              
018400        05  WS-ART-KVDISP-AKT    PIC S9(11)      VALUE ZERO.              
018500        05  WS-ART-KVDISP-PAS    PIC S9(11)      VALUE ZERO.              
018600        05  WS-ART-KVDISP-PR-AKT PIC S9(11)V9(2) VALUE ZERO.              
018700        05  WS-ART-KVDISP-PR-PAS PIC S9(11)V9(2) VALUE ZERO.              
018800        05  WS-ART-KVOKS-AKT     PIC S9(11)      VALUE ZERO.              
018900        05  WS-ART-KVOKS-PAS     PIC S9(11)      VALUE ZERO.              
019000        05  WS-ART-OK-PR-AKT     PIC S9(11)V9(2) VALUE ZERO.              
019100        05  WS-ART-OK-PR-PAS     PIC S9(11)V9(2) VALUE ZERO.              
019200        05  WS-ART-KVAKS-AKT     PIC S9(11)      VALUE ZERO.              
019300        05  WS-ART-KVAKS-PAS     PIC S9(11)      VALUE ZERO.              
019400        05  WS-ART-AK-PR-AKT     PIC S9(11)V9(2) VALUE ZERO.              
019500        05  WS-ART-AK-PR-PAS     PIC S9(11)V9(2) VALUE ZERO.              
019600        05  WS-ART-KVOI          PIC S9(11)V9(2) VALUE ZERO.              
019700        05  WS-ART-KVOI-AKT      PIC S9(9)      VALUE ZERO COMP-3.        
019800        05  WS-ART-KVOI-PAS      PIC S9(9)      VALUE ZERO COMP-3.        
019900        05  WS-ART-KVOI-TEO      PIC S9(9)      VALUE ZERO COMP-3.        
020000        05  WS-ART-KVOI-SAK      PIC S9(9)      VALUE ZERO COMP-3.        
020100        05  WS-ART-KVOI-CDC-AKT  PIC S9(9)      VALUE ZERO COMP-3.        
020200        05  WS-ART-KVOI-CDC-PAS  PIC S9(9)      VALUE ZERO COMP-3.        
020300        05  WS-ART-KVOI-CDC-TEO  PIC S9(9)      VALUE ZERO COMP-3.        
020400        05  WS-ART-KVOI-CDC-SAK  PIC S9(9)      VALUE ZERO COMP-3.        
020500     EJECT                                                                
020600 01  PSUM-TABELL.                                                         
020700     03 PSUM-RAD OCCURS 9.                                                
020800        05  PSUM-KVANT-AKT      PIC S9(9)      VALUE ZERO COMP-3.         
020900        05  PSUM-KVANT-PAS      PIC S9(9)      VALUE ZERO COMP-3.         
021000        05  PSUM-PROC-KVANT-A   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
021100        05  PSUM-PROC-KVANT-P   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
021200        05  PSUM-KVDISP-AKT     PIC S9(11)     VALUE ZERO COMP-3.         
021300        05  PSUM-PROC-KVDISP-A  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
021400        05  PSUM-KVDISP-PAS     PIC S9(11)     VALUE ZERO COMP-3.         
021500        05  PSUM-PROC-KVDISP-P  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
021600        05  PSUM-LS-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
021700        05  PSUM-PROC-LS-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
021800        05  PSUM-LS-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
021900        05  PSUM-PROC-LS-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022000        05  PSUM-AK-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
022100        05  PSUM-PROC-AK-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022200        05  PSUM-AK-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
022300        05  PSUM-PROC-AK-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022400        05  PSUM-OLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
022500        05  PSUM-PROC-OLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022600        05  PSUM-SLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
022700        05  PSUM-PROC-SLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
022800        05  PSUM-KVOT           PIC S9(9)      VALUE ZERO COMP-3.         
022900        05  PSUM-PROC-KVOT      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023000        05  PSUM-MLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
023100        05  PSUM-PROC-MLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023200        05  PSUM-SPLIT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023300        05  PSUM-OMSHAST-DISP   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023400        05  PSUM-OMSHAST-PROC-D  PIC S9(7)V9(1) VALUE ZERO COMP-3.        
023500        05  PSUM-OMSHAST-LS     PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023600        05  PSUM-OMSHAST-PROC-LS PIC S9(7)V9(1) VALUE ZERO COMP-3.        
023700        05  PSUM-SERVG-TOT      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023800        05  PSUM-SERVG-AKT      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
023900        05  PSUM-SERVG-PAS      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
024000        05  PSUM-SERVG-TEO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
024100*******  ARBETSFÄLT                                                       
024200        05  WS-PSUM-SLAGER       PIC S9(11)V9(2) VALUE ZERO.              
024300        05  WS-PSUM-OLAGER       PIC S9(11)V9(2) VALUE ZERO.              
024400        05  WS-PSUM-MLAGER       PIC S9(11)V9(2) VALUE ZERO.              
024500        05  WS-PSUM-LS-AKT       PIC S9(11)      VALUE ZERO.              
024600        05  WS-PSUM-LS-PAS       PIC S9(11)      VALUE ZERO.              
024700        05  WS-PSUM-LS-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
024800        05  WS-PSUM-LS-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
024900        05  WS-PSUM-KVDISP-AKT   PIC S9(11)      VALUE ZERO.              
025000        05  WS-PSUM-KVDISP-PAS   PIC S9(11)      VALUE ZERO.              
025100        05  WS-PSUM-KVDISP-PR-AKT PIC S9(11)V9(2) VALUE ZERO.             
025200        05  WS-PSUM-KVDISP-PR-PAS PIC S9(11)V9(2) VALUE ZERO.             
025300        05  WS-PSUM-KVOKS-AKT    PIC S9(11)      VALUE ZERO.              
025400        05  WS-PSUM-KVOKS-PAS    PIC S9(11)      VALUE ZERO.              
025500        05  WS-PSUM-OK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
025600        05  WS-PSUM-OK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
025700        05  WS-PSUM-KVAKS-AKT    PIC S9(11)      VALUE ZERO.              
025800        05  WS-PSUM-KVAKS-PAS    PIC S9(11)      VALUE ZERO.              
025900        05  WS-PSUM-AK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
026000        05  WS-PSUM-AK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
026100        05  WS-PSUM-KVOI         PIC S9(11)V9(2) VALUE ZERO.              
026200        05  WS-PSUM-KVOI-AKT     PIC S9(9)       VALUE ZERO.              
026300        05  WS-PSUM-KVOI-PAS     PIC S9(9)       VALUE ZERO.              
026400        05  WS-PSUM-KVOI-TEO     PIC S9(9)       VALUE ZERO.              
026500        05  WS-PSUM-KVOI-SAK     PIC S9(9)       VALUE ZERO.              
026600        05  WS-PSUM-KVOI-CDC-AKT PIC S9(9)       VALUE ZERO.              
026700        05  WS-PSUM-KVOI-CDC-PAS PIC S9(9)       VALUE ZERO.              
026800        05  WS-PSUM-KVOI-CDC-TEO PIC S9(9)       VALUE ZERO.              
026900        05  WS-PSUM-KVOI-CDC-SAK PIC S9(9)       VALUE ZERO.              
027000     EJECT                                                                
027100 01  FSUM-TABELL.                                                         
027200     03 FSUM-RAD OCCURS 8.                                                
027300        05  FSUM-KVANT-AKT      PIC S9(9)      VALUE ZERO COMP-3.         
027400        05  FSUM-KVANT-PAS      PIC S9(9)      VALUE ZERO COMP-3.         
027500        05  FSUM-PROC-KVANT-A   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
027600        05  FSUM-PROC-KVANT-P   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
027700        05  FSUM-KVDISP-AKT     PIC S9(11)     VALUE ZERO COMP-3.         
027800        05  FSUM-PROC-KVDISP-A  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
027900        05  FSUM-KVDISP-PAS     PIC S9(11)     VALUE ZERO COMP-3.         
028000        05  FSUM-PROC-KVDISP-P  PIC S9(7)V9(1) VALUE ZERO COMP-3.         
028100        05  FSUM-LS-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
028200        05  FSUM-PROC-LS-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
028300        05  FSUM-LS-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
028400        05  FSUM-PROC-LS-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
028500        05  FSUM-AK-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
028600        05  FSUM-PROC-AK-A      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
028700        05  FSUM-AK-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
028800        05  FSUM-PROC-AK-P      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
028900        05  FSUM-OLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
029000        05  FSUM-PROC-OLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029100        05  FSUM-SLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
029200        05  FSUM-PROC-SLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029300        05  FSUM-MLAGER         PIC S9(9)      VALUE ZERO COMP-3.         
029400        05  FSUM-PROC-MLAGER    PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029500        05  FSUM-KVOT           PIC S9(9)      VALUE ZERO COMP-3.         
029600        05  FSUM-PROC-KVOT      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029700        05  FSUM-SPLIT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029800        05  FSUM-OMSHAST-DISP   PIC S9(7)V9(1) VALUE ZERO COMP-3.         
029900        05  FSUM-OMSHAST-PROC-D PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030000        05  FSUM-OMSHAST-LS     PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030100        05  FSUM-OMSHAST-PROC-LS PIC S9(7)V9(1) VALUE ZERO COMP-3.        
030200        05  FSUM-SERVG-TOT      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030300        05  FSUM-SERVG-AKT      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030400        05  FSUM-SERVG-PAS      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030500        05  FSUM-SERVG-TEO      PIC S9(7)V9(1) VALUE ZERO COMP-3.         
030600*******  ARBETSFÄLT                                                       
030700        05  WS-FSUM-SLAGER       PIC S9(11)V9(2) VALUE ZERO.              
030800        05  WS-FSUM-OLAGER       PIC S9(11)V9(2) VALUE ZERO.              
030900        05  WS-FSUM-MLAGER       PIC S9(11)V9(2) VALUE ZERO.              
031000        05  WS-FSUM-LS-AKT       PIC S9(11)      VALUE ZERO.              
031100        05  WS-FSUM-LS-PAS       PIC S9(11)      VALUE ZERO.              
031200        05  WS-FSUM-LS-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
031300        05  WS-FSUM-LS-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
031400        05  WS-FSUM-KVDISP-AKT   PIC S9(11)      VALUE ZERO.              
031500        05  WS-FSUM-KVDISP-PAS   PIC S9(11)      VALUE ZERO.              
031600        05  WS-FSUM-KVDISP-PR-AKT  PIC S9(11)V9(2) VALUE ZERO.            
031700        05  WS-FSUM-KVDISP-PR-PAS  PIC S9(11)V9(2) VALUE ZERO.            
031800        05  WS-FSUM-KVOKS-AKT    PIC S9(11)      VALUE ZERO.              
031900        05  WS-FSUM-KVOKS-PAS    PIC S9(11)      VALUE ZERO.              
032000        05  WS-FSUM-OK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
032100        05  WS-FSUM-OK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
032200        05  WS-FSUM-KVAKS-AKT    PIC S9(11)      VALUE ZERO.              
032300        05  WS-FSUM-KVAKS-PAS    PIC S9(11)      VALUE ZERO.              
032400        05  WS-FSUM-AK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.              
032500        05  WS-FSUM-AK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.              
032600        05  WS-FSUM-KVOI         PIC S9(11)V9(2) VALUE ZERO.              
032700        05  WS-FSUM-KVOI-AKT     PIC S9(9)       VALUE ZERO.              
032800        05  WS-FSUM-KVOI-PAS     PIC S9(9)       VALUE ZERO.              
032900        05  WS-FSUM-KVOI-TEO     PIC S9(9)       VALUE ZERO.              
033000        05  WS-FSUM-KVOI-SAK     PIC S9(9)       VALUE ZERO.              
033100        05  WS-FSUM-KVOI-CDC-AKT PIC S9(9)       VALUE ZERO.              
033200        05  WS-FSUM-KVOI-CDC-PAS PIC S9(9)       VALUE ZERO.              
033300        05  WS-FSUM-KVOI-CDC-TEO PIC S9(9)       VALUE ZERO.              
033400        05  WS-FSUM-KVOI-CDC-SAK PIC S9(9)       VALUE ZERO.              
033500     EJECT                                                                
033600 01  TOTAL-RUTA.                                                          
033700     03  TOT-KVANT-AKT          PIC S9(9)      VALUE ZERO COMP-3.         
033800     03  TOT-KVANT-PAS          PIC S9(9)      VALUE ZERO COMP-3.         
033900     03  TOT-KVDISP-AKT         PIC S9(11)     VALUE ZERO COMP-3.         
034000     03  TOT-KVDISP-PAS         PIC S9(11)     VALUE ZERO COMP-3.         
034100     03  TOT-LS-AKT             PIC S9(11)     VALUE ZERO COMP-3.         
034200     03  TOT-LS-PAS             PIC S9(11)     VALUE ZERO COMP-3.         
034300     03  TOT-AK-AKT             PIC S9(11)     VALUE ZERO COMP-3.         
034400     03  TOT-AK-PAS             PIC S9(11)     VALUE ZERO COMP-3.         
034500     03  TOT-SLAGER             PIC S9(9)      VALUE ZERO COMP-3.         
034600     03  TOT-PROC-SLAGER        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
034700     03  TOT-MLAGER             PIC S9(9)      VALUE ZERO COMP-3.         
034800     03  TOT-PROC-MLAGER        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
034900     03  TOT-KVOT               PIC S9(9)      VALUE ZERO COMP-3.         
035000     03  TOT-PROC-KVOT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
035100     03  TOT-OLAGER             PIC S9(9)      VALUE ZERO COMP-3.         
035200     03  TOT-PROC-OLAGER        PIC S9(7)V9(1) VALUE ZERO COMP-3.         
035300     03  TOT-SPLIT              PIC S9(7)V9(1) VALUE ZERO COMP-3.         
035400     03  TOT-OMSHAST-DISP       PIC S9(7)V9(1) VALUE ZERO COMP-3.         
035500     03  TOT-OMSHAST-LS         PIC S9(7)V9(1) VALUE ZERO COMP-3.         
035600     03  TOT-SERVG-TOT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
035700     03  TOT-SERVG-AKT          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
035800     03  TOT-SERVG-PAS          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
035900     03  TOT-SERVG-TEO          PIC S9(7)V9(1) VALUE ZERO COMP-3.         
036000*******  ARBETSFÄLT                                                       
036100     03  WS-TOT-SLAGER       PIC S9(11)V9(2) VALUE ZERO.                  
036200     03  WS-TOT-OLAGER       PIC S9(11)V9(2) VALUE ZERO.                  
036300     03  WS-TOT-MLAGER       PIC S9(11)V9(2) VALUE ZERO.                  
036400     03  WS-TOT-LS-AKT       PIC S9(11)      VALUE ZERO.                  
036500     03  WS-TOT-LS-PAS       PIC S9(11)      VALUE ZERO.                  
036600     03  WS-TOT-LS-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.                  
036700     03  WS-TOT-LS-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.                  
036800     03  WS-TOT-KVDISP-AKT   PIC S9(11)      VALUE ZERO.                  
036900     03  WS-TOT-KVDISP-PAS   PIC S9(11)      VALUE ZERO.                  
037000     03  WS-TOT-KVDISP-PR-AKT  PIC S9(11)V9(2) VALUE ZERO.                
037100     03  WS-TOT-KVDISP-PR-PAS  PIC S9(11)V9(2) VALUE ZERO.                
037200     03  WS-TOT-KVOKS-AKT    PIC S9(11)      VALUE ZERO.                  
037300     03  WS-TOT-KVOKS-PAS    PIC S9(11)      VALUE ZERO.                  
037400     03  WS-TOT-OK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.                  
037500     03  WS-TOT-OK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.                  
037600     03  WS-TOT-KVAKS-AKT    PIC S9(11)      VALUE ZERO.                  
037700     03  WS-TOT-KVAKS-PAS    PIC S9(11)      VALUE ZERO.                  
037800     03  WS-TOT-AK-PR-AKT    PIC S9(11)V9(2) VALUE ZERO.                  
037900     03  WS-TOT-AK-PR-PAS    PIC S9(11)V9(2) VALUE ZERO.                  
038000     03  WS-TOT-KVOI         PIC S9(11)V9(2) VALUE ZERO.                  
038100     03  WS-TOT-KVOI-AKT     PIC S9(9)       VALUE ZERO.                  
038200     03  WS-TOT-KVOI-PAS     PIC S9(9)       VALUE ZERO.                  
038300     03  WS-TOT-KVOI-TEO     PIC S9(9)       VALUE ZERO.                  
038400     03  WS-TOT-KVOI-SAK     PIC S9(9)       VALUE ZERO.                  
038500     03  WS-TOT-KVOI-CDC-AKT PIC S9(9)       VALUE ZERO.                  
038600     03  WS-TOT-KVOI-CDC-PAS PIC S9(9)       VALUE ZERO.                  
038700     03  WS-TOT-KVOI-CDC-TEO PIC S9(9)       VALUE ZERO.                  
038800     03  WS-TOT-KVOI-CDC-SAK PIC S9(9)       VALUE ZERO.                  
038900     EJECT                                                                
039000 01  DYNAMISKA-SUBPROGRAM.                                                
039100*                                                                         
039200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
039300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
039400     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
039500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
039600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
039700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
039800                                                                          
039900     EJECT                                                                
040000 01  PARAM-TILL-DATKORT.                                                  
040100     03  PROG-ID                 PIC X(8)    VALUE 'W2317100'.            
040200     03  KORT-ID                 PIC X(6)    VALUE 'WDATUM'.              
040300*03  -COPY WDATKORT                                                       
040400     EJECT                                                                
040500*03  -COPY WDATAREA                                                       
040600     EJECT                                                                
040700*    --- PARAMETRAR TILL POSTSUM                                          
040800*                                                                         
040900*01  -COPY W0005   -PRE  POSTSUM-                                         
041000     EJECT                                                                
041100 01  IN-AREA-START               PIC X(24)   VALUE                        
041200                                 'IN-AREA-START    '.                     
041300*01  AREA  -COPY W231701A   -PRE IN-                                      
041400     EJECT                                                                
041500 01  W001-AREA-START             PIC X(24)   VALUE                        
041600                                 'W001-AREA-START  '.                     
041700     SKIP2                                                                
041800 01  W001-HJALPAREOR.                                                     
041900*                                                                         
042000     03  W001-SKIP               PIC 9(3) COMP-3  VALUE 1.                
042100     03  W001-ANTAL-RADER                                                 
042200                                 PIC 9(3)    VALUE 999.                   
042300     03  W001-MAX-RADER-PER-SIDA                                          
042400                                 PIC 9(3)    VALUE 63.                    
042500     03  W001-MAX-POSITIONER-PER-RAD                                      
042600                                 PIC 9(3)    VALUE 165.                   
042700     03  W001-LISTNR             PIC X(11)   VALUE SPACE.                 
042800     03  W001-SIDRAKNARE         PIC S9(5)   COMP-3 VALUE ZERO.           
042900     SKIP2                                                                
043000 01  W001-RAD.                                                            
043100     03  FILLER                  PIC X(165)  VALUE SPACE.                 
043200     EJECT                                                                
043300 01  W001-DAP.                                                            
043400     03  FILLER                  PIC X(165)  VALUE SPACE.                 
043500     EJECT                                                                
043600 01  W001-RUBRIK1.                                                        
043700*                                                                         
043800     03  FILLER                  PIC X(3)    VALUE SPACE.                 
043900     03  FILLER                  PIC X(18)                                
044000                              VALUE 'VOLVO CAR PARTS   '.                 
044100     03  W001-LISTID             PIC X(12)                                
044200                                 VALUE SPACE.                             
044300     03  FILLER                  PIC X(20)                                
044400             VALUE 'REFILL UPPFÖLJNING  '.                                
044500     03  FILLER                  PIC X(5)    VALUE SPACE.                 
044600     03  W001-SDC-LAGER          PIC X(9)    VALUE SPACE.                 
044700     03  FILLER                  PIC X(2)    VALUE SPACE.                 
044800     03  W001-AKTUELLT-IDDC      PIC X(2)    VALUE SPACE.                 
044900     03  FILLER                  PIC X(6)    VALUE SPACE.                 
045000     03  FILLER                  PIC X(6)    VALUE 'VECKA '.              
045100     03  W001-AKTUELL-VECKA      PIC 9(4)    VALUE ZERO.                  
045200*    03  FILLER                  PIC X(27)   VALUE SPACE.                 
045300     03  FILLER                  PIC X(37)   VALUE SPACE.                 
045400     03  W001-DATUM              PIC XXBXXBXX.                            
045500     03  FILLER                  PIC X(7)    VALUE SPACE.                 
045600     03  FILLER                  PIC X(4)    VALUE 'SID '.                
045700     03  W001-SID                PIC Z(4)9.                               
045800     EJECT                                                                
045900 01  W001-RUBRIK3.                                                        
046000*                                                                         
046100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
046200     03  FILLER                  PIC X(9)                                 
046300                             VALUE 'PRISKLASS'.                           
046400     03  FILLER                  PIC X(13)   VALUE SPACE.                 
046500     03  FILLER                  PIC X(7)    VALUE 'A      '.             
046600     03  FILLER                  PIC X(7)    VALUE SPACE.                 
046700     03  FILLER                  PIC X(8)    VALUE 'B       '.            
046800     03  FILLER                  PIC X(6)    VALUE SPACE.                 
046900     03  FILLER                  PIC X(8)    VALUE 'C       '.            
047000     03  FILLER                  PIC X(6)    VALUE SPACE.                 
047100     03  FILLER                  PIC X(9)    VALUE 'D        '.           
047200     03  FILLER                  PIC X(5)    VALUE SPACE.                 
047300     03  FILLER                  PIC X(9)    VALUE 'E        '.           
047400     03  FILLER                  PIC X(5)    VALUE SPACE.                 
047500     03  FILLER                  PIC X(10)   VALUE 'F         '.          
047600     03  FILLER                  PIC X(4)    VALUE SPACE.                 
047700     03  FILLER                  PIC X(10)   VALUE 'G         '.          
047800     03  FILLER                  PIC X(4)    VALUE SPACE.                 
047900     03  FILLER                  PIC X(10)   VALUE 'H         '.          
048000     03  FILLER                  PIC X(11)   VALUE SPACE.                 
048100     03  FILLER                  PIC X(6)    VALUE 'TOTALT'.              
048200     EJECT                                                                
048300 01  W001-DETALJRAD-1.                                                    
048400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
048500     03  W001-DET1-PRISKLASS     PIC X       VALUE SPACE.                 
048600     03  FILLER                  PIC X       VALUE SPACE.                 
048700     03  FILLER                  PIC X(12)   VALUE 'ANT ART  AKT'.        
048800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
048900     03  W001-DET1-KVANTA        PIC Z(7)9.                               
049000     03  FILLER                  PIC X       VALUE SPACE.                 
049100     03  W001-DET1-P-KVANTA      PIC Z9.9.                                
049200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
049300     03  W001-DET1-KVANTB        PIC Z(7)9.                               
049400     03  FILLER                  PIC X       VALUE SPACE.                 
049500     03  W001-DET1-P-KVANTB      PIC Z9.9.                                
049600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
049700     03  W001-DET1-KVANTC        PIC Z(7)9.                               
049800     03  FILLER                  PIC X       VALUE SPACE.                 
049900     03  W001-DET1-P-KVANTC      PIC Z9.9.                                
050000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
050100     03  W001-DET1-KVANTD        PIC Z(7)9.                               
050200     03  FILLER                  PIC X       VALUE SPACE.                 
050300     03  W001-DET1-P-KVANTD      PIC Z9.9.                                
050400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
050500     03  W001-DET1-KVANTE        PIC Z(7)9.                               
050600     03  FILLER                  PIC X       VALUE SPACE.                 
050700     03  W001-DET1-P-KVANTE      PIC Z9.9.                                
050800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
050900     03  W001-DET1-KVANTF        PIC Z(7)9.                               
051000     03  FILLER                  PIC X       VALUE SPACE.                 
051100     03  W001-DET1-P-KVANTF      PIC Z9.9.                                
051200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
051300     03  W001-DET1-KVANTG        PIC Z(7)9.                               
051400     03  FILLER                  PIC X       VALUE SPACE.                 
051500     03  W001-DET1-P-KVANTG      PIC Z9.9.                                
051600     03  FILLER                  PIC X       VALUE SPACE.                 
051700     03  W001-DET1-KVANTH        PIC Z(7)9.                               
051800     03  FILLER                  PIC X       VALUE SPACE.                 
051900     03  W001-DET1-P-KVANTH      PIC Z9.9.                                
052000     03  FILLER                  PIC X       VALUE SPACE.                 
052100     03  W001-DET1-TOT           PIC Z(13)9.                              
052200     03  FILLER                  PIC X       VALUE SPACE.                 
052300     03  W001-DET1-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
052400     EJECT                                                                
052500 01  W001-DETALJRAD-2.                                                    
052600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
052700     03  W001-DET2-PRISKLASS     PIC X       VALUE SPACE.                 
052800     03  FILLER                  PIC X       VALUE SPACE.                 
052900     03  FILLER                  PIC X(12)   VALUE 'ANT ART  PAS'.        
053000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
053100     03  W001-DET2-KVANTA        PIC Z(7)9.                               
053200     03  FILLER                  PIC X       VALUE SPACE.                 
053300     03  W001-DET2-P-KVANTA      PIC Z9.9.                                
053400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
053500     03  W001-DET2-KVANTB        PIC Z(7)9.                               
053600     03  FILLER                  PIC X       VALUE SPACE.                 
053700     03  W001-DET2-P-KVANTB      PIC Z9.9.                                
053800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
053900     03  W001-DET2-KVANTC        PIC Z(7)9.                               
054000     03  FILLER                  PIC X       VALUE SPACE.                 
054100     03  W001-DET2-P-KVANTC      PIC Z9.9.                                
054200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
054300     03  W001-DET2-KVANTD        PIC Z(7)9.                               
054400     03  FILLER                  PIC X       VALUE SPACE.                 
054500     03  W001-DET2-P-KVANTD      PIC Z9.9.                                
054600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
054700     03  W001-DET2-KVANTE        PIC Z(7)9.                               
054800     03  FILLER                  PIC X       VALUE SPACE.                 
054900     03  W001-DET2-P-KVANTE      PIC Z9.9.                                
055000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
055100     03  W001-DET2-KVANTF        PIC Z(7)9.                               
055200     03  FILLER                  PIC X       VALUE SPACE.                 
055300     03  W001-DET2-P-KVANTF      PIC Z9.9.                                
055400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
055500     03  W001-DET2-KVANTG        PIC Z(7)9.                               
055600     03  FILLER                  PIC X       VALUE SPACE.                 
055700     03  W001-DET2-P-KVANTG      PIC Z9.9.                                
055800     03  FILLER                  PIC X       VALUE SPACE.                 
055900     03  W001-DET2-KVANTH        PIC Z(7)9.                               
056000     03  FILLER                  PIC X       VALUE SPACE.                 
056100     03  W001-DET2-P-KVANTH      PIC Z9.9.                                
056200     03  FILLER                  PIC X       VALUE SPACE.                 
056300     03  W001-DET2-TOT           PIC Z(13)9.                              
056400     03  FILLER                  PIC X       VALUE SPACE.                 
056500     03  W001-DET2-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
056600     EJECT                                                                
056700 01  W001-DETALJRAD-3.                                                    
056800     03  FILLER                  PIC X(3)    VALUE SPACE.                 
056900     03  FILLER                  PIC X(12)   VALUE 'DISP LAGER A'.        
057000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
057100     03  W001-DET3-DLAGERA       PIC Z(7)9.                               
057200     03  FILLER                  PIC X       VALUE SPACE.                 
057300     03  W001-DET3-P-DLAGERA     PIC Z9.9.                                
057400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
057500     03  W001-DET3-DLAGERB       PIC Z(7)9.                               
057600     03  FILLER                  PIC X       VALUE SPACE.                 
057700     03  W001-DET3-P-DLAGERB     PIC Z9.9.                                
057800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
057900     03  W001-DET3-DLAGERC       PIC Z(7)9.                               
058000     03  FILLER                  PIC X       VALUE SPACE.                 
058100     03  W001-DET3-P-DLAGERC     PIC Z9.9.                                
058200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
058300     03  W001-DET3-DLAGERD       PIC Z(7)9.                               
058400     03  FILLER                  PIC X       VALUE SPACE.                 
058500     03  W001-DET3-P-DLAGERD     PIC Z9.9.                                
058600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
058700     03  W001-DET3-DLAGERE       PIC Z(7)9.                               
058800     03  FILLER                  PIC X       VALUE SPACE.                 
058900     03  W001-DET3-P-DLAGERE     PIC Z9.9.                                
059000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
059100     03  W001-DET3-DLAGERF       PIC Z(7)9.                               
059200     03  FILLER                  PIC X       VALUE SPACE.                 
059300     03  W001-DET3-P-DLAGERF     PIC Z9.9.                                
059400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
059500     03  W001-DET3-DLAGERG       PIC Z(7)9.                               
059600     03  FILLER                  PIC X       VALUE SPACE.                 
059700     03  W001-DET3-P-DLAGERG     PIC Z9.9.                                
059800     03  FILLER                  PIC X       VALUE SPACE.                 
059900     03  W001-DET3-DLAGERH       PIC Z(7)9.                               
060000     03  FILLER                  PIC X       VALUE SPACE.                 
060100     03  W001-DET3-P-DLAGERH     PIC Z9.9.                                
060200     03  FILLER                  PIC X       VALUE SPACE.                 
060300     03  W001-DET3-TOT           PIC Z(13)9.                              
060400     03  FILLER                  PIC X       VALUE SPACE.                 
060500     03  W001-DET3-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
060600 01  W001-DETALJRAD-4.                                                    
060700     03  FILLER                  PIC X(3)    VALUE SPACE.                 
060800     03  FILLER                  PIC X(12)   VALUE 'DISP LAGER P'.        
060900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
061000     03  W001-DET4-DLAGERA       PIC Z(7)9.                               
061100     03  FILLER                  PIC X       VALUE SPACE.                 
061200     03  W001-DET4-P-DLAGERA     PIC Z9.9.                                
061300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
061400     03  W001-DET4-DLAGERB       PIC Z(7)9.                               
061500     03  FILLER                  PIC X       VALUE SPACE.                 
061600     03  W001-DET4-P-DLAGERB     PIC Z9.9.                                
061700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
061800     03  W001-DET4-DLAGERC       PIC Z(7)9.                               
061900     03  FILLER                  PIC X       VALUE SPACE.                 
062000     03  W001-DET4-P-DLAGERC     PIC Z9.9.                                
062100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
062200     03  W001-DET4-DLAGERD       PIC Z(7)9.                               
062300     03  FILLER                  PIC X       VALUE SPACE.                 
062400     03  W001-DET4-P-DLAGERD     PIC Z9.9.                                
062500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
062600     03  W001-DET4-DLAGERE       PIC Z(7)9.                               
062700     03  FILLER                  PIC X       VALUE SPACE.                 
062800     03  W001-DET4-P-DLAGERE     PIC Z9.9.                                
062900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
063000     03  W001-DET4-DLAGERF       PIC Z(7)9.                               
063100     03  FILLER                  PIC X       VALUE SPACE.                 
063200     03  W001-DET4-P-DLAGERF     PIC Z9.9.                                
063300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
063400     03  W001-DET4-DLAGERG       PIC Z(7)9.                               
063500     03  FILLER                  PIC X       VALUE SPACE.                 
063600     03  W001-DET4-P-DLAGERG     PIC Z9.9.                                
063700     03  FILLER                  PIC X       VALUE SPACE.                 
063800     03  W001-DET4-DLAGERH       PIC Z(7)9.                               
063900     03  FILLER                  PIC X       VALUE SPACE.                 
064000     03  W001-DET4-P-DLAGERH     PIC Z9.9.                                
064100     03  FILLER                  PIC X       VALUE SPACE.                 
064200     03  W001-DET4-TOT           PIC Z(13)9.                              
064300     03  FILLER                  PIC X       VALUE SPACE.                 
064400     03  W001-DET4-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
064500     EJECT                                                                
064600 01  W001-DETALJRAD-5.                                                    
064700     03  FILLER                  PIC X(3)    VALUE SPACE.                 
064800     03  FILLER                  PIC X(12)   VALUE 'LAGERVÄRDE A'.        
064900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
065000     03  W001-DET5-LLAGERA       PIC Z(7)9.                               
065100     03  FILLER                  PIC X       VALUE SPACE.                 
065200     03  W001-DET5-P-LLAGERA     PIC Z9.9.                                
065300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
065400     03  W001-DET5-LLAGERB       PIC Z(7)9.                               
065500     03  FILLER                  PIC X       VALUE SPACE.                 
065600     03  W001-DET5-P-LLAGERB     PIC Z9.9.                                
065700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
065800     03  W001-DET5-LLAGERC       PIC Z(7)9.                               
065900     03  FILLER                  PIC X       VALUE SPACE.                 
066000     03  W001-DET5-P-LLAGERC     PIC Z9.9.                                
066100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
066200     03  W001-DET5-LLAGERD       PIC Z(7)9.                               
066300     03  FILLER                  PIC X       VALUE SPACE.                 
066400     03  W001-DET5-P-LLAGERD     PIC Z9.9.                                
066500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
066600     03  W001-DET5-LLAGERE       PIC Z(7)9.                               
066700     03  FILLER                  PIC X       VALUE SPACE.                 
066800     03  W001-DET5-P-LLAGERE     PIC Z9.9.                                
066900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
067000     03  W001-DET5-LLAGERF       PIC Z(7)9.                               
067100     03  FILLER                  PIC X       VALUE SPACE.                 
067200     03  W001-DET5-P-LLAGERF     PIC Z9.9.                                
067300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
067400     03  W001-DET5-LLAGERG       PIC Z(7)9.                               
067500     03  FILLER                  PIC X       VALUE SPACE.                 
067600     03  W001-DET5-P-LLAGERG     PIC Z9.9.                                
067700     03  FILLER                  PIC X       VALUE SPACE.                 
067800     03  W001-DET5-LLAGERH       PIC Z(7)9.                               
067900     03  FILLER                  PIC X       VALUE SPACE.                 
068000     03  W001-DET5-P-LLAGERH     PIC Z9.9.                                
068100     03  FILLER                  PIC X       VALUE SPACE.                 
068200     03  W001-DET5-TOT           PIC Z(13)9.                              
068300     03  FILLER                  PIC X       VALUE SPACE.                 
068400     03  W001-DET5-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
068500     EJECT                                                                
068600 01  W001-DETALJRAD-6.                                                    
068700     03  FILLER                  PIC X(3)    VALUE SPACE.                 
068800     03  FILLER                  PIC X(12)   VALUE 'LAGERVÄRDE P'.        
068900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
069000     03  W001-DET6-LLAGERA       PIC Z(7)9.                               
069100     03  FILLER                  PIC X       VALUE SPACE.                 
069200     03  W001-DET6-P-LLAGERA     PIC Z9.9.                                
069300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
069400     03  W001-DET6-LLAGERB       PIC Z(7)9.                               
069500     03  FILLER                  PIC X       VALUE SPACE.                 
069600     03  W001-DET6-P-LLAGERB     PIC Z9.9.                                
069700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
069800     03  W001-DET6-LLAGERC       PIC Z(7)9.                               
069900     03  FILLER                  PIC X       VALUE SPACE.                 
070000     03  W001-DET6-P-LLAGERC     PIC Z9.9.                                
070100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
070200     03  W001-DET6-LLAGERD       PIC Z(7)9.                               
070300     03  FILLER                  PIC X       VALUE SPACE.                 
070400     03  W001-DET6-P-LLAGERD     PIC Z9.9.                                
070500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
070600     03  W001-DET6-LLAGERE       PIC Z(7)9.                               
070700     03  FILLER                  PIC X       VALUE SPACE.                 
070800     03  W001-DET6-P-LLAGERE     PIC Z9.9.                                
070900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
071000     03  W001-DET6-LLAGERF       PIC Z(7)9.                               
071100     03  FILLER                  PIC X       VALUE SPACE.                 
071200     03  W001-DET6-P-LLAGERF     PIC Z9.9.                                
071300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
071400     03  W001-DET6-LLAGERG       PIC Z(7)9.                               
071500     03  FILLER                  PIC X       VALUE SPACE.                 
071600     03  W001-DET6-P-LLAGERG     PIC Z9.9.                                
071700     03  FILLER                  PIC X       VALUE SPACE.                 
071800     03  W001-DET6-LLAGERH       PIC Z(7)9.                               
071900     03  FILLER                  PIC X       VALUE SPACE.                 
072000     03  W001-DET6-P-LLAGERH     PIC Z9.9.                                
072100     03  FILLER                  PIC X       VALUE SPACE.                 
072200     03  W001-DET6-TOT           PIC Z(13)9.                              
072300     03  FILLER                  PIC X       VALUE SPACE.                 
072400     03  W001-DET6-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
072500     EJECT                                                                
072600 01  W001-DETALJRAD-7.                                                    
072700     03  FILLER                  PIC X(3)    VALUE SPACE.                 
072800     03  FILLER                  PIC X(12)   VALUE 'AK VÄRDE   A'.        
072900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
073000     03  W001-DET7-ALAGERA       PIC Z(7)9.                               
073100     03  FILLER                  PIC X       VALUE SPACE.                 
073200     03  W001-DET7-P-ALAGERA     PIC Z9.9.                                
073300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
073400     03  W001-DET7-ALAGERB       PIC Z(7)9.                               
073500     03  FILLER                  PIC X       VALUE SPACE.                 
073600     03  W001-DET7-P-ALAGERB     PIC Z9.9.                                
073700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
073800     03  W001-DET7-ALAGERC       PIC Z(7)9.                               
073900     03  FILLER                  PIC X       VALUE SPACE.                 
074000     03  W001-DET7-P-ALAGERC     PIC Z9.9.                                
074100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
074200     03  W001-DET7-ALAGERD       PIC Z(7)9.                               
074300     03  FILLER                  PIC X       VALUE SPACE.                 
074400     03  W001-DET7-P-ALAGERD     PIC Z9.9.                                
074500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
074600     03  W001-DET7-ALAGERE       PIC Z(7)9.                               
074700     03  FILLER                  PIC X       VALUE SPACE.                 
074800     03  W001-DET7-P-ALAGERE     PIC Z9.9.                                
074900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
075000     03  W001-DET7-ALAGERF       PIC Z(7)9.                               
075100     03  FILLER                  PIC X       VALUE SPACE.                 
075200     03  W001-DET7-P-ALAGERF     PIC Z9.9.                                
075300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
075400     03  W001-DET7-ALAGERG       PIC Z(7)9.                               
075500     03  FILLER                  PIC X       VALUE SPACE.                 
075600     03  W001-DET7-P-ALAGERG     PIC Z9.9.                                
075700     03  FILLER                  PIC X       VALUE SPACE.                 
075800     03  W001-DET7-ALAGERH       PIC Z(7)9.                               
075900     03  FILLER                  PIC X       VALUE SPACE.                 
076000     03  W001-DET7-P-ALAGERH     PIC Z9.9.                                
076100     03  FILLER                  PIC X       VALUE SPACE.                 
076200     03  W001-DET7-TOT           PIC Z(13)9.                              
076300     03  FILLER                  PIC X       VALUE SPACE.                 
076400     03  W001-DET7-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
076500     EJECT                                                                
076600 01  W001-DETALJRAD-8.                                                    
076700     03  FILLER                  PIC X(3)    VALUE SPACE.                 
076800     03  FILLER                  PIC X(12)   VALUE 'AK VÄRDE   P'.        
076900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
077000     03  W001-DET8-ALAGERA       PIC Z(7)9.                               
077100     03  FILLER                  PIC X       VALUE SPACE.                 
077200     03  W001-DET8-P-ALAGERA     PIC Z9.9.                                
077300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
077400     03  W001-DET8-ALAGERB       PIC Z(7)9.                               
077500     03  FILLER                  PIC X       VALUE SPACE.                 
077600     03  W001-DET8-P-ALAGERB     PIC Z9.9.                                
077700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
077800     03  W001-DET8-ALAGERC       PIC Z(7)9.                               
077900     03  FILLER                  PIC X       VALUE SPACE.                 
078000     03  W001-DET8-P-ALAGERC     PIC Z9.9.                                
078100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
078200     03  W001-DET8-ALAGERD       PIC Z(7)9.                               
078300     03  FILLER                  PIC X       VALUE SPACE.                 
078400     03  W001-DET8-P-ALAGERD     PIC Z9.9.                                
078500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
078600     03  W001-DET8-ALAGERE       PIC Z(7)9.                               
078700     03  FILLER                  PIC X       VALUE SPACE.                 
078800     03  W001-DET8-P-ALAGERE     PIC Z9.9.                                
078900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
079000     03  W001-DET8-ALAGERF       PIC Z(7)9.                               
079100     03  FILLER                  PIC X       VALUE SPACE.                 
079200     03  W001-DET8-P-ALAGERF     PIC Z9.9.                                
079300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
079400     03  W001-DET8-ALAGERG       PIC Z(7)9.                               
079500     03  FILLER                  PIC X       VALUE SPACE.                 
079600     03  W001-DET8-P-ALAGERG     PIC Z9.9.                                
079700     03  FILLER                  PIC X       VALUE SPACE.                 
079800     03  W001-DET8-ALAGERH       PIC Z(7)9.                               
079900     03  FILLER                  PIC X       VALUE SPACE.                 
080000     03  W001-DET8-P-ALAGERH     PIC Z9.9.                                
080100     03  FILLER                  PIC X       VALUE SPACE.                 
080200     03  W001-DET8-TOT           PIC Z(13)9.                              
080300     03  FILLER                  PIC X       VALUE SPACE.                 
080400     03  W001-DET8-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
080500     EJECT                                                                
080600 01  W001-DETALJRAD-9.                                                    
080700     03  FILLER                  PIC X(3)    VALUE SPACE.                 
080800     03  FILLER                  PIC X(12)   VALUE 'ÖVERLAGER   '.        
080900     03  FILLER                  PIC X       VALUE SPACE.                 
081000     03  W001-DET9-OLAGERA       PIC Z(7)9.                               
081100     03  FILLER                  PIC X       VALUE SPACE.                 
081200     03  W001-DET9-P-OLAGERA     PIC Z9.9    BLANK WHEN ZERO.             
081300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
081400     03  W001-DET9-OLAGERB       PIC Z(7)9.                               
081500     03  FILLER                  PIC X       VALUE SPACE.                 
081600     03  W001-DET9-P-OLAGERB     PIC Z9.9    BLANK WHEN ZERO.             
081700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
081800     03  W001-DET9-OLAGERC       PIC Z(7)9.                               
081900     03  FILLER                  PIC X       VALUE SPACE.                 
082000     03  W001-DET9-P-OLAGERC     PIC Z9.9    BLANK WHEN ZERO.             
082100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
082200     03  W001-DET9-OLAGERD       PIC Z(7)9.                               
082300     03  FILLER                  PIC X       VALUE SPACE.                 
082400     03  W001-DET9-P-OLAGERD     PIC Z9.9    BLANK WHEN ZERO.             
082500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
082600     03  W001-DET9-OLAGERE       PIC Z(7)9.                               
082700     03  FILLER                  PIC X       VALUE SPACE.                 
082800     03  W001-DET9-P-OLAGERE     PIC Z9.9    BLANK WHEN ZERO.             
082900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
083000     03  W001-DET9-OLAGERF       PIC Z(7)9.                               
083100     03  FILLER                  PIC X       VALUE SPACE.                 
083200     03  W001-DET9-P-OLAGERF     PIC Z9.9    BLANK WHEN ZERO.             
083300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
083400     03  W001-DET9-OLAGERG       PIC Z(7)9.                               
083500     03  FILLER                  PIC X       VALUE SPACE.                 
083600     03  W001-DET9-P-OLAGERG     PIC Z9.9    BLANK WHEN ZERO.             
083700     03  FILLER                  PIC X       VALUE SPACE.                 
083800     03  W001-DET9-OLAGERH       PIC Z(7)9.                               
083900     03  FILLER                  PIC X       VALUE SPACE.                 
084000     03  W001-DET9-P-OLAGERH     PIC Z9.9    BLANK WHEN ZERO.             
084100     03  FILLER                  PIC X       VALUE SPACE.                 
084200     03  W001-DET9-TOT           PIC Z(13)9.                              
084300     03  FILLER                  PIC X       VALUE SPACE.                 
084400     03  W001-DET9-P-TOT         PIC ZZ9.9   BLANK WHEN ZERO.             
084500     EJECT                                                                
084600 01  W001-DETALJRAD-10.                                                   
084700     03  FILLER                  PIC X(3)    VALUE SPACE.                 
084800     03  FILLER                  PIC X(12)   VALUE 'SÄK-LAGER   '.        
084900     03  FILLER                  PIC X       VALUE SPACE.                 
085000     03  W001-DET10-SLAGERA      PIC Z(7)9.                               
085100     03  FILLER                  PIC X       VALUE SPACE.                 
085200     03  W001-DET10-P-SLAGERA    PIC Z9.9    BLANK WHEN ZERO.             
085300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
085400     03  W001-DET10-SLAGERB      PIC Z(7)9.                               
085500     03  FILLER                  PIC X       VALUE SPACE.                 
085600     03  W001-DET10-P-SLAGERB    PIC Z9.9    BLANK WHEN ZERO.             
085700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
085800     03  W001-DET10-SLAGERC      PIC Z(7)9.                               
085900     03  FILLER                  PIC X       VALUE SPACE.                 
086000     03  W001-DET10-P-SLAGERC    PIC Z9.9    BLANK WHEN ZERO.             
086100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
086200     03  W001-DET10-SLAGERD      PIC Z(7)9.                               
086300     03  FILLER                  PIC X       VALUE SPACE.                 
086400     03  W001-DET10-P-SLAGERD    PIC Z9.9    BLANK WHEN ZERO.             
086500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
086600     03  W001-DET10-SLAGERE      PIC Z(7)9.                               
086700     03  FILLER                  PIC X       VALUE SPACE.                 
086800     03  W001-DET10-P-SLAGERE    PIC Z9.9    BLANK WHEN ZERO.             
086900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
087000     03  W001-DET10-SLAGERF      PIC Z(7)9.                               
087100     03  FILLER                  PIC X       VALUE SPACE.                 
087200     03  W001-DET10-P-SLAGERF    PIC Z9.9    BLANK WHEN ZERO.             
087300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
087400     03  W001-DET10-SLAGERG      PIC Z(7)9.                               
087500     03  FILLER                  PIC X       VALUE SPACE.                 
087600     03  W001-DET10-P-SLAGERG    PIC Z9.9    BLANK WHEN ZERO.             
087700     03  FILLER                  PIC X       VALUE SPACE.                 
087800     03  W001-DET10-SLAGERH      PIC Z(7)9.                               
087900     03  FILLER                  PIC X       VALUE SPACE.                 
088000     03  W001-DET10-P-SLAGERH    PIC Z9.9    BLANK WHEN ZERO.             
088100     03  FILLER                  PIC X       VALUE SPACE.                 
088200     03  W001-DET10-TOT          PIC Z(13)9.                              
088300     03  FILLER                  PIC X       VALUE SPACE.                 
088400     03  W001-DET10-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
088500     EJECT                                                                
088600 01  W001-DETALJRAD-11.                                                   
088700     03  FILLER                  PIC X(3)    VALUE SPACE.                 
088800     03  FILLER                  PIC X(12)   VALUE 'MEDELLAGER  '.        
088900     03  FILLER                  PIC X       VALUE SPACE.                 
089000     03  W001-DET11-MLAGERA      PIC Z(7)9.                               
089100     03  FILLER                  PIC X       VALUE SPACE.                 
089200     03  W001-DET11-P-MLAGERA    PIC Z9.9    BLANK WHEN ZERO.             
089300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
089400     03  W001-DET11-MLAGERB      PIC Z(7)9.                               
089500     03  FILLER                  PIC X       VALUE SPACE.                 
089600     03  W001-DET11-P-MLAGERB    PIC Z9.9    BLANK WHEN ZERO.             
089700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
089800     03  W001-DET11-MLAGERC      PIC Z(7)9.                               
089900     03  FILLER                  PIC X       VALUE SPACE.                 
090000     03  W001-DET11-P-MLAGERC    PIC Z9.9    BLANK WHEN ZERO.             
090100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
090200     03  W001-DET11-MLAGERD      PIC Z(7)9.                               
090300     03  FILLER                  PIC X       VALUE SPACE.                 
090400     03  W001-DET11-P-MLAGERD    PIC Z9.9    BLANK WHEN ZERO.             
090500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
090600     03  W001-DET11-MLAGERE      PIC Z(7)9.                               
090700     03  FILLER                  PIC X       VALUE SPACE.                 
090800     03  W001-DET11-P-MLAGERE    PIC Z9.9    BLANK WHEN ZERO.             
090900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
091000     03  W001-DET11-MLAGERF      PIC Z(7)9.                               
091100     03  FILLER                  PIC X       VALUE SPACE.                 
091200     03  W001-DET11-P-MLAGERF    PIC Z9.9    BLANK WHEN ZERO.             
091300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
091400     03  W001-DET11-MLAGERG      PIC Z(7)9.                               
091500     03  FILLER                  PIC X       VALUE SPACE.                 
091600     03  W001-DET11-P-MLAGERG    PIC Z9.9    BLANK WHEN ZERO.             
091700     03  FILLER                  PIC X       VALUE SPACE.                 
091800     03  W001-DET11-MLAGERH      PIC Z(7)9.                               
091900     03  FILLER                  PIC X       VALUE SPACE.                 
092000     03  W001-DET11-P-MLAGERH    PIC Z9.9    BLANK WHEN ZERO.             
092100     03  FILLER                  PIC X       VALUE SPACE.                 
092200     03  W001-DET11-TOT          PIC Z(13)9.                              
092300     03  FILLER                  PIC X       VALUE SPACE.                 
092400     03  W001-DET11-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
092500     EJECT                                                                
092600 01  W001-DETALJRAD-12.                                                   
092700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
092800     03  W001-DET12-PRISKLASS    PIC X       VALUE SPACE.                 
092900     03  FILLER                  PIC X       VALUE SPACE.                 
093000     03  FILLER                  PIC X(12)   VALUE 'ORDERTRÄFFAR'.        
093100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
093200     03  W001-DET12-KVOTA        PIC Z(7)9.                               
093300     03  FILLER                  PIC X       VALUE SPACE.                 
093400     03  W001-DET12-P-KVOTA      PIC Z9.9.                                
093500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
093600     03  W001-DET12-KVOTB        PIC Z(7)9.                               
093700     03  FILLER                  PIC X       VALUE SPACE.                 
093800     03  W001-DET12-P-KVOTB      PIC Z9.9.                                
093900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
094000     03  W001-DET12-KVOTC        PIC Z(7)9.                               
094100     03  FILLER                  PIC X       VALUE SPACE.                 
094200     03  W001-DET12-P-KVOTC      PIC Z9.9.                                
094300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
094400     03  W001-DET12-KVOTD        PIC Z(7)9.                               
094500     03  FILLER                  PIC X       VALUE SPACE.                 
094600     03  W001-DET12-P-KVOTD      PIC Z9.9.                                
094700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
094800     03  W001-DET12-KVOTE        PIC Z(7)9.                               
094900     03  FILLER                  PIC X       VALUE SPACE.                 
095000     03  W001-DET12-P-KVOTE      PIC Z9.9.                                
095100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
095200     03  W001-DET12-KVOTF        PIC Z(7)9.                               
095300     03  FILLER                  PIC X       VALUE SPACE.                 
095400     03  W001-DET12-P-KVOTF      PIC Z9.9.                                
095500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
095600     03  W001-DET12-KVOTG        PIC Z(7)9.                               
095700     03  FILLER                  PIC X       VALUE SPACE.                 
095800     03  W001-DET12-P-KVOTG      PIC Z9.9.                                
095900     03  FILLER                  PIC X       VALUE SPACE.                 
096000     03  W001-DET12-KVOTH        PIC Z(7)9.                               
096100     03  FILLER                  PIC X       VALUE SPACE.                 
096200     03  W001-DET12-P-KVOTH      PIC Z9.9.                                
096300     03  FILLER                  PIC X       VALUE SPACE.                 
096400     03  W001-DET12-TOT          PIC Z(13)9.                              
096500     03  FILLER                  PIC X       VALUE SPACE.                 
096600     03  W001-DET12-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
096700     EJECT                                                                
096800 01  W001-DETALJRAD-13.                                                   
096900     03  FILLER                  PIC X(3)    VALUE SPACE.                 
097000     03  FILLER                  PIC X(13) VALUE 'SPLITFAKTOR  '.         
097100     03  FILLER                  PIC X(4)    VALUE SPACE.                 
097200     03  W001-DET13-SPLITA       PIC Z9.9.                                
097300     03  FILLER                  PIC X       VALUE SPACE.                 
097400     03  W001-DET13-FILLERA      PIC X(4)    VALUE SPACE.                 
097500     03  FILLER                  PIC X(5)    VALUE SPACE.                 
097600     03  W001-DET13-SPLITB       PIC Z9.9.                                
097700     03  FILLER                  PIC X       VALUE SPACE.                 
097800     03  W001-DET13-FILLERB      PIC X(4)    VALUE SPACE.                 
097900     03  FILLER                  PIC X(5)    VALUE SPACE.                 
098000     03  W001-DET13-SPLITC       PIC Z9.9.                                
098100     03  FILLER                  PIC X       VALUE SPACE.                 
098200     03  W001-DET13-FILLERC      PIC X(4)    VALUE SPACE.                 
098300     03  FILLER                  PIC X(5)    VALUE SPACE.                 
098400     03  W001-DET13-SPLITD       PIC Z9.9.                                
098500     03  FILLER                  PIC X       VALUE SPACE.                 
098600     03  W001-DET13-FILLERD      PIC X(4)    VALUE SPACE.                 
098700     03  FILLER                  PIC X(5)    VALUE SPACE.                 
098800     03  W001-DET13-SPLITE       PIC Z9.9.                                
098900     03  FILLER                  PIC X       VALUE SPACE.                 
099000     03  W001-DET13-FILLERE      PIC X(4)    VALUE SPACE.                 
099100     03  FILLER                  PIC X(5)    VALUE SPACE.                 
099200     03  W001-DET13-SPLITF       PIC Z9.9.                                
099300     03  FILLER                  PIC X       VALUE SPACE.                 
099400     03  W001-DET13-FILLERF      PIC X(4)    VALUE SPACE.                 
099500     03  FILLER                  PIC X(5)    VALUE SPACE.                 
099600     03  W001-DET13-SPLITG       PIC Z9.9.                                
099700     03  FILLER                  PIC X       VALUE SPACE.                 
099800     03  W001-DET13-FILLERG      PIC X(4)    VALUE SPACE.                 
099900     03  FILLER                  PIC X(5)    VALUE SPACE.                 
100000     03  W001-DET13-SPLITH       PIC Z9.9.                                
100100     03  FILLER                  PIC X       VALUE SPACE.                 
100200     03  W001-DET13-FILLERH      PIC X(4)    VALUE SPACE.                 
100300     03  FILLER                  PIC X(11)   VALUE SPACE.                 
100400     03  W001-DET13-TOT          PIC Z9.9.                                
100500     03  FILLER                  PIC X       VALUE SPACE.                 
100600     03  W001-DET13-P-TOT        PIC Z(2)9.9 BLANK WHEN ZERO.             
100700     EJECT                                                                
100800 01  W001-DETALJRAD-14.                                                   
100900     03  FILLER                  PIC X(3)    VALUE SPACE.                 
101000     03  FILLER                  PIC X(13) VALUE 'TOR      DISP'.         
101100     03  W001-DET14-OMSHASTA     PIC Z(5)9.9.                             
101200     03  FILLER                  PIC X       VALUE SPACE.                 
101300     03  W001-DET14-P-OMSHASTA   PIC Z9.9    BLANK WHEN ZERO.             
101400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
101500     03  W001-DET14-OMSHASTB     PIC Z(5)9.9.                             
101600     03  FILLER                  PIC X       VALUE SPACE.                 
101700     03  W001-DET14-P-OMSHASTB   PIC Z9.9    BLANK WHEN ZERO.             
101800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
101900     03  W001-DET14-OMSHASTC     PIC Z(5)9.9.                             
102000     03  FILLER                  PIC X       VALUE SPACE.                 
102100     03  W001-DET14-P-OMSHASTC   PIC Z9.9    BLANK WHEN ZERO.             
102200     03  FILLER                  PIC X(1)    VALUE SPACE.                 
102300     03  W001-DET14-OMSHASTD     PIC Z(5)9.9.                             
102400     03  FILLER                  PIC X       VALUE SPACE.                 
102500     03  W001-DET14-P-OMSHASTD   PIC Z9.9    BLANK WHEN ZERO.             
102600     03  FILLER                  PIC X(1)    VALUE SPACE.                 
102700     03  W001-DET14-OMSHASTE     PIC Z(5)9.9.                             
102800     03  FILLER                  PIC X       VALUE SPACE.                 
102900     03  W001-DET14-P-OMSHASTE   PIC Z9.9    BLANK WHEN ZERO.             
103000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
103100     03  W001-DET14-OMSHASTF     PIC Z(5)9.9.                             
103200     03  FILLER                  PIC X       VALUE SPACE.                 
103300     03  W001-DET14-P-OMSHASTF   PIC Z9.9    BLANK WHEN ZERO.             
103400     03  FILLER                  PIC X(1)    VALUE SPACE.                 
103500     03  W001-DET14-OMSHASTG     PIC Z(5)9.9.                             
103600     03  FILLER                  PIC X       VALUE SPACE.                 
103700     03  W001-DET14-P-OMSHASTG   PIC Z9.9    BLANK WHEN ZERO.             
103800     03  FILLER                  PIC X       VALUE SPACE.                 
103900     03  W001-DET14-OMSHASTH     PIC Z(5)9.9.                             
104000     03  FILLER                  PIC X       VALUE SPACE.                 
104100     03  W001-DET14-P-OMSHASTH   PIC Z9.9    BLANK WHEN ZERO.             
104200     03  FILLER                  PIC X       VALUE SPACE.                 
104300     03  W001-DET14-TOT          PIC Z(11)9.9.                            
104400     03  FILLER                  PIC X       VALUE SPACE.                 
104500     03  W001-DET14-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
104600     EJECT                                                                
104700 01  W001-DETALJRAD-15.                                                   
104800     03  FILLER                  PIC X(3)    VALUE SPACE.                 
104900     03  FILLER                  PIC X(13)  VALUE 'TOR LS+AK+GIT'.        
105000     03  W001-DET15-OMSHASTA     PIC Z(5)9.9.                             
105100     03  FILLER                  PIC X       VALUE SPACE.                 
105200     03  W001-DET15-P-OMSHASTA   PIC Z9.9    BLANK WHEN ZERO.             
105300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
105400     03  W001-DET15-OMSHASTB     PIC Z(5)9.9.                             
105500     03  FILLER                  PIC X       VALUE SPACE.                 
105600     03  W001-DET15-P-OMSHASTB   PIC Z9.9    BLANK WHEN ZERO.             
105700     03  FILLER                  PIC X(1)    VALUE SPACE.                 
105800     03  W001-DET15-OMSHASTC     PIC Z(5)9.9.                             
105900     03  FILLER                  PIC X       VALUE SPACE.                 
106000     03  W001-DET15-P-OMSHASTC   PIC Z9.9    BLANK WHEN ZERO.             
106100     03  FILLER                  PIC X(1)    VALUE SPACE.                 
106200     03  W001-DET15-OMSHASTD     PIC Z(5)9.9.                             
106300     03  FILLER                  PIC X       VALUE SPACE.                 
106400     03  W001-DET15-P-OMSHASTD   PIC Z9.9    BLANK WHEN ZERO.             
106500     03  FILLER                  PIC X(1)    VALUE SPACE.                 
106600     03  W001-DET15-OMSHASTE     PIC Z(5)9.9.                             
106700     03  FILLER                  PIC X       VALUE SPACE.                 
106800     03  W001-DET15-P-OMSHASTE   PIC Z9.9    BLANK WHEN ZERO.             
106900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
107000     03  W001-DET15-OMSHASTF     PIC Z(5)9.9.                             
107100     03  FILLER                  PIC X       VALUE SPACE.                 
107200     03  W001-DET15-P-OMSHASTF   PIC Z9.9    BLANK WHEN ZERO.             
107300     03  FILLER                  PIC X(1)    VALUE SPACE.                 
107400     03  W001-DET15-OMSHASTG     PIC Z(5)9.9.                             
107500     03  FILLER                  PIC X       VALUE SPACE.                 
107600     03  W001-DET15-P-OMSHASTG   PIC Z9.9    BLANK WHEN ZERO.             
107700     03  FILLER                  PIC X       VALUE SPACE.                 
107800     03  W001-DET15-OMSHASTH     PIC Z(5)9.9.                             
107900     03  FILLER                  PIC X       VALUE SPACE.                 
108000     03  W001-DET15-P-OMSHASTH   PIC Z9.9    BLANK WHEN ZERO.             
108100     03  FILLER                  PIC X       VALUE SPACE.                 
108200     03  W001-DET15-TOT          PIC Z(11)9.9.                            
108300     03  FILLER                  PIC X       VALUE SPACE.                 
108400     03  W001-DET15-P-TOT        PIC ZZ9.9   BLANK WHEN ZERO.             
108500     EJECT                                                                
108600 01  W001-DETALJRAD-16.                                                   
108700     03  FILLER                  PIC X(3)    VALUE SPACE.                 
108800     03  FILLER                  PIC X(12) VALUE 'SERVGRAD A/P'.          
108900     03  FILLER                  PIC X(5)    VALUE SPACE.                 
109000     03  W001-DET16-SERVGA-A     PIC Z9.9.                                
109100     03  FILLER                  PIC X       VALUE SPACE.                 
109200     03  W001-DET16-SERVGA-P     PIC Z9.9.                                
109300     03  FILLER                  PIC X(5)    VALUE SPACE.                 
109400     03  W001-DET16-SERVGB-A     PIC Z9.9.                                
109500     03  FILLER                  PIC X       VALUE SPACE.                 
109600     03  W001-DET16-SERVGB-P     PIC Z9.9.                                
109700     03  FILLER                  PIC X(5)    VALUE SPACE.                 
109800     03  W001-DET16-SERVGC-A     PIC Z9.9.                                
109900     03  FILLER                  PIC X       VALUE SPACE.                 
110000     03  W001-DET16-SERVGC-P     PIC Z9.9.                                
110100     03  FILLER                  PIC X(5)    VALUE SPACE.                 
110200     03  W001-DET16-SERVGD-A     PIC Z9.9.                                
110300     03  FILLER                  PIC X       VALUE SPACE.                 
110400     03  W001-DET16-SERVGD-P     PIC Z9.9.                                
110500     03  FILLER                  PIC X(5)    VALUE SPACE.                 
110600     03  W001-DET16-SERVGE-A     PIC Z9.9.                                
110700     03  FILLER                  PIC X       VALUE SPACE.                 
110800     03  W001-DET16-SERVGE-P     PIC Z9.9.                                
110900     03  FILLER                  PIC X(5)    VALUE SPACE.                 
111000     03  W001-DET16-SERVGF-A     PIC Z9.9.                                
111100     03  FILLER                  PIC X       VALUE SPACE.                 
111200     03  W001-DET16-SERVGF-P     PIC Z9.9.                                
111300     03  FILLER                  PIC X(5)    VALUE SPACE.                 
111400     03  W001-DET16-SERVGG-A     PIC Z9.9.                                
111500     03  FILLER                  PIC X       VALUE SPACE.                 
111600     03  W001-DET16-SERVGG-P     PIC Z9.9.                                
111700     03  FILLER                  PIC X(5)    VALUE SPACE.                 
111800     03  W001-DET16-SERVGH-A     PIC Z9.9.                                
111900     03  FILLER                  PIC X       VALUE SPACE.                 
112000     03  W001-DET16-SERVGH-P     PIC Z9.9.                                
112100     03  FILLER                  PIC X(11)   VALUE SPACE.                 
112200     03  W001-DET16-TOT          PIC Z9.9.                                
112300     03  FILLER                  PIC X       VALUE SPACE.                 
112400     03  W001-DET16-P-TOT        PIC Z(2)9.9 BLANK WHEN ZERO.             
112500     EJECT                                                                
112600 01  W001-DETALJRAD-17.                                                   
112700     03  FILLER                  PIC X(3)    VALUE SPACE.                 
112800     03  FILLER                  PIC X(12) VALUE 'SERVGRAD TOT'.          
112900     03  FILLER                  PIC X(5)    VALUE SPACE.                 
113000     03  W001-DET17-SERVGA-TOT   PIC Z9.9.                                
113100     03  FILLER                  PIC X       VALUE SPACE.                 
113200     03  W001-DET17-SERVGA-BTO   PIC X(4)    VALUE SPACE.                 
113300     03  FILLER                  PIC X(5)    VALUE SPACE.                 
113400     03  W001-DET17-SERVGB-TOT   PIC Z9.9.                                
113500     03  FILLER                  PIC X       VALUE SPACE.                 
113600     03  W001-DET17-SERVGB-BTO   PIC X(4)    VALUE SPACE.                 
113700     03  FILLER                  PIC X(5)    VALUE SPACE.                 
113800     03  W001-DET17-SERVGC-TOT   PIC Z9.9.                                
113900     03  FILLER                  PIC X       VALUE SPACE.                 
114000     03  W001-DET17-SERVGC-BTO   PIC X(4)    VALUE SPACE.                 
114100     03  FILLER                  PIC X(5)    VALUE SPACE.                 
114200     03  W001-DET17-SERVGD-TOT   PIC Z9.9.                                
114300     03  FILLER                  PIC X       VALUE SPACE.                 
114400     03  W001-DET17-SERVGD-BTO   PIC X(4)    VALUE SPACE.                 
114500     03  FILLER                  PIC X(5)    VALUE SPACE.                 
114600     03  W001-DET17-SERVGE-TOT   PIC Z9.9.                                
114700     03  FILLER                  PIC X       VALUE SPACE.                 
114800     03  W001-DET17-SERVGE-BTO   PIC X(4)    VALUE SPACE.                 
114900     03  FILLER                  PIC X(5)    VALUE SPACE.                 
115000     03  W001-DET17-SERVGF-TOT   PIC Z9.9.                                
115100     03  FILLER                  PIC X       VALUE SPACE.                 
115200     03  W001-DET17-SERVGF-BTO   PIC X(4)    VALUE SPACE.                 
115300     03  FILLER                  PIC X(5)    VALUE SPACE.                 
115400     03  W001-DET17-SERVGG-TOT   PIC Z9.9.                                
115500     03  FILLER                  PIC X       VALUE SPACE.                 
115600     03  W001-DET17-SERVGG-BTO   PIC X(4)    VALUE SPACE.                 
115700     03  FILLER                  PIC X(5)    VALUE SPACE.                 
115800     03  W001-DET17-SERVGH-TOT   PIC Z9.9.                                
115900     03  FILLER                  PIC X       VALUE SPACE.                 
116000     03  W001-DET17-SERVGH-BTO   PIC X(4)    VALUE SPACE.                 
116100     03  FILLER                  PIC X(11)   VALUE SPACE.                 
116200     03  W001-DET17-TOT          PIC Z9.9.                                
116300     03  FILLER                  PIC X       VALUE SPACE.                 
116400     03  W001-DET17-P-TOT        PIC Z(2)9.9 BLANK WHEN ZERO.             
116500     EJECT                                                                
116600 01  W001-DETALJRAD-18.                                                   
116700     03  FILLER                  PIC X(3)    VALUE SPACE.                 
116800     03  FILLER                  PIC X(12) VALUE 'SERVGRAD TEO'.          
116900     03  FILLER                  PIC X(5)    VALUE SPACE.                 
117000     03  W001-DET18-SERVGA-TEO   PIC Z9.9.                                
117100     03  FILLER                  PIC X       VALUE SPACE.                 
117200     03  W001-DET18-SERVGA-BTO   PIC X(4)    VALUE SPACE.                 
117300     03  FILLER                  PIC X(5)    VALUE SPACE.                 
117400     03  W001-DET18-SERVGB-TEO   PIC Z9.9.                                
117500     03  FILLER                  PIC X       VALUE SPACE.                 
117600     03  W001-DET18-SERVGB-BTO   PIC X(4)    VALUE SPACE.                 
117700     03  FILLER                  PIC X(5)    VALUE SPACE.                 
117800     03  W001-DET18-SERVGC-TEO   PIC Z9.9.                                
117900     03  FILLER                  PIC X       VALUE SPACE.                 
118000     03  W001-DET18-SERVGC-BTO   PIC X(4)    VALUE SPACE.                 
118100     03  FILLER                  PIC X(5)    VALUE SPACE.                 
118200     03  W001-DET18-SERVGD-TEO   PIC Z9.9.                                
118300     03  FILLER                  PIC X       VALUE SPACE.                 
118400     03  W001-DET18-SERVGD-BTO   PIC X(4)    VALUE SPACE.                 
118500     03  FILLER                  PIC X(5)    VALUE SPACE.                 
118600     03  W001-DET18-SERVGE-TEO   PIC Z9.9.                                
118700     03  FILLER                  PIC X       VALUE SPACE.                 
118800     03  W001-DET18-SERVGE-BTO   PIC X(4)    VALUE SPACE.                 
118900     03  FILLER                  PIC X(5)    VALUE SPACE.                 
119000     03  W001-DET18-SERVGF-TEO   PIC Z9.9.                                
119100     03  FILLER                  PIC X       VALUE SPACE.                 
119200     03  W001-DET18-SERVGF-BTO   PIC X(4)    VALUE SPACE.                 
119300     03  FILLER                  PIC X(5)    VALUE SPACE.                 
119400     03  W001-DET18-SERVGG-TEO   PIC Z9.9.                                
119500     03  FILLER                  PIC X       VALUE SPACE.                 
119600     03  W001-DET18-SERVGG-BTO   PIC X(4)    VALUE SPACE.                 
119700     03  FILLER                  PIC X(5)    VALUE SPACE.                 
119800     03  W001-DET18-SERVGH-TEO   PIC Z9.9.                                
119900     03  FILLER                  PIC X       VALUE SPACE.                 
120000     03  W001-DET18-SERVGH-BTO   PIC X(4)    VALUE SPACE.                 
120100     03  FILLER                  PIC X(11)   VALUE SPACE.                 
120200     03  W001-DET18-TOT          PIC Z9.9.                                
120300     03  FILLER                  PIC X       VALUE SPACE.                 
120400     03  W001-DET18-P-TOT        PIC Z(2)9.9 BLANK WHEN ZERO.             
120500     EJECT                                                                
120600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
120700*                                                                         
120800     EJECT                                                                
120900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
121000     SKIP3                                                                
121100 01  NYCKLAR-TILL-DLI.                                                    
121200     03  W-IDDC-B6-X.                                                     
121300         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
121400     SKIP2                                                                
121500*    --- STATUS-KOD FRÅN IMS                                              
121600 01  STATUS-WS                   PIC XX.                                  
121700     88  SEGMENT-FINNS                       VALUE '  '.                  
121800     88  BASEN-SLUT                          VALUE 'GB'.                  
121900     SKIP2                                                                
122000 01  GODK-STATUSKODER.                                                    
122100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
122200     SKIP3                                                                
122300 01  SSA1                        PIC X(64).                               
122400 01  SSA2                        PIC X(64).                               
122500     EJECT                                                                
122600*    --- IMS FUNKTIONSKODER                                               
122700*01  -COPY W0003                                                          
122800     EJECT                                                                
122900*    ---  DLI INPUT-OUTPUT AREA                                           
123000 01  FILLER               PIC X(16)   VALUE 'WDB6   AREA'.                
123100 01   DLI-IO-AREA-B6      PIC X(900).                                     
123200 01   DLI-IO-AREA-B601    REDEFINES DLI-IO-AREA-B6.                       
123300*     03  -COPY WDB601                                                    
123400     EJECT                                                                
123500 01   DLI-IO-AREA-B616    REDEFINES DLI-IO-AREA-B6.                       
123600*     03  -COPY WDB616                                                    
123700     EJECT                                                                
123800 LINKAGE SECTION.                                                         
123900                                                                          
124000*01  -COPY W0008      -PRE WDB6-                                          
124100     05  FILLER                  PIC X.                                   
124200     EJECT                                                                
124300 PROCEDURE DIVISION  USING WDB6-PCB.                                      
124400 MAIN SECTION.                                                            
124500     ENTRY 'DLITCBL' USING WDB6-PCB.                                      
124600                                                                          
124700     PERFORM A-INIT                                                       
124800                                                                          
124900     PERFORM S01-LAS-W23170                                               
125000                                                                          
125100     PERFORM UNTIL END-OF-W23170                                          
125200       PERFORM B-SKAPA-LISTA                                              
125300       PERFORM C-SKRIV-LISTA                                              
125400     END-PERFORM                                                          
125500     PERFORM Z-FINIT                                                      
125600                                                                          
125700     MOVE ZERO TO RETURN-CODE                                             
125800     GOBACK                                                               
125900     .                                                                    
126000     EJECT                                                                
126100 A-INIT SECTION.                                                          
126200                                                                          
126300     OPEN INPUT  W23170                                                   
126400     OPEN OUTPUT W23171-001                                               
126500                                                                          
126600     CALL DATKORT USING PROG-ID KORT-ID DATUMKORT                         
126700     MOVE D-AAR    TO DAGENS-AAR                                          
126800                      D-VECKA-AAR                                         
126900     MOVE D-MAANAD TO DAGENS-MAANAD                                       
127000     MOVE D-VECKA  TO D-VECKA-VECKA                                       
127100     MOVE D-DAG    TO DAGENS-DAG                                          
127200     MOVE DAGENS-DATUM TO W001-DATUM                                      
127300     MOVE DAGENS-VECKA TO W001-AKTUELL-VECKA                              
127400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
127500                                                                          
127600     PERFORM AA-LADDA-DC-TABELL                                           
127700     .                                                                    
127800     EJECT                                                                
127900 AA-LADDA-DC-TABELL SECTION.                                              
128000                                                                          
128100                                                                          
128200     INITIALIZE WS-DC-TABELL                                              
128300     MOVE 1 TO IDDC-IX                                                    
128400     PERFORM IMS-GET-WDB6                                                 
128500                                                                          
128600     PERFORM UNTIL BASEN-SLUT                                             
128700                OR IDDC-IX > IDDC-IX-MAX                                  
128800                                                                          
128900        IF WDB6-SEG-NAME-FB = 'WDB601  '                                  
129000           MOVE DCS-IDDC     TO W-IDDC                                    
129100           MOVE DCS-IDLISTNR TO W-IDLISTNR                                
129200        END-IF                                                            
129300                                                                          
129400        IF WDB6-SEG-NAME-FB = 'WDB616  '                                  
129500           MOVE W-IDDC           TO WS-IDDC        (IDDC-IX)              
129600           MOVE W-IDLISTNR       TO WS-IDLISTNR    (IDDC-IX)              
129700           MOVE REF-IDDC-REF     TO WS-IDDC-REF    (IDDC-IX)              
129800           MOVE REF-KVDLTID-TOT  TO WS-KVDAGAR-TOT (IDDC-IX)              
129900           ADD 1 TO IDDC-IX                                               
130000        END-IF                                                            
130100                                                                          
130200        PERFORM IMS-GET-WDB6                                              
130300     END-PERFORM                                                          
130400                                                                          
130500     IF IDDC-IX > IDDC-IX-MAX                                             
130600        MOVE 'DC-TABELL FULL'    TO WS-FELTEXT                            
130700        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
130800     END-IF                                                               
130900     .                                                                    
131000     EJECT                                                                
131100 B-SKAPA-LISTA SECTION.                                                   
131200                                                                          
131300     MOVE IN-IDDC TO W001-AKTUELLT-IDDC                                   
131400                                                                          
131500     MOVE +1 TO IDDC-IX                                                   
131600     PERFORM UNTIL IDDC-IX > IDDC-IX-MAX OR                               
131700                   (IN-IDDC     = WS-IDDC     (IDDC-IX)  AND              
131800                    IN-IDDC-REF = WS-IDDC-REF (IDDC-IX)  )                
131900                                                                          
132000        ADD +1 TO IDDC-IX                                                 
132100     END-PERFORM                                                          
132200     IF IDDC-IX NOT > IDDC-IX-MAX                                         
132300        MOVE IDDC-IX TO SDC-IX                                            
132400     ELSE                                                                 
132500        MOVE 'IDDC/IDDC-REF SAKNAS' TO WS-FELTEXT                         
132600        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
132700     END-IF                                                               
132800                                                                          
132900     MOVE 'DC-LAGER'           TO W001-SDC-LAGER                          
133000     MOVE 'W23171-'            TO W001-LISTNR (1:7)                       
133100                                  W001-LISTID (1:7)                       
133200                                                                          
133300     MOVE WS-IDLISTNR(IDDC-IX) TO W001-LISTNR (8:3)                       
133400                                  W001-LISTID (8:3)                       
133500     PERFORM S30-SKRIV-DAP1                                               
133600     PERFORM S31-SKRIV-DAP2                                               
133700     PERFORM BA-NOLLSTALL                                                 
133800                                                                          
133900     MOVE IN-IDDC TO WS-PREV-IDDC                                         
134000     PERFORM UNTIL END-OF-W23170    OR IN-IDDC NOT = WS-PREV-IDDC         
134100        PERFORM BB-SKAPA-TABELLER                                         
134200        PERFORM S01-LAS-W23170                                            
134300     END-PERFORM                                                          
134400                                                                          
134500     PERFORM BC-SUMMERA                                                   
134600     .                                                                    
134700     EJECT                                                                
134800 BA-NOLLSTALL SECTION.                                                    
134900******************************************************************        
135000*  LISTAN BESTÅR AV ARTIKELUPPGIFTER PER PRISKLASS OCH           *        
135100*  FREKVENSKLASS                                                 *        
135200*     PRIS-KLASSER   = 1 2 3 4 5 6 7 8 9                         *        
135300*     FREKV-KLASSER  = A B C D E F G                             *        
135400*  SUMMERING GÖRS PER PRISKLASS OBEROENDE AV FREKVENSKLASS       *        
135500*                 PER FREKVENSKLASS OBEROENDE AV PRISKLASS       *        
135600*                 TOTAL-SUMMERING                                *        
135700* ****************************************************************        
135800                                                                          
135900******* NOLLSTÄLLNING AV 63 'RUTOR' PER PRISKLASS/FREKVKLASS              
136000                                                                          
136100     MOVE +1  TO ART-IX                                                   
136200*    MOVE +63 TO ART-IX-MAX                                               
136300     MOVE +72 TO ART-IX-MAX                                               
136400     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
136500        MOVE ZERO TO     ART-KVANT-AKT(ART-IX)                            
136600                         ART-KVANT-PAS(ART-IX)                            
136700                         ART-PROC-KVANT-A(ART-IX)                         
136800                         ART-PROC-KVANT-P(ART-IX)                         
136900                         ART-KVDISP-AKT(ART-IX)                           
137000                         ART-PROC-KVDISP-A(ART-IX)                        
137100                         ART-KVDISP-PAS(ART-IX)                           
137200                         ART-PROC-KVDISP-P(ART-IX)                        
137300                         ART-LS-AKT(ART-IX)                               
137400                         ART-PROC-LS-A(ART-IX)                            
137500                         ART-LS-PAS(ART-IX)                               
137600                         ART-PROC-LS-P(ART-IX)                            
137700                         ART-AK-AKT(ART-IX)                               
137800                         ART-PROC-AK-A(ART-IX)                            
137900                         ART-AK-PAS(ART-IX)                               
138000                         ART-PROC-AK-P(ART-IX)                            
138100                         ART-OLAGER(ART-IX)                               
138200                         ART-PROC-OLAGER(ART-IX)                          
138300                         ART-SLAGER(ART-IX)                               
138400                         ART-PROC-SLAGER(ART-IX)                          
138500                         ART-MLAGER(ART-IX)                               
138600                         ART-PROC-MLAGER(ART-IX)                          
138700                         ART-KVOT(ART-IX)                                 
138800                         ART-PROC-KVOT(ART-IX)                            
138900                         ART-SPLIT(ART-IX)                                
139000                         ART-OMSHAST-DISP(ART-IX)                         
139100                         ART-OMSHAST-PROC-D(ART-IX)                       
139200                         ART-OMSHAST-LS(ART-IX)                           
139300                         ART-OMSHAST-PROC-LS(ART-IX)                      
139400                         ART-SERVG-TOT(ART-IX)                            
139500                         ART-SERVG-AKT(ART-IX)                            
139600                         ART-SERVG-PAS(ART-IX)                            
139700                         ART-SERVG-TEO(ART-IX)                            
139800                         WS-ART-SLAGER(ART-IX)                            
139900                         WS-ART-OLAGER(ART-IX)                            
140000                         WS-ART-MLAGER(ART-IX)                            
140100                         WS-ART-KVLS-AKT(ART-IX)                          
140200                         WS-ART-KVLS-PAS(ART-IX)                          
140300                         WS-ART-LS-AKT(ART-IX)                            
140400                         WS-ART-LS-PAS(ART-IX)                            
140500                         WS-ART-LS-PR-AKT(ART-IX)                         
140600                         WS-ART-LS-PR-PAS(ART-IX)                         
140700                         WS-ART-KVDISP-AKT(ART-IX)                        
140800                         WS-ART-KVDISP-PAS(ART-IX)                        
140900                         WS-ART-KVDISP-PR-AKT(ART-IX)                     
141000                         WS-ART-KVDISP-PR-PAS(ART-IX)                     
141100                         WS-ART-KVOKS-AKT(ART-IX)                         
141200                         WS-ART-KVOKS-PAS(ART-IX)                         
141300                         WS-ART-OK-PR-AKT(ART-IX)                         
141400                         WS-ART-OK-PR-PAS(ART-IX)                         
141500                         WS-ART-KVAKS-AKT(ART-IX)                         
141600                         WS-ART-KVAKS-PAS(ART-IX)                         
141700                         WS-ART-AK-PR-AKT(ART-IX)                         
141800                         WS-ART-AK-PR-PAS(ART-IX)                         
141900                         WS-ART-KVOI(ART-IX)                              
142000                         WS-ART-KVOI-AKT(ART-IX)                          
142100                         WS-ART-KVOI-PAS(ART-IX)                          
142200                         WS-ART-KVOI-TEO(ART-IX)                          
142300                         WS-ART-KVOI-SAK(ART-IX)                          
142400                         WS-ART-KVOI-CDC-AKT(ART-IX)                      
142500                         WS-ART-KVOI-CDC-PAS(ART-IX)                      
142600                         WS-ART-KVOI-CDC-TEO(ART-IX)                      
142700                         WS-ART-KVOI-CDC-SAK(ART-IX)                      
142800                                                                          
142900        ADD +1 TO ART-IX                                                  
143000     END-PERFORM                                                          
143100                                                                          
143200******* NOLLSTÄLLNING AV 9 'RUTOR' TOTALSUMMA PER PRISKLASS               
143300*******                          OBEROENDE AV FREKVENSKLASS               
143400                                                                          
143500     MOVE +1 TO PSUM-IX                                                   
143600     MOVE +9 TO PSUM-IX-MAX                                               
143700     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
143800        MOVE ZERO TO     PSUM-KVANT-AKT(PSUM-IX)                          
143900                         PSUM-KVANT-PAS(PSUM-IX)                          
144000                         PSUM-PROC-KVANT-A(PSUM-IX)                       
144100                         PSUM-PROC-KVANT-P(PSUM-IX)                       
144200                         PSUM-KVDISP-AKT(PSUM-IX)                         
144300                         PSUM-PROC-KVDISP-A(PSUM-IX)                      
144400                         PSUM-KVDISP-PAS(PSUM-IX)                         
144500                         PSUM-PROC-KVDISP-P(PSUM-IX)                      
144600                         PSUM-LS-AKT(PSUM-IX)                             
144700                         PSUM-PROC-LS-A(PSUM-IX)                          
144800                         PSUM-LS-PAS(PSUM-IX)                             
144900                         PSUM-PROC-LS-P(PSUM-IX)                          
145000                         PSUM-AK-AKT(PSUM-IX)                             
145100                         PSUM-PROC-AK-A(PSUM-IX)                          
145200                         PSUM-AK-PAS(PSUM-IX)                             
145300                         PSUM-PROC-AK-P(PSUM-IX)                          
145400                         PSUM-OLAGER(PSUM-IX)                             
145500                         PSUM-PROC-OLAGER(PSUM-IX)                        
145600                         PSUM-SLAGER(PSUM-IX)                             
145700                         PSUM-PROC-SLAGER(PSUM-IX)                        
145800                         PSUM-MLAGER(PSUM-IX)                             
145900                         PSUM-PROC-MLAGER(PSUM-IX)                        
146000                         PSUM-KVOT(PSUM-IX)                               
146100                         PSUM-PROC-KVOT(PSUM-IX)                          
146200                         PSUM-SPLIT(PSUM-IX)                              
146300                         PSUM-OMSHAST-DISP(PSUM-IX)                       
146400                         PSUM-OMSHAST-PROC-D(PSUM-IX)                     
146500                         PSUM-OMSHAST-LS(PSUM-IX)                         
146600                         PSUM-OMSHAST-PROC-LS(PSUM-IX)                    
146700                         PSUM-SERVG-TOT(PSUM-IX)                          
146800                         PSUM-SERVG-AKT(PSUM-IX)                          
146900                         PSUM-SERVG-PAS(PSUM-IX)                          
147000                         PSUM-SERVG-TEO(PSUM-IX)                          
147100                         WS-PSUM-SLAGER(PSUM-IX)                          
147200                         WS-PSUM-OLAGER(PSUM-IX)                          
147300                         WS-PSUM-MLAGER(PSUM-IX)                          
147400                         WS-PSUM-LS-AKT(PSUM-IX)                          
147500                         WS-PSUM-LS-PAS(PSUM-IX)                          
147600                         WS-PSUM-LS-PR-AKT(PSUM-IX)                       
147700                         WS-PSUM-LS-PR-PAS(PSUM-IX)                       
147800                         WS-PSUM-KVDISP-AKT(PSUM-IX)                      
147900                         WS-PSUM-KVDISP-PAS(PSUM-IX)                      
148000                         WS-PSUM-KVDISP-PR-AKT(PSUM-IX)                   
148100                         WS-PSUM-KVDISP-PR-PAS(PSUM-IX)                   
148200                         WS-PSUM-KVOKS-AKT(PSUM-IX)                       
148300                         WS-PSUM-KVOKS-PAS(PSUM-IX)                       
148400                         WS-PSUM-OK-PR-AKT(PSUM-IX)                       
148500                         WS-PSUM-OK-PR-PAS(PSUM-IX)                       
148600                         WS-PSUM-KVAKS-AKT(PSUM-IX)                       
148700                         WS-PSUM-KVAKS-PAS(PSUM-IX)                       
148800                         WS-PSUM-AK-PR-AKT(PSUM-IX)                       
148900                         WS-PSUM-AK-PR-PAS(PSUM-IX)                       
149000                         WS-PSUM-KVOI(PSUM-IX)                            
149100                         WS-PSUM-KVOI-AKT(PSUM-IX)                        
149200                         WS-PSUM-KVOI-PAS(PSUM-IX)                        
149300                         WS-PSUM-KVOI-TEO(PSUM-IX)                        
149400                         WS-PSUM-KVOI-SAK(PSUM-IX)                        
149500                         WS-PSUM-KVOI-CDC-AKT(PSUM-IX)                    
149600                         WS-PSUM-KVOI-CDC-PAS(PSUM-IX)                    
149700                         WS-PSUM-KVOI-CDC-TEO(PSUM-IX)                    
149800                         WS-PSUM-KVOI-CDC-SAK(PSUM-IX)                    
149900                                                                          
150000        ADD +1 TO PSUM-IX                                                 
150100     END-PERFORM                                                          
150200                                                                          
150300******* NOLLSTÄLLNING AV 7 'RUTOR' TOTALSUMMA PER FREKVENSKLASS           
150400*******                            OBEROENDE AV PRISKLASS                 
150500                                                                          
150600     MOVE +1 TO FSUM-IX                                                   
150700     MOVE +8 TO FSUM-IX-MAX                                               
150800     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
150900        MOVE ZERO TO     FSUM-KVANT-AKT(FSUM-IX)                          
151000                         FSUM-KVANT-PAS(FSUM-IX)                          
151100                         FSUM-PROC-KVANT-A(FSUM-IX)                       
151200                         FSUM-PROC-KVANT-P(FSUM-IX)                       
151300                         FSUM-KVDISP-AKT(FSUM-IX)                         
151400                         FSUM-PROC-KVDISP-A(FSUM-IX)                      
151500                         FSUM-KVDISP-PAS(FSUM-IX)                         
151600                         FSUM-PROC-KVDISP-P(FSUM-IX)                      
151700                         FSUM-LS-AKT(FSUM-IX)                             
151800                         FSUM-PROC-LS-A(FSUM-IX)                          
151900                         FSUM-LS-PAS(FSUM-IX)                             
152000                         FSUM-PROC-LS-P(FSUM-IX)                          
152100                         FSUM-AK-AKT(FSUM-IX)                             
152200                         FSUM-PROC-AK-A(FSUM-IX)                          
152300                         FSUM-AK-PAS(FSUM-IX)                             
152400                         FSUM-PROC-AK-P(FSUM-IX)                          
152500                         FSUM-OLAGER(FSUM-IX)                             
152600                         FSUM-PROC-OLAGER(FSUM-IX)                        
152700                         FSUM-SLAGER(FSUM-IX)                             
152800                         FSUM-PROC-SLAGER(FSUM-IX)                        
152900                         FSUM-MLAGER(FSUM-IX)                             
153000                         FSUM-PROC-MLAGER(FSUM-IX)                        
153100                         FSUM-KVOT(FSUM-IX)                               
153200                         FSUM-PROC-KVOT(FSUM-IX)                          
153300                         FSUM-SPLIT(FSUM-IX)                              
153400                         FSUM-OMSHAST-DISP(FSUM-IX)                       
153500                         FSUM-OMSHAST-PROC-D(FSUM-IX)                     
153600                         FSUM-OMSHAST-LS(FSUM-IX)                         
153700                         FSUM-OMSHAST-PROC-LS(FSUM-IX)                    
153800                         FSUM-SERVG-TOT(FSUM-IX)                          
153900                         FSUM-SERVG-AKT(FSUM-IX)                          
154000                         FSUM-SERVG-PAS(FSUM-IX)                          
154100                         FSUM-SERVG-TEO(FSUM-IX)                          
154200                         WS-FSUM-SLAGER(FSUM-IX)                          
154300                         WS-FSUM-OLAGER(FSUM-IX)                          
154400                         WS-FSUM-MLAGER(FSUM-IX)                          
154500                         WS-FSUM-LS-AKT(FSUM-IX)                          
154600                         WS-FSUM-LS-PAS(FSUM-IX)                          
154700                         WS-FSUM-LS-PR-AKT(FSUM-IX)                       
154800                         WS-FSUM-LS-PR-PAS(FSUM-IX)                       
154900                         WS-FSUM-KVDISP-AKT(FSUM-IX)                      
155000                         WS-FSUM-KVDISP-PAS(FSUM-IX)                      
155100                         WS-FSUM-KVDISP-PR-AKT(FSUM-IX)                   
155200                         WS-FSUM-KVDISP-PR-PAS(FSUM-IX)                   
155300                         WS-FSUM-KVOKS-AKT(FSUM-IX)                       
155400                         WS-FSUM-KVOKS-PAS(FSUM-IX)                       
155500                         WS-FSUM-OK-PR-AKT(FSUM-IX)                       
155600                         WS-FSUM-OK-PR-PAS(FSUM-IX)                       
155700                         WS-FSUM-KVAKS-AKT(FSUM-IX)                       
155800                         WS-FSUM-KVAKS-PAS(FSUM-IX)                       
155900                         WS-FSUM-AK-PR-AKT(FSUM-IX)                       
156000                         WS-FSUM-AK-PR-PAS(FSUM-IX)                       
156100                         WS-FSUM-KVOI(FSUM-IX)                            
156200                         WS-FSUM-KVOI-AKT(FSUM-IX)                        
156300                         WS-FSUM-KVOI-PAS(FSUM-IX)                        
156400                         WS-FSUM-KVOI-TEO(FSUM-IX)                        
156500                         WS-FSUM-KVOI-SAK(FSUM-IX)                        
156600                         WS-FSUM-KVOI-CDC-AKT(FSUM-IX)                    
156700                         WS-FSUM-KVOI-CDC-PAS(FSUM-IX)                    
156800                         WS-FSUM-KVOI-CDC-TEO(FSUM-IX)                    
156900                         WS-FSUM-KVOI-CDC-SAK(FSUM-IX)                    
157000                                                                          
157100        ADD +1 TO FSUM-IX                                                 
157200     END-PERFORM                                                          
157300                                                                          
157400******* NOLLSTÄLLNING AV TOTALRUTA                                        
157500                                                                          
157600     MOVE ZERO TO     TOT-KVANT-AKT                                       
157700                      TOT-KVANT-PAS                                       
157800                      TOT-KVDISP-AKT                                      
157900                      TOT-KVDISP-PAS                                      
158000                      TOT-LS-AKT                                          
158100                      TOT-LS-PAS                                          
158200                      TOT-AK-AKT                                          
158300                      TOT-AK-PAS                                          
158400                      TOT-SLAGER                                          
158500                      TOT-MLAGER                                          
158600                      TOT-OLAGER                                          
158700                      TOT-KVOT                                            
158800                      TOT-PROC-OLAGER                                     
158900                      TOT-PROC-MLAGER                                     
159000                      TOT-PROC-SLAGER                                     
159100                      TOT-SPLIT                                           
159200                      TOT-OMSHAST-DISP                                    
159300                      TOT-OMSHAST-LS                                      
159400                      TOT-SERVG-TOT                                       
159500                      TOT-SERVG-AKT                                       
159600                      TOT-SERVG-PAS                                       
159700                      TOT-SERVG-TEO                                       
159800                      WS-TOT-SLAGER                                       
159900                      WS-TOT-OLAGER                                       
160000                      WS-TOT-MLAGER                                       
160100                      WS-TOT-LS-AKT                                       
160200                      WS-TOT-LS-PAS                                       
160300                      WS-TOT-LS-PR-AKT                                    
160400                      WS-TOT-LS-PR-PAS                                    
160500                      WS-TOT-KVDISP-AKT                                   
160600                      WS-TOT-KVDISP-PAS                                   
160700                      WS-TOT-KVDISP-PR-AKT                                
160800                      WS-TOT-KVDISP-PR-PAS                                
160900                      WS-TOT-KVOKS-AKT                                    
161000                      WS-TOT-KVOKS-PAS                                    
161100                      WS-TOT-OK-PR-AKT                                    
161200                      WS-TOT-OK-PR-PAS                                    
161300                      WS-TOT-KVAKS-AKT                                    
161400                      WS-TOT-KVAKS-PAS                                    
161500                      WS-TOT-AK-PR-AKT                                    
161600                      WS-TOT-AK-PR-PAS                                    
161700                      WS-TOT-KVOI                                         
161800                      WS-TOT-KVOI-AKT                                     
161900                      WS-TOT-KVOI-PAS                                     
162000                      WS-TOT-KVOI-TEO                                     
162100                      WS-TOT-KVOI-SAK                                     
162200                      WS-TOT-KVOI-CDC-AKT                                 
162300                      WS-TOT-KVOI-CDC-PAS                                 
162400                      WS-TOT-KVOI-CDC-TEO                                 
162500                      WS-TOT-KVOI-CDC-SAK                                 
162600     .                                                                    
162700     EJECT                                                                
162800 BB-SKAPA-TABELLER SECTION.                                               
162900                                                                          
163000     PERFORM BBA-SAETT-ART-IX                                             
163100     IF SW-ARTIKEL-SAKNAS-WDK7 = JA                                       
163200        PERFORM BBC-UPPDAT-SAKN-ART                                       
163300     END-IF                                                               
163400     IF ART-IX > ZERO                                                     
163500        PERFORM BBB-UPPDATERA-TABELLER                                    
163600     END-IF                                                               
163700     .                                                                    
163800     EJECT                                                                
163900 BBA-SAETT-ART-IX SECTION.                                                
164000******************************************************************        
164100* ART-IX SÄTTS BEROENDE PÅ PRISKLASS OCH FREKVENSKLASS           *        
164200******************************************************************        
164300                                                                          
164400     MOVE NEJ TO SW-ARTIKEL-SAKNAS-WDK7                                   
164500     EVALUATE TRUE                                                        
164600     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'A'                         
164700          MOVE +1 TO ART-IX                                               
164800     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'B'                         
164900          MOVE +2 TO ART-IX                                               
165000     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'C'                         
165100          MOVE +3 TO ART-IX                                               
165200     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'D'                         
165300          MOVE +4 TO ART-IX                                               
165400     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'E'                         
165500          MOVE +5 TO ART-IX                                               
165600     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'F'                         
165700          MOVE +6 TO ART-IX                                               
165800     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'G'                         
165900          MOVE +7 TO ART-IX                                               
166000     WHEN IN-KDPRISKL = '1' AND IN-KDFREKKL = 'H'                         
166100          MOVE +8 TO ART-IX                                               
166200                                                                          
166300     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'A'                         
166400          MOVE +9 TO ART-IX                                               
166500     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'B'                         
166600          MOVE +10 TO ART-IX                                              
166700     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'C'                         
166800          MOVE +11 TO ART-IX                                              
166900     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'D'                         
167000          MOVE +12 TO ART-IX                                              
167100     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'E'                         
167200          MOVE +13 TO ART-IX                                              
167300     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'F'                         
167400          MOVE +14 TO ART-IX                                              
167500     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'G'                         
167600          MOVE +15 TO ART-IX                                              
167700     WHEN IN-KDPRISKL = '2' AND IN-KDFREKKL = 'H'                         
167800          MOVE +16 TO ART-IX                                              
167900                                                                          
168000     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'A'                         
168100          MOVE +17 TO ART-IX                                              
168200     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'B'                         
168300          MOVE +18 TO ART-IX                                              
168400     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'C'                         
168500          MOVE +19 TO ART-IX                                              
168600     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'D'                         
168700          MOVE +20 TO ART-IX                                              
168800     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'E'                         
168900          MOVE +21 TO ART-IX                                              
169000     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'F'                         
169100          MOVE +22 TO ART-IX                                              
169200     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'G'                         
169300          MOVE +23 TO ART-IX                                              
169400     WHEN IN-KDPRISKL = '3' AND IN-KDFREKKL = 'H'                         
169500          MOVE +24 TO ART-IX                                              
169600                                                                          
169700     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'A'                         
169800          MOVE +25 TO ART-IX                                              
169900     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'B'                         
170000          MOVE +26 TO ART-IX                                              
170100     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'C'                         
170200          MOVE +27 TO ART-IX                                              
170300     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'D'                         
170400          MOVE +28 TO ART-IX                                              
170500     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'E'                         
170600          MOVE +29 TO ART-IX                                              
170700     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'F'                         
170800          MOVE +30 TO ART-IX                                              
170900     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'G'                         
171000          MOVE +31 TO ART-IX                                              
171100     WHEN IN-KDPRISKL = '4' AND IN-KDFREKKL = 'H'                         
171200          MOVE +32 TO ART-IX                                              
171300                                                                          
171400     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'A'                         
171500          MOVE +33 TO ART-IX                                              
171600     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'B'                         
171700          MOVE +34 TO ART-IX                                              
171800     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'C'                         
171900          MOVE +35 TO ART-IX                                              
172000     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'D'                         
172100          MOVE +36 TO ART-IX                                              
172200     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'E'                         
172300          MOVE +37 TO ART-IX                                              
172400     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'F'                         
172500          MOVE +38 TO ART-IX                                              
172600     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'G'                         
172700          MOVE +39 TO ART-IX                                              
172800     WHEN IN-KDPRISKL = '5' AND IN-KDFREKKL = 'H'                         
172900          MOVE +40 TO ART-IX                                              
173000                                                                          
173100     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'A'                         
173200          MOVE +41 TO ART-IX                                              
173300     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'B'                         
173400          MOVE +42 TO ART-IX                                              
173500     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'C'                         
173600          MOVE +43 TO ART-IX                                              
173700     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'D'                         
173800          MOVE +44 TO ART-IX                                              
173900     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'E'                         
174000          MOVE +45 TO ART-IX                                              
174100     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'F'                         
174200          MOVE +46 TO ART-IX                                              
174300     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'G'                         
174400          MOVE +47 TO ART-IX                                              
174500     WHEN IN-KDPRISKL = '6' AND IN-KDFREKKL = 'H'                         
174600          MOVE +48 TO ART-IX                                              
174700                                                                          
174800     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'A'                         
174900          MOVE +49 TO ART-IX                                              
175000     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'B'                         
175100          MOVE +50 TO ART-IX                                              
175200     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'C'                         
175300          MOVE +51 TO ART-IX                                              
175400     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'D'                         
175500          MOVE +52 TO ART-IX                                              
175600     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'E'                         
175700          MOVE +53 TO ART-IX                                              
175800     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'F'                         
175900          MOVE +54 TO ART-IX                                              
176000     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'G'                         
176100          MOVE +55 TO ART-IX                                              
176200     WHEN IN-KDPRISKL = '7' AND IN-KDFREKKL = 'H'                         
176300          MOVE +56 TO ART-IX                                              
176400                                                                          
176500     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'A'                         
176600          MOVE +57 TO ART-IX                                              
176700     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'B'                         
176800          MOVE +58 TO ART-IX                                              
176900     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'C'                         
177000          MOVE +59 TO ART-IX                                              
177100     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'D'                         
177200          MOVE +60 TO ART-IX                                              
177300     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'E'                         
177400          MOVE +61 TO ART-IX                                              
177500     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'F'                         
177600          MOVE +62 TO ART-IX                                              
177700     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'G'                         
177800          MOVE +63 TO ART-IX                                              
177900     WHEN IN-KDPRISKL = '8' AND IN-KDFREKKL = 'H'                         
178000          MOVE +64 TO ART-IX                                              
178100                                                                          
178200     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'A'                         
178300          MOVE +65 TO ART-IX                                              
178400     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'B'                         
178500          MOVE +66 TO ART-IX                                              
178600     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'C'                         
178700          MOVE +67 TO ART-IX                                              
178800     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'D'                         
178900          MOVE +68 TO ART-IX                                              
179000     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'E'                         
179100          MOVE +69 TO ART-IX                                              
179200     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'F'                         
179300          MOVE +70 TO ART-IX                                              
179400     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'G'                         
179500          MOVE +71 TO ART-IX                                              
179600     WHEN IN-KDPRISKL = '9' AND IN-KDFREKKL = 'H'                         
179700          MOVE +72 TO ART-IX                                              
179800     WHEN OTHER                                                           
179900          MOVE ZERO TO ART-IX                                             
180000          IF IN-KDPRISKL = SPACE AND IN-KDFREKKL = SPACE                  
180100             MOVE JA TO SW-ARTIKEL-SAKNAS-WDK7                            
180200          END-IF                                                          
180300     END-EVALUATE                                                         
180400     .                                                                    
180500     EJECT                                                                
180600 BBB-UPPDATERA-TABELLER SECTION.                                          
180700                                                                          
180800*********  ANTAL ARTIKLAR                                                 
180900     IF IN-KDREFSTA = 'A'                                                 
181000        ADD +1 TO ART-KVANT-AKT(ART-IX)                                   
181100     ELSE                                                                 
181200        IF IN-KDREFSTA = 'P' AND IN-KVLS > ZERO                           
181300           ADD +1 TO ART-KVANT-PAS(ART-IX)                                
181400        END-IF                                                            
181500     END-IF                                                               
181600                                                                          
181700*********  DISP-LAGER LAGERVÄRDE AK-VÄRDE OKS-VÄRDE/ARTIKEL               
181800     IF IN-KDREFSTA = 'A'                                                 
181900        ADD IN-KVLS         TO WS-ART-KVLS-AKT(ART-IX)                    
182000        ADD IN-KVOKS        TO WS-ART-KVOKS-AKT(ART-IX)                   
182100        COMPUTE WS-KVDISP = IN-KVLS - IN-KVOKS                            
182200        COMPUTE WS-SUMMA = WS-KVDISP * IN-PRARTSTD                        
182300        ADD WS-SUMMA TO WS-ART-KVDISP-PR-AKT(ART-IX)                      
182400                                                                          
182500        COMPUTE WS-SUMMA = IN-KVOKS * IN-PRARTSTD                         
182600        ADD WS-SUMMA TO WS-ART-OK-PR-AKT(ART-IX)                          
182700                                                                          
182800        COMPUTE WS-SUMMA = IN-KVLS * IN-PRARTSTD                          
182900        ADD WS-SUMMA TO WS-ART-LS-PR-AKT(ART-IX)                          
183000                                                                          
183100        COMPUTE WS-KVAKS = IN-KVAKS-SDC + IN-KVAKS-PAV                    
183200        ADD WS-KVAKS        TO WS-ART-KVAKS-AKT(ART-IX)                   
183300        COMPUTE WS-SUMMA = WS-KVAKS * IN-PRARTSTD                         
183400        ADD WS-SUMMA  TO WS-ART-AK-PR-AKT(ART-IX)                         
183500     ELSE                                                                 
183600        IF IN-KDREFSTA = 'P'                                              
183700           ADD IN-KVLS         TO WS-ART-KVLS-PAS(ART-IX)                 
183800           ADD IN-KVOKS        TO WS-ART-KVOKS-PAS(ART-IX)                
183900           COMPUTE WS-KVDISP = IN-KVLS - IN-KVOKS                         
184000           COMPUTE WS-SUMMA = WS-KVDISP * IN-PRARTSTD                     
184100           ADD WS-SUMMA TO WS-ART-KVDISP-PR-PAS(ART-IX)                   
184200                                                                          
184300           COMPUTE WS-SUMMA = IN-KVOKS * IN-PRARTSTD                      
184400           ADD WS-SUMMA  TO WS-ART-OK-PR-PAS(ART-IX)                      
184500                                                                          
184600           COMPUTE WS-SUMMA = IN-KVLS * IN-PRARTSTD                       
184700           ADD WS-SUMMA  TO WS-ART-LS-PR-PAS(ART-IX)                      
184800                                                                          
184900           COMPUTE WS-KVAKS = IN-KVAKS-SDC + IN-KVAKS-PAV                 
185000           ADD WS-KVAKS TO WS-ART-KVAKS-PAS(ART-IX)                       
185100           COMPUTE WS-SUMMA = WS-KVAKS * IN-PRARTSTD                      
185200           ADD WS-SUMMA TO WS-ART-AK-PR-PAS(ART-IX)                       
185300        END-IF                                                            
185400     END-IF                                                               
185500                                                                          
185600*********  OMSÄTTNINGSHASTIGHET                                           
185700     MOVE +1 TO KVOI-IX                                                   
185800     MOVE ZERO TO WS-KVOI-TOT-AAR                                         
185900     PERFORM UNTIL KVOI-IX > 53                                           
186000        ADD IN-KVOI-RULL(KVOI-IX) TO WS-KVOI-TOT-AAR                      
186100        ADD +1 TO KVOI-IX                                                 
186200     END-PERFORM                                                          
186300                                                                          
186400     COMPUTE WS-KVOI = WS-KVOI-TOT-AAR * IN-PRARTSTD                      
186500     ADD WS-KVOI TO WS-ART-KVOI(ART-IX)                                   
186600                                                                          
186700*********  SÄKERHETSLAGER/ARTIKEL                                         
186800     IF IN-KDREFSTA = 'A'                                                 
186900        COMPUTE WS-SUMMA = IN-KVREFPKT * IN-PRARTSTD                      
187000        ADD WS-SUMMA TO WS-ART-SLAGER(ART-IX)                             
187100     END-IF                                                               
187200                                                                          
187300*********  ÖVERLAGER/ARTIKEL                                              
187400     COMPUTE WS-KVDISP = IN-KVLS - IN-KVOKS                               
187500     IF WS-KVDISP > IN-KVREFOVL                                           
187600        COMPUTE WS-OLAGER = WS-KVDISP - IN-KVREFOVL                       
187700        COMPUTE WS-SUMMA = WS-OLAGER * IN-PRARTSTD                        
187800        ADD WS-SUMMA TO WS-ART-OLAGER(ART-IX)                             
187900     END-IF                                                               
188000                                                                          
188100*********  MEDELLAGER/ARTIKEL                                             
188200     COMPUTE WS-KVPB-VECKA-SDC = IN-KVPB-REF / 4.33                       
188300     COMPUTE WS-KVPB-DAG-SDC-NORM = WS-KVPB-VECKA-SDC / 5                 
188400     COMPUTE WS-LT-BEHOV-SDC-NORM = WS-KVDAGAR-TOT(SDC-IX)                
188500                                  * WS-KVPB-DAG-SDC-NORM                  
188600     COMPUTE WS-MLAGER = (IN-KVREFPKT - WS-LT-BEHOV-SDC-NORM)             
188700                        + (IN-KVREFBER / 2)                               
188800     COMPUTE WS-SUMMA = WS-MLAGER * IN-PRARTSTD                           
188900     ADD WS-SUMMA TO WS-ART-MLAGER(ART-IX)                                
189000                                                                          
189100*********  SERVICEGRAD OCH SPLITFAKTOR ORDERRADER/ARTIKEL                 
189200                                                                          
189300     MOVE +1 TO KVOI-IX                                                   
189400     MOVE NEJ TO SW-KVOI-TRAFF                                            
189500     PERFORM UNTIL KVOI-IX > 5                                            
189600        IF IN-TIVV(KVOI-IX) = D-VECKA-VECKA                               
189700           MOVE JA TO SW-KVOI-TRAFF                                       
189800           IF IN-KDREFSTA = 'A'                                           
189900              ADD IN-KVOT-INNEV(KVOI-IX)                                  
190000                                TO WS-ART-KVOI-AKT(ART-IX)                
190100              ADD IN-KVOT-CDC-INNEV(KVOI-IX)                              
190200                                TO WS-ART-KVOI-CDC-AKT(ART-IX)            
190300           ELSE                                                           
190400              IF IN-KDREFSTA = 'P' AND IN-KVLS > ZERO                     
190500                 ADD IN-KVOT-INNEV(KVOI-IX)                               
190600                                TO WS-ART-KVOI-TEO(ART-IX)                
190700                 ADD IN-KVOT-CDC-INNEV(KVOI-IX)                           
190800                                TO WS-ART-KVOI-CDC-TEO(ART-IX)            
190900              ELSE                                                        
191000                 IF IN-KDREFSTA = 'P'                                     
191100                    ADD IN-KVOT-INNEV(KVOI-IX)                            
191200                                   TO WS-ART-KVOI-PAS(ART-IX)             
191300                    ADD IN-KVOT-CDC-INNEV(KVOI-IX)                        
191400                                   TO WS-ART-KVOI-CDC-PAS(ART-IX)         
191500                 ELSE                                                     
191600                    ADD IN-KVOT-INNEV(KVOI-IX)                            
191700                                   TO WS-ART-KVOI-SAK(ART-IX)             
191800                    ADD IN-KVOT-CDC-INNEV(KVOI-IX)                        
191900                                   TO WS-ART-KVOI-CDC-SAK(ART-IX)         
192000                 END-IF                                                   
192100              END-IF                                                      
192200           END-IF                                                         
192300        END-IF                                                            
192400        ADD +1 TO KVOI-IX                                                 
192500     END-PERFORM                                                          
192600                                                                          
192700     IF SW-KVOI-TRAFF = NEJ                                               
192800        MOVE D-VECKA-VECKA TO KVOI-IX                                     
192900        IF IN-KDREFSTA = 'A'                                              
193000           ADD IN-KVOT-RULL(KVOI-IX)                                      
193100                              TO WS-ART-KVOI-AKT(ART-IX)                  
193200           ADD IN-KVOT-CDC-RULL(KVOI-IX)                                  
193300                              TO WS-ART-KVOI-CDC-AKT(ART-IX)              
193400        ELSE                                                              
193500           IF IN-KDREFSTA = 'P' AND IN-KVLS > ZERO                        
193600              ADD IN-KVOT-RULL(KVOI-IX)                                   
193700                              TO WS-ART-KVOI-TEO(ART-IX)                  
193800              ADD IN-KVOT-CDC-RULL(KVOI-IX)                               
193900                              TO WS-ART-KVOI-CDC-TEO(ART-IX)              
194000           ELSE                                                           
194100              IF IN-KDREFSTA = 'P'                                        
194200                 ADD IN-KVOT-RULL(KVOI-IX)                                
194300                                 TO WS-ART-KVOI-PAS(ART-IX)               
194400                 ADD IN-KVOT-CDC-RULL(KVOI-IX)                            
194500                                 TO WS-ART-KVOI-CDC-PAS(ART-IX)           
194600              ELSE                                                        
194700                 ADD IN-KVOT-RULL(KVOI-IX)                                
194800                                 TO WS-ART-KVOI-SAK(ART-IX)               
194900                 ADD IN-KVOT-CDC-RULL(KVOI-IX)                            
195000                                 TO WS-ART-KVOI-CDC-SAK(ART-IX)           
195100              END-IF                                                      
195200           END-IF                                                         
195300        END-IF                                                            
195400     END-IF                                                               
195500     .                                                                    
195600     EJECT                                                                
195700 BBC-UPPDAT-SAKN-ART SECTION.                                             
195800                                                                          
195900     MOVE +1 TO KVOI-IX                                                   
196000     MOVE NEJ TO SW-KVOI-TRAFF                                            
196100     PERFORM UNTIL KVOI-IX > 5                                            
196200        IF IN-TIVV(KVOI-IX) = D-VECKA-VECKA                               
196300           MOVE JA TO SW-KVOI-TRAFF                                       
196400           ADD IN-KVOT-INNEV(KVOI-IX) TO WS-KVOT-SAKNAS-WDK7              
196500           ADD IN-KVOT-CDC-INNEV(KVOI-IX)                                 
196600                                    TO WS-KVOT-CDC-SAKNAS-WDK7            
196700        END-IF                                                            
196800        ADD +1 TO KVOI-IX                                                 
196900     END-PERFORM                                                          
197000                                                                          
197100     IF SW-KVOI-TRAFF = NEJ                                               
197200        MOVE D-VECKA-VECKA TO KVOI-IX                                     
197300        ADD IN-KVOT-RULL(KVOI-IX) TO WS-KVOT-SAKNAS-WDK7                  
197400        ADD IN-KVOT-CDC-RULL(KVOI-IX) TO WS-KVOT-CDC-SAKNAS-WDK7          
197500     END-IF                                                               
197600     .                                                                    
197700     EJECT                                                                
197800 BC-SUMMERA SECTION.                                                      
197900                                                                          
198000     PERFORM BCA-SUMMERA-RUTA                                             
198100     PERFORM BCB-SUMMERA-PRISKLASS                                        
198200     PERFORM BCC-SUMMERA-FREKVENSKLASS                                    
198300     PERFORM BCD-SUMMERA-TOTAL                                            
198400     PERFORM BCE-BERAKNINGAR-AV-TOTAL                                     
198500     .                                                                    
198600     EJECT                                                                
198700 BCA-SUMMERA-RUTA SECTION.                                                
198800                                                                          
198900     MOVE +1  TO ART-IX                                                   
199000     MOVE +72 TO ART-IX-MAX                                               
199100     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
199200                                                                          
199300*********  DISP LAGER VÄRDE/RUTA                                          
199400        IF WS-ART-KVDISP-PR-AKT(ART-IX) = ZERO                            
199500           CONTINUE                                                       
199600        ELSE                                                              
199700           COMPUTE WS-SUMMA-KR ROUNDED =                                  
199800                          WS-ART-KVDISP-PR-AKT(ART-IX) / 1000             
199900           MOVE WS-SUMMA-KR TO ART-KVDISP-AKT(ART-IX)                     
200000        END-IF                                                            
200100                                                                          
200200        IF WS-ART-KVDISP-PR-PAS(ART-IX) = ZERO                            
200300           CONTINUE                                                       
200400        ELSE                                                              
200500           COMPUTE WS-SUMMA-KR ROUNDED =                                  
200600                          WS-ART-KVDISP-PR-PAS(ART-IX) / 1000             
200700           MOVE WS-SUMMA-KR TO ART-KVDISP-PAS(ART-IX)                     
200800        END-IF                                                            
200900                                                                          
201000*********  LAGERVÄRDE/RUTA                                                
201100        IF WS-ART-LS-PR-AKT(ART-IX) = ZERO                                
201200           CONTINUE                                                       
201300        ELSE                                                              
201400           COMPUTE WS-SUMMA-KR ROUNDED =                                  
201500                          WS-ART-LS-PR-AKT(ART-IX) / 1000                 
201600           MOVE WS-SUMMA-KR TO ART-LS-AKT(ART-IX)                         
201700        END-IF                                                            
201800                                                                          
201900        IF WS-ART-LS-PR-PAS(ART-IX) = ZERO                                
202000           CONTINUE                                                       
202100        ELSE                                                              
202200           COMPUTE WS-SUMMA-KR ROUNDED =                                  
202300                          WS-ART-LS-PR-PAS(ART-IX) / 1000                 
202400           MOVE WS-SUMMA-KR TO ART-LS-PAS(ART-IX)                         
202500        END-IF                                                            
202600                                                                          
202700*********  AK-VÄRDE/RUTA                                                  
202800        IF WS-ART-AK-PR-AKT(ART-IX) = ZERO                                
202900           CONTINUE                                                       
203000        ELSE                                                              
203100           COMPUTE WS-SUMMA-KR ROUNDED =                                  
203200                          WS-ART-AK-PR-AKT(ART-IX) / 1000                 
203300           MOVE WS-SUMMA-KR TO ART-AK-AKT(ART-IX)                         
203400        END-IF                                                            
203500                                                                          
203600        IF WS-ART-AK-PR-PAS(ART-IX) = ZERO                                
203700           CONTINUE                                                       
203800        ELSE                                                              
203900           COMPUTE WS-SUMMA-KR ROUNDED =                                  
204000                          WS-ART-AK-PR-PAS(ART-IX) / 1000                 
204100           MOVE WS-SUMMA-KR TO ART-AK-PAS(ART-IX)                         
204200        END-IF                                                            
204300                                                                          
204400*********  SÄKERHETSLAGER/RUTA                                            
204500        IF WS-ART-SLAGER(ART-IX) = ZERO                                   
204600           CONTINUE                                                       
204700        ELSE                                                              
204800           COMPUTE WS-SUMMA-KR ROUNDED =                                  
204900                          WS-ART-SLAGER(ART-IX) / 1000                    
205000           MOVE WS-SUMMA-KR TO ART-SLAGER(ART-IX)                         
205100        END-IF                                                            
205200                                                                          
205300*********  ÖVERLAGER/RUTA                                                 
205400        IF WS-ART-OLAGER(ART-IX) = ZERO                                   
205500           CONTINUE                                                       
205600        ELSE                                                              
205700           COMPUTE WS-SUMMA-KR ROUNDED                                    
205800                              = WS-ART-OLAGER(ART-IX) / 1000              
205900           MOVE WS-SUMMA-KR TO ART-OLAGER(ART-IX)                         
206000        END-IF                                                            
206100                                                                          
206200*********  MEDELLAGER/RUTA                                                
206300        IF WS-ART-MLAGER(ART-IX) = ZERO                                   
206400           CONTINUE                                                       
206500        ELSE                                                              
206600           COMPUTE WS-SUMMA-KR =                                          
206700                                WS-ART-MLAGER(ART-IX) / 1000              
206800           ADD WS-SUMMA-KR TO ART-MLAGER(ART-IX)                          
206900        END-IF                                                            
207000                                                                          
207100*********  OMSHASTIGHET/RUTA                                              
207200        COMPUTE WS-SUMMA = WS-ART-KVDISP-PR-AKT(ART-IX) +                 
207300                           WS-ART-KVDISP-PR-PAS(ART-IX)                   
207400        IF WS-SUMMA = ZERO                                                
207500           CONTINUE                                                       
207600        ELSE                                                              
207700           COMPUTE WS-OMSHAST ROUNDED =                                   
207800               WS-ART-KVOI(ART-IX) /  WS-SUMMA                            
207900           MOVE WS-OMSHAST TO ART-OMSHAST-DISP(ART-IX)                    
208000        END-IF                                                            
208100                                                                          
208200        COMPUTE WS-SUMMA = WS-ART-LS-PR-AKT(ART-IX) +                     
208300                           WS-ART-AK-PR-AKT(ART-IX) +                     
208400                           WS-ART-LS-PR-PAS(ART-IX) +                     
208500                           WS-ART-AK-PR-PAS(ART-IX)                       
208600        IF WS-SUMMA = ZERO                                                
208700           CONTINUE                                                       
208800        ELSE                                                              
208900           COMPUTE WS-OMSHAST ROUNDED =                                   
209000               WS-ART-KVOI(ART-IX) /  WS-SUMMA                            
209100           MOVE WS-OMSHAST TO ART-OMSHAST-LS(ART-IX)                      
209200        END-IF                                                            
209300                                                                          
209400*********  TOTAL SERVICEGRAD/RUTA                                         
209500        COMPUTE WS-KVOI-TOT-VECKA = WS-ART-KVOI-AKT(ART-IX) +             
209600                                    WS-ART-KVOI-PAS(ART-IX) +             
209700                                    WS-ART-KVOI-TEO(ART-IX) +             
209800                                    WS-ART-KVOI-SAK(ART-IX)               
209900        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
210000                        =   WS-ART-KVOI-CDC-AKT(ART-IX) +                 
210100                            WS-ART-KVOI-CDC-PAS(ART-IX) +                 
210200                            WS-ART-KVOI-CDC-TEO(ART-IX) +                 
210300                            WS-ART-KVOI-CDC-SAK(ART-IX)                   
210400        IF WS-KVOI-TOT-VECKA = ZERO                                       
210500           MOVE 99.9   TO ART-SERVG-TOT(ART-IX)                           
210600        ELSE                                                              
210700           COMPUTE WS-SERVG ROUNDED =                                     
210800             (WS-KVOI-TOT-VECKA - WS-KVOI-TOT-CDC-VECKA) * 100  /         
210900                              WS-KVOI-TOT-VECKA                           
211000           IF WS-SERVG = 100.0                                            
211100              MOVE 99.9 TO ART-SERVG-TOT(ART-IX)                          
211200           ELSE                                                           
211300              MOVE WS-SERVG TO ART-SERVG-TOT(ART-IX)                      
211400           END-IF                                                         
211500        END-IF                                                            
211600                                                                          
211700*********  SERVICEGRAD/RUTA AKTIVA ARTIKLAR                               
211800        IF WS-ART-KVOI-AKT(ART-IX) = ZERO                                 
211900           MOVE 99.9   TO ART-SERVG-AKT(ART-IX)                           
212000        ELSE                                                              
212100           COMPUTE WS-SERVG ROUNDED = (WS-ART-KVOI-AKT(ART-IX) -          
212200                         WS-ART-KVOI-CDC-AKT(ART-IX)) * 100 /             
212300                         WS-ART-KVOI-AKT(ART-IX)                          
212400           IF WS-SERVG = 100.0                                            
212500              MOVE 99.9 TO ART-SERVG-AKT(ART-IX)                          
212600           ELSE                                                           
212700              MOVE WS-SERVG TO ART-SERVG-AKT(ART-IX)                      
212800           END-IF                                                         
212900        END-IF                                                            
213000                                                                          
213100*********  SERVICEGRAD/RUTA PASSIVA ARTIKLAR                              
213200        COMPUTE WS-KVOI-TOT-VECKA = WS-ART-KVOI-PAS(ART-IX) +             
213300                                    WS-ART-KVOI-TEO(ART-IX)               
213400        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
213500                        =   WS-ART-KVOI-CDC-PAS(ART-IX) +                 
213600                            WS-ART-KVOI-CDC-TEO(ART-IX)                   
213700        IF WS-KVOI-TOT-VECKA = ZERO                                       
213800           MOVE 99.9 TO ART-SERVG-PAS(ART-IX)                             
213900        ELSE                                                              
214000           COMPUTE WS-SERVG ROUNDED = (WS-KVOI-TOT-VECKA -                
214100                         WS-KVOI-TOT-CDC-VECKA) * 100                     
214200                        / WS-KVOI-TOT-VECKA                               
214300           IF WS-SERVG = 100.0                                            
214400              MOVE 99.9 TO ART-SERVG-PAS(ART-IX)                          
214500           ELSE                                                           
214600              MOVE WS-SERVG TO ART-SERVG-PAS(ART-IX)                      
214700           END-IF                                                         
214800        END-IF                                                            
214900                                                                          
215000*********  TEORETISK SERVICEGRAD/RUTA                                     
215100        COMPUTE WS-KVOI-TOT-VECKA = WS-ART-KVOI-AKT(ART-IX) +             
215200                                    WS-ART-KVOI-TEO(ART-IX)               
215300        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
215400                        =   WS-ART-KVOI-CDC-AKT(ART-IX) +                 
215500                            WS-ART-KVOI-CDC-TEO(ART-IX)                   
215600        IF WS-KVOI-TOT-VECKA = ZERO                                       
215700           MOVE 99.9   TO ART-SERVG-TEO(ART-IX)                           
215800        ELSE                                                              
215900           COMPUTE WS-SERVG ROUNDED = (WS-KVOI-TOT-VECKA -                
216000                         WS-KVOI-TOT-CDC-VECKA) * 100                     
216100                        / WS-KVOI-TOT-VECKA                               
216200           IF WS-SERVG = 100.0                                            
216300              MOVE 99.9 TO ART-SERVG-TEO(ART-IX)                          
216400           ELSE                                                           
216500              MOVE WS-SERVG TO ART-SERVG-TEO(ART-IX)                      
216600           END-IF                                                         
216700        END-IF                                                            
216800                                                                          
216900*********  SPLITFAKTOR/RUTA                                               
217000        COMPUTE WS-KVOI-TOT-VECKA = WS-ART-KVOI-AKT(ART-IX) +             
217100                                    WS-ART-KVOI-PAS(ART-IX) +             
217200                                    WS-ART-KVOI-TEO(ART-IX) +             
217300                                    WS-ART-KVOI-SAK(ART-IX)               
217400        COMPUTE WS-KVOI-TOT-CDC-VECKA =                                   
217500                            WS-ART-KVOI-CDC-SAK(ART-IX) +                 
217600                            WS-ART-KVOI-CDC-PAS(ART-IX)                   
217700        IF WS-KVOI-TOT-VECKA = ZERO                                       
217800           MOVE 99.9 TO ART-SPLIT(ART-IX)                                 
217900        ELSE                                                              
218000           COMPUTE WS-SERVG ROUNDED = (WS-KVOI-TOT-VECKA -                
218100                         WS-KVOI-TOT-CDC-VECKA) * 100                     
218200                        / WS-KVOI-TOT-VECKA                               
218300           IF WS-SERVG = 100.0                                            
218400              MOVE 99.9 TO ART-SPLIT(ART-IX)                              
218500           ELSE                                                           
218600              MOVE WS-SERVG TO ART-SPLIT(ART-IX)                          
218700           END-IF                                                         
218800        END-IF                                                            
218900*********  ORDERTRÄFFAR/RUTA                                              
219000        COMPUTE WS-KVOI-TOT-VECKA = WS-ART-KVOI-AKT(ART-IX) +             
219100                                    WS-ART-KVOI-PAS(ART-IX) +             
219200                                    WS-ART-KVOI-TEO(ART-IX) +             
219300                                    WS-ART-KVOI-SAK(ART-IX)               
219400        ADD WS-KVOI-TOT-VECKA TO ART-KVOT(ART-IX)                         
219500                                                                          
219600        ADD +1  TO ART-IX                                                 
219700     END-PERFORM                                                          
219800     .                                                                    
219900     EJECT                                                                
220000 BCB-SUMMERA-PRISKLASS SECTION.                                           
220100******************************************************************        
220200* SUMMERING PER PRISKLASS                                        *        
220300******************************************************************        
220400                                                                          
220500     MOVE +1 TO PSUM-IX                                                   
220600                ART-IX                                                    
220700     MOVE +8 TO ART-IX-MAX                                                
220800                                                                          
220900     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
221000        PERFORM UNTIL ART-IX > ART-IX-MAX                                 
221100                                                                          
221200          ADD ART-KVANT-AKT(ART-IX) TO PSUM-KVANT-AKT(PSUM-IX)            
221300          ADD ART-KVANT-PAS(ART-IX) TO PSUM-KVANT-PAS(PSUM-IX)            
221400          ADD ART-KVOT(ART-IX)      TO PSUM-KVOT(PSUM-IX)                 
221500          ADD WS-ART-KVOI-AKT(ART-IX) TO WS-PSUM-KVOI-AKT(PSUM-IX)        
221600          ADD WS-ART-KVOI-PAS(ART-IX) TO WS-PSUM-KVOI-PAS(PSUM-IX)        
221700          ADD WS-ART-KVOI-TEO(ART-IX) TO WS-PSUM-KVOI-TEO(PSUM-IX)        
221800          ADD WS-ART-KVOI-SAK(ART-IX) TO WS-PSUM-KVOI-SAK(PSUM-IX)        
221900          ADD WS-ART-KVOI-CDC-AKT(ART-IX)                                 
222000                                  TO WS-PSUM-KVOI-CDC-AKT(PSUM-IX)        
222100          ADD WS-ART-KVOI-CDC-PAS(ART-IX)                                 
222200                                  TO WS-PSUM-KVOI-CDC-PAS(PSUM-IX)        
222300          ADD WS-ART-KVOI-CDC-TEO(ART-IX)                                 
222400                                  TO WS-PSUM-KVOI-CDC-TEO(PSUM-IX)        
222500          ADD WS-ART-KVOI-CDC-SAK(ART-IX)                                 
222600                                  TO WS-PSUM-KVOI-CDC-SAK(PSUM-IX)        
222700          ADD WS-ART-KVOI(ART-IX) TO WS-PSUM-KVOI(PSUM-IX)                
222800          ADD WS-ART-KVDISP-PR-AKT(ART-IX)                                
222900                             TO WS-PSUM-KVDISP-PR-AKT (PSUM-IX)           
223000          ADD WS-ART-OK-PR-AKT(ART-IX)                                    
223100                             TO WS-PSUM-OK-PR-AKT(PSUM-IX)                
223200          ADD WS-ART-LS-PR-AKT(ART-IX)                                    
223300                             TO WS-PSUM-LS-PR-AKT(PSUM-IX)                
223400          ADD WS-ART-AK-PR-AKT(ART-IX)                                    
223500                             TO WS-PSUM-AK-PR-AKT(PSUM-IX)                
223600          ADD WS-ART-KVDISP-PR-PAS(ART-IX)                                
223700                             TO WS-PSUM-KVDISP-PR-PAS(PSUM-IX)            
223800          ADD WS-ART-OK-PR-PAS(ART-IX)                                    
223900                             TO WS-PSUM-OK-PR-PAS(PSUM-IX)                
224000          ADD WS-ART-LS-PR-PAS(ART-IX)                                    
224100                             TO WS-PSUM-LS-PR-PAS(PSUM-IX)                
224200          ADD WS-ART-AK-PR-PAS(ART-IX)                                    
224300                             TO WS-PSUM-AK-PR-PAS(PSUM-IX)                
224400          ADD WS-ART-OLAGER(ART-IX) TO WS-PSUM-OLAGER(PSUM-IX)            
224500          ADD WS-ART-MLAGER(ART-IX) TO WS-PSUM-MLAGER(PSUM-IX)            
224600          ADD WS-ART-SLAGER(ART-IX) TO WS-PSUM-SLAGER(PSUM-IX)            
224700                                                                          
224800          ADD +1 TO ART-IX                                                
224900        END-PERFORM                                                       
225000                                                                          
225100        ADD +1 TO PSUM-IX                                                 
225200        ADD +8 TO ART-IX-MAX                                              
225300     END-PERFORM                                                          
225400                                                                          
225500     MOVE +1 TO PSUM-IX                                                   
225600     MOVE +9 TO PSUM-IX-MAX                                               
225700     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
225800                                                                          
225900*********  DISP LAGER VÄRDE/PRISKLASS                                     
226000        IF WS-PSUM-KVDISP-PR-AKT(PSUM-IX) = ZERO                          
226100           CONTINUE                                                       
226200        ELSE                                                              
226300           COMPUTE WS-SUMMA-KR ROUNDED =                                  
226400                     WS-PSUM-KVDISP-PR-AKT(PSUM-IX) / 1000                
226500           MOVE WS-SUMMA-KR TO PSUM-KVDISP-AKT(PSUM-IX)                   
226600        END-IF                                                            
226700                                                                          
226800        IF WS-PSUM-KVDISP-PR-PAS(PSUM-IX) = ZERO                          
226900           CONTINUE                                                       
227000        ELSE                                                              
227100           COMPUTE WS-SUMMA-KR ROUNDED =                                  
227200                     WS-PSUM-KVDISP-PR-PAS(PSUM-IX) / 1000                
227300           MOVE WS-SUMMA-KR TO PSUM-KVDISP-PAS(PSUM-IX)                   
227400        END-IF                                                            
227500                                                                          
227600*********  LAGERVÄRDE/PRISKLASS                                           
227700        IF WS-PSUM-LS-PR-AKT(PSUM-IX) = ZERO                              
227800           CONTINUE                                                       
227900        ELSE                                                              
228000           COMPUTE WS-SUMMA-KR ROUNDED =                                  
228100                     WS-PSUM-LS-PR-AKT(PSUM-IX) / 1000                    
228200           MOVE WS-SUMMA-KR TO PSUM-LS-AKT(PSUM-IX)                       
228300        END-IF                                                            
228400                                                                          
228500        IF WS-PSUM-LS-PR-PAS(PSUM-IX) = ZERO                              
228600           CONTINUE                                                       
228700        ELSE                                                              
228800           COMPUTE WS-SUMMA-KR ROUNDED =                                  
228900                     WS-PSUM-LS-PR-PAS(PSUM-IX) / 1000                    
229000           MOVE WS-SUMMA-KR TO PSUM-LS-PAS(PSUM-IX)                       
229100        END-IF                                                            
229200                                                                          
229300*********  AK-VÄRDE/PRISKLASS                                             
229400        IF WS-PSUM-AK-PR-AKT(PSUM-IX) = ZERO                              
229500           CONTINUE                                                       
229600        ELSE                                                              
229700           COMPUTE WS-SUMMA-KR ROUNDED =                                  
229800                     WS-PSUM-AK-PR-AKT(PSUM-IX) / 1000                    
229900           MOVE WS-SUMMA-KR TO PSUM-AK-AKT(PSUM-IX)                       
230000        END-IF                                                            
230100                                                                          
230200        IF WS-PSUM-AK-PR-PAS(PSUM-IX) = ZERO                              
230300           CONTINUE                                                       
230400        ELSE                                                              
230500           COMPUTE WS-SUMMA-KR ROUNDED =                                  
230600                     WS-PSUM-AK-PR-PAS(PSUM-IX) / 1000                    
230700           MOVE WS-SUMMA-KR TO PSUM-AK-PAS(PSUM-IX)                       
230800        END-IF                                                            
230900                                                                          
231000*********  SÄKERHETSLAGER/PRISKLASS                                       
231100        IF WS-PSUM-SLAGER(PSUM-IX) = ZERO                                 
231200           CONTINUE                                                       
231300        ELSE                                                              
231400           COMPUTE WS-SUMMA-KR ROUNDED =                                  
231500                          WS-PSUM-SLAGER(PSUM-IX) / 1000                  
231600           MOVE WS-SUMMA-KR TO PSUM-SLAGER(PSUM-IX)                       
231700        END-IF                                                            
231800                                                                          
231900*********  ÖVERLAGER/PRISKLASS                                            
232000        IF WS-PSUM-OLAGER(PSUM-IX) = ZERO                                 
232100           CONTINUE                                                       
232200        ELSE                                                              
232300           COMPUTE WS-SUMMA-KR ROUNDED                                    
232400                     = WS-PSUM-OLAGER(PSUM-IX) / 1000                     
232500           MOVE WS-SUMMA-KR TO PSUM-OLAGER(PSUM-IX)                       
232600        END-IF                                                            
232700                                                                          
232800*********  MEDELLAGER/PRISKLASS                                           
232900        IF WS-PSUM-MLAGER(PSUM-IX) = ZERO                                 
233000           CONTINUE                                                       
233100        ELSE                                                              
233200           COMPUTE WS-SUMMA-KR =                                          
233300                       WS-PSUM-MLAGER(PSUM-IX) / 1000                     
233400           ADD WS-SUMMA-KR TO PSUM-MLAGER(PSUM-IX)                        
233500        END-IF                                                            
233600                                                                          
233700*********  TOTAL SERVICEGRAD/PRISKLASS                                    
233800        COMPUTE WS-KVOI-TOT-VECKA = WS-PSUM-KVOI-AKT(PSUM-IX) +           
233900                                    WS-PSUM-KVOI-PAS(PSUM-IX) +           
234000                                    WS-PSUM-KVOI-TEO(PSUM-IX) +           
234100                                    WS-PSUM-KVOI-SAK(PSUM-IX)             
234200        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
234300                       =   WS-PSUM-KVOI-CDC-AKT(PSUM-IX) +                
234400                           WS-PSUM-KVOI-CDC-PAS(PSUM-IX) +                
234500                           WS-PSUM-KVOI-CDC-TEO(PSUM-IX) +                
234600                           WS-PSUM-KVOI-CDC-SAK(PSUM-IX)                  
234700                                                                          
234800        IF WS-KVOI-TOT-VECKA = ZERO                                       
234900           MOVE 99.9 TO PSUM-SERVG-TOT(PSUM-IX)                           
235000        ELSE                                                              
235100           COMPUTE WS-SERVG ROUNDED =                                     
235200             (WS-KVOI-TOT-VECKA - WS-KVOI-TOT-CDC-VECKA) * 100  /         
235300                              WS-KVOI-TOT-VECKA                           
235400           IF WS-SERVG = 100.0                                            
235500              MOVE 99.9 TO PSUM-SERVG-TOT(PSUM-IX)                        
235600           ELSE                                                           
235700              MOVE WS-SERVG TO PSUM-SERVG-TOT(PSUM-IX)                    
235800           END-IF                                                         
235900        END-IF                                                            
236000                                                                          
236100*********  SERVICEGRAD/PRISKLASS  AKTIVA ARTIKLAR                         
236200        IF WS-PSUM-KVOI-AKT(PSUM-IX) = ZERO                               
236300           MOVE 99.9 TO PSUM-SERVG-AKT(PSUM-IX)                           
236400        ELSE                                                              
236500           COMPUTE WS-SERVG ROUNDED = (WS-PSUM-KVOI-AKT(PSUM-IX)          
236600                  - WS-PSUM-KVOI-CDC-AKT(PSUM-IX)) * 100                  
236700                        / WS-PSUM-KVOI-AKT(PSUM-IX)                       
236800           IF WS-SERVG = 100.0                                            
236900              MOVE 99.9 TO PSUM-SERVG-AKT(PSUM-IX)                        
237000           ELSE                                                           
237100              MOVE WS-SERVG TO PSUM-SERVG-AKT(PSUM-IX)                    
237200           END-IF                                                         
237300        END-IF                                                            
237400                                                                          
237500*********  SERVICEGRAD/RUTA PASSIVA ARTIKLAR                              
237600        COMPUTE WS-KVOI-TOT-VECKA = WS-PSUM-KVOI-PAS(PSUM-IX) +           
237700                                    WS-PSUM-KVOI-TEO(PSUM-IX)             
237800        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
237900                        =   WS-PSUM-KVOI-CDC-PAS(PSUM-IX) +               
238000                            WS-PSUM-KVOI-CDC-TEO(PSUM-IX)                 
238100        IF WS-KVOI-TOT-VECKA = ZERO                                       
238200           MOVE 99.9 TO PSUM-SERVG-PAS(PSUM-IX)                           
238300        ELSE                                                              
238400           COMPUTE WS-SERVG ROUNDED = (WS-KVOI-TOT-VECKA -                
238500                         WS-KVOI-TOT-CDC-VECKA) * 100                     
238600                        / WS-KVOI-TOT-VECKA                               
238700           IF WS-SERVG = 100.0                                            
238800              MOVE 99.9 TO PSUM-SERVG-PAS(PSUM-IX)                        
238900           ELSE                                                           
239000              MOVE WS-SERVG TO PSUM-SERVG-PAS(PSUM-IX)                    
239100           END-IF                                                         
239200        END-IF                                                            
239300                                                                          
239400*********  TEORETISK SERVICEGRAD/RUTA                                     
239500        COMPUTE WS-KVOI-TOT-VECKA = WS-PSUM-KVOI-AKT(PSUM-IX) +           
239600                                    WS-PSUM-KVOI-TEO(PSUM-IX)             
239700        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
239800                        =   WS-PSUM-KVOI-CDC-AKT(PSUM-IX) +               
239900                            WS-PSUM-KVOI-CDC-TEO(PSUM-IX)                 
240000        IF WS-KVOI-TOT-VECKA = ZERO                                       
240100           MOVE 99.9 TO PSUM-SERVG-TEO(PSUM-IX)                           
240200        ELSE                                                              
240300           COMPUTE WS-SERVG ROUNDED = (WS-KVOI-TOT-VECKA -                
240400                         WS-KVOI-TOT-CDC-VECKA) * 100                     
240500                        / WS-KVOI-TOT-VECKA                               
240600           IF WS-SERVG = 100.0                                            
240700              MOVE 99.9 TO PSUM-SERVG-TEO(PSUM-IX)                        
240800           ELSE                                                           
240900              MOVE WS-SERVG TO PSUM-SERVG-TEO(PSUM-IX)                    
241000           END-IF                                                         
241100        END-IF                                                            
241200                                                                          
241300*********  SPLITFAKTOR/PRISKLASS                                          
241400        COMPUTE WS-KVOI-TOT-VECKA = WS-PSUM-KVOI-AKT(PSUM-IX) +           
241500                                    WS-PSUM-KVOI-PAS(PSUM-IX) +           
241600                                    WS-PSUM-KVOI-TEO(PSUM-IX) +           
241700                                    WS-PSUM-KVOI-SAK(PSUM-IX)             
241800        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
241900                        =   WS-PSUM-KVOI-CDC-SAK(PSUM-IX) +               
242000                            WS-PSUM-KVOI-CDC-PAS(PSUM-IX)                 
242100        IF WS-KVOI-TOT-VECKA = ZERO                                       
242200           MOVE 99.9 TO PSUM-SPLIT(PSUM-IX)                               
242300        ELSE                                                              
242400           COMPUTE WS-SERVG ROUNDED =                                     
242500             (WS-KVOI-TOT-VECKA - WS-KVOI-TOT-CDC-VECKA) * 100  /         
242600                              WS-KVOI-TOT-VECKA                           
242700           IF WS-SERVG = 100.0                                            
242800              MOVE 99.9 TO PSUM-SPLIT(PSUM-IX)                            
242900           ELSE                                                           
243000              MOVE WS-SERVG TO PSUM-SPLIT(PSUM-IX)                        
243100           END-IF                                                         
243200        END-IF                                                            
243300                                                                          
243400*********  OMSHASTIGHET/PRISKLASS                                         
243500        COMPUTE WS-SUMMA = WS-PSUM-KVDISP-PR-AKT(PSUM-IX) +               
243600                           WS-PSUM-KVDISP-PR-PAS(PSUM-IX)                 
243700        IF WS-SUMMA = ZERO                                                
243800           CONTINUE                                                       
243900        ELSE                                                              
244000           COMPUTE WS-OMSHAST ROUNDED =                                   
244100               WS-PSUM-KVOI(PSUM-IX) / WS-SUMMA                           
244200           MOVE WS-OMSHAST TO PSUM-OMSHAST-DISP(PSUM-IX)                  
244300        END-IF                                                            
244400                                                                          
244500        COMPUTE WS-SUMMA = WS-PSUM-LS-PR-AKT(PSUM-IX) +                   
244600                           WS-PSUM-AK-PR-AKT(PSUM-IX) +                   
244700                           WS-PSUM-LS-PR-PAS(PSUM-IX) +                   
244800                           WS-PSUM-AK-PR-PAS(PSUM-IX)                     
244900        IF WS-SUMMA = ZERO                                                
245000           CONTINUE                                                       
245100        ELSE                                                              
245200           COMPUTE WS-OMSHAST ROUNDED =                                   
245300               WS-PSUM-KVOI(PSUM-IX) / WS-SUMMA                           
245400           MOVE WS-OMSHAST TO PSUM-OMSHAST-LS(PSUM-IX)                    
245500        END-IF                                                            
245600                                                                          
245700        ADD +1 TO PSUM-IX                                                 
245800     END-PERFORM                                                          
245900     .                                                                    
246000     EJECT                                                                
246100 BCC-SUMMERA-FREKVENSKLASS SECTION.                                       
246200******************************************************************        
246300* SUMMERING PER FREKVENSKLASS                                    *        
246400******************************************************************        
246500                                                                          
246600     MOVE +1 TO FSUM-IX                                                   
246700                ART-IX                                                    
246800     MOVE +65 TO ART-IX-MAX                                               
246900     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
247000                                                                          
247100        PERFORM UNTIL ART-IX > ART-IX-MAX                                 
247200          ADD ART-KVANT-AKT(ART-IX) TO FSUM-KVANT-AKT(FSUM-IX)            
247300          ADD ART-KVANT-PAS(ART-IX) TO FSUM-KVANT-PAS(FSUM-IX)            
247400          ADD ART-KVOT(ART-IX)      TO FSUM-KVOT(FSUM-IX)                 
247500          ADD WS-ART-KVOI-AKT(ART-IX) TO WS-FSUM-KVOI-AKT(FSUM-IX)        
247600          ADD WS-ART-KVOI-PAS(ART-IX) TO WS-FSUM-KVOI-PAS(FSUM-IX)        
247700          ADD WS-ART-KVOI-TEO(ART-IX) TO WS-FSUM-KVOI-TEO(FSUM-IX)        
247800          ADD WS-ART-KVOI-SAK(ART-IX) TO WS-FSUM-KVOI-SAK(FSUM-IX)        
247900          ADD WS-ART-KVOI-CDC-AKT(ART-IX)                                 
248000                                  TO WS-FSUM-KVOI-CDC-AKT(FSUM-IX)        
248100          ADD WS-ART-KVOI-CDC-PAS(ART-IX)                                 
248200                                  TO WS-FSUM-KVOI-CDC-PAS(FSUM-IX)        
248300          ADD WS-ART-KVOI-CDC-TEO(ART-IX)                                 
248400                                  TO WS-FSUM-KVOI-CDC-TEO(FSUM-IX)        
248500          ADD WS-ART-KVOI-CDC-SAK(ART-IX)                                 
248600                                  TO WS-FSUM-KVOI-CDC-SAK(FSUM-IX)        
248700          ADD WS-ART-KVOI(ART-IX) TO WS-FSUM-KVOI(FSUM-IX)                
248800          ADD WS-ART-KVDISP-PR-AKT(ART-IX)                                
248900                             TO WS-FSUM-KVDISP-PR-AKT (FSUM-IX)           
249000          ADD WS-ART-OK-PR-AKT(ART-IX)                                    
249100                             TO WS-FSUM-OK-PR-AKT(FSUM-IX)                
249200          ADD WS-ART-LS-PR-AKT(ART-IX)                                    
249300                             TO WS-FSUM-LS-PR-AKT(FSUM-IX)                
249400          ADD WS-ART-AK-PR-AKT(ART-IX)                                    
249500                             TO WS-FSUM-AK-PR-AKT(FSUM-IX)                
249600          ADD WS-ART-KVDISP-PR-PAS(ART-IX)                                
249700                             TO WS-FSUM-KVDISP-PR-PAS(FSUM-IX)            
249800          ADD WS-ART-OK-PR-PAS(ART-IX)                                    
249900                             TO WS-FSUM-OK-PR-PAS(FSUM-IX)                
250000          ADD WS-ART-LS-PR-PAS(ART-IX)                                    
250100                             TO WS-FSUM-LS-PR-PAS(FSUM-IX)                
250200          ADD WS-ART-AK-PR-PAS(ART-IX)                                    
250300                             TO WS-FSUM-AK-PR-PAS(FSUM-IX)                
250400          ADD WS-ART-OLAGER(ART-IX) TO WS-FSUM-OLAGER(FSUM-IX)            
250500          ADD WS-ART-MLAGER(ART-IX) TO WS-FSUM-MLAGER(FSUM-IX)            
250600          ADD WS-ART-SLAGER(ART-IX) TO WS-FSUM-SLAGER(FSUM-IX)            
250700                                                                          
250800          ADD +8 TO ART-IX                                                
250900        END-PERFORM                                                       
251000        ADD +1 TO FSUM-IX                                                 
251100                                                                          
251200        EVALUATE TRUE                                                     
251300           WHEN  FSUM-IX = 1                                              
251400                 MOVE +1 TO ART-IX                                        
251500           WHEN  FSUM-IX = 2                                              
251600                 MOVE +2 TO ART-IX                                        
251700                 MOVE +66 TO ART-IX-MAX                                   
251800           WHEN  FSUM-IX = 3                                              
251900                 MOVE +3 TO ART-IX                                        
252000                 MOVE +67 TO ART-IX-MAX                                   
252100           WHEN  FSUM-IX = 4                                              
252200                 MOVE +4 TO ART-IX                                        
252300                 MOVE +68 TO ART-IX-MAX                                   
252400           WHEN  FSUM-IX = 5                                              
252500                 MOVE +5 TO ART-IX                                        
252600                 MOVE +69 TO ART-IX-MAX                                   
252700           WHEN  FSUM-IX = 6                                              
252800                 MOVE +6 TO ART-IX                                        
252900                 MOVE +70 TO ART-IX-MAX                                   
253000           WHEN  FSUM-IX = 7                                              
253100                 MOVE +7 TO ART-IX                                        
253200                 MOVE +71 TO ART-IX-MAX                                   
253300           WHEN  FSUM-IX = 8                                              
253400                 MOVE +8 TO ART-IX                                        
253500                 MOVE +72 TO ART-IX-MAX                                   
253600           WHEN OTHER                                                     
253700                CONTINUE                                                  
253800        END-EVALUATE                                                      
253900                                                                          
254000     END-PERFORM                                                          
254100                                                                          
254200     MOVE +1 TO FSUM-IX                                                   
254300     MOVE +8 TO FSUM-IX-MAX                                               
254400     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
254500                                                                          
254600*********  DISP LAGER VÄRDE/FREKVENSKLASS                                 
254700        IF WS-FSUM-KVDISP-PR-AKT(FSUM-IX) = ZERO                          
254800           CONTINUE                                                       
254900        ELSE                                                              
255000           COMPUTE WS-SUMMA-KR ROUNDED =                                  
255100                     WS-FSUM-KVDISP-PR-AKT(FSUM-IX) / 1000                
255200           MOVE WS-SUMMA-KR TO FSUM-KVDISP-AKT(FSUM-IX)                   
255300        END-IF                                                            
255400                                                                          
255500        IF WS-FSUM-KVDISP-PR-PAS(FSUM-IX) = ZERO                          
255600           CONTINUE                                                       
255700        ELSE                                                              
255800           COMPUTE WS-SUMMA-KR ROUNDED =                                  
255900                     WS-FSUM-KVDISP-PR-PAS(FSUM-IX) / 1000                
256000           MOVE WS-SUMMA-KR TO FSUM-KVDISP-PAS(FSUM-IX)                   
256100        END-IF                                                            
256200                                                                          
256300*********  LAGERVÄRDE/FREKVENSKLASS                                       
256400        IF WS-FSUM-LS-PR-AKT(FSUM-IX) = ZERO                              
256500           CONTINUE                                                       
256600        ELSE                                                              
256700           COMPUTE WS-SUMMA-KR ROUNDED =                                  
256800                     WS-FSUM-LS-PR-AKT(FSUM-IX) / 1000                    
256900           MOVE WS-SUMMA-KR TO FSUM-LS-AKT(FSUM-IX)                       
257000        END-IF                                                            
257100                                                                          
257200        IF WS-FSUM-LS-PR-PAS(FSUM-IX) = ZERO                              
257300           CONTINUE                                                       
257400        ELSE                                                              
257500           COMPUTE WS-SUMMA-KR ROUNDED =                                  
257600                     WS-FSUM-LS-PR-PAS(FSUM-IX) / 1000                    
257700           MOVE WS-SUMMA-KR TO FSUM-LS-PAS(FSUM-IX)                       
257800        END-IF                                                            
257900                                                                          
258000*********  AK-VÄRDE/FREKVENSKLASS                                         
258100        IF WS-FSUM-AK-PR-AKT(FSUM-IX) = ZERO                              
258200           CONTINUE                                                       
258300        ELSE                                                              
258400           COMPUTE WS-SUMMA-KR ROUNDED =                                  
258500                     WS-FSUM-AK-PR-AKT(FSUM-IX) / 1000                    
258600           MOVE WS-SUMMA-KR TO FSUM-AK-AKT(FSUM-IX)                       
258700        END-IF                                                            
258800                                                                          
258900        IF WS-FSUM-AK-PR-PAS(FSUM-IX) = ZERO                              
259000           CONTINUE                                                       
259100        ELSE                                                              
259200           COMPUTE WS-SUMMA-KR ROUNDED =                                  
259300                     WS-FSUM-AK-PR-PAS(FSUM-IX) / 1000                    
259400           MOVE WS-SUMMA-KR TO FSUM-AK-PAS(FSUM-IX)                       
259500        END-IF                                                            
259600                                                                          
259700*********  SÄKERHETSLAGER/FREKVENSKLASS                                   
259800        IF WS-FSUM-SLAGER(FSUM-IX) = ZERO                                 
259900           CONTINUE                                                       
260000        ELSE                                                              
260100           COMPUTE WS-SUMMA-KR ROUNDED =                                  
260200                          WS-FSUM-SLAGER(FSUM-IX) / 1000                  
260300           MOVE WS-SUMMA-KR TO FSUM-SLAGER(FSUM-IX)                       
260400        END-IF                                                            
260500                                                                          
260600*********  ÖVERLAGER/FREKVENSKLASS                                        
260700        IF WS-FSUM-OLAGER(FSUM-IX) = ZERO                                 
260800           CONTINUE                                                       
260900        ELSE                                                              
261000           COMPUTE WS-SUMMA-KR ROUNDED                                    
261100                     = WS-FSUM-OLAGER(FSUM-IX) / 1000                     
261200           MOVE WS-SUMMA-KR TO FSUM-OLAGER(FSUM-IX)                       
261300        END-IF                                                            
261400                                                                          
261500*********  MEDELLAGER/FREKVENSKLASS                                       
261600        IF WS-FSUM-MLAGER(FSUM-IX) = ZERO                                 
261700           CONTINUE                                                       
261800        ELSE                                                              
261900           COMPUTE WS-SUMMA-KR =                                          
262000                       WS-FSUM-MLAGER(FSUM-IX) / 1000                     
262100           ADD WS-SUMMA-KR TO FSUM-MLAGER(FSUM-IX)                        
262200        END-IF                                                            
262300                                                                          
262400*********  TOTAL SERVICEGRAD/FREKVENSKLASS                                
262500        COMPUTE WS-KVOI-TOT-VECKA = WS-FSUM-KVOI-AKT(FSUM-IX) +           
262600                                    WS-FSUM-KVOI-PAS(FSUM-IX) +           
262700                                    WS-FSUM-KVOI-TEO(FSUM-IX) +           
262800                                    WS-FSUM-KVOI-SAK(FSUM-IX)             
262900        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
263000                       =   WS-FSUM-KVOI-CDC-AKT(FSUM-IX) +                
263100                           WS-FSUM-KVOI-CDC-PAS(FSUM-IX) +                
263200                           WS-FSUM-KVOI-CDC-TEO(FSUM-IX) +                
263300                           WS-FSUM-KVOI-CDC-SAK(FSUM-IX)                  
263400                                                                          
263500        IF WS-KVOI-TOT-VECKA = ZERO                                       
263600           MOVE 99.9 TO FSUM-SERVG-TOT(FSUM-IX)                           
263700        ELSE                                                              
263800           COMPUTE WS-SERVG ROUNDED =                                     
263900             (WS-KVOI-TOT-VECKA - WS-KVOI-TOT-CDC-VECKA) * 100  /         
264000                              WS-KVOI-TOT-VECKA                           
264100           IF WS-SERVG = 100.0                                            
264200              MOVE 99.9 TO FSUM-SERVG-TOT(FSUM-IX)                        
264300           ELSE                                                           
264400              MOVE WS-SERVG TO FSUM-SERVG-TOT(FSUM-IX)                    
264500           END-IF                                                         
264600        END-IF                                                            
264700                                                                          
264800*********  SERVICEGRAD/FREKVENSKLASS AKTIVA ARTIKLAR                      
264900        IF WS-FSUM-KVOI-AKT(FSUM-IX) = ZERO                               
265000           MOVE 99.9 TO FSUM-SERVG-AKT(FSUM-IX)                           
265100        ELSE                                                              
265200           COMPUTE WS-SERVG ROUNDED = (WS-FSUM-KVOI-AKT(FSUM-IX)          
265300                - WS-FSUM-KVOI-CDC-AKT(FSUM-IX)) * 100                    
265400                        / WS-FSUM-KVOI-AKT(FSUM-IX)                       
265500           IF WS-SERVG = 100.0                                            
265600              MOVE 99.9 TO FSUM-SERVG-AKT(FSUM-IX)                        
265700           ELSE                                                           
265800              MOVE WS-SERVG TO FSUM-SERVG-AKT(FSUM-IX)                    
265900           END-IF                                                         
266000        END-IF                                                            
266100                                                                          
266200*********  SERVICEGRAD/FREKVENSKLASS PASSIVA ARTIKLAR                     
266300        COMPUTE WS-KVOI-TOT-VECKA = WS-FSUM-KVOI-PAS(FSUM-IX) +           
266400                                    WS-FSUM-KVOI-TEO(FSUM-IX)             
266500        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
266600                        =   WS-FSUM-KVOI-CDC-PAS(FSUM-IX) +               
266700                            WS-FSUM-KVOI-CDC-TEO(FSUM-IX)                 
266800        IF WS-KVOI-TOT-VECKA = ZERO                                       
266900           MOVE 99.9 TO FSUM-SERVG-PAS(FSUM-IX)                           
267000        ELSE                                                              
267100           COMPUTE WS-SERVG ROUNDED = (WS-KVOI-TOT-VECKA -                
267200                         WS-KVOI-TOT-CDC-VECKA) * 100                     
267300                        / WS-KVOI-TOT-VECKA                               
267400           IF WS-SERVG = 100.0                                            
267500              MOVE 99.9 TO FSUM-SERVG-PAS(FSUM-IX)                        
267600           ELSE                                                           
267700              MOVE WS-SERVG TO FSUM-SERVG-PAS(FSUM-IX)                    
267800           END-IF                                                         
267900        END-IF                                                            
268000                                                                          
268100*********  TEORETISK SERVICEGRAD/FREKVENSKLASS                            
268200        COMPUTE WS-KVOI-TOT-VECKA = WS-FSUM-KVOI-AKT(FSUM-IX) +           
268300                                    WS-FSUM-KVOI-TEO(FSUM-IX)             
268400        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
268500                        =   WS-FSUM-KVOI-CDC-AKT(FSUM-IX) +               
268600                            WS-FSUM-KVOI-CDC-TEO(FSUM-IX)                 
268700        IF WS-KVOI-TOT-VECKA = ZERO                                       
268800           MOVE 99.9 TO FSUM-SERVG-TEO(FSUM-IX)                           
268900        ELSE                                                              
269000           COMPUTE WS-SERVG ROUNDED = (WS-KVOI-TOT-VECKA -                
269100                         WS-KVOI-TOT-CDC-VECKA) * 100                     
269200                        / WS-KVOI-TOT-VECKA                               
269300           IF WS-SERVG = 100.0                                            
269400              MOVE 99.9 TO FSUM-SERVG-TEO(FSUM-IX)                        
269500           ELSE                                                           
269600              MOVE WS-SERVG TO FSUM-SERVG-TEO(FSUM-IX)                    
269700           END-IF                                                         
269800        END-IF                                                            
269900                                                                          
270000*********  SPLITFAKTOR/FREKVENSKLASS                                      
270100        COMPUTE WS-KVOI-TOT-VECKA = WS-FSUM-KVOI-AKT(FSUM-IX) +           
270200                                    WS-FSUM-KVOI-PAS(FSUM-IX) +           
270300                                    WS-FSUM-KVOI-TEO(FSUM-IX) +           
270400                                    WS-FSUM-KVOI-SAK(FSUM-IX)             
270500        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
270600                        =   WS-FSUM-KVOI-CDC-SAK(FSUM-IX) +               
270700                            WS-FSUM-KVOI-CDC-PAS(FSUM-IX)                 
270800        IF WS-KVOI-TOT-VECKA = ZERO                                       
270900           MOVE 99.9 TO FSUM-SPLIT(FSUM-IX)                               
271000        ELSE                                                              
271100           COMPUTE WS-SERVG ROUNDED =                                     
271200             (WS-KVOI-TOT-VECKA - WS-KVOI-TOT-CDC-VECKA) * 100  /         
271300                              WS-KVOI-TOT-VECKA                           
271400           IF WS-SERVG = 100.0                                            
271500              MOVE 99.9 TO FSUM-SPLIT(FSUM-IX)                            
271600           ELSE                                                           
271700              MOVE WS-SERVG TO FSUM-SPLIT(FSUM-IX)                        
271800           END-IF                                                         
271900        END-IF                                                            
272000                                                                          
272100*********  OMSHASTIGHET/FREKVENSKLASS                                     
272200        COMPUTE WS-SUMMA = WS-FSUM-KVDISP-PR-AKT(FSUM-IX) +               
272300                           WS-FSUM-KVDISP-PR-PAS(FSUM-IX)                 
272400        IF WS-SUMMA = ZERO                                                
272500           CONTINUE                                                       
272600        ELSE                                                              
272700           COMPUTE WS-OMSHAST ROUNDED =                                   
272800               WS-FSUM-KVOI(FSUM-IX) / WS-SUMMA                           
272900           MOVE WS-OMSHAST TO FSUM-OMSHAST-DISP(FSUM-IX)                  
273000        END-IF                                                            
273100                                                                          
273200        COMPUTE WS-SUMMA = WS-FSUM-LS-PR-AKT(FSUM-IX) +                   
273300                           WS-FSUM-AK-PR-AKT(FSUM-IX) +                   
273400                           WS-FSUM-LS-PR-PAS(FSUM-IX) +                   
273500                           WS-FSUM-AK-PR-PAS(FSUM-IX)                     
273600        IF WS-SUMMA = ZERO                                                
273700           CONTINUE                                                       
273800        ELSE                                                              
273900           COMPUTE WS-OMSHAST ROUNDED =                                   
274000               WS-FSUM-KVOI(FSUM-IX) / WS-SUMMA                           
274100           MOVE WS-OMSHAST TO FSUM-OMSHAST-LS(FSUM-IX)                    
274200        END-IF                                                            
274300                                                                          
274400        ADD +1 TO FSUM-IX                                                 
274500                                                                          
274600     END-PERFORM                                                          
274700     .                                                                    
274800     EJECT                                                                
274900 BCD-SUMMERA-TOTAL SECTION.                                               
275000******************************************************************        
275100* TOTALSUMMERING SAMTLIGA PRISKLASSER                            *        
275200******************************************************************        
275300                                                                          
275400     MOVE +1 TO PSUM-IX                                                   
275500     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
275600        ADD PSUM-KVANT-AKT(PSUM-IX) TO TOT-KVANT-AKT                      
275700        ADD PSUM-KVANT-PAS(PSUM-IX) TO TOT-KVANT-PAS                      
275800        ADD PSUM-KVOT(PSUM-IX) TO TOT-KVOT                                
275900        ADD WS-PSUM-KVOI-AKT(PSUM-IX) TO WS-TOT-KVOI-AKT                  
276000        ADD WS-PSUM-KVOI-PAS(PSUM-IX) TO WS-TOT-KVOI-PAS                  
276100        ADD WS-PSUM-KVOI-TEO(PSUM-IX) TO WS-TOT-KVOI-TEO                  
276200        ADD WS-PSUM-KVOI-SAK(PSUM-IX) TO WS-TOT-KVOI-SAK                  
276300        ADD WS-PSUM-KVOI-CDC-AKT(PSUM-IX)                                 
276400                                TO WS-TOT-KVOI-CDC-AKT                    
276500        ADD WS-PSUM-KVOI-CDC-PAS(PSUM-IX)                                 
276600                                TO WS-TOT-KVOI-CDC-PAS                    
276700        ADD WS-PSUM-KVOI-CDC-TEO(PSUM-IX)                                 
276800                                TO WS-TOT-KVOI-CDC-TEO                    
276900        ADD WS-PSUM-KVOI-CDC-SAK(PSUM-IX)                                 
277000                                TO WS-TOT-KVOI-CDC-SAK                    
277100        ADD WS-PSUM-KVOI(PSUM-IX) TO WS-TOT-KVOI                          
277200        ADD WS-PSUM-KVDISP-PR-AKT(PSUM-IX)                                
277300                           TO WS-TOT-KVDISP-PR-AKT                        
277400        ADD WS-PSUM-OK-PR-AKT(PSUM-IX)                                    
277500                           TO WS-TOT-OK-PR-AKT                            
277600        ADD WS-PSUM-LS-PR-AKT(PSUM-IX)                                    
277700                           TO WS-TOT-LS-PR-AKT                            
277800        ADD WS-PSUM-AK-PR-AKT(PSUM-IX)                                    
277900                           TO WS-TOT-AK-PR-AKT                            
278000        ADD WS-PSUM-KVDISP-PR-PAS(PSUM-IX)                                
278100                           TO WS-TOT-KVDISP-PR-PAS                        
278200        ADD WS-PSUM-OK-PR-PAS(PSUM-IX)                                    
278300                           TO WS-TOT-OK-PR-PAS                            
278400        ADD WS-PSUM-LS-PR-PAS(PSUM-IX)                                    
278500                           TO WS-TOT-LS-PR-PAS                            
278600        ADD WS-PSUM-AK-PR-PAS(PSUM-IX)                                    
278700                           TO WS-TOT-AK-PR-PAS                            
278800        ADD WS-PSUM-OLAGER(PSUM-IX) TO WS-TOT-OLAGER                      
278900        ADD WS-PSUM-MLAGER(PSUM-IX) TO WS-TOT-MLAGER                      
279000        ADD WS-PSUM-SLAGER(PSUM-IX) TO WS-TOT-SLAGER                      
279100                                                                          
279200        ADD +1 TO PSUM-IX                                                 
279300     END-PERFORM                                                          
279400                                                                          
279500*********  DISP LAGER VÄRDE TOTALT                                        
279600        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
279700           CONTINUE                                                       
279800        ELSE                                                              
279900           COMPUTE WS-SUMMA-KR ROUNDED =                                  
280000                     WS-TOT-KVDISP-PR-AKT / 1000                          
280100           MOVE WS-SUMMA-KR TO TOT-KVDISP-AKT                             
280200        END-IF                                                            
280300                                                                          
280400        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
280500           CONTINUE                                                       
280600        ELSE                                                              
280700           COMPUTE WS-SUMMA-KR ROUNDED =                                  
280800                     WS-TOT-KVDISP-PR-PAS / 1000                          
280900           MOVE WS-SUMMA-KR TO TOT-KVDISP-PAS                             
281000        END-IF                                                            
281100                                                                          
281200*********  LAGERVÄRDE TOTALT                                              
281300        IF WS-TOT-LS-PR-AKT = ZERO                                        
281400           CONTINUE                                                       
281500        ELSE                                                              
281600           COMPUTE WS-SUMMA-KR ROUNDED =                                  
281700                     WS-TOT-LS-PR-AKT / 1000                              
281800           MOVE WS-SUMMA-KR TO TOT-LS-AKT                                 
281900        END-IF                                                            
282000                                                                          
282100        IF WS-TOT-LS-PR-PAS = ZERO                                        
282200           CONTINUE                                                       
282300        ELSE                                                              
282400           COMPUTE WS-SUMMA-KR ROUNDED =                                  
282500                     WS-TOT-LS-PR-PAS / 1000                              
282600           MOVE WS-SUMMA-KR TO TOT-LS-PAS                                 
282700        END-IF                                                            
282800                                                                          
282900*********  AK-VÄRDE TOTALT                                                
283000        IF WS-TOT-AK-PR-AKT = ZERO                                        
283100           CONTINUE                                                       
283200        ELSE                                                              
283300           COMPUTE WS-SUMMA-KR ROUNDED =                                  
283400                     WS-TOT-AK-PR-AKT / 1000                              
283500           MOVE WS-SUMMA-KR TO TOT-AK-AKT                                 
283600        END-IF                                                            
283700                                                                          
283800        IF WS-TOT-AK-PR-PAS = ZERO                                        
283900           CONTINUE                                                       
284000        ELSE                                                              
284100           COMPUTE WS-SUMMA-KR ROUNDED =                                  
284200                     WS-TOT-AK-PR-PAS / 1000                              
284300           MOVE WS-SUMMA-KR TO TOT-AK-PAS                                 
284400        END-IF                                                            
284500                                                                          
284600*********  SÄKERHETSLAGER TOTALT                                          
284700        IF WS-TOT-SLAGER = ZERO                                           
284800           CONTINUE                                                       
284900        ELSE                                                              
285000           COMPUTE WS-SUMMA-KR ROUNDED =                                  
285100                          WS-TOT-SLAGER / 1000                            
285200           MOVE WS-SUMMA-KR TO TOT-SLAGER                                 
285300        END-IF                                                            
285400                                                                          
285500*********  ÖVERLAGER TOTALT                                               
285600        IF WS-TOT-OLAGER = ZERO                                           
285700           CONTINUE                                                       
285800        ELSE                                                              
285900           COMPUTE WS-SUMMA-KR ROUNDED                                    
286000                     = WS-TOT-OLAGER / 1000                               
286100           MOVE WS-SUMMA-KR TO TOT-OLAGER                                 
286200        END-IF                                                            
286300                                                                          
286400*********  MEDELLAGER TOTALT                                              
286500        IF WS-TOT-MLAGER = ZERO                                           
286600           CONTINUE                                                       
286700        ELSE                                                              
286800           COMPUTE WS-SUMMA-KR =                                          
286900                       WS-TOT-MLAGER / 1000                               
287000           ADD WS-SUMMA-KR TO TOT-MLAGER                                  
287100        END-IF                                                            
287200                                                                          
287300*********  TOTAL SERVICEGRAD TOTALT                                       
287400        COMPUTE WS-KVOI-TOT-VECKA = WS-TOT-KVOI-AKT +                     
287500                                    WS-TOT-KVOI-PAS +                     
287600                                    WS-TOT-KVOI-TEO +                     
287700                                    WS-TOT-KVOI-SAK                       
287800        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
287900                       =   WS-TOT-KVOI-CDC-AKT +                          
288000                           WS-TOT-KVOI-CDC-PAS +                          
288100                           WS-TOT-KVOI-CDC-TEO +                          
288200                           WS-TOT-KVOI-CDC-SAK                            
288300                                                                          
288400        IF WS-KVOI-TOT-VECKA = ZERO                                       
288500           MOVE 99.9 TO TOT-SERVG-TOT                                     
288600        ELSE                                                              
288700           COMPUTE WS-SERVG ROUNDED =                                     
288800             (WS-KVOI-TOT-VECKA - WS-KVOI-TOT-CDC-VECKA) * 100  /         
288900                              WS-KVOI-TOT-VECKA                           
289000           IF WS-SERVG = 100.0                                            
289100              MOVE 99.9 TO TOT-SERVG-TOT                                  
289200           ELSE                                                           
289300              MOVE WS-SERVG TO TOT-SERVG-TOT                              
289400           END-IF                                                         
289500        END-IF                                                            
289600                                                                          
289700*********  SERVICEGRAD TOTALT AKTIVA ARTIKLAR                             
289800                                                                          
289900        IF WS-TOT-KVOI-AKT = ZERO                                         
290000           MOVE 99.9 TO TOT-SERVG-AKT                                     
290100        ELSE                                                              
290200           COMPUTE WS-SERVG ROUNDED = (WS-TOT-KVOI-AKT                    
290300                      - WS-TOT-KVOI-CDC-AKT) * 100                        
290400                        / WS-TOT-KVOI-AKT                                 
290500           IF WS-SERVG = 100.0                                            
290600              MOVE 99.9 TO TOT-SERVG-AKT                                  
290700           ELSE                                                           
290800              MOVE WS-SERVG TO TOT-SERVG-AKT                              
290900           END-IF                                                         
291000        END-IF                                                            
291100                                                                          
291200*********  SERVICEGRAD TOTALT PASSIVA ARTIKLAR                            
291300        COMPUTE WS-KVOI-TOT-VECKA = WS-TOT-KVOI-PAS +                     
291400                                    WS-TOT-KVOI-TEO                       
291500        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
291600                        =   WS-TOT-KVOI-CDC-PAS +                         
291700                            WS-TOT-KVOI-CDC-TEO                           
291800        IF WS-KVOI-TOT-VECKA = ZERO                                       
291900           MOVE 99.9 TO TOT-SERVG-PAS                                     
292000        ELSE                                                              
292100           COMPUTE WS-SERVG ROUNDED = (WS-KVOI-TOT-VECKA -                
292200                         WS-KVOI-TOT-CDC-VECKA) * 100                     
292300                        / WS-KVOI-TOT-VECKA                               
292400           IF WS-SERVG = 100.0                                            
292500              MOVE 99.9 TO TOT-SERVG-PAS                                  
292600           ELSE                                                           
292700              MOVE WS-SERVG TO TOT-SERVG-PAS                              
292800           END-IF                                                         
292900        END-IF                                                            
293000                                                                          
293100*********  TEORETISK SERVICEGRAD TOTALT                                   
293200        COMPUTE WS-KVOI-TOT-VECKA = WS-TOT-KVOI-AKT +                     
293300                                    WS-TOT-KVOI-TEO                       
293400        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
293500                        =   WS-TOT-KVOI-CDC-AKT +                         
293600                            WS-TOT-KVOI-CDC-TEO                           
293700        IF WS-KVOI-TOT-VECKA = ZERO                                       
293800           MOVE 99.9 TO TOT-SERVG-TEO                                     
293900        ELSE                                                              
294000           COMPUTE WS-SERVG ROUNDED = (WS-KVOI-TOT-VECKA -                
294100                         WS-KVOI-TOT-CDC-VECKA) * 100                     
294200                        / WS-KVOI-TOT-VECKA                               
294300           IF WS-SERVG = 100.0                                            
294400              MOVE 99.9 TO TOT-SERVG-TEO                                  
294500           ELSE                                                           
294600              MOVE WS-SERVG TO TOT-SERVG-TEO                              
294700           END-IF                                                         
294800        END-IF                                                            
294900                                                                          
295000*********  SPLITFAKTOR TOTALT                                             
295100        COMPUTE WS-KVOI-TOT-VECKA = WS-TOT-KVOI-AKT +                     
295200                                    WS-TOT-KVOI-PAS +                     
295300                                    WS-TOT-KVOI-TEO +                     
295400                                    WS-TOT-KVOI-SAK +                     
295500                                    WS-KVOT-SAKNAS-WDK7                   
295600        COMPUTE WS-KVOI-TOT-CDC-VECKA                                     
295700                        =   WS-TOT-KVOI-CDC-SAK +                         
295800                            WS-TOT-KVOI-CDC-PAS +                         
295900                            WS-KVOT-CDC-SAKNAS-WDK7                       
296000        IF WS-KVOI-TOT-VECKA = ZERO                                       
296100           MOVE 99.9 TO TOT-SPLIT                                         
296200        ELSE                                                              
296300           COMPUTE WS-SERVG ROUNDED =                                     
296400             (WS-KVOI-TOT-VECKA - WS-KVOI-TOT-CDC-VECKA) * 100  /         
296500                              WS-KVOI-TOT-VECKA                           
296600           IF WS-SERVG = 100.0                                            
296700              MOVE 99.9 TO TOT-SPLIT                                      
296800           ELSE                                                           
296900              MOVE WS-SERVG TO TOT-SPLIT                                  
297000           END-IF                                                         
297100        END-IF                                                            
297200                                                                          
297300*********  OMSHASTIGHET TOTALT                                            
297400        COMPUTE WS-SUMMA = WS-TOT-KVDISP-PR-AKT +                         
297500                           WS-TOT-KVDISP-PR-PAS                           
297600        IF WS-SUMMA = ZERO                                                
297700           CONTINUE                                                       
297800        ELSE                                                              
297900           COMPUTE WS-OMSHAST ROUNDED =                                   
298000               WS-TOT-KVOI / WS-SUMMA                                     
298100           MOVE WS-OMSHAST TO TOT-OMSHAST-DISP                            
298200        END-IF                                                            
298300                                                                          
298400        COMPUTE WS-SUMMA = WS-TOT-LS-PR-AKT +                             
298500                           WS-TOT-AK-PR-AKT +                             
298600                           WS-TOT-LS-PR-PAS +                             
298700                           WS-TOT-AK-PR-PAS                               
298800        IF WS-SUMMA = ZERO                                                
298900           CONTINUE                                                       
299000        ELSE                                                              
299100           COMPUTE WS-OMSHAST ROUNDED =                                   
299200               WS-TOT-KVOI / WS-SUMMA                                     
299300           MOVE WS-OMSHAST TO TOT-OMSHAST-LS                              
299400                                                                          
299500        END-IF                                                            
299600*********  ORDERTRÄFFAR TOTALT                                            
299700                                                                          
299800        ADD WS-KVOT-SAKNAS-WDK7 TO TOT-KVOT                               
299900     .                                                                    
300000     EJECT                                                                
300100 BCE-BERAKNINGAR-AV-TOTAL SECTION.                                        
300200******************************************************************        
300300* % BERÄKNING PER RUTA / PRISKLASS / FREKVENSKLASS               *        
300400******************************************************************        
300500                                                                          
300600     MOVE +1 TO ART-IX                                                    
300700     MOVE +72 TO ART-IX-MAX                                               
300800     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
300900                                                                          
301000******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  AKTIVA                          
301100        IF TOT-KVANT-AKT = ZERO                                           
301200           CONTINUE                                                       
301300        ELSE                                                              
301400           COMPUTE WS-PROC = ART-KVANT-AKT(ART-IX)                        
301500                                    * 100 / TOT-KVANT-AKT                 
301600           MOVE WS-PROC TO ART-PROC-KVANT-A(ART-IX)                       
301700        END-IF                                                            
301800                                                                          
301900******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  PASSIVA                         
302000        IF TOT-KVANT-PAS = ZERO                                           
302100           CONTINUE                                                       
302200        ELSE                                                              
302300           COMPUTE WS-PROC = ART-KVANT-PAS(ART-IX)                        
302400                                    * 100 / TOT-KVANT-PAS                 
302500           MOVE WS-PROC TO ART-PROC-KVANT-P(ART-IX)                       
302600        END-IF                                                            
302700                                                                          
302800******** % ANTAL ORDERTRÄFFAR AV TOTAL                                    
302900        IF TOT-KVOT = ZERO                                                
303000           CONTINUE                                                       
303100        ELSE                                                              
303200           COMPUTE WS-PROC = ART-KVOT(ART-IX)                             
303300                                    * 100 / TOT-KVOT                      
303400           MOVE WS-PROC TO ART-PROC-KVOT(ART-IX)                          
303500        END-IF                                                            
303600                                                                          
303700*******  %  RUTANS DISP.LAGER/TOT DISP-LAGER  AKTIVA                      
303800                                                                          
303900        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
304000           CONTINUE                                                       
304100        ELSE                                                              
304200           IF WS-ART-KVDISP-PR-AKT(ART-IX) > ZERO                         
304300              COMPUTE WS-PROC = WS-ART-KVDISP-PR-AKT(ART-IX)              
304400                              * 100 / WS-TOT-KVDISP-PR-AKT                
304500              MOVE WS-PROC TO ART-PROC-KVDISP-A(ART-IX)                   
304600           END-IF                                                         
304700        END-IF                                                            
304800                                                                          
304900*******  %  RUTANS DISP.LAGER/TOT DISP-LAGER  PASSIVA                     
305000                                                                          
305100        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
305200           CONTINUE                                                       
305300        ELSE                                                              
305400           IF WS-ART-KVDISP-PR-PAS(ART-IX) > ZERO                         
305500              COMPUTE WS-PROC = WS-ART-KVDISP-PR-PAS(ART-IX)              
305600                              * 100 / WS-TOT-KVDISP-PR-PAS                
305700              MOVE WS-PROC TO ART-PROC-KVDISP-P(ART-IX)                   
305800           END-IF                                                         
305900        END-IF                                                            
306000                                                                          
306100*******  %  RUTANS LAGERVÄRDE/TOT LAGERVÄRDE  AKTIVA                      
306200                                                                          
306300        IF WS-TOT-LS-PR-AKT = ZERO                                        
306400           CONTINUE                                                       
306500        ELSE                                                              
306600           IF WS-ART-LS-PR-AKT(ART-IX) > ZERO                             
306700              COMPUTE WS-PROC = WS-ART-LS-PR-AKT(ART-IX)                  
306800                              * 100 / WS-TOT-LS-PR-AKT                    
306900              MOVE WS-PROC TO ART-PROC-LS-A(ART-IX)                       
307000           END-IF                                                         
307100        END-IF                                                            
307200                                                                          
307300*******  %  RUTANS LAGERVÄRDE/TOT LAGERVÄRDE  PASSIVA                     
307400                                                                          
307500        IF WS-TOT-LS-PR-PAS = ZERO                                        
307600           CONTINUE                                                       
307700        ELSE                                                              
307800           IF WS-ART-LS-PR-PAS(ART-IX) > ZERO                             
307900              COMPUTE WS-PROC = WS-ART-LS-PR-PAS(ART-IX)                  
308000                              * 100 / WS-TOT-LS-PR-PAS                    
308100              MOVE WS-PROC TO ART-PROC-LS-P(ART-IX)                       
308200           END-IF                                                         
308300        END-IF                                                            
308400                                                                          
308500*******  %  RUTANS AK-VÄRDE/TOT AK-VÄRDE  AKTIVA                          
308600                                                                          
308700        IF WS-TOT-AK-PR-AKT = ZERO                                        
308800           CONTINUE                                                       
308900        ELSE                                                              
309000           IF WS-ART-AK-PR-AKT(ART-IX) > ZERO                             
309100              COMPUTE WS-PROC = WS-ART-AK-PR-AKT(ART-IX)                  
309200                              * 100 / WS-TOT-AK-PR-AKT                    
309300              MOVE WS-PROC TO ART-PROC-AK-A(ART-IX)                       
309400           END-IF                                                         
309500        END-IF                                                            
309600                                                                          
309700*******  %  RUTANS AK-VÄRDE/TOT AK-VÄRDE  PASSIVA                         
309800                                                                          
309900        IF WS-TOT-AK-PR-PAS = ZERO                                        
310000           CONTINUE                                                       
310100        ELSE                                                              
310200           IF WS-ART-AK-PR-PAS(ART-IX) > ZERO                             
310300              COMPUTE WS-PROC = WS-ART-AK-PR-PAS(ART-IX)                  
310400                              * 100 / WS-TOT-AK-PR-PAS                    
310500              MOVE WS-PROC TO ART-PROC-AK-P(ART-IX)                       
310600           END-IF                                                         
310700        END-IF                                                            
310800                                                                          
310900        ADD +1 TO ART-IX                                                  
311000     END-PERFORM                                                          
311100                                                                          
311200****************************                                              
311300                                                                          
311400     MOVE +1 TO PSUM-IX                                                   
311500     MOVE +9 TO PSUM-IX-MAX                                               
311600     PERFORM UNTIL PSUM-IX > PSUM-IX-MAX                                  
311700                                                                          
311800******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  AKTIVA                          
311900        IF TOT-KVANT-AKT = ZERO                                           
312000           CONTINUE                                                       
312100        ELSE                                                              
312200           COMPUTE WS-PROC = PSUM-KVANT-AKT(PSUM-IX) * 100 /              
312300                           TOT-KVANT-AKT                                  
312400           MOVE WS-PROC TO PSUM-PROC-KVANT-A(PSUM-IX)                     
312500        END-IF                                                            
312600                                                                          
312700******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  PASSIVA                         
312800        IF TOT-KVANT-PAS = ZERO                                           
312900           CONTINUE                                                       
313000        ELSE                                                              
313100           COMPUTE WS-PROC = PSUM-KVANT-PAS(PSUM-IX) * 100 /              
313200                           TOT-KVANT-PAS                                  
313300           MOVE WS-PROC TO PSUM-PROC-KVANT-P(PSUM-IX)                     
313400        END-IF                                                            
313500                                                                          
313600******** % ANTAL ORDERTRÄFFAR AV TOTALA                                   
313700        IF TOT-KVOT = ZERO                                                
313800           CONTINUE                                                       
313900        ELSE                                                              
314000           COMPUTE WS-PROC = PSUM-KVOT(PSUM-IX) * 100 /                   
314100                           TOT-KVOT                                       
314200           MOVE WS-PROC TO PSUM-PROC-KVOT(PSUM-IX)                        
314300        END-IF                                                            
314400                                                                          
314500*******  %  PRISKLASSENS DISP.LAGER/TOT DISP-LAGER  AKTIVA                
314600                                                                          
314700        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
314800           CONTINUE                                                       
314900        ELSE                                                              
315000           IF WS-PSUM-KVDISP-PR-AKT(PSUM-IX) > ZERO                       
315100              COMPUTE WS-PROC = WS-PSUM-KVDISP-PR-AKT(PSUM-IX)            
315200                              * 100 / WS-TOT-KVDISP-PR-AKT                
315300              MOVE WS-PROC TO PSUM-PROC-KVDISP-A(PSUM-IX)                 
315400           END-IF                                                         
315500        END-IF                                                            
315600                                                                          
315700*******  %  PRISKLASSENS DISP.LAGER/TOT DISP-LAGER  PASSIVA               
315800                                                                          
315900        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
316000           CONTINUE                                                       
316100        ELSE                                                              
316200           IF WS-PSUM-KVDISP-PR-PAS(PSUM-IX) > ZERO                       
316300              COMPUTE WS-PROC = WS-PSUM-KVDISP-PR-PAS(PSUM-IX)            
316400                              * 100 / WS-TOT-KVDISP-PR-PAS                
316500              MOVE WS-PROC TO PSUM-PROC-KVDISP-P(PSUM-IX)                 
316600           END-IF                                                         
316700        END-IF                                                            
316800                                                                          
316900*******  %  PRISKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  AKTIVA                
317000                                                                          
317100        IF WS-TOT-LS-PR-AKT = ZERO                                        
317200           CONTINUE                                                       
317300        ELSE                                                              
317400           IF WS-PSUM-LS-PR-AKT(PSUM-IX) > ZERO                           
317500              COMPUTE WS-PROC = WS-PSUM-LS-PR-AKT(PSUM-IX)                
317600                              * 100 / WS-TOT-LS-PR-AKT                    
317700              MOVE WS-PROC TO PSUM-PROC-LS-A(PSUM-IX)                     
317800           END-IF                                                         
317900        END-IF                                                            
318000                                                                          
318100*******  %  PRISKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  PASSIVA               
318200                                                                          
318300        IF WS-TOT-LS-PR-PAS = ZERO                                        
318400           CONTINUE                                                       
318500        ELSE                                                              
318600           IF WS-PSUM-LS-PR-PAS(PSUM-IX) > ZERO                           
318700              COMPUTE WS-PROC = WS-PSUM-LS-PR-PAS(PSUM-IX)                
318800                              * 100 / WS-TOT-LS-PR-PAS                    
318900              MOVE WS-PROC TO PSUM-PROC-LS-P(PSUM-IX)                     
319000           END-IF                                                         
319100        END-IF                                                            
319200                                                                          
319300*******  %  PRISKLASSENS AK-VÄRDE/TOT AK-VÄRDE  AKTIVA                    
319400                                                                          
319500        IF WS-TOT-AK-PR-AKT = ZERO                                        
319600           CONTINUE                                                       
319700        ELSE                                                              
319800           IF WS-PSUM-AK-PR-AKT(PSUM-IX) > ZERO                           
319900              COMPUTE WS-PROC = WS-PSUM-AK-PR-AKT(PSUM-IX)                
320000                              * 100 / WS-TOT-AK-PR-AKT                    
320100              MOVE WS-PROC TO PSUM-PROC-AK-A(PSUM-IX)                     
320200           END-IF                                                         
320300        END-IF                                                            
320400                                                                          
320500*******  %  PRISKLASSENS AK-VÄRDE/TOT AK-VÄRDE  PASSIVA                   
320600                                                                          
320700        IF WS-TOT-AK-PR-PAS = ZERO                                        
320800           CONTINUE                                                       
320900        ELSE                                                              
321000           IF WS-PSUM-AK-PR-PAS(PSUM-IX) > ZERO                           
321100              COMPUTE WS-PROC = WS-PSUM-AK-PR-PAS(PSUM-IX)                
321200                              * 100 / WS-TOT-AK-PR-PAS                    
321300              MOVE WS-PROC TO PSUM-PROC-AK-P(PSUM-IX)                     
321400           END-IF                                                         
321500        END-IF                                                            
321600                                                                          
321700        ADD +1 TO PSUM-IX                                                 
321800     END-PERFORM                                                          
321900                                                                          
322000****************************************                                  
322100                                                                          
322200     MOVE +1 TO FSUM-IX                                                   
322300     MOVE +8 TO FSUM-IX-MAX                                               
322400     PERFORM UNTIL FSUM-IX > FSUM-IX-MAX                                  
322500                                                                          
322600******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  AKTIVA                          
322700        IF TOT-KVANT-AKT = ZERO                                           
322800           CONTINUE                                                       
322900        ELSE                                                              
323000           COMPUTE WS-PROC = FSUM-KVANT-AKT(FSUM-IX) * 100 /              
323100                           TOT-KVANT-AKT                                  
323200           MOVE WS-PROC TO FSUM-PROC-KVANT-A(FSUM-IX)                     
323300        END-IF                                                            
323400                                                                          
323500******** % ANTAL ARTIKLAR AV TOTAL-ANTAL  PASSIVA                         
323600        IF TOT-KVANT-PAS = ZERO                                           
323700           CONTINUE                                                       
323800        ELSE                                                              
323900           COMPUTE WS-PROC = FSUM-KVANT-PAS(FSUM-IX) * 100 /              
324000                           TOT-KVANT-PAS                                  
324100           MOVE WS-PROC TO FSUM-PROC-KVANT-P(FSUM-IX)                     
324200        END-IF                                                            
324300                                                                          
324400******** % ANTAL ORDERTRÄFFAR TOTALA                                      
324500        IF TOT-KVOT = ZERO                                                
324600           CONTINUE                                                       
324700        ELSE                                                              
324800           COMPUTE WS-PROC = FSUM-KVOT(FSUM-IX) * 100 /                   
324900                           TOT-KVOT                                       
325000           MOVE WS-PROC TO FSUM-PROC-KVOT(FSUM-IX)                        
325100        END-IF                                                            
325200                                                                          
325300*******  %  FREKVENSKLASSENS DISP.LAGER/TOT DISP-LAGER  AKTIVA            
325400                                                                          
325500        IF WS-TOT-KVDISP-PR-AKT = ZERO                                    
325600           CONTINUE                                                       
325700        ELSE                                                              
325800           IF WS-FSUM-KVDISP-PR-AKT(FSUM-IX) > ZERO                       
325900              COMPUTE WS-PROC = WS-FSUM-KVDISP-PR-AKT(FSUM-IX)            
326000                              * 100 / WS-TOT-KVDISP-PR-AKT                
326100              MOVE WS-PROC TO FSUM-PROC-KVDISP-A(FSUM-IX)                 
326200           END-IF                                                         
326300        END-IF                                                            
326400                                                                          
326500*******  %  FREKVENSKLASSENS DISP.LAGER/TOT DISP-LAGER  PASSIVA           
326600                                                                          
326700        IF WS-TOT-KVDISP-PR-PAS = ZERO                                    
326800           CONTINUE                                                       
326900        ELSE                                                              
327000           IF WS-FSUM-KVDISP-PR-PAS(FSUM-IX) > ZERO                       
327100              COMPUTE WS-PROC = WS-FSUM-KVDISP-PR-PAS(FSUM-IX)            
327200                              * 100 / WS-TOT-KVDISP-PR-PAS                
327300              MOVE WS-PROC TO FSUM-PROC-KVDISP-P(FSUM-IX)                 
327400           END-IF                                                         
327500        END-IF                                                            
327600                                                                          
327700*******  %  FREKVENSKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  AKTIVA            
327800                                                                          
327900        IF WS-TOT-LS-PR-AKT = ZERO                                        
328000           CONTINUE                                                       
328100        ELSE                                                              
328200           IF WS-FSUM-LS-PR-AKT(FSUM-IX) > ZERO                           
328300              COMPUTE WS-PROC = WS-FSUM-LS-PR-AKT(FSUM-IX)                
328400                              * 100 / WS-TOT-LS-PR-AKT                    
328500              MOVE WS-PROC TO FSUM-PROC-LS-A(FSUM-IX)                     
328600           END-IF                                                         
328700        END-IF                                                            
328800                                                                          
328900*******  %  FREKVENSKLASSENS LAGERVÄRDE/TOT LAGERVÄRDE  PASSIVA           
329000                                                                          
329100        IF WS-TOT-LS-PR-PAS = ZERO                                        
329200           CONTINUE                                                       
329300        ELSE                                                              
329400           IF WS-FSUM-LS-PR-PAS(FSUM-IX) > ZERO                           
329500              COMPUTE WS-PROC = WS-FSUM-LS-PR-PAS(FSUM-IX)                
329600                              * 100 / WS-TOT-LS-PR-PAS                    
329700              MOVE WS-PROC TO FSUM-PROC-LS-P(FSUM-IX)                     
329800           END-IF                                                         
329900        END-IF                                                            
330000                                                                          
330100*******  %  FREKVENSSKLASSENS AK-VÄRDE/TOT AK-VÄRDE  AKTIVA               
330200                                                                          
330300        IF WS-TOT-AK-PR-AKT = ZERO                                        
330400           CONTINUE                                                       
330500        ELSE                                                              
330600           IF WS-FSUM-AK-PR-AKT(FSUM-IX) > ZERO                           
330700              COMPUTE WS-PROC = WS-FSUM-AK-PR-AKT(FSUM-IX)                
330800                              * 100 / WS-TOT-AK-PR-AKT                    
330900              MOVE WS-PROC TO FSUM-PROC-AK-A(FSUM-IX)                     
331000           END-IF                                                         
331100        END-IF                                                            
331200                                                                          
331300*******  %  FREKVENSKLASSENS AK-VÄRDE/TOT AK-VÄRDE  PASSIVA               
331400                                                                          
331500        IF WS-TOT-AK-PR-PAS = ZERO                                        
331600           CONTINUE                                                       
331700        ELSE                                                              
331800           IF WS-FSUM-AK-PR-PAS(FSUM-IX) > ZERO                           
331900              COMPUTE WS-PROC = WS-FSUM-AK-PR-PAS(FSUM-IX)                
332000                              * 100 / WS-TOT-AK-PR-PAS                    
332100              MOVE WS-PROC TO FSUM-PROC-AK-P(FSUM-IX)                     
332200           END-IF                                                         
332300        END-IF                                                            
332400                                                                          
332500        ADD +1 TO FSUM-IX                                                 
332600     END-PERFORM                                                          
332700     .                                                                    
332800     EJECT                                                                
332900 C-SKRIV-LISTA SECTION.                                                   
333000******************************************************************        
333100*  SID 1 BESTÅR AV 3 RUTRADER INKL PRISKLASS-TOTAL               *        
333200*      2           3 RUTRADER INKL PRISKLASS-TOTAL               *        
333300*      3           3 RUTRADER INKL PRISKLASS-TOTAL               *        
333400*      4           1 RUTRAD   FREKVENS-TOTAL OCH TOTAL-TOTAL     *        
333500******************************************************************        
333600                                                                          
333700     MOVE +1 TO IX1                                                       
333800     MOVE +2 TO IX2                                                       
333900     MOVE +3 TO IX3                                                       
334000     MOVE +4 TO IX4                                                       
334100     MOVE +5 TO IX5                                                       
334200     MOVE +6 TO IX6                                                       
334300     MOVE +7 TO IX7                                                       
334400     MOVE +8 TO IX8                                                       
334500     MOVE +1 TO PSUM-IX                                                   
334600                                                                          
334700*********** SKRIVER SID-1                                                 
334800                                                                          
334900     PERFORM S21A-SKRIV-RUBRIKER                                          
335000     MOVE '1' TO W001-DET1-PRISKLASS                                      
335100     PERFORM CA-FLYTTA-SKRIV-RAD                                          
335200     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
335300     ADD +1 TO PSUM-IX                                                    
335400     MOVE '2' TO W001-DET1-PRISKLASS                                      
335500     PERFORM CA-FLYTTA-SKRIV-RAD                                          
335600                                                                          
335700     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
335800     ADD +1 TO PSUM-IX                                                    
335900     MOVE '3' TO W001-DET1-PRISKLASS                                      
336000     PERFORM CA-FLYTTA-SKRIV-RAD                                          
336100                                                                          
336200*********** SKRIVER SID-2                                                 
336300     PERFORM S21A-SKRIV-RUBRIKER                                          
336400     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
336500     ADD +1 TO PSUM-IX                                                    
336600     MOVE '4' TO W001-DET1-PRISKLASS                                      
336700     PERFORM CA-FLYTTA-SKRIV-RAD                                          
336800                                                                          
336900     ADD +1 TO PSUM-IX                                                    
337000     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
337100     MOVE '5' TO W001-DET1-PRISKLASS                                      
337200     PERFORM CA-FLYTTA-SKRIV-RAD                                          
337300                                                                          
337400     ADD +1 TO PSUM-IX                                                    
337500     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
337600     MOVE '6' TO W001-DET1-PRISKLASS                                      
337700     PERFORM CA-FLYTTA-SKRIV-RAD                                          
337800                                                                          
337900*********** SKRIVER SID-3                                                 
338000     PERFORM S21A-SKRIV-RUBRIKER                                          
338100     ADD +1 TO PSUM-IX                                                    
338200     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
338300     MOVE '7' TO W001-DET1-PRISKLASS                                      
338400     PERFORM CA-FLYTTA-SKRIV-RAD                                          
338500                                                                          
338600     ADD +1 TO PSUM-IX                                                    
338700     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
338800     MOVE '8' TO W001-DET1-PRISKLASS                                      
338900     PERFORM CA-FLYTTA-SKRIV-RAD                                          
339000                                                                          
339100     ADD +1 TO PSUM-IX                                                    
339200     ADD +8 TO IX1 IX2 IX3 IX4 IX5 IX6 IX7 IX8                            
339300     MOVE '9' TO W001-DET1-PRISKLASS                                      
339400     PERFORM CA-FLYTTA-SKRIV-RAD                                          
339500                                                                          
339600*********** SKRIVER SID-4                                                 
339700     PERFORM S21A-SKRIV-RUBRIKER                                          
339800     MOVE +1 TO IX1                                                       
339900     MOVE +2 TO IX2                                                       
340000     MOVE +3 TO IX3                                                       
340100     MOVE +4 TO IX4                                                       
340200     MOVE +5 TO IX5                                                       
340300     MOVE +6 TO IX6                                                       
340400     MOVE +7 TO IX7                                                       
340500     MOVE +8 TO IX8                                                       
340600     MOVE SPACE TO W001-DET1-PRISKLASS                                    
340700     PERFORM CB-FLYTTA-SKRIV-TOT                                          
340800     .                                                                    
340900     EJECT                                                                
341000 CA-FLYTTA-SKRIV-RAD SECTION.                                             
341100                                                                          
341200     MOVE ART-KVANT-AKT(IX1)       TO  W001-DET1-KVANTA                   
341300     MOVE ART-PROC-KVANT-A(IX1)    TO  W001-DET1-P-KVANTA                 
341400     MOVE ART-KVANT-AKT(IX2)       TO  W001-DET1-KVANTB                   
341500     MOVE ART-PROC-KVANT-A(IX2)    TO  W001-DET1-P-KVANTB                 
341600     MOVE ART-KVANT-AKT(IX3)       TO  W001-DET1-KVANTC                   
341700     MOVE ART-PROC-KVANT-A(IX3)    TO  W001-DET1-P-KVANTC                 
341800     MOVE ART-KVANT-AKT(IX4)       TO  W001-DET1-KVANTD                   
341900     MOVE ART-PROC-KVANT-A(IX4)    TO  W001-DET1-P-KVANTD                 
342000     MOVE ART-KVANT-AKT(IX5)       TO  W001-DET1-KVANTE                   
342100     MOVE ART-PROC-KVANT-A(IX5)    TO  W001-DET1-P-KVANTE                 
342200     MOVE ART-KVANT-AKT(IX6)       TO  W001-DET1-KVANTF                   
342300     MOVE ART-PROC-KVANT-A(IX6)    TO  W001-DET1-P-KVANTF                 
342400     MOVE ART-KVANT-AKT(IX7)       TO  W001-DET1-KVANTG                   
342500     MOVE ART-PROC-KVANT-A(IX7)    TO  W001-DET1-P-KVANTG                 
342600     MOVE ART-KVANT-AKT(IX8)       TO  W001-DET1-KVANTH                   
342700     MOVE ART-PROC-KVANT-A(IX8)    TO  W001-DET1-P-KVANTH                 
342800     MOVE PSUM-KVANT-AKT(PSUM-IX)  TO  W001-DET1-TOT                      
342900     MOVE PSUM-PROC-KVANT-A(PSUM-IX) TO W001-DET1-P-TOT                   
343000     MOVE W001-DETALJRAD-1 TO W001-RAD                                    
343100     MOVE +2 TO W001-SKIP                                                 
343200     PERFORM S21-SKRIV-LISTA                                              
343300                                                                          
343400     MOVE ART-KVANT-PAS(IX1)       TO  W001-DET2-KVANTA                   
343500     MOVE ART-PROC-KVANT-P(IX1)    TO  W001-DET2-P-KVANTA                 
343600     MOVE ART-KVANT-PAS(IX2)       TO  W001-DET2-KVANTB                   
343700     MOVE ART-PROC-KVANT-P(IX2)    TO  W001-DET2-P-KVANTB                 
343800     MOVE ART-KVANT-PAS(IX3)       TO  W001-DET2-KVANTC                   
343900     MOVE ART-PROC-KVANT-P(IX3)    TO  W001-DET2-P-KVANTC                 
344000     MOVE ART-KVANT-PAS(IX4)       TO  W001-DET2-KVANTD                   
344100     MOVE ART-PROC-KVANT-P(IX4)    TO  W001-DET2-P-KVANTD                 
344200     MOVE ART-KVANT-PAS(IX5)       TO  W001-DET2-KVANTE                   
344300     MOVE ART-PROC-KVANT-P(IX5)    TO  W001-DET2-P-KVANTE                 
344400     MOVE ART-KVANT-PAS(IX6)       TO  W001-DET2-KVANTF                   
344500     MOVE ART-PROC-KVANT-P(IX6)    TO  W001-DET2-P-KVANTF                 
344600     MOVE ART-KVANT-PAS(IX7)       TO  W001-DET2-KVANTG                   
344700     MOVE ART-PROC-KVANT-P(IX7)    TO  W001-DET2-P-KVANTG                 
344800     MOVE ART-KVANT-PAS(IX8)       TO  W001-DET2-KVANTH                   
344900     MOVE ART-PROC-KVANT-P(IX8)    TO  W001-DET2-P-KVANTH                 
345000     MOVE PSUM-KVANT-PAS(PSUM-IX)  TO  W001-DET2-TOT                      
345100     MOVE PSUM-PROC-KVANT-P(PSUM-IX) TO W001-DET2-P-TOT                   
345200     MOVE W001-DETALJRAD-2 TO W001-RAD                                    
345300     MOVE +1 TO W001-SKIP                                                 
345400     PERFORM S21-SKRIV-LISTA                                              
345500                                                                          
345600     MOVE ART-KVDISP-AKT(IX1)      TO  W001-DET3-DLAGERA                  
345700     MOVE ART-PROC-KVDISP-A(IX1)   TO  W001-DET3-P-DLAGERA                
345800     MOVE ART-KVDISP-AKT(IX2)      TO  W001-DET3-DLAGERB                  
345900     MOVE ART-PROC-KVDISP-A(IX2)   TO  W001-DET3-P-DLAGERB                
346000     MOVE ART-KVDISP-AKT(IX3)      TO  W001-DET3-DLAGERC                  
346100     MOVE ART-PROC-KVDISP-A(IX3)   TO  W001-DET3-P-DLAGERC                
346200     MOVE ART-KVDISP-AKT(IX4)      TO  W001-DET3-DLAGERD                  
346300     MOVE ART-PROC-KVDISP-A(IX4)   TO  W001-DET3-P-DLAGERD                
346400     MOVE ART-KVDISP-AKT(IX5)      TO  W001-DET3-DLAGERE                  
346500     MOVE ART-PROC-KVDISP-A(IX5)   TO  W001-DET3-P-DLAGERE                
346600     MOVE ART-KVDISP-AKT(IX6)      TO  W001-DET3-DLAGERF                  
346700     MOVE ART-PROC-KVDISP-A(IX6)   TO  W001-DET3-P-DLAGERF                
346800     MOVE ART-KVDISP-AKT(IX7)      TO  W001-DET3-DLAGERG                  
346900     MOVE ART-PROC-KVDISP-A(IX7)   TO  W001-DET3-P-DLAGERG                
347000     MOVE ART-KVDISP-AKT(IX8)      TO  W001-DET3-DLAGERH                  
347100     MOVE ART-PROC-KVDISP-A(IX8)   TO  W001-DET3-P-DLAGERH                
347200     MOVE PSUM-KVDISP-AKT(PSUM-IX) TO  W001-DET3-TOT                      
347300     MOVE PSUM-PROC-KVDISP-A(PSUM-IX)                                     
347400                                   TO  W001-DET3-P-TOT                    
347500     MOVE W001-DETALJRAD-3 TO W001-RAD                                    
347600     MOVE +1 TO W001-SKIP                                                 
347700     PERFORM S21-SKRIV-LISTA                                              
347800                                                                          
347900     MOVE ART-KVDISP-PAS(IX1)      TO  W001-DET4-DLAGERA                  
348000     MOVE ART-PROC-KVDISP-P(IX1)   TO  W001-DET4-P-DLAGERA                
348100     MOVE ART-KVDISP-PAS(IX2)      TO  W001-DET4-DLAGERB                  
348200     MOVE ART-PROC-KVDISP-P(IX2)   TO  W001-DET4-P-DLAGERB                
348300     MOVE ART-KVDISP-PAS(IX3)      TO  W001-DET4-DLAGERC                  
348400     MOVE ART-PROC-KVDISP-P(IX3)   TO  W001-DET4-P-DLAGERC                
348500     MOVE ART-KVDISP-PAS(IX4)      TO  W001-DET4-DLAGERD                  
348600     MOVE ART-PROC-KVDISP-P(IX4)   TO  W001-DET4-P-DLAGERD                
348700     MOVE ART-KVDISP-PAS(IX5)      TO  W001-DET4-DLAGERE                  
348800     MOVE ART-PROC-KVDISP-P(IX5)   TO  W001-DET4-P-DLAGERE                
348900     MOVE ART-KVDISP-PAS(IX6)      TO  W001-DET4-DLAGERF                  
349000     MOVE ART-PROC-KVDISP-P(IX6)   TO  W001-DET4-P-DLAGERF                
349100     MOVE ART-KVDISP-PAS(IX7)      TO  W001-DET4-DLAGERG                  
349200     MOVE ART-PROC-KVDISP-P(IX7)   TO  W001-DET4-P-DLAGERG                
349300     MOVE ART-KVDISP-PAS(IX8)      TO  W001-DET4-DLAGERH                  
349400     MOVE ART-PROC-KVDISP-P(IX8)   TO  W001-DET4-P-DLAGERH                
349500     MOVE PSUM-KVDISP-PAS(PSUM-IX) TO  W001-DET4-TOT                      
349600     MOVE PSUM-PROC-KVDISP-P(PSUM-IX)                                     
349700                                   TO  W001-DET4-P-TOT                    
349800     MOVE W001-DETALJRAD-4 TO W001-RAD                                    
349900     MOVE +1 TO W001-SKIP                                                 
350000     PERFORM S21-SKRIV-LISTA                                              
350100                                                                          
350200     MOVE ART-LS-AKT(IX1)          TO  W001-DET5-LLAGERA                  
350300     MOVE ART-PROC-LS-A(IX1)       TO  W001-DET5-P-LLAGERA                
350400     MOVE ART-LS-AKT(IX2)          TO  W001-DET5-LLAGERB                  
350500     MOVE ART-PROC-LS-A(IX2)       TO  W001-DET5-P-LLAGERB                
350600     MOVE ART-LS-AKT(IX3)          TO  W001-DET5-LLAGERC                  
350700     MOVE ART-PROC-LS-A(IX3)       TO  W001-DET5-P-LLAGERC                
350800     MOVE ART-LS-AKT(IX4)          TO  W001-DET5-LLAGERD                  
350900     MOVE ART-PROC-LS-A(IX4)       TO  W001-DET5-P-LLAGERD                
351000     MOVE ART-LS-AKT(IX5)          TO  W001-DET5-LLAGERE                  
351100     MOVE ART-PROC-LS-A(IX5)       TO  W001-DET5-P-LLAGERE                
351200     MOVE ART-LS-AKT(IX6)          TO  W001-DET5-LLAGERF                  
351300     MOVE ART-PROC-LS-A(IX6)       TO  W001-DET5-P-LLAGERF                
351400     MOVE ART-LS-AKT(IX7)          TO  W001-DET5-LLAGERG                  
351500     MOVE ART-PROC-LS-A(IX7)       TO  W001-DET5-P-LLAGERG                
351600     MOVE ART-LS-AKT(IX8)          TO  W001-DET5-LLAGERH                  
351700     MOVE ART-PROC-LS-A(IX8)       TO  W001-DET5-P-LLAGERH                
351800     MOVE PSUM-LS-AKT(PSUM-IX)     TO  W001-DET5-TOT                      
351900     MOVE PSUM-PROC-LS-A(PSUM-IX)  TO  W001-DET5-P-TOT                    
352000     MOVE W001-DETALJRAD-5 TO W001-RAD                                    
352100     MOVE +1 TO W001-SKIP                                                 
352200     PERFORM S21-SKRIV-LISTA                                              
352300                                                                          
352400     MOVE ART-LS-PAS(IX1)          TO  W001-DET6-LLAGERA                  
352500     MOVE ART-PROC-LS-P(IX1)       TO  W001-DET6-P-LLAGERA                
352600     MOVE ART-LS-PAS(IX2)          TO  W001-DET6-LLAGERB                  
352700     MOVE ART-PROC-LS-P(IX2)       TO  W001-DET6-P-LLAGERB                
352800     MOVE ART-LS-PAS(IX3)          TO  W001-DET6-LLAGERC                  
352900     MOVE ART-PROC-LS-P(IX3)       TO  W001-DET6-P-LLAGERC                
353000     MOVE ART-LS-PAS(IX4)          TO  W001-DET6-LLAGERD                  
353100     MOVE ART-PROC-LS-P(IX4)       TO  W001-DET6-P-LLAGERD                
353200     MOVE ART-LS-PAS(IX5)          TO  W001-DET6-LLAGERE                  
353300     MOVE ART-PROC-LS-P(IX5)       TO  W001-DET6-P-LLAGERE                
353400     MOVE ART-LS-PAS(IX6)          TO  W001-DET6-LLAGERF                  
353500     MOVE ART-PROC-LS-P(IX6)       TO  W001-DET6-P-LLAGERF                
353600     MOVE ART-LS-PAS(IX7)          TO  W001-DET6-LLAGERG                  
353700     MOVE ART-PROC-LS-P(IX7)       TO  W001-DET6-P-LLAGERG                
353800     MOVE ART-LS-PAS(IX8)          TO  W001-DET6-LLAGERH                  
353900     MOVE ART-PROC-LS-P(IX8)       TO  W001-DET6-P-LLAGERH                
354000     MOVE PSUM-LS-PAS(PSUM-IX)     TO  W001-DET6-TOT                      
354100     MOVE PSUM-PROC-LS-P(PSUM-IX)  TO  W001-DET6-P-TOT                    
354200     MOVE W001-DETALJRAD-6 TO W001-RAD                                    
354300     MOVE +1 TO W001-SKIP                                                 
354400     PERFORM S21-SKRIV-LISTA                                              
354500                                                                          
354600     MOVE ART-AK-AKT(IX1)          TO  W001-DET7-ALAGERA                  
354700     MOVE ART-PROC-AK-A(IX1)       TO  W001-DET7-P-ALAGERA                
354800     MOVE ART-AK-AKT(IX2)          TO  W001-DET7-ALAGERB                  
354900     MOVE ART-PROC-AK-A(IX2)       TO  W001-DET7-P-ALAGERB                
355000     MOVE ART-AK-AKT(IX3)          TO  W001-DET7-ALAGERC                  
355100     MOVE ART-PROC-AK-A(IX3)       TO  W001-DET7-P-ALAGERC                
355200     MOVE ART-AK-AKT(IX4)          TO  W001-DET7-ALAGERD                  
355300     MOVE ART-PROC-AK-A(IX4)       TO  W001-DET7-P-ALAGERD                
355400     MOVE ART-AK-AKT(IX5)          TO  W001-DET7-ALAGERE                  
355500     MOVE ART-PROC-AK-A(IX5)       TO  W001-DET7-P-ALAGERE                
355600     MOVE ART-AK-AKT(IX6)          TO  W001-DET7-ALAGERF                  
355700     MOVE ART-PROC-AK-A(IX6)       TO  W001-DET7-P-ALAGERF                
355800     MOVE ART-AK-AKT(IX7)          TO  W001-DET7-ALAGERG                  
355900     MOVE ART-PROC-AK-A(IX7)       TO  W001-DET7-P-ALAGERG                
356000     MOVE ART-AK-AKT(IX8)          TO  W001-DET7-ALAGERH                  
356100     MOVE ART-PROC-AK-A(IX8)       TO  W001-DET7-P-ALAGERH                
356200     MOVE PSUM-AK-AKT(PSUM-IX)     TO  W001-DET7-TOT                      
356300     MOVE PSUM-PROC-AK-A(PSUM-IX)  TO  W001-DET7-P-TOT                    
356400     MOVE W001-DETALJRAD-7 TO W001-RAD                                    
356500     MOVE +1 TO W001-SKIP                                                 
356600     PERFORM S21-SKRIV-LISTA                                              
356700                                                                          
356800     MOVE ART-AK-PAS(IX1)          TO  W001-DET8-ALAGERA                  
356900     MOVE ART-PROC-AK-P(IX1)       TO  W001-DET8-P-ALAGERA                
357000     MOVE ART-AK-PAS(IX2)          TO  W001-DET8-ALAGERB                  
357100     MOVE ART-PROC-AK-P(IX2)       TO  W001-DET8-P-ALAGERB                
357200     MOVE ART-AK-PAS(IX3)          TO  W001-DET8-ALAGERC                  
357300     MOVE ART-PROC-AK-P(IX3)       TO  W001-DET8-P-ALAGERC                
357400     MOVE ART-AK-PAS(IX4)          TO  W001-DET8-ALAGERD                  
357500     MOVE ART-PROC-AK-P(IX4)       TO  W001-DET8-P-ALAGERD                
357600     MOVE ART-AK-PAS(IX5)          TO  W001-DET8-ALAGERE                  
357700     MOVE ART-PROC-AK-P(IX5)       TO  W001-DET8-P-ALAGERE                
357800     MOVE ART-AK-PAS(IX6)          TO  W001-DET8-ALAGERF                  
357900     MOVE ART-PROC-AK-P(IX6)       TO  W001-DET8-P-ALAGERF                
358000     MOVE ART-AK-PAS(IX7)          TO  W001-DET8-ALAGERG                  
358100     MOVE ART-PROC-AK-P(IX7)       TO  W001-DET8-P-ALAGERG                
358200     MOVE ART-AK-PAS(IX8)          TO  W001-DET8-ALAGERH                  
358300     MOVE ART-PROC-AK-P(IX8)       TO  W001-DET8-P-ALAGERH                
358400     MOVE PSUM-AK-PAS(PSUM-IX)     TO  W001-DET8-TOT                      
358500     MOVE PSUM-PROC-AK-P(PSUM-IX)  TO  W001-DET8-P-TOT                    
358600     MOVE W001-DETALJRAD-8 TO W001-RAD                                    
358700     MOVE +1 TO W001-SKIP                                                 
358800     PERFORM S21-SKRIV-LISTA                                              
358900                                                                          
359000     MOVE ART-OLAGER(IX1)          TO  W001-DET9-OLAGERA                  
359100     MOVE ART-PROC-OLAGER(IX1)     TO  W001-DET9-P-OLAGERA                
359200     MOVE ART-OLAGER(IX2)          TO  W001-DET9-OLAGERB                  
359300     MOVE ART-PROC-OLAGER(IX2)     TO  W001-DET9-P-OLAGERB                
359400     MOVE ART-OLAGER(IX3)          TO  W001-DET9-OLAGERC                  
359500     MOVE ART-PROC-OLAGER(IX3)     TO  W001-DET9-P-OLAGERC                
359600     MOVE ART-OLAGER(IX4)          TO  W001-DET9-OLAGERD                  
359700     MOVE ART-PROC-OLAGER(IX4)     TO  W001-DET9-P-OLAGERD                
359800     MOVE ART-OLAGER(IX5)          TO  W001-DET9-OLAGERE                  
359900     MOVE ART-PROC-OLAGER(IX5)     TO  W001-DET9-P-OLAGERE                
360000     MOVE ART-OLAGER(IX6)          TO  W001-DET9-OLAGERF                  
360100     MOVE ART-PROC-OLAGER(IX6)     TO  W001-DET9-P-OLAGERF                
360200     MOVE ART-OLAGER(IX7)          TO  W001-DET9-OLAGERG                  
360300     MOVE ART-PROC-OLAGER(IX7)     TO  W001-DET9-P-OLAGERG                
360400     MOVE ART-OLAGER(IX8)          TO  W001-DET9-OLAGERH                  
360500     MOVE ART-PROC-OLAGER(IX8)     TO  W001-DET9-P-OLAGERH                
360600     MOVE PSUM-OLAGER(PSUM-IX)     TO  W001-DET9-TOT                      
360700     MOVE PSUM-PROC-OLAGER(PSUM-IX)                                       
360800                                   TO  W001-DET9-P-TOT                    
360900     MOVE W001-DETALJRAD-9 TO W001-RAD                                    
361000     MOVE +1 TO W001-SKIP                                                 
361100     PERFORM S21-SKRIV-LISTA                                              
361200                                                                          
361300     MOVE ART-SLAGER(IX1)          TO  W001-DET10-SLAGERA                 
361400     MOVE ART-PROC-SLAGER(IX1)     TO  W001-DET10-P-SLAGERA               
361500     MOVE ART-SLAGER(IX2)          TO  W001-DET10-SLAGERB                 
361600     MOVE ART-PROC-SLAGER(IX2)     TO  W001-DET10-P-SLAGERB               
361700     MOVE ART-SLAGER(IX3)          TO  W001-DET10-SLAGERC                 
361800     MOVE ART-PROC-SLAGER(IX3)     TO  W001-DET10-P-SLAGERC               
361900     MOVE ART-SLAGER(IX4)          TO  W001-DET10-SLAGERD                 
362000     MOVE ART-PROC-SLAGER(IX4)     TO  W001-DET10-P-SLAGERD               
362100     MOVE ART-SLAGER(IX5)          TO  W001-DET10-SLAGERE                 
362200     MOVE ART-PROC-SLAGER(IX5)     TO  W001-DET10-P-SLAGERE               
362300     MOVE ART-SLAGER(IX6)          TO  W001-DET10-SLAGERF                 
362400     MOVE ART-PROC-SLAGER(IX6)     TO  W001-DET10-P-SLAGERF               
362500     MOVE ART-SLAGER(IX7)          TO  W001-DET10-SLAGERG                 
362600     MOVE ART-PROC-SLAGER(IX7)     TO  W001-DET10-P-SLAGERG               
362700     MOVE ART-SLAGER(IX8)          TO  W001-DET10-SLAGERH                 
362800     MOVE ART-PROC-SLAGER(IX8)     TO  W001-DET10-P-SLAGERH               
362900     MOVE PSUM-SLAGER(PSUM-IX)     TO  W001-DET10-TOT                     
363000     MOVE PSUM-PROC-SLAGER(PSUM-IX)                                       
363100                                   TO  W001-DET10-P-TOT                   
363200     MOVE W001-DETALJRAD-10 TO W001-RAD                                   
363300     MOVE +1 TO W001-SKIP                                                 
363400     PERFORM S21-SKRIV-LISTA                                              
363500                                                                          
363600     MOVE ART-MLAGER(IX1)          TO  W001-DET11-MLAGERA                 
363700     MOVE ART-PROC-MLAGER(IX1)     TO  W001-DET11-P-MLAGERA               
363800     MOVE ART-MLAGER(IX2)          TO  W001-DET11-MLAGERB                 
363900     MOVE ART-PROC-MLAGER(IX2)     TO  W001-DET11-P-MLAGERB               
364000     MOVE ART-MLAGER(IX3)          TO  W001-DET11-MLAGERC                 
364100     MOVE ART-PROC-MLAGER(IX3)     TO  W001-DET11-P-MLAGERC               
364200     MOVE ART-MLAGER(IX4)          TO  W001-DET11-MLAGERD                 
364300     MOVE ART-PROC-MLAGER(IX4)     TO  W001-DET11-P-MLAGERD               
364400     MOVE ART-MLAGER(IX5)          TO  W001-DET11-MLAGERE                 
364500     MOVE ART-PROC-MLAGER(IX5)     TO  W001-DET11-P-MLAGERE               
364600     MOVE ART-MLAGER(IX6)          TO  W001-DET11-MLAGERF                 
364700     MOVE ART-PROC-MLAGER(IX6)     TO  W001-DET11-P-MLAGERF               
364800     MOVE ART-MLAGER(IX7)          TO  W001-DET11-MLAGERG                 
364900     MOVE ART-PROC-MLAGER(IX7)     TO  W001-DET11-P-MLAGERG               
365000     MOVE ART-MLAGER(IX8)          TO  W001-DET11-MLAGERH                 
365100     MOVE ART-PROC-MLAGER(IX8)     TO  W001-DET11-P-MLAGERH               
365200     MOVE PSUM-MLAGER(PSUM-IX)     TO  W001-DET11-TOT                     
365300     MOVE PSUM-PROC-MLAGER(PSUM-IX)                                       
365400                                   TO  W001-DET11-P-TOT                   
365500     MOVE W001-DETALJRAD-11 TO W001-RAD                                   
365600     MOVE +1 TO W001-SKIP                                                 
365700     PERFORM S21-SKRIV-LISTA                                              
365800                                                                          
365900     MOVE ART-KVOT(IX1)            TO  W001-DET12-KVOTA                   
366000     MOVE ART-PROC-KVOT(IX1)       TO  W001-DET12-P-KVOTA                 
366100     MOVE ART-KVOT(IX2)            TO  W001-DET12-KVOTB                   
366200     MOVE ART-PROC-KVOT(IX2)       TO  W001-DET12-P-KVOTB                 
366300     MOVE ART-KVOT(IX3)            TO  W001-DET12-KVOTC                   
366400     MOVE ART-PROC-KVOT(IX3)       TO  W001-DET12-P-KVOTC                 
366500     MOVE ART-KVOT(IX4)            TO  W001-DET12-KVOTD                   
366600     MOVE ART-PROC-KVOT(IX4)       TO  W001-DET12-P-KVOTD                 
366700     MOVE ART-KVOT(IX5)            TO  W001-DET12-KVOTE                   
366800     MOVE ART-PROC-KVOT(IX5)       TO  W001-DET12-P-KVOTE                 
366900     MOVE ART-KVOT(IX6)            TO  W001-DET12-KVOTF                   
367000     MOVE ART-PROC-KVOT(IX6)       TO  W001-DET12-P-KVOTF                 
367100     MOVE ART-KVOT(IX7)            TO  W001-DET12-KVOTG                   
367200     MOVE ART-PROC-KVOT(IX7)       TO  W001-DET12-P-KVOTG                 
367300     MOVE ART-KVOT(IX8)            TO  W001-DET12-KVOTH                   
367400     MOVE ART-PROC-KVOT(IX8)       TO  W001-DET12-P-KVOTH                 
367500     MOVE PSUM-KVOT(PSUM-IX)       TO  W001-DET12-TOT                     
367600     MOVE PSUM-PROC-KVOT(PSUM-IX)  TO  W001-DET12-P-TOT                   
367700     MOVE W001-DETALJRAD-12 TO W001-RAD                                   
367800     MOVE +1 TO W001-SKIP                                                 
367900     PERFORM S21-SKRIV-LISTA                                              
368000                                                                          
368100     MOVE ART-SPLIT(IX1)           TO  W001-DET13-SPLITA                  
368200     MOVE ART-SPLIT(IX2)           TO  W001-DET13-SPLITB                  
368300     MOVE ART-SPLIT(IX3)           TO  W001-DET13-SPLITC                  
368400     MOVE ART-SPLIT(IX4)           TO  W001-DET13-SPLITD                  
368500     MOVE ART-SPLIT(IX5)           TO  W001-DET13-SPLITE                  
368600     MOVE ART-SPLIT(IX6)           TO  W001-DET13-SPLITF                  
368700     MOVE ART-SPLIT(IX7)           TO  W001-DET13-SPLITG                  
368800     MOVE ART-SPLIT(IX8)           TO  W001-DET13-SPLITH                  
368900     MOVE PSUM-SPLIT(PSUM-IX)      TO  W001-DET13-TOT                     
369000     MOVE ZERO                     TO  W001-DET13-P-TOT                   
369100     MOVE W001-DETALJRAD-13 TO W001-RAD                                   
369200     MOVE +1 TO W001-SKIP                                                 
369300     PERFORM S21-SKRIV-LISTA                                              
369400                                                                          
369500     MOVE ART-OMSHAST-DISP(IX1)    TO  W001-DET14-OMSHASTA                
369600     MOVE ART-OMSHAST-PROC-D(IX1)  TO  W001-DET14-P-OMSHASTA              
369700     MOVE ART-OMSHAST-DISP(IX2)    TO  W001-DET14-OMSHASTB                
369800     MOVE ART-OMSHAST-PROC-D(IX2)  TO  W001-DET14-P-OMSHASTB              
369900     MOVE ART-OMSHAST-DISP(IX3)    TO  W001-DET14-OMSHASTC                
370000     MOVE ART-OMSHAST-PROC-D(IX3)  TO  W001-DET14-P-OMSHASTC              
370100     MOVE ART-OMSHAST-DISP(IX4)    TO  W001-DET14-OMSHASTD                
370200     MOVE ART-OMSHAST-PROC-D(IX4)  TO  W001-DET14-P-OMSHASTD              
370300     MOVE ART-OMSHAST-DISP(IX5)    TO  W001-DET14-OMSHASTE                
370400     MOVE ART-OMSHAST-PROC-D(IX5)  TO  W001-DET14-P-OMSHASTE              
370500     MOVE ART-OMSHAST-DISP(IX6)    TO  W001-DET14-OMSHASTF                
370600     MOVE ART-OMSHAST-PROC-D(IX6)  TO  W001-DET14-P-OMSHASTF              
370700     MOVE ART-OMSHAST-DISP(IX7)    TO  W001-DET14-OMSHASTG                
370800     MOVE ART-OMSHAST-PROC-D(IX7)  TO  W001-DET14-P-OMSHASTG              
370900     MOVE ART-OMSHAST-DISP(IX8)    TO  W001-DET14-OMSHASTH                
371000     MOVE ART-OMSHAST-PROC-D(IX8)  TO  W001-DET14-P-OMSHASTH              
371100     MOVE PSUM-OMSHAST-DISP(PSUM-IX) TO W001-DET14-TOT                    
371200     MOVE PSUM-OMSHAST-PROC-D(PSUM-IX)                                    
371300                                     TO W001-DET14-P-TOT                  
371400     MOVE W001-DETALJRAD-14 TO W001-RAD                                   
371500     MOVE +1 TO W001-SKIP                                                 
371600     PERFORM S21-SKRIV-LISTA                                              
371700                                                                          
371800     MOVE ART-OMSHAST-LS(IX1)      TO  W001-DET15-OMSHASTA                
371900     MOVE ART-OMSHAST-PROC-LS(IX1) TO  W001-DET15-P-OMSHASTA              
372000     MOVE ART-OMSHAST-LS(IX2)      TO  W001-DET15-OMSHASTB                
372100     MOVE ART-OMSHAST-PROC-LS(IX2) TO  W001-DET15-P-OMSHASTB              
372200     MOVE ART-OMSHAST-LS(IX3)      TO  W001-DET15-OMSHASTC                
372300     MOVE ART-OMSHAST-PROC-LS(IX3) TO  W001-DET15-P-OMSHASTC              
372400     MOVE ART-OMSHAST-LS(IX4)      TO  W001-DET15-OMSHASTD                
372500     MOVE ART-OMSHAST-PROC-LS(IX4) TO  W001-DET15-P-OMSHASTD              
372600     MOVE ART-OMSHAST-LS(IX5)      TO  W001-DET15-OMSHASTE                
372700     MOVE ART-OMSHAST-PROC-LS(IX5) TO  W001-DET15-P-OMSHASTE              
372800     MOVE ART-OMSHAST-LS(IX6)      TO  W001-DET15-OMSHASTF                
372900     MOVE ART-OMSHAST-PROC-LS(IX6) TO  W001-DET15-P-OMSHASTF              
373000     MOVE ART-OMSHAST-LS(IX7)      TO  W001-DET15-OMSHASTG                
373100     MOVE ART-OMSHAST-PROC-LS(IX7) TO  W001-DET15-P-OMSHASTG              
373200     MOVE ART-OMSHAST-LS(IX8)      TO  W001-DET15-OMSHASTH                
373300     MOVE ART-OMSHAST-PROC-LS(IX8) TO  W001-DET15-P-OMSHASTH              
373400     MOVE PSUM-OMSHAST-LS(PSUM-IX) TO  W001-DET15-TOT                     
373500     MOVE PSUM-OMSHAST-PROC-LS(PSUM-IX)                                   
373600                                     TO W001-DET15-P-TOT                  
373700     MOVE W001-DETALJRAD-15 TO W001-RAD                                   
373800     MOVE +1 TO W001-SKIP                                                 
373900     PERFORM S21-SKRIV-LISTA                                              
374000                                                                          
374100     MOVE ART-SERVG-AKT(IX1)       TO  W001-DET16-SERVGA-A                
374200     MOVE ART-SERVG-PAS(IX1)       TO  W001-DET16-SERVGA-P                
374300     MOVE ART-SERVG-AKT(IX2)       TO  W001-DET16-SERVGB-A                
374400     MOVE ART-SERVG-PAS(IX2)       TO  W001-DET16-SERVGB-P                
374500     MOVE ART-SERVG-AKT(IX3)       TO  W001-DET16-SERVGC-A                
374600     MOVE ART-SERVG-PAS(IX3)       TO  W001-DET16-SERVGC-P                
374700     MOVE ART-SERVG-AKT(IX4)       TO  W001-DET16-SERVGD-A                
374800     MOVE ART-SERVG-PAS(IX4)       TO  W001-DET16-SERVGD-P                
374900     MOVE ART-SERVG-AKT(IX5)       TO  W001-DET16-SERVGE-A                
375000     MOVE ART-SERVG-PAS(IX5)       TO  W001-DET16-SERVGE-P                
375100     MOVE ART-SERVG-AKT(IX6)       TO  W001-DET16-SERVGF-A                
375200     MOVE ART-SERVG-PAS(IX6)       TO  W001-DET16-SERVGF-P                
375300     MOVE ART-SERVG-AKT(IX7)       TO  W001-DET16-SERVGG-A                
375400     MOVE ART-SERVG-PAS(IX7)       TO  W001-DET16-SERVGG-P                
375500     MOVE ART-SERVG-AKT(IX8)       TO  W001-DET16-SERVGH-A                
375600     MOVE ART-SERVG-PAS(IX8)       TO  W001-DET16-SERVGH-P                
375700     MOVE PSUM-SERVG-AKT (PSUM-IX) TO  W001-DET16-TOT                     
375800     MOVE PSUM-SERVG-PAS                                                  
375900          (PSUM-IX)                TO  W001-DET16-P-TOT                   
376000     MOVE W001-DETALJRAD-16 TO W001-RAD                                   
376100     MOVE +1 TO W001-SKIP                                                 
376200     PERFORM S21-SKRIV-LISTA                                              
376300                                                                          
376400     MOVE ART-SERVG-TOT(IX1)       TO  W001-DET17-SERVGA-TOT              
376500     MOVE ART-SERVG-TOT(IX2)       TO  W001-DET17-SERVGB-TOT              
376600     MOVE ART-SERVG-TOT(IX3)       TO  W001-DET17-SERVGC-TOT              
376700     MOVE ART-SERVG-TOT(IX4)       TO  W001-DET17-SERVGD-TOT              
376800     MOVE ART-SERVG-TOT(IX5)       TO  W001-DET17-SERVGE-TOT              
376900     MOVE ART-SERVG-TOT(IX6)       TO  W001-DET17-SERVGF-TOT              
377000     MOVE ART-SERVG-TOT(IX7)       TO  W001-DET17-SERVGG-TOT              
377100     MOVE ART-SERVG-TOT(IX8)       TO  W001-DET17-SERVGH-TOT              
377200     MOVE PSUM-SERVG-TOT(PSUM-IX)  TO  W001-DET17-TOT                     
377300     MOVE ZERO                     TO  W001-DET17-P-TOT                   
377400     MOVE W001-DETALJRAD-17 TO W001-RAD                                   
377500     MOVE +1 TO W001-SKIP                                                 
377600     PERFORM S21-SKRIV-LISTA                                              
377700                                                                          
377800     MOVE ART-SERVG-TEO(IX1)       TO  W001-DET18-SERVGA-TEO              
377900     MOVE ART-SERVG-TEO(IX2)       TO  W001-DET18-SERVGB-TEO              
378000     MOVE ART-SERVG-TEO(IX3)       TO  W001-DET18-SERVGC-TEO              
378100     MOVE ART-SERVG-TEO(IX4)       TO  W001-DET18-SERVGD-TEO              
378200     MOVE ART-SERVG-TEO(IX5)       TO  W001-DET18-SERVGE-TEO              
378300     MOVE ART-SERVG-TEO(IX6)       TO  W001-DET18-SERVGF-TEO              
378400     MOVE ART-SERVG-TEO(IX7)       TO  W001-DET18-SERVGG-TEO              
378500     MOVE ART-SERVG-TEO(IX8)       TO  W001-DET18-SERVGH-TEO              
378600     MOVE PSUM-SERVG-TEO(PSUM-IX)  TO  W001-DET18-TOT                     
378700     MOVE ZERO                     TO  W001-DET18-P-TOT                   
378800     MOVE W001-DETALJRAD-18 TO W001-RAD                                   
378900     MOVE +1 TO W001-SKIP                                                 
379000     PERFORM S21-SKRIV-LISTA                                              
379100     .                                                                    
379200     EJECT                                                                
379300 CB-FLYTTA-SKRIV-TOT SECTION.                                             
379400                                                                          
379500     MOVE FSUM-KVANT-AKT(IX1)      TO  W001-DET1-KVANTA                   
379600     MOVE FSUM-PROC-KVANT-A(IX1)   TO  W001-DET1-P-KVANTA                 
379700     MOVE FSUM-KVANT-AKT(IX2)      TO  W001-DET1-KVANTB                   
379800     MOVE FSUM-PROC-KVANT-A(IX2)   TO  W001-DET1-P-KVANTB                 
379900     MOVE FSUM-KVANT-AKT(IX3)      TO  W001-DET1-KVANTC                   
380000     MOVE FSUM-PROC-KVANT-A(IX3)   TO  W001-DET1-P-KVANTC                 
380100     MOVE FSUM-KVANT-AKT(IX4)      TO  W001-DET1-KVANTD                   
380200     MOVE FSUM-PROC-KVANT-A(IX4)   TO  W001-DET1-P-KVANTD                 
380300     MOVE FSUM-KVANT-AKT(IX5)      TO  W001-DET1-KVANTE                   
380400     MOVE FSUM-PROC-KVANT-A(IX5)   TO  W001-DET1-P-KVANTE                 
380500     MOVE FSUM-KVANT-AKT(IX6)      TO  W001-DET1-KVANTF                   
380600     MOVE FSUM-PROC-KVANT-A(IX6)   TO  W001-DET1-P-KVANTF                 
380700     MOVE FSUM-KVANT-AKT(IX7)      TO  W001-DET1-KVANTG                   
380800     MOVE FSUM-PROC-KVANT-A(IX7)   TO  W001-DET1-P-KVANTG                 
380900     MOVE FSUM-KVANT-AKT(IX8)      TO  W001-DET1-KVANTH                   
381000     MOVE FSUM-PROC-KVANT-A(IX8)   TO  W001-DET1-P-KVANTH                 
381100     MOVE TOT-KVANT-AKT            TO  W001-DET1-TOT                      
381200     MOVE ZERO                     TO  W001-DET1-P-TOT                    
381300     MOVE W001-DETALJRAD-1 TO W001-RAD                                    
381400     MOVE +2 TO W001-SKIP                                                 
381500     PERFORM S21-SKRIV-LISTA                                              
381600                                                                          
381700     MOVE FSUM-KVANT-PAS(IX1)      TO  W001-DET2-KVANTA                   
381800     MOVE FSUM-PROC-KVANT-P(IX1)   TO  W001-DET2-P-KVANTA                 
381900     MOVE FSUM-KVANT-PAS(IX2)      TO  W001-DET2-KVANTB                   
382000     MOVE FSUM-PROC-KVANT-P(IX2)   TO  W001-DET2-P-KVANTB                 
382100     MOVE FSUM-KVANT-PAS(IX3)      TO  W001-DET2-KVANTC                   
382200     MOVE FSUM-PROC-KVANT-P(IX3)   TO  W001-DET2-P-KVANTC                 
382300     MOVE FSUM-KVANT-PAS(IX4)      TO  W001-DET2-KVANTD                   
382400     MOVE FSUM-PROC-KVANT-P(IX4)   TO  W001-DET2-P-KVANTD                 
382500     MOVE FSUM-KVANT-PAS(IX5)      TO  W001-DET2-KVANTE                   
382600     MOVE FSUM-PROC-KVANT-P(IX5)   TO  W001-DET2-P-KVANTE                 
382700     MOVE FSUM-KVANT-PAS(IX6)      TO  W001-DET2-KVANTF                   
382800     MOVE FSUM-PROC-KVANT-P(IX6)   TO  W001-DET2-P-KVANTF                 
382900     MOVE FSUM-KVANT-PAS(IX7)      TO  W001-DET2-KVANTG                   
383000     MOVE FSUM-PROC-KVANT-P(IX7)   TO  W001-DET2-P-KVANTG                 
383100     MOVE FSUM-KVANT-PAS(IX8)      TO  W001-DET2-KVANTH                   
383200     MOVE FSUM-PROC-KVANT-P(IX8)   TO  W001-DET2-P-KVANTH                 
383300     MOVE TOT-KVANT-PAS            TO  W001-DET2-TOT                      
383400     MOVE ZERO                     TO  W001-DET2-P-TOT                    
383500     MOVE W001-DETALJRAD-2 TO W001-RAD                                    
383600     MOVE +1 TO W001-SKIP                                                 
383700     PERFORM S21-SKRIV-LISTA                                              
383800                                                                          
383900     MOVE FSUM-KVDISP-AKT(IX1)     TO  W001-DET3-DLAGERA                  
384000     MOVE FSUM-PROC-KVDISP-A(IX1)  TO  W001-DET3-P-DLAGERA                
384100     MOVE FSUM-KVDISP-AKT(IX2)     TO  W001-DET3-DLAGERB                  
384200     MOVE FSUM-PROC-KVDISP-A(IX2)  TO  W001-DET3-P-DLAGERB                
384300     MOVE FSUM-KVDISP-AKT(IX3)     TO  W001-DET3-DLAGERC                  
384400     MOVE FSUM-PROC-KVDISP-A(IX3)  TO  W001-DET3-P-DLAGERC                
384500     MOVE FSUM-KVDISP-AKT(IX4)     TO  W001-DET3-DLAGERD                  
384600     MOVE FSUM-PROC-KVDISP-A(IX4)  TO  W001-DET3-P-DLAGERD                
384700     MOVE FSUM-KVDISP-AKT(IX5)     TO  W001-DET3-DLAGERE                  
384800     MOVE FSUM-PROC-KVDISP-A(IX5)  TO  W001-DET3-P-DLAGERE                
384900     MOVE FSUM-KVDISP-AKT(IX6)     TO  W001-DET3-DLAGERF                  
385000     MOVE FSUM-PROC-KVDISP-A(IX6)  TO  W001-DET3-P-DLAGERF                
385100     MOVE FSUM-KVDISP-AKT(IX7)     TO  W001-DET3-DLAGERG                  
385200     MOVE FSUM-PROC-KVDISP-A(IX7)  TO  W001-DET3-P-DLAGERG                
385300     MOVE FSUM-KVDISP-AKT(IX8)     TO  W001-DET3-DLAGERH                  
385400     MOVE FSUM-PROC-KVDISP-A(IX8)  TO  W001-DET3-P-DLAGERH                
385500     MOVE TOT-KVDISP-AKT           TO  W001-DET3-TOT                      
385600     MOVE ZERO                     TO  W001-DET3-P-TOT                    
385700     MOVE W001-DETALJRAD-3 TO W001-RAD                                    
385800     MOVE +1 TO W001-SKIP                                                 
385900     PERFORM S21-SKRIV-LISTA                                              
386000                                                                          
386100     MOVE FSUM-KVDISP-PAS(IX1)     TO  W001-DET4-DLAGERA                  
386200     MOVE FSUM-PROC-KVDISP-P(IX1)  TO  W001-DET4-P-DLAGERA                
386300     MOVE FSUM-KVDISP-PAS(IX2)     TO  W001-DET4-DLAGERB                  
386400     MOVE FSUM-PROC-KVDISP-P(IX2)  TO  W001-DET4-P-DLAGERB                
386500     MOVE FSUM-KVDISP-PAS(IX3)     TO  W001-DET4-DLAGERC                  
386600     MOVE FSUM-PROC-KVDISP-P(IX3)  TO  W001-DET4-P-DLAGERC                
386700     MOVE FSUM-KVDISP-PAS(IX4)     TO  W001-DET4-DLAGERD                  
386800     MOVE FSUM-PROC-KVDISP-P(IX4)  TO  W001-DET4-P-DLAGERD                
386900     MOVE FSUM-KVDISP-PAS(IX5)     TO  W001-DET4-DLAGERE                  
387000     MOVE FSUM-PROC-KVDISP-P(IX5)  TO  W001-DET4-P-DLAGERE                
387100     MOVE FSUM-KVDISP-PAS(IX6)     TO  W001-DET4-DLAGERF                  
387200     MOVE FSUM-PROC-KVDISP-P(IX6)  TO  W001-DET4-P-DLAGERF                
387300     MOVE FSUM-KVDISP-PAS(IX7)     TO  W001-DET4-DLAGERG                  
387400     MOVE FSUM-PROC-KVDISP-P(IX7)  TO  W001-DET4-P-DLAGERG                
387500     MOVE FSUM-KVDISP-PAS(IX8)     TO  W001-DET4-DLAGERH                  
387600     MOVE FSUM-PROC-KVDISP-P(IX8)  TO  W001-DET4-P-DLAGERH                
387700     MOVE TOT-KVDISP-PAS           TO  W001-DET4-TOT                      
387800     MOVE ZERO                     TO  W001-DET4-P-TOT                    
387900     MOVE W001-DETALJRAD-4 TO W001-RAD                                    
388000     MOVE +1 TO W001-SKIP                                                 
388100     PERFORM S21-SKRIV-LISTA                                              
388200                                                                          
388300     MOVE FSUM-LS-AKT(IX1)         TO  W001-DET5-LLAGERA                  
388400     MOVE FSUM-PROC-LS-A(IX1)      TO  W001-DET5-P-LLAGERA                
388500     MOVE FSUM-LS-AKT(IX2)         TO  W001-DET5-LLAGERB                  
388600     MOVE FSUM-PROC-LS-A(IX2)      TO  W001-DET5-P-LLAGERB                
388700     MOVE FSUM-LS-AKT(IX3)         TO  W001-DET5-LLAGERC                  
388800     MOVE FSUM-PROC-LS-A(IX3)      TO  W001-DET5-P-LLAGERC                
388900     MOVE FSUM-LS-AKT(IX4)         TO  W001-DET5-LLAGERD                  
389000     MOVE FSUM-PROC-LS-A(IX4)      TO  W001-DET5-P-LLAGERD                
389100     MOVE FSUM-LS-AKT(IX5)         TO  W001-DET5-LLAGERE                  
389200     MOVE FSUM-PROC-LS-A(IX5)      TO  W001-DET5-P-LLAGERE                
389300     MOVE FSUM-LS-AKT(IX6)         TO  W001-DET5-LLAGERF                  
389400     MOVE FSUM-PROC-LS-A(IX6)      TO  W001-DET5-P-LLAGERF                
389500     MOVE FSUM-LS-AKT(IX7)         TO  W001-DET5-LLAGERG                  
389600     MOVE FSUM-PROC-LS-A(IX7)      TO  W001-DET5-P-LLAGERG                
389700     MOVE FSUM-LS-AKT(IX8)         TO  W001-DET5-LLAGERH                  
389800     MOVE FSUM-PROC-LS-A(IX8)      TO  W001-DET5-P-LLAGERH                
389900     MOVE TOT-LS-AKT               TO  W001-DET5-TOT                      
390000     MOVE ZERO                     TO  W001-DET5-P-TOT                    
390100     MOVE W001-DETALJRAD-5 TO W001-RAD                                    
390200     MOVE +1 TO W001-SKIP                                                 
390300     PERFORM S21-SKRIV-LISTA                                              
390400                                                                          
390500     MOVE FSUM-LS-PAS(IX1)         TO  W001-DET6-LLAGERA                  
390600     MOVE FSUM-PROC-LS-P(IX1)      TO  W001-DET6-P-LLAGERA                
390700     MOVE FSUM-LS-PAS(IX2)         TO  W001-DET6-LLAGERB                  
390800     MOVE FSUM-PROC-LS-P(IX2)      TO  W001-DET6-P-LLAGERB                
390900     MOVE FSUM-LS-PAS(IX3)         TO  W001-DET6-LLAGERC                  
391000     MOVE FSUM-PROC-LS-P(IX3)      TO  W001-DET6-P-LLAGERC                
391100     MOVE FSUM-LS-PAS(IX4)         TO  W001-DET6-LLAGERD                  
391200     MOVE FSUM-PROC-LS-P(IX4)      TO  W001-DET6-P-LLAGERD                
391300     MOVE FSUM-LS-PAS(IX5)         TO  W001-DET6-LLAGERE                  
391400     MOVE FSUM-PROC-LS-P(IX5)      TO  W001-DET6-P-LLAGERE                
391500     MOVE FSUM-LS-PAS(IX6)         TO  W001-DET6-LLAGERF                  
391600     MOVE FSUM-PROC-LS-P(IX6)      TO  W001-DET6-P-LLAGERF                
391700     MOVE FSUM-LS-PAS(IX7)         TO  W001-DET6-LLAGERG                  
391800     MOVE FSUM-PROC-LS-P(IX7)      TO  W001-DET6-P-LLAGERG                
391900     MOVE FSUM-LS-PAS(IX8)         TO  W001-DET6-LLAGERH                  
392000     MOVE FSUM-PROC-LS-P(IX8)      TO  W001-DET6-P-LLAGERH                
392100     MOVE TOT-LS-PAS               TO  W001-DET6-TOT                      
392200     MOVE ZERO                     TO  W001-DET6-P-TOT                    
392300     MOVE W001-DETALJRAD-6 TO W001-RAD                                    
392400     MOVE +1 TO W001-SKIP                                                 
392500     PERFORM S21-SKRIV-LISTA                                              
392600                                                                          
392700     MOVE FSUM-AK-AKT(IX1)         TO  W001-DET7-ALAGERA                  
392800     MOVE FSUM-PROC-AK-A(IX1)      TO  W001-DET7-P-ALAGERA                
392900     MOVE FSUM-AK-AKT(IX2)         TO  W001-DET7-ALAGERB                  
393000     MOVE FSUM-PROC-AK-A(IX2)      TO  W001-DET7-P-ALAGERB                
393100     MOVE FSUM-AK-AKT(IX3)         TO  W001-DET7-ALAGERC                  
393200     MOVE FSUM-PROC-AK-A(IX3)      TO  W001-DET7-P-ALAGERC                
393300     MOVE FSUM-AK-AKT(IX4)         TO  W001-DET7-ALAGERD                  
393400     MOVE FSUM-PROC-AK-A(IX4)      TO  W001-DET7-P-ALAGERD                
393500     MOVE FSUM-AK-AKT(IX5)         TO  W001-DET7-ALAGERE                  
393600     MOVE FSUM-PROC-AK-A(IX5)      TO  W001-DET7-P-ALAGERE                
393700     MOVE FSUM-AK-AKT(IX6)         TO  W001-DET7-ALAGERF                  
393800     MOVE FSUM-PROC-AK-A(IX6)      TO  W001-DET7-P-ALAGERF                
393900     MOVE FSUM-AK-AKT(IX7)         TO  W001-DET7-ALAGERG                  
394000     MOVE FSUM-PROC-AK-A(IX7)      TO  W001-DET7-P-ALAGERG                
394100     MOVE FSUM-AK-AKT(IX8)         TO  W001-DET7-ALAGERH                  
394200     MOVE FSUM-PROC-AK-A(IX8)      TO  W001-DET7-P-ALAGERH                
394300     MOVE TOT-AK-AKT               TO  W001-DET7-TOT                      
394400     MOVE ZERO                     TO  W001-DET7-P-TOT                    
394500     MOVE W001-DETALJRAD-7 TO W001-RAD                                    
394600     MOVE +1 TO W001-SKIP                                                 
394700     PERFORM S21-SKRIV-LISTA                                              
394800                                                                          
394900     MOVE FSUM-AK-PAS(IX1)         TO  W001-DET8-ALAGERA                  
395000     MOVE FSUM-PROC-AK-P(IX1)      TO  W001-DET8-P-ALAGERA                
395100     MOVE FSUM-AK-PAS(IX2)         TO  W001-DET8-ALAGERB                  
395200     MOVE FSUM-PROC-AK-P(IX2)      TO  W001-DET8-P-ALAGERB                
395300     MOVE FSUM-AK-PAS(IX3)         TO  W001-DET8-ALAGERC                  
395400     MOVE FSUM-PROC-AK-P(IX3)      TO  W001-DET8-P-ALAGERC                
395500     MOVE FSUM-AK-PAS(IX4)         TO  W001-DET8-ALAGERD                  
395600     MOVE FSUM-PROC-AK-P(IX4)      TO  W001-DET8-P-ALAGERD                
395700     MOVE FSUM-AK-PAS(IX5)         TO  W001-DET8-ALAGERE                  
395800     MOVE FSUM-PROC-AK-P(IX5)      TO  W001-DET8-P-ALAGERE                
395900     MOVE FSUM-AK-PAS(IX6)         TO  W001-DET8-ALAGERF                  
396000     MOVE FSUM-PROC-AK-P(IX6)      TO  W001-DET8-P-ALAGERF                
396100     MOVE FSUM-AK-PAS(IX7)         TO  W001-DET8-ALAGERG                  
396200     MOVE FSUM-PROC-AK-P(IX7)      TO  W001-DET8-P-ALAGERG                
396300     MOVE FSUM-AK-PAS(IX8)         TO  W001-DET8-ALAGERH                  
396400     MOVE FSUM-PROC-AK-P(IX8)      TO  W001-DET8-P-ALAGERH                
396500     MOVE TOT-AK-PAS               TO  W001-DET8-TOT                      
396600     MOVE ZERO                     TO  W001-DET8-P-TOT                    
396700     MOVE W001-DETALJRAD-8 TO W001-RAD                                    
396800     MOVE +1 TO W001-SKIP                                                 
396900     PERFORM S21-SKRIV-LISTA                                              
397000                                                                          
397100     MOVE FSUM-OLAGER(IX1)         TO  W001-DET9-OLAGERA                  
397200     MOVE FSUM-PROC-OLAGER(IX1)    TO  W001-DET9-P-OLAGERA                
397300     MOVE FSUM-OLAGER(IX2)         TO  W001-DET9-OLAGERB                  
397400     MOVE FSUM-PROC-OLAGER(IX2)    TO  W001-DET9-P-OLAGERB                
397500     MOVE FSUM-OLAGER(IX3)         TO  W001-DET9-OLAGERC                  
397600     MOVE FSUM-PROC-OLAGER(IX3)    TO  W001-DET9-P-OLAGERC                
397700     MOVE FSUM-OLAGER(IX4)         TO  W001-DET9-OLAGERD                  
397800     MOVE FSUM-PROC-OLAGER(IX4)    TO  W001-DET9-P-OLAGERD                
397900     MOVE FSUM-OLAGER(IX5)         TO  W001-DET9-OLAGERE                  
398000     MOVE FSUM-PROC-OLAGER(IX5)    TO  W001-DET9-P-OLAGERE                
398100     MOVE FSUM-OLAGER(IX6)         TO  W001-DET9-OLAGERF                  
398200     MOVE FSUM-PROC-OLAGER(IX6)    TO  W001-DET9-P-OLAGERF                
398300     MOVE FSUM-OLAGER(IX7)         TO  W001-DET9-OLAGERG                  
398400     MOVE FSUM-PROC-OLAGER(IX7)    TO  W001-DET9-P-OLAGERG                
398500     MOVE FSUM-OLAGER(IX8)         TO  W001-DET9-OLAGERH                  
398600     MOVE FSUM-PROC-OLAGER(IX8)    TO  W001-DET9-P-OLAGERH                
398700     MOVE TOT-OLAGER               TO  W001-DET9-TOT                      
398800     MOVE TOT-PROC-OLAGER          TO  W001-DET9-P-TOT                    
398900     MOVE W001-DETALJRAD-9 TO W001-RAD                                    
399000     MOVE +1 TO W001-SKIP                                                 
399100     PERFORM S21-SKRIV-LISTA                                              
399200                                                                          
399300     MOVE FSUM-SLAGER(IX1)         TO  W001-DET10-SLAGERA                 
399400     MOVE FSUM-PROC-SLAGER(IX1)    TO  W001-DET10-P-SLAGERA               
399500     MOVE FSUM-SLAGER(IX2)         TO  W001-DET10-SLAGERB                 
399600     MOVE FSUM-PROC-SLAGER(IX2)    TO  W001-DET10-P-SLAGERB               
399700     MOVE FSUM-SLAGER(IX3)         TO  W001-DET10-SLAGERC                 
399800     MOVE FSUM-PROC-SLAGER(IX3)    TO  W001-DET10-P-SLAGERC               
399900     MOVE FSUM-SLAGER(IX4)         TO  W001-DET10-SLAGERD                 
400000     MOVE FSUM-PROC-SLAGER(IX4)    TO  W001-DET10-P-SLAGERD               
400100     MOVE FSUM-SLAGER(IX5)         TO  W001-DET10-SLAGERE                 
400200     MOVE FSUM-PROC-SLAGER(IX5)    TO  W001-DET10-P-SLAGERE               
400300     MOVE FSUM-SLAGER(IX6)         TO  W001-DET10-SLAGERF                 
400400     MOVE FSUM-PROC-SLAGER(IX6)    TO  W001-DET10-P-SLAGERF               
400500     MOVE FSUM-SLAGER(IX7)         TO  W001-DET10-SLAGERG                 
400600     MOVE FSUM-PROC-SLAGER(IX7)    TO  W001-DET10-P-SLAGERG               
400700     MOVE FSUM-SLAGER(IX8)         TO  W001-DET10-SLAGERH                 
400800     MOVE FSUM-PROC-SLAGER(IX8)    TO  W001-DET10-P-SLAGERH               
400900     MOVE TOT-SLAGER               TO  W001-DET10-TOT                     
401000     MOVE TOT-PROC-SLAGER          TO  W001-DET10-P-TOT                   
401100     MOVE W001-DETALJRAD-10 TO W001-RAD                                   
401200     MOVE +1 TO W001-SKIP                                                 
401300     PERFORM S21-SKRIV-LISTA                                              
401400                                                                          
401500     MOVE FSUM-MLAGER(IX1)         TO  W001-DET11-MLAGERA                 
401600     MOVE FSUM-PROC-MLAGER(IX1)    TO  W001-DET11-P-MLAGERA               
401700     MOVE FSUM-MLAGER(IX2)         TO  W001-DET11-MLAGERB                 
401800     MOVE FSUM-PROC-MLAGER(IX2)    TO  W001-DET11-P-MLAGERB               
401900     MOVE FSUM-MLAGER(IX3)         TO  W001-DET11-MLAGERC                 
402000     MOVE FSUM-PROC-MLAGER(IX3)    TO  W001-DET11-P-MLAGERC               
402100     MOVE FSUM-MLAGER(IX4)         TO  W001-DET11-MLAGERD                 
402200     MOVE FSUM-PROC-MLAGER(IX4)    TO  W001-DET11-P-MLAGERD               
402300     MOVE FSUM-MLAGER(IX5)         TO  W001-DET11-MLAGERE                 
402400     MOVE FSUM-PROC-MLAGER(IX5)    TO  W001-DET11-P-MLAGERE               
402500     MOVE FSUM-MLAGER(IX6)         TO  W001-DET11-MLAGERF                 
402600     MOVE FSUM-PROC-MLAGER(IX6)    TO  W001-DET11-P-MLAGERF               
402700     MOVE FSUM-MLAGER(IX7)         TO  W001-DET11-MLAGERG                 
402800     MOVE FSUM-PROC-MLAGER(IX7)    TO  W001-DET11-P-MLAGERG               
402900     MOVE FSUM-MLAGER(IX8)         TO  W001-DET11-MLAGERH                 
403000     MOVE FSUM-PROC-MLAGER(IX8)    TO  W001-DET11-P-MLAGERH               
403100     MOVE TOT-MLAGER               TO  W001-DET11-TOT                     
403200     MOVE TOT-PROC-MLAGER          TO  W001-DET11-P-TOT                   
403300     MOVE W001-DETALJRAD-11 TO W001-RAD                                   
403400     MOVE +1 TO W001-SKIP                                                 
403500     PERFORM S21-SKRIV-LISTA                                              
403600                                                                          
403700     MOVE FSUM-KVOT(IX1)           TO  W001-DET12-KVOTA                   
403800     MOVE FSUM-PROC-KVOT(IX1)      TO  W001-DET12-P-KVOTA                 
403900     MOVE FSUM-KVOT(IX2)           TO  W001-DET12-KVOTB                   
404000     MOVE FSUM-PROC-KVOT(IX2)      TO  W001-DET12-P-KVOTB                 
404100     MOVE FSUM-KVOT(IX3)           TO  W001-DET12-KVOTC                   
404200     MOVE FSUM-PROC-KVOT(IX3)      TO  W001-DET12-P-KVOTC                 
404300     MOVE FSUM-KVOT(IX4)           TO  W001-DET12-KVOTD                   
404400     MOVE FSUM-PROC-KVOT(IX4)      TO  W001-DET12-P-KVOTD                 
404500     MOVE FSUM-KVOT(IX5)           TO  W001-DET12-KVOTE                   
404600     MOVE FSUM-PROC-KVOT(IX5)      TO  W001-DET12-P-KVOTE                 
404700     MOVE FSUM-KVOT(IX6)           TO  W001-DET12-KVOTF                   
404800     MOVE FSUM-PROC-KVOT(IX6)      TO  W001-DET12-P-KVOTF                 
404900     MOVE FSUM-KVOT(IX7)           TO  W001-DET12-KVOTG                   
405000     MOVE FSUM-PROC-KVOT(IX7)      TO  W001-DET12-P-KVOTG                 
405100     MOVE FSUM-KVOT(IX8)           TO  W001-DET12-KVOTH                   
405200     MOVE FSUM-PROC-KVOT(IX8)      TO  W001-DET12-P-KVOTH                 
405300     MOVE TOT-KVOT                 TO  W001-DET12-TOT                     
405400     MOVE TOT-PROC-MLAGER          TO  W001-DET12-P-TOT                   
405500     MOVE W001-DETALJRAD-12 TO W001-RAD                                   
405600     MOVE +1 TO W001-SKIP                                                 
405700     PERFORM S21-SKRIV-LISTA                                              
405800                                                                          
405900     MOVE FSUM-SPLIT(IX1)          TO  W001-DET13-SPLITA                  
406000     MOVE FSUM-SPLIT(IX2)          TO  W001-DET13-SPLITB                  
406100     MOVE FSUM-SPLIT(IX3)          TO  W001-DET13-SPLITC                  
406200     MOVE FSUM-SPLIT(IX4)          TO  W001-DET13-SPLITD                  
406300     MOVE FSUM-SPLIT(IX5)          TO  W001-DET13-SPLITE                  
406400     MOVE FSUM-SPLIT(IX6)          TO  W001-DET13-SPLITF                  
406500     MOVE FSUM-SPLIT(IX7)          TO  W001-DET13-SPLITG                  
406600     MOVE FSUM-SPLIT(IX8)          TO  W001-DET13-SPLITH                  
406700     MOVE TOT-SPLIT                TO  W001-DET13-TOT                     
406800     MOVE ZERO                     TO  W001-DET13-P-TOT                   
406900     MOVE W001-DETALJRAD-13 TO W001-RAD                                   
407000     MOVE +1 TO W001-SKIP                                                 
407100     PERFORM S21-SKRIV-LISTA                                              
407200                                                                          
407300     MOVE FSUM-OMSHAST-DISP(IX1)   TO  W001-DET14-OMSHASTA                
407400     MOVE FSUM-OMSHAST-PROC-D(IX1) TO  W001-DET14-P-OMSHASTA              
407500     MOVE FSUM-OMSHAST-DISP(IX2)   TO  W001-DET14-OMSHASTB                
407600     MOVE FSUM-OMSHAST-PROC-D(IX2) TO  W001-DET14-P-OMSHASTB              
407700     MOVE FSUM-OMSHAST-DISP(IX3)   TO  W001-DET14-OMSHASTC                
407800     MOVE FSUM-OMSHAST-PROC-D(IX3) TO  W001-DET14-P-OMSHASTC              
407900     MOVE FSUM-OMSHAST-DISP(IX4)   TO  W001-DET14-OMSHASTD                
408000     MOVE FSUM-OMSHAST-PROC-D(IX4) TO  W001-DET14-P-OMSHASTD              
408100     MOVE FSUM-OMSHAST-DISP(IX5)   TO  W001-DET14-OMSHASTE                
408200     MOVE FSUM-OMSHAST-PROC-D(IX5) TO  W001-DET14-P-OMSHASTE              
408300     MOVE FSUM-OMSHAST-DISP(IX6)   TO  W001-DET14-OMSHASTF                
408400     MOVE FSUM-OMSHAST-PROC-D(IX6) TO  W001-DET14-P-OMSHASTF              
408500     MOVE FSUM-OMSHAST-DISP(IX7)   TO  W001-DET14-OMSHASTG                
408600     MOVE FSUM-OMSHAST-PROC-D(IX7) TO  W001-DET14-P-OMSHASTG              
408700     MOVE FSUM-OMSHAST-DISP(IX8)   TO  W001-DET14-OMSHASTH                
408800     MOVE FSUM-OMSHAST-PROC-D(IX8) TO  W001-DET14-P-OMSHASTH              
408900     MOVE TOT-OMSHAST-DISP         TO  W001-DET14-TOT                     
409000     MOVE ZERO                     TO  W001-DET14-P-TOT                   
409100     MOVE W001-DETALJRAD-14 TO W001-RAD                                   
409200     MOVE +1 TO W001-SKIP                                                 
409300     PERFORM S21-SKRIV-LISTA                                              
409400                                                                          
409500     MOVE FSUM-OMSHAST-LS(IX1)      TO W001-DET15-OMSHASTA                
409600     MOVE FSUM-OMSHAST-PROC-LS(IX1) TO W001-DET15-P-OMSHASTA              
409700     MOVE FSUM-OMSHAST-LS(IX2)      TO  W001-DET15-OMSHASTB               
409800     MOVE FSUM-OMSHAST-PROC-LS(IX2) TO  W001-DET15-P-OMSHASTB             
409900     MOVE FSUM-OMSHAST-LS(IX3)      TO  W001-DET15-OMSHASTC               
410000     MOVE FSUM-OMSHAST-PROC-LS(IX3) TO  W001-DET15-P-OMSHASTC             
410100     MOVE FSUM-OMSHAST-LS(IX4)      TO  W001-DET15-OMSHASTD               
410200     MOVE FSUM-OMSHAST-PROC-LS(IX4) TO  W001-DET15-P-OMSHASTD             
410300     MOVE FSUM-OMSHAST-LS(IX5)      TO  W001-DET15-OMSHASTE               
410400     MOVE FSUM-OMSHAST-PROC-LS(IX5) TO  W001-DET15-P-OMSHASTE             
410500     MOVE FSUM-OMSHAST-LS(IX6)      TO  W001-DET15-OMSHASTF               
410600     MOVE FSUM-OMSHAST-PROC-LS(IX6) TO  W001-DET15-P-OMSHASTF             
410700     MOVE FSUM-OMSHAST-LS(IX7)      TO  W001-DET15-OMSHASTG               
410800     MOVE FSUM-OMSHAST-PROC-LS(IX7) TO  W001-DET15-P-OMSHASTG             
410900     MOVE FSUM-OMSHAST-LS(IX8)      TO  W001-DET15-OMSHASTH               
411000     MOVE FSUM-OMSHAST-PROC-LS(IX8) TO  W001-DET15-P-OMSHASTH             
411100     MOVE TOT-OMSHAST-LS            TO  W001-DET15-TOT                    
411200     MOVE ZERO                      TO  W001-DET15-P-TOT                  
411300     MOVE W001-DETALJRAD-15 TO W001-RAD                                   
411400     MOVE +1 TO W001-SKIP                                                 
411500     PERFORM S21-SKRIV-LISTA                                              
411600                                                                          
411700     MOVE FSUM-SERVG-AKT(IX1)      TO  W001-DET16-SERVGA-A                
411800     MOVE FSUM-SERVG-PAS(IX1)      TO  W001-DET16-SERVGA-P                
411900     MOVE FSUM-SERVG-AKT(IX2)      TO  W001-DET16-SERVGB-A                
412000     MOVE FSUM-SERVG-PAS(IX2)      TO  W001-DET16-SERVGB-P                
412100     MOVE FSUM-SERVG-AKT(IX3)      TO  W001-DET16-SERVGC-A                
412200     MOVE FSUM-SERVG-PAS(IX3)      TO  W001-DET16-SERVGC-P                
412300     MOVE FSUM-SERVG-AKT(IX4)      TO  W001-DET16-SERVGD-A                
412400     MOVE FSUM-SERVG-PAS(IX4)      TO  W001-DET16-SERVGD-P                
412500     MOVE FSUM-SERVG-AKT(IX5)      TO  W001-DET16-SERVGE-A                
412600     MOVE FSUM-SERVG-PAS(IX5)      TO  W001-DET16-SERVGE-P                
412700     MOVE FSUM-SERVG-AKT(IX6)      TO  W001-DET16-SERVGF-A                
412800     MOVE FSUM-SERVG-PAS(IX6)      TO  W001-DET16-SERVGF-P                
412900     MOVE FSUM-SERVG-AKT(IX7)      TO  W001-DET16-SERVGG-A                
413000     MOVE FSUM-SERVG-PAS(IX7)      TO  W001-DET16-SERVGG-P                
413100     MOVE FSUM-SERVG-AKT(IX8)      TO  W001-DET16-SERVGH-A                
413200     MOVE FSUM-SERVG-PAS(IX8)      TO  W001-DET16-SERVGH-P                
413300     MOVE TOT-SERVG-AKT            TO  W001-DET16-TOT                     
413400     MOVE TOT-SERVG-PAS            TO  W001-DET16-P-TOT                   
413500     MOVE W001-DETALJRAD-16 TO W001-RAD                                   
413600     MOVE +1 TO W001-SKIP                                                 
413700     PERFORM S21-SKRIV-LISTA                                              
413800                                                                          
413900     MOVE FSUM-SERVG-TOT(IX1)      TO  W001-DET17-SERVGA-TOT              
414000     MOVE FSUM-SERVG-TOT(IX2)      TO  W001-DET17-SERVGB-TOT              
414100     MOVE FSUM-SERVG-TOT(IX3)      TO  W001-DET17-SERVGC-TOT              
414200     MOVE FSUM-SERVG-TOT(IX4)      TO  W001-DET17-SERVGD-TOT              
414300     MOVE FSUM-SERVG-TOT(IX5)      TO  W001-DET17-SERVGE-TOT              
414400     MOVE FSUM-SERVG-TOT(IX6)      TO  W001-DET17-SERVGF-TOT              
414500     MOVE FSUM-SERVG-TOT(IX7)      TO  W001-DET17-SERVGG-TOT              
414600     MOVE FSUM-SERVG-TOT(IX8)      TO  W001-DET17-SERVGH-TOT              
414700     MOVE TOT-SERVG-TOT            TO  W001-DET17-TOT                     
414800     MOVE W001-DETALJRAD-17 TO W001-RAD                                   
414900     MOVE +1 TO W001-SKIP                                                 
415000     PERFORM S21-SKRIV-LISTA                                              
415100                                                                          
415200     MOVE FSUM-SERVG-TEO(IX1)      TO  W001-DET18-SERVGA-TEO              
415300     MOVE FSUM-SERVG-TEO(IX2)      TO  W001-DET18-SERVGB-TEO              
415400     MOVE FSUM-SERVG-TEO(IX3)      TO  W001-DET18-SERVGC-TEO              
415500     MOVE FSUM-SERVG-TEO(IX4)      TO  W001-DET18-SERVGD-TEO              
415600     MOVE FSUM-SERVG-TEO(IX5)      TO  W001-DET18-SERVGE-TEO              
415700     MOVE FSUM-SERVG-TEO(IX6)      TO  W001-DET18-SERVGF-TEO              
415800     MOVE FSUM-SERVG-TEO(IX7)      TO  W001-DET18-SERVGG-TEO              
415900     MOVE FSUM-SERVG-TEO(IX8)      TO  W001-DET18-SERVGH-TEO              
416000     MOVE TOT-SERVG-TEO            TO  W001-DET18-TOT                     
416100     MOVE W001-DETALJRAD-18 TO W001-RAD                                   
416200     MOVE +1 TO W001-SKIP                                                 
416300     PERFORM S21-SKRIV-LISTA                                              
416400     .                                                                    
416500     EJECT                                                                
416600 Z-FINIT SECTION.                                                         
416700                                                                          
416800     CLOSE W23170                                                         
416900           W23171-001                                                     
417000                                                                          
417100     MOVE 'S' TO POSTSUM-OPKOD                                            
417200     CALL POSTSUM USING POSTSUM-PARM                                      
417300     .                                                                    
417400     EJECT                                                                
417500 S01-LAS-W23170 SECTION.                                                  
417600                                                                          
417700     READ W23170 INTO IN-AREA                                             
417800     AT END                                                               
417900         SET END-OF-W23170 TO TRUE                                        
418000                                                                          
418100     NOT AT END                                                           
418200        MOVE 'W23171'      TO POSTSUM-FDNAMN                              
418300        MOVE 'W23171D1'    TO POSTSUM-DDNAMN2                             
418400        MOVE SPACE         TO POSTSUM-TRANSTYP                            
418500        CALL POSTSUM USING POSTSUM-PARM                                   
418600     .                                                                    
418700     EJECT                                                                
418800 S21-SKRIV-LISTA SECTION.                                                 
418900                                                                          
419000     WRITE W23171-001-RAD FROM W001-RAD AFTER W001-SKIP                   
419100                                                                          
419200     MOVE SPACE TO W001-RAD                                               
419300     ADD  +1 TO W001-ANTAL-RADER                                          
419400     .                                                                    
419500     EJECT                                                                
419600 S21A-SKRIV-RUBRIKER SECTION.                                             
419700                                                                          
419800     ADD +1 TO W001-SIDRAKNARE                                            
419900     MOVE W001-SIDRAKNARE TO W001-SID                                     
420000     WRITE W23171-001-RAD FROM W001-RUBRIK1 AFTER PAGE                    
420100     WRITE W23171-001-RAD FROM W001-RUBRIK3 AFTER 2                       
420200     MOVE +2 TO W001-ANTAL-RADER                                          
420300     MOVE +2 TO W001-SKIP                                                 
420400     .                                                                    
420500     EJECT                                                                
420600 S30-SKRIV-DAP1 SECTION.                                                  
420700                                                                          
420800     STRING ' ¤DAPW23171-0' WS-IDDC (IDDC-IX)                             
420900       DELIMITED BY SIZE INTO W001-DAP                                    
421000     WRITE W23171-001-RAD FROM W001-DAP                                   
421100                                                                          
421200     MOVE SPACE TO W001-DAP                                               
421300     .                                                                    
421400 S31-SKRIV-DAP2 SECTION.                                                  
421500                                                                          
421600     MOVE ' ¤DAPW23171' TO W001-DAP                                       
421700     WRITE W23171-001-RAD FROM W001-DAP                                   
421800                                                                          
421900     MOVE SPACE TO W001-DAP                                               
422000     .                                                                    
422100     EJECT                                                                
422200* --- IMS SEKTIONER ---                                                   
422300     SKIP3                                                                
422400                                                                          
422500 IMS-GET-WDB6      SECTION.                                               
422600                                                                          
422700     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B6                        
422800     MOVE '  GAGKGB'          TO GODK-STATUSKODER                         
422900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
423000     PERFORM IMS-STATUSKONTROLL                                           
423100     .                                                                    
423200     EJECT                                                                
423300 IMS-STATUSKONTROLL SECTION.                                              
423400                                                                          
423500     SET STATUS-IX TO 1                                                   
423600     SEARCH GODK-STATUS                                                   
423700       AT END                                                             
423800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
423900           DELIMITED BY SIZE INTO FELTEXT                                 
424000         DISPLAY FELTEXT                                                  
424100         CALL FELLOG                                                      
424200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
424300         CONTINUE                                                         
424400     END-SEARCH                                                           
424500     .                                                                    
